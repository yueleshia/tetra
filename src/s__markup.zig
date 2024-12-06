const std = @import("std");
const core = @import("s__core.zig");
const util = @import("util.zig");

////////////////////////////////////////////////////////////////////////////////
fn EnumFromList(list: []const LangEntry) type {
    return @Type(std.builtin.Type{ .Enum = .{
        .tag_type = u4,
        .decls = &.{},
        .fields = blk: {
            var variants: [list.len]std.builtin.Type.EnumField = undefined;
            for (0.., list) |i, entry| {
                const value = for (0.., lang_full_list) |j, val| {
                    if (std.mem.eql(u8, val[0], entry[0])) break j;
                } else @compileError("Is not a subset of lang_full_list");
                variants[i] = .{
                    .name = entry[0],
                    .value = value,
                };
            }
            break :blk &variants;
        },
        .is_exhaustive = true,
    } });
}
const LangEntry = struct { [:0]const u8, []const []const u8 };
const lang_full_list = [_]LangEntry{
    .{ "_remark", &[_][]const u8{} },
} ++ lang_hunk_list;
const lang_hunk_list = [_]LangEntry{
    .{ "_string", &[_][]const u8{} },
} ++ lang_code_list;
const lang_code_list = [_]LangEntry{
    .{ "sh", &[_][]const u8{"sh"} },
    .{ "tetra", &[_][]const u8{ "tetra", "" } },
    .{ "eyg", &[_][]const u8{"eyg"} },
    .{ "python3", &[_][]const u8{ "python3", "py3" } },
};

const LangAtom = EnumFromList(&lang_full_list);
const LangHunk = EnumFromList(&lang_hunk_list);
const LangCode = EnumFromList(&lang_code_list);

const text_2_lang = blk: {
    var flat_list_len = 0;
    for (lang_code_list) |entry| flat_list_len += entry[1].len;

    var mapping: [flat_list_len]struct { []const u8, LangCode } = undefined;
    var idx = 0;
    for (lang_code_list) |entry| {
        for (entry[1]) |alias| {
            mapping[idx] = .{ alias, @field(LangCode, entry[0]) };
            idx += 1;
        }
    }
    break :blk std.StaticStringMap(LangCode).initComptime(mapping);
};

////////////////////////////////////////////////////////////////////////////////
pub const StrIndex = enum(u16) { _ };
const AtomIndex = enum(u16) { _ };
const AtomRange = struct { AtomIndex, AtomIndex };
const SourceRange = struct { StrIndex, StrIndex };

//stanza: id, tags             // e.g. `@en jp` text
//action: id, lang, tags, text // e.g. `|sh en jp` text

//inline: id, lang, text       // e.g. `$sh echo hello$`
//string: id, text             // e.g. general text
//remark: id, text             // e.g. `#stuff`

const Atom = struct {
    source: struct { StrIndex, StrIndex },
    lang: LangAtom = ._remark,

    // Everything except tags and final tick
    pub fn format(self: *const @This(), writer: anytype, source: util.KeyedArray(StrIndex, u8)) !void {
        switch (self.lang) {
            ._remark => try std.fmt.format(writer, "`#{s}`", .{source.range(self.source).arr}),
            ._string => _ = try writer.writeAll(source.range(self.source).arr),
            else => try std.fmt.format(writer, "`${s} {s}$`", .{ @tagName(self.lang), source.range(self.source).arr }),
        }
    }
};

const Hunk = struct {
    id: struct { StrIndex, StrIndex } = .{ @enumFromInt(0), @enumFromInt(0) },
    lang: LangHunk = ._string,
    tags: struct { core.FragIndex, core.FragIndex } = .{ @enumFromInt(0), @enumFromInt(0) },
    body: struct { AtomIndex, AtomIndex } = .{ @enumFromInt(0), @enumFromInt(0) },

    pub fn format(
        self: *const @This(),
        writer: anytype,
        source: util.KeyedArray(StrIndex, u8),
        frags: util.KeyedArray(core.FragIndex, u8),
        atoms: util.KeyedArray(AtomIndex, Atom),
    ) !void {
        // Head
        const tags = frags.range(self.tags).arr;
        switch (self.lang) {
            ._string => if (0 == @intFromEnum(self.body[0])) {
                // Do nothing
            } else {
                try std.fmt.format(writer, "`@{s}`", .{tags});
            },
            else => try std.fmt.format(writer, "`|{s} {s}`", .{ @tagName(self.lang), tags }),
        }

        // Body
        for (atoms.range(self.body).arr) |atom| {
            try atom.format(writer, source);
        }
    }
};

pub const Marker = struct {
    const State = enum { Body, Tick, Lang, Tags, Code, RemarkBegin, RemarkClose };

    state: State = .Body,
    //atom_type: enum { Inline, String, Action, Remark } = .String,
    next_state: State = .Body,

    atoms: util.KeyedArrayList(AtomIndex, Atom),
    tokens: std.ArrayList(Hunk),

    idx: StrIndex = @enumFromInt(0),
    prev: u8 = 0,
    prev_idx: StrIndex = @enumFromInt(0),
    lang_hunk: LangHunk = ._string, // The first hunk is always a string
    lang_atom: LangAtom = ._string,

    cursor_atom: AtomIndex = @enumFromInt(0),
    cursor_src: StrIndex = @enumFromInt(0),
    cursor_tag: StrIndex = @enumFromInt(0),
    cursor_frag: core.FragIndex = @enumFromInt(0),

    pub fn init(context: core.Context, allocator: std.mem.Allocator) !@This() {
        return .{
            .atoms = try util.KeyedArrayList(AtomIndex, Atom).init_capacity(allocator, context.len + 1),
            .tokens = try std.ArrayList(Hunk).initCapacity(allocator, context.len),
        };
    }

    // Only pushing to frags will allocate
    pub fn process(self: *@This(), input: util.KeyedArray(StrIndex, u8), frags: *util.KeyedArrayList(core.FragIndex, u8)) !void {
        for (input.arr) |curr| {
            const idx_plus_1: StrIndex = @enumFromInt(@intFromEnum(self.idx) + 1);
            switch (self.state) {
                .Body => switch (curr) {
                    '`' => self.state = .Tick,
                    else => {},
                },
                .Tick => {
                    // `next_state == .Body` means ignore
                    // `next_state` is only used to queue transition after .Lang
                    self.state, self.next_state, const is_hunk = switch (curr) {
                        '@' => .{ .Tags, .Body, true },
                        '|' => .{ .Lang, .Tags, true },
                        '$' => .{ .Lang, .Code, false },
                        '#' => .{ .RemarkBegin, .Body, false },
                        else => .{ .Body, .Body, false },
                    };
                    if (self.state != .Body) {
                        self.atoms.arr.appendAssumeCapacity(Atom{
                            .source = .{ self.cursor_src, self.prev_idx },
                            .lang = ._string,
                        });
                        self.cursor_src = idx_plus_1;
                        self.cursor_tag = idx_plus_1;
                    }
                    if (is_hunk) {
                        self.tokens.appendAssumeCapacity(Hunk{
                            .tags = .{ self.cursor_frag, frags.len() },
                            //.head = self.cursor_atom,
                            .lang = self.lang_hunk,
                            .body = .{ self.cursor_atom, self.atoms.len() },
                        });
                        self.cursor_atom = self.atoms.len();
                        self.cursor_frag = frags.len();
                    }
                    // Set back to default so that `process_eof()` will end text
                    self.lang_hunk = ._string;
                },
                .Lang => switch (curr) {
                    ' ', '\n', '\r' => {
                        const lang_str = input.range(.{ self.cursor_src, self.idx }).arr;
                        const lang_var = text_2_lang.get(lang_str) orelse {
                            std.debug.panic("Unsupported language: '{s}'", .{lang_str});
                        };
                        switch (self.next_state) {
                            .Body => unreachable,
                            .Tick => unreachable,
                            .Lang => unreachable,
                            .Tags => self.lang_hunk = @enumFromInt(@intFromEnum(lang_var)),
                            .Code => self.lang_atom = @enumFromInt(@intFromEnum(lang_var)),
                            .RemarkBegin => unreachable,
                            .RemarkClose => unreachable,
                        }
                        self.state = self.next_state;
                        self.cursor_src = idx_plus_1;
                        self.cursor_tag = idx_plus_1;
                    },
                    '`' => @panic("Closed unexpectedly"),
                    else => {},
                },
                .Tags => switch (curr) {
                    ' ', '\n', '\r' => {
                        // An empty tag is a null allocation
                        try frags.arr.append(' ');
                        try frags.arr.appendSlice(input.range(.{ self.cursor_tag, self.idx }).arr);
                        self.cursor_tag = idx_plus_1;
                    },
                    '`' => {
                        try frags.arr.append(' ');
                        try frags.arr.appendSlice(input.range(.{ self.cursor_tag, self.idx }).arr);

                        self.state = .Body;
                        self.cursor_src = idx_plus_1;
                    },
                    '*', '_', 'a'...'z', 'A'...'Z' => {},
                    else => std.debug.panic("Invalid character: '{c}'", .{curr}),
                },
                .Code => switch (curr) {
                    '`' => if (self.prev == '$') {
                        self.state = .Body;
                        self.atoms.arr.appendAssumeCapacity(Atom{
                            .source = .{ self.cursor_src, self.prev_idx },
                            .lang = self.lang_atom,
                        });
                        self.cursor_src = idx_plus_1;
                    },
                    else => {},
                },
                .RemarkBegin => switch (curr) {
                    '`' => {
                        self.atoms.arr.appendAssumeCapacity(Atom{
                            .source = .{ self.cursor_src, self.idx },
                            .lang = ._remark,
                        });
                        self.cursor_src = idx_plus_1;
                        self.state = .Body;
                    },
                    '#' => self.state = .RemarkClose,
                    else => {},
                },
                .RemarkClose => switch (curr) {
                    '`' => {
                        self.atoms.arr.appendAssumeCapacity(Atom{
                            .source = .{ self.cursor_src, self.idx },
                            .lang = ._remark,
                        });
                        self.cursor_src = idx_plus_1;
                        self.state = .Body;
                    },
                    '#' => {},
                    else => {},
                },
            }
            self.prev = curr;
            self.prev_idx = self.idx;
            self.idx = idx_plus_1;
        }
    }

    pub fn process_eof(self: *@This(), frags: *util.KeyedArrayList(core.FragIndex, u8)) void {
        //std.debug.assert(self.tokens.items[0].lang == ._string);

        switch (self.state) {
            .Body => {
                self.atoms.arr.appendAssumeCapacity(Atom{
                    .source = .{ self.cursor_src, self.idx },
                    .lang = ._string,
                });
                self.tokens.appendAssumeCapacity(Hunk{
                    .tags = .{ self.cursor_frag, frags.len() },
                    .lang = self.lang_hunk,
                    .body = .{ self.cursor_atom, self.atoms.len() },
                });
            },
            .Tick => std.debug.panic("@TODO: {s}", .{@tagName(self.state)}),
            .Lang => std.debug.panic("@TODO: {s}", .{@tagName(self.state)}),
            .Tags => std.debug.panic("@TODO: {s}", .{@tagName(self.state)}),
            .Code => std.debug.panic("@TODO: {s}", .{@tagName(self.state)}),
            .RemarkBegin => std.debug.panic("@TODO: {s}", .{@tagName(self.state)}),
            .RemarkClose => std.debug.panic("@TODO: {s}", .{@tagName(self.state)}),
        }
    }
};
