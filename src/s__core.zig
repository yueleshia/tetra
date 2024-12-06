const std = @import("std");
const util = @import("util.zig");
const markup = @import("s__markup.zig");

pub const FragIndex = enum(u16) { _ };
pub const SourceIndex = enum(u16) { _ };
pub const FileIndex = enum(u8) { _ };
pub const File = struct {
    path: [std.fs.MAX_PATH_BYTES]u8,
    source: []const u8,
    buffer: util.KeyedArrayMut(util.BodyIndex, u8),
};

pub const Context = struct {
    max_chord_count: u16 = 0,
    len: u16 = 0,

    const Self = @This();

    pub fn process(input: []const u8) Self {
        var chord_count: std.meta.FieldType(Context, .max_chord_count) = 0;

        var fsm: enum { Head, Body } = .Body;
        var prev: u8 = '\n';
        for (input) |curr| {
            switch (fsm) {
                .Head => switch (curr) {
                    '|', '!' => {
                        fsm = .Body;
                        chord_count += 0;
                    },
                    else => {},
                },
                .Body => switch (curr) {
                    '|', '!' => if (prev == '\n') {
                        fsm = .Head;
                    },
                    else => {},
                },
            }
            prev = curr;
        }
        return .{
            .max_chord_count = chord_count,
            .len = @truncate(input.len),
        };
    }

    fn add(self: *Self, addee: *const Self) void {
        inline for (@typeInfo(Self).Struct.fields) |field| {
            @field(self, field.name) += @field(addee, field.name);
        }
    }
};

const OutputIndex = enum(u32) { _ };
const Segment = struct {
    text: struct { OutputIndex, OutputIndex },
    tags: struct { FragIndex, FragIndex },
};

pub const Parser = struct {
    file_list: util.KeyedArrayList(FileIndex, File),
    output: util.KeyedArrayList(OutputIndex, u8),
    frags: util.KeyedArrayList(FragIndex, u8),
    tokens: std.ArrayList(Segment),
    //// @NOTE: Maximum number of concurrent errors in syntax before we exit
    //error_list: [4]struct {
    //    file: FileIndex,
    //    mesg: []const u8,
    //},

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{
            .file_list = util.KeyedArrayList(FileIndex, File).init(allocator),
            .output = util.KeyedArrayList(OutputIndex, u8).init(allocator),
            .frags = try util.KeyedArrayList(FragIndex, u8).init_capacity(allocator, 4096),
            .tokens = std.ArrayList(Segment).init(allocator),
        };
    }

    pub fn add_file(self: *Self, allocator: std.mem.Allocator, path: []const u8) !void {
        var fh = std.fs.cwd().openFile(path, .{ .mode = .read_only }) catch |err| switch (err) {
            else => std.debug.panic("Could not read '{s}'", .{path}),
        };
        defer fh.close();

        if (path.len > std.fs.MAX_PATH_BYTES) {
            @panic("@TODO");
        }

        var buf_reader = std.io.bufferedReader(fh.reader());
        var in_stream = buf_reader.reader();

        // Alloc for both 'source' and 'buffer'
        const stat = fh.stat() catch |err| switch (err) {
            else => std.debug.panic("@TODO", .{}),
        };
        const padding = 1024; // In the file is edited while reading, gives us some leeway
        var double_buffer = try std.ArrayList(u8).initCapacity(allocator, stat.size * 2 + padding);
        var buffer: [std.mem.page_size]u8 = undefined;
        while (true) {
            const size = in_stream.read(&buffer) catch |err| switch (err) {
                else => std.debug.panic("Error reading '{s}'", .{path}),
            };
            if (size == 0) break;
            if ((double_buffer.items.len + size) * 2 > double_buffer.capacity) {
                @setCold(true); // Probably only can happen if you write to the file at the same time as running us
                try double_buffer.ensureTotalCapacity(double_buffer.capacity * 2 + buffer.len);
                std.debug.print("If this does fail, please report it. Memory allocation strategy should be rethought", .{});
            }
            double_buffer.appendSliceAssumeCapacity(buffer[0..size]);
        }
        const len = double_buffer.items.len;
        double_buffer.resize(len * 2) catch std.debug.panic("The ensureTotalCapacity() should have caught this.", .{});

        var stuff = File{
            .path = undefined,
            .source = double_buffer.items[0..len],
            .buffer = .{ .arr = double_buffer.items[len..] },
        };
        @memcpy(stuff.path[0..path.len], path);

        self.file_list.arr.append(stuff) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
    }

    pub fn process_all(self: *Self, allocator: std.mem.Allocator) !void {
        const IndexType = @typeInfo(FileIndex).Enum.tag_type;
        std.debug.assert(self.file_list.arr.items.len < std.math.maxInt(IndexType));

        var context = Context{};
        var arena = std.heap.ArenaAllocator.init(allocator);
        const output_allocator = arena.allocator();

        for (0.., self.file_list.arr.items) |i, *file| {
            std.debug.assert(i < std.math.maxInt(IndexType));

            const file_context = Context.process(file.source);
            context.add(&file_context);

            var marker = markup.Marker.init(context, output_allocator) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
            };
            const source: util.KeyedArray(markup.StrIndex, u8) = .{ .arr = file.source };
            marker.process(source, &self.frags) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
            };
            marker.process_eof(&self.frags);

            if (false) {
                const writer = std.io.getStdErr().writer();
                for (marker.tokens.items) |chunk| {
                    chunk.format(writer, source, self.frags.to_array(), marker.atoms.to_array()) catch |err| std.debug.panic("{!}", .{err});
                }
            }

            // Execute
            var script_output = try std.ArrayList(u8).initCapacity(output_allocator, file.source.len);
            var cursor: OutputIndex = @enumFromInt(0);
            for (marker.tokens.items) |hunk| {
                var chunk_output = switch (hunk.lang) {
                    ._string => &self.output.arr,
                    else => &script_output,
                };

                // Parse body
                for (marker.atoms.to_array().range(hunk.body).arr) |atom| {
                    const atom_text = source.range(atom.source).arr;
                    switch (atom.lang) {
                        ._remark => {},
                        ._string => try chunk_output.appendSlice(atom_text),
                        .sh => {
                            util.run_append_stdout(output_allocator, &[_][]const u8{ "/bin/sh", "-c", atom_text }, chunk_output) catch |err| {
                                std.debug.panic("{!}", .{err});
                            };
                        },
                        else => std.debug.panic("@TODO: '{s}' is not supported yet", .{@tagName(atom.lang)}),
                    }
                }

                // Parse head
                switch (hunk.lang) {
                    ._string => {},
                    .sh => {
                        util.run_append_stdout(output_allocator, &[_][]const u8{ "/bin/sh", "-c", chunk_output.items }, &self.output.arr) catch |err| {
                            std.debug.panic("{!}", .{err});
                        };
                    },
                    else => {
                        std.debug.panic("@TODO: '{s}' is not supported yet", .{@tagName(hunk.lang)});
                    },
                }

                try self.tokens.append(.{
                    .text = .{ cursor, self.output.len() },
                    .tags = hunk.tags,
                });
                cursor = self.output.len();
            }
            _ = arena.reset(.retain_capacity);
        }
        return error.MaxErrors;
    }
};

//fn Token(T: type) type {
//    const A = struct {
//        start: Parser.MaxStrLen,
//        close: Parser.MaxStrLen,
//        value: T,
//    };
//    return A;
//}

pub const ParseError = struct {
    index: FileIndex,
    start: SourceIndex,
    close: SourceIndex,
    source: std.builtin.SourceLocation,
    message: []const u8,

    const Self = @This();
    const IndexType = usize;

    const line_max = 120;
    const line_max_str = std.fmt.comptimePrint("{d}", .{line_max});
    const max_int = std.math.maxInt(IndexType);
    const max_int_str = std.fmt.comptimePrint("{d}", .{max_int});
    const space_buf: [line_max]u8 = std.fmt.comptimePrint("{s: <" ++ line_max_str ++ "}", .{""}).*;
    const arrow_buf: [line_max]u8 = std.fmt.comptimePrint("{s:^<" ++ line_max_str ++ "}", .{""}).*;

    pub fn print(self: *const Self, file_list: util.KeyedArray(FileIndex, File)) void {
        const file = file_list.item(self.index);
        const first_start = @intFromEnum(self.start);
        const final_close = @intFromEnum(self.close);
        std.debug.print("Error found in {s}\n", .{file.path});
        std.debug.print("Source code {s}:{d}:{d}\n", .{ self.source.file, self.source.line, self.source.column });

        const first_close = std.mem.indexOfScalar(u8, file.source[first_start..final_close], '\n') orelse final_close;

        var iter = std.mem.splitScalar(u8, file.source, '\n');
        var i: IndexType = 1;
        var line_start: IndexType = 0;
        while (iter.next()) |line| {
            i += 1;
            const line_close = line_start + line.len;

            if (line_start <= first_start and first_start < line_close) {
                //std.debug.print("     |{d} <= {d} {d} <= {d}\n", .{line_start, first_start, final_close, line_close});
                var l_num_buf: [std.fmt.count("{d}", .{max_int})]u8 = undefined;
                const l_num = std.fmt.bufPrint(&l_num_buf, "{d}", .{i}) catch unreachable;
                //@memset(arrow_buf, '^');

                if (line.len > line_max) {
                    std.debug.panic(" {d} | @TODO: long terminal lines: {s}\n", .{ i, line });
                } else {
                    const template = " {s} | {s}{s}\n";
                    var context_buf: [std.fmt.count(template, .{ max_int_str, space_buf, "" })]u8 = undefined;
                    var pointer_buf: [context_buf.len]u8 = undefined;
                    const context = std.fmt.bufPrint(&context_buf, template, .{ l_num, line, "" }) catch unreachable;
                    const pointer = std.fmt.bufPrint(&pointer_buf, template, .{ space_buf[0..l_num.len], space_buf[0 .. first_start - line_start], arrow_buf[0 .. first_close - first_start] }) catch unreachable;

                    std.debug.print("{s}", .{context});
                    std.debug.print("{s}", .{pointer});
                }
            }
            line_start = iter.index orelse file.source.len;
        }
        std.debug.print("\n{s}\n", .{self.message});
    }

    //pub fn print_line(self *const Self, file_list: []const File) void {
    //}
};

pub var parse_errors = util.ComptimeArrayListInit(ParseError, 8).init();
pub fn append_parse_error(parse_err: ParseError) !void {
    parse_errors.append(parse_err) catch |err| switch (err) {
        error.OutOfMemory => return error.MaxErrors,
    };
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
