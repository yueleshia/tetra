const std = @import("std");
const core = @import("./s__core.zig");
const markup = @import("./s__markup.zig");
const util = @import("./util.zig");

// run: zig build run -- parse --format adt src/test.adoc
//run: zig build run -- parse src/test.adoc

const CmdTree = struct {
    markup: struct {
        format: enum { adt, json, csv_debug } = .adt,
    } = .{},
    parse: struct {
        format: enum { adt, raw } = .raw,
        filter: []u8 = "",
    } = .{},
};

var cmdtree = CmdTree{};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var leak_malloc = std.heap.GeneralPurposeAllocator(.{}){};
    const leak_allocator = leak_malloc.allocator();

    // Parse args into string array (error union needs 'try')
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var parser = core.Parser.init(leak_allocator) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };
    if (false) {
        var git_root_dir = std.ArrayList(u8).init(leak_allocator);
        try util.run_append_stdout(leak_allocator, &.{ "git", "rev-parse", "--show-toplevel" }, &git_root_dir);
        parser.add_file(leak_allocator, try std.fmt.allocPrint(leak_allocator, "{s}/src/test.adoc", .{git_root_dir.items[0 .. git_root_dir.items.len - 1]})) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        parser.process_all(leak_allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.MaxErrors => {},
        };

        for (parser.tokens.items) |segment| {
            std.debug.print("{s}", .{parser.output.range(segment.text).arr});
        }
        for (core.parse_errors.items) |err| {
            err.print(parser.file_list.to_array());
        }
        std.process.exit(0);
    }

    ////////////////////////////////////////////////////////////////////////////
    const command = switch (args.len) {
        0, 1 => std.debug.panic("@TODO: display help because not enough arguments", .{}),
        else => args[1],
    };

    var fsm: enum { opt, pos } = .pos;
    var positionals = try std.ArrayList([]u8).initCapacity(leak_allocator, command.len);
    var prev_option: enum {
        @"!",
        filter,
        format,
    } = .@"!";
    if (std.mem.eql(u8, command, "tags")) {
        for (args[2..]) |arg| {
            positionals.appendAssumeCapacity(arg);
        }
        if (positionals.items.len != 1) {
            std.debug.print("Expected 1 args. You gave {d}\n", .{positionals.items.len});
            return error.NotEnoughArgs;
        }

        parser.add_file(leak_allocator, positionals.items[0]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        const source = parser.file_list.arr.items[0].source;
        var marker = markup.Marker.init(core.Context.process(source), leak_allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };

        marker.process(.{ .arr = source }, &parser.frags) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        marker.process_eof(&parser.frags);

        const writer = std.io.getStdOut().writer();
        var buffer = std.StringHashMap(void).init(leak_allocator);
        try buffer.ensureTotalCapacity(1024);
        for (marker.tokens.items) |hunk| {
            var iter = std.mem.splitAny(u8, parser.frags.range(hunk.tags).arr, " \n\r");
            while (iter.next()) |tag| {
                if (tag.len > 0) {
                    if (try buffer.fetchPut(tag, {})) |_| {} else {
                        _ = try writer.writeAll(tag);
                        _ = try writer.writeAll("\n");
                    }
                }
            }
        }
    } else if (std.mem.eql(u8, command, "markup")) {
        for (args[2..]) |arg| {
            switch (fsm) {
                .pos => {
                    if (std.mem.eql(u8, "--format", arg)) {
                        prev_option = .format;
                        fsm = .opt;
                    } else {
                        positionals.appendAssumeCapacity(arg);
                    }
                },
                .opt => {
                    fsm = .pos;
                    switch (prev_option) {
                        .format => if (std.mem.eql(u8, "adt", arg)) {
                            cmdtree.markup.format = .adt;
                        } else if (std.mem.eql(u8, "csv_debug", arg)) {
                            cmdtree.markup.format = .csv_debug;
                        } else if (std.mem.eql(u8, "json", arg)) {
                            cmdtree.markup.format = .json;
                        } else {
                            std.debug.panic("--format '{s}' is invalid format", .{arg});
                        },
                        .filter => unreachable,
                        .@"!" => std.debug.panic("Not a valid option: --format {s}\n{s}", .{ arg, @tagName(prev_option) }),
                    }
                },
            }
        }

        if (positionals.items.len != 1) {
            std.debug.print("Expected 1 args. You gave {d}", .{positionals.items.len});
            return error.NotEnoughArgs;
        }
        parser.add_file(leak_allocator, positionals.items[0]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        const source = parser.file_list.arr.items[0].source;
        var marker = markup.Marker.init(core.Context.process(source), leak_allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };

        const source2: util.KeyedArray(markup.StrIndex, u8) = .{ .arr = source };
        marker.process(source2, &parser.frags) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        marker.process_eof(&parser.frags);

        switch (cmdtree.markup.format) {
            .adt => {
                const writer = std.io.getStdOut().writer();

                for (0.., marker.tokens.items) |i, hunk| {
                    const tags = std.mem.trim(u8, parser.frags.range(hunk.tags).arr, " \n\r");

                    if (0 != i) {
                        switch (hunk.lang) {
                            ._string => _ = try writer.writeAll("stanza" ++ UNIT_SEPARATOR ++ UNIT_SEPARATOR),
                            else => _ = try writer.writeAll("action" ++ UNIT_SEPARATOR ++ UNIT_SEPARATOR),
                        }
                        _ = try writer.writeAll(tags);
                        _ = try writer.writeAll(UNIT_SEPARATOR);
                        _ = try writer.writeAll(RECORD_SEPARATOR);
                    }
                    for (marker.atoms.range(hunk.body).arr) |atom| {
                        switch (atom.lang) {
                            ._remark => _ = try writer.writeAll("remark" ++ UNIT_SEPARATOR ++ UNIT_SEPARATOR),
                            ._string => switch (hunk.lang) {
                                ._string => _ = try writer.writeAll("string" ++ UNIT_SEPARATOR ++ UNIT_SEPARATOR),
                                else => _ = try writer.writeAll("script" ++ UNIT_SEPARATOR ++ UNIT_SEPARATOR),
                            },
                            else => {
                                _ = try writer.writeAll("inline" ++ UNIT_SEPARATOR);
                                _ = try writer.writeAll(@tagName(atom.lang));
                                _ = try writer.writeAll(UNIT_SEPARATOR);
                            },
                        }
                        _ = try writer.writeAll(tags);
                        _ = try writer.writeAll(UNIT_SEPARATOR);
                        _ = try writer.writeAll(source2.range(atom.source).arr);
                        _ = try writer.writeAll(RECORD_SEPARATOR);
                    }
                }
            },
            .csv_debug => {
                const writer = std.io.getStdOut().writer();

                for (marker.tokens.items) |hunk| {
                    const tags = std.mem.trim(u8, parser.frags.range(hunk.tags).arr, " \n\r");

                    switch (hunk.lang) {
                        ._string => _ = try writer.writeAll("stanza,,"),
                        else => _ = try writer.writeAll("action,,"),
                    }
                    _ = try writer.writeAll(tags);
                    _ = try writer.writeAll(",");
                    _ = try writer.writeAll("\n");

                    for (marker.atoms.range(hunk.body).arr) |atom| {
                        switch (atom.lang) {
                            ._remark => _ = try writer.writeAll("remark,,"),
                            ._string => switch (hunk.lang) {
                                ._string => _ = try writer.writeAll("string,,"),
                                else => _ = try writer.writeAll("script,,"),
                            },
                            else => {
                                _ = try writer.writeAll("inline,");
                                _ = try writer.writeAll(@tagName(atom.lang));
                                _ = try writer.writeAll(",");
                            },
                        }
                        _ = try writer.writeAll(tags);
                        _ = try writer.writeAll(",");
                        _ = try writer.writeAll(source2.range(atom.source).arr);
                        _ = try writer.writeAll("\n");
                    }
                }
            },
            .json => {
                std.debug.print("STUB: json output\n", .{});
                return error.TODO;
            },
        }
    } else if (std.mem.eql(u8, command, "parse")) {
        for (args[2..]) |arg| {
            switch (fsm) {
                .pos => {
                    if (std.mem.eql(u8, "--format", arg)) {
                        prev_option = .format;
                        fsm = .opt;
                    } else if (std.mem.eql(u8, "--filter", arg)) {
                        prev_option = .filter;
                        fsm = .opt;
                    } else {
                        positionals.appendAssumeCapacity(arg);
                    }
                },
                .opt => {
                    fsm = .pos;
                    switch (prev_option) {
                        .format => if (std.mem.eql(u8, "adt", arg)) {
                            cmdtree.parse.format = .adt;
                        } else if (std.mem.eql(u8, "raw", arg)) {
                            cmdtree.parse.format = .raw;
                        } else {
                            std.debug.panic("--format '{s}' is invalid format", .{arg});
                        },
                        .filter => cmdtree.parse.filter = arg,
                        .@"!" => std.debug.panic("Not a valid option: --format {s}\n{s}", .{ arg, @tagName(prev_option) }),
                    }
                },
            }
        }
        if (positionals.items.len != 1) {
            std.debug.print("Expected 1 args. You gave {d}", .{positionals.items.len});
            return error.NotEnoughArgs;
        }
        parser.add_file(leak_allocator, positionals.items[0]) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        parser.process_all(leak_allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.MaxErrors => {},
        };

        const writer = std.io.getStdOut().writer();
        switch (cmdtree.parse.format) {
            .adt => for (parser.tokens.items) |segment| {
                if (is_filter_match(parser.frags.range(segment.tags).arr, cmdtree.parse.filter)) continue;
                std.fmt.format(writer, "{s}{c}{s}{c}", .{
                    parser.frags.range(segment.tags).arr,
                    UNIT_SEPARATOR,
                    parser.output.range(segment.text).arr,
                    RECORD_SEPARATOR,
                }) catch |err| {
                    std.debug.panic("Failed to write to stdout unreachable: {!}", .{err});
                };
            },
            .raw => for (parser.tokens.items) |segment| {
                if (is_filter_match(parser.frags.range(segment.tags).arr, cmdtree.parse.filter)) continue;
                _ = writer.write(parser.output.range(segment.text).arr) catch |err| {
                    std.debug.panic("Failed to write to stdout unreachable: {!}", .{err});
                };
            },
        }
    } else {
        std.debug.panic("'{s}' is an unexpected command", .{command});
    }

    for (core.parse_errors.items) |err| {
        err.print(parser.file_list.to_array());
    }
}

fn is_filter_match(tags: []const u8, filter: []const u8) bool {
    var iter = std.mem.splitScalar(u8, tags, ' ');
    std.debug.assert(0 == (iter.next() orelse "").len);
    while (iter.next()) |tag| {
        if (std.mem.eql(u8, tag, filter) or std.mem.eql(u8, tag, "*")) {
            return false;
        }
    }
    return filter.len != 0;
}

const RECORD_SEPARATOR = &[_]u8{30};
const UNIT_SEPARATOR = &[_]u8{31};

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
