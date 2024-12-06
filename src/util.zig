const std = @import("std");

pub const BodyIndex = enum(usize) { _ };

pub fn KeyedArrayList(Index: type, T: type) type {
    if (@typeInfo(Index) != .Enum) {
        @compileError("index '" ++ @typeName(Index) ++ "' must be an enum");
    }
    return struct {
        arr: std.ArrayList(T),

        const Self = @This();

        pub inline fn init(allocator: std.mem.Allocator) Self {
            return .{
                .arr = std.ArrayList(T).init(allocator),
            };
        }
        pub inline fn init_capacity(allocator: std.mem.Allocator, capacity: usize) !Self {
            return .{
                .arr = try std.ArrayList(T).initCapacity(allocator, capacity),
            };
        }

        pub inline fn range(self: *const Self, span: struct { Index, Index }) KeyedArray(Index, T) {
            return .{ .arr = self.arr.items[@intFromEnum(span[0])..@intFromEnum(span[1])] };
        }

        pub inline fn len(self: *const Self) Index {
            return @enumFromInt(self.arr.items.len);
        }

        pub inline fn to_array(self: *const Self) KeyedArray(Index, T) {
            return .{ .arr = self.arr.items };
        }

        pub inline fn to_array_mut(self: *Self) KeyedArrayMut(Index, T) {
            return .{ .arr = self.arr.items };
        }
    };
}

pub fn KeyedArray(Index: type, T: type) type {
    if (@typeInfo(Index) != .Enum) {
        @compileError("index '" ++ @typeName(Index) ++ "' must be an enum");
    }
    return struct {
        arr: []const T = &.{},

        const Self = @This();
        pub inline fn len(self: *const Self) Index {
            return @enumFromInt(self.arr.items.len);
        }

        pub inline fn range(self: *const Self, span: struct { Index, Index }) Self {
            return .{ .arr = self.arr[@intFromEnum(span[0])..@intFromEnum(span[1])] };
        }
        pub inline fn range_till_end(self: *const Self, start: Index) Self {
            return .{ .arr = self.arr[@intFromEnum(start)..] };
        }

        pub inline fn item(self: *const Self, idx: Index) *const T {
            return &self.arr[@intFromEnum(idx)];
        }
    };
}

pub fn KeyedArrayMut(Index: type, T: type) type {
    if (@typeInfo(Index) != .Enum) {
        @compileError("index '" ++ @typeName(Index) ++ "' must be an enum");
    }
    return struct {
        arr: []T = &.{},

        const Self = @This();
        pub inline fn len(self: *const Self) Index {
            return @enumFromInt(self.arr.items.len);
        }

        pub inline fn range_mut(self: *Self, span: struct { Index, Index }) Self {
            return .{ .arr = self.arr.items[@intFromEnum(span[0])..@intFromEnum(span[1])] };
        }
        pub inline fn range_mut_till_end(self: *const Self, start: Index) Self {
            return .{ .arr = self.arr[@intFromEnum(start)..] };
        }
        pub inline fn item_mut(self: *Self, idx: Index) *T {
            return &self.arr.items[@intFromEnum(idx)];
        }

        pub inline fn to_array(self: *const Self) KeyedArray(Index, T) {
            return .{ .arr = self.arr };
        }
    };
}

pub fn ComptimeArrayListInit(T: type, count: comptime_int) type {
    return struct {
        len: usize,
        items: []T,
        buffer: [count]T,

        const Self = @This();

        pub fn init() Self {
            return .{
                .len = 0,
                .items = &[_]T{},
                .buffer = undefined,
            };
        }

        pub fn append(self: *Self, item: T) !void {
            if (self.len < self.buffer.len) {
                self.buffer[self.len] = item;
                self.len += 1;
                self.items = self.buffer[0..self.len];
            } else {
                return error.OutOfMemory;
            }
        }
    };
}

pub fn run_append_stdout(allocator: std.mem.Allocator, argv: []const []const u8, output: *std.ArrayList(u8)) !void {
    var child = std.process.Child.init(argv, allocator);
    child.stdout_behavior = .Pipe;
    child.spawn() catch @panic(""); //|err| std.debug.panic("Failed to execute '{s}': {!}\n=====\n{s}", .{@tagName(atom.lang, err, atom_text)});

    var poller = std.io.poll(allocator, enum { stdout }, .{ .stdout = child.stdout.? });
    defer poller.deinit();
    while (poller.poll() catch |err| std.debug.panic("{!}", .{err})) {
        if (poller.fifo(.stdout).count > std.math.maxInt(u32)) {
            std.debug.panic("Too large", .{});
        }
    }
    const fifo = poller.fifo(.stdout);
    try output.appendSlice(fifo.buf[fifo.head..][0..fifo.count]);

    const term = child.wait() catch |err| std.debug.panic("{!}", .{err});
    _ = term;
}

pub fn assert(T: type, a: T, comptime cmp: enum { LE, GE, L, G, EQ, NE }, b: T) ?struct { T, []const u8, T } {
    const op, const success = switch (cmp) {
        .L => .{ "<", a < b },
        .LE => .{ "<=", a <= b },
        .G => .{ ">", a > b },
        .GE => .{ ">=", a >= b },
        .EQ => .{ "==", a == b },
        .NE => .{ "!=", a != b },
    };
    if (!success) {
        return .{ a, op, b };
    } else {
        return null;
    }
}
