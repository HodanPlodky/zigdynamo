const std = @import("std");

pub fn FlexibleArr(comptime T: type, comptime Index: type) type {
    comptime {
        if (@alignOf(Index) < @alignOf(T)) {
            @compileError("Type of index in flexible array has lower align then the type of store value");
        }
    }
    return packed struct {
        const Self = @This();
        count: Index,

        pub fn additional_size(count: Index) usize {
            return @sizeOf(T) * count;
        }

        pub fn get_ptr(self: *Self, index: Index) *T {
            const place = @intFromPtr(&self.count);
            return @ptrFromInt(place + @sizeOf(Index) + @sizeOf(T) * index);
        }

        pub fn get_ptr_const(self: *const Self, index: Index) *const T {
            const place = @intFromPtr(&self.count);
            return @ptrFromInt(place + @sizeOf(Index) + @sizeOf(T) * index);
        }

        pub fn get(self: *Self, index: Index) T {
            return self.get_ptr(index).*;
        }

        pub fn set(self: *Self, index: Index, val: T) void {
            self.get_ptr(index).* = val;
        }

        pub fn get_slice(self: *Self) []T {
            return self.get_unchecked_slice()[0..@intCast(self.count)];
        }

        pub fn get_slice_const(self: *const Self) []const T {
            return self.get_unchecked_slice_const()[0..@intCast(self.count)];
        }

        pub fn get_unchecked_slice(self: *Self) [*]T {
            return self.get_ptr(@intCast(0));
        }

        pub fn get_unchecked_slice_const(self: *const Self) [*]const T {
            return @ptrCast(self.get_ptr_const(@intCast(0)));
        }
    };
}

pub fn DistinctIndex(comptime Index: type) type {
    return struct {
        const Self = @This();

        index: Index,

        pub fn new(index: Index) Self {
            return Self{ .index = index };
        }

        pub fn get_usize(self: Self) usize {
            return @intCast(self.index);
        }
    };
}

pub fn DistinctArrayList(comptime Index: type, comptime T: type) type {
    return struct {
        const Self = @This();

        data: std.ArrayList(T),

        pub fn init(alloc: std.mem.Allocator) Self {
            return Self{
                .data = std.ArrayList(T).init(alloc),
            };
        }

        pub fn deinit(self: *const Self) void {
            self.data.deinit();
        }

        pub fn get(self: *const Self, index: Index) T {
            const raw_index = index.get_usize();
            return self.data.items[raw_index];
        }

        pub fn get_ptr(self: *const Self, index: Index) *T {
            const raw_index = index.get_usize();
            return &self.data.items[raw_index];
        }

        pub fn get_ptr_const(self: *const Self, index: Index) *const T {
            const raw_index = index.get_usize();
            return &self.data.items[raw_index];
        }

        pub fn set(self: *const Self, index: Index, val: T) void {
            const raw_index = index.get_usize();
            self.data.items[raw_index] = val;
        }
    };
}

pub fn DistinctData(comptime Index: type, comptime T: type) struct { Index: type, ArrayList: type } {
    const DistIndex = DistinctIndex(Index);
    const DistArrayList = DistinctArrayList(DistIndex, T);

    return struct {
        .Index = DistIndex,
        .ArrayList = DistArrayList,
    };
}

test "distinct arrays 1" {
    const SmallIndex = DistinctIndex(u8);

    const SmallArray = DistinctArrayList(SmallIndex, i32);
    var small = SmallArray.init(std.testing.allocator);
    defer small.deinit();

    try small.data.append(10);
    small.set(SmallIndex.new(0), 11);

    try std.testing.expectEqual(11, small.get(SmallIndex.new(0)));
}
