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

        pub fn get(self: *Self, index: Index) T {
            return self.get_ptr(index).*;
        }

        pub fn set(self: *Self, index: Index, val: T) void {
            self.get_ptr(index).* = val;
        }

        pub fn get_slice(self: *Self) []T {
            return self.get_unchecked_slice()[0..@intCast(self.count)];
        }

        pub fn get_unchecked_slice(self: *Self) [*]T {
            return self.get_ptr(@intCast(0));
        }
    };
}
