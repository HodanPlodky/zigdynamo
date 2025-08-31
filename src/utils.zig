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

pub fn DistinctData(comptime I: type, comptime T: type) type {
    return struct {
        pub const Index = struct {
            const Self = @This();

            index: I,

            pub fn new(index: I) Self {
                return Self{ .index = index };
            }

            pub fn get_usize(self: Self) usize {
                return @intCast(self.index);
            }
        };

        pub const ArrayList = struct {
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

        pub const Multi = struct {
            const Self = @This();
            pub const Inner = T;
            pub const DistIndex = Index;

            data: std.MultiArrayList(T) = .{},

            pub fn init() Self {
                return Self{
                    .data = std.MultiArrayList(T){},
                };
            }

            pub fn len(self: *const Self) usize {
                return self.data.len;
            }

            pub fn deinit(self: *const Self) void {
                self.data.deinit();
            }

            pub fn get(self: *const Self, index: Index) T {
                const raw_index = index.get_usize();
                return self.data.get(raw_index);
            }

            pub fn set(self: *Self, index: Index, val: T) void {
                const raw_index = index.get_usize();
                self.data.set(raw_index, val);
            }
        };

        pub const ArrayListUn = struct {
            const Self = @This();
            pub const Inner = T;
            pub const DistIndex = Index;

            data: std.ArrayListUnmanaged(T) = .{},

            pub fn init(alloc: std.mem.Allocator) Self {
                return Self{
                    .data = std.ArrayList(T).init(alloc),
                };
            }

            pub fn len(self: *const Self) usize {
                return self.data.items.len;
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
    };
}

/// copy dat from one bit set
/// to another I dont know why
/// this is not in the standard lib
pub fn bit_set_move(noalias src: *const std.DynamicBitSetUnmanaged, noalias dst: *const std.DynamicBitSetUnmanaged) void {
    std.debug.assert(src.bit_length == dst.bit_length);
    // taken from stdlib
    // if this will create error I will be angy
    const len = (src.bit_length + (@bitSizeOf(std.DynamicBitSetUnmanaged.MaskInt) - 1)) / @bitSizeOf(std.DynamicBitSetUnmanaged.MaskInt);
    @memcpy(dst.masks[0..len], src.masks[0..len]);
}

test "distinct arrays 1" {
    const Small = DistinctData(u8, i32);
    const SmallIndex = Small.Index;

    const SmallArray = Small.ArrayList;
    var small = SmallArray.init(std.testing.allocator);
    defer small.deinit();

    try small.data.append(10);
    small.set(SmallIndex.new(0), 11);

    try std.testing.expectEqual(11, small.get(SmallIndex.new(0)));
}

test "bit set move" {
    const BitSet = std.DynamicBitSetUnmanaged;
    var a_set = try BitSet.initEmpty(std.testing.allocator, 1021);
    defer a_set.deinit(std.testing.allocator);
    var b_set = try BitSet.initEmpty(std.testing.allocator, 1021);
    defer b_set.deinit(std.testing.allocator);

    // randomly generated beforehand
    const bits: [127]usize = .{
        386, 105, 331, 751, 525, 678, 913, 198, 922, 658, 1019, 361, 515, 1007, 208, 549, 599, 358, 283, 333, 238, 643, 327, 488, 643, 834, 643, 960, 923, 938, 686, 542, 642, 659, 580, 844, 666, 918, 52, 326, 831, 686, 902, 694, 777, 865, 399, 735, 931, 437, 755, 164, 950, 585, 223, 214, 759, 360, 953, 975, 698, 597, 412, 954, 148, 952, 327, 473, 78, 961, 922, 952, 344, 687, 203, 667, 517, 540, 916, 649, 92, 852, 459, 738, 113, 889, 667, 877, 799, 771, 269, 225, 752, 240, 614, 198, 378, 456, 356, 102, 478, 8, 19, 861, 1011, 545, 1, 117, 100, 387, 199, 360, 208, 823, 498, 319, 412, 378, 266, 823, 202, 93, 701, 774, 72, 605, 206,
    };

    for (bits) |bit| {
        a_set.set(bit);
    }

    // this is the function I am testing
    bit_set_move(&a_set, &b_set);

    try std.testing.expect(a_set.eql(b_set));

    for (bits) |bit| {
        try std.testing.expect(a_set.isSet(bit));
        try std.testing.expect(b_set.isSet(bit));
    }

    const not_there: [907]usize = .{ 0, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 73, 74, 75, 76, 77, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 94, 95, 96, 97, 98, 99, 101, 103, 104, 106, 107, 108, 109, 110, 111, 112, 114, 115, 116, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 200, 201, 204, 205, 207, 209, 210, 211, 212, 213, 215, 216, 217, 218, 219, 220, 221, 222, 224, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 239, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 267, 268, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 320, 321, 322, 323, 324, 325, 328, 329, 330, 332, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 357, 359, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 379, 380, 381, 382, 383, 384, 385, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 457, 458, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 474, 475, 476, 477, 479, 480, 481, 482, 483, 484, 485, 486, 487, 489, 490, 491, 492, 493, 494, 495, 496, 497, 499, 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 516, 518, 519, 520, 521, 522, 523, 524, 526, 527, 528, 529, 530, 531, 532, 533, 534, 535, 536, 537, 538, 539, 541, 543, 544, 546, 547, 548, 550, 551, 552, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 575, 576, 577, 578, 579, 581, 582, 583, 584, 586, 587, 588, 589, 590, 591, 592, 593, 594, 595, 596, 598, 600, 601, 602, 603, 604, 606, 607, 608, 609, 610, 611, 612, 613, 615, 616, 617, 618, 619, 620, 621, 622, 623, 624, 625, 626, 627, 628, 629, 630, 631, 632, 633, 634, 635, 636, 637, 638, 639, 640, 641, 644, 645, 646, 647, 648, 650, 651, 652, 653, 654, 655, 656, 657, 660, 661, 662, 663, 664, 665, 668, 669, 670, 671, 672, 673, 674, 675, 676, 677, 679, 680, 681, 682, 683, 684, 685, 688, 689, 690, 691, 692, 693, 695, 696, 697, 699, 700, 702, 703, 704, 705, 706, 707, 708, 709, 710, 711, 712, 713, 714, 715, 716, 717, 718, 719, 720, 721, 722, 723, 724, 725, 726, 727, 728, 729, 730, 731, 732, 733, 734, 736, 737, 739, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749, 750, 753, 754, 756, 757, 758, 760, 761, 762, 763, 764, 765, 766, 767, 768, 769, 770, 772, 773, 775, 776, 778, 779, 780, 781, 782, 783, 784, 785, 786, 787, 788, 789, 790, 791, 792, 793, 794, 795, 796, 797, 798, 800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 824, 825, 826, 827, 828, 829, 830, 832, 833, 835, 836, 837, 838, 839, 840, 841, 842, 843, 845, 846, 847, 848, 849, 850, 851, 853, 854, 855, 856, 857, 858, 859, 860, 862, 863, 864, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 878, 879, 880, 881, 882, 883, 884, 885, 886, 887, 888, 890, 891, 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 914, 915, 917, 919, 920, 921, 924, 925, 926, 927, 928, 929, 930, 932, 933, 934, 935, 936, 937, 939, 940, 941, 942, 943, 944, 945, 946, 947, 948, 949, 951, 955, 956, 957, 958, 959, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974, 976, 977, 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 989, 990, 991, 992, 993, 994, 995, 996, 997, 998, 999, 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1008, 1009, 1010, 1012, 1013, 1014, 1015, 1016, 1017, 1018, 1020 };

    for (not_there) |bit| {
        try std.testing.expect(!a_set.isSet(bit));
        try std.testing.expect(!b_set.isSet(bit));
    }
}
