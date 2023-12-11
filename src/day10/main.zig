// how i'm running it
//   $env:Path += ";D:\dev\otherpeoplescode\zig-windows-x86_64-0.12.0-dev.1808+69195d0cd"
//   zig run ".\src\day10\main.zig"
const std = @import("std");

const Coord = struct { x: usize, y: usize };
const DijkStackItem = struct { coord: Coord, dist: usize };

pub fn Queue(comptime T: type) type {
    return struct {
        const This = @This();
        const Node = struct { data: T, next: ?*Node };
        start: ?*Node,
        end: ?*Node,

        pub fn init() This {
            return This{
                .start = null,
                .end = null,
            };
        }
        pub fn deinit(this: *This) void {
            while (null != this.start) {
                _ = this.dequeue();
            }
        }
        pub fn enqueue(this: *This, value: T) !void {
            const node = try std.heap.page_allocator.create(Node);
            node.* = .{ .data = value, .next = null };
            if (this.end) |end| end.next = node //
            else this.start = node;
            this.end = node;
        }
        pub fn dequeue(this: *This) ?T {
            const start = this.start orelse return null;
            defer std.heap.page_allocator.destroy(start);
            if (start.next) |next| {
                this.start = next;
            } else {
                this.start = null;
                this.end = null;
            }
            return start.data;
        }
        pub fn is_empty(this: *This) bool {
            return null == this.start;
        }
    };
}

pub fn input(buffer: []u8) ![]u8 {
    var dir = try std.fs.cwd().openDir("puzzle_input", .{});
    defer dir.close();
    return dir.readFile("day10", buffer);
}

pub fn rows(data: []u8, pipe_buf: [][]u8) [][]const u8 {
    var written: usize = 0;
    var start: usize = 0;
    for (data, 0..) |c, i| {
        _ = c;
        if (data[i] == '\n') {
            pipe_buf[written] = data[start..i];
            written += 1;
            start = i + 1;
        }
    }
    pipe_buf[written] = data[start..];
    written += 1;
    return pipe_buf[0..written];
}

pub fn find_start(data: []u8) Coord {
    var x: usize = 0;
    var y: usize = 0;
    for (data) |c| {
        if (c == 'S') {
            break;
        }
        if (c == '\n') {
            y += 1;
            x = 0;
        } else {
            x += 1;
        }
    }
    return .{ .x = x, .y = y };
}

pub fn test_inside(dijkstra_map: std.AutoHashMap(Coord, usize), pipes: [][]const u8, coord: Coord) !bool {
    if (dijkstra_map.contains(coord)) {
        return false;
    }
    const test_slice = pipes[coord.y][0..coord.x];
    var is_inside = false;
    var began_wall_with: u8 = 0;
    for (test_slice, 0..) |pipe, x| {
        const test_coord = .{ .x = x, .y = coord.y };
        if (dijkstra_map.contains(test_coord)) {
            // std.debug.print("{c}", .{pipe});
            switch (pipe) {
                '|' => {
                    is_inside = !is_inside;
                    began_wall_with = 0;
                },
                'F', 'L' => {
                    began_wall_with = pipe;
                },
                'J' => {
                    if (began_wall_with == 'F') {
                        is_inside = !is_inside;
                        began_wall_with = 0;
                    }
                },
                'S', '7' => {
                    if (began_wall_with == 'L') {
                        is_inside = !is_inside;
                        began_wall_with = 0;
                    }
                },
                '-', '.' => {},
                else => {
                    std.debug.print("unhandled character \"{}\"", .{pipe});
                    return error.Oops;
                },
            }
        }
    }
    return is_inside;
}

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var buf: [141 * 141 + 1]u8 = undefined;
    var pipes_buf: [142][]u8 = undefined;
    const input_slice = try input(&buf);
    const pipes = rows(input_slice, &pipes_buf);
    const start = find_start(input_slice);
    var p1: usize = 0;

    // dijkstra map
    var dijkstra_map = std.AutoHashMap(Coord, usize).init(std.heap.page_allocator);
    defer dijkstra_map.deinit();
    var queue = Queue(DijkStackItem).init();
    defer queue.deinit();
    try queue.enqueue(.{ .coord = start, .dist = 0 });
    while (!queue.is_empty()) {
        const q_item = queue.dequeue() orelse return;
        const coord = q_item.coord;
        const dist = q_item.dist;
        const x = coord.x;
        const y = coord.y;
        if (dijkstra_map.contains(coord)) {
            continue;
        }
        if (dist > p1) {
            p1 = dist;
        }
        try dijkstra_map.put(coord, dist);
        const pipe = pipes[y][x];
        switch (pipe) {
            '|' => {
                try queue.enqueue(.{ .coord = .{ .x = x, .y = y - 1 }, .dist = dist + 1 });
                try queue.enqueue(.{ .coord = .{ .x = x, .y = y + 1 }, .dist = dist + 1 });
            },
            '-' => {
                try queue.enqueue(.{ .coord = .{ .x = x - 1, .y = y }, .dist = dist + 1 });
                try queue.enqueue(.{ .coord = .{ .x = x + 1, .y = y }, .dist = dist + 1 });
            },
            'L' => {
                try queue.enqueue(.{ .coord = .{ .x = x, .y = y - 1 }, .dist = dist + 1 });
                try queue.enqueue(.{ .coord = .{ .x = x + 1, .y = y }, .dist = dist + 1 });
            },
            'J' => {
                try queue.enqueue(.{ .coord = .{ .x = x, .y = y - 1 }, .dist = dist + 1 });
                try queue.enqueue(.{ .coord = .{ .x = x - 1, .y = y }, .dist = dist + 1 });
            },
            'S', '7' => {
                try queue.enqueue(.{ .coord = .{ .x = x, .y = y + 1 }, .dist = dist + 1 });
                try queue.enqueue(.{ .coord = .{ .x = x - 1, .y = y }, .dist = dist + 1 });
            },
            'F' => {
                try queue.enqueue(.{ .coord = .{ .x = x, .y = y + 1 }, .dist = dist + 1 });
                try queue.enqueue(.{ .coord = .{ .x = x + 1, .y = y }, .dist = dist + 1 });
            },
            '.' => {},
            else => {
                std.debug.print("unhandled character \"{}\"", .{pipe});
                return error.Oops;
            },
        }
    }

    // count all that are enclosed
    var p2: usize = 0;
    for (pipes, 0..) |row_slice, y| {
        for (row_slice, 0..) |_, x| {
            const coord = .{ .x = x, .y = y };
            if (try test_inside(dijkstra_map, pipes, coord)) {
                p2 += 1;
            }
        }
    }

    try stdout.print("{} {}\n", .{ p1, p2 }); // 6907 541
}
