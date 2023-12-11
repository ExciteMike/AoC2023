// how i'm running it
//   $env:Path += ";D:\dev\otherpeoplescode\zig-windows-x86_64-0.12.0-dev.1808+69195d0cd"
//   zig run ".\src\day09\main.zig"
const std = @import("std");

const Pair: type = struct { fwd: i64, back: i64 };

fn input(buffer: []u8) ![]u8 {
    var dir = try std.fs.cwd().openDir("puzzle_input", .{});
    defer dir.close();
    return dir.readFile("day09", buffer);
}

fn extrapolate(history: []i64) Pair {
    const all_zero = for (history) |x| {
        if (x != 0) {
            break false;
        }
    } else true;
    if (all_zero) {
        return .{ .fwd = 0, .back = 0 };
    }
    var deltas: [20]i64 = undefined;
    var n_deltas: usize = 0;
    for (history[0 .. history.len - 1], history[1..]) |a, b| {
        deltas[n_deltas] = b - a;
        n_deltas += 1;
    }
    const p = extrapolate(deltas[0..n_deltas]);
    return .{ .fwd = history[history.len - 1] + p.fwd, .back = history[0] - p.back };
}

pub fn main() !void {
    var input_buffer: [25 * 1024]u8 = undefined;
    var history_buffer: [21]i64 = undefined;
    var p1: i64 = 0;
    var p2: i64 = 0;
    var lines = std.mem.splitScalar(u8, try input(&input_buffer), '\n');
    while (lines.next()) |line| {
        var hist_len: usize = 0;
        var numbers = std.mem.splitScalar(u8, line, ' ');
        while (numbers.next()) |x| {
            if (x.len == 0) {
                continue;
            }
            history_buffer[hist_len] = try std.fmt.parseInt(i64, x, 10);
            hist_len += 1;
        }
        const pair = extrapolate(history_buffer[0..hist_len]);
        p1 += pair.fwd;
        p2 += pair.back;
    }

    // 1884768153 1031
    try std.io.getStdOut().writer().print("{} {}\n", .{ p1, p2 });
}
