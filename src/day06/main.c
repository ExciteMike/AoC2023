// $env:Path += ";path/to/gcc"
// gcc -Wall -Wextra -std=c99 .\src\day06\main.c -I . -o .\target\day06.exe
// .\target\day06.exe
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define PANIC(s) {                                          \
        fprintf(stderr, s ": %s:%d\n", __FILE__, __LINE__); \
        abort();                                            \
    }
#define ASSERT(cond, s) {                                       \
        if (!(cond)) {                                          \
            fprintf(stderr, s ": %s:%d\n", __FILE__, __LINE__); \
            abort();                                            \
        }                                                       \
    }

int64_t count_ways(int64_t time, int64_t dist) {
    double x = sqrt((double)(time*time - 4 * dist));
    int64_t lo = (int64_t)ceil(0.5 * ((double)time - x));
    int64_t hi = (int64_t)(0.5 * ((double)time + x));
    if (lo * (time - lo) == dist) {
        lo += 1;
    }
    if (hi * (time - hi) == dist) {
        hi -= 1;
    }
    return 1 + hi - lo;
}

int64_t next_highest_power_of_10(int64_t n) {
    if (n < 1) return 1;
    if (n < 10) return 10;
    if (n < 100) return 100;
    if (n < 1000) return 1000;
    if (n < 10000) return 10000;
    PANIC("unhandled case");
    return 10;
}
/// @brief read whitespace-delimited 64-bit ints
/// @param f file to read from
/// @param buf where to store them, with room for four of them
/// @return how many were read
int64_t read_4_i64s(FILE *f, int64_t *buf) {
    return fscanf(
        f,
        " %" SCNi64 " %" SCNi64 " %" SCNi64 " %" SCNi64,
        buf,
        buf + 1,
        buf + 2,
        buf + 3);
}


int main() {
    FILE *f = fopen("./puzzle_input/day06", "r");
    ASSERT(f != NULL, "Error opening input");
    fscanf(f, "Time: ");
    int64_t times[4] = {0};
    int64_t n_times = read_4_i64s(f, times);
    fscanf(f, " Distance: ");
    int64_t dists[4] = {0};
    int64_t n_dists = read_4_i64s(f, dists);
    ASSERT(n_times == n_dists, "bad input");
    int64_t p1 = 1;
    int64_t p2_time = 0;
    int64_t p2_dist = 0;
    for (int64_t i=0; i<n_times; ++i) {
        int64_t t = times[i];
        int64_t d = dists[i];
        p1 *= count_ways(t, d);
        p2_time = p2_time * next_highest_power_of_10(t) + t;
        p2_dist = p2_dist * next_highest_power_of_10(d) + d;
    }
    int64_t p2 = count_ways(p2_time, p2_dist);
    printf("%" PRIi64 "\n%" PRIi64 "\n", p1, p2); // 252000 36992486
    return 0;
}