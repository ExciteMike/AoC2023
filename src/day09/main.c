// I don't have gcc in a reasonable place or in my path at the moment so do this:
// $env:Path += ";D:\dev\otherpeoplescode\mingw-w64\i686-8.1.0-posix-dwarf-rt_v6-rev0\mingw32\bin"
// gcc -Wall -Wextra -std=c99 .\src\day09\main.c -I . -o .\target\day09.exe
// .\target\day09.exe
#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX_HISTORY (21)

int all_zero(int64_t *data, size_t n) {
    for (size_t i=0; i<n; ++i) {
        if (0 != data[i]) {
            return 0;
        }
    }
    return 1;
}

typedef struct Pair {
    int64_t fwd;
    int64_t back;
} Pair;

Pair extrapolate(int64_t *data, size_t n) {
    if (all_zero(data, n)) {
        Pair p = {0, 0};
        return p;
    }
    int64_t deltas[MAX_HISTORY] = {0};
    for (size_t i=0; i<n-1; ++i) {
        deltas[i] = data[i+1] - data[i];
    }
    Pair p = extrapolate(deltas, n-1);
    p.fwd = data[n-1] + p.fwd;
    p.back = data[0] - p.back;
    return p;
}

int main() {
    int c;
    FILE *f = fopen("./puzzle_input/day09", "r");

    int64_t history[MAX_HISTORY] = {0};
    size_t history_len = 0;
    int64_t p1 = 0;
    int64_t p2 = 0;

    while ((c = fgetc(f))) {
        if (isdigit(c) || (c=='-')) {
            assert((history_len < MAX_HISTORY) && "insufficient space");
            ungetc(c, f);
            fscanf(f, "%" SCNi64, &history[history_len++]);
        } else if (c == '\n') {
            Pair p = extrapolate(history, history_len);
            p1 += p.fwd;
            p2 += p.back;
            history_len = 0;
        } else if (EOF == c) {
            break;
        }
    }
    printf("%" PRIi64 "\n%" PRIi64 "\n", p1, p2); // 1884768153 1031

    return 0;
}