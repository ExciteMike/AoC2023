// $env:Path += ";C:\ghcup\ghc\9.2.8\mingw\bin\"
// gcc -Wall -Wextra -std=c99 .\src\day11\main.c -I . -o .\target\day11.exe
// then .\target\day11.exe
#define __STDC_FORMAT_MACROS
#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define MAX_GALAXIES (447)
#define MAX_X (140)
#define MAX_Y (140)

uint64_t distance(
    uint64_t x1,
    uint64_t y1,
    uint64_t x2,
    uint64_t y2
) {
    return ((x2>=x1) ? (x2-x1) : (x1-x2)) + ((y2>=y1) ? (y2-y1) : (y1-y2));
}

uint64_t score(
    uint64_t *galaxy_xs,
    uint64_t *galaxy_ys,
    uint64_t n_galaxies,
    uint64_t *expanded_xs,
    uint64_t n_xs,
    uint64_t *expanded_ys,
    uint64_t n_ys
) {
    uint64_t total = 0;
    for (uint64_t i=0; i<n_galaxies; ++i) {
        assert((galaxy_xs[i] < n_xs) && "column outside bounds");
        assert((galaxy_ys[i] < n_ys) && "row outside bounds");
        uint64_t x1 = expanded_xs[galaxy_xs[i]];
        uint64_t y1 = expanded_ys[galaxy_ys[i]];
        for (uint64_t j=i+1; j<n_galaxies; ++j) {
            assert((galaxy_xs[j] < n_xs) && "column outside bounds");
            assert((galaxy_ys[j] < n_ys) && "row outside bounds");
            uint64_t x2 = expanded_xs[galaxy_xs[j]];
            uint64_t y2 = expanded_ys[galaxy_ys[j]];
            total += distance(x1,y1,x2,y2);
        }
    }
    return total;
}

uint64_t expand_and_score(
    uint64_t *galaxy_xs,
    uint64_t *galaxy_ys,
    uint64_t n_galaxies,
    uint8_t *row_occupied,
    uint64_t n_rows,
    uint8_t *col_occupied,
    uint64_t n_cols,
    uint64_t scale
) {
    uint64_t expanded_xs[MAX_GALAXIES] = {0};
    uint64_t expanded_ys[MAX_GALAXIES] = {0};
    uint64_t expanded_i = 0;
    for (uint64_t i=0; i<n_cols; ++i) {
        expanded_xs[i] = expanded_i;
        if (col_occupied[i]) {
            expanded_i += 1;
        } else {
            expanded_i += scale;
        }
    }
    expanded_i = 0;
    for (uint64_t i=0; i<n_rows; ++i) {
        expanded_ys[i] = expanded_i;
        if (row_occupied[i]) {
            expanded_i += 1;
        } else {
            expanded_i += scale;
        }
    }
    return score(
        galaxy_xs,
        galaxy_ys,
        n_galaxies,
        expanded_xs,
        n_cols,
        expanded_ys,
        n_rows
    );
}

int main() {
    // read data
    FILE *f = fopen("./puzzle_input/day11", "r");
    if (f==NULL) {
        fprintf(stderr, "Error opening input");
        return 1;
    }
    uint64_t x = 0;
    uint64_t y = 0;
    uint64_t galaxy_xs[MAX_GALAXIES] = {0};
    uint64_t galaxy_ys[MAX_GALAXIES] = {0};
    uint64_t n_galaxies = 0;
    uint8_t row_occupied[MAX_Y] = {0};
    uint8_t col_occupied[MAX_X] = {0};
    uint64_t n_rows = 0;
    uint64_t n_cols = 0;
    while (1) {
        int c = fgetc(f);
        if (c == EOF) {
            break;
        } else if (c == '\n') {
            y += 1;
            x = 0;
        } else if (c == '#') {
            if (n_galaxies >= MAX_GALAXIES) {
                fprintf(stderr, "insufficient space for galaxies");
                return 1;
            }
            if (y >= MAX_Y) {
                fprintf(stderr, "insufficient space for rows");
                return 1;
            }
            if (x >= MAX_X) {
                fprintf(stderr, "insufficient space for cols");
                return 1;
            }
            galaxy_xs[n_galaxies] = x;
            galaxy_ys[n_galaxies] = y;
            if (!row_occupied[y]) { row_occupied[y] = 1; }
            if (!col_occupied[x]) { col_occupied[x] = 1; }
            if (y >= n_rows) { n_rows = y + 1; }
            if (x >= n_cols) { n_cols = x + 1; }
            ++n_galaxies;
            x += 1;
        } else if (c == '.') {
            x += 1;
        } else {
            assert(!"unhandled character");
        }
    }
    fclose(f);

    uint64_t p1 = expand_and_score(
        galaxy_xs,
        galaxy_ys,
        n_galaxies,
        row_occupied,
        n_rows,
        col_occupied,
        n_cols,
        2);
    uint64_t p2 = expand_and_score(
        galaxy_xs,
        galaxy_ys,
        n_galaxies,
        row_occupied,
        n_rows,
        col_occupied,
        n_cols,
        1000000);

    // 10292708 790194712336
    printf("%" PRIu64 " %" PRIu64, p1, p2);
}