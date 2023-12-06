// gcc -Wall -Wextra -std=c99 .\src\day05\main.c -I . -o .\target\day05.exe
// then .\target\day05.exe
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "src/shared/lib.c"
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#define MAX_SEEDS (20)
#define MAX_STEPS (7)
#define MAX_STEP_MAPPINGS (64)

typedef struct Mapping {
    int64_t dst;
    int64_t src;
    int64_t range;
} Mapping;

typedef struct ConversionStep {
    size_t n;
    Mapping ms[MAX_STEP_MAPPINGS];
} ConversionStep;

size_t read_seeds(FILE *f, int64_t *buf, size_t n) {
    size_t i = 0;
    fscanf(f, "seeds: ");
    while (1) {
        int64_t temp;
        switch (fscanf(f, " %" SCNi64 " ", &temp)) {
        case 0:
        case EOF:
            return i;
        case 1:
            if (i >= n) {
                PANIC("insufficient space");
            }
            *(buf + (i++)) = temp;
            break;
        default:
            assert(!"unhandled case");
        }
    }
    return i;
}

size_t read_conversion_step(FILE *f, ConversionStep *dst, size_t max_steps) {
    dst->n = 0;
    // ignore first line
    skip_to_eol(f);
    while (1) {
        Mapping m;
        switch (fscanf(f, " %" SCNi64 " %" SCNi64 " %" SCNi64 " ", &m.dst, &m.src, &m.range)) {
        case 0:
        case EOF:
            if (dst->n) {
                return 1;
            }
            return 0;
        case 3:
            if (dst->n >= max_steps) {
                PANIC("insufficient space");
            }
            *(dst->ms + dst->n)= m;
            ++dst->n;
            break;
        default:
            assert(!"unhandled case");
        }
    }
}

size_t read_conversion_steps(FILE *f, ConversionStep *buf, size_t n) {
    size_t i = 0;
    while (1) {
        ConversionStep temp;
        switch (read_conversion_step(f, &temp, MAX_STEP_MAPPINGS))
        {
        case 0:
        case EOF:
            return i;
        case 1:
            if (i >= n)
            {
                PANIC("insufficient space");
            }
            *(buf + (i++)) = temp;
            break;
        default:
            PANIC("unhandled case");
        }
    }
}

int64_t map_seed(
    const Mapping *mappings,
    size_t n_mappings,
    int64_t seed
) {
    for (size_t ms_i=0; ms_i<n_mappings; ++ms_i) {
        const Mapping *m = mappings + ms_i;
        if ((m->src <= seed) && (seed <= m->src + m->range)) {
            seed += m->dst - m->src;
            break;
        }
    }
    return seed;
}

void map_seeds(
    const Mapping *mappings,
    size_t n_mappings,
    int64_t* seeds,
    size_t n_seeds
) {
    for (size_t seed_i=0; seed_i<n_seeds; ++seed_i) {
        seeds[seed_i] = map_seed(mappings, n_mappings, seeds[seed_i]);
    }
}

int64_t p1(int64_t* seeds, size_t n_seeds, const ConversionStep* steps, size_t n_steps) {
    if ((n_seeds <= 0)||(n_steps<=0)) { return 0; }
    if (n_seeds > MAX_SEEDS) { PANIC("insufficient space"); }
    int64_t buf[MAX_SEEDS] = { 0 };
    memcpy(buf, seeds, n_seeds * sizeof(seeds[0]));
    for (size_t i=0; i<n_steps; ++i) {
        const ConversionStep *step = steps + i;
        size_t n_mappings = step->n;
        const Mapping *mappings = step->ms;
        map_seeds(mappings, n_mappings, buf, n_seeds);
    }
    int64_t min = buf[0];
    for (size_t i=1; i<n_seeds; ++i) {
        if (buf[i] < min) {
            min = buf[i];
        }
    }
    return min;
}

typedef struct SeedRange {
    int64_t start;
    int64_t range;
} SeedRange;

size_t convert_range(
    const Mapping *mappings,
    size_t n_mappings,
    const SeedRange range,
    SeedRange * outputs,
    size_t max_outputs
) {
    int64_t lo = range.start;
    int64_t hi = lo + range.range - 1;
    size_t n_written = 0;
    #define WRITE(START, RANGE) { \
        ASSERT(n_written < max_outputs, "insufficient space"); \
        (outputs + n_written)->start = (START); \
        (outputs + n_written)->range = (RANGE); \
        ++n_written; \
    }
    #define RECURSE(START, RANGE) { \
        SeedRange RECURSE = {(START), (RANGE)}; \
        n_written += convert_range( \
            mappings, \
            n_mappings, \
            RECURSE, \
            outputs + n_written, \
            max_outputs - n_written); \
    }
    for (size_t ms_i=0; ms_i<n_mappings; ++ms_i) {
        const Mapping *m = mappings + ms_i;
        int64_t src_end = m->src + m->range - 1;
        int64_t offset = m->dst - m->src;

        // range is fully outside the mapping
        if ((hi < m->src) || (lo > src_end)) {
            continue;
        }

        // range is fully within the mapping
        if ((m->src <= lo) && (hi <= src_end)) {
            WRITE(lo + offset, range.range);
            break;
        }

        // overlaps into src range from below...
        if (lo < m->src) {
            // ..and ends within
            if (hi <= src_end) {
printf("case @ line %i\n", __LINE__); abort();
                RECURSE(lo, m->src - lo + 1);
                break;
            }

            // ...and extends beyond
printf("case @ line %i\n", __LINE__); abort();
            RECURSE(lo, m->src - lo + 1);
            WRITE(m->dst, src_end + offset);
            RECURSE(src_end + 1, hi - src_end);
            break;
        }

        // overlaps into src range from above (and began within)
        if (hi > src_end) {
printf("case @ line %i\n", __LINE__);
printf("  input= %" PRIi64 ", %" PRIi64 "\n", range.start, range.range);
printf("  mapping dst= %" PRIi64 ", src=%" PRIi64 ", range=%" PRIi64 "\n", m->dst, m->src, m->range);
printf("  offset = %" PRIi64 "\n", offset);
printf("  %" PRIi64 ", %" PRIi64 "\n", lo + offset, src_end - lo + 1);
printf("  %" PRIi64 ", %" PRIi64 "\n", src_end + 1, hi - src_end);
            WRITE(lo + offset, src_end - lo + 1);
            RECURSE(src_end + 1, hi - src_end);
            break;
        }

        PANIC("unreachable");
    }
    // if it was never split above, just pass it through
    if (n_written == 0) {
        WRITE(range.start, range.range);
    }
    #undef WRITE
    return n_written;
}

size_t convert_ranges(
    const Mapping *mappings,
    size_t n_mappings,
    const SeedRange * inputs,
    size_t n_inputs,
    SeedRange * outputs,
    size_t max_outputs
) {
    const SeedRange * read_head = inputs;
    const SeedRange * read_end = inputs + n_inputs;
    size_t n_written = 0;

    while (read_head < read_end) {
       n_written += convert_range(mappings, n_mappings, *read_head, outputs + n_written, max_outputs - n_written);
       ++read_head;
    }
    return n_written;
}

void print_ranges(const SeedRange *ranges, size_t n_ranges) {
    printf("%" PRIu64 " ranges:\n", (uint64_t)n_ranges);
    for (size_t i=0; i<n_ranges; ++i) {
        printf("  %" PRIi64 " %" PRIi64 "\n",
               (ranges + i)->start,
               (ranges + i)->range);
    }
}

int64_t p2(const SeedRange *ranges, size_t n_ranges, const ConversionStep* steps, size_t n_steps) {
    if ((n_ranges <= 0)||(n_steps <= 0)) { return 0; }
    #define BUF_SIZE (4)
    if (n_ranges > BUF_SIZE) { PANIC("insufficient space"); }
    SeedRange buf_a[BUF_SIZE] = {0};
    memcpy(buf_a, ranges, n_ranges * sizeof(ranges[0]));
    SeedRange buf_b[BUF_SIZE] = {0};
    SeedRange *read_buf = buf_a;
    SeedRange *write_buf = buf_b;
    size_t n_written = n_ranges;
    print_ranges(read_buf, n_written);
    for (size_t i=0; i<n_steps; ++i) {
        const ConversionStep *step = steps + i;
        size_t n_mappings = step->n;
        const Mapping *mappings = step->ms;
        n_written = convert_ranges(mappings, n_mappings, read_buf, n_written, write_buf, BUF_SIZE);
        SWAP(read_buf, write_buf, SeedRange*)
print_ranges(read_buf, n_written);
    }

    int64_t min = read_buf->start;
    for (size_t i=1; i<n_written; ++i) {
        int64_t x = (read_buf + i)->start;
        if (x < min) {
            min = x;
        }
    }
    return min;
}

int main() {
    FILE *f = fopen("./puzzle_input/day05example", "r");
    ASSERT(f != NULL, "Error opening input");
    int64_t seeds[MAX_SEEDS] = {0};
    size_t n_seeds = read_seeds(f, seeds, MAX_SEEDS);
    ConversionStep conversion_steps[MAX_STEPS] = {0};
    size_t n_steps = read_conversion_steps(f, conversion_steps, MAX_STEPS);
    printf("%" PRIu64 "\n", p1(seeds, n_seeds, conversion_steps, n_steps)); // 525792406
    SeedRange *ranges = (SeedRange*)seeds;
    size_t n_ranges = n_seeds >> 1;
    printf("%" PRIu64 "\n", p2(ranges, n_ranges, conversion_steps, n_steps)); // 79004094
    return 0;
}
