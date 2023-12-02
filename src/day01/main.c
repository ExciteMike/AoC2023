#include <stdio.h>
#include <stdint.h>
#include <string.h>

int main() {
    FILE *f = fopen("./puzzle_input/day01", "r");
    if (f==NULL) {
        printf("Error opening input");
        return 1;
    }
    uint32_t p1first = 0;
    uint32_t p1last = 0;
    uint32_t p1total = 0;
    uint32_t p2first = 0;
    uint32_t p2last = 0;
    uint32_t p2total = 0;
    char buf[23000] = {0};
    char *read_head = buf;
    int num_read = fread(&buf, 1, sizeof(buf), f);
    char *end = buf + num_read;
    while (read_head < end) {
        if (*read_head == '\n') {
            p1total += p1first * 10 + p1last;
            p2total += p2first * 10 + p2last;
            p1first = p1last = p2first = p2last = 0;
            ++read_head;
        } else if (('1' <= *read_head) && (*read_head <= '9')) {
            p1last = p2last = *read_head - '0';
            ++read_head;
        } else if (0 == strncmp(read_head, "one", 3)) {
            p2last = 1;
            read_head += 3;
        } else if (0 == strncmp(read_head, "two", 3)) {
            p2last = 2;
            read_head += 3;
        } else if (0 == strncmp(read_head, "three", 5)) {
            p2last = 3;
            read_head += 5;
        } else if (0 == strncmp(read_head, "four", 4)) {
            p2last = 4;
            read_head += 4;
        } else if (0 == strncmp(read_head, "five", 4)) {
            p2last = 5;
            read_head += 4;
        } else if (0 == strncmp(read_head, "six", 3)) {
            p2last = 6;
            read_head += 3;
        } else if (0 == strncmp(read_head, "seven", 5)) {
            p2last = 7;
            read_head += 5;
        } else if (0 == strncmp(read_head, "eight", 5)) {
            p2last = 8;
            read_head += 5;
        } else if (0 == strncmp(read_head, "nine", 4)) {
            p2last = 9;
            read_head += 4;
        } else {
            ++read_head;
        }
        if (p1first == 0) {
            p1first = p1last;
        }
        if (p2first == 0) {
            p2first = p2last;
        }
    }
    p1total += p1first * 10 + p1last;
    p2total += p2first * 10 + p2last;
    printf("part 1: %d\n", p1total);
    printf("part 2: %d\n", p2total);
    return 0;
}