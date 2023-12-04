#include <assert.h>
#include <stdio.h>

#define MAX_CARDS (219)
#define MAX_WINNING (10)

int find(int value, int *set, int set_len) {
    for (int i=0; i<set_len; ++i) {
        if (value == *(set+i)) {
            return i;
        }
    }
    return -1;
}

int main() {
    FILE *f = fopen("./puzzle_input/day04", "r");
    if (f==NULL) { return 1; }
    long long p1 = 0, p2 = 0;
    long long extra_copies[MAX_CARDS] = {0};
    for (int i = 0, value = 0; 0 < fscanf(f, "Card %i: ", &value); ++i) {
        // read winning numbers
        int winning[MAX_WINNING] = {0};
        int n_winning = 0;
        while (0 < fscanf(f, "%lli", &value)) {
            assert(n_winning<MAX_WINNING);
            winning[n_winning++] = value;
        }

        fscanf(f, "|", &value);

        // match the numbers you have against them
        int matches = 0;
        long long card_value = 0;
        while (0 < fscanf(f, "%lli", &value)) {
            if (-1 != find(value, winning, n_winning)) {
                ++matches;
                if (card_value == 0) {
                    ++card_value;
                } else {
                    card_value <<= 1;
                }
            }
        }

        // add copies
        long long n = 1 + extra_copies[i];
        for (int j=i+1; j<i+1+matches; ++j) {
            assert(j<MAX_CARDS);
            extra_copies[j] += n;
        }

        p1 += card_value;
        assert(i<MAX_CARDS);
        p2 += 1+extra_copies[i];
    }
    printf("%lli\n", p1); // 33950
    printf("%lli", p2); // 14814534

    return 0;
}