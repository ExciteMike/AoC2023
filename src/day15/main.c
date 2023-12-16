// $env:Path += ";path/to/gcc"
// gcc -Wall -Wextra -std=c99 .\src\day15\main.c -I . -o .\target\day15.exe
// then .\target\day15.exe
#include "src/shared/lib.c"

typedef struct Bu {
    uint8_t *data;
    size_t num_items;
} Bu;

typedef struct HaTa {
    Bu buckets[256];
    size_t max_items_per_bucket;
    size_t key_size;
    size_t value_size;
} HaTa;

HaTa* new_hata(
    size_t max_items_per_bucket,
    size_t key_size,
    size_t value_size
) {
    HaTa *hata = malloc(sizeof(HaTa));
    memset(hata, 0, sizeof(HaTa));
    hata->max_items_per_bucket = max_items_per_bucket;
    hata->key_size = key_size;
    hata->value_size = value_size;
    size_t space = max_items_per_bucket * (key_size + value_size);
    for (int i=0; i<256; ++i) {
        hata->buckets[i].data = malloc(space),
        memset(hata->buckets[i].data, 0, space);
    }
    return hata;
}

size_t hash(uint8_t *bytes, size_t n) {
    size_t x = 0;
    for (size_t i=0; i<n; ++i) {
        if (bytes[i]) {
            x += bytes[i];
            x *= 17;
            x %= 256;
        }
    }
    return x;
}

void hata_insert(HaTa *hata, uint8_t *in_key, uint8_t *value_src) {
    size_t h = hash(in_key, hata->key_size);
    Bu *bucket = &hata->buckets[h];

    // replace?
    size_t pair_size = (hata->key_size + hata->value_size);
    for (size_t i=0; i<bucket->num_items; ++i) {
        uint8_t *key_addr = bucket->data + (i * pair_size);
        if (0 == memcmp(in_key, key_addr, hata->key_size)) {
            uint8_t* value_addr = key_addr + hata->key_size;
            memcpy(value_addr, value_src, hata->value_size);
            return;
        }
    }

    // add
    assert((bucket->num_items < hata->max_items_per_bucket) && "bucket not large enough");
    uint8_t *key_dst = bucket->data + (bucket->num_items * pair_size);
    memcpy(key_dst, in_key, hata->key_size);
    uint8_t *value_dst = key_dst + hata->key_size;
    memcpy(value_dst, value_src, hata->value_size);
    bucket->num_items++;
}

void hata_remove(HaTa *hata, uint8_t *key) {
    size_t h = hash(key, hata->key_size);
    Bu *bucket = &hata->buckets[h];
    
    size_t pair_size = (hata->key_size + hata->value_size);
    for (size_t i=0; i<bucket->num_items; ++i) {
        uint8_t *key_addr = bucket->data + (i * pair_size);
        if (0 == memcmp(key, key_addr, hata->key_size)) {
            size_t remaining_space = (bucket->num_items - i) * pair_size;
            memmove(key_addr, key_addr+pair_size, remaining_space);
            bucket->num_items--;
            return;
        }
    }
}

typedef char Key[8];
typedef size_t Value;

size_t hata_score(HaTa *hata) {
    size_t total = 0;
    size_t pair_size = (hata->key_size + hata->value_size);
    for (size_t bu_i=0; bu_i<256; ++bu_i) {
        size_t bucket_score = 0;
        Bu *bu = &hata->buckets[bu_i];
        if (bu->num_items) {
            for (size_t item_i=0; item_i<bu->num_items; ++item_i) {
                uint8_t *ptr = bu->data + hata->key_size + item_i * pair_size;
                Value * vptr = (Value*)ptr;
                bucket_score += (1+item_i) * (*vptr);
            }
            total += (1+bu_i) * bucket_score;
        }
    }
    return total;
}

size_t p1() {
    FILE *f = fopen("./puzzle_input/day15", "r");
    size_t hash = 0;
    size_t total = 0;
    while (1) {
        int c;
        switch (c = fgetc(f)) {
            case ',':
                {
                    total += hash;
                    hash = 0;
                }
                break;
            case EOF:
                {
                    total += hash;
                    return total;
                }
                break;
            case '\r':
            case '\n':
                break;
            default:
                hash += c;
                hash *= 17;
                hash %= 256;
                break;
        }
    }
    fclose(f);
}

void dump(HaTa *hata) {
    size_t pair_size = sizeof(Key) + sizeof(Value);
    for (int i=0; i<256; ++i) {
        Bu *bu = &hata->buckets[i];
        if (bu->num_items) {
            fprintf(stderr, "Box %i: ", i);
            for (size_t j=0; j<bu->num_items; ++j) {
                uint8_t *p = bu->data + j*pair_size;
                Key * k = (Key*)p;
                Value * v = (Value*)(p + sizeof(Key));
                fprintf(stderr, " [%s, %" PRIu64 "]", *k, *v);
            }
            fprintf(stderr, "\n");
        }
    }
    fprintf(stderr, "\n");
}

size_t p2() {
    FILE *f = fopen("./puzzle_input/day15", "r");
    HaTa *hata = new_hata(16, sizeof(Key), sizeof(Value));
    char key[8] = {0};
    size_t key_i = 0;
    int c;
    while (EOF != (c = fgetc(f))) {
        switch (c) {
            case ',':
                break;
            case '=':
                {
                    Value value = 0;
                    fscanf(f, "%" SCNu64, &value);
                    hata_insert(hata, (uint8_t*)key, (uint8_t*)&value);
                    key_i = 0;
                    memset(key, 0, sizeof(key));
                    break;
                }
            case '-':
            {
                hata_remove(hata, (uint8_t*)key);
                key_i = 0;
                memset(key, 0, sizeof(key));
                break;
            }
            default:
                if (isalpha(c)) {
                    assert((key_i < sizeof(key)) && "insufficient space for key");
                    key[key_i++] = c;
                } else if (isspace(c)) {
                    // ignore
                } else {
                    PANIC("unhandled case");
                }
                break;
        }
    }
    fclose(f);
    return hata_score(hata);
}

int main() {
    printf("%" PRIu64 "\n%" PRIu64 "\n", p1(), p2()); // 506869 271384
}