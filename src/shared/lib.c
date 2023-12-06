#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define DEF_VEC(NAME, T)                                              \
    typedef struct {                                                  \
        T *data;                                                      \
        size_t len;                                                   \
        size_t cap;                                                   \
        size_t item_size;                                             \
    } NAME;                                                           \
    NAME NAME##_new(size_t cap) {                                     \
        if (cap < 2) {                                                \
            cap = 8;                                                  \
        }                                                             \
        size_t item_size = sizeof(T);                                 \
        void *data = malloc(item_size * cap);                         \
        NAME v = { data, 0, cap, item_size };                         \
        return v;                                                     \
    }                                                                 \
    void NAME##_grow(NAME *v) {                                       \
        size_t at_least = v->cap + 1;                                 \
        v->cap = at_least + (at_least >> 3) + (at_least < 9 ? 3 : 6); \
        v->data = (T *)realloc(v->data, v->item_size * v->cap);       \
        assert(v->data);                                              \
    }                                                                 \
    void NAME##_push(NAME *vec, T value) {                            \
        if (vec->len >= vec->cap) {                                   \
            NAME##_grow(vec);                                         \
        }                                                             \
        *(vec->data + (vec->len++)) = value;                          \
    }                                                                 \
    void NAME##_print(const NAME *v, void (*print_item)(const T *)) { \
        printf(#NAME "[");                                            \
        for (size_t i = 0; i < v->len; ++i) {                         \
            print_item(v->data + i);                                  \
            if (i > 0) {                                              \
                printf("");                                           \
            }                                                         \
        }                                                             \
        printf("]");                                                  \
    }

#define PANIC(s) {                                          \
        fprintf(stderr, s ": %s:%d\n", __FILE__, __LINE__); \
        abort();                                            \
    }
#define ASSERT(cond, s) {                                       \
        if (!(cond)) {                                            \
            fprintf(stderr, s ": %s:%d\n", __FILE__, __LINE__); \
            abort();                                            \
        }                                                       \
    }

#define SWAP(VALUE1, VALUE2, TYPE) {  \
        TYPE SWAP = VALUE1;           \
        VALUE1 = VALUE2;              \
        VALUE2 = SWAP;                \
    }

typedef int (*CharPred)(int);

void skip_pred(FILE *f, CharPred pred) {
    char c;
    while ((c = getc(f))) {
        if (!pred(c)) {
            ungetc(c, f);
            break;
        }
    }
}
void skip_until(FILE *f, char end_c) {
    char c;
    while ((c = getc(f))) {
        if (c == EOF) {
            break;
        }
        if (c == end_c) {
            ungetc(c, f);
            break;
        }
    }
}
void skip_word(FILE *f) { skip_pred(f, isalnum); }
void skip_punct(FILE *f) { skip_pred(f, ispunct); }
void skip_space(FILE *f) { skip_pred(f, isspace); }
void skip_to_eol(FILE *f) { skip_until(f, '\n'); }
int64_t read_i64(FILE *f) {
    int is_neg = 0;
    int64_t value = 0;
    char c;
    if ((c = getc(f))) {
        if (c == '-') {
            is_neg = 1;
        } else {
            ungetc(c, f);
            value = c - 0x30;
        }
    } else {
        assert(0);
    }
    while ((c = getc(f))) {
        if (isdigit(c)) {
            value = value * 10 + c - 0x30;
        } else {
            ungetc(c, f);
            break;
        }
    }
    return is_neg ? -value : value;
}