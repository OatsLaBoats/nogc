#pragma once

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define PTR(T) const T * const
#define  ng_Unit void

typedef int64_t ng_Int;

struct ng_String {
    size_t cap;
    size_t len;
    char* mem;
};

// I probably won't implement this but in a full featured language when we need borrowed string we can just pass a slice and save a bit on stack memory
// This also makes it faster if we have small string optimization since we don't need to check if the starting byte is set! when we clone we don't need the capacity anyways.
struct ng_StringSlice {
    size_t len;
    const char *mem;
};

static inline struct ng_StringSlice ng_sliceString(const struct ng_String str);
static inline struct ng_String ng_cloneString(const struct ng_StringSlice str);
static inline ng_Unit ng_dropString(const struct ng_String str);
static inline ng_Unit ng_printLn(struct ng_StringSlice str);
static inline ng_Int ng_addInt(ng_Int a, ng_Int b);
static inline ng_Int ng_subInt(ng_Int a, ng_Int b);

static inline ng_Int ng_addInt(ng_Int a, ng_Int b) {
    return a + b;
}

static inline ng_Int ng_subInt(ng_Int a, ng_Int b) {
    return a - b;
}

static inline struct ng_StringSlice ng_sliceString(const struct ng_String str) {
    return (struct ng_StringSlice){.len = str.len, .mem = str.mem};
}

static inline struct ng_String ng_cloneString(const struct ng_StringSlice str) {
    char* mem = (char*)malloc(str.len);
    assert(mem != NULL);

    memcpy_s(mem, str.len, str.mem, str.len);
    return (struct ng_String){.cap = str.len, .len = str.len, .mem = mem};
}

static inline struct ng_String ng_appendString(struct ng_String str, struct ng_StringSlice) {

}

static inline ng_Unit ng_dropString(const struct ng_String str) {
    if (str.cap > 0) {

    }
}

// The default bevaviour is that any struct larger than 24 bytes should be passed by pointer
static inline ng_Unit ng_printLn(const struct ng_StringSlice str) {
    fwrite(str.mem, sizeof(*str.mem), str.len, stdout);
}
