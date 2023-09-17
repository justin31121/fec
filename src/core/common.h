#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef uint8_t u8;
typedef char s8;
typedef uint16_t u16;
typedef int16_t s16;
typedef uint32_t u32;
typedef int32_t s32;
typedef uint64_t u64;
typedef int64_t s64;

typedef float f32;
typedef double f64;

#define return_defer(n) do{ result = (n); goto defer; }while(0)

#define da_append_many(n, xs, xs_len) do{				\
    u64 new_cap = (n)->cap;						\
    while((n)->len + xs_len >= new_cap) {				\
      new_cap *= 2;							\
      if(new_cap == 0) new_cap = 16;					\
    }									\
    if(new_cap != (n)->cap) {						\
      (n)->cap = new_cap;						\
      (n)->items = realloc((n)->items, (n)->cap * sizeof(*((n)->items))); \
      assert((n)->items);						\
    }									\
    memcpy((n)->items + (n)->len, xs, xs_len * sizeof(*((n)->items)));	\
    (n)->len += xs_len;							\
  }while(0)
    

#define da_append(n, x)	do{						\
    u64 new_cap = (n)->cap;						\
    while((n)->len >= new_cap) {					\
      new_cap *= 2;							\
      if(new_cap == 0) new_cap = 16;					\
    }									\
    if(new_cap != (n)->cap) {						\
      (n)->cap = new_cap;						\
      (n)->items = realloc((n)->items, (n)->cap * sizeof(*((n)->items))); \
      assert((n)->items);						\
    }									\
    (n)->items[(n)->len++] = x;						\
  }while(0)

s8 *cstrf_impl(void *space, u64 space_len, const s8 *_fmt, const s8 *fmt, ...);
#define cstrf_len(n) _alloca((n)), (n)
#define cstrf(...) cstrf_impl( cstrf_len(snprintf(NULL, 0, __VA_ARGS__)+1) , "", __VA_ARGS__)

#define bufsnprintf(buf, ...) assert( snprintf((buf), sizeof((buf)), __VA_ARGS__) < (s32) sizeof((buf)))
