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
  
