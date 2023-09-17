#include "common.h"

#include <stdio.h>
#include <stdarg.h>

s8 *cstrf_impl(void *space, u64 space_len,
		     const s8 *_fmt,
		     const s8 *fmt, ...) {
  va_list args;
  (void) _fmt;
  va_start(args, fmt);
  vsnprintf(space, space_len, fmt, args);
  va_end(args);
  return space;
}
