#include "string.h"

#include <ctype.h>
#include <string.h>

bool string_eq(string a, string b) {
  if(a.len != b.len) {
    return false;
  }

  return memcmp(a.data, b.data, a.len) == 0;
}

bool string_parse_u64(string s, u64 *out) {
  u64 result = 0;
  u64 i = 0;

  while(i<s.len && isdigit(s.data[i])) {
    result *= 10;
    result += s.data[i] - '0';
    i++;
  }

  *out = result;
  return i==s.len;
}
