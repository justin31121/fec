#include "string.h"

#include <ctype.h>
#include <string.h>

string string_from_cstr(const s8 *cstr) {
  return (string) {(u8 *) cstr, strlen(cstr)};
}

bool string_eq(string a, string b) {
  if(a.len != b.len) {
    return false;
  }

  return memcmp(a.data, b.data, a.len) == 0;
}

static inline s32 string_index_of_impl(const s8 *haystack, u64 haystack_size, const s8* needle, u64 needle_size) {
  if(needle_size > haystack_size) {
    return -1;
  }
  haystack_size -= needle_size;
  u64 i, j;
  for(i=0;i<=haystack_size;i++) {
    for(j=0;j<needle_size;j++) {
      if(haystack[i+j] != needle[j]) {
	break;
      }
    }
    if(j == needle_size) {
      return (s32) i;
    }
  }
  return -1;
}

s32 string_index_of(string s, const s8 *needle) {
  const s8 *haystack = (const s8 *) s.data;
  u64 haystack_size = s.len;

  return string_index_of_impl(haystack, haystack_size, needle, strlen(needle));
}

static inline s32 string_last_index_of_impl(const s8 *haystack, u64 haystack_size, const s8* needle, u64 needle_size) {

  if(needle_size > haystack_size) {
    return -1;
  }
  
  s32 i;

  for(i=haystack_size - needle_size - 1;i>=0;i--) {
    u64 j;
    for(j=0;j<needle_size;j++) {
      if(haystack[i+j] != needle[j]) {
	break;
      }
    }
    if(j == needle_size) {
      return i;
    }
  }
  
  return -1;
}

s32 string_last_index_of(string s, const s8 *needle) {
  const s8 *haystack = (const s8 *) s.data;
  size_t haystack_size = s.len;

  return string_last_index_of_impl(haystack, haystack_size, needle, strlen(needle));
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

#define string_substring_impl(s, start, len) (string) { .data = ((s).data + (start)), .len = (len)}

bool string_substring(string s, u64 start, u64 len, string *d) {
  if(start > s.len) {
    return false;
  }

  if(start + len > s.len) {
    return false;
  }

  *d = string_substring_impl(s, start, len);

  return true;
}
