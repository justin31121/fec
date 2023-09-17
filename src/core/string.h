#ifndef STRING_H
#define STRING_H

#include "common.h"

typedef struct{
  u8 *data;
  u64 len;
}string;

#define str_fmt "%.*s"
#define str_arg(s) (s32) (s).len, (s8 *) (s).data

#define string_from(data, len) (string) {(data), (len)}

#define string_to_cstr(s) \
  memcpy((u8 *) memcpy( (u8 *) _alloca((s).len +1) + (s).len, "\0", 1) - (s.len), (s).data, (s).len)
string string_from_cstr(const s8 *cstr);

bool string_eq(string a, string b);

s32 string_index_of(string s, const s8 *needle);
s32 string_last_index_of(string s, const s8 *needle);
bool string_substring(string s, u64 start, u64 len, string *d);

bool string_parse_u64(string s, u64 *out);

typedef struct{
  u8 *items;
  u64 len;
  u64 cap;
}string_builder;

#define string_builder_append(sb, xs, xs_len) da_append_many((sb), (xs), (xs_len))
#define string_builder_append_cstr(sb, cstr) da_append_many((sb), (cstr), strlen((cstr)))

#endif // STRING_H
