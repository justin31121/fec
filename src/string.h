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

bool string_eq(string a, string b);
bool string_parse_u64(string s, u64 *out);

typedef struct{
  u8 *items;
  u64 len;
  u64 cap;
}string_builder;

#endif // STRING_H
