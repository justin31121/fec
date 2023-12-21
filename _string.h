#ifndef STRING_H
#define STRING_H

typedef unsigned char string_u8;
typedef int string_s32;
typedef unsigned int string_u32;
typedef long long int string_s64;
typedef unsigned long long int string_u64;
typedef double string_f64;

#define  u8 string_u8
#define s32 string_s32
#define u32 string_u32
#define s64 string_s64
#define u64 string_u64
#define f64 string_f64

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#ifndef STRING_DEF
#  define STRING_DEF static inline
#endif // STRING_DEF

#ifndef STRING_ASSERT
#  define STRING_ASSERT
#endif // STRING_ASSERT

typedef struct{
  const char *data;
  u64 len;
}string;

#define str_fmt "%.*s"
#define str_arg(s) (int) (s).len, (s).data

#define string_from(d, l) (string) { (d), (l)}
#define string_from_cstr(cstr) (string) { (cstr), strlen(cstr) }
#define string_from_cstr2(cstr, cstr_len) (string) { (cstr), (cstr_len) }
#define string_to_cstr(s) ((char *) (memcpy(memset(_alloca((s).len + 1), 0, s.len + 1), (s).data, (s).len)) )
string string_from_u64(u64 n);
string stringf(const char *fmt, ...);

#define string_free(s) free((char *) (s).data)

STRING_DEF bool string_copy(string s, string *d);
STRING_DEF bool string_copy_cstr(const char *cstr, string *d);
STRING_DEF bool string_copy_cstr2(const char *cstr, u64 cstr_len, string *d);

STRING_DEF int string_compare(const void *a, const void *b);
STRING_DEF bool string_eq(string s, string d);
STRING_DEF bool string_eq_cstr(string s, const char *cstr);
STRING_DEF bool string_eq_cstr2(string s, const char *cstr, u64 cstr_len);

STRING_DEF s32 string_index_of(string s, string needle);
STRING_DEF s32 string_index_of_off(string s, u64 off, string needle);
STRING_DEF s32 string_last_index_of(string s, string needle);
STRING_DEF s32 string_index_ofc(string s, const char *needle);
STRING_DEF s32 string_index_of_offc(string s, u64 off, const char *needle);
STRING_DEF s32 string_last_index_ofc(string s, const char *needle);

STRING_DEF bool string_contains(string s, const char *needle);
STRING_DEF bool string_substring(string s, u64 start, u64 len, string *d);

STRING_DEF bool string_chop_by(string *s, const char *delim, string *d);
STRING_DEF bool string_chop_by_pred(string *s, bool (*predicate)(char x), string *d);
STRING_DEF bool string_chop_left(string *s, u64 n);
STRING_DEF bool string_chop_right(string *s, u64 n);

STRING_DEF bool string_parse_s64(string s, s64 *n);
STRING_DEF bool string_parse_f64(string s, f64 *n);

typedef struct{
  u64 off;
}cstr_view;

#define cstr_v_arg(cstr, sb) (sb).data + (cstr).off

typedef struct{
  u64 off;
  u64 len;
}string_view;

#define str_v_arg(v, sb) (int) (v).len, (sb).data + (v).off

typedef bool (*string_builder_map)(const char *input, u64 input_size,
				   char *buffer, u64 buffer_size,
				   u64 *output_size);

// e.g. 'json' -> 'utf8'
STRING_DEF bool string_unescape(const char *input, u64 input_size,
				char *buffer, u64 buffer_size,
				u64 *output_size);
// eg. 'utf-8' -> 'json'
STRING_DEF bool string_escape(const char *input, u64 input_size,
			      char *buffer, u64 buffer_size,
			      u64 *out_output_size);

#define STRING_BUILDER_DEFAULT_CAP 256

typedef struct{
  char *data;
  u64 len;
  u64 cap;
  
  u64 last;
}string_builder;

#define sb_fmt "{ data: %p, len: %llu, cap: %llu, last: %llu }"
#define sb_arg(sb) (void *) (sb).data, (sb).len, (sb).cap, ((sb).last)

#define string_builder_free(sb) free((sb).data);

#define string_builder_capture(sb) (sb).last = (sb).len; (sb).len
#define string_builder_rewind(sb, l) (sb).last = (l); (sb).len = (l)

STRING_DEF bool string_builder_reserve(string_builder *sb, u64 abs_cap);
STRING_DEF bool string_builder_append(string_builder *sb, const char *data, u64 data_len);
STRING_DEF bool string_builder_appendc(string_builder *sb, const char *cstr);
STRING_DEF bool string_builder_appends(string_builder *sb, string s);
STRING_DEF bool string_builder_appendf(string_builder *sb, const char *fmt, ...);
STRING_DEF bool string_builder_appendm(string_builder *sb, const char *data, u64 data_len, string_builder_map map);

STRING_DEF bool string_builder_to_cstr(string_builder *sb, char **cstr);
STRING_DEF bool string_builder_to_cstr_view(string_builder *sb, cstr_view *v);
STRING_DEF bool string_builder_to_string(string_builder *sb, string *s);
STRING_DEF void string_builder_to_string_view(string_builder *sb, string_view *v);

typedef u32 Rune;

STRING_DEF void rune_encode(Rune rune, char buf[4], u64 *buf_len);
STRING_DEF void rune_escape(Rune rune, char buf[6], u64 *buf_len);

STRING_DEF Rune rune_decode(const char **data, u64 *data_len);
STRING_DEF Rune rune_unescape(const char **data, u64 *data_len);

#ifdef STRING_IMPLEMENTATION

string stringf_impl(char *space, int n, ...) {

  va_list list;
  va_start(list, n);

  const char *fmt = va_arg(list, const char *);

  if(!fmt) {
    return (string) {NULL, 0};
  }

  vsnprintf(space, n, fmt, list);

  va_end(list);
  
  return (string) { space, n };
}

#define stringf_concat_impl(a, b) a ##b
#define stringf_concat(a, b) stringf_concat_impl(a, b)
#define stringf(...) stringf_impl(_alloca(snprintf(NULL, 0, __VA_ARGS__) + 1), snprintf(NULL, 0, __VA_ARGS__) + 1, __VA_ARGS__)
 
#define STRING_FROM_U_CAP 32

string string_from_u64_impl(char *space, u64 n) {
  u64 m = snprintf(space, STRING_FROM_U_CAP, "%llu", n);
  return (string) { space, m };
}

#define string_from_u64(n) string_from_u64_impl(_alloca(STRING_FROM_U_CAP), n)

STRING_DEF bool string_copy(string s, string *d) {
  return string_copy_cstr2(s.data, s.len, d);
}

STRING_DEF bool string_copy_cstr(const char *cstr, string *d) {
  return string_copy_cstr2(cstr, strlen(cstr), d);
}

STRING_DEF bool string_copy_cstr2(const char *cstr, u64 cstr_len, string *d) {
  char *data = malloc(cstr_len);
  if(!data) return false;
  memcpy(data, cstr, cstr_len);
  d->data = (const char *) data;
  d->len  = cstr_len;
  return true;
}

STRING_DEF int string_compare(const void *_a, const void *_b) {
  const string *a = _a;
  const string *b = _b;

  u64 i =0 ;

  while(true) {
    
    if(i >= a->len) {
      if(i >= b->len) {
	return 0;
      } else {
	return -1;
      }
    }

    if(i >= b->len) {
      if(i >= a->len) {
	return 0;
      } else {
	return 1;
      }
    }

    if(a->data[i] < b->data[i] ) {
      return -1;
    } else if(a->data[i] > b->data[i] )  {
      return 1;
    }

    i++;
  }

  return 0;
}

STRING_DEF bool string_eq(string s, string d) {
  return string_eq_cstr2(s, d.data, d.len);
}

STRING_DEF bool string_eq_cstr(string s, const char *cstr) {
  return string_eq_cstr2(s, cstr, strlen(cstr));
}

STRING_DEF bool string_eq_cstr2(string s, const char *cstr, u64 cstr_len) {
  if(s.len != cstr_len) {
    return false;
  }

  return memcmp(s.data, cstr, cstr_len) == 0;
}

#define string_substring_impl(s, start, len) (string) { (s).data + start, len }

static int string_index_of_impl(const char *haystack, u64 haystack_size, const char* needle, u64 needle_size) {
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
      return (int) i;
    }
  }
  return -1;

}

STRING_DEF s32 string_index_of_off(string s, u64 off, string needle) {
  if(off > s.len) {
    return - 1;
  }

  s32 pos = string_index_of_impl(s.data + off, s.len - off, needle.data, needle.len);
  if(pos < 0) {
    return -1;
  }

  return pos + (s32) off;
}

STRING_DEF s32 string_index_of_offc(string s, u64 off, const char *needle) {

  if(off > s.len) {
    return -1;
  }
  
  s32 pos = string_index_of_impl(s.data + off, s.len - off, needle, strlen(needle));
  if(pos < 0) {
    return -1;
  }

  return pos + (s32) off;
}

static s32 string_last_index_of_impl(const char *haystack, u64 haystack_size, const char* needle, u64 needle_size) {

  if(needle_size > haystack_size) {
    return -1;
  }
  
  s64 i;

  for(i=haystack_size - needle_size - 1;i>=0;i--) {
    u64 j;
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

STRING_DEF s32 string_last_index_of(string s, string needle) {
  return string_last_index_of_impl(s.data, s.len, needle.data, needle.len);
}

STRING_DEF s32 string_last_index_ofc(string s, const char *needle) {
  return string_last_index_of_impl(s.data, s.len, needle, strlen(needle));  
}

STRING_DEF bool string_contains(string s, const char *needle) {
  return string_index_ofc(s, needle) >= 0;
}

STRING_DEF s32 string_index_of(string s, string needle) {
  return string_index_of_impl(s.data, s.len, needle.data, needle.len);
}

STRING_DEF s32 string_index_ofc(string s, const char *needle) {  
  return string_index_of_impl(s.data, s.len, needle, strlen(needle));
}

STRING_DEF bool string_chop_by(string *s, const char *delim, string *d) {
  if(!s->len) return false;
  
  s32 pos = string_index_ofc(*s, delim);
  if(pos < 0) pos = (int) s->len;
    
  if(d && !string_substring(*s, 0, pos, d))
    return false;

  if(pos == (int) s->len) {
    *d = *s;
    s->len = 0;
    return true;
  } else {
    return string_substring(*s, pos + 1, s->len - pos - 1, s);
  }

}

STRING_DEF bool string_chop_by_pred(string *s, bool (*predicate)(char x), string *d) {
  if(!s->len) return false;

  u64 i=0;
  while(i < s->len && !predicate(s->data[i])) {
    i++;
  }
  u64 j=i;
  while(j < s->len && predicate(s->data[j])) {
    j++;
  }

  if(d && !string_substring(*s, 0, i, d)) {
    return false;
  }
  
  if(j == s->len) {
    *d = *s;
    s->len = 0;
    return true;
  } else {
    return string_substring(*s, j, s->len - j, s);
  }
}

STRING_DEF bool string_chop_left(string *s, u64 n) {
  if(s->len < n) {
    return false;
  }

  s->data += n;
  s->len  -= n;
  
  return true;
}

STRING_DEF bool string_chop_right(string *s, u64 n) {
  if(s->len < n) {
    return false;
  }

  s->data += n;
  s->len  -= n;

  return true;
}

STRING_DEF bool string_parse_s64(string s, s64 *n) {
  u64 i=0;
  s64 sum = 0;
  int negative = 0;

  const char *data = s.data;
  
  if(s.len && data[0]=='-') {
    negative = 1;
    i++;
  }  
  while(i<s.len &&
	'0' <= data[i] && data[i] <= '9') {
    sum *= 10;
    s32 digit = data[i] - '0';
    sum += digit;
    i++;
  }

  if(negative) sum*=-1;
  *n = sum;
  
  return i==s.len;
}

STRING_DEF bool string_parse_f64(string s, f64 *n) {
  if (s.len == 0) {
    return false;
  }

  f64 parsedResult = 0.0;
  int sign = 1;
  int decimalFound = 0;
  int decimalPlaces = 0;
  f64 exponentFactor = 1.0;

  u8 *data = (u8 *)s.data;

  u64 i = 0;

  if (i < s.len && (data[i] == '+' || data[i] == '-')) {
    if (data[i] == '-') {
      sign = -1;
    }
    i++;
  }

  while (i < s.len && isdigit(data[i])) {
    parsedResult = parsedResult * 10.0 + (data[i] - '0');
    i++;
  }

  if (i < s.len && data[i] == '.') {
    i++;
    while (i < s.len && isdigit(data[i])) {
      parsedResult = parsedResult * 10.0 + (data[i] - '0');
      decimalPlaces++;
      i++;
    }
    decimalFound = 1;
  }

  exponentFactor = 1.0;
  for (int j = 0; j < decimalPlaces; j++) {
    exponentFactor *= 10.0;
  }
  
  parsedResult *= sign;
  if (decimalFound) {
    parsedResult /= exponentFactor;
  }

  *n = parsedResult;

  return true; // Parsing was successful
}

STRING_DEF bool string_substring(string s, u64 start, u64 len, string *d) {

  if(start > s.len) {
    return false;
  }

  if(start + len > s.len) {
    return false;
  }

  *d = string_substring_impl(s, start, len);
  
  return true;
}

STRING_DEF bool string_unescape(const char *input, u64 input_size,
				char *buffer, u64 buffer_size,
				u64 *out_output_size) {

  char buf[4];
  u64 buf_len;

  u64 output_size = 0;
  
  while(input_size) {
    
    Rune rune = rune_unescape(&input, &input_size);
    rune_encode(rune, buf, &buf_len);
    
    if(output_size + buf_len >= buffer_size) return false;
    memcpy(buffer + output_size, buf, buf_len);
    output_size += buf_len;
  }

  *out_output_size = output_size;
  return true;
}

STRING_DEF bool string_escape(const char *input, u64 input_size,
				char *buffer, u64 buffer_size,
				u64 *out_output_size) {
  char buf[6];
  u64 buf_len;

  u64 output_size = 0;
  
  while(input_size) {
    
    Rune rune = rune_decode(&input, &input_size);
    rune_escape(rune, buf, &buf_len);
    
    if(output_size + buf_len >= buffer_size) return false;
    memcpy(buffer + output_size, buf, buf_len);
    output_size += buf_len;
  }

  *out_output_size = output_size;
  return true;
}

STRING_DEF bool string_builder_reserve(string_builder *sb, u64 abs_cap) {
  u64 cap = sb->cap;
  if(cap == 0) cap = STRING_BUILDER_DEFAULT_CAP;
  while(abs_cap > cap) {
    cap *= 2;
  }
  if(cap != sb->cap) {
    sb->cap = cap;
    sb->data = realloc(sb->data, sb->cap);
    if(!sb->data) return false;
  }
  return true;
}

STRING_DEF bool string_builder_append(string_builder *sb, const char *data, u64 data_len) {
  if(!string_builder_reserve(sb, sb->len + data_len)) {
    return false;
  }
  memcpy(sb->data + sb->len, data, data_len);
  sb->len += data_len;
  return true;
}

STRING_DEF bool string_builder_appendc(string_builder *sb, const char *cstr) {
  u64 cstr_len = strlen(cstr);
  if(!string_builder_reserve(sb, sb->len + cstr_len)) {
    return false;
  }
  memcpy(sb->data + sb->len, cstr, cstr_len);
  sb->len += cstr_len;
  return true;
}

STRING_DEF bool string_builder_appends(string_builder *sb, string s) {
  return string_builder_append(sb, s.data, s.len);
}

STRING_DEF bool string_builder_appendf(string_builder *sb, const char *fmt, ...) {
  va_list list;  
  va_start(list, fmt);
  va_list list_copy = list;
  
  s32 _n = vsnprintf(NULL, 0, fmt, list) + 1;
  va_end(list);
  u64 n = (u64) _n; 

  if(!string_builder_reserve(sb, sb->len + n)) {
    return false;
  }

  vsnprintf(sb->data + sb->len, n, fmt, list_copy);
  
  sb->len += n - 1;
  
  return true;

}

STRING_DEF bool string_builder_appendm(string_builder *sb, const char *data, u64 data_len, string_builder_map map) {
  u64 estimated_len = data_len * 2;
  if(!string_builder_reserve(sb, sb->len + estimated_len)) {
    return false;
  }

  u64 actual_len;
  bool result = map(data, data_len, sb->data + sb->len, sb->cap - sb->len, &actual_len);
  while(!result) {
    estimated_len *= 2;
    if(!string_builder_reserve(sb, sb->len + estimated_len)) {
      return false;
    }
    result = map(data, data_len, sb->data + sb->len, sb->cap - sb->len, &actual_len);
  }
  sb->len += actual_len;

  return true;
}

STRING_DEF bool string_builder_to_cstr(string_builder *sb, char **cstr) {
  char *data = malloc(sb->len + 1);
  if(!data) return false;
  memcpy(data, sb->data, sb->len);
  data[sb->len] = 0;
  *cstr = data;
  return true; 
}

STRING_DEF bool string_builder_to_cstr_view(string_builder *sb, cstr_view *v) {
  if(!string_builder_append(sb, "\0", 1)) {
    return false;
  }

  v->off = sb->last;
  sb->last = sb->len;

  return true;
}

STRING_DEF bool string_builder_to_string(string_builder *sb, string *d) {
  return string_copy_cstr2(sb->data, sb->len, d);
}

STRING_DEF void string_builder_to_string_view(string_builder *sb, string_view *v) {
  v->off = sb->last;
  v->len = sb->len - sb->last;
  sb->last = sb->len;
}

//https://en.wikipedia.org/wiki/UTF-8

// <= 128     ( 7)  0*** ****
// <= 2048    (11)  110* ****  10** ****
// <= 262144  (18)  1110 ****  10** ****  10** ****
// <= 2097152 (21)  1111 0***  10** ****  10** ****  10** ****

STRING_DEF void rune_encode(Rune rune, char buf[4], u64 *buf_len) {

  *buf_len = 0;
  
  if(rune <= 128) {
    buf[(*buf_len)++] = (char) rune;
  } else if(rune <= 2048) {
    // **** ****  **** ****  **** *123  4567 89AB
    //                     |
    //                     v
    // ***1 2345 **67 89AB
    buf[(*buf_len)++] = 0xc0 | ((rune >> 6) & 0x1f);
    buf[(*buf_len)++] = 0x80 | (rune & 0x3f);
  } else if(rune <= 65536) {
    // **** ****  **** ****  1234 5678  9ABC DEFG
    //                     |
    //                     v
    // **** 1234  **56 789A  **BC DEFG
    buf[(*buf_len)++] = 0xe0 | ((rune >> 12) & 0x0f);
    buf[(*buf_len)++] = 0x80 | ((rune >> 6) & 0x3f);
    buf[(*buf_len)++] = 0x80 | (rune & 0x3f);
  } else {
    // **** ****  ***1 2345  6789 ABCD  EFGH IJKL
    //                     |
    //                     v
    // **** *123  **45 6789  **AB CDEF  **GH IJKL
    buf[(*buf_len)++] = 0xf0 | ((rune >> 18) & 0x07);
    buf[(*buf_len)++] = 0x80 | ((rune >> 12) & 0x3f);
    buf[(*buf_len)++] = 0x80 | ((rune >> 6) & 0x3f);
    buf[(*buf_len)++] = 0x80 | (rune & 0x3f);
  }
}

STRING_DEF void rune_escape(Rune rune, char buf[6], u64 *buf_len) {
  *buf_len = 0;

  if(rune <= 128) {
    char c = (char) rune;
    
    switch(c) {
    case '\"': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = '\"';
    } break;
    case '\\': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = '\\';	
    } break;
    case '/': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = '/';      
    } break;
    case '\b': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = 'f';
    } break;
    case '\f': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = 'f';      
    } break;
    case '\n': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = 'n';      
    } break;
    case '\r': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = 'r';
    } break;
    case '\t': {
      buf[(*buf_len)++] = '\\';
      buf[(*buf_len)++] = 't';
    } break;
    default: {
      buf[(*buf_len)++] = (char) rune;
    } break;
    } 
        
  } else {
    buf[0] = '\\';
    buf[1] = 'u';

    u64 pos = 5;

    while(rune > 0) {
      char c = (char) (rune % 16);
      if(c < 10) {
	c += '0';
      } else {
	c += 'W';
      }
      buf[pos--] = c;
      rune /= 16;
    }

    while(pos >= 2) buf[pos--] = '0';
    *buf_len = 6;
  }
  
}

STRING_DEF Rune rune_decode(const char **data, u64 *data_len) {
  char c = (*data)[0];

  Rune rune;
  if((0xf0 & c) == 0xf0) {    
    rune = ((Rune) (c & 0x07)) << 18;
    rune |= ((Rune) ( (*data)[1] & 0x3f )) << 12;
    rune |= ((Rune) ( (*data)[2] & 0x3f )) <<  6;
    rune |= (Rune) ( (*data)[3] & 0x3f );
      
    *data = *data + 4;
    *data_len = *data_len - 4;
  } else if((0xe0 & c) == 0xe0) {

    rune = ((Rune) (c & 0x0f)) << 12;
    rune |= ((Rune) ( (*data)[1] & 0x3f )) << 6;
    rune |= (Rune) ( (*data)[2] & 0x3f );

    *data = *data + 3;
    *data_len = *data_len - 3;
  } else if((0xc0 & c) == 0xc0) {
    
    rune = ((Rune) (c & 0x1f)) << 6;
    rune |= (Rune) ( (*data)[1] & 0x3f );

    *data = *data + 2;
    *data_len = *data_len - 2;
  } else {
    STRING_ASSERT((0x80 & c) != 0x80);
    rune = (Rune) c;

    *data = *data + 1;
    *data_len = *data_len - 1;
  }

  return rune;
}

STRING_DEF Rune rune_unescape(const char **data, u64 *data_len) {
  char c = (*data)[0];

  Rune rune = 0;
  if(c != '\\') {  // unescaped char 'a'
    rune = (Rune) c;
    *data = *data + 1;
    *data_len = *data_len - 1;
  } else {
    c = (*data)[1];

    if(c != 'u') { // escaped char '\n'

      switch(c) {
      case '\"': rune = (Rune) '\"'; break;
      case '\\': rune = (Rune) '\\'; break;
      case '/': rune = (Rune) '/'; break;
      case 'b': rune = (Rune) '\b'; break;
      case 'f': rune = (Rune) '\f'; break;
      case 'n': rune = (Rune) '\n'; break;
      case 'r': rune = (Rune) '\r'; break;
      case 't': rune = (Rune) '\t'; break;
      }

      *data = *data + 2;
      *data_len = *data_len - 2;
    } else { // unicode character '\u00d6'
      
      Rune n;
      for(u8 i=0;i<4;i++) {
    
	c = (*data)[2 + i];
	if('0' <= c && c <= '9') {
	  n = c - '0';
	} else if('a' <= c && c <= 'f') {
	  n = c - 'W';
	} else if('A' <= c && c <= 'F') {
	  n = c - '7';
	} else {
	  break;
	}
	rune *= 16;
	rune += n;
      }

      *data = *data + 6;
      *data_len = *data_len - 6;
    }
     
  }

  return rune;
}


#endif // STRING_IMPLEMENTATION

#undef  u8
#undef s32
#undef u32
#undef s64
#undef u64
#undef f64

#endif // STRING_H
