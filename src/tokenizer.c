#include "tokenizer.h"

#include <string.h>
#include <ctype.h>

Tokenizer tokenizer_from(string src) {
  return (Tokenizer) {src.data, src.len, 0, 0};
}

#define __tokenizer_tokenize_singleton(ttype)\
  token->type = (ttype);\
  token->content = string_from(&c, 1);\
  t->i++;\
  return true

bool tokenizer_peek(Tokenizer *t, u64 n, Token *token) {
  u64 i = t->i;
  
  for(u64 i=0;i<n;i++) {
    if(!tokenizer_next(t, token)) {
      return false;
    }
  }

  t->i = i;
  return true;
}

bool tokenizer_next(Tokenizer *t, Token *token) {
  while(t->i < t->len && isspace(t->data[t->i]) ) {
    t->i++;
  }
  
  if(t->i >= t->len) {
    return false;
  }

  t->last = t->i;

  if(tokenizer_tokenize_keyword(t, TOKEN_TYPE_EXIT, "exit", token)) {
    return true;
  } else if(tokenizer_tokenize_keyword(t, TOKEN_TYPE_ASSIGN, ":=", token)) {
    return true;
  } else if(tokenizer_tokenize_keyword(t, TOKEN_TYPE_PRINT, "print", token)) {
    return true;
  }

  u8 c = t->data[t->i];
  switch(c) {
    
  case ';': __tokenizer_tokenize_singleton(TOKEN_TYPE_SEMI);
  case '(': __tokenizer_tokenize_singleton(TOKEN_TYPE_OPEN_PARENT);
  case ')': __tokenizer_tokenize_singleton(TOKEN_TYPE_CLOSE_PARENT);
    
  }

  if(tokenizer_tokenize_string(t, token)) {
    return true;
  }

  if(tokenizer_tokenize_name(t, token)) {
    return true;
  }

  if(tokenizer_tokenize_number(t, token)) {
    return true;
  }
  
  return false;
}

bool tokenizer_tokenize_keyword(Tokenizer *t, Token_Type type, const char *cstr, Token *token) {
  
  u64 cstr_len = strlen(cstr);
  if(t->i + cstr_len >= t->len) {
    return false;
  }

  if(memcmp(t->data + t->i, cstr, cstr_len) != 0) {
    return false;
  }

  token->type = type;
  token->content = string_from(t->data + t->i, cstr_len);

  t->i += cstr_len;
    
  return true;
}

bool tokenizer_tokenize_string(Tokenizer *t, Token *token) {
  u64 i = t->i;

  if(t->i >= t->len || t->data[t->i] != '\"') {
    return false;
  }
  t->i++;
  
  while(t->i < t->len && t->data[t->i] != '\"') {
    t->i++;    
  }
  
  if(t->i == t->len) {
    t->i = i;
    return false;
  }
  t->i++;

  token->type = TOKEN_TYPE_STRING;
  token->content = string_from(t->data + i + 1, t->i - i - 2);

  return true;
}

bool tokenizer_tokenize_name(Tokenizer *t, Token *token) {
  u64 i = t->i;

  if(!isalpha(t->data[t->i])) {
    return false;
  }
  
  while(t->i < t->len &&
	( isalpha(t->data[t->i]) || isdigit(t->data[t->i]) ) ) {
    t->i++;
  }

  if(i == t->i) {
    return false;
  }

  token->type = TOKEN_TYPE_NAME;
  token->content = string_from(t->data + i, t->i - i);

  return true;
  
}

bool tokenizer_tokenize_number(Tokenizer *t, Token *token) {
  u64 i = t->i;
  
  while(t->i < t->len && isdigit(t->data[t->i])) {
    t->i++;
  }

  if(i == t->i) {
    return false;
  }

  token->type = TOKEN_TYPE_NUMBER;
  token->content = string_from(t->data + i, t->i - i);

  return true;
}

u64 tokenizer_row(Tokenizer *t) {
  u64 row = 1;
  
  for(u64 i=0;i<=t->i;i++) {
    u8 c = t->data[i];
    if(c == '\n') {
      row++;
    }
  }

  return row;
}

u64 tokenizer_column(Tokenizer *t) {
  u64 column = 0;
  
  for(u64 i=0;i<=t->i;i++) {
    u8 c = t->data[i];
    if(c == '\n') {
      column = 0;
    } else {
      column++;
    }
  }

  return column;
}
