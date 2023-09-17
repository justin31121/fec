#ifndef TOKENIZER_H
#define TOKENIZER_H

#include "string.h"
#include "common.h"

#include "token.h"

typedef struct{
  u8 *data;
  u64 len;
  u64 i;
  u64 last;
}Tokenizer;

Tokenizer tokenizer_from(string src);

bool tokenizer_tokenize_keyword(Tokenizer *t, Token_Type type, const char *cstr, Token *token);
bool tokenizer_tokenize_number(Tokenizer *t, Token *token);
bool tokenizer_tokenize_name(Tokenizer *t, Token *token);
bool tokenizer_tokenize_string(Tokenizer *t, Token *token);

bool tokenizer_peek(Tokenizer *t, u64 n, Token *token);
bool tokenizer_next(Tokenizer *t, Token *token);

u64 tokenizer_row(Tokenizer *t);
u64 tokenizer_column(Tokenizer *t);

#endif // TOKENIZER_H
