#ifndef PARSER_H
#define PARSER_H

#include "statement.h"
#include "tokenizer.h"

typedef void* (*Parser_Alloc)(void *userdata, u64 size_bytes);

typedef struct{
  const char *filepath;
  Tokenizer tokenizer;
  Parser_Alloc alloc;
  void *userdata;
}Parser;

bool parser_from(const char *filepath, Parser_Alloc parser_alloc, void *userdata, Parser *p);

bool parser_parse_expr_string(Parser *p, Expr **expr);
bool parser_parse_expr_variable(Parser *p, Expr **expr);
bool parser_parse_expr_number(Parser *p, Expr **expr);
bool parser_parse_expr(Parser *p, Expr **expr);

bool parser_parse_statement_print(Parser *p, Statement **statement);
bool parser_parse_statement_decl(Parser *p, Statement **statement);
bool parser_parse_statement_exit(Parser *p, Statement **statement);
bool parser_parse_statement(Parser *p, Statement **statement);

#endif // PARSER_H
