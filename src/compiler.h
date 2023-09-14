#ifndef COMPILER_H
#define COMPILER_H

#include "parser.h"
#include "string.h"

typedef struct{
  string identifier;
  u64 stack_pos;
  Expr_Type type;
}Compiler_Variable;

typedef struct{
  Compiler_Variable *items;
  u64 len;
  u64 cap;
}Compiler_Variables;

typedef struct{
  Parser parser;

  string_builder *sb;
  u8 buffer[8192];
  u64 buffer_size;
  
  bool has_exit;

  u64 stack_pos;
  Compiler_Variables vars;
  
  string_builder temp;
  u64 temp_strings;
}Compiler;

bool compiler_from(const char *filepath, Parser_Alloc parser_alloc, void *userdata, Compiler *c);
bool compiler_compile(Compiler *c, string_builder *sb);

bool compiler_compile_expr_add(Compiler *c, Expr *expr);
bool compiler_compile_expr_variable(Compiler *c, Expr *expr);
void compiler_compile_expr_number(Compiler *c, Expr *expr);
bool compiler_compile_expr(Compiler *c, Expr *expr);

bool compiler_compile_statement_print(Compiler *c, Statement *statement);
bool compiler_compile_statement_decl(Compiler *c, Statement *statement);
bool compiler_compile_statement_exit(Compiler *c, Statement *statement);
bool compiler_compile_statement(Compiler *c, Statement *statement);

bool compiler_get_variable(Compiler *c, string identifier, Compiler_Variable **var);

#endif // COMPILER_H
