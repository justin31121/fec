#include "parser.h"

#define IO_IMPLEMENTATION
#include "io.h"

bool parser_from(const char *filepath, Parser_Alloc parser_alloc, void *userdata, Parser *p) {

  u8 *data;
  u64 len;
  if(!io_slurp_file(filepath, &data, &len)) {
    return false;
  }
  
  *p = (Parser) { filepath, tokenizer_from(string_from(data, len)), parser_alloc, userdata };

  return true;
}

#define __parser_error(p, ...) do{					\
    fprintf(stderr, "%s:%llu:%llu: ERROR: ", p->filepath,		\
	    tokenizer_row(&p->tokenizer), tokenizer_column(&p->tokenizer)); \
    fprintf(stderr, __VA_ARGS__);					\
    fprintf(stderr, "\n");						\
  }while(0)

#define __parser_expect_token(ttype)					\
  if(!tokenizer_next(&p->tokenizer, &token)) {				\
    __parser_error(p, "Expected %s but found EOF", token_type_name((ttype)) ); \
    p->tokenizer.i = i;							\
    return false;							\
  }									\
  if(token.type != (ttype)) {						\
    p->tokenizer.i = p->tokenizer.last;				\
    __parser_error(p, "Expected %s but found %s", token_type_name((ttype)), token_type_name(token.type)); \
    p->tokenizer.i = i;							\
    return false;							\
  }									\
  
bool parser_parse_statement_exit(Parser *p, Statement **statement) {

  Token token;
  u64 i = p->tokenizer.i;
  
  if(!tokenizer_next(&p->tokenizer, &token) ||
     token.type != TOKEN_TYPE_EXIT) {
    p->tokenizer.i = i;
    return false;
  }

  __parser_expect_token(TOKEN_TYPE_OPEN_PARENT);

  Expr *expr;
  if(!parser_parse_expr(p, &expr)) {
    __parser_error(p, "Expected EXPR but found %s", token_type_name(token.type));
    p->tokenizer.i = i;
    return false;
  }
  
  __parser_expect_token(TOKEN_TYPE_CLOSE_PARENT);
  __parser_expect_token(TOKEN_TYPE_SEMI);

  *statement = p->alloc(p->userdata, sizeof(Statement));
  (*statement)->type = STATEMENT_TYPE_EXIT;
  (*statement)->as.expr = expr;
  
  return true;
}

bool parser_parse_statement_print(Parser *p, Statement **statement)  {
  Token token;
  u64 i = p->tokenizer.i;
  
  if(!tokenizer_next(&p->tokenizer, &token) ||
     token.type != TOKEN_TYPE_PRINT) {
    p->tokenizer.i = i;
    return false;
  }

  __parser_expect_token(TOKEN_TYPE_OPEN_PARENT);

  Expr *expr;
  if(!parser_parse_expr(p, &expr)) {
    __parser_error(p, "Expected EXPR but found %s", token_type_name(token.type));
    p->tokenizer.i = i;
    return false;
  }
  
  __parser_expect_token(TOKEN_TYPE_CLOSE_PARENT);
  __parser_expect_token(TOKEN_TYPE_SEMI);

  *statement = p->alloc(p->userdata, sizeof(Statement));
  (*statement)->type = STATEMENT_TYPE_PRINT;
  (*statement)->as.expr = expr;
  
  return true;
  
}

bool parser_parse_statement_decl(Parser *p, Statement **statement)  {
  Token token;
  u64 i = p->tokenizer.i;
  
  if(!tokenizer_next(&p->tokenizer, &token) ||
     token.type != TOKEN_TYPE_NAME) {
    p->tokenizer.i = i;
    return false;
  }

  string identifier = token.content;
  
  __parser_expect_token(TOKEN_TYPE_ASSIGN);

  Expr *expr;
  if(!parser_parse_expr(p, &expr)) {
    __parser_error(p, "Expected EXPR but found %s", token_type_name(token.type));
    p->tokenizer.i = i;
    return false;
  }

  __parser_expect_token(TOKEN_TYPE_SEMI);

  *statement = p->alloc(p->userdata, sizeof(Statement));
  (*statement)->type = STATEMENT_TYPE_DECL;
  (*statement)->identifier = identifier;
  (*statement)->as.expr = expr;

  return true;
}

bool parser_parse_expr_variable(Parser *p, Expr **expr) {
  Token token;
  u64 i = p->tokenizer.i;
  
  if(!tokenizer_next(&p->tokenizer, &token) ||
     token.type != TOKEN_TYPE_NAME) {
    p->tokenizer.i = i;
    return false;
  }

  *expr = p->alloc(p->userdata, sizeof(Expr));
  (*expr)->type = EXPR_TYPE_VARIABLE;
  (*expr)->as.content = token.content;

  return true;  
}

bool parser_parse_expr_string(Parser *p, Expr **expr) {
  Token token;
  u64 i = p->tokenizer.i;
  
  if(!tokenizer_next(&p->tokenizer, &token) ||
     token.type != TOKEN_TYPE_STRING) {
    p->tokenizer.i = i;
    return false;
  }

  *expr = p->alloc(p->userdata, sizeof(Expr));
  (*expr)->type = EXPR_TYPE_STRING;
  (*expr)->as.content = token.content;

  return true;  
}

bool parser_parse_expr_number(Parser *p, Expr **expr) {
  Token token;
  u64 i = p->tokenizer.i;
  
  if(!tokenizer_next(&p->tokenizer, &token) ||
     token.type != TOKEN_TYPE_NUMBER) {
    p->tokenizer.i = i;
    return false;
  }

  *expr = p->alloc(p->userdata, sizeof(Expr));
  (*expr)->type = EXPR_TYPE_NUMBER;
  (*expr)->as.content = token.content;

  return true;
}

bool parser_parse_expr(Parser *p, Expr **expr) {
  if(parser_parse_expr_number(p, expr)) {
    return true;
  } else if(parser_parse_expr_variable(p, expr)) {
    return true;
  } else if(parser_parse_expr_string(p, expr)) {
    return true;
  } else {
    return false;
  }
}

bool parser_parse_statement(Parser *p, Statement **statement) {

  if(parser_parse_statement_exit(p, statement)) {
    return true;
  } else if(parser_parse_statement_decl(p, statement)) {
    return true;
  } else if(parser_parse_statement_print(p, statement)) {
    return true;
  } else {
    return false;
  }  

}

bool parser_parse_block(Parser *p, Statements *statements) {
  while(p->tokenizer.i != p->tokenizer.len) {
    Statement *s = NULL;
    da_append(statements, s);
    
    if(!parser_parse_statement(p, &statements->items[statements->len - 1])) {
      /* Token token; */
      /* assert(tokenizer_next(&p->tokenizer, &token)); */
      /* __parser_error(p, "Expected statement but got: "token_fmt, token_arg(token)); */
      return false;
    }
  }

  return true;
}
