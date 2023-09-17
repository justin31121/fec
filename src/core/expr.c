#include "expr.h"

#include <stdlib.h>
#include <assert.h>

const char *expr_type_name(Expr_Type type) {
  
  switch(type) {
  case EXPR_TYPE_NONE: return "EXPR_NONE";
  case EXPR_TYPE_NUMBER: return "EXPR_NUMBER";
  case EXPR_TYPE_STRING: return "EXPR_STRING";
  case EXPR_TYPE_VARIABLE: return "EXPR_TYPE_VARIABLE";
  case EXPR_TYPE_ADD: return "EXPR_ADD";
  case EXPR_TYPE_SUB: return "EXPR_SUB";
  case EXPR_TYPE_MUL: return "EXPR_MUL";
  case EXPR_TYPE_DIV: return "EXPR_DIV";
  }

  assert(!"unreachable");
  return NULL;  
}

Expr_Type expr_type_from(Token_Type type) {
  switch(type) {
  case TOKEN_TYPE_PLUS: return EXPR_TYPE_ADD;
  case TOKEN_TYPE_MINUS: return EXPR_TYPE_SUB;
  case TOKEN_TYPE_DASH: return EXPR_TYPE_MUL;
  case TOKEN_TYPE_STAR: return EXPR_TYPE_DIV;
  default: {
    assert(!"unreachable");    
  }
  }

  return EXPR_TYPE_NONE;
}
