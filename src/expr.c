#include "expr.h"

#include <stdlib.h>
#include <assert.h>

const char *expr_type_name(Expr_Type type) {
  
  switch(type) {
  case EXPR_TYPE_NONE: return "EXPR_NONE";
  case EXPR_TYPE_NUMBER: return "EXPR_NUMBER";
  case EXPR_TYPE_STRING: return "EXPR_STRING";
  case EXPR_TYPE_VARIABLE: return "EXPR_TYPE_VARIABLE";    
  }

  assert(!"unreachable");
  return NULL;  
}
