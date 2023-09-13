#include "statement.h"

#include <stdlib.h>
#include <assert.h>

const char *statement_type_name(Statement_Type type) {

  switch(type) {
  case STATEMENT_TYPE_NONE: return "STATEMENT_NONE";
  case STATEMENT_TYPE_EXIT: return "STATEMENT_EXIT";
  case STATEMENT_TYPE_DECL: return "STATEMENT_DECL";
  case STATEMENT_TYPE_PRINT: return "STATEMENT_PRINT";
  }

  assert(!"unreachable");
  return NULL;
}
