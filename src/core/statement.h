#ifndef STATEMENT_H
#define STATEMENT_H

#include "expr.h"

typedef enum{
  STATEMENT_TYPE_NONE = 0,
  STATEMENT_TYPE_EXIT,
  STATEMENT_TYPE_PRINT,
  STATEMENT_TYPE_DECL,
}Statement_Type;

const char *statement_type_name(Statement_Type type);

typedef struct{
  Statement_Type type;
  string identifier;
  union{
    Expr *expr;    
  }as;
}Statement;

typedef struct{
  Statement **items;
  u64 len;
  u64 cap;
}Statements;

#endif // STATEMENT_H
