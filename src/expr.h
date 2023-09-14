#ifndef EXPR_H
#define EXPR_H

#include "token.h"
#include "string.h"

typedef enum{
  EXPR_TYPE_NONE = 0,
  EXPR_TYPE_NUMBER,
  EXPR_TYPE_STRING,
  EXPR_TYPE_VARIABLE,

  EXPR_TYPE_ADD,
  EXPR_TYPE_SUB,
  EXPR_TYPE_MUL,
  EXPR_TYPE_DIV
}Expr_Type;
 
const char *expr_type_name(Expr_Type type);
Expr_Type expr_type_from(Token_Type type);

typedef struct Expr Expr;

typedef struct{
  Expr *lhs;
  Expr *rhs;
}Expr_Binary;

struct Expr{
  Expr_Type type;
  union{
    string content;
    Expr_Binary binary;
  }as;
};

#endif // EXPR_H
