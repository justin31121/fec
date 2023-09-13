#ifndef EXPR_H
#define EXPR_H

#include "string.h"

typedef enum{
  EXPR_TYPE_NONE = 0,
  EXPR_TYPE_NUMBER,
  EXPR_TYPE_STRING,
  EXPR_TYPE_VARIABLE,
}Expr_Type;
 
const char *expr_type_name(Expr_Type type);

typedef struct Expr Expr;

struct Expr{
  Expr_Type type;
  union{
    string content;
  }as;
};

#endif // EXPR_H
