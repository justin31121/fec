#ifndef TOKEN_H
#define TOKEN_H

#include "string.h"

typedef enum{
  TOKEN_TYPE_NONE = 0,
  
  TOKEN_TYPE_SEMI,
  TOKEN_TYPE_OPEN_PARENT,
  TOKEN_TYPE_CLOSE_PARENT,

  TOKEN_TYPE_NUMBER,
  TOKEN_TYPE_NAME,
  TOKEN_TYPE_STRING,
  
  TOKEN_TYPE_EXIT,
  TOKEN_TYPE_PRINT,
  TOKEN_TYPE_ASSIGN,
}Token_Type;

const char *token_type_name(Token_Type type);

typedef struct{
  Token_Type type;
  string content;
}Token;

#define token_fmt "%s \""str_fmt"\""
#define token_arg(t) token_type_name((t).type), str_arg((t).content)

#endif // TOKEN_H
