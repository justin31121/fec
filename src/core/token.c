#include "token.h"

const char *token_type_name(Token_Type type) {

  switch(type) {
  case TOKEN_TYPE_NONE: return "TOKEN_NONE";
    
  case TOKEN_TYPE_SEMI: return "TOKEN_SEMI";
  case TOKEN_TYPE_OPEN_PARENT: return "TOKEN_OPEN_PARENT";
  case TOKEN_TYPE_CLOSE_PARENT: return "TOKEN_CLOSE_PARENT";
  case TOKEN_TYPE_PLUS: return "TOKEN_PLUS";
  case TOKEN_TYPE_MINUS: return "TOKEN_MINUS";
  case TOKEN_TYPE_STAR: return "TOKEN_STAR";
  case TOKEN_TYPE_DASH: return "TOKEN_DASH";
    
  case TOKEN_TYPE_NUMBER: return "TOKEN_NUMBER";
  case TOKEN_TYPE_NAME: return "TOKEN_NAME";
  case TOKEN_TYPE_ASSIGN: return "TOKEN_ASSIGN";
    
  case TOKEN_TYPE_STRING: return "TOKEN_STRING";
  case TOKEN_TYPE_PRINT: return "TOKEN_PRINT";
  case TOKEN_TYPE_EXIT: return "TOKEN_EXIT";
  }

  assert(!"unreachable");
  return NULL;
}

bool token_type_is_operator(Token_Type type) {
  switch(type) {
  case TOKEN_TYPE_PLUS:
  case TOKEN_TYPE_MINUS:
  case TOKEN_TYPE_STAR:
  case TOKEN_TYPE_DASH:
    return true;
  default:
    return false;
  }

}
