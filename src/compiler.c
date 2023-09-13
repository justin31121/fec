#include "compiler.h"

#include "string.h"
#include <stdio.h>
#include <string.h>

bool compiler_from(const char *filepath, Parser_Alloc parser_alloc, void *userdata, Compiler *c) {
  if(!parser_from(filepath, parser_alloc, userdata, &c->parser)) {
    return false;
  }

  c->sb = NULL;

  return true;
}

#define __compiler_appendf(...) do{					\
    c->buffer_size = snprintf((s8 *) c->buffer, sizeof(c->buffer), __VA_ARGS__); \
    assert(c->buffer_size < sizeof(c->buffer));				\
    da_append_many(c->sb, c->buffer, c->buffer_size);			\
  }while(0)

#define __compiler_temp_appendf(...) do{				\
    c->buffer_size = snprintf((s8 *) c->buffer, sizeof(c->buffer), __VA_ARGS__); \
    assert(c->buffer_size < sizeof(c->buffer));				\
    da_append_many(&c->temp, c->buffer, c->buffer_size);		\
  }while(0)

#define __compiler_error(c, str, ...) do{				\
    u64 __compiler_error_temp_i = (c)->parser.tokenizer.i;					\
    (c)->parser.tokenizer.i = (str).data - (c)->parser.tokenizer.data;	\
    fprintf(stderr, "%s:%llu:%llu: ERROR: ", (c)->parser.filepath,	\
	    tokenizer_row(&(c)->parser.tokenizer), tokenizer_column(&(c)->parser.tokenizer)); \
    fprintf(stderr, __VA_ARGS__);					\
    fprintf(stderr, "\n");						\
    (c)->parser.tokenizer.i = __compiler_error_temp_i;			\
  }while(0)

void compiler_compile_expr_string(Compiler *c, string identifier, Expr *expr) {
  if(!c->temp.len) {
    __compiler_temp_appendf("    section .data\n");
  }

  __compiler_temp_appendf(str_fmt":         db '"str_fmt"'\n", str_arg(identifier), str_arg(expr->as.content));
  __compiler_temp_appendf(str_fmt"_len:     equ $-"str_fmt"\n", str_arg(identifier), str_arg(identifier));

  __compiler_appendf("    mov rdi, "str_fmt"_len\n", str_arg(identifier));
  __compiler_appendf("    push rdi \n");
  __compiler_appendf("    mov rdi, "str_fmt"\n", str_arg(identifier));
  __compiler_appendf("    push rdi \n");
}

bool compiler_compile_expr_variable(Compiler *c, Expr *expr) {

  string identifier = expr->as.content;
  for(u64 i=0;i<c->vars.len;i++) {
    Compiler_Variable *var = &c->vars.items[i];
    if(string_eq(var->identifier, identifier)) {

      if(var->type != EXPR_TYPE_STRING) {
	__compiler_appendf("    mov rdi, qword [rsp + %llu]\n", (c->stack_pos - var->stack_pos - 1) * 8);
	__compiler_appendf("    push rdi\n");
      } else {
	__compiler_appendf("    mov rdi, qword [rsp + %llu]\n", (c->stack_pos - var->stack_pos - 1) * 8);
	__compiler_appendf("    push rdi\n");

	__compiler_appendf("    mov rdi, qword [rsp + %llu]\n", (c->stack_pos - var->stack_pos - 1) * 8);
	__compiler_appendf("    push rdi\n");
     }

      return true;
    }
  }

  __compiler_error(c, identifier, "Variable: '"str_fmt"' is undeclared", str_arg(identifier));
  return false;
}

//m
//len
//m
//len

void compiler_compile_expr_number(Compiler *c, Expr *expr) {
  u64 out;
  assert(string_parse_u64(expr->as.content, &out));
  __compiler_appendf("    push %llu\n", out);
}

bool compiler_compile_expr(Compiler *c, Expr *expr) {
  switch(expr->type) {
  case EXPR_TYPE_NUMBER: {
    compiler_compile_expr_number(c, expr);  
  } break;
  case EXPR_TYPE_VARIABLE: {
    if(!compiler_compile_expr_variable(c, expr)) {
      return false;
    }
  } break;
  case EXPR_TYPE_STRING: {
    u8 buffer[1024];
    u64 buffer_size = snprintf((s8 *) buffer, sizeof(buffer), "__fec_temp_string_%llu", c->temp_strings++);
    assert(buffer_size < sizeof(buffer));
    compiler_compile_expr_string(c, string_from(buffer, buffer_size), expr);
  } break;
  default: {
    printf("Unimplemented expr in compiler_copmile_expr: %s\n", expr_type_name(expr->type));
    assert(!"unimplemented");
  } break;
  }

  return true;
}

bool compiler_compile_statement_decl(Compiler *c, Statement *statement) {

  __compiler_appendf("\n    ;; "str_fmt" := ...\n", str_arg(statement->identifier));
  
  Expr *expr = statement->as.expr;  

  string identifier = statement->identifier;
  for(u64 i=0;i<c->vars.len;i++) {
    Compiler_Variable *var = &c->vars.items[i];
    if(string_eq(var->identifier, identifier)) {

      __compiler_error(c, identifier, "Variable: '"str_fmt"' is already decleared", str_arg(identifier));
      
      return false;
    }
  }
  
  switch(expr->type) {
  case EXPR_TYPE_NUMBER:
  case EXPR_TYPE_VARIABLE: {
    if(!compiler_compile_expr(c, expr)) {
      return false;
    }
    
    Compiler_Variable var = {statement->identifier, c->stack_pos, expr->type};
    c->stack_pos++;
    da_append(&c->vars, var);
  } break;
  case EXPR_TYPE_STRING: {
    compiler_compile_expr_string(c, statement->identifier, expr);
    
    Compiler_Variable var = {statement->identifier, c->stack_pos, expr->type};
    c->stack_pos+=2;
    da_append(&c->vars, var);
  } break;
  default: {
    printf("Unimplemented expr in compiler_copmile_statement_decl: %s\n", expr_type_name(expr->type));
    assert(!"unimplemented");
  } break;
  }
  
  return true;
}

bool compiler_compile_statement_print(Compiler *c, Statement *statement) {

  __compiler_appendf("\n    ;; print\n");

  Expr *expr = statement->as.expr;
  if(expr->type == EXPR_TYPE_VARIABLE) {
    
    string identifier = expr->as.content;
    for(u64 i=0;i<c->vars.len;i++) {
      Compiler_Variable *var = &c->vars.items[i];
      if(string_eq(var->identifier, identifier)) {
        if(var->type != EXPR_TYPE_STRING) {
	  __compiler_error(c, expr->as.content, "Can not print %s", expr_type_name(var->type));
	  return false;
	}
      }
    }    
    
  } else if(expr->type != EXPR_TYPE_STRING) {
    __compiler_error(c, expr->as.content, "Can not print %s", expr_type_name(expr->type));
    return false;
  }

  __compiler_appendf("    mov rcx, -11\n");
  __compiler_appendf("    call GetStdHandle\n");

  if(!compiler_compile_expr(c, expr)) {
    return false;
  }
  __compiler_appendf("    pop rdx\n");
  __compiler_appendf("    pop r8\n");

  __compiler_appendf("    sub rsp, 40\n");
  
  __compiler_appendf("    mov rcx, rax\n");
  __compiler_appendf("    lea r9, [rsp]\n");
  __compiler_appendf("    mov qword [rsp + 32], 0\n");
  __compiler_appendf("    call WriteFile\n");
  __compiler_appendf("    add rsp, 40\n");

    
  return true;
}

bool compiler_compile_statement_exit(Compiler *c, Statement *statement) {

  __compiler_appendf("\n    ;; exit\n");

  Expr *expr = statement->as.expr;
  if(expr->type == EXPR_TYPE_VARIABLE) {
    
    string identifier = expr->as.content;
    for(u64 i=0;i<c->vars.len;i++) {
      Compiler_Variable *var = &c->vars.items[i];
      if(string_eq(var->identifier, identifier)) {
        if(var->type != EXPR_TYPE_NUMBER) {
	  __compiler_error(c, expr->as.content, "exit expects EXPR_NUMBER but got %s", expr_type_name(var->type));
	  return false;
	}
      }
    }    
    
  } else if(expr->type != EXPR_TYPE_NUMBER) {
    __compiler_error(c, expr->as.content, "exit expects EXPR_NUMBER but got %s", expr_type_name(expr->type));
    return false;
  }

  if(!compiler_compile_expr(c, expr)) {
    return false;
  }

  __compiler_appendf("    pop rcx\n");
  __compiler_appendf("    call ExitProcess\n");
  
  c->has_exit = true;
  return true;
}

bool compiler_compile_statement(Compiler *c, Statement *statement) {
  switch(statement->type) {
  case STATEMENT_TYPE_EXIT: {
    if(!compiler_compile_statement_exit(c, statement)) {
      return false;
    }
  } break;
  case STATEMENT_TYPE_DECL: {
    if(!compiler_compile_statement_decl(c, statement)) {
      return false;
    }
  } break;
  case STATEMENT_TYPE_PRINT: {
    if(!compiler_compile_statement_print(c, statement)) {
      return false;
    }
  } break;
  default: {
    printf("Unimplemented statement in compiler_copmile_statement: %s\n", statement_type_name(statement->type));
    assert(!"unimplemented");
  } break;
  }

  return true;
}

bool compiler_compile(Compiler *c, string_builder *sb) {

  c->temp = (string_builder) {0};
  c->sb = sb;
  c->vars = (Compiler_Variables) {0};
  c->has_exit = false;
  c->stack_pos = 0;
  c->temp_strings = 0;

  __compiler_appendf("    global _main\n");
  __compiler_appendf("    extern ExitProcess\n");
  __compiler_appendf("    extern GetStdHandle\n");
  __compiler_appendf("    extern WriteFile\n\n");
  
  __compiler_appendf("    section .text\n");
  __compiler_appendf("_main:\n");
  __compiler_appendf("    sub rsp, 8+8\n");

  Statements statements = {0};
  if(!parser_parse_block(&c->parser, &statements)) {
    return false;
  }
  
  for(u64 i=0;i<statements.len;i++) {
    if(!compiler_compile_statement(c, statements.items[i])) {
      return false;
    }    
  }

  if(!c->has_exit) {
    __compiler_appendf("\n    ;; exit\n");
    __compiler_appendf("    mov rcx, 0\n");
    __compiler_appendf("    call ExitProcess\n");
  }

  if(c->temp.len) {
    __compiler_appendf("\n");
    da_append_many(c->sb, c->temp.items, c->temp.len);
  }

  da_append(sb, '\0');

  return true;
}
