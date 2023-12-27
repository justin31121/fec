#include <stdio.h>

#define STRING_IMPLEMENTATION
#include "_string.h"

#define IO_IMPLEMENTATION
#include "io.h"

#define INDENT "    "

typedef unsigned long long int u64;
typedef long long int s64;
typedef unsigned char u8;

typedef enum {
  DATA_TYPE_NONE,
  DATA_TYPE_U64,
  DATA_TYPE_U64_PTR,
}Data_Type;

u64 data_type_size(Data_Type type) {
  switch(type) {

    //TODO: check if it needs to be aligned to 16
  case DATA_TYPE_U64:
  case DATA_TYPE_U64_PTR:
    return 16;
    
  default:
    fprintf(stderr, "ERROR: unimplemented data_type: data_type_size\n");
    exit(1);
  }
}

typedef enum {
  EXPR_TYPE_NONE,
  EXPR_TYPE_CONSTANT,
  EXPR_TYPE_VARIABLE,
}Expr_Type;

typedef struct {
  Expr_Type type;
  union{
    s64 sval;
    string name;
  }as;
}Expr;

void expr_dump(Expr *e, u64 indent) {
  switch(e->type) {

  case EXPR_TYPE_CONSTANT: {

    for(u64 i=0;i<indent;i++) printf(INDENT);
    printf("EXPR_CONSTANT %lld\n", e->as.sval);
    
  } break;

  case EXPR_TYPE_VARIABLE: {

    for(u64 i=0;i<indent;i++) printf(INDENT);
    printf("EXPR_VARIABLE \""str_fmt"\"\n", str_arg(e->as.name));
    
  } break;
    
  default:
    fprintf(stderr, "ERROR: unimplemented expr_type: expr_dump\n");
    exit(1);

  }
}

char *REGISTER_ORDER[] = {
  "rcx", "rdx", "r8", "r9"
};
u64 REGISTER_COUNT = sizeof(REGISTER_ORDER) / sizeof(REGISTER_ORDER[0]);

void expr_compile(Expr *e, s64 register_index, string_builder *sb) {
  
  switch(e->type) {

  case EXPR_TYPE_CONSTANT: {

    if(register_index < 0) { // relative to rsp
      register_index *= -1;
      
      string_builder_appendf(sb, "        mov [rsp + %lld], %lld\n", e->as.sval);
    } else { // call-register or above
      
      if((u64) register_index < REGISTER_COUNT) {
	string_builder_appendf(sb, "        mov %s, %lld\n", REGISTER_ORDER[register_index], e->as.sval);
      } else {
	string_builder_appendf(sb, "        push qword %lld\n", e->as.sval);
      }
    }

    
  } break;

  default:
    fprintf(stderr, "ERROR: unimplemented expr_type: expr_compile\n");
    exit(1);
  }
}

typedef enum {
  STMT_TYPE_NONE,
  STMT_TYPE_FUNCCALL,
  STMT_TYPE_DECLARATION,
  STMT_TYPE_ASSIGNMENT,
}Stmt_Type;

typedef struct{
  Expr args[8];
  u64 args_count;
  string name;
}Stmt_Funccall;

typedef struct{
  Data_Type type;
  string name;
}Stmt_Declaration;

// You can only assign variables, so lhs is a string?
typedef struct{
  string lhs;
  Expr rhs;
}Stmt_Assignment;

typedef struct {
  Stmt_Type type;
  union{    
    Stmt_Funccall funccall;
    Stmt_Declaration declaration;
    Stmt_Assignment assignment;
  }as;
}Stmt;

void stmt_dump(Stmt *s, u64 indent) {

  switch(s->type) {
  case STMT_TYPE_FUNCCALL: {
    for(u64 i=0;i<indent;i++) printf(INDENT);
    printf("STMT_FUNCCALL\n");

    for(u64 i=0;i<indent+1;i++) printf(INDENT);
    printf("name = \""str_fmt"\"\n", str_arg(s->as.funccall.name));

    for(u64 i=0;i<indent+1;i++) printf(INDENT);
    printf("args = \n");
    for(u64 i=0;i<s->as.funccall.args_count;i++) {
      expr_dump(&s->as.funccall.args[i], indent + 2); 
    }
    
  } break;

  case STMT_TYPE_DECLARATION: {
    for(u64 i=0;i<indent;i++) printf(INDENT);
    printf("STMT_DECLARATION\n");

    for(u64 i=0;i<indent+1;i++) printf(INDENT);
    printf("name = \""str_fmt"\"\n", str_arg(s->as.declaration.name));

    for(u64 i=0;i<indent+1;i++) printf(INDENT);
    printf("size = %llu\n", data_type_size(s->as.declaration.type));
    
  } break;

  case STMT_TYPE_ASSIGNMENT: {

    for(u64 i=0;i<indent;i++) printf(INDENT);
    printf("STMT_ASSIGNMENT\n");

    for(u64 i=0;i<indent+1;i++) printf(INDENT);
    printf("lhs = \""str_fmt"\"\n", str_arg(s->as.assignment.lhs));

    for(u64 i=0;i<indent+1;i++) printf(INDENT);
    printf("rhs = \n");
    expr_dump(&s->as.assignment.rhs, indent + 2); 
    
  } break;
    
  default:
    fprintf(stderr, "ERROR: unimplemented stmt_type: stmt_dump\n");
    exit(1);
  }

}

typedef struct Program Program;

void program_variable_append(Program *p, string name, u64 size);
u64 program_variable_off(Program *p, string name);

void stmt_compile(Program *p, Stmt *s, string_builder *sb) {
  switch(s->type) {

  case STMT_TYPE_FUNCCALL: {

    for(u64 i=0;i<s->as.funccall.args_count;i++) {
      expr_compile(&s->as.funccall.args[i], (s64) i, sb);
    }

    // TODO: check shadow_space
    string_builder_appendf(sb, "        sub rsp, %llu\n", s->as.funccall.args_count * 8);
    string_builder_appendf(sb, "        call "str_fmt"\n", str_arg(s->as.funccall.name));
    string_builder_appendf(sb, "        add rsp, %llu\n\n", s->as.funccall.args_count * 8);
    
  } break;

  case STMT_TYPE_DECLARATION: {
        
    u64 variable_size = data_type_size(s->as.declaration.type);
    program_variable_append(p, s->as.declaration.name, variable_size);
    string_builder_appendf(sb, "        sub rsp, %llu\n\n", variable_size);
        
  } break;

  case STMT_TYPE_ASSIGNMENT: {

    u64 variable_off = program_variable_off(p, s->as.assignment.lhs);
    
  } break;

  default:
    fprintf(stderr, "ERROR: unimplemented stmt_type: stmt_compile\n");
    exit(1);
  }

}

typedef struct {
  string name;
  u64 off;
}Variable;

struct Program {
  Stmt stmts[64];
  u64 stmt_count;

  Variable variables[64];
  u64 variables_count;

  u64 stack_ptr;
};

void program_dump(Program *p, u64 indent) {
  for(u64 i=0;i<p->stmt_count;i++) {
    Stmt *stmt = &p->stmts[i];
    stmt_dump(stmt, indent);
  }  
}

void program_variable_append(Program *p, string name, u64 size) {
  if(p->variables_count >= sizeof(p->variables)) {
    fprintf(stderr, "ERROR: program.variables overflow\n");
    exit(1);
  }

  Variable *v = &p->variables[p->variables_count++];
  v->name = name;
  v->off  = p->stack_ptr;
  p->stack_ptr += size;
}

u64 program_variable_off(Program *p, string name) {
  for(u64 i=0;i<p->variables_count;i++) {
    Variable *v = &p->variables[i];

    if(string_eq(v->name, name)) {
      return v->off;
    }
  }

  fprintf(stderr, "ERROR: can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
  exit(1);
}

void program_compile(Program *p, string_builder *sb) {
  string_builder_appendc(sb, "        global main\n");

  // TODO: keep track of foreign functions
  string_builder_appendc(sb, "        extern GetStdHandle\n");
  string_builder_appendc(sb, "        extern ExitProcess\n");
  
  string_builder_appendc(sb, "        section .text\n");
  string_builder_appendc(sb, "main:\n");

  for(u64 i=0;i<p->stmt_count;i++) {
    Stmt *stmt = &p->stmts[i];
    stmt_compile(p, stmt, sb);
  }

  string_builder_appendf(sb, "        add rsp, %llu\n\n", p->stack_ptr);
  
  string_builder_appendc(sb, "        mov rcx, 0\n");
  string_builder_appendc(sb, "        sub rsp, 8\n");
  string_builder_appendc(sb, "        call ExitProcess\n");
}

typedef void* (*alloc_func)(u64 bytes, void *userdata);

void *malloc_wrapper(u64 bytes, void *userdata) {
  (void) userdata;
  return malloc(bytes);
}

int main() {

  Program program = {0};

  program.stmts[program.stmt_count++] = (Stmt) {
    .type = STMT_TYPE_DECLARATION,
    .as.declaration = (Stmt_Declaration) {
      .type = DATA_TYPE_U64,
      .name = string_from_cstr("handle"),
    }
  };

  program.stmts[program.stmt_count++] = (Stmt) {
    .type = STMT_TYPE_ASSIGNMENT,
    .as.assignment = (Stmt_Assignment) {
      .lhs = string_from_cstr("handle"),
      .rhs = (Expr) {
	.type = EXPR_TYPE_CONSTANT,
	.as.sval = 69
      }
    }
  };

  program.stmts[program.stmt_count++] = (Stmt) {
    .type = STMT_TYPE_FUNCCALL,
    .as.funccall = (Stmt_Funccall) {
      .name = string_from_cstr("GetStdHandle"),
      .args_count = 1,
      .args[0] = (Expr) {
	.type = EXPR_TYPE_CONSTANT,
	.as.sval = -11,
      }
    }
  };
  
  /* program.stmts[program.stmt_count++] = (Stmt) { */
  /*   .type = STMT_TYPE_FUNCCALL, */
  /*   .as.funccall = (Stmt_Funccall) { */
  /*     .name = string_from_cstr("ExitProcess"), */
  /*     .args_count = 1, */
  /*     .args[0] = (Expr) { */
  /* 	.type = EXPR_TYPE_CONSTANT, */
  /* 	.as.sval = 3, */
  /*     } */
  /*   } */
  /* }; */

  program_dump(&program, 0);
  
  printf("\n\n\n==============================================\n\n\n");

  string_builder sb = {0};
  program_compile(&program, &sb);
  printf("%.*s\n", (int) sb.len, sb.data);
  if(!io_write_file("main.asm", (u8 *) sb.data, sb.len)) {
    return 1;
  }

  return 0;
}
