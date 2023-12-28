#include <stdio.h>

// https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture

#define STRING_IMPLEMENTATION
#include "_string.h"

#define IO_IMPLEMENTATION
#include "io.h"

#define FILE_PATH "main.asm"

#define panic(fmt, ...) do{ fprintf(stderr, "%s:%d:%s:ERROR: " fmt "\n", __FILE__, __LINE__, __func__, #__VA_ARGS__); exit(1); }while(0)

typedef long long int s64;
typedef unsigned long long int u64;
typedef unsigned char u8;

char *REGISTER_NAMES[] = {
  "rcx", "rdx", "r8", "r9", "rax", "rbx", "rsi", "rdi",
};
u64 REGISTER_COUNT =
  sizeof(REGISTER_NAMES)/sizeof(REGISTER_NAMES[0]);

typedef enum{
  LOCATION_TYPE_NONE,
  LOCATION_TYPE_REGISTER,
  LOCATION_TYPE_STACK,
  LOCATION_TYPE_PUSH,
}Location_Type;

typedef struct{
  Location_Type type;
  u64 value;
}Location;

#define LOCATION_RAX (Location) { .type = LOCATION_TYPE_REGISTER, .value = 4 }
#define LOCATION_RCX (Location) { .type = LOCATION_TYPE_REGISTER, .value = 0 }
#define LOCATION_PUSH (Location) { .type = LOCATION_TYPE_PUSH }

char *FASTCALL_REGISTER_NAMES[] = {
  "rcx", "rdx", "r8", "r9"
};
u64 FASTCALL_REGISTER_COUNT =
  sizeof(FASTCALL_REGISTER_NAMES)/sizeof(FASTCALL_REGISTER_NAMES[0]);

typedef enum{
  TYPE_NONE,
  TYPE_U64,
  TYPE_PTR,
}Type;

u64 type_size(Type type) {
  switch(type) {

  case TYPE_U64:
    return 8;

  case TYPE_PTR:
    return 8;
    
  default: {
    panic("unimplemented type");
  } break;
  }
}

const char *type_cstr(Type type) {
  switch(type) {

  case TYPE_U64:
    return "u64";

  case TYPE_PTR:
    return "void*";
    
  default: {
    panic("unimplemented data_type");
  } break;
  }
}

typedef enum{
  EXPR_TYPE_NONE,
  EXPR_TYPE_POP,
  EXPR_TYPE_VALUE,
  EXPR_TYPE_VARIABLE,
  EXPR_TYPE_FUNCCALL,
  EXPR_TYPE_CONSTANT,
  EXPR_TYPE_VARIABLE_PTR,
}Expr_Type;

#define FUNCCALL_ARGS_CAP 8

typedef struct Expr Expr;

typedef struct{
  Expr *args[FUNCCALL_ARGS_CAP];
  u64 args_len;
  string name;
}Expr_Funccall;

typedef struct Constant Constant;

struct Expr{
  Expr_Type type;
  union{
    s64 sval;
    string stringval;
    Expr_Funccall funccall;
    Constant *constant;
  }as;
};

typedef enum{
  STMT_TYPE_NONE,
  STMT_TYPE_DECLARATION,
  STMT_TYPE_ASSIGNMENT,
  STMT_TYPE_FUNCCALL,
  STMT_TYPE_IF,
}Stmt_Type;

typedef struct{
  string name;
  Type type;
}Stmt_Declaration;

typedef struct{
  Expr *lhs;
  Expr *rhs;
}Stmt_Assignment;

typedef enum{
  STMT_IF_TYPE_NONE,
  STMT_IF_TYPE_EQUALS,
  STMT_IF_TYPE_NOT_EQUALS,
}Stmt_If_Type;

typedef struct Stmts Stmts;

typedef struct{
  Stmt_If_Type type;
  Expr *lhs;
  Expr *rhs;
  Stmts *body;
}Stmt_If;

typedef struct{
  Stmt_Type type;
  union {
    Stmt_Declaration declaration;
    Stmt_Assignment assignment;
    Expr_Funccall funccall;
    Stmt_If iff;
  }as;
}Stmt;

struct Stmts{
  Stmt *data;
  u64 len;
  u64 cap;
};

#define INITIAL_CAP 64

Stmt *stmts_append(Stmts *s) {
  if(s->len < s->cap) {
    return &s->data[s->len++];
  }

  if(s->cap == 0) {
    s->cap = INITIAL_CAP;
  } else {
    s->cap = s->cap * 2;
  }

  s->data = realloc(s->data, s->cap * sizeof(Stmt));
  if(!s->data) {
    fprintf(stderr, "ERROR: Failed to realloc\n");
    exit(1);
  }

  return &s->data[s->len++];
}

typedef struct{
  string name;
  Type type;
  u64 off;
}Var;

typedef struct{
  Var *data;
  u64 len;
  u64 cap;
}Vars;

Var *vars_append(Vars *s) {
  if(s->len < s->cap) {
    return &s->data[s->len++];
  }

  if(s->cap == 0) {
    s->cap = INITIAL_CAP;
  } else {
    s->cap = s->cap * 2;
  }

  s->data = realloc(s->data, s->cap * sizeof(Var));
  if(!s->data) {
    fprintf(stderr, "ERROR: Failed to realloc\n");
    exit(1);
  }

  return &s->data[s->len++];
}

Var *vars_find(Vars *vars, string name, Type type) {

  for(u64 i=0;i<vars->len;i++) {
    Var *var = &vars->data[i];
    if(!string_eq(name, var->name)) {
      continue;
    }

    if(type != TYPE_NONE &&
       type != var->type) {
      continue;
    }

    return var;
  }

  return NULL;
  
}

typedef enum{
  CONSTANT_TYPE_NONE,
  CONSTANT_TYPE_STRING,
  CONSTANT_TYPE_U64,
}Constant_Type;

struct Constant{

  Constant_Type type;
  union{
    u8 *cstrval;
    u64 uval;
  }as;
  
};

typedef struct{
  Constant *data;
  u64 len;
  u64 cap;
}Constants;

Constant *constants_append(Constants *s) {
  if(s->len < s->cap) {
    return &s->data[s->len++];
  }

  if(s->cap == 0) {
    s->cap = INITIAL_CAP;
  } else {
    s->cap = s->cap * 2;
  }

  s->data = realloc(s->data, s->cap * sizeof(Constant));
  if(!s->data) {
    fprintf(stderr, "ERROR: Failed to realloc\n");
    exit(1);
  }

  return &s->data[s->len++];
}

typedef struct{
  Constants constants;
  Stmts stmts;
  Vars vars;  
  
  u64 stack_ptr;
}Program;


void program_funccall_compile(Program *p, Expr_Funccall *funccall, string_builder *sb);

void program_expr_compile(Program *p, Expr *e, Location loc, string_builder *sb) {

  switch(e->type) {

  case EXPR_TYPE_VALUE: {

    string_builder_appendc(sb, "        ;;     EXPR_VALUE\n");
    string_builder_appendf(sb, "        ;;         value=%lld\n", e->as.sval);

    switch(loc.type) {

    case LOCATION_TYPE_STACK: {
      string_builder_appendf(sb, "        mov qword [rsp + %llu], %lld\n",
			     p->stack_ptr - loc.value, e->as.sval);
    } break;

    case LOCATION_TYPE_REGISTER: {
      string_builder_appendf(sb, "        mov qword %s, %lld\n",
			     REGISTER_NAMES[loc.value], e->as.sval);
    } break;

    case LOCATION_TYPE_PUSH: {
      string_builder_appendf(sb, "        push qword %lld\n", e->as.sval);
      p->stack_ptr += 8;
    } break;
      
    default: {
      panic("unimplemented location_type");
    } break;
    }
    
  } break;

  case EXPR_TYPE_VARIABLE: {

    Var *var = vars_find(&p->vars, e->as.stringval, TYPE_NONE);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(e->as.stringval));
    }
    
    string_builder_appendc(sb, "        ;;     EXPR_VARIABLE\n");
    string_builder_appendf(sb, "        ;;         name=\""str_fmt"\"\n", str_arg(e->as.stringval));

    switch(loc.type) {

    case LOCATION_TYPE_REGISTER: {
      string_builder_appendf(sb, "        mov %s, [rsp + %llu]\n",
			     REGISTER_NAMES[loc.value], p->stack_ptr - var->off);
    } break;

    case LOCATION_TYPE_PUSH: {
      string_builder_appendf(sb, "        push qword [rsp + %llu]\n",
			     p->stack_ptr - var->off);
      p->stack_ptr += 8;
    } break;
      
    default: {
      panic("unimplemented location_type");
    } break;
    }
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    string_builder_appendc(sb, "        ;;     EXPR_FUNCCALL\n");

    program_funccall_compile(p, &e->as.funccall, sb);

    switch(loc.type) {

    case LOCATION_TYPE_REGISTER: {

      // Result of funccall already is in RAX,
      // no need for any instructions
      if(loc.value == LOCATION_RAX.value) {
	return;
      }
      
      string_builder_appendf(sb, "        mov %s, rax\n",
			     REGISTER_NAMES[loc.value]);
    } break;

    case LOCATION_TYPE_STACK: {
      string_builder_appendf(sb, "        mov [rsp + %llu], rax\n",
			     p->stack_ptr - loc.value);
    } break;

    case LOCATION_TYPE_PUSH: {
      string_builder_appendf(sb, "        push rax\n");
      p->stack_ptr += 8;
    } break;
      
    default: {
      panic("unimplemented location_type");
    } break;
    }
    
  } break;

  case EXPR_TYPE_CONSTANT: {

    string_builder_appendc(sb, "        ;;     EXPR_CONSTANT\n");

    switch(loc.type) {

    case LOCATION_TYPE_REGISTER: {

      Constant *c = e->as.constant;
      string_builder_appendf(sb, "        mov %s, constant_%p\n",
			     REGISTER_NAMES[loc.value], (void *) c);
      
    } break;
      
    default: {
      panic("unimplemented location_type");
    } break;
    }
    
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {

    string_builder_appendc(sb, "        ;;     EXPR_VARIABLE_PTR\n");

    Var *var = vars_find(&p->vars, e->as.stringval, TYPE_NONE);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(e->as.stringval));
    }

    switch(loc.type) {

    case LOCATION_TYPE_REGISTER: {
      string_builder_appendf(sb, "        lea %s, [rsp + %llu]\n",
			     REGISTER_NAMES[loc.value], p->stack_ptr - var->off);
    } break;

    case LOCATION_TYPE_STACK: {
      string_builder_appendf(sb, "        lea rax, [rsp + %llu]\n",
			     p->stack_ptr - var->off);
      string_builder_appendf(sb, "        mov [rsp + %llu], rax\n",
			     p->stack_ptr - loc.value);
    } break;
      
    default: {
      panic("unimplemented location_type");
    } break;
    }
    
  } break;
    
  default: {
    panic("unimplemented expr_type");
  } break;
  }
  
}

void program_funccall_compile(Program *p, Expr_Funccall *funccall, string_builder *sb) {
  u64 stack_ptr_before = p->stack_ptr;
  
  for(u64 i=0;i<funccall->args_len;i++) {
    Location loc;
    if(i < FASTCALL_REGISTER_COUNT) {
      // "value = i" is only valid, since REGISTER_NAMES 
      // starts with the FASTCALL_REGISTER_NAMES
      loc = (Location) { .type = LOCATION_TYPE_REGISTER, .value = i };
    } else {
      loc = (Location) { .type = LOCATION_TYPE_PUSH };
    }
      
    program_expr_compile(p, funccall->args[i], loc, sb);
  }

  u64 shadow_space = 32;
  u64 alignment = (p->stack_ptr + shadow_space) % 16;
  if(alignment != 0) {
    shadow_space += 16 - alignment;
  }
  string_builder_appendf(sb, "        sub rsp, %llu\n", shadow_space);
  p->stack_ptr += shadow_space;
  string_builder_appendf(sb, "        call "str_fmt"\n", str_arg(funccall->name));  

  string_builder_appendf(sb, "        add rsp, %llu\n", p->stack_ptr - stack_ptr_before);
  p->stack_ptr = stack_ptr_before;
  
}

Location program_expr_location(Program *p, Expr *e) {
  switch(e->type) {

  case EXPR_TYPE_VARIABLE: {
    
    Var *var = vars_find(&p->vars, e->as.stringval, TYPE_NONE);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(e->as.stringval));
    }
      
    return (Location) { .type = LOCATION_TYPE_STACK, .value = var->off };
    
  } break;
    
  default: {
    panic("unimplemented expr_type");
  } break;
  }
}

void program_stmt_compile(Program *p, Stmt *s, string_builder *sb) {

  switch(s->type) {

  case STMT_TYPE_DECLARATION: {

    Stmt_Declaration *declaration = &s->as.declaration;
    if(vars_find(&p->vars, declaration->name, TYPE_NONE)) {
      panic("variable already declared");
    }

    u64 size = type_size(declaration->type);

    Var *var = vars_append(&p->vars);
    var->name = declaration->name;
    var->type = declaration->type;
    var->off  = p->stack_ptr + size;

    string_builder_appendc(sb, "        ;; STMT_DECLARATION\n");
    string_builder_appendf(sb, "        ;;     name=\""str_fmt"\"\n", str_arg(declaration->name));
    string_builder_appendf(sb, "        ;;     type=%s\n", type_cstr(declaration->type));
        
    string_builder_appendf(sb, "        sub rsp, %llu\n", size);
    p->stack_ptr += size;
    
  } break;

  case STMT_TYPE_ASSIGNMENT: {

    string_builder_appendc(sb, "        ;; STMT_ASSIGN\n");

    Stmt_Assignment *assignment = &s->as.assignment;
    Expr *lhs = assignment->lhs;
    Expr *rhs = assignment->rhs;
      
    Location loc = program_expr_location(p, lhs);
    program_expr_compile(p, rhs, loc, sb);
    
  } break;

  case STMT_TYPE_FUNCCALL: {

    string_builder_appendc(sb, "        ;; STMT_FUNCCALL\n");

    Expr_Funccall *funccall = &s->as.funccall;
    program_funccall_compile(p, funccall, sb);    
    
  } break;

  case STMT_TYPE_IF: {

    string_builder_appendc(sb, "        ;; STMT_IF\n");

    Stmt_If *iff = &s->as.iff;
    Expr *lhs = iff->lhs;
    Expr *rhs = iff->rhs;
    
    // Do not push, if you dont need to
    switch(rhs->type) {

    case EXPR_TYPE_VALUE: 
    case EXPR_TYPE_VARIABLE: {
      program_expr_compile(p, lhs, LOCATION_RAX, sb);
      program_expr_compile(p, rhs, LOCATION_RCX, sb);
    } break;

    case EXPR_TYPE_FUNCCALL: {
      program_expr_compile(p, lhs, LOCATION_PUSH, sb);
      program_expr_compile(p, rhs, LOCATION_RCX, sb);
      string_builder_appendc(sb, "        pop rax\n");
      p->stack_ptr -= 8;
    } break;
      
    default: {
      panic("unimplemented expr_type");
    } break;
    }

    string_builder_appendc(sb, "        cmp rax, rcx\n");
    
    switch(iff->type) {
    case STMT_IF_TYPE_EQUALS: {
      string_builder_appendf(sb, "        jne .label_%p\n", (void *) s);
    } break;

    case STMT_IF_TYPE_NOT_EQUALS: {
      string_builder_appendf(sb, "        je .label_%p\n", (void *) s);
    } break;
      
    default: {
      panic("unimplemented if_type");
    } break;
    }

    for(u64 i=0;i<iff->body->len;i++) {
      program_stmt_compile(p, &iff->body->data[i], sb);
    }

    string_builder_appendf(sb, ".label_%p:\n", (void *) s);
    
    
  } break;
    
  default: {
    panic("unimplemented stmt_type");
  } break;
  }

}

void program_constant_compile(Program *p, Constant *c, string_builder *sb) {
  switch(c->type) {

  case CONSTANT_TYPE_STRING: {

    string_builder_appendf(sb, "        ;; CONSTANT_STRING\n");
    string s = string_from_cstr(c->as.cstrval);
    string copy = s;
    string line;
    string_builder_appendf(sb, "        ;;     value=\"");
    int i = 0;
    while(string_chop_by(&s, "\n", &line)) {
      if(i++ == 0) {
	string_builder_appendf(sb, str_fmt"\n", str_arg(line));
      } else {
	string_builder_appendf(sb, "        ;;            "str_fmt"\n", str_arg(line));
      }
    }
    s = copy;
    sb->len = sb->len - 1;
    string_builder_appendf(sb, "\"\n");
    
    
    string_builder_appendf(sb, "constant_%p: db ", (void *) c);
    for(u64 i=0;i<s.len;i++) {
      string_builder_appendf(sb, "%u", s.data[i]);
      if(i != s.len - 1) {
        string_builder_appendc(sb, ",");
      }
    }

    string_builder_appendf(sb, "\n");
    
  } break;

  case CONSTANT_TYPE_U64: {

    string_builder_appendf(sb, "        ;; CONSTANT_U64\n");
    string_builder_appendf(sb, "        ;;     value=%llu\n", c->as.uval);
    
    
    string_builder_appendf(sb, "%%define constant_%p %llu\n",
			   (void *) c, c->as.uval);
    
  } break;
    
  default: {
    panic("unimplemented constant_type");
  } break;
  }
}

void program_compile(Program *p, string_builder *sb) {

  // PREFIX
  string_builder_appendc(sb,
			 "        global main\n"
			 "        extern ExitProcess\n"
			 "        extern GetStdHandle\n"
			 "        extern WriteFile\n");

  // Constants
  if(p->constants.len > 0) {

    string_builder_appendc(sb, "\n");
    string_builder_appendc(sb, "        section .data\n");    

    for(u64 i=0;i<p->constants.len;i++) {
      program_constant_compile(p, &p->constants.data[i], sb);
    }

    string_builder_appendc(sb, "\n");
  }

  // Stmts
  string_builder_appendc(sb,
			 "        section .text:\n"
			 "main:\n");
  
  for(u64 i=0;i<p->stmts.len;i++) {
    program_stmt_compile(p, &p->stmts.data[i], sb);
  }

  string_builder_appendc(sb, "\n");

  // SUFFIX
  if(p->stack_ptr != 0) {
    string_builder_appendf(sb,
			   "        add rsp, %llu\n", p->stack_ptr);
  }
  
  string_builder_appendc(sb,
			 "        mov rcx, 0\n"
			 "        sub rsp, 40\n"
			 "        call ExitProcess\n");
}


void stmts_append_declaration(Stmts *s, string name, Type type) {

  Stmt *stmt = stmts_append(s);
  stmt->type = STMT_TYPE_DECLARATION;

  Stmt_Declaration *declaration = &stmt->as.declaration;
  declaration->name = name;
  declaration->type = type; 
}

void stmts_append_assignment(Stmts *s, Expr *lhs, Expr *rhs) {
  
  Stmt *stmt = stmts_append(s);
  stmt->type = STMT_TYPE_ASSIGNMENT;

  Stmt_Assignment *assignment = &stmt->as.assignment;
  assignment->lhs = lhs;
  assignment->rhs = rhs;  
}

void stmts_append_funccall(Stmts *s, string name, Expr **args, u64 args_len) {

  Stmt *stmt = stmts_append(s);
  stmt->type = STMT_TYPE_FUNCCALL;
  
  Expr_Funccall *funccall = &stmt->as.funccall;
  memcpy(funccall->args, args, args_len * sizeof(Expr*));
  funccall->args_len = args_len;
  funccall->name = name;
}

void stmts_append_if(Stmts *s,
		     Expr *lhs,
		     Stmt_If_Type operand,
		     Expr *rhs,
		     Stmts *body) {
  
  Stmt *stmt = stmts_append(s);
  stmt->type = STMT_TYPE_IF;

  Stmt_If *stmt_if = &stmt->as.iff;
  stmt_if->rhs = rhs;
  stmt_if->type = operand;
  stmt_if->lhs = lhs;
  stmt_if->body = body;
}

// TODO:
//     derefence ptr
//     structures
//     allocate arrays on stack

int main() {

  string_builder sb = {0};

  u64 args_len;
  Expr *args[FUNCCALL_ARGS_CAP];

  Program program = {0};

  Constant *hello_world_data = constants_append(&program.constants);
  hello_world_data->type = CONSTANT_TYPE_STRING;
  hello_world_data->as.cstrval = "Hello World!\n";

  Constant *hello_world_len = constants_append(&program.constants);
  hello_world_len->type = CONSTANT_TYPE_U64;
  hello_world_len->as.uval = 13;

  ///////////////////////////////////////////////////////////
  
  // handle : u64*
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("handle"),
			   TYPE_PTR);

  // handle = GetStdHandle(-11);
  Expr expr_handle = {
    .type = EXPR_TYPE_VARIABLE,
    .as.stringval = string_from_cstr("handle")
  };
  Expr expr_neg_11 = {
    .type = EXPR_TYPE_VALUE,
    .as.sval = -11
  };
  Expr expr_GetStdHandle = {
    .type = EXPR_TYPE_FUNCCALL,
  };
  expr_GetStdHandle.as.funccall.name = string_from_cstr("GetStdHandle");
  expr_GetStdHandle.as.funccall.args_len = 1;
  expr_GetStdHandle.as.funccall.args[0] = &expr_neg_11;
  stmts_append_assignment(&program.stmts,
			  &expr_handle,
			  &expr_GetStdHandle);

  // if(handle == -1) {
  //   ExitProcess(1);
  // }
  Stmts if_body = {0};
  Expr expr_1 = {
    .type = EXPR_TYPE_VALUE,
    .as.sval = 1
  };
  Expr *expr_1_ptr = &expr_1;
  stmts_append_funccall(&if_body,
			string_from_cstr("ExitProcess"),
			&expr_1_ptr,
			1);
  Expr expr_neg_1 = {
    .type = EXPR_TYPE_VALUE,
    .as.sval = -1
  };
  stmts_append_if(&program.stmts,
		  &expr_handle,
		  STMT_IF_TYPE_EQUALS,
		  &expr_neg_1,
		  &if_body);

  // written : u64
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("written"),
			   TYPE_U64);

  // if(WriteFile(handle, hello_world_data, hello_world_len, &written, NULL) == 0) {
  //   ExitProcess(1);
  // }
  Expr *exprs[8];
  u64 exprs_len = 0;
  Expr expr_hello_world_data = {
    .type = EXPR_TYPE_CONSTANT,
    .as.constant = hello_world_data,
  };
  Expr expr_hello_world_len = {
    .type = EXPR_TYPE_CONSTANT,
    .as.constant = hello_world_len,
  };
  Expr expr_ptr_to_written = {
    .type = EXPR_TYPE_VARIABLE_PTR,
    .as.stringval = string_from_cstr("written")
  };
  Expr expr_0 = {
    .type = EXPR_TYPE_VALUE,
    .as.sval = 0,
  };
  exprs[exprs_len++] = &expr_handle;
  exprs[exprs_len++] = &expr_hello_world_data;
  exprs[exprs_len++] = &expr_hello_world_len;
  exprs[exprs_len++] = &expr_ptr_to_written;
  exprs[exprs_len++] = &expr_0;
  Expr expr_WriteFile = {
    .type = EXPR_TYPE_FUNCCALL,
  };
  expr_WriteFile.as.funccall.name = string_from_cstr("WriteFile");
  memcpy(&expr_WriteFile.as.funccall.args, exprs, exprs_len * sizeof(Expr *));
  expr_WriteFile.as.funccall.args_len = exprs_len;

  stmts_append_if(&program.stmts,
		  &expr_WriteFile,
		  STMT_IF_TYPE_EQUALS,
		  &expr_0,
		  &if_body);
  
  program_compile(&program, &sb);

  printf("\n===================================================\n");
  printf("\n%.*s\n", (int) sb.len, sb.data);
  printf("\n===================================================\n");
  
  if(!io_write_file(FILE_PATH, (u8 *) sb.data, sb.len)) {
    return 1;
  }
  
  return 0;
}
