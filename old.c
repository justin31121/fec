#include <stdio.h>
#include <assert.h>

// https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture

#define STRING_IMPLEMENTATION
#include "_string.h"

#define IO_IMPLEMENTATION
#include "io.h"

#define FILE_PATH "main.asm"

#define panic(fmt, ...) do{ fprintf(stderr, "%s:%d:%s:ERROR: " fmt "\n", __FILE__, __LINE__, __func__, __VA_ARGS__); exit(1); }while(0)

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

#define da_append(n, x)	do{						\
    u64 new_cap = (n)->cap;						\
    while((n)->len >= new_cap) {					\
      new_cap *= 2;							\
      if(new_cap == 0) new_cap = 64;					\
    }									\
    if(new_cap != (n)->cap) {						\
      (n)->cap = new_cap;						\
      (n)->data = realloc((n)->data, (n)->cap * sizeof(*((n)->data))); \
      assert((n)->data);						\
    }									\
    (n)->data[(n)->len++] = x;						\
  }while(0)

#define da_append_many(n, xs, xs_len) do{				\
    u64 new_cap = (n)->cap;						\
    while((n)->len + xs_len >= new_cap) {				\
      new_cap *= 2;							\
      if(new_cap == 0) new_cap = 64;					\
    }									\
    if(new_cap != (n)->cap) {						\
      (n)->cap = new_cap;						\
      (n)->data = realloc((n)->data, (n)->cap * sizeof(*((n)->data))); \
      assert((n)->data);						\
    }									\
    memcpy((n)->data + (n)->len, xs, xs_len * sizeof(*((n)->data)));	\
    (n)->len += xs_len;							\
  }while(0)

typedef enum{
  TYPE_NONE,
  TYPE_U64,
  TYPE_U64_PTR,
}Type;

u64 type_size(Type type) {
  switch(type) {

  case TYPE_U64:
    return 8;

  case TYPE_U64_PTR:
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

  case TYPE_U64_PTR:
    return "u64*";
    
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

typedef struct{
  Expr *data;
  u64 len;
  u64 cap;
}Exprs;

Expr *exprs_append_variable(Exprs *exprs, string name) {
  da_append(exprs, ((Expr) {
      .type = EXPR_TYPE_VARIABLE,
      .as.stringval = name,
      }));

  return &exprs->data[exprs->len - 1];
}

Expr *exprs_append_pointer(Exprs *exprs, string name) {
  da_append(exprs, ((Expr) {
      .type = EXPR_TYPE_VARIABLE_PTR,
      .as.stringval = name,
      }));

  return &exprs->data[exprs->len - 1];
}

Expr *exprs_append_constant(Exprs *exprs, Constant *c) {
  da_append(exprs, ((Expr) {
      .type = EXPR_TYPE_CONSTANT,
      .as.constant = c,
      }));

  return &exprs->data[exprs->len - 1];
}

Expr *exprs_append_value(Exprs *exprs, s64 value) {
  da_append(exprs, ((Expr) {
      .type = EXPR_TYPE_VALUE,
      .as.sval = value,
      }));

  return &exprs->data[exprs->len - 1];
}

#define exprs_append_funccall(exprs, name, ...) \
  exprs_append_funccall_impl((exprs), (name), __VA_ARGS__, NULL)

Expr *exprs_append_funccall_impl(Exprs *exprs, string name, ...) {

  Expr_Funccall funccall;
  funccall.name = name;
  
  va_list args;
  va_start(args, name);

  funccall.args_len = 0;
  for(;;) {

    Expr *arg = va_arg(args, Expr *);
    if(arg == NULL) {
      break;
    }

    if(funccall.args_len >= sizeof(funccall.args)/sizeof(funccall.args[0])) {
      panic("argument overflow");
    }
    
    funccall.args[funccall.args_len++] = arg;
  }
  
  va_end(args);
  
  da_append(exprs, ((Expr) {
      .type = EXPR_TYPE_FUNCCALL,
      .as.funccall = funccall,
      }));

  return &exprs->data[exprs->len - 1];
}

#define INITIAL_CAP 64

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
  CONSTANT_TYPE_CSTR,
  CONSTANT_TYPE_U64,
}Constant_Type;

struct Constant{

  Constant_Type type;
  union{
    const char *cstrval;
    u64 uval;
  }as;
  
};

typedef struct{
  Constant *data;
  u64 len;
  u64 cap;
}Constants;

Constant *constants_append_cstr(Constants *c, const char *cstr) {
  da_append(c, ((Constant) {
	.type = CONSTANT_TYPE_CSTR,
	.as.cstrval = cstr
      }));
  
  return &c->data[c->len - 1];
}

Constant *constants_append_u64(Constants *c, u64 value) {
  da_append(c, ((Constant) {
	.type = CONSTANT_TYPE_U64,
	.as.uval = value
      }));
  
  return &c->data[c->len - 1];
}

typedef struct{
  Constants constants;
  Stmts stmts;
  Vars vars;
  Exprs exprs;
  
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

    case LOCATION_TYPE_PUSH: {

      Constant *c = e->as.constant;
      string_builder_appendf(sb, "        push qword constant_%p\n",
			     (void *) c);
      
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
    panic("unimplemented expr_type: %d", e->type);
  } break;
  }
  
}

void program_funccall_compile(Program *p, Expr_Funccall *funccall, string_builder *sb) {
  u64 stack_ptr_before = p->stack_ptr;
  
  u64 i=0;
  for(;i<4 && i<funccall->args_len;i++) {
    // "value = i" is only valid, since REGISTER_NAMES
    // starts with the FASTCALL_REGISTER_NAMES
    Location loc = (Location) { .type = LOCATION_TYPE_REGISTER, .value = i };
    program_expr_compile(p, funccall->args[i], loc, sb);
  }

  if(funccall->args_len > 4) {
    for(s64 j=(s64) funccall->args_len - 1;j>=4;j--) {
      Location loc = (Location) { .type = LOCATION_TYPE_PUSH };
      program_expr_compile(p, funccall->args[j], loc, sb);
    }
    
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
    panic("unimplemented expr_type: %d", e->type);
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

    Var var;
    var.name = declaration->name;
    var.type = declaration->type;
    var.off  = p->stack_ptr + size;

    string_builder_appendc(sb, "        ;; STMT_DECLARATION\n");
    string_builder_appendf(sb, "        ;;     name=\""str_fmt"\"\n", str_arg(declaration->name));
    string_builder_appendf(sb, "        ;;     type=%s\n", type_cstr(declaration->type));
        
    string_builder_appendf(sb, "        sub rsp, %llu\n", size);
    p->stack_ptr += size;

    da_append(&p->vars, var);
    
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

    u64 vars_len = p->vars.len;
    u64 stack_ptr_before = p->stack_ptr;

    for(u64 i=0;i<iff->body->len;i++) {
      program_stmt_compile(p, &iff->body->data[i], sb);
    }

    p->vars.len = vars_len;
    if(stack_ptr_before != p->stack_ptr) {
      string_builder_append(sb, "        add rsp, %llu\n", p->stack_ptr - stack_ptr_before);
      p->stack_ptr = stack_ptr_before;
    }

    string_builder_appendf(sb, ".label_%p:\n", (void *) s);
    
    
  } break;
    
  default: {
    panic("unimplemented stmt_type");
  } break;
  }

}

void program_constant_compile(Program *p, Constant *c, string_builder *sb) {

  (void) p;
  
  switch(c->type) {

  case CONSTANT_TYPE_CSTR: {

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
    for(u64 j=0;j<s.len;j++) {
      string_builder_appendf(sb, "%u", s.data[j]);
      if(j != s.len - 1) {
        string_builder_appendc(sb, ",");
      }
    }
    string_builder_appendc(sb, ",0");    

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
			 "        extern WriteFile\n"
			 "        extern GetLastError\n"
			 "        extern CreateFileA\n"
			 "        extern CloseHandle\n"
			 "        extern GetFileSize\n"
			 "        extern GetProcessHeap\n"
			 "        extern HeapAlloc\n"
			 "        extern ReadFile\n"
			 "        extern foo\n");

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

  // Constants
  if(p->constants.len > 0) {

    string_builder_appendc(sb, "\n");
    string_builder_appendc(sb, "        section .data\n");    

    for(u64 i=0;i<p->constants.len;i++) {
      program_constant_compile(p, &p->constants.data[i], sb);
    }

    string_builder_appendc(sb, "\n");
  }

}

void stmts_append_declaration(Stmts *s, string name, Type type) {

  Stmt_Declaration declaration;
  declaration.name = name;
  declaration.type = type;

  da_append(s, ((Stmt) {
	.type = STMT_TYPE_DECLARATION,
	.as.declaration = declaration
      }));
}

void stmts_append_assignment(Stmts *s, Expr *lhs, Expr *rhs) {
  
  Stmt_Assignment assignment;
  assignment.lhs = lhs;
  assignment.rhs = rhs;

  da_append(s, ((Stmt) {
	.type = STMT_TYPE_ASSIGNMENT,
	.as.assignment = assignment
      }));
}

#define stmts_append_funccall(stmts, name, ...) \
  stmts_append_funccall_impl((stmts), (name), __VA_ARGS__, NULL)

Stmt *stmts_append_funccall_impl(Stmts *stmts, string name, ...) {

  Expr_Funccall funccall;
  funccall.name = name;
  
  va_list args;
  va_start(args, name);

  funccall.args_len = 0;
  for(;;) {

    Expr *arg = va_arg(args, Expr *);
    if(arg == NULL) {
      break;
    }

    if(funccall.args_len >= sizeof(funccall.args)/sizeof(funccall.args[0])) {
      panic("argument overflow");
    }
    
    funccall.args[funccall.args_len++] = arg;
  }  
  va_end(args);
  
  da_append(stmts, ((Stmt) {
      .type = STMT_TYPE_FUNCCALL,
      .as.funccall = funccall,
      }));

  return &stmts->data[stmts->len - 1];
}

void stmts_append_if(Stmts *s,
		     Expr *lhs,
		     Stmt_If_Type operand,
		     Expr *rhs,
		     Stmts *body) {

  Stmt_If iff;
  iff.rhs = rhs;
  iff.type = operand;
  iff.lhs = lhs;
  iff.body = body;

  da_append(s, ((Stmt) {
	.type = STMT_TYPE_IF,
	.as.iff = iff
      }));
}

///////////////////////////////////////////////////////////

Program hello_world_program() {

  Program program = {0};
  
  // handle : u64*  
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("handle"),
			   TYPE_U64_PTR);

  // handle = GetStdHandle(-11);
  stmts_append_assignment(&program.stmts,
			  exprs_append_variable(&program.exprs, string_from_cstr("handle")),
			  exprs_append_funccall(&program.exprs,
						string_from_cstr("GetStdHandle"),
						exprs_append_value(&program.exprs, -11)));

  // if(handle == -1) {
  //   ExitProcess(GetLastError());
  // }
  static Stmts if_body = {0};
  
  stmts_append_funccall(&if_body,
		        string_from_cstr("ExitProcess"),
			exprs_append_funccall(&program.exprs,
					      string_from_cstr("GetLastError")));

  stmts_append_if(&program.stmts,
		  exprs_append_variable(&program.exprs, string_from_cstr("handle")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_value(&program.exprs, 1),
		  &if_body);

  // written : u64
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("written"),
			   TYPE_U64);

  // if(WriteFile(handle, hello_world_data, hello_world_len, &written, NULL) == 0) {
  //   ExitProcess(GetLastError());
  // }
  string message = string_from_cstr("Hello, World!");
  stmts_append_if(&program.stmts,
		  exprs_append_funccall(&program.exprs,
					string_from_cstr("WriteFile"),
					exprs_append_variable(&program.exprs, string_from_cstr("handle")),
					exprs_append_constant(&program.exprs,
							      constants_append_cstr(&program.constants, message.data)),
					exprs_append_constant(&program.exprs,
							      constants_append_u64(&program.constants, message.len)),
					exprs_append_pointer(&program.exprs, string_from_cstr("written")),
					exprs_append_value(&program.exprs, 0)),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_value(&program.exprs, 0),
		  &if_body
		  );
   return program;
}

Program slurp_file_program() {
  Program program = {0};

  // handle : u64*
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("handle"),
			   TYPE_U64_PTR);
  Expr *handle = exprs_append_variable(&program.exprs, string_from_cstr("handle"));

  /* handle = CreateFileA("main.asm",
     GENERIC_READ,
     FILE_SHARE_READ,
     0,
     OPEN_EXISTING,
     FILE_ATTRIBUTE_NORMAL,
     NULL);
  */
  stmts_append_assignment(&program.stmts,
			  handle,
			  exprs_append_funccall(&program.exprs,
						string_from_cstr("CreateFileA"),
						exprs_append_constant(&program.exprs,
								      constants_append_cstr(&program.constants,"main.fe")),
						exprs_append_value(&program.exprs, GENERIC_READ),
						exprs_append_value(&program.exprs, FILE_SHARE_READ),
						exprs_append_value(&program.exprs, 0),
						exprs_append_value(&program.exprs, OPEN_EXISTING),
						exprs_append_value(&program.exprs, FILE_ATTRIBUTE_NORMAL),
						exprs_append_value(&program.exprs, 0)
						));

  /* if(handle == INVALID_HANDLE_VALUE) {
     ExitProcess(GetLastError());
     }
  */
  static Stmts if_body = {0};
  stmts_append_funccall(&if_body,
			string_from_cstr("ExitProcess"),
			exprs_append_funccall(&program.exprs,
					      string_from_cstr("GetLastError")));
  stmts_append_if(&program.stmts,
		  handle,
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_value(&program.exprs, (s64) INVALID_HANDLE_VALUE),
		  &if_body);

  // size : u64;
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("size"),
			   TYPE_U64);  
  Expr *size = exprs_append_variable(&program.exprs, string_from_cstr("size"));
  /*
    size = GetFileSize(handle);
  */
  stmts_append_assignment(&program.stmts,
			  size,
			  exprs_append_funccall(&program.exprs,
						string_from_cstr("GetFileSize"),
						handle));

  /*
    if(size == INVALID_FILE_SIZE) {
    ExitProcess(GetLastError());
    }
  */
  stmts_append_if(&program.stmts,
		  size,
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_value(&program.exprs, INVALID_FILE_SIZE),
		  &if_body);

  // space = u64*
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("space"),
			   TYPE_U64_PTR);
  Expr *space = exprs_append_variable(&program.exprs, string_from_cstr("space"));

  // space = HeapAlloc(GetProcessHeap(), 0, size);
  Expr *zero = exprs_append_value(&program.exprs, 0);
  Expr *getProcessHeap = exprs_append_funccall(&program.exprs,
					       string_from_cstr("GetProcessHeap"));
  Expr *heapAlloc = exprs_append_funccall(&program.exprs,
					  string_from_cstr("HeapAlloc"),
					  getProcessHeap,
					  zero,
					  size);
  stmts_append_assignment(&program.stmts,
			  space,
			  heapAlloc);
  
  // written : u64*
  stmts_append_declaration(&program.stmts,
			   string_from_cstr("written"),
			   TYPE_U64);
  Expr *written = exprs_append_variable(&program.exprs, string_from_cstr("written"));

  // ReadFile(handle, space, size, &written, NULL)
  stmts_append_funccall(&program.stmts,
			string_from_cstr("ReadFile"),
			handle,
		        space,
			size,
			exprs_append_pointer(&program.exprs, string_from_cstr("written")),
			exprs_append_value(&program.exprs, 0));

  // WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), space, written, &written, NULL);
  stmts_append_funccall(&program.stmts,
			string_from_cstr("WriteFile"),
		        exprs_append_funccall(&program.exprs,
					      string_from_cstr("GetStdHandle"),
					      exprs_append_value(&program.exprs, STD_OUTPUT_HANDLE)),
		        space,
		        written,
			exprs_append_pointer(&program.exprs, string_from_cstr("written")),
			exprs_append_value(&program.exprs, 0));

  // CloseHandle(handle);
  stmts_append_funccall(&program.stmts,
		        string_from_cstr("CloseHandle"),
			handle);

  return program;
}

// TODO:
//     derefence ptr
//     structures
//     allocate arrays on stack
//     arithmetic
//     foreign functions

//     create stream of instructions and remove unnecassary

int main() {

  string_builder sb = {0};
    
  Program program = {0};

  stmts_append_funccall(&program.stmts,
			string_from_cstr("ExitProcess"),
			exprs_append_funccall(&program.exprs, string_from_cstr("foo")));
  
  program_compile(&program, &sb);

  printf("\n===================================================\n");
  printf("\n%.*s\n", (int) sb.len, sb.data);
  printf("\n===================================================\n");
  
  if(!io_write_file(FILE_PATH, (u8 *) sb.data, sb.len)) {
    return 1;
  }
  
  return 0;
}
