#include <stdio.h>
#include <assert.h>

#define STRING_IMPLEMENTATION
#include "_string.h"

#define IO_IMPLEMENTATION
#include "io.h"

#define panic(fmt, ...) do{ fprintf(stderr, "%s:%d:%s:ERROR: " fmt "\n", __FILE__, __LINE__, __func__, __VA_ARGS__); exit(1); }while(0)

typedef long long int s64;
typedef unsigned long long int u64;
typedef unsigned char u8;

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

typedef enum{
  VALUE_TYPE_NONE = 0,
  VALUE_TYPE_LITERAL,
  VALUE_TYPE_REGISTER,
  VALUE_TYPE_REGISTER_OFF,
  VALUE_TYPE_WORD,  
}Value_Type;

char *REGISTER_NAMES[] = {
  "rax", "rbx", "rcx", "rsp", "rbp", "rdi", "rsi", "rdx", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
};
u64 REGISTER_NAMES_COUNT =
  sizeof(REGISTER_NAMES)/sizeof(REGISTER_NAMES[0]);

u64 FASTCALL_REGISTERS[] = {
  2, 5, 7, 8, 9
};
u64 FASTCALL_REGISTER_COUNT =
  sizeof(FASTCALL_REGISTERS)/sizeof(FASTCALL_REGISTERS[0]);

#define REGISTER(n) ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (n) })
#define RAX ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (0) })
#define RBX ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (1) })
#define RCX ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (2) })
#define RSP ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (3) })

#define LITERAL(n) ((Value) { .type = VALUE_TYPE_LITERAL, .as.sval = (n) })
#define WORD(s) ((Value) { .type = VALUE_TYPE_WORD, .as.stringval = (s) })
#define RSP_OFF(n) ((Value) { .type = VALUE_TYPE_REGISTER_OFF, .as.sval = (3), .off = (n) })

typedef struct{
  Value_Type type;
  
  union {
    string stringval;
    s64 sval;
  }as;  
  s64 off;
}Value;

typedef enum {
  INSTR_TYPE_NONE = 0,
  INSTR_TYPE_MOV,
  INSTR_TYPE_SUB,
  INSTR_TYPE_ADD,
  INSTR_TYPE_CALL,
}Instr_Type;

typedef struct{
  Instr_Type type;
  Value lhs;
  Value rhs;
}Instr;

#define MOV(l, r) ((Instr) { .type = INSTR_TYPE_MOV, .lhs = (l), .rhs = (r), })
#define SUB(l, r) ((Instr) { .type = INSTR_TYPE_SUB, .lhs = (l), .rhs = (r), })
#define ADD(l, r) ((Instr) { .type = INSTR_TYPE_ADD, .lhs = (l), .rhs = (r), })
#define CALL(l) ((Instr) { .type = INSTR_TYPE_CALL, .lhs = (l) })

void instr_append(Instr* instr, string_builder *sb) {
  Value lhs = instr->lhs;
  Value rhs = instr->rhs;

  switch(instr->type) {

  case INSTR_TYPE_MOV: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_LITERAL)  {
      string_builder_appendf(sb, "        mov qword %s, %lld\n",
			     REGISTER_NAMES[lhs.as.sval], rhs.as.sval);
    } else if(lhs.type == VALUE_TYPE_REGISTER &&
	       rhs.type == VALUE_TYPE_REGISTER) {
      string_builder_appendf(sb, "        mov %s, %s\n",
			     REGISTER_NAMES[lhs.as.sval], REGISTER_NAMES[rhs.as.sval]);
    } else if(lhs.type == VALUE_TYPE_REGISTER_OFF &&
	      rhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        mov qword [%s + %lld], %lld\n",
			     REGISTER_NAMES[lhs.as.sval],
			     lhs.off,
			     rhs.as.sval);
    } else if(lhs.type == VALUE_TYPE_REGISTER_OFF &&
	      rhs.type == VALUE_TYPE_REGISTER) {
      string_builder_appendf(sb, "        mov [%s + %lld], %s\n",
			     REGISTER_NAMES[lhs.as.sval],
			     lhs.off,
			     REGISTER_NAMES[rhs.as.sval]);
      
    } else if(lhs.type == VALUE_TYPE_REGISTER &&
	      rhs.type == VALUE_TYPE_REGISTER_OFF) {
      string_builder_appendf(sb, "        mov %s, [%s + %lld]\n",
			     REGISTER_NAMES[lhs.as.sval],
			     REGISTER_NAMES[rhs.as.sval],
			     rhs.off);
    } else {
      panic("Unimplemented value->type's"); 
    }
    
  } break;

  case INSTR_TYPE_SUB: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_LITERAL)  {
      string_builder_appendf(sb, "        sub %s, %lld\n",
			     REGISTER_NAMES[lhs.as.sval], rhs.as.sval);
    } else {
      panic("Unimplemented value->type's");    	
    }
      
  } break;

  case INSTR_TYPE_ADD: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_LITERAL)  {
      string_builder_appendf(sb, "        add %s, %lld\n",
			     REGISTER_NAMES[lhs.as.sval], rhs.as.sval);
    } else {
      panic("Unimplemented value->type's");    	
    }
      
  } break;

  case INSTR_TYPE_CALL: {

    if(lhs.type == VALUE_TYPE_WORD)  {
      string_builder_appendf(sb, "        call "str_fmt"\n",
			     str_arg(lhs.as.stringval));
    } else {
      panic("Unimplemented value->type");
    }
    
  } break;
        
  default:
    panic("Unimplemented instr->type");
  }
}

typedef struct{
  Instr *data;
  u64 cap;
  u64 len;
}Instrs;

void instrs_append(Instrs *instrs, string_builder *sb) {
  for(u64 i=0;i<instrs->len;i++) {
    instr_append(&instrs->data[i], sb);
  }
}

#define FUNCCALL_ARGS_CAP 8

typedef enum{
  EXPR_TYPE_NONE,
  EXPR_TYPE_POP,
  EXPR_TYPE_VALUE,
  EXPR_TYPE_VARIABLE,
  EXPR_TYPE_FUNCCALL,
  EXPR_TYPE_CONSTANT,
  EXPR_TYPE_VARIABLE_PTR,
}Expr_Type;

typedef struct Expr Expr;

typedef struct{
  Expr *args[FUNCCALL_ARGS_CAP];
  u64 args_len;
  string name;
}Expr_Funccall;

typedef struct Constant Constant;

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

typedef struct{
  Expr expr_pool[16];
  u64 expr_pool_count;
  
  Vars vars;
  Instrs instrs;

  u64 stack_ptr;
}Program;

Expr *program_expr_append(Program *p) {
  if(p->expr_pool_count >= sizeof(p->expr_pool)/sizeof(p->expr_pool[0])) {
    panic("expr_pool overflow");
  }
  return &p->expr_pool[p->expr_pool_count++];
}

Expr *program_expr_append_value(Program *p, s64 value) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_VALUE;
  e->as.sval = value;

  return e;
}

Expr *program_expr_append_variable(Program *p, string name) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_VARIABLE;
  e->as.stringval = name;
  
  return e;
}

#define program_expr_append_funccall(exprs, name, ...)			\
  program_expr_append_funccall_impl((exprs), (name), __VA_ARGS__, NULL)

Expr *program_expr_append_funccall_impl(Program *p, string name, ...) {
  
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

  Expr *e = program_expr_append(p);
  e->type = EXPR_TYPE_FUNCCALL;
  e->as.funccall = funccall;
  
  return e;
}

void program_append(Program *p, string_builder *sb) {

  string_builder_appendf(sb,
			 "        global main\n"
			 "        extern GetStdHandle\n"
			 "        extern ExitProcess\n"
			 "        section .text\n"
			 "main:\n");
  
  instrs_append(&p->instrs, sb);

  string_builder_appendf(sb,
			 "        mov rcx, 0\n"
			 "        sub rsp, 40\n"
			 "        call ExitProcess\n");
}

void program_compile_stmt_declaration(Program *p,
				      string name,
				      Type type) {
  if(vars_find(&p->vars, name, TYPE_NONE)) {
    panic("variable already declared");
  }

  u64 size = type_size(type);

  Var var;
  var.name = name;
  var.type = type;
  var.off  = p->stack_ptr + size;

  da_append(&p->instrs, SUB(RSP, LITERAL(size)));
  p->stack_ptr += size;
  
  da_append(&p->vars, var);

  
  p->expr_pool_count = 0;
}

Value program_expr_location(Program *p, Expr *e) {
  switch(e->type) {

  case EXPR_TYPE_VARIABLE: {
    
    Var *var = vars_find(&p->vars, e->as.stringval, TYPE_NONE);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(e->as.stringval));
    }

    return RSP_OFF(var->off - type_size(var->type));
    
  } break;
    
  default:
    panic("unimplemented expr_type: %d", e->type);

  }
}

void program_compile_funccall(Program *p, Expr_Funccall *funccall);

void program_expr_compile(Program *p,
			  Expr *e,
			  Value location) {

  switch(e->type) {

  case EXPR_TYPE_VALUE: {
    
    da_append(&p->instrs, MOV(location, LITERAL(e->as.sval)));
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    program_compile_funccall(p, &e->as.funccall);
    da_append(&p->instrs, MOV(location, RAX));
    
  } break;

  default:
    panic("Unimplemented expr->type");
    
  }
  
}

void program_compile_funccall(Program *p, Expr_Funccall *funccall) {

  u64 n = funccall->args_len * 8;
  if(n < 32) {
    n = 32;
  }
  p->stack_ptr += n;
  u64 alignment = p->stack_ptr % 16;
  if(alignment != 0) {
    p->stack_ptr += alignment;
    n += alignment;
  }
  da_append(&p->instrs, SUB(RSP, LITERAL(n)));  
  
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];
    program_expr_compile(p, e, RSP_OFF(i * 8));
  }    
  for(u64 i=0;i<funccall->args_len;i++) {
    da_append(&p->instrs, MOV(REGISTER(FASTCALL_REGISTERS[i]), RSP_OFF(i * 8)));
  }

  da_append(&p->instrs, CALL(WORD(funccall->name)));
  
  da_append(&p->instrs, ADD(RSP, LITERAL(n)));
  p->stack_ptr -= n;

  
}

void program_compile_assignment(Program *p,
				Expr *lhs,
				Expr *rhs) {
  
  program_expr_compile(p, rhs, program_expr_location(p, lhs));

  p->expr_pool_count = 0;
}

int main() {

  Program program = {0};

  // handle : u64*;
  program_compile_stmt_declaration(&program,
				   string_from_cstr("handle"),
				   TYPE_U64_PTR);
  
  // handle = GetStdHandle(-11);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("handle")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetStdHandle"),
							  program_expr_append_value(&program,
										    -11)));
  string_builder sb = {0};
  program_append(&program, &sb);
  printf("%.*s", (int) sb.len, sb.data);

  if(!io_write_file("main.asm", (u8 *) sb.data, sb.len)) {
    return 1;
  }


  return 0;
}
