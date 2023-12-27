#include <stdio.h>

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

char *FASTCALL_REGISTER_NAMES[] = {
  "rcx", "rdx", "r8", "r9"
};
u64 FASTCALL_REGISTER_COUNT =
  sizeof(FASTCALL_REGISTER_NAMES)/sizeof(FASTCALL_REGISTER_NAMES[0]);

typedef enum{
  TYPE_NONE,
  TYPE_U64,
}Type;

u64 type_size(Type type) {
  switch(type) {

  case TYPE_U64:
    return 16;
    
  default: {
    panic("unimplemented type");
  } break;
  }
}

const char *type_cstr(Type type) {
  switch(type) {

  case TYPE_U64:
    return "u64";
    
  default: {
    fprintf(stderr, "ERROR: unimplemented data_type: data_type_cstr");
    exit(1);
  } break;
  }
}

typedef enum{
  EXPR_TYPE_NONE,
  EXPR_TYPE_POP,
  EXPR_TYPE_CONSTANT,
  EXPR_TYPE_VARIABLE,
  EXPR_TYPE_FUNCCALL,
}Expr_Type;

#define FUNCCALL_ARGS_CAP 8

typedef struct Expr Expr;

typedef struct{
  Expr *args[FUNCCALL_ARGS_CAP];
  u64 args_len;
  string name;
}Expr_Funccall;

struct Expr{
  Expr_Type type;
  union{
    s64 sval;
    string stringval;
    Expr_Funccall funccall;
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

typedef struct{
  Stmts stmts;
  Vars vars;
  
  u64 stack_ptr;
}Program;

void program_funccall_compile(Program *p, Expr_Funccall *funccall, string_builder *sb) {
  panic("unimplemented");
}

/*
void program_funccall_compile(Program *p, Expr_Funccall *funccall, string_builder *sb) {

  string_builder_appendc(sb, "        ;; EXPR_FUNCCALL\n");
  string_builder_appendf(sb, "        ;;     name=\""str_fmt"\"\n", str_arg(funccall->name));
  string_builder_appendf(sb, "        ;;         args_len=%llu\n", funccall->args_len);

  for(u64 i=0;i<funccall->args_len;i++) {
    string_builder_appendf(sb, "        ;;    arg%llu=\n", i);  
    

    Expr *arg = funccall->args[i];
    switch(arg->type) {

    case EXPR_TYPE_CONSTANT: {

      string_builder_appendc(sb, "        ;;        EXPR_TYPE_CONSTANT\n");
      string_builder_appendf(sb, "        ;;            value=%lld\n", arg->as.sval);	

      if(i < FASTCALL_REGISTER_COUNT) {
	string_builder_appendf(sb, "        mov %s, %lld\n", FASTCALL_REGISTER_NAMES[i], arg->as.sval);
      } else {
	string_builder_appendf(sb, "        push qword %lld\n", arg->as.sval);
      }
    } break;

    case EXPR_TYPE_VARIABLE: {

      Var *var = &p->vars[arg->as.uval];

      string_builder_appendc(sb, "        ;;        EXPR_TYPE_VARIABLE\n");
      string_builder_appendf(sb, "        ;;            name=\""str_fmt"\"\n", str_arg(var->name));	
	
      if(i < FASTCALL_REGISTER_COUNT) {
	string_builder_appendf(sb, "        mov %s, [rsp + %lld]\n",
			       FASTCALL_REGISTER_NAMES[i], p->stack_ptr - var->off);
      } else {
	string_builder_appendf(sb, "        push [rsp + %lld]\n", p->stack_ptr - var->off);
      }
		
    } break;
	
    default: {
      fprintf(stderr, "ERROR: unimplemented expr_type: program_funccall_compile\n");
      exit(1);
    } break;
    }
    
  }

  u64 shadow_space = 40;
  if(funccall->args_len > 4 &&
     funccall->args_len % 2 == 1) {
    shadow_space = 32;
  }  
  string_builder_appendf(sb, "        sub rsp, %llu\n", shadow_space);
  string_builder_appendf(sb, "        call "str_fmt"\n", str_arg(funccall->name));

  u64 space_to_deallocate = shadow_space;
  if(funccall->args_len > 4) {
    if(shadow_space == 32) {
      space_to_deallocate += funccall->args_len * 8 - 8;
    } else {
      space_to_deallocate += funccall->args_len * 8;  
    }    
  }
  string_builder_appendf(sb, "        add rsp, %llu\n\n", space_to_deallocate);

}
  */

void program_expr_compile(Program *p, Expr *e, Location loc, string_builder *sb) {

  switch(e->type) {

  case EXPR_TYPE_CONSTANT: {

    string_builder_appendc(sb, "        ;; EXPR_CONSTANT\n");
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
    
    string_builder_appendc(sb, "        ;; EXPR_VARIABLE\n");
    string_builder_appendf(sb, "        ;;         name=\""str_fmt"\"\n", str_arg(e->as.stringval));

    switch(loc.type) {

    case LOCATION_TYPE_REGISTER: {
      string_builder_appendf(sb, "        mov %s, [rsp + %llu]\n",
			     REGISTER_NAMES[loc.value], p->stack_ptr - var->off);
    } break;

    case LOCATION_TYPE_PUSH: {
      string_builder_appendf(sb, "        push qword, [rsp + %llu]\n",
			     REGISTER_NAMES[loc.value], p->stack_ptr - var->off);
      p->stack_ptr += 8;
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

/*
u64s program_expr_compile(Program *p, Expr *e, string_builder *sb) {

  u64s off;

  switch(e->type) {
    
  case EXPR_TYPE_VARIABLE: {

    Var *var = &p->vars[e->as.uval];

    string_builder_appendf(sb, "        ;; EXPR_VARIABLE\n");
    string_builder_appendf(sb, "        ;;     name=\""str_fmt"\"n", str_arg(var->name));
    string_builder_appendf(sb, "        mov rax, [rsp + %llu]\n", p->stack_ptr - var->off);
    string_builder_appendf(sb, "        sub rsp, 16\n");
    off.fst = 16;
    off.snd = p->stack_ptr;
    p->stack_ptr += 16;
    string_builder_appendf(sb, "        mov [rsp], rax\n\n", p->stack_ptr - var->off);
    
  } break;

  case EXPR_TYPE_CONSTANT: {

    string_builder_appendf(sb, "        ;; EXPR_CONSTANT\n");
    string_builder_appendf(sb, "        ;;     value=%lld\n", e->as.sval);
    string_builder_appendf(sb, "        sub rsp, 16\n");
    off.fst = 16;
    off.snd = p->stack_ptr;
    p->stack_ptr += 16;
    string_builder_appendf(sb, "        mov qword [rsp], %llu\n\n", e->as.sval);
    
  } break;
    
  default: {
    fprintf(stderr, "ERROR: unimplemented expr_type: program_expr_copmile\n");
    exit(1);
  } break;
    
  }

  return off;
}
*/

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

    Var *var = vars_append(&p->vars);
    var->name = declaration->name;
    var->type = declaration->type;
    var->off  = p->stack_ptr;

    string_builder_appendc(sb, "        ;; STMT_DECLARATION\n");
    string_builder_appendf(sb, "        ;;     name=\""str_fmt"\"\n", str_arg(declaration->name));
    string_builder_appendf(sb, "        ;;     type=%s\n", type_cstr(declaration->type));
    
    u64 size = type_size(declaration->type);
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

    Expr_Funccall *funccall = &s->as.funccall;

    for(u64 i=0;i<funccall->args_len;i++) {
      Location loc;
      if(i < FASTCALL_REGISTER_COUNT) {
	// "value = i" is only valid, since REGISTER_NAMES 
	// are starting with the FASTCALL_REGISTER_NAMES
	loc = (Location) { .type = LOCATION_TYPE_REGISTER, .value = i };
      } else {
	loc = (Location) { .type = LOCATION_TYPE_PUSH };
      }
      
      program_expr_compile(p, funccall->args[i], loc, sb);
    }
    
  } break;

  case STMT_TYPE_IF: {

    Stmt_If *iff = &s->as.iff;
    Expr *lhs = iff->lhs;
    Expr *rhs = iff->rhs;
    
    program_expr_compile(p, lhs, (Location) { .type = LOCATION_TYPE_PUSH }, sb);
    program_expr_compile(p, rhs, (Location) { .type = LOCATION_TYPE_PUSH }, sb);

    
    
  } break;
    
  default: {
    panic("unimplemented stmt_type");
  } break;
  }

  string_builder_appendc(sb, "\n");
}

/*
void program_stmt_compile(Program *p, Stmt *s, string_builder *sb) {
  switch(s->type) {

  case STMT_TYPE_DECLARATION: {
    Var *var = &p->vars[s->as.var_index];

    u64 size = data_type_size(var->type);
    var->off = p->stack_ptr + size;
    p->stack_ptr += size;

    string_builder_appendf(sb,"        ;; STMT_DECLARATION\n");
    string_builder_appendf(sb,"        ;;     name=\""str_fmt"\"\n", str_arg(var->name));
    string_builder_appendf(sb,"        ;;     type=%s\n", data_type_cstr(var->type));
    string_builder_appendf(sb,"        sub rsp, %llu\n\n", size);
    
  } break;

  case STMT_TYPE_ASSIGNMENT: {

    string_builder_appendc(sb, "        ;; STMT_ASSIGN\n");

    Expr *lhs = s->as.assignment.lhs;
    Expr *rhs = s->as.assignment.rhs;

    if(lhs->type != EXPR_TYPE_VARIABLE) {
      fprintf(stderr, "ERROR: unimplemented expr_type: program_stmt_compile, ASSIGNMENT, lhs\n");
      exit(1);
    }
    Var *dst = &p->vars[lhs->as.uval];
    string_builder_appendc(sb, "        ;;    lhs=\n");
    string_builder_appendc(sb, "        ;;        EXPR_TYPE_VARIABLE\n");
    string_builder_appendf(sb, "        ;;            name=\""str_fmt"\"\n", str_arg(dst->name));

    string_builder_appendc(sb, "        ;;    rhs=\n");
    u64s src = program_expr_compile(p, s->rhs);
    string_builder_appendf(sb, "        mov rax, [rsp + %llu]\n", p->stack_ptr - src.snd);
    string_builder_appendf(sb, "        mov [rsp + %llu], rax\n", p->stack_ptr - dst->off);
    
    switch(rhs->type) {

    case EXPR_TYPE_CONSTANT: {
      
      string_builder_appendc(sb, "        ;;        EXPR_TYPE_CONSTANT\n");
      string_builder_appendf(sb, "        ;;            value=%lld\n", rhs->as.sval);
      string_builder_appendf(sb, "        mov qword [rsp + %llu], %lld\n\n",
			     p->stack_ptr - dst->off, rhs->as.sval);
      
    } break;

    case EXPR_TYPE_VARIABLE: {

      Var *src = &p->vars[rhs->as.uval];
      string_builder_appendc(sb, "        ;;        EXPR_TYPE_VARIABLE\n");
      string_builder_appendf(sb, "        ;;            name=\""str_fmt"\"\n", str_arg(src->name));
      string_builder_appendf(sb, "        mov rax, [rsp + %llu]\n", p->stack_ptr - src->off);
      string_builder_appendf(sb, "        mov [rsp + %llu], rax\n\n",
			     p->stack_ptr - dst->off);
      
    } break;

    case EXPR_TYPE_FUNCCALL: {

      program_funccall_compile(p, &rhs->as.funccall, sb);
      string_builder_appendf(sb, "        mov qword [rsp + %llu], rax\n\n", p->stack_ptr - dst->off);
      
    } break;
      
    default: {
      fprintf(stderr, "ERROR: unimplemented expr_type: program_stmt_compile, ASSIGNMENT, rhs\n");
      exit(1);
    } break;
    }
    
  } break;

  case STMT_TYPE_FUNCCALL: {
    program_funccall_compile(p, &s->as.funccall, sb);
    
  } break;

  case STMT_TYPE_IF: {

    Stmt_If *iff = &s->as.iff;

    u64s lhs = program_expr_compile(p, iff->lhs, sb);
    u64s rhs = program_expr_compile(p, iff->rhs, sb);
    string_builder_appendf(sb, "        mov rax, [rsp + %llu]\n",
			   p->stack_ptr + lhs.snd);
    string_builder_appendf(sb, "        mov rdx, [rsp + %llu]\n",
			   p->stack_ptr + rhs.snd);
    u64 off = lhs.fst + rhs.fst;
    string_builder_appendf(sb, "        add rsp, %llu\n", off);
    p->stack_ptr -= off;
    string_builder_appendf(sb, "        cmp rax, rdx\n");

    switch(iff->type) {
      
    case STMT_IF_TYPE_EQUALS: {
      string_builder_appendf(sb, "        jne .label_%p\n", (void *) s);
    } break;

    default: {
      fprintf(stderr, "ERROR: unimplemented stmt_if_type: program_stmt_compile\n");
      exit(1);
    } break;
      
    }

    for(u64 i=0;i<iff->body->len;i++) {
      Stmt *stmt = &iff->body->data[i];
      program_stmt_compile(p, stmt, sb);
    }

    string_builder_appendf(sb, ".label_%p:\n\n", (void *) s);
    
  } break;
    
  default: {
    fprintf(stderr, "ERROR: unimplemented stmt: program_stmt_compile\n");
    exit(1);
  }
    
  }
}
*/

void program_compile(Program *p, string_builder *sb) {
  string_builder_appendc(sb,
			 "        global main\n"
			 "        extern ExitProcess\n"
			 "        extern GetStdHandle\n"
			 "\n"
			 "        section .text:\n"
			 "main:\n");

  for(u64 i=0;i<p->stmts.len;i++) {
    program_stmt_compile(p, &p->stmts.data[i], sb);
  }

  if(p->stack_ptr != 0) {
    string_builder_appendf(sb,
			   "        add rsp, %llu\n\n", p->stack_ptr);
  }
  
  string_builder_appendc(sb,
			 "        mov rcx, 0\n"
			 "        sub rsp, 8\n"
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

int main() {

  string_builder sb = {0};

  u64 args_len;
  Expr *args[FUNCCALL_ARGS_CAP];
  
  Stmts body = {0};
  s64 value = 69;

  // foo : u64;
  stmts_append_declaration(&body,
			   string_from_cstr("foo"),
			   TYPE_U64);
  // foo = value;  
  Expr expr_foo = {
    .type=EXPR_TYPE_VARIABLE,
    .as.stringval = string_from_cstr("foo"),
  };
  Expr expr_value = {
    .type=EXPR_TYPE_CONSTANT,
    .as.sval = value
  };
  stmts_append_assignment(&body,
			    &expr_foo,
			    &expr_value);

  // if(value == 2) {
  //   foo = 4;
  // }
  Expr expr_4 = { .type = EXPR_TYPE_CONSTANT, .as.sval = 4 };
  
  Stmts if_body = {0};
  stmts_append_assignment(&if_body,
			  &expr_foo,
			  &expr_4);

  Expr expr_2 = { .type=EXPR_TYPE_CONSTANT, .as.sval = 2 };
  stmts_append_if(&body,
		    &expr_value,
		    STMT_IF_TYPE_EQUALS,
		    &expr_2,
		    &if_body);
  
  // ExitProcess(foo);
  args_len = 0;
  args[args_len++] = &expr_foo;
  stmts_append_funccall(&body,
			  string_from_cstr("ExitProcess"),
			  args, args_len);
  
  
  /* // handle : u64; */
  /* u64 variable_handle = program_append_declaration(&program, */
  /* 						   string_from_cstr("handle"), DATA_TYPE_U64); */
  /* // handle = GetStdHandle(-11); */
  /* Expr expr_handle = { .type=EXPR_TYPE_VARIABLE, .as.uval = variable_handle }; */
  /* Expr expr_neg_11 = (Expr) { .type=EXPR_TYPE_CONSTANT, .as.sval=-11 }; */
  /* args_len = 0; */
  /* Expr expr_getstdhandle; */
  /* expr_getstdhandle.type = EXPR_TYPE_FUNCCALL; */
  /* Expr_Funccall *expr_funccall = &expr_getstdhandle.as.funccall; */
  /* expr_funccall->args_len = 1; */
  /* expr_funccall->args[0] = &expr_neg_11; */
  /* expr_funccall->name = string_from_cstr("GetStdHandle"); */
  /* program_append_assignment(&program, */
  /* 			    &expr_handle, */
  /* 			    &expr_getstdhandle); */

  /* // foo : u64; */
  /* u64 variable_foo = program_append_declaration(&program, */
  /* 						string_from_cstr("foo"), DATA_TYPE_U64); */
  /* // foo = handle; */
  /* Expr expr_0 = { .type=EXPR_TYPE_CONSTANT, .as.sval=0 }; */
  /* Expr expr_foo = (Expr) { .type=EXPR_TYPE_VARIABLE, .as.uval=variable_foo }; */
  /* program_append_assignment(&program, &expr_foo, &expr_handle); */

  /* // foo = 0 */
  /* program_append_assignment(&program, &expr_foo, &expr_0); */

  /* // if(foo == handle) { */
  /* //   ExitProcess(1); */
  /* // } */
  /* Stmts stmts = {0}; */
  /* Stmt *stmt = stmts_append(&stmts); */
  /* stmt->type = STMT_TYPE_FUNCCALL; */
  /* Expr expr_1 = { .type=EXPR_TYPE_CONSTANT, .as.sval = 1}; */
  /* Expr_Funccall *funccall = &stmt->as.funccall; */
  /* args_len = 0; */
  /* args[args_len++] = &expr_1; */
  /* memcpy(funccall->args, args, args_len * sizeof(Expr*)); */
  /* program_append_if(&program, */
  /* 		    &expr_foo, */
  /* 		    STMT_IF_TYPE_EQUALS, */
  /* 		    &expr_handle, */
  /* 		    &stmts); */
      
  /* // ExitProcess(foo); */
  /* args_len = 0; */
  /* args[args_len++] = &expr_foo; */
  /* program_append_funccall(&program, */
  /* 			  string_from_cstr("ExitProcess"), */
  /* 			  args, args_len); */
  Program program = {0};
  program.stmts = body;
  program_compile(&program, &sb);

  printf("\n===================================================\n");
  printf("\n%.*s\n", (int) sb.len, sb.data);
  printf("\n===================================================\n");
  
  if(!io_write_file(FILE_PATH, (u8 *) sb.data, sb.len)) {
    return 1;
  }
  
  return 0;
}
