#include <stdio.h>
#include <limits.h>
#include <assert.h>

// https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture
// https://en.wikipedia.org/wiki/X86_instruction_listings

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
      if(new_cap == 0) new_cap = 2;					\
    }									\
    if(new_cap != (n)->cap) {						\
      (n)->cap = new_cap;						\
      (n)->data = realloc((n)->data, (n)->cap * sizeof(*((n)->data)));	\
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
  VALUE_TYPE_CONSTANT,
}Value_Type;



char *REGISTER_NAMES[][4] = {
  { "rax", "eax", "ax", "al" },
  { "rbx", "ebx", "bx", "bl" },
  { "rcx", "ecx", "cx", "cl" },
  { "rsp", "esp", "sp", "spl" },
  { "rbp", "ebp", "bp", "bpl" },
  { "rdi", "edi", "di", "dil" },
  { "rsi", "esi", "si", "sil" },
  { "rdx", "edx", "dx", "dl" },
  { "r8", "r8d", "r8w", "r8b" },
  { "r9", "r9d", "r9w", "r9b" },
  { "r10", "r10d", "r10w", "r10b" },
  { "r11", "r11d", "r11w", "r11b" },
  { "r12", "r12d", "r12w", "r12b" },
  { "r13", "r13d", "r13w", "r13b" },
  { "r14", "r14d", "r14w", "r14b" },
  { "r15", "r15d", "r15w", "r15b" },
};
u64 REGISTER_NAMES_COUNT =
  sizeof(REGISTER_NAMES)/sizeof(REGISTER_NAMES[0]);

u64 FASTCALL_REGISTERS[] = {
  2, 7, 8, 9
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
#define RAX_OFF(n) ((Value) { .type = VALUE_TYPE_REGISTER_OFF, .as.sval = (0), .off = (n) })
#define CONSTANT(n) ((Value) { .type = VALUE_TYPE_CONSTANT, .as.sval = (n), })

typedef struct{
  Value_Type type;
  
  union {
    string stringval;
    s64 sval;
  }as;  
  s64 off;
}Value;

typedef enum {
  SIZE_BYTE = 0,   //  8 bits 1 byte(s)
  SIZE_WORD,   // 16 bits 2 bytes
  SIZE_DWORD,  // 32 bits 4 bytes
  SIZE_QWORD,  // 64 bits 8 bytes
  COUNT_SIZES,
}Size;

u64 size_in_bytes(Size size) {
  switch(size) {
  case SIZE_BYTE:
    return 1;
  case SIZE_WORD:
    return 2;
  case SIZE_DWORD:
    return 4;
  case SIZE_QWORD:
    return 8;
  }

  panic("unimplemented size_type");
}

// TODO: i am not sure, if this is right
Size size_of_s64(s64 val) {
  if(val < CHAR_MAX) {
    return SIZE_BYTE;
  } else if(val < SHRT_MAX) {
    return SIZE_WORD;
  } else if(val < INT_MAX) {
    return SIZE_DWORD;
  } else {
    return SIZE_QWORD;
  }  
}

typedef enum {
  INSTR_TYPE_NONE = 0,
  INSTR_TYPE_MOV,
  INSTR_TYPE_SUB,
  INSTR_TYPE_ADD,
  INSTR_TYPE_CALL,
  INSTR_TYPE_CMP,
  INSTR_TYPE_JNE,
  INSTR_TYPE_JGE,
  INSTR_TYPE_JLE,
  INSTR_TYPE_JMP,
  INSTR_TYPE_JL,
  INSTR_TYPE_JE,
  INSTR_TYPE_LABEL,
  INSTR_TYPE_LEA,
}Instr_Type;

typedef struct{
  Instr_Type type;
  Value lhs;
  Value rhs;
  Size size;
}Instr;

#define MOV(l, r, s) ((Instr) { .type = INSTR_TYPE_MOV, .lhs = (l), .rhs = (r), .size = (s)})
#define LEA(l, r) ((Instr) { .type = INSTR_TYPE_LEA, .lhs = (l), .rhs = (r), })
#define SUB(l, r) ((Instr) { .type = INSTR_TYPE_SUB, .lhs = (l), .rhs = (r), })
#define ADD(l, r) ((Instr) { .type = INSTR_TYPE_ADD, .lhs = (l), .rhs = (r), })
#define CMP(l, r) ((Instr) { .type = INSTR_TYPE_CMP, .lhs = (l), .rhs = (r), })
#define CALL(l) ((Instr) { .type = INSTR_TYPE_CALL, .lhs = (l) })
#define JNE(l) ((Instr) { .type = INSTR_TYPE_JNE, .lhs = (l) })
#define JGE(l) ((Instr) { .type = INSTR_TYPE_JGE, .lhs = (l) })
#define JLE(l) ((Instr) { .type = INSTR_TYPE_JLE, .lhs = (l) })
#define JL(l) ((Instr) { .type = INSTR_TYPE_JL, .lhs = (l) })
#define JE(l) ((Instr) { .type = INSTR_TYPE_JE, .lhs = (l) })
#define JMP(l) ((Instr) { .type = INSTR_TYPE_JMP, .lhs = (l) })
#define LABEL(l) ((Instr) { .type = INSTR_TYPE_LABEL, .lhs = (l) })

void instr_append(Instr* instr, string_builder *sb) {
  Value lhs = instr->lhs;
  Value rhs = instr->rhs;

  switch(instr->type) {

  case INSTR_TYPE_MOV: {

    Size s = instr->size;
    if(s < SIZE_BYTE || COUNT_SIZES <= s) {
      panic("Invalid size: %d", s);
    }
    // TODO: yooooooooooooooooooooooo
    s = 3 - s;

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        mov %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       rhs.as.sval);
      } break;

      case VALUE_TYPE_REGISTER: {
	string_builder_appendf(sb, "        mov %s, %s\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       REGISTER_NAMES[rhs.as.sval][s]);
      } break;

      case VALUE_TYPE_REGISTER_OFF: {
	string_builder_appendf(sb, "        mov %s, [%s + %lld]\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       REGISTER_NAMES[rhs.as.sval][0], rhs.off);
      } break;

      case VALUE_TYPE_CONSTANT: {
	string_builder_appendf(sb, "        mov %s, constant%lld\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       rhs.as.sval);
      } break;
	
      default:
	panic("unimplemented value.type (rhs)");
      }
      
    } break;

    case VALUE_TYPE_REGISTER_OFF: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        mov [%s + %lld], %lld\n",
			       REGISTER_NAMES[lhs.as.sval][0], lhs.off,
			       rhs.as.sval);
      } break;

      case VALUE_TYPE_REGISTER: {
        string_builder_appendf(sb, "        mov [%s + %lld], %s\n",
			       REGISTER_NAMES[lhs.as.sval][0], lhs.off,
			       REGISTER_NAMES[rhs.as.sval][s]);
      } break;
	
      default:
	panic("unimplemented value.type (rhs)");
      }
      
    } break;


    default:
      panic("unimplemented value.type (lhs)");
      
    }

    /*
      if(lhs.type == VALUE_TYPE_REGISTER &&
      rhs.type == VALUE_TYPE_LITERAL)  {
      string_builder_appendf(sb, "        mov qword %s, %lld\n",
      REGISTER_NAMES[lhs.as.sval][0], rhs.as.sval);
      } else if(lhs.type == VALUE_TYPE_REGISTER &&
      rhs.type == VALUE_TYPE_REGISTER) {
      string_builder_appendf(sb, "        mov %s, %s\n",
      REGISTER_NAMES[lhs.as.sval][0], REGISTER_NAMES[rhs.as.sval][0]);
      } else if(lhs.type == VALUE_TYPE_REGISTER_OFF &&
      rhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        mov qword [%s + %lld], %lld\n",
      REGISTER_NAMES[lhs.as.sval][0],
      lhs.off,
      rhs.as.sval);
      } else if(lhs.type == VALUE_TYPE_REGISTER_OFF &&
      rhs.type == VALUE_TYPE_REGISTER) {
      string_builder_appendf(sb, "        mov [%s + %lld], %s\n",
      REGISTER_NAMES[lhs.as.sval][0],
      lhs.off,
      REGISTER_NAMES[rhs.as.sval][0]);
      
      } else if(lhs.type == VALUE_TYPE_REGISTER &&
      rhs.type == VALUE_TYPE_REGISTER_OFF) {
      string_builder_appendf(sb, "        mov %s, [%s + %lld]\n",
      REGISTER_NAMES[lhs.as.sval][0],
      REGISTER_NAMES[rhs.as.sval][0],
      rhs.off);
      } else if(lhs.type == VALUE_TYPE_REGISTER &&
      rhs.type == VALUE_TYPE_CONSTANT) {
      string_builder_appendf(sb, "        mov %s, constant%lld\n",
      REGISTER_NAMES[lhs.as.sval][0],
      rhs.as.sval);
      } else if(lhs.type == VALUE_TYPE_REGISTER_OFF &&
      rhs.type == VALUE_TYPE_CONSTANT) {
      string_builder_appendf(sb, "        mov qword [%s + %lld], constant%lld\n",
      REGISTER_NAMES[lhs.as.sval][0],
      lhs.off,
      rhs.as.sval);
      } else {
      panic("Unimplemented value->type's");
      }
    */
    
  } break;

  case INSTR_TYPE_SUB: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_LITERAL)  {
      string_builder_appendf(sb, "        sub %s, %lld\n",
			     REGISTER_NAMES[lhs.as.sval][0], rhs.as.sval);
    } else if(lhs.type == VALUE_TYPE_REGISTER &&
	      rhs.type == VALUE_TYPE_REGISTER)  {
      string_builder_appendf(sb, "        sub %s, %s\n",
			     REGISTER_NAMES[lhs.as.sval][0],
			     REGISTER_NAMES[rhs.as.sval][0]);

    } else {
      panic("Unimplemented value->type's");    	
    }
      
  } break;

  case INSTR_TYPE_ADD: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_LITERAL)  {
      string_builder_appendf(sb, "        add %s, %lld\n",
			     REGISTER_NAMES[lhs.as.sval][0], rhs.as.sval);
    } else if(lhs.type == VALUE_TYPE_REGISTER &&
	      rhs.type == VALUE_TYPE_REGISTER)  {
      string_builder_appendf(sb, "        add %s, %s\n",
			     REGISTER_NAMES[lhs.as.sval][0],
			     REGISTER_NAMES[rhs.as.sval][0]);
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

  case INSTR_TYPE_CMP: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_REGISTER) {
      string_builder_appendf(sb, "        cmp %s, %s\n",
			     REGISTER_NAMES[lhs.as.sval][0],
			     REGISTER_NAMES[rhs.as.sval][0]);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;

  case INSTR_TYPE_JNE: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        jne .label%lld\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;

  case INSTR_TYPE_JL: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        jl .label%lld\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;

  case INSTR_TYPE_JE: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        je .label%lld\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;


  case INSTR_TYPE_JGE: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        jge .label%lld\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;

  case INSTR_TYPE_JLE: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        jle .label%lld\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;


  case INSTR_TYPE_JMP: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, "        jmp .label%lld\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;


  case INSTR_TYPE_LABEL: {

    if(lhs.type == VALUE_TYPE_LITERAL) {
      string_builder_appendf(sb, ".label%lld:\n",
			     lhs.as.sval);
    } else {
      panic("Unimplemented value->type's");
    }
    
  } break;

  case INSTR_TYPE_LEA: {

    if(lhs.type == VALUE_TYPE_REGISTER &&
       rhs.type == VALUE_TYPE_REGISTER_OFF) {
      string_builder_appendf(sb, "        lea %s, [%s + %lld]\n",
			     REGISTER_NAMES[lhs.as.sval][0],
			     REGISTER_NAMES[rhs.as.sval][0],
			     rhs.off);
    } else {
      panic("Unimplemented value->type's");
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
  EXPR_TYPE_SUBTRACTION,
  EXPR_TYPE_STRUCT_FIELD,
  EXPR_TYPE_VARIABLE_DEREF,
  EXPR_TYPE_SUM,
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
  CONSTANT_TYPE_S64,
}Constant_Type;

struct Constant{

  Constant_Type type;
  union{
    const char *cstrval;
    s64 sval;
  }as;
  
};

typedef struct{
  Expr *lhs;
  Expr *rhs;
}Expr_Bin_Op;

typedef struct{
  string fst;
  string snd;
}Expr_Strings;

struct Expr{
  Expr_Type type;
  union{
    s64 sval;
    Expr_Funccall funccall;
    Expr_Bin_Op bin_op;
    Expr_Strings strings;
  }as;
};

bool expr_is_static(Expr expr) {
  switch(expr.type) {
  }

  panic("unimplemented expr_type");
}

typedef enum{
  TYPE_NONE = 0,
  TYPE_U64,
  TYPE_U64_PTR,
  TYPE_U8,
  TYPE_U8_PTR,
}Type;

Size type_size(Type type) {
  switch(type) {

  case TYPE_U64:
    return SIZE_QWORD;

  case TYPE_U64_PTR:
    return SIZE_QWORD;

  case TYPE_U8:
    return SIZE_BYTE;

  case TYPE_U8_PTR:
    return SIZE_QWORD;

  default: {
    panic("unimplemented type");
  } break;
  }
}

Type type_ptr(Type type) {
  switch(type) {
  case TYPE_U8:
    return TYPE_U8_PTR;

  case TYPE_U64:
    return TYPE_U64_PTR;
  }

  panic("unimplemented type: %d", type);
}

typedef enum{
  VAR_TYPE_NONE = 0,
  VAR_TYPE_PLAIN,
  VAR_TYPE_STRUCT,
}Var_Type;

typedef struct{
  string name;
  Var_Type type;
  u64 off;
  union{
    Type plain_type;
    u64 struct_index; // in 'Structures' type
  }as;  
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
  Constant *data;
  u64 len;
  u64 cap;
}Constants;

u64 constants_append_cstr(Constants *c, const char *cstr) {
  da_append(c, ((Constant) {
	.type = CONSTANT_TYPE_CSTR,
	.as.cstrval = cstr
      }));
  
  return c->len - 1;
}

u64 constants_append_s64(Constants *c, s64 value) {
  da_append(c, ((Constant) {
	.type = CONSTANT_TYPE_S64,
	.as.sval = value
      }));
  
  return c->len - 1;
}

void constants_append(Constants *constants, string_builder *sb) {
  for(u64 k=0;k<constants->len;k++) {

    Constant *c = &constants->data[k];
    
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
    
    
      string_builder_appendf(sb, "constant%llu: db ", k);
      for(u64 j=0;j<s.len;j++) {
	string_builder_appendf(sb, "%u", s.data[j]);
	if(j != s.len - 1) {
	  string_builder_appendc(sb, ",");
	}
      }
      string_builder_appendc(sb, ",0");    

      string_builder_appendf(sb, "\n");
    
    } break;

    case CONSTANT_TYPE_S64: {

      string_builder_appendf(sb, "        ;; CONSTANT_S64\n");
      string_builder_appendf(sb, "        ;;     value=%lld\n", c->as.sval);
    
    
      string_builder_appendf(sb, "%%define constant%llu %lld\n",
			     k, c->as.sval);
    
    } break;
    
    default: {
      panic("unimplemented constant_type");
    } break;
    }

  }
}

typedef struct{
  string *data;
  u64 len;
  u64 cap;
}strings;

void strings_append_if_not_contains(strings *ss, string s) {
  for(u64 i=0;i<ss->len;i++) {
    if(string_eq(ss->data[i], s)) {
      return;
    }
  }

  da_append(ss, s);
}

typedef struct{
  string name;
  Type type;
}Structure_Field;

typedef struct{
  Structure_Field *data;
  u64 len;
  u64 cap;
}Structure_Fields;

Structure_Field *structure_fields_find(Structure_Fields *fields, string name) {
  for(u64 i=0;i<fields->len;i++) {
    Structure_Field *f = &fields->data[i];
    if(string_eq(f->name, name)) {
      return f;
    }
  }

  return NULL;
}

typedef struct{
  string name;
  Structure_Fields fields;
}Structure;

typedef struct{
  Structure *data;
  u64 len;
  u64 cap;
}Structures;

Structure *structures_find(Structures *structs, string name) {
  for(u64 i=0;i<structs->len;i++) {
    Structure *s = &structs->data[i];
    if(string_eq(s->name, name)) {
      return s;
    }
  }

  return NULL;
}

typedef struct{
  Expr expr_pool[16];
  u64 expr_pool_count;

  Constants constants;
  Vars vars;
  Instrs instrs;
  strings foreign_functions;
  Structures structs;

  u64 label_count;
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

Expr *program_expr_append_subtraction(Program *p, Expr *lhs, Expr *rhs) {

  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_SUBTRACTION;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

Expr *program_expr_append_sum(Program *p, Expr *lhs, Expr *rhs) {

  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_SUM;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

Expr *program_expr_append_variable(Program *p, string name) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_VARIABLE;
  e->as.strings.fst = name;
  
  return e;
}

Expr *program_expr_append_deref(Program *p, string name) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_VARIABLE_DEREF;
  e->as.strings.fst = name;
  
  return e;
}

Expr *program_expr_append_struct_field(Program *p, string name, string field_name) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_STRUCT_FIELD;
  e->as.strings = (Expr_Strings) { name, field_name };
  
  return e;
}

Expr *program_expr_append_pointer(Program *p, string name) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_VARIABLE_PTR;
  e->as.strings.fst = name;
  
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

Expr *program_expr_append_constant(Program *p, u64 index) {
  
  Expr *e = program_expr_append(p);
  e->type = EXPR_TYPE_CONSTANT;
  e->as.sval = (s64) index;

  return e;
}

void program_append(Program *p, string_builder *sb) {

  string_builder_appendf(sb,
			 "        global main\n");

  strings_append_if_not_contains(&p->foreign_functions, string_from_cstr("ExitProcess"));
  for(u64 i=0;i<p->foreign_functions.len;i++) {
    string_builder_appendf(sb,
			   "        extern "str_fmt"\n", str_arg(p->foreign_functions.data[i]));
  }

  if(p->constants.len > 0) {
    string_builder_appendc(sb, "\n");
    string_builder_appendc(sb, "        section .data\n");    

    constants_append(&p->constants, sb);

    string_builder_appendc(sb, "\n");
  }
  
  string_builder_appendf(sb,
			 "        section .text\n"
			 "main:\n");

  if(p->stack_ptr != 0) {
    da_append(&p->instrs, ADD(RSP, LITERAL(p->stack_ptr)));
  }
  instrs_append(&p->instrs, sb);


  string_builder_appendf(sb,
			 "        mov rcx, 0\n"
			 "        sub rsp, 40\n"
			 "        call ExitProcess\n");

}

void program_compile_declaration(Program *p,
				 string name,
				 Type type) {
  if(vars_find(&p->vars, name, TYPE_NONE)) {
    panic("variable already declared");
  }

  Size size = type_size(type);
  u64 n = size_in_bytes(size);

  da_append(&p->instrs, SUB(RSP, LITERAL(n)));
  p->stack_ptr += n;

  Var var;
  var.name = name;
  var.type = VAR_TYPE_PLAIN;
  var.as.plain_type = type;
  var.off  = p->stack_ptr;
  
  da_append(&p->vars, var);

  
  p->expr_pool_count = 0;
}

void program_compile_declaration_array(Program *p,
				       string name,
				       Type type,
				       u64 n) {
  if(vars_find(&p->vars, name, TYPE_NONE)) {
    panic("variable already declared");
  }

  Size size = type_size(type);
  Type ptr_type = type_ptr(type);
    
  Size size_ptr = type_size(ptr_type);
  u64 n_ptr = size_in_bytes(size_ptr);
  
  u64 ns = n * size_in_bytes(size) + n_ptr;
  
  da_append(&p->instrs, SUB(RSP, LITERAL(ns)));
  da_append(&p->instrs, LEA(RAX, RSP_OFF(n_ptr)));
  da_append(&p->instrs, MOV(RSP_OFF(0), RAX, size_ptr));
  p->stack_ptr += ns;

  Var var;
  var.name = name;
  var.type = VAR_TYPE_PLAIN;
  var.as.plain_type = ptr_type;
  var.off  = p->stack_ptr;
  da_append(&p->vars, var);
  
  p->expr_pool_count = 0;
}

void program_compile_declaration_struct(Program *p,
					string name,
					string struct_name) {

  Structure *structure = structures_find(&p->structs, struct_name);
  if(!structure) {
    panic("variable already declared");
  }
  
  if(vars_find(&p->vars, name, TYPE_NONE)) {
    panic("variable already declared");
  }

  u64 size = 0;
  for(u64 i=0;i<structure->fields.len;i++) {
    Structure_Field *field = &structure->fields.data[i];
    size += size_in_bytes(type_size(field->type));
  }

  da_append(&p->instrs, SUB(RSP, LITERAL(size)));
  p->stack_ptr += size;

  Var var;
  var.name = name;
  var.type = VAR_TYPE_STRUCT;
  var.as.struct_index = (u64)
    (((unsigned char *) structure - (unsigned char *) p->structs.data) / sizeof(Structure));
  var.off  = p->stack_ptr;
  da_append(&p->vars, var);
  
  p->expr_pool_count = 0;
}

u64 program_structure_off(Program *p, Var *var, string field_name, Type *out_type) {
   
  Structure *structure = &p->structs.data[var->as.struct_index];

  Structure_Field *field = structure_fields_find(&structure->fields, field_name);
  if(!field) {
    panic("structure: \""str_fmt"\" has no field named: \""str_fmt"\"\n",
	  str_arg(structure->name), str_arg(field_name));
  }
  *out_type = field->type;
  u64 field_index = (u64)
    (((unsigned char *) field - (unsigned char *) structure->fields.data) / sizeof(Structure_Field));

  u64 off = 0;
  for(u64 i=0;i<field_index;i++) {
    Structure_Field *sub_field = &structure->fields.data[i];
    off += size_in_bytes(type_size(sub_field->type));
  }

  return off;
}

Value program_expr_location(Program *p, Expr *e, Type *out_type) {

  string name = e->as.strings.fst;
  string field_name = e->as.strings.snd;
  
  Var *var = vars_find(&p->vars, name, TYPE_NONE);
  if(!var) {
    panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
  }

  Type type = var->as.plain_type;
  Value result;
  switch(e->type) {

  case EXPR_TYPE_VARIABLE: {
    result = RSP_OFF(p->stack_ptr - var->off);    
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {
    result = RSP_OFF(p->stack_ptr - var->off);
  } break;

  case EXPR_TYPE_STRUCT_FIELD: {
    if(var->type != VAR_TYPE_STRUCT) {
      panic("variable with the name: \""str_fmt"\" is not a structure\n", str_arg(name));
    }
    
    result = RSP_OFF(p->stack_ptr - var->off + program_structure_off(p, var, field_name, &type));
  } break;

  case EXPR_TYPE_VARIABLE_DEREF: {
    // TODO: deref var->type and update 'type'
    
    // TODO: Maybe check that the pointer is actually 8 bytes long
    Size size = SIZE_QWORD;
    
    da_append(&p->instrs, MOV(RAX, RSP_OFF(p->stack_ptr - var->off), size));
    result = RAX_OFF(0);    
  } break;
    
  default:
    panic("unimplemented expr_type: %d", e->type);
  }

  if(out_type) {
    *out_type = type;
  }

  return result;
}

void program_funccall_compile(Program *p, Expr_Funccall *funccall);

void program_expr_compile(Program *p,
			  Expr *e,
			  Value location,
			  Size size) {

  switch(e->type) {

  case EXPR_TYPE_VALUE: {

    s64 value = e->as.sval;
    
    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(&p->instrs, MOV(location, LITERAL(value), size));
    } else {
      da_append(&p->instrs, MOV(RAX, LITERAL(value), size));
      da_append(&p->instrs, MOV(location, RAX, size));
    }
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    program_funccall_compile(p, &e->as.funccall);
    da_append(&p->instrs, MOV(location, RAX, size));
    
  } break;

  case EXPR_TYPE_CONSTANT: {

    // TODO: Typecheck ???

    Constant *c = &p->constants.data[e->as.sval];

    Size value_size;
    if(c->type == CONSTANT_TYPE_CSTR) {
      value_size = SIZE_QWORD; // pointer-size
    } else { // CONSTANT_TYPE_S64
      value_size = size_of_s64(c->as.sval);
    }
    
    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(&p->instrs, MOV(location, CONSTANT(e->as.sval), value_size));
    } else {
      da_append(&p->instrs, MOV(RAX, CONSTANT(e->as.sval), value_size));
      da_append(&p->instrs, MOV(location, RAX, value_size));
    }
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD:
  case EXPR_TYPE_VARIABLE: {

    Type variable_type;
    Value variable_location = program_expr_location(p, e, &variable_type);

    if(location.type == VALUE_TYPE_REGISTER_OFF) {

      Value temp;
      if(location.as.sval == 0) { // RAX := 0
	temp = RCX;
      } else {
	temp = RAX;
      }

      da_append(&p->instrs, MOV(temp, variable_location, size));
      da_append(&p->instrs, MOV(location, temp, size));
      
    } else {
      da_append(&p->instrs, MOV(location, variable_location, size));
    }    
    
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {

    Type variable_type;
    Value variable_location = program_expr_location(p, e, &variable_type);
    Type variable_ptr_type = type_ptr(variable_type);
    Size variable_ptr_size = type_size(variable_ptr_type);

    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(&p->instrs, LEA(location, variable_location));
    } else {
      da_append(&p->instrs, LEA(RAX, variable_location));
      da_append(&p->instrs, MOV(location, RAX, variable_ptr_size));
    }    
    
  } break;

  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    // Do not push, if you dont need to
    switch(rhs->type) {

    case EXPR_TYPE_VALUE: 
    case EXPR_TYPE_VARIABLE: {
      program_expr_compile(p, lhs, RAX, size);
      program_expr_compile(p, rhs, RCX, size);
    } break;

    case EXPR_TYPE_FUNCCALL: {
      da_append(&p->instrs, SUB(RSP, LITERAL(8)));
      p->stack_ptr += 8;
      program_expr_compile(p, lhs, RSP_OFF(0), size);
    
      program_expr_compile(p, rhs, RCX, size);
    
      da_append(&p->instrs, MOV(RAX, RSP_OFF(0), size));
      da_append(&p->instrs, ADD(RSP, LITERAL(8)));
      p->stack_ptr -= 8;
    } break;
      
    default: {
      panic("unimplemented expr_type");
    } break;
    }
    
    da_append(&p->instrs, SUB(RAX, RCX));
    da_append(&p->instrs, MOV(location, RAX, size));
    
  } break;

  case EXPR_TYPE_SUM: {

    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    // Do not push, if you dont need to
    switch(rhs->type) {

    case EXPR_TYPE_VALUE: 
    case EXPR_TYPE_VARIABLE: {
      program_expr_compile(p, lhs, RAX, size);
      program_expr_compile(p, rhs, RCX, size);
    } break;
      
    case EXPR_TYPE_SUBTRACTION:
    case EXPR_TYPE_FUNCCALL: {

      // TODO: maybe only allocate as much space as you need
      // 8 corresponds to SIZE_QWORD, this may be less
      da_append(&p->instrs, SUB(RSP, LITERAL(8)));
      p->stack_ptr += 8;
      program_expr_compile(p, lhs, RSP_OFF(0), size);
    
      program_expr_compile(p, rhs, RCX, size);
    
      da_append(&p->instrs, MOV(RAX, RSP_OFF(0), size));
      da_append(&p->instrs, ADD(RSP, LITERAL(8)));
      p->stack_ptr -= 8;
    } break;
      
    default: {
      panic("unimplemented expr_type");
    } break;
    }
    
    da_append(&p->instrs, ADD(RAX, RCX));
    da_append(&p->instrs, MOV(location, RAX, size));
    
  } break;
    
  default:
    panic("Unimplemented expr->type");
    
  }
  
}

void program_funccall_compile(Program *p, Expr_Funccall *funccall) {

  u64 stack_ptr_before = p->stack_ptr;

  s64 shadow_space = funccall->args_len * 8;
  if(shadow_space < 32) {
    shadow_space = 32;
  }
  s64 alignment = (p->stack_ptr + shadow_space) % 16;
  if(alignment != 0) {    
    shadow_space += 16 - alignment;
  }
  p->stack_ptr += shadow_space;
  da_append(&p->instrs, SUB(RSP, LITERAL(shadow_space)));

  // TODO: define function-defintions and determine size
  // for now everything is SIZE_QWORD
  Size size = SIZE_QWORD;

  // calculate argument-expressions and save them in the shadowspace
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];
    if(e->type == EXPR_TYPE_VARIABLE) {
      //pass
    } else {
            
      program_expr_compile(p, e, RSP_OFF(p->stack_ptr - stack_ptr_before - shadow_space + i  * 8
					 //i  * 8
					 ), size);
    }
  }

  // move the first four arguments out of the shadowspace
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];
    s64 target = (s64) p->stack_ptr - (s64) stack_ptr_before - (s64) shadow_space + i  * 8;
    //s64 target = i  * 8;

    if(i < FASTCALL_REGISTER_COUNT) { // rcx, rdx, r8, r9

      Value source;
      if(e->type == EXPR_TYPE_VARIABLE) {
	source = program_expr_location(p, e, NULL);
      } else {
	source = RSP_OFF(target);
      }

      da_append(&p->instrs, MOV(REGISTER(FASTCALL_REGISTERS[i]), source, size));
    } else { // push ...

      if(e->type == EXPR_TYPE_VARIABLE) {

	da_append(&p->instrs, MOV(RAX, program_expr_location(p, e, NULL), size));
	da_append(&p->instrs, MOV(RSP_OFF(target), RAX, size));
      } else {

	// variable already in the right place
      }
      
    }
    

    
  }

  strings_append_if_not_contains(&p->foreign_functions, funccall->name);
  da_append(&p->instrs, CALL(WORD(funccall->name)));

  da_append(&p->instrs, ADD(RSP, LITERAL(p->stack_ptr - stack_ptr_before)));
  p->stack_ptr = stack_ptr_before;

  
}

void program_compile_assignment(Program *p,
				Expr *lhs,
				Expr *rhs,
				Size size) {

  Value location = program_expr_location(p, lhs, NULL);
  program_expr_compile(p, rhs, location, size);  
  p->expr_pool_count = 0;
}

#define program_compile_funccall(p, name, ...)			\
  program_compile_funccall_impl((p), (name), __VA_ARGS__, NULL)

void program_compile_funccall_impl(Program *p,
				   string name,
				   ...) {
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

  program_funccall_compile(p, &funccall);
  
  p->expr_pool_count = 0;
}

typedef enum{
  STMT_IF_TYPE_NONE,
  STMT_IF_TYPE_EQUALS,
  STMT_IF_TYPE_NOT_EQUALS,
  STMT_IF_TYPE_LESS,
  STMT_IF_TYPE_GREATER_OR_EQUAL,
  STMT_IF_TYPE_GREATER,
}Stmt_If_Type;

typedef struct{
  u64 var_count;
  u64 stack_ptr;
  u64 label_index;
}Foo;

Foo program_compile_if_begin(Program *p,
			     Expr *lhs,
			     Stmt_If_Type operand,
			     Expr *rhs) {

  
  // TODO: evaluate size of expressions  
    Size size = SIZE_QWORD;  
  
  // Do not push, if you dont need to
  switch(rhs->type) {

  case EXPR_TYPE_VALUE: 
  case EXPR_TYPE_VARIABLE: {
    program_expr_compile(p, lhs, RAX, size);
    program_expr_compile(p, rhs, RCX, size);
  } break;

    // TODO: take into account size
  case EXPR_TYPE_FUNCCALL: {
    da_append(&p->instrs, SUB(RSP, LITERAL(8)));
    p->stack_ptr += 8;
    program_expr_compile(p, lhs, RSP_OFF(0), size);
    
    program_expr_compile(p, rhs, RCX, size);
    
    da_append(&p->instrs, MOV(RAX, RSP_OFF(0), size));
    da_append(&p->instrs, ADD(RSP, LITERAL(8)));
    p->stack_ptr -= 8;
  } break;
      
  default: {
    panic("unimplemented expr_type");
  } break;
  }

  da_append(&p->instrs, CMP(RAX, RCX));

  switch(operand) {
  case STMT_IF_TYPE_EQUALS: {
    da_append(&p->instrs, JNE(LITERAL(p->label_count)));
  } break;

  case STMT_IF_TYPE_LESS: {
    da_append(&p->instrs, JGE(LITERAL(p->label_count)));
  } break;

  case STMT_IF_TYPE_GREATER_OR_EQUAL: {
    da_append(&p->instrs, JL(LITERAL(p->label_count)));
  } break;

  case STMT_IF_TYPE_NOT_EQUALS: {
    da_append(&p->instrs, JE(LITERAL(p->label_count)));
  } break;

  case STMT_IF_TYPE_GREATER: {
    da_append(&p->instrs, JLE(LITERAL(p->label_count)));
  } break;
      
  default: {
    panic("unimplemented if_type");
  } break;
  }
  
  p->expr_pool_count = 0;

  return (Foo) { p->vars.len, p->stack_ptr, p->label_count++ };
}

void program_compile_if_end(Program *p,
			    Foo state) {

  p->vars.len = state.var_count;      
  p->expr_pool_count = 0;
  
  if(p->stack_ptr != state.stack_ptr) {
    da_append(&p->instrs, ADD(RSP, LITERAL(p->stack_ptr - state.stack_ptr)));
    p->stack_ptr = state.stack_ptr;
  }
  da_append(&p->instrs, LABEL(LITERAL(state.label_index)));

}

Foo program_compile_while_begin(Program *p,
				Expr *lhs,
				Stmt_If_Type operand,
				Expr *rhs) {
  da_append(&p->instrs, LABEL(LITERAL(p->label_count++)));

  return program_compile_if_begin(p, lhs, operand, rhs);
}

void program_compile_while_end(Program *p,
			       Foo state) {
  da_append(&p->instrs, JMP(LITERAL(state.label_index - 1)));
  program_compile_if_end(p, state);
}

Program slurp_file_program() {
  Program program = {0};

  // handle : u64*;
  program_compile_declaration(&program,
			      string_from_cstr("handle"),
			      TYPE_U64_PTR);
  
  // handle = CreateFileA("main.asm", GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("handle")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("CreateFileA"),
							  program_expr_append_constant(&program,
										       constants_append_cstr(&program.constants,
													     "main.fe")),
							  program_expr_append_value(&program, GENERIC_READ),
							  program_expr_append_value(&program, FILE_SHARE_READ),
							  program_expr_append_value(&program, 0),
							  program_expr_append_value(&program, OPEN_EXISTING),
							  program_expr_append_value(&program, FILE_ATTRIBUTE_NORMAL),
							  program_expr_append_value(&program, 0)),
			     SIZE_QWORD);

  // if(handle == INVALID_HANDLE_VALUE) {
  Foo state = program_compile_if_begin(&program,
				       program_expr_append_variable(&program,
								    string_from_cstr("handle")),
				       STMT_IF_TYPE_EQUALS,
				       program_expr_append_value(&program,
								 (s64) INVALID_HANDLE_VALUE));
  //     error : u64 = GetLastError()
  program_compile_declaration(&program,
			      string_from_cstr("error"),
			      TYPE_U64);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("error")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetLastError")),
			     SIZE_QWORD);

  //     ExitProcess(error);
  program_compile_funccall(&program,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_variable(&program, string_from_cstr("error")));
  // }
  program_compile_if_end(&program, state);

  // process_heap : u64*;
  program_compile_declaration(&program,
			      string_from_cstr("process_heap"),
			      TYPE_U64_PTR);

  // process_heap = GetProcessHeap();
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("process_heap")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetProcessHeap")),
			     SIZE_QWORD);

  // size : u64;
  program_compile_declaration(&program,
			      string_from_cstr("size"),
			      TYPE_U64);

  // size = GetFileSize(handle);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("size")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetFileSize"),
							  program_expr_append_variable(&program,
										       string_from_cstr("handle"))),
			     SIZE_QWORD);
  // space : u64;
  program_compile_declaration(&program,
			      string_from_cstr("space"),
			      TYPE_U64);

  // space = GetProcessHeap(process_heap, 0, size);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("space")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("HeapAlloc"),
							  program_expr_append_variable(&program,
										       string_from_cstr("process_heap")),
							  program_expr_append_value(&program,
										    0),
							  program_expr_append_variable(&program,
										       string_from_cstr("size"))),
			     SIZE_QWORD);

  // written : u64;
  program_compile_declaration(&program,
			      string_from_cstr("written"),
			      TYPE_U64);
    
  // ReadFile(handle, space, size, &written, NULL);
  program_compile_funccall(&program,
			   string_from_cstr("ReadFile"),
			   program_expr_append_variable(&program,
							string_from_cstr("handle")),
			   program_expr_append_variable(&program,
							string_from_cstr("space")),
			   program_expr_append_variable(&program,
							string_from_cstr("size")),
			   program_expr_append_pointer(&program,
						       string_from_cstr("written")),
			   program_expr_append_value(&program,
						     0));

  // WriteFile(GetStdHandle(-11), space, size, &written, NULL);
  program_compile_funccall(&program,
			   string_from_cstr("WriteFile"),
			   program_expr_append_funccall(&program,
							string_from_cstr("GetStdHandle"),
							program_expr_append_value(&program,
										  -11)),
			   program_expr_append_variable(&program,
							string_from_cstr("space")),
			   program_expr_append_variable(&program,
							string_from_cstr("size")),
			   program_expr_append_pointer(&program,
						       string_from_cstr("written")),
			   program_expr_append_value(&program,
						     0)
			   );
  
  // CloseHandle(handle);
  program_compile_funccall(&program,
			   string_from_cstr("CloseHandle"),
			   program_expr_append_variable(&program,
							string_from_cstr("handle")));


  return program;
}

Program buffered_slurp_file_program() {

  Program program = {0};

  // handle : u64*;
  program_compile_declaration(&program,
			      string_from_cstr("handle"),
			      TYPE_U64_PTR);
  
  // handle = CreateFileA("main.asm", GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("handle")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("CreateFileA"),
							  program_expr_append_constant(&program,
										       constants_append_cstr(&program.constants,
													     "main.fe")),
							  program_expr_append_value(&program, GENERIC_READ),
							  program_expr_append_value(&program, FILE_SHARE_READ),
							  program_expr_append_value(&program, 0),
							  program_expr_append_value(&program, OPEN_EXISTING),
							  program_expr_append_value(&program, FILE_ATTRIBUTE_NORMAL),
							  program_expr_append_value(&program, 0)),
			     SIZE_QWORD);

  // if(handle == INVALID_HANDLE_VALUE) {
  Foo state = program_compile_if_begin(&program,
				       program_expr_append_variable(&program,
								    string_from_cstr("handle")),
				       STMT_IF_TYPE_EQUALS,
				       program_expr_append_value(&program,
								 (s64) INVALID_HANDLE_VALUE));
  //     ExitProcess(GetLastError());
  program_compile_funccall(&program,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_funccall(&program,
							string_from_cstr("GetLastError")));
  // }
  program_compile_if_end(&program, state);

  // size : u64;
  program_compile_declaration(&program,
			      string_from_cstr("size"),
			      TYPE_U64);

  // size = GetFileSize(handle);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("size")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetFileSize"),
							  program_expr_append_variable(&program,
										       string_from_cstr("handle"))),
			     SIZE_QWORD);

  // written : u64;
  program_compile_declaration(&program,
			      string_from_cstr("written"),
			      TYPE_U64);

  u64 buf_size = 8;
  // buf : u8[8]
  program_compile_declaration_array(&program,
				    string_from_cstr("buf"),
				    TYPE_U8,
				    buf_size);

  // len : u64;
  program_compile_declaration(&program,
			      string_from_cstr("len"),
			      TYPE_U64);

  // stdOutputHandle : u64* = GetStdHandle(-11);
  program_compile_declaration(&program,
			      string_from_cstr("stdOutputHandle"),
			      TYPE_U64_PTR);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("stdOutputHandle")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetStdHandle"),
							  program_expr_append_value(&program,
										    -11)),
			     SIZE_QWORD);

  // while(size != 0) {
  Foo loop_state = program_compile_while_begin(&program,
					       program_expr_append_variable(&program,
									    string_from_cstr("size")),
					       STMT_IF_TYPE_NOT_EQUALS,
					       program_expr_append_value(&program,
									 0));

  //     len = 8;
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("len")),
			     program_expr_append_value(&program, buf_size),
			     SIZE_QWORD);

  //     if(size < len) {
  state = program_compile_if_begin(&program,
				   program_expr_append_variable(&program,
								string_from_cstr("size")),
				   STMT_IF_TYPE_LESS,
				   program_expr_append_variable(&program,
								string_from_cstr("len")));


  //         len = size
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("len")),
			     program_expr_append_variable(&program,
							  string_from_cstr("size")),
			     SIZE_QWORD);

  //     }
  program_compile_if_end(&program, state);

  //     ReadFile(handle, buf, len, &written, NULL);
  program_compile_funccall(&program,
			   string_from_cstr("ReadFile"),
			   program_expr_append_variable(&program,
							string_from_cstr("handle")),
			   program_expr_append_variable(&program,
						       string_from_cstr("buf")),
			   program_expr_append_variable(&program,
							string_from_cstr("len")),
			   program_expr_append_pointer(&program,
						       string_from_cstr("written")),
			   program_expr_append_value(&program,
						     0));

  //     size = size - written
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("size")),
			     program_expr_append_subtraction(&program,
							     program_expr_append_variable(&program,
											  string_from_cstr("size")),
							     program_expr_append_variable(&program,
											  string_from_cstr("written"))),
			     SIZE_QWORD);

  //     WriteFile(stdOutputHandle, buf, written, &written, NULL);
  program_compile_funccall(&program,
			   string_from_cstr("WriteFile"),
			   program_expr_append_variable(&program,
							string_from_cstr("stdOutputHandle")),
			   program_expr_append_variable(&program,
						       string_from_cstr("buf")),
			   program_expr_append_variable(&program,
							string_from_cstr("written")),
			   program_expr_append_pointer(&program,
						       string_from_cstr("written")),
			   program_expr_append_value(&program,
						     0)
			   );

  // }
  program_compile_while_end(&program, loop_state);

  // CloseHandle(handle);
  program_compile_funccall(&program,
			   string_from_cstr("CloseHandle"),
			   program_expr_append_variable(&program,
							string_from_cstr("handle")));

  // ExitProcess(size);  
  program_compile_funccall(&program,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_variable(&program,
							string_from_cstr("size")));  
  return program;
}

Program struct_example_program() {
  Program program = {0};

  /*
    struct :: string {
    u8* data;
    u64 len;
    }
  */
  Structure_Fields string_fields = {0};
  da_append(&string_fields, ((Structure_Field) {
	.name = string_from_cstr("data"),
	.type = TYPE_U8_PTR,
      }));
  da_append(&string_fields, ((Structure_Field) {
	.name = string_from_cstr("len"),
	.type = TYPE_U64,
      }));
  da_append(&program.structs, ((Structure) {
	.name = string_from_cstr("string"),
	.fields = string_fields,
      }));

  // s : string;
  program_compile_declaration_struct(&program,
				     string_from_cstr("s"),
				     string_from_cstr("string"));

  // s = "Hello, World!";
  string s_content = string_from_cstr("Hello, World!\n");
  program_compile_assignment(&program,
			     program_expr_append_struct_field(&program,
							      string_from_cstr("s"),
							      string_from_cstr("data")),
			     program_expr_append_constant(&program,
							  constants_append_cstr(&program.constants,
										s_content.data)),
			     SIZE_QWORD);
  program_compile_assignment(&program,
			     program_expr_append_struct_field(&program,
							      string_from_cstr("s"),
							      string_from_cstr("len")),
			     program_expr_append_constant(&program,
							  constants_append_s64(&program.constants,
									       s_content.len)),
			     SIZE_QWORD);

  // written : u64;
  program_compile_declaration(&program,
			      string_from_cstr("written"),
			      TYPE_U64);

  // WriteFile(GetStdHandle(-11), s.data, s.len, &written, NULL);
  program_compile_funccall(&program,
			   string_from_cstr("WriteFile"),
			   program_expr_append_funccall(&program,
							string_from_cstr("GetStdHandle"),
							program_expr_append_value(&program,
										  -11)),
			   program_expr_append_struct_field(&program,
							    string_from_cstr("s"),
							    string_from_cstr("data")),
			   program_expr_append_struct_field(&program,
							    string_from_cstr("s"),
							    string_from_cstr("len")),
			   program_expr_append_pointer(&program,
						       string_from_cstr("written")),
			   program_expr_append_value(&program,
						     0));

  return program;
}

Program dbca_program() {
  Program program = {0};

  // buf : u8[8];
  program_compile_declaration_array(&program,
				    string_from_cstr("buf"),
				    TYPE_U8,
				    8);

  // buf_len: u64 = 0;
  program_compile_declaration(&program,
			      string_from_cstr("buf_len"),
			      TYPE_U64);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("buf_len")),
			     program_expr_append_value(&program,
						       0),
			     SIZE_QWORD);

  // i : u64 = 4
  program_compile_declaration(&program,
			      string_from_cstr("i"),
			      TYPE_U64);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("i")),
			     program_expr_append_value(&program,
						       4),
			     SIZE_QWORD);

  // ptr : u64* = buf + i - 1;
  program_compile_declaration(&program,
			      string_from_cstr("ptr"),
			      TYPE_U64_PTR);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("ptr")),
			     program_expr_append_sum(&program,
						     program_expr_append_variable(&program,
										  string_from_cstr("buf")),
						     program_expr_append_subtraction(&program,
										     program_expr_append_variable(&program,
														  string_from_cstr("i")),
										     program_expr_append_value(&program,
													       1))),
			     SIZE_QWORD);

  // value : u8 = 65
  program_compile_declaration(&program,
			      string_from_cstr("value"),
			      TYPE_U64);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("value")),
			     program_expr_append_value(&program,
						       65),
			     SIZE_BYTE);
  
  // while(i > 0) {
  Foo for_loop = program_compile_while_begin(&program,
					     program_expr_append_variable(&program,
									  string_from_cstr("i")),
					     STMT_IF_TYPE_GREATER,
					     program_expr_append_value(&program,
								       0));

  //     *ptr = value;
  program_compile_assignment(&program,
			     program_expr_append_deref(&program,
						       string_from_cstr("ptr")),
			     program_expr_append_variable(&program,
							  string_from_cstr("value")),
			     SIZE_BYTE);
  
  //     ptr = ptr - 1
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("ptr")),
			     program_expr_append_subtraction(&program,
							     program_expr_append_variable(&program,
											  string_from_cstr("ptr")),
							     program_expr_append_value(&program,
										       1)),
			     SIZE_QWORD);

  //     value = value + 1
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("value")),
			     program_expr_append_sum(&program,
						     program_expr_append_variable(&program,
										  string_from_cstr("value")),
						     program_expr_append_value(&program,
									       1)),
			     SIZE_QWORD);

  //     buf_len = buf_len + 1
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("buf_len")),
			     program_expr_append_sum(&program,
						     program_expr_append_variable(&program,
										  string_from_cstr("buf_len")),
						     program_expr_append_value(&program,
									       1)),
			     SIZE_QWORD);

  //     i = i - 1
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("i")),
			     program_expr_append_subtraction(&program,
							     program_expr_append_variable(&program,
											  string_from_cstr("i")),
							     program_expr_append_value(&program,
										       1)),
			     SIZE_QWORD);


  // }
  program_compile_while_end(&program, for_loop);
  

  // if(buf_len > 0) {
  Foo buf_len_positive = program_compile_if_begin(&program,
						  program_expr_append_variable(&program,
									       string_from_cstr("buf_len")),
						  STMT_IF_TYPE_GREATER,
						  program_expr_append_value(&program,
									    0));

  //     handle : u64* = GetStdHandle(-11);
  program_compile_declaration(&program,
			      string_from_cstr("handle"),
			      TYPE_U64);
  program_compile_assignment(&program,
			     program_expr_append_variable(&program,
							  string_from_cstr("handle")),
			     program_expr_append_funccall(&program,
							  string_from_cstr("GetStdHandle"),
							  program_expr_append_value(&program,
										    -11)),
			     SIZE_QWORD);

  //     if(handle == INVALID_HAMDLE_VALUE) {
  Foo GetStdHandleFailed = program_compile_if_begin(&program,
						    program_expr_append_variable(&program,
										 string_from_cstr("handle")),
						    STMT_IF_TYPE_EQUALS,
						    program_expr_append_value(&program,
									      (s64) INVALID_HANDLE_VALUE));
  //         ExitProcess(GetLastError());
  program_compile_funccall(&program,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_funccall(&program,
							string_from_cstr("GetLastError")));


  //     }
  program_compile_if_end(&program, GetStdHandleFailed);
  
  //     written : u64*;
  program_compile_declaration(&program,
			      string_from_cstr("written"),
			      TYPE_U64);

  //     if(WriteFile(handle, buf, buf_len, &written, NULL) == 0) {
  Foo WriteFileFailed = program_compile_if_begin(&program,
						 program_expr_append_funccall(&program,
									      string_from_cstr("WriteFile"),
									      program_expr_append_variable(&program,
													   string_from_cstr("handle")),
									      program_expr_append_variable(&program,
													   string_from_cstr("buf")),
									      program_expr_append_variable(&program,
													   string_from_cstr("buf_len")),
									      program_expr_append_pointer(&program,
													  string_from_cstr("written")),
									      program_expr_append_value(&program,
													0)),
						 STMT_IF_TYPE_EQUALS,
						 program_expr_append_value(&program,
									   0));

  //         ExitProcess(GetLastError());
  program_compile_funccall(&program,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_funccall(&program,
							string_from_cstr("GetLastError")));
  
  //     }
  program_compile_if_end(&program, WriteFileFailed);

  // }
  program_compile_if_end(&program, buf_len_positive);


  return program;
}

// TODO:
//     functions
//     type checking

//     remove unnecassary instructions

int main() {

  //Program program = {0};
  //Program program = struct_example_program();
  Program program = dbca_program();
  //Program program = slurp_file_program();
  //Program program = buffered_slurp_file_program();
  
  string_builder sb = {0};
  program_append(&program, &sb);
  printf("%.*s", (int) sb.len, sb.data);

  if(!io_write_file("main.asm", (u8 *) sb.data, sb.len)) {
    return 1;
  }


  return 0;
}
