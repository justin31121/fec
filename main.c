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
typedef unsigned int u32;
typedef int s32;
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
  SIZE_NONE,
}Size;

const char *SIZE_NAMES[] = {
  "byte", "word", "dword", "qword"
};

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
  INSTR_TYPE_AND,
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
#define LEA(l, r) ((Instr) { .type = INSTR_TYPE_LEA, .lhs = (l), .rhs = (r), .size = (0) })
#define AND(l, r, s) ((Instr) { .type = INSTR_TYPE_AND, .lhs = (l), .rhs = (r), .size = (s)})
#define SUB(l, r, s) ((Instr) { .type = INSTR_TYPE_SUB, .lhs = (l), .rhs = (r), .size = (s)})
#define ADD(l, r, s) ((Instr) { .type = INSTR_TYPE_ADD, .lhs = (l), .rhs = (r), .size = (s)})
#define CMP(l, r, s) ((Instr) { .type = INSTR_TYPE_CMP, .lhs = (l), .rhs = (r), .size = (s)})
#define CALL(l) ((Instr) { .type = INSTR_TYPE_CALL, .lhs = (l), .size = (0) })
#define JNE(l) ((Instr) { .type = INSTR_TYPE_JNE, .lhs = (l), .size = (0) })
#define JGE(l) ((Instr) { .type = INSTR_TYPE_JGE, .lhs = (l), .size = (0)  })
#define JLE(l) ((Instr) { .type = INSTR_TYPE_JLE, .lhs = (l), .size = (0)  })
#define JL(l) ((Instr) { .type = INSTR_TYPE_JL, .lhs = (l), .size = (0)  })
#define JE(l) ((Instr) { .type = INSTR_TYPE_JE, .lhs = (l), .size = (0)  })
#define JMP(l) ((Instr) { .type = INSTR_TYPE_JMP, .lhs = (l), .size = (0)  })
#define LABEL(l) ((Instr) { .type = INSTR_TYPE_LABEL, .lhs = (l), .size = (0)  })

void instr_append(Instr* instr, string_builder *sb) {
  Value lhs = instr->lhs;
  Value rhs = instr->rhs;

  Size s = instr->size;
  if(s < SIZE_BYTE || COUNT_SIZES <= s) {
    panic("Invalid size: %d", s);
  }
  // TODO: yooooooooooooooooooooooo
  s = 3 - s;

  switch(instr->type) {

  case INSTR_TYPE_AND: {
    
    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {

	string_builder_appendf(sb, "        and %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       rhs.as.sval);
	
      } break;

      case VALUE_TYPE_REGISTER: {

	string_builder_appendf(sb, "        and %s, %s\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       REGISTER_NAMES[rhs.as.sval][s]);	  
	
      } break;


      default:
        panic("unimplemented rhs->type");
	
      }     
      
    } break;

    default:
      panic("unimplenmented lhs->type");      
    }
    
  } break;

  case INSTR_TYPE_MOV: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        mov %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       rhs.as.sval);
      } break;

      case VALUE_TYPE_REGISTER: {
	if(lhs.as.sval != rhs.as.sval) {
	  string_builder_appendf(sb, "        mov %s, %s\n",
				 REGISTER_NAMES[lhs.as.sval][s],
				 REGISTER_NAMES[rhs.as.sval][s]);
	}
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
	string_builder_appendf(sb, "        mov %s [%s + %lld], %lld\n",
			       SIZE_NAMES[s],
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

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        sub %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][0], rhs.as.sval);	
      } break;

      case VALUE_TYPE_REGISTER: {
	string_builder_appendf(sb, "        sub %s, %s\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       REGISTER_NAMES[rhs.as.sval][s]);
      } break;

      default:
	panic("unimplemented rhs.type");
	
      } 
      
    } break;

    default:
      panic("unimplemented lhs.type");
      
    }
      
  } break;

  case INSTR_TYPE_ADD: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        add %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][0], rhs.as.sval);	
      } break;

      case VALUE_TYPE_REGISTER: {
	string_builder_appendf(sb, "        add %s, %s\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       REGISTER_NAMES[rhs.as.sval][s]);
      } break;

      default:
	panic("unimplemented rhs.type");
	
      } 
      
    } break;

    default:
      panic("unimplemented lhs.type");
      
    }

    /* if(lhs.type == VALUE_TYPE_REGISTER && */
    /*    rhs.type == VALUE_TYPE_LITERAL)  { */
    /*   string_builder_appendf(sb, "        add %s, %lld\n", */
    /* 			     REGISTER_NAMES[lhs.as.sval][0], rhs.as.sval); */
    /* } else if(lhs.type == VALUE_TYPE_REGISTER && */
    /* 	      rhs.type == VALUE_TYPE_REGISTER)  { */
    /*   string_builder_appendf(sb, "        add %s, %s\n", */
    /* 			     REGISTER_NAMES[lhs.as.sval][0], */
    /* 			     REGISTER_NAMES[rhs.as.sval][0]); */
    /* } else { */
    /*   panic("Unimplemented value->type's");    	 */
    /* } */
      
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

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_REGISTER: {
	string_builder_appendf(sb, "        cmp %s, %s\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       REGISTER_NAMES[rhs.as.sval][s]);
      } break;

      default:
        panic("unimplemented value_type (rhs)");
	
      }
      
    } break;
      
    default:
      panic("unimplemented value_type (lhs)");
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

typedef enum{
  TYPE_NONE = 0,
  TYPE_STRUCT,
  TYPE_VOID,
  TYPE_U64,
  TYPE_U32,
  TYPE_U8,
  TYPE_S64,
}Type_Type;

#define NONE (Type) { TYPE_NONE, 0, 0 }
#define _VOID (Type) { TYPE_VOID, 0, 0 }
#define U8 (Type) { TYPE_U8, 0, 0 }
#define U32 (Type) { TYPE_U32, 0, 0 }
#define U64 (Type) { TYPE_U64, 0, 0 }
#define S64 (Type) { TYPE_S64, 0, 0 }
#define PTR(t) (Type) { t, 1, 0 }
#define STRUCT(n) (Type) { TYPE_STRUCT, 0, (n) }

typedef struct{
  Type_Type type;
  
  u32 ptr_degree;
  // ptr_degree > 0 => ptr degree of 'type'
  // ptr_degree = 0 => type

  u32 struct_index;
  // WHEN type == TYPE_STRUCT THEN struct_index  => index into 'Structure* structs'
}Type;

#define type_fmt "{ type :: %d, ptr_degree :: %u, struct_index :: %u }"
#define type_arg(t) (t).type, (t).ptr_degree, (t).struct_index

bool type_equal(Type a, Type b) {
  return a.type == b.type &&
    a.ptr_degree == b.ptr_degree &&
    a.struct_index == b.struct_index;
}

#define FUNCCALL_ARGS_CAP 8

typedef enum{
  EXPR_TYPE_NONE = 0,
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
  EXPR_TYPE_CAST,
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

typedef struct{  
  Expr *expr;
  Type to_type;
}Expr_Cast;

struct Expr{
  Expr_Type type;
  union{
    s64 sval;
    Expr_Funccall funccall;
    Expr_Bin_Op bin_op;
    Expr_Strings strings;
    Expr_Cast cast;
  }as;
};

Size type_size(Type type) {
  
  if(type.ptr_degree > 0) {
    
    return SIZE_QWORD;
  } else {
    
    switch(type.type) {

    case TYPE_U64:
      return SIZE_QWORD;

    case TYPE_U8:
      return SIZE_BYTE;

    case TYPE_U32:
      return SIZE_DWORD;

    case TYPE_S64:
      return SIZE_QWORD;
      
    case TYPE_STRUCT:
      panic("Cannot infer size of type without program");

    }
    
    panic("unimplemented type: %d", type.type);
  }
  
}

Type type_ptr(Type type) {
  return (Type) { type.type, type.ptr_degree + 1 };
}

Type type_deref(Type type) {
  return (Type) { type.type, type.ptr_degree - 1 };
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

Var *vars_find(Vars *vars, string name) {

  for(u64 i=0;i<vars->len;i++) {
    Var *var = &vars->data[i];
    if(!string_eq(name, var->name)) {
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
  Type type;
  string name;
}Param;

typedef struct{
  Param *data;
  u64 len;
  u64 cap;
}Params;

typedef struct{
  string name;
  Type return_type;
  Params params;  
}Function;

typedef struct{
  Function *data;
  u64 len;
  u64 cap;
}Functions;

Function *functions_find(Functions *fs, string name) {

  for(u64 i=0;i<fs->len;i++) {
    Function *f = &fs->data[i];
    
    if(string_eq(f->name, name)) {
      return f;
    }
  }

  return NULL;
}

typedef struct{
  Expr expr_pool[16];
  u64 expr_pool_count;

  Structures structs;
  Functions functions;
  Constants constants;

  Vars vars;
  Instrs instrs;

  u64 label_count;
  u64 stack_ptr;
}Program;

bool program_type_check(Program *p, Expr *lhs, Expr *rhs, Type *type);

Type expr_to_type(Program *p, Expr *e) {

  switch(e->type) {

  case EXPR_TYPE_CAST:
    return e->as.cast.to_type;

  case EXPR_TYPE_VALUE:
    return S64;

  case EXPR_TYPE_VARIABLE_DEREF: {

    string name = e->as.strings.fst;

    Var *var = vars_find(&p->vars, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    if(var->type.ptr_degree <= 0) {
      panic("can not derefence type");
    }

    return type_deref(var->type);
    
  } break;
    
  case EXPR_TYPE_VARIABLE_PTR: {

    string name = e->as.strings.fst;

    Var *var = vars_find(&p->vars, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    return type_ptr(var->type);
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD: {

    string name = e->as.strings.fst;
    string field_name = e->as.strings.snd;

    Var *var = vars_find(&p->vars, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }
    assert(var->type.type == TYPE_STRUCT && var->type.struct_index >= 0);
    Structure *structure = &p->structs.data[var->type.struct_index];

    Structure_Field *field = structure_fields_find(&structure->fields, field_name);    
    if(!field) {
      panic("structure: \""str_fmt"\" has no field named: \""str_fmt"\"\n",
	    str_arg(structure->name), str_arg(field_name));
    }

    return field->type;
    
  } break;

  case EXPR_TYPE_VARIABLE: {

    string name = e->as.strings.fst;

    Var *var = vars_find(&p->vars, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    return var->type;
    
  } break;

  case EXPR_TYPE_SUM: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!program_type_check(p, lhs, rhs, &type)) {
      panic("Can not add expressions with different types");
    }

    return type;
    
  } break;    
    
  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!program_type_check(p, lhs, rhs, &type)) {
      panic("Can not subtract expressions with different types");
    }

    return type;
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    string name = e->as.funccall.name;
      
    Function *f = functions_find(&p->functions, name);
    if(f == NULL) {
      panic("Can not find function with the name: '"str_fmt"'\n", str_arg(name));
    }

    return f->return_type;
    
  } break;

  case EXPR_TYPE_CONSTANT: {
    Constant *c = &p->constants.data[e->as.sval];

    switch(c->type) {
      
    case CONSTANT_TYPE_CSTR:
      return PTR(TYPE_U8);

    case CONSTANT_TYPE_S64:
      return S64;
    }

    panic("unimplemented constant_type");
    
  } break;
    
  }

  panic("unimplemented expr_type: %d", e->type);
}

// if compiling EXPR_TYPE_ does
// involve touching registers => false

// if not => true

// RAX and RBX are excluded
bool expr_is_static(Expr *e) {
  
  switch(e->type) {

  case EXPR_TYPE_CONSTANT:
    return true;

  case EXPR_TYPE_VARIABLE_PTR:
    return true;

  case EXPR_TYPE_STRUCT_FIELD:
    return true;

  case EXPR_TYPE_VARIABLE:
    return true;

  case EXPR_TYPE_VALUE:
    return true;

  case EXPR_TYPE_SUM:
  case EXPR_TYPE_SUBTRACTION:
    return false;

  case EXPR_TYPE_FUNCCALL:
    return false;
    
  case EXPR_TYPE_CAST:
    return false;
  }

  panic("unimplemented expr_type");
}


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

Expr *program_expr_append_cast(Program *p, Expr *expr, Type to_type) {
  Expr *e = program_expr_append(p);
  
  e->type = EXPR_TYPE_CAST;
  e->as.cast = (Expr_Cast) { expr, to_type };

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

  for(u64 i=0;i<p->functions.len;i++) {
    string_builder_appendf(sb,
			   "        extern "str_fmt"\n", str_arg(p->functions.data[i].name));
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
    da_append(&p->instrs, ADD(RSP, LITERAL(p->stack_ptr), SIZE_QWORD));
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
  if(vars_find(&p->vars, name)) {
    panic("variable already declared");
  }

  Size size = type_size(type);
  u64 n = size_in_bytes(size);

  da_append(&p->instrs, SUB(RSP, LITERAL(n), SIZE_QWORD));
  p->stack_ptr += n;

  Var var;
  var.name = name;
  var.type = type;
  var.off  = p->stack_ptr;
  
  da_append(&p->vars, var);

  
  p->expr_pool_count = 0;
}

void program_compile_declaration_array(Program *p,
				       string name,
				       Type type,
				       u64 n) {
  if(vars_find(&p->vars, name)) {
    panic("variable already declared");
  }

  Size size = type_size(type);
  Type ptr_type = type_ptr(type);
  Size size_ptr = type_size(ptr_type);
  
  u64 n_ptr = size_in_bytes(size_ptr);
  u64 ns = n * size_in_bytes(size);
  
  da_append(&p->instrs, SUB(RSP, LITERAL(ns + n_ptr), SIZE_QWORD));
  da_append(&p->instrs, LEA(RAX, RSP_OFF(n_ptr)));
  da_append(&p->instrs, MOV(RSP_OFF(0), RAX, size_ptr));
  p->stack_ptr += ns + n_ptr;

  Var var;
  var.name = name;
  var.type = ptr_type;
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
  
  if(vars_find(&p->vars, name)) {
    panic("variable already declared");
  }

  u64 size = 0;
  for(u64 i=0;i<structure->fields.len;i++) {
    Structure_Field *field = &structure->fields.data[i];
    size += size_in_bytes(type_size(field->type));
  }

  da_append(&p->instrs, SUB(RSP, LITERAL(size), SIZE_QWORD));
  p->stack_ptr += size;

  Var var;
  var.name = name;
  var.type = STRUCT((u32)
		    (((unsigned char *) structure - (unsigned char *) p->structs.data) / sizeof(Structure)));
  var.off  = p->stack_ptr;
  da_append(&p->vars, var);
  
  p->expr_pool_count = 0;
}

u64 program_structure_off(Program *p, Var *var, string field_name, Type *out_type) {

  assert(var->type.type == TYPE_STRUCT && var->type.struct_index >= 0);
  Structure *structure = &p->structs.data[var->type.struct_index];

  Structure_Field *field = structure_fields_find(&structure->fields, field_name);
  if(!field) {
    panic("structure: \""str_fmt"\" has no field named: \""str_fmt"\"\n",
	  str_arg(structure->name), str_arg(field_name));
  }
  if(out_type) *out_type = field->type;
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
  
  Var *var = vars_find(&p->vars, name);
  if(!var) {
    panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
  }  

  Value result;
  switch(e->type) {

  case EXPR_TYPE_VARIABLE: {
    result = RSP_OFF(p->stack_ptr - var->off);
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {
    result = RSP_OFF(p->stack_ptr - var->off);
  } break;

  case EXPR_TYPE_STRUCT_FIELD: {
    
    if(var->type.type != TYPE_STRUCT) {
      panic("variable with the name: \""str_fmt"\" is not a structure\n", str_arg(name));
    }
    
    result = RSP_OFF(p->stack_ptr - var->off + program_structure_off(p, var, field_name, NULL));
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
    *out_type = expr_to_type(p, e);
  }

  return result;
}

Type program_funccall_compile(Program *p, Expr_Funccall *funccall);

void program_compile_smart(Program *p,
			   Expr *lhs, Value lhs_loc,
			   Expr *rhs, Value rhs_loc,
			   Size size);

bool program_type_check(Program *p, Expr *lhs, Expr *rhs, Type *type) {

  Type lhs_type = expr_to_type(p, lhs);
  Type rhs_type = expr_to_type(p, rhs);
  
  bool okay = type_equal(lhs_type, rhs_type);  

  if(okay) {
    if(type) *type = rhs_type;
    return true;
  } else {
    printf(type_fmt", "type_fmt, type_arg(lhs_type), type_arg(rhs_type));
    return false;
  }
  
}

void program_expr_compile(Program *p,
			  Expr *e,
			  Value location,
			  Size cast_size) {

  Value temp;
  if(location.as.sval == 0) { // RAX := 0
    temp = RBX;
  } else {
    temp = RAX;
  }

  switch(e->type) {

  case EXPR_TYPE_VALUE: {
    
    s64 value = e->as.sval;
    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(S64);
    } else {
      size = cast_size;
    }
    
    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(&p->instrs, MOV(location, LITERAL(value), size));
    } else {
      da_append(&p->instrs, MOV(temp, LITERAL(value), size));
      da_append(&p->instrs, MOV(location, temp, size));
    }
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    Type type = program_funccall_compile(p, &e->as.funccall);

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }
    
    da_append(&p->instrs, MOV(location, RAX, size));
    
  } break;

  case EXPR_TYPE_CONSTANT: {

    Constant *c = &p->constants.data[e->as.sval];

    Size value_size;
    if(cast_size == SIZE_NONE) {

      
      switch(c->type) {

      case CONSTANT_TYPE_CSTR:
	value_size = SIZE_QWORD; // pointer-size
	break;
	
      case CONSTANT_TYPE_S64:
	value_size = size_of_s64(c->as.sval);
	break;
	
      default:
	panic("unimplemented content->type");
      }
      
    } else {
      value_size = cast_size;
    }
    
    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(&p->instrs, MOV(location, CONSTANT(e->as.sval), value_size));
    } else {
      da_append(&p->instrs, MOV(temp, CONSTANT(e->as.sval), value_size));
      da_append(&p->instrs, MOV(location, temp, value_size));
    }
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD:
  case EXPR_TYPE_VARIABLE: {

    Type variable_type;
    Value variable_location = program_expr_location(p, e, &variable_type);
    Size variable_size;
    if(cast_size == SIZE_NONE) {
      variable_size = type_size(variable_type);
    } else {
      variable_size = cast_size;
    }

    if(location.type == VALUE_TYPE_REGISTER_OFF) {

      da_append(&p->instrs, MOV(temp, variable_location, variable_size));
      da_append(&p->instrs, MOV(location, temp, variable_size));
      
    } else {
      da_append(&p->instrs, MOV(location, variable_location, variable_size));
    }
    
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {

    Type variable_type;
    Value variable_location = program_expr_location(p, e, &variable_type);
        
    Size variable_ptr_size;
    if(cast_size == SIZE_NONE) {
      Type variable_ptr_type = type_ptr(variable_type);
      variable_ptr_size = type_size(variable_ptr_type);
    } else {
      variable_ptr_size = cast_size;
    }

    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(&p->instrs, LEA(location, variable_location));
    } else {
      da_append(&p->instrs, LEA(temp, variable_location));
      da_append(&p->instrs, MOV(location, temp, variable_ptr_size));
    }    
    
  } break;

  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!program_type_check(p, lhs, rhs, &type)) {
      panic("Can not subtract expressions with different types");
    }
    
    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    program_compile_smart(p,
			  lhs, RAX,
			  rhs, RBX,
			  type_size(type));
    
    da_append(&p->instrs, SUB(RAX, RBX, size));
    da_append(&p->instrs, MOV(location, RAX, size));
    
  } break;

  case EXPR_TYPE_CAST: {

    Type type = expr_to_type(p, e->as.cast.expr);
    Size size = type_size(type);

    Type to_cast_type = e->as.cast.to_type;
    Size to_cast_size = type_size(to_cast_type);

    if(size == to_cast_size) {
      program_expr_compile(p, e->as.cast.expr, location, to_cast_size);
    } else if(size > to_cast_size) {
      program_expr_compile(p, e->as.cast.expr, location, to_cast_size);
    } else  { // size < to_cast_size
      da_append(&p->instrs, MOV(temp, LITERAL(0), SIZE_QWORD));
      program_expr_compile(p, e->as.cast.expr, temp, size);
      da_append(&p->instrs, MOV(location, temp, to_cast_size));
    }        
    
  } break;

  case EXPR_TYPE_SUM: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;
    
    Type type;
    if(!program_type_check(p, lhs, rhs, &type)) {
      panic("Can not add expressions with different types");
    }

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    program_compile_smart(p,
			  lhs, RAX,
			  rhs, RBX,
			  type_size(type));
    
    da_append(&p->instrs, ADD(RAX, RBX, size));
    da_append(&p->instrs, MOV(location, RAX, size));
    
  } break;
    
  default:
    panic("Unimplemented expr->type");
    
  }
  
}

void program_compile_smart(Program *p,
			   Expr *lhs, Value lhs_loc,
			   Expr *rhs, Value rhs_loc,
			   Size size) {

  bool is_static = expr_is_static(rhs);

  /* printf("program_compile_smart - static=%s, expr_type: %d\n", */
  /* 	 is_static ? "true" : "false", rhs->type); */
  
  if(is_static) {
    program_expr_compile(p, lhs, lhs_loc, size);
    program_expr_compile(p, rhs, rhs_loc, size);
  } else {
    u64 size_bytes = size_in_bytes(size);
    
    da_append(&p->instrs, SUB(RSP, LITERAL(size_bytes), SIZE_QWORD));
    p->stack_ptr += size_bytes;
    program_expr_compile(p, lhs, RSP_OFF(0), size);
    
    program_expr_compile(p, rhs, rhs_loc, size);
    
    da_append(&p->instrs, MOV(lhs_loc, RSP_OFF(0), size));
    da_append(&p->instrs, ADD(RSP, LITERAL(size_bytes), SIZE_WORD));
    p->stack_ptr -= size_bytes;
    
  }

}

Type program_funccall_compile(Program *p, Expr_Funccall *funccall) {

  Function *f = functions_find(&p->functions, funccall->name);
  if(f == NULL) {
    panic("Can not find function with the name: '"str_fmt"'\n", str_arg(funccall->name));
  }

  if(f->params.len != funccall->args_len) {
    panic("Function: '"str_fmt"'. Number of parameters: %llu and number of arguments: %llu does not match",
	  str_arg(f->name), f->params.len, funccall->args_len);
  }

  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *arg = funccall->args[i];
    Type arg_type = expr_to_type(p, arg);

    Param *param = &f->params.data[i];
    Type param_type = param->type;

    if(!type_equal(arg_type, param_type)) {
      printf(type_fmt", "type_fmt, type_arg(arg_type), type_arg(param_type));
      panic("Function: '"str_fmt"'. Argument does not match Parameter: '"str_fmt"'",
	    str_arg(f->name),
	    str_arg(param->name));
    }
    
  }

  ////////////////////////////////////////////////////////////////////

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
  da_append(&p->instrs, SUB(RSP, LITERAL(shadow_space), SIZE_QWORD));

  // calculate argument-expressions and save them in the shadowspace
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];

    if(expr_is_static(e)) {
      // pass
    } else {
      program_expr_compile(p, e, RSP_OFF(p->stack_ptr - stack_ptr_before - shadow_space + i  * 8), SIZE_NONE);
    }
    
    /* if(e->type == EXPR_TYPE_VARIABLE) { */
    /*   //pass */
    /* } else { */
            
    /*   program_expr_compile(p, e, RSP_OFF(p->stack_ptr - stack_ptr_before - shadow_space + i  * 8 */
    /* 					 //i  * 8 */
    /* 					 )); */
    /* } */
  }

  // move the first four arguments out of the shadowspace
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];
    s64 target = (s64) p->stack_ptr - (s64) stack_ptr_before - (s64) shadow_space + i  * 8;
    //s64 target = i  * 8;

    Param *param = &f->params.data[i];
    Type param_type = param->type;
    Size param_size = type_size(param_type);

    if(i < FASTCALL_REGISTER_COUNT) { // rcx, rdx, r8, r9

      if(expr_is_static(e)) {
	program_expr_compile(p, e, REGISTER(FASTCALL_REGISTERS[i]), SIZE_NONE);
      } else {
	da_append(&p->instrs, MOV(REGISTER(FASTCALL_REGISTERS[i]), RSP_OFF(target), param_size));
      }

      /* Value source; */
      /* if(e->type == EXPR_TYPE_VARIABLE) { */
      /* 	source = program_expr_location(p, e, NULL); */
      /* } else { */
      /* 	source = RSP_OFF(target); */
      /* } */

      /* da_append(&p->instrs, MOV(REGISTER(FASTCALL_REGISTERS[i]), source, size)); */
    } else { // push ...

      if(expr_is_static(e)) {
	program_expr_compile(p, e, RAX, SIZE_NONE);
	da_append(&p->instrs, MOV(RSP_OFF(target), RAX, param_size));
      } else {
	// variable already in the right place
      }

      /* if(e->type == EXPR_TYPE_VARIABLE) { */

      /* 	da_append(&p->instrs, MOV(RAX, program_expr_location(p, e, NULL), size)); */
      /* 	da_append(&p->instrs, MOV(RSP_OFF(target), RAX, size)); */
      /* } else { */

      /* 	// variable already in the right place */
      /* } */
      
    }
    

    
  }  
  
  da_append(&p->instrs, CALL(WORD(funccall->name)));

  da_append(&p->instrs, ADD(RSP, LITERAL(p->stack_ptr - stack_ptr_before), SIZE_QWORD));
  p->stack_ptr = stack_ptr_before;

  return f->return_type;
}

void program_compile_assignment(Program *p,
				Expr *lhs,
				Expr *rhs) {

  if(!program_type_check(p, lhs, rhs, NULL)) {
    panic("Can not assign expressions with different type");
  }

  Value location = program_expr_location(p, lhs, NULL);
  program_expr_compile(p, rhs, location, SIZE_NONE);
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

  Type type;
  if(!program_type_check(p, lhs, rhs, &type)) {
    panic("Can compare expressions with different types"); 
  }
  Size size = type_size(type);
      
  program_compile_smart(p,
			lhs, RAX,
			rhs, RBX,
			size);  
  da_append(&p->instrs, CMP(RAX, RBX, size));

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
    da_append(&p->instrs, ADD(RSP, LITERAL(p->stack_ptr - state.stack_ptr), SIZE_QWORD));
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

void slurp_file_program(Program *p) {

  // handle : u64* = CreateFileA("main.asm", (u32) GENERIC_READ, (u32) FILE_SHARE_READ, (u64*) 0, (u32) OPEN_EXISTING, (u32) FILE_ATTRIBUTE_NORMAL, (u64*) NULL);
  program_compile_declaration(p,
			      string_from_cstr("handle"),
			      PTR(TYPE_U64));  
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("handle")),
			     program_expr_append_funccall(p,
							  string_from_cstr("CreateFileA"),
							  program_expr_append_constant(p,
										       constants_append_cstr(&p->constants,
													     "main.fe")),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, GENERIC_READ),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, FILE_SHARE_READ),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, 0),
										   PTR(TYPE_U64)),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, OPEN_EXISTING),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, FILE_ATTRIBUTE_NORMAL),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, 0),
										   PTR(TYPE_U64))));

  // if(handle == INVALID_HANDLE_VALUE) {
  Foo state = program_compile_if_begin(p,
				       program_expr_append_variable(p,
								    string_from_cstr("handle")),
				       STMT_IF_TYPE_EQUALS,
				       program_expr_append_cast(p,
								program_expr_append_value(p,
											  (s64) INVALID_HANDLE_VALUE),
								PTR(TYPE_U64)));
  //     error : u32 = GetLastError()
  program_compile_declaration(p,
			      string_from_cstr("error"),
			      U32);
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("error")),
			     program_expr_append_funccall(p,
							  string_from_cstr("GetLastError")));

  //     ExitProcess((u8) error);
  program_compile_funccall(p,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_cast(p,
						    program_expr_append_variable(p, string_from_cstr("error")),
						    U8));
  // }
  program_compile_if_end(p, state);

  // process_heap : u64* = GetProcessHeap();
  program_compile_declaration(p,
			      string_from_cstr("process_heap"),
			      PTR(TYPE_U64));
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("process_heap")),
			     program_expr_append_funccall(p,
							  string_from_cstr("GetProcessHeap")));

  // size : u32 = GetFileSize(handle, (u32*) 0);
  program_compile_declaration(p,
			      string_from_cstr("size"),
			      U32);
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("size")),
			     program_expr_append_funccall(p,
							  string_from_cstr("GetFileSize"),
							  program_expr_append_variable(p,
										       string_from_cstr("handle")),
							  program_expr_append_cast(p,
										   program_expr_append_value(p,
													     0),
										   PTR(TYPE_U32))));

  // size_big : u64 = (u64) size;
  program_compile_declaration(p,
			      string_from_cstr("size_big"),
			      U64);
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("size_big")),
			     program_expr_append_cast(p,
						      program_expr_append_variable(p,
										   string_from_cstr("size")),
						      U64));
  
  // space : u8* = HeapAlloc(process_heap, (u32) 0, (u64) size);
  program_compile_declaration(p,
			      string_from_cstr("space"),
			      PTR(TYPE_U8));
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("space")),
			     program_expr_append_funccall(p,
							  string_from_cstr("HeapAlloc"),
							  program_expr_append_variable(p,
										       string_from_cstr("process_heap")),
							  program_expr_append_cast(p,
										   program_expr_append_value(p,
													     0),
										   U32),	  
							  program_expr_append_variable(p,
										       string_from_cstr("size_big"))
							  ));
  // written : u32;
  program_compile_declaration(p,
			      string_from_cstr("written"),
			      U32);
  
  // ReadFile(handle, space, size, &written, (u64*) 0);
  program_compile_funccall(p,
			   string_from_cstr("ReadFile"),
			   program_expr_append_variable(p,
							string_from_cstr("handle")),
			   program_expr_append_variable(p,
							string_from_cstr("space")),
			   program_expr_append_variable(p,
							string_from_cstr("size")),
			   program_expr_append_pointer(p,
						       string_from_cstr("written")),
			   program_expr_append_cast(p,
						    program_expr_append_value(p,
									      0),
						    PTR(TYPE_U64)));

  // WriteFile(GetStdHandle((u32) -11), space, size, &written, (u64) NULL);
  program_compile_funccall(p,
			   string_from_cstr("WriteFile"),
			   program_expr_append_funccall(p,
							string_from_cstr("GetStdHandle"),
						        program_expr_append_cast(p,
										 program_expr_append_value(p,
													   -11),
										 U32)),
			   program_expr_append_variable(p,
							string_from_cstr("space")),
			   program_expr_append_variable(p,
							string_from_cstr("size")),
			   program_expr_append_pointer(p,
						       string_from_cstr("written")),
			   program_expr_append_cast(p,
						    program_expr_append_value(p,
									      0),
						    PTR(TYPE_U64))
			   );
  
  // CloseHandle(handle);
  program_compile_funccall(p,
			   string_from_cstr("CloseHandle"),
			   program_expr_append_variable(p,
							string_from_cstr("handle")));
}

void buffered_slurp_file_program(Program *p) {

  // handle : u64*;
  program_compile_declaration(p,
			      string_from_cstr("handle"),
			      PTR(TYPE_U64));
  
  // handle = CreateFileA("main.asm", (u32) GENERIC_READ, (u32) FILE_SHARE_READ, (u64*) 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("handle")),
			     program_expr_append_funccall(p,
							  string_from_cstr("CreateFileA"),
							  program_expr_append_constant(p,
										       constants_append_cstr(&p->constants,
													     "main.fe")),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, GENERIC_READ),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, FILE_SHARE_READ),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, 0),
										   PTR(TYPE_U64)),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, OPEN_EXISTING),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, FILE_ATTRIBUTE_NORMAL),
										   U32),
							  program_expr_append_cast(p,
										   program_expr_append_value(p, 0),
										   PTR(TYPE_U64))));

  // if(handle == INVALID_HANDLE_VALUE) {
  Foo state = program_compile_if_begin(p,
				       program_expr_append_cast(p,
								program_expr_append_variable(p,
											     string_from_cstr("handle")),
							        S64),
				       STMT_IF_TYPE_EQUALS,
				       program_expr_append_value(p,
								 (s64) INVALID_HANDLE_VALUE));
  //     ExitProcess(GetLastError());
  program_compile_funccall(p,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_cast(p,
						    program_expr_append_funccall(p,
										 string_from_cstr("GetLastError")),
						    U8));
  // }
  program_compile_if_end(p, state);

  // size : u32;
  program_compile_declaration(p,
			      string_from_cstr("size"),
			      U32);

  // size = GetFileSize(handle, NULL);
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("size")),			     
			     program_expr_append_funccall(p,
							  string_from_cstr("GetFileSize"),
							  program_expr_append_variable(p,
										       string_from_cstr("handle")),
							  program_expr_append_cast(p,program_expr_append_value(p,
													       0),
										   PTR(TYPE_U32))));

  // written : u32;
  program_compile_declaration(p,
			      string_from_cstr("written"),
			      U32);

  s64 buf_size = 2;
  // buf : u8[8]
  program_compile_declaration_array(p,
				    string_from_cstr("buf"),
				    U8,
				    buf_size);

  // len : u32;
  program_compile_declaration(p,
			      string_from_cstr("len"),
			      U32);

  // stdOutputHandle : u64* = GetStdHandle(-11);
  program_compile_declaration(p,
			      string_from_cstr("stdOutputHandle"),
			      PTR(TYPE_U64));
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("stdOutputHandle")),
			     program_expr_append_funccall(p,
							  string_from_cstr("GetStdHandle"),
							  program_expr_append_cast(p,
										   program_expr_append_value(p,
													     -11),
										   U32)));

  // while(size != 0) {
  Foo loop_state = program_compile_while_begin(p,
					       program_expr_append_variable(p,
									    string_from_cstr("size")),
					       STMT_IF_TYPE_NOT_EQUALS,
					       program_expr_append_cast(p,
									program_expr_append_value(p,
												  0),
								        U32));

  //     len = 8;
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("len")),
			     program_expr_append_cast(p,
						      program_expr_append_value(p, buf_size),
						      U32));

  //     if(size < len) {
  state = program_compile_if_begin(p,
				   program_expr_append_variable(p,
								string_from_cstr("size")),
				   STMT_IF_TYPE_LESS,
				   program_expr_append_variable(p,
								string_from_cstr("len")));


  //         len = size
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("len")),
			     program_expr_append_variable(p,
							  string_from_cstr("size")));

  //     }
  program_compile_if_end(p, state);

  //     ReadFile(handle, buf, len, &written, NULL);
  program_compile_funccall(p,
			   string_from_cstr("ReadFile"),
			   program_expr_append_variable(p,
							string_from_cstr("handle")),
			   program_expr_append_variable(p,
							string_from_cstr("buf")),
			   program_expr_append_variable(p,
							string_from_cstr("len")),
			   program_expr_append_pointer(p,
						       string_from_cstr("written")),
			   program_expr_append_cast(p,
						    program_expr_append_value(p,
									      0),
						    PTR(TYPE_U64)));

  //     size = size - written
  program_compile_assignment(p,
			     program_expr_append_variable(p,
							  string_from_cstr("size")),
			     program_expr_append_subtraction(p,
							     program_expr_append_variable(p,
											  string_from_cstr("size")),							     
							     program_expr_append_variable(p,
											  string_from_cstr("written"))));

  //     WriteFile(stdOutputHandle, buf, written, &written, NULL);
  program_compile_funccall(p,
			   string_from_cstr("WriteFile"),
			   program_expr_append_variable(p,
							string_from_cstr("stdOutputHandle")),
			   program_expr_append_variable(p,
							string_from_cstr("buf")),
			   program_expr_append_variable(p,
							string_from_cstr("written")),
			   program_expr_append_cast(p,
						    program_expr_append_pointer(p,
										string_from_cstr("written")),
						    PTR(TYPE_U32)),
			   program_expr_append_cast(p,
						    program_expr_append_value(p,
									      0),
						    PTR(TYPE_U64))
			   );

  // }
  program_compile_while_end(p, loop_state);

  // CloseHandle(handle);
  program_compile_funccall(p,
			   string_from_cstr("CloseHandle"),
			   program_expr_append_variable(p,
							string_from_cstr("handle")));

  // ExitProcess(size);  
  program_compile_funccall(p,
			   string_from_cstr("ExitProcess"),
			   program_expr_append_cast(p,
						    program_expr_append_variable(p,
										 string_from_cstr("size")),
						    U8));  
}

void struct_example_program(Program *p) {

  /*
    struct :: string {
    u8* data;
    u64 len;
    }
  */
  Structure_Fields string_fields = {0};
  da_append(&string_fields, ((Structure_Field) {
	.name = string_from_cstr("data"),
	.type = PTR(TYPE_U8),
      }));
  da_append(&string_fields, ((Structure_Field) {
	.name = string_from_cstr("len"),
	.type = TYPE_U64,
      }));
  da_append(&p->structs, ((Structure) {
	.name = string_from_cstr("string"),
	.fields = string_fields,
      }));

  // s : string;
  program_compile_declaration_struct(p,
				     string_from_cstr("s"),
				     string_from_cstr("string"));

  // s = "Hello, World!";
  string s_content = string_from_cstr("Hello, World!\n");
  program_compile_assignment(p,
			     program_expr_append_struct_field(p,
							      string_from_cstr("s"),
							      string_from_cstr("data")),
			     program_expr_append_constant(p,
							  constants_append_cstr(&p->constants,
										s_content.data)));
  program_compile_assignment(p,
			     program_expr_append_struct_field(p,
							      string_from_cstr("s"),
							      string_from_cstr("len")),
			     program_expr_append_cast(p,
						      program_expr_append_constant(p,
										   constants_append_s64(&p->constants,
													s_content.len)),
						      U64));

  // written : u64;
  program_compile_declaration(p,
			      string_from_cstr("written"),
			      U32);

  // WriteFile(GetStdHandle(-11), s.data, s.len, &written, NULL);
  program_compile_funccall(p,
			   string_from_cstr("WriteFile"),
			   program_expr_append_funccall(p,
							string_from_cstr("GetStdHandle"),
						        program_expr_append_cast(p,
										 program_expr_append_value(p,
													   -11),
										 U32)),
			   program_expr_append_struct_field(p,
							    string_from_cstr("s"),
							    string_from_cstr("data")),
			   program_expr_append_cast(p,
						    program_expr_append_struct_field(p,
										     string_from_cstr("s"),
										     string_from_cstr("len")),
						    U32),
			   program_expr_append_pointer(p,
						       string_from_cstr("written")),
			   program_expr_append_cast(p,
						    program_expr_append_value(p,
											   0),
						    PTR(TYPE_U64)));

			   }

    void dcba_program(Program *p) {

    // buf : u8[8];
    program_compile_declaration_array(p,
				      string_from_cstr("buf"),
				      U8,
				      8);

    // buf_len: u32 = (u32) 0;
    program_compile_declaration(p,
				string_from_cstr("buf_len"),
				U32);
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("buf_len")),
			       program_expr_append_cast(p,
							program_expr_append_value(p,
										  0),
							U32));

    // i : u32 = (u32) 4;
    program_compile_declaration(p,
				string_from_cstr("i"),
				U32);
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("i")),
			       program_expr_append_cast(p,
							program_expr_append_value(p,
										  4),
							U32));

    // ptr : u8* = buf + (i - 1);
    program_compile_declaration(p,
				string_from_cstr("ptr"),
				PTR(TYPE_U8));
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("ptr")),
			       program_expr_append_sum(p,
						       program_expr_append_variable(p,
										    string_from_cstr("buf")),
						       program_expr_append_cast(p,
										program_expr_append_subtraction(p,
														program_expr_append_variable(p,
																	     string_from_cstr("i")),
														program_expr_append_cast(p,
																	 program_expr_append_value(p,
																				   1),
																	 U32)),
										PTR(TYPE_U8))));

    // value : u8 = 65
    program_compile_declaration(p,
				string_from_cstr("value"),
				U8);
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("value")),
			       program_expr_append_cast(p,
							program_expr_append_value(p,
										  65),
							U8));
  
    // while(i > 0) {
    Foo for_loop = program_compile_while_begin(p,
					       program_expr_append_variable(p,
									    string_from_cstr("i")),
					       STMT_IF_TYPE_GREATER,
					       program_expr_append_cast(p,
									program_expr_append_value(p,
												  0),
									U32));

    //     *ptr = value;
    program_compile_assignment(p,
			       program_expr_append_deref(p,
							 string_from_cstr("ptr")),
			       program_expr_append_variable(p,
							    string_from_cstr("value")));
  
    //     ptr = ptr - (u8 *) 1
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("ptr")),
			       program_expr_append_subtraction(p,
							       program_expr_append_variable(p,
											    string_from_cstr("ptr")),
							       program_expr_append_cast(p,
											program_expr_append_value(p,
														  1),
											PTR(TYPE_U8))));

    //     value = value + (u8) 1
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("value")),
			       program_expr_append_sum(p,
						       program_expr_append_variable(p,
										    string_from_cstr("value")),
						       program_expr_append_cast(p,
										program_expr_append_value(p,
													  1),
										U8)));

    //     buf_len = buf_len + (u32) 1
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("buf_len")),
			       program_expr_append_sum(p,
						       program_expr_append_variable(p,
										    string_from_cstr("buf_len")),
						       program_expr_append_cast(p,
										program_expr_append_value(p,
													  1),
										U32)));
    //     i = i - (u32) 1
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("i")),
			       program_expr_append_subtraction(p,
							       program_expr_append_variable(p,
											    string_from_cstr("i")),
							       program_expr_append_cast(p,
											program_expr_append_value(p,
														  1),
											U32)));

    // }
    program_compile_while_end(p, for_loop);
  

    // if(buf_len > (u32) 0) {
    Foo buf_len_positive = program_compile_if_begin(p,
						    program_expr_append_variable(p,
										 string_from_cstr("buf_len")),
						    STMT_IF_TYPE_GREATER,
						    program_expr_append_cast(p,
									     program_expr_append_value(p,
												       0),
									     U32));

    //     handle : u64* = GetStdHandle((u32) -11);
    program_compile_declaration(p,
				string_from_cstr("handle"),
				(Type) { TYPE_U64, 1});
    program_compile_assignment(p,
			       program_expr_append_variable(p,
							    string_from_cstr("handle")),
			       program_expr_append_funccall(p,
							    string_from_cstr("GetStdHandle"),
							    program_expr_append_cast(p,
										     program_expr_append_value(p,
													       -11),
										     U32)));

    //     if(handle == (u64*) INVALID_HAMDLE_VALUE) {
    Foo GetStdHandleFailed = program_compile_if_begin(p,
						      program_expr_append_variable(p,
										   string_from_cstr("handle")),
						      STMT_IF_TYPE_EQUALS,
						      program_expr_append_cast(p,
									       program_expr_append_value(p,
													 (s64) INVALID_HANDLE_VALUE),
									       PTR(TYPE_U64)));
    //         ExitProcess(GetLastError());
    program_compile_funccall(p,
			     string_from_cstr("ExitProcess"),
			     program_expr_append_cast(p,
						      program_expr_append_funccall(p,
										   string_from_cstr("GetLastError")),
						      U8));


    //     }
    program_compile_if_end(p, GetStdHandleFailed);
  
    //     written : u32;
    program_compile_declaration(p,
				string_from_cstr("written"),
				U32);

    //     if(WriteFile(handle, buf, buf_len, &written, (u64 *) NULL) == (u8) 0) {
    Foo WriteFileFailed = program_compile_if_begin(p,
						   program_expr_append_funccall(p,
										string_from_cstr("WriteFile"),
										program_expr_append_variable(p,
													     string_from_cstr("handle")),
										program_expr_append_variable(p,
													     string_from_cstr("buf")),
										program_expr_append_variable(p,
													     string_from_cstr("buf_len")),
										program_expr_append_pointer(p,
													    string_from_cstr("written")),
										program_expr_append_cast(p,
													 program_expr_append_value(p,
																   0),
													 PTR(TYPE_U64))),
						   STMT_IF_TYPE_EQUALS,
						   program_expr_append_cast(p,
									    program_expr_append_value(p,
												      0),
									    U8));

    //         ExitProcess(GetLastError());
    program_compile_funccall(p,
			     string_from_cstr("ExitProcess"),
			     program_expr_append_cast(p,
						      program_expr_append_funccall(p,
										   string_from_cstr("GetLastError")),
						      U8));
  
    //     }
    program_compile_if_end(p, WriteFileFailed);

    // }
    program_compile_if_end(p, buf_len_positive);
  }

  // TODO:
  //     functions
  //     type checking

  //     remove unnecassary instructions

  int main() {

    Program program = {0};

    /*
      u64* CreateFileA(
      u8* fileName,
      u32 desiredAccess,
      u32 sharedMode,
      u64* securiyAttributes,
      u32 creationDisposition,
      u32 flagsAndAttributes,
      u64* templateFile);
    */

    Function createFileA = {0};
    createFileA.return_type = PTR(TYPE_U64);
    createFileA.name = string_from_cstr("CreateFileA");
    da_append(&createFileA.params, ((Param) {
	  .type = PTR(TYPE_U8),
	  .name = string_from_cstr("fileName"),
	}));
    da_append(&createFileA.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("desiredAccess"),
	}));
    da_append(&createFileA.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("sharedMode"),
	}));
    da_append(&createFileA.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("securityAttributes"),
	}));
    da_append(&createFileA.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("createDisposition"),
	}));
    da_append(&createFileA.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("flagsAndAttributes"),
	}));
    da_append(&createFileA.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("templateFile"),
	}));
    da_append(&program.functions, createFileA);

    /*
      void ExitProcess(u8 exitCode);
    */

    Function exitProcess = {0};
    exitProcess.return_type = _VOID;
    exitProcess.name = string_from_cstr("ExitProcess");
    da_append(&exitProcess.params, ((Param) {
	  .type = U8,
	  .name = string_from_cstr("exitCode"),
	}));
    da_append(&program.functions, exitProcess);

    /*
      u32 GetLastError();
    */
    Function getLastError = {0};
    getLastError.return_type = U32;
    getLastError.name = string_from_cstr("GetLastError");
    da_append(&program.functions, getLastError);

    /*
      u32 GetFileSize(u64 *file, u32 *high);
    */
    Function getFileSize = {0};
    getFileSize.return_type = U32;
    getFileSize.name = string_from_cstr("GetFileSize");
    da_append(&getFileSize.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("file")
	}));
    da_append(&getFileSize.params, ((Param) {
	  .type = PTR(TYPE_U32),
	  .name = string_from_cstr("high"),
	}));
    da_append(&program.functions, getFileSize);

    /*
      u64* GetStdHandle(u32 stdHandle);
    */
    Function getStdHandle = {0};
    getStdHandle.return_type = PTR(TYPE_U64);
    getStdHandle.name = string_from_cstr("GetStdHandle");
    da_append(&getStdHandle.params, ((Param){
	  .type = U32,
	  .name = string_from_cstr("stdHandle")
	}));
    da_append(&program.functions, getStdHandle);

    /*
      u8 ReadFile(
      u64* file,
      u8* buffer,
      u32 numberOfBytesToRead,
      u32* numberOfBytesRead,
      u64* overlapped);
    */
    Function readFile = {0};
    readFile.return_type = U8;
    readFile.name = string_from_cstr("ReadFile");
    da_append(&readFile.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("file")
	}));
    da_append(&readFile.params, ((Param) {
	  .type = PTR(TYPE_U8),
	  .name = string_from_cstr("buffer")
	}));  
    da_append(&readFile.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("numberOfBytesToRead")
	}));
    da_append(&readFile.params, ((Param) {
	  .type = PTR(TYPE_U32),
	  .name = string_from_cstr("numberOfBytesRead")
	}));
    da_append(&readFile.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("overlapped")
	}));  
    da_append(&program.functions, readFile);

    /*
      u8 WriteFile(
      u64* file,
      u8* buffer,
      u32 numberOfBytesToWrite,
      u32* numberOfBytesWritten,
      u64* overlapped);
    */
    Function writeFile = {0};
    writeFile.return_type = U8;
    writeFile.name = string_from_cstr("WriteFile");
    da_append(&writeFile.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("file")
	}));
    da_append(&writeFile.params, ((Param) {
	  .type = PTR(TYPE_U8),
	  .name = string_from_cstr("buffer")
	}));  
    da_append(&writeFile.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("numberOfBytesToWrite")
	}));
    da_append(&writeFile.params, ((Param) {
	  .type = PTR(TYPE_U32),
	  .name = string_from_cstr("numberOfBytesWritten")
	}));
    da_append(&writeFile.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("overlapped")
	}));  
    da_append(&program.functions, writeFile);

    /*
      u8 CloseHandle(u64 *object);
    */
    Function closeHandle = {0};
    closeHandle.return_type = U8;
    closeHandle.name = string_from_cstr("CloseHandle");
    da_append(&closeHandle.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("object"),
	}));
    da_append(&program.functions, closeHandle);

    /*
      u64 *GetProcessHeap();
    */
    Function getProcessHeap = {0};
    getProcessHeap.return_type  = PTR(TYPE_U64);
    getProcessHeap.name = string_from_cstr("GetProcessHeap");
    da_append(&program.functions, getProcessHeap);

    /*
      u8* HeapAlloc(u64 *heap,
      u32 flags,
      u64 bytes);
    */
    Function heapAlloc = {0};
    heapAlloc.return_type = PTR(TYPE_U8);
    heapAlloc.name = string_from_cstr("HeapAlloc");
    da_append(&heapAlloc.params, ((Param) {
	  .type = PTR(TYPE_U64),
	  .name = string_from_cstr("heap")
	}));
    da_append(&heapAlloc.params, ((Param) {
	  .type = U32,
	  .name = string_from_cstr("flags")
	}));
    da_append(&heapAlloc.params, ((Param) {
	  .type = U64,
	  .name = string_from_cstr("bytes")
	}));
    da_append(&program.functions, heapAlloc);
    
    struct_example_program(&program);
    //dcba_program(&program);
    //slurp_file_program(&program);
    //buffered_slurp_file_program(&program);
  
    string_builder sb = {0};
    program_append(&program, &sb);
    printf("%.*s", (int) sb.len, sb.data);

    if(!io_write_file("main.asm", (u8 *) sb.data, sb.len)) {
      return 1;
    }


    return 0;
  }
