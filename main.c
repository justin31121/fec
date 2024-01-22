#include <stdio.h>
#include <assert.h>

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
  INSTR_TYPE_RET,
  INSTR_TYPE_PUSH,
  INSTR_TYPE_POP,
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
#define RET ((Instr) { .type = INSTR_TYPE_RET, .size = (0) })
#define PUSH(l) ((Instr) { .type = INSTR_TYPE_PUSH, .lhs = (l), .size = (0) })
#define POP(l) ((Instr) { .type = INSTR_TYPE_POP, .lhs = (l), .size = (0) })

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

  case INSTR_TYPE_RET: {

    string_builder_appendf(sb, "        ret\n");
    
  } break;

  case INSTR_TYPE_PUSH: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {
      string_builder_appendf(sb, "        push %s\n",
			     REGISTER_NAMES[lhs.as.sval][0]);
    } break;

      
    default:
      panic("unimplemented lhs_type");
    }
    
  } break;

  case INSTR_TYPE_POP: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {
      string_builder_appendf(sb, "        pop %s\n",
			     REGISTER_NAMES[lhs.as.sval][0]);
    } break;
      
    default:
      panic("unimplemented lhs_type");
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

Type type_deref(Type type) {
  return (Type) { type.type, type.ptr_degree - 1 };
}

Type type_ptr(Type type) {
  return (Type) { type.type, type.ptr_degree + 1 };
}

bool type_equal(Type a, Type b) {
  return a.type == b.type &&
    a.ptr_degree == b.ptr_degree &&
    a.struct_index == b.struct_index;
}

#define type_fmt "{ type :: %d, ptr_degree :: %u, struct_index :: %u }"
#define type_arg(t) (t).type, (t).ptr_degree, (t).struct_index

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

typedef struct{
  Type type;
  string name;
}Param;

typedef struct{
  Param *data;
  u64 len;
  u64 cap;
}Params;

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

void constant_append(Constant *c, u64 k, string_builder *sb) {

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

#define BATCH_SIZE 4

typedef struct{
  Constant *data;
  u64 len;
  u64 cap;
}Constants;

u64 constants_append_cstr(Constants *cs, const char *cstr) {
  da_append(cs, ((Constant) {
	.type = CONSTANT_TYPE_CSTR,
	.as.cstrval = cstr
      }));
  
  return cs->len - 1;
}

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

typedef struct Exprs Exprs;
struct Exprs{
  Expr *data;
  u64 len;  
  Exprs *next;
};

Expr *exprs_append(Exprs *es) {
  
  if(es->len == 0) {
    es->data = malloc(sizeof(Expr) * BATCH_SIZE);
    assert(es->data);
    
    es->next = malloc(sizeof(Exprs));
    assert(es->next);
    memset(es->next, 0, sizeof(Exprs));
    
  } else if(es->len >= BATCH_SIZE) {    
    return exprs_append(es->next);
  }

  return &es->data[es->len++];
}

Expr *exprs_append_value(Exprs *es, s64 value) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_VALUE;
  e->as.sval = value;

  return e;
}

Expr *exprs_append_cast(Exprs *es, Expr *expr, Type to_type) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_CAST;
  e->as.cast = (Expr_Cast) { expr, to_type };

  return e;
}

Expr *exprs_append_variable(Exprs *es, string name) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_VARIABLE;
  e->as.strings.fst = name;

  return e;
 
}

Expr *exprs_append_constant(Exprs *es, u64 index) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_CONSTANT;
  e->as.sval = (s64) index;

  return e;
}

Expr *exprs_append_pointer(Exprs *es, string name) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_VARIABLE_PTR;
  e->as.strings.fst = name;
  
  return e;
}

Expr *exprs_append_sum(Exprs *es, Expr *lhs, Expr *rhs) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_SUM;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

Expr *exprs_append_sub(Exprs *es, Expr *lhs, Expr *rhs) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_SUBTRACTION;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

#define exprs_append_funccall(exprs, name, ...)				\
  exprs_append_funccall_impl((exprs), (name), __VA_ARGS__, NULL)

Expr* exprs_append_funccall_impl(Exprs *es, string name, ...) {

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

  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_FUNCCALL;
  e->as.funccall = funccall;
  
  return e;
}

typedef struct{
  string name;
  Type type;
  s64 off;
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

typedef enum{
  STMT_TYPE_NONE = 0,
  STMT_TYPE_FUNCCALL,
  STMT_TYPE_DECLARATION,
  STMT_TYPE_ASSIGNMENT,
  STMT_TYPE_IF,
  STMT_TYPE_RETURN,
}Stmt_Type;

typedef struct{
  string name;
  Type type;
}Stmt_Declaration;

typedef struct{
  Expr *lhs;
  Expr *rhs;
}Stmt_Assignment;

typedef struct Stmts Stmts;

typedef enum {
  STMT_IF_TYPE_NONE = 0,
  STMT_IF_TYPE_EQUALS,
}Stmt_If_Type;

typedef struct{
  Expr *lhs;
  Stmt_If_Type op;
  Expr *rhs;
  Stmts *body;
}Stmt_If;

typedef struct{
  Stmt_Type type;
  union{
    Expr *expr;
    Expr_Funccall funccall;
    Stmt_Declaration declaration;
    Stmt_Assignment assignment;
    Stmt_If iff;    
  }as;
}Stmt;

struct Stmts{
  Stmt *data;
  u64 len;
  u64 cap;
};

void stmts_append_declaration(Stmts *ss, string name, Type type) {

  Stmt_Declaration declaration = { .name = name, .type = type };
  
  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_DECLARATION,
	.as.declaration = declaration,
      }));  
}

void stmts_append_assignment(Stmts *ss, Expr *lhs, Expr *rhs) {

  Stmt_Assignment assignment = { .lhs = lhs, .rhs = rhs };
  
  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_ASSIGNMENT,
	.as.assignment = assignment,
      }));    
}

void stmts_append_if(Stmts *ss, Expr *lhs, Stmt_If_Type op, Expr *rhs, Stmts *body) {

  Stmt_If iff = { .lhs = lhs, .op = op, .rhs = rhs , .body = body };

  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_IF,
	.as.iff = iff,
      }));    

}

void stmts_append_return(Stmts *ss, Expr *expr) {
  
  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_RETURN,
	.as.expr = expr,
      }));
  
}
  
#define stmts_append_funccall(exprs, name, ...)				\
  stmts_append_funccall_impl((exprs), (name), __VA_ARGS__, NULL)

void stmts_append_funccall_impl(Stmts *ss, string name, ...) {

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

  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_FUNCCALL,
	.as.funccall = funccall
      }));
}

typedef struct Stmtss Stmtss;
struct Stmtss{
  Stmts *data;
  u64 len;  
  Stmtss *next;
};

Stmts *stmtss_append(Stmtss *sss) {
  
  if(sss->len == 0) {
    sss->data = malloc(sizeof(Stmts) * BATCH_SIZE);
    assert(sss->data);
    
    sss->next = malloc(sizeof(Stmtss));
    assert(sss->next);
    memset(sss->next, 0, sizeof(Stmtss));
    
  } else if(sss->len >= BATCH_SIZE) {    
    return stmtss_append(sss->next);
  }

  return &sss->data[sss->len++];
}

typedef struct{
  Type return_type;
  string name;
  Params params;

  boolean external;

  Stmts stmts;
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

Type expr_to_type(Expr *e, Vars *vars, Functions *fs, Constants *cs);

bool type_check(Expr *lhs, Expr *rhs, Type *type,
		Vars *vs, Functions *fs, Constants *cs) {

  Type lhs_type = expr_to_type(lhs, vs, fs, cs);
  Type rhs_type = expr_to_type(rhs, vs, fs, cs);
  
  bool okay = type_equal(lhs_type, rhs_type);

  if(okay) {
    if(type) *type = rhs_type;
    return true;
  } else {
    printf(type_fmt", "type_fmt, type_arg(lhs_type), type_arg(rhs_type));
    return false;
  }
  
}

Type expr_to_type(Expr *e, Vars *vs, Functions *fs, Constants *cs) {
  
  switch(e->type) {

  case EXPR_TYPE_CAST:
    return e->as.cast.to_type;

  case EXPR_TYPE_VALUE:
    return S64;

  case EXPR_TYPE_VARIABLE_DEREF: {

    string name = e->as.strings.fst;

    Var *var = vars_find(vs, name);
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

    Var *var = vars_find(vs, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    return type_ptr(var->type);
    
  } break;

    /*
      case EXPR_TYPE_STRUCT_FIELD: {

      string name = e->as.strings.fst;
      string field_name = e->as.strings.snd;

      Var *var = vars_find(&p->vs, name);
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
    */

  case EXPR_TYPE_VARIABLE: {

    string name = e->as.strings.fst;

    Var *var = vars_find(vs, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    return var->type;
    
  } break;

  case EXPR_TYPE_SUM: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type, vs, fs, cs)) {
      panic("Can not add expressions with different types");
    }

    return type;
    
  } break;    
    
  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type, vs, fs, cs)) {
      panic("Can not subtract expressions with different types");
    }

    return type;
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    string name = e->as.funccall.name;
      
    Function *f = functions_find(fs, name);
    if(f == NULL) {
      panic("Can not find function with the name: '"str_fmt"'\n", str_arg(name));
    }

    return f->return_type;
    
  } break;

  case EXPR_TYPE_CONSTANT: {
    Constant *c = &cs->data[e->as.sval];

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

Type funccall_compile(Expr_Funccall *funccall, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs);

Value expr_location(Expr *e, Type *out_type,
		    Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs) {
  
  string name = e->as.strings.fst;
  string field_name = e->as.strings.snd;
  
  Var *var = vars_find(vs, name);
  if(!var) {
    panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
  }  

  Value result;
  switch(e->type) {

  case EXPR_TYPE_VARIABLE: {
    result = RSP_OFF((s64) (*stack_ptr) - var->off);
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {
    result = RSP_OFF((s64) (*stack_ptr) - var->off);
  } break;

  /* case EXPR_TYPE_STRUCT_FIELD: { */
    
  /*   if(var->type.type != TYPE_STRUCT) { */
  /*     panic("variable with the name: \""str_fmt"\" is not a structure\n", str_arg(name)); */
  /*   } */
    
  /*   result = RSP_OFF((*stack_ptr) - var->off + structure_off(var, field_name, NULL)); */
  /* } break; */

  case EXPR_TYPE_VARIABLE_DEREF: {
    // TODO: deref var->type and update 'type'
    
    // TODO: Maybe check that the pointer is actually 8 bytes long
    Size size = SIZE_QWORD;
    
    da_append(is, MOV(RAX, RSP_OFF((s64) (*stack_ptr) - var->off), size));
    result = RAX_OFF(0);
  } break;
    
  default:
    panic("unimplemented expr_type: %d", e->type);
  }

  if(out_type) {
    *out_type = expr_to_type(e, vs, fs, cs);
  }

  return result;

}

void expr_compile(Expr *e, Value location, Size cast_size,
		  Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs);

void expr_compile_smart(Expr *lhs, Value lhs_loc,
			Expr *rhs, Value rhs_loc,
			Size size,
			Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs) {
  
  bool is_static = expr_is_static(rhs);
  
  if(is_static) {
    expr_compile(lhs, lhs_loc, size,
		 is, stack_ptr, vs, fs, cs);
    expr_compile(rhs, rhs_loc, size,
		 is, stack_ptr, vs, fs, cs);
  } else {
    u64 size_bytes = size_in_bytes(size);
    
    da_append(is, SUB(RSP, LITERAL(size_bytes), SIZE_QWORD));
    (*stack_ptr) += size_bytes;
    expr_compile(lhs, RSP_OFF(0), size,
		 is, stack_ptr, vs, fs, cs);
    
    expr_compile(rhs, rhs_loc, size,
		 is, stack_ptr, vs, fs, cs);
    
    da_append(is, MOV(lhs_loc, RSP_OFF(0), size));
    da_append(is, ADD(RSP, LITERAL(size_bytes), SIZE_WORD));
    (*stack_ptr) -= size_bytes;
    
  }

}

void expr_compile(Expr *e, Value location, Size cast_size,
		  Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs) {

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

    /* if(location.type == VALUE_TYPE_REGISTER_OFF) { */
    /*   if(size != SIZE_QWORD) { */
    /* 	da_append(is, MOV(location, LITERAL(value), size)); */
    /*   } else { */
    /* 	da_append(is, MOV(temp, LITERAL(value), size)); */
    /* 	da_append(is, MOV(location, temp, size)); */

    /*   } */
      
    /* } else { */
    /*   if(location.type == VALUE_TYPE_REGISTER) { */
    /* 	da_append(is, MOV(location, LITERAL(value), size)); */
    /*   } else { */
    /* 	da_append(is, MOV(temp, LITERAL(value), size)); */
    /* 	da_append(is, MOV(location, temp, size)); */
    /*   } */

    /* } */

     if(location.type == VALUE_TYPE_REGISTER) {
      da_append(is, MOV(location, LITERAL(value), size));
    } else {
      da_append(is, MOV(temp, LITERAL(value), size));
      da_append(is, MOV(location, temp, size));
    }
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    Type type = funccall_compile(&e->as.funccall,
				 is, stack_ptr, vs, fs, cs);

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }
    
    da_append(is, MOV(location, RAX, size));
    
  } break;

  case EXPR_TYPE_CONSTANT: {

    Constant *c = &cs->data[e->as.sval];

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
      da_append(is, MOV(location, CONSTANT(e->as.sval), value_size));
    } else {
      da_append(is, MOV(temp, CONSTANT(e->as.sval), value_size));
      da_append(is, MOV(location, temp, value_size));
    }
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD:
  case EXPR_TYPE_VARIABLE: {

    Type variable_type;
    Value variable_location = expr_location(e, &variable_type,
					    is, stack_ptr, vs, fs, cs);
    Size variable_size;
    if(cast_size == SIZE_NONE) {
      variable_size = type_size(variable_type);
    } else {
      variable_size = cast_size;
    }

    if(location.type == VALUE_TYPE_REGISTER_OFF) {

      da_append(is, MOV(temp, variable_location, variable_size));
      da_append(is, MOV(location, temp, variable_size));
      
    } else {
      da_append(is, MOV(location, variable_location, variable_size));
    }
    
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {

    Type variable_type;
    Value variable_location = expr_location(e, &variable_type,
					    is, stack_ptr, vs, fs, cs);
        
    Size variable_ptr_size;
    if(cast_size == SIZE_NONE) {
      Type variable_ptr_type = type_ptr(variable_type);
      variable_ptr_size = type_size(variable_ptr_type);
    } else {
      variable_ptr_size = cast_size;
    }

    if(location.type == VALUE_TYPE_REGISTER) {
      da_append(is, LEA(location, variable_location));
    } else {
      da_append(is, LEA(temp, variable_location));
      da_append(is, MOV(location, temp, variable_ptr_size));
    }    
    
  } break;

  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs)) {
      panic("Can not subtract expressions with different types");
    }
    
    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    expr_compile_smart(lhs, RAX,
		       rhs, RBX,
		       type_size(type),
		       is, stack_ptr, vs, fs, cs);
    
    da_append(is, SUB(RAX, RBX, size));
    da_append(is, MOV(location, RAX, size));
    
  } break;

  case EXPR_TYPE_CAST: {

    Type type = expr_to_type(e->as.cast.expr, vs, fs, cs);
    Size size = type_size(type);

    Type to_cast_type = e->as.cast.to_type;
    Size to_cast_size = type_size(to_cast_type);

    if(size == to_cast_size) {
      expr_compile(e->as.cast.expr, location, to_cast_size,
		   is, stack_ptr, vs, fs, cs);
    } else if(size > to_cast_size) {
      expr_compile(e->as.cast.expr, location, to_cast_size,
		   is, stack_ptr, vs, fs, cs);
    } else  { // size < to_cast_size
      da_append(is, MOV(temp, LITERAL(0), SIZE_QWORD));
      expr_compile(e->as.cast.expr, temp, size,
		   is, stack_ptr, vs, fs, cs);
      da_append(is, MOV(location, temp, to_cast_size));
    }        
    
  } break;

  case EXPR_TYPE_SUM: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;
    
    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs)) {
      panic("Can not add expressions with different types");
    }

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    expr_compile_smart(lhs, RAX,
		       rhs, RBX,
		       type_size(type),
		       is, stack_ptr, vs, fs, cs);
    
    da_append(is, ADD(RAX, RBX, size));
    da_append(is, MOV(location, RAX, size));
    
  } break;
    
  default:
    panic("Unimplemented expr->type");
    
  }

  
}

Type funccall_compile(Expr_Funccall *funccall, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs) {

  Function *f = functions_find(fs, funccall->name);
  if(f == NULL) {
    panic("Can not find function with the name: '"str_fmt"'\n", str_arg(funccall->name));
  }

  if(f->params.len != funccall->args_len) {
    panic("Function: '"str_fmt"'. Number of parameters: %llu and number of arguments: %llu does not match",
	  str_arg(f->name), f->params.len, funccall->args_len);
  }

  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *arg = funccall->args[i];
    Type arg_type = expr_to_type(arg, vs, fs, cs);

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

  u64 stack_ptr_before = *stack_ptr;

  s64 shadow_space = funccall->args_len * 8;
  if(shadow_space < 32) {
    shadow_space = 32;
  }
  s64 alignment = ((*stack_ptr) + shadow_space) % 16;
  if(alignment != 0) {    
    shadow_space += 16 - alignment;
  }
  *stack_ptr += shadow_space;
  da_append(is, SUB(RSP, LITERAL(shadow_space), SIZE_QWORD));

  // calculate argument-expressions and save them in the shadowspace
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];

    if(expr_is_static(e)) {
      // pass
    } else {
      expr_compile(e, RSP_OFF((*stack_ptr) - stack_ptr_before - shadow_space + i  * 8), SIZE_NONE,
		   is, stack_ptr, vs, fs, cs);
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
    s64 target = (s64) (*stack_ptr) - (s64) stack_ptr_before - (s64) shadow_space + i  * 8;
    //s64 target = i  * 8;

    Param *param = &f->params.data[i];
    Type param_type = param->type;
    Size param_size = type_size(param_type);

    if(i < FASTCALL_REGISTER_COUNT) { // rcx, rdx, r8, r9

      if(expr_is_static(e)) {
	expr_compile(e, REGISTER(FASTCALL_REGISTERS[i]), SIZE_NONE,
		     is, stack_ptr, vs, fs, cs);
      } else {
	da_append(is, MOV(REGISTER(FASTCALL_REGISTERS[i]), RSP_OFF(target), param_size));
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
	expr_compile(e, RAX, SIZE_NONE,
		     is, stack_ptr, vs, fs, cs);
	da_append(is, MOV(RSP_OFF(target), RAX, param_size));
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
  
  da_append(is, CALL(WORD(funccall->name)));

  da_append(is, ADD(RSP, LITERAL((*stack_ptr) - stack_ptr_before), SIZE_QWORD));
  *stack_ptr = stack_ptr_before;

  return f->return_type;
}

// Return true, if stmt returns and write output into 'out_type'
bool stmt_compile(Type return_type,
		  Stmt *s,
		  Instrs *is,
		  u64 *stack_ptr,
		  u64 *label_count,
		  Vars *vs,		  
		  Functions *fs,
		  Constants *cs,
		  Type *out_type) {

  switch(s->type) {

  case STMT_TYPE_FUNCCALL: {

    // TODO: add warning of unused return_type
    funccall_compile(&s->as.funccall, is, stack_ptr, vs, fs, cs);
    
  } break;

  case STMT_TYPE_DECLARATION: {

    Stmt_Declaration *declaration = &s->as.declaration;
    
    u64 n = size_in_bytes(type_size(declaration->type));

    da_append(is, SUB(RSP, LITERAL(n), SIZE_QWORD));
    *stack_ptr += n;
  
    da_append(vs, ((Var) {
	  .name = declaration->name,
	  .type = declaration->type,
	  .off  = (s64) (*stack_ptr),
	}));
    
  } break;

  case STMT_TYPE_ASSIGNMENT: {

    Expr *lhs = s->as.assignment.lhs;
    Expr *rhs = s->as.assignment.rhs;

    if(!type_check(lhs, rhs, NULL,
		   vs, fs, cs)) {
      panic("Can not assign expressions with different type");
    }

    Value location = expr_location(lhs, NULL,
				   is, stack_ptr, vs, fs, cs);
    expr_compile(rhs, location, SIZE_NONE,
		 is, stack_ptr, vs, fs, cs);
    
  } break;

  case STMT_TYPE_IF: {

    Expr *lhs = s->as.iff.lhs;
    Stmt_If_Type op = s->as.iff.op;
    Expr *rhs = s->as.iff.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs)) {
      panic("Can compare expressions with different types"); 
    }
    Size size = type_size(type);
      
    expr_compile_smart(lhs, RAX,
		       rhs, RBX,
		       size,
		       is, stack_ptr, vs, fs, cs);  
    da_append(is, CMP(RAX, RBX, size));

    u64 stack = *stack_ptr;
    u64 vars_len = vs->len;
    u64 label = *label_count;
    (*label_count) = (*label_count) + 1;

    switch(op) {
      
    case STMT_IF_TYPE_EQUALS: {
      da_append(is, JNE(LITERAL(label)));
    } break;

    default:
      panic("Unimplemented stmt_if_type");
      
    }    

    Stmts *stmts = s->as.iff.body;
    bool returned = false;
    for(u64 i=0;i<stmts->len;i++) {

      if(returned) {
	panic("Found dead code in if");
      }
      
      if(stmt_compile(return_type,
		      &stmts->data[i],
		      is,
		      stack_ptr,
		      label_count,
		      vs,		  
		      fs,
		      cs,
		      out_type)) {
        returned = true;
      }
    }

    vs->len = vars_len;

    if(!returned && (*stack_ptr) != stack) {
      da_append(is, ADD(RSP, LITERAL((*stack_ptr) - stack), SIZE_QWORD));      
    }
    (*stack_ptr) = stack;
    da_append(is, LABEL(LITERAL(label)));

    // TODO: figure this logic out
    
    /* if(do_return) { */
    /*   return true; */
    /* } */
    
  } break;

  case STMT_TYPE_RETURN: {

    Expr *expr = s->as.expr;
    Type type = expr_to_type(expr, vs, fs, cs);

    if(!type_equal(type, return_type)) {
      printf(type_fmt", "type_fmt, type_arg(type), type_arg(return_type));
      panic("Can not return with a different type");
    }

    if(type.ptr_degree > 0) {
      expr_compile(expr, RAX, SIZE_NONE,
		   is, stack_ptr, vs, fs, cs);
    } else if(type.type == TYPE_STRUCT) {
      panic("todo");
    } else if(type.type == TYPE_VOID) {
      // pass
    } else {
      expr_compile(expr, RAX, SIZE_NONE,
		   is, stack_ptr, vs, fs, cs);
    }    

    if((*stack_ptr) > 0) {
      da_append(is, ADD(RSP, LITERAL((*stack_ptr)), SIZE_QWORD));
      *stack_ptr = 0;
    }
    da_append(is, RET);

    *out_type = type;
    return true;
  
  } break;
    
  default:
    panic("unimplemented stmt_type");
  }

  return false;
}

typedef struct{
  Exprs exprs;
  Stmtss stmtss;
  Functions functions;
  Constants constants;
}Program;

void program_append(Program *p, string_builder *sb) {

  Vars vars = {0};
  Instrs instrs = {0};

  u64 implementations = 0;

  // constants
  if(p->constants.len > 0) {
    string_builder_appendf(sb, "        section .data\n");
  }
  for(u64 i=0;i<p->constants.len;i++) {
    constant_append(&p->constants.data[i], i, sb);
  }

  // declarations
  for(u64 i=0;i<p->functions.len;i++) {
    Function *f = &p->functions.data[i];
    if(f->external) {
      string_builder_appendf(sb, "        extern "str_fmt"\n", str_arg(f->name));
    } else {
      string_builder_appendf(sb, "        global "str_fmt"\n", str_arg(f->name));
      implementations++;
    }
    
  }

  // implementations
  if(implementations > 0) {
    string_builder_appendf(sb, "        section .text\n");
  }
  for(u64 i=0;i<p->functions.len;i++) {
    Function *f = &p->functions.data[i];
    if(f->external) {
      continue;
    }    
    string_builder_appendf(sb, ""str_fmt":\n", str_arg(f->name));

    u64 stack_ptr = 0;
    u64 label_count = 0;

    for(u64 j=0;j<f->params.len && j<FASTCALL_REGISTER_COUNT;j++) {
      Param *param = &f->params.data[j];

      s64 off = (j + 1) * 8;
      da_append(&instrs, MOV(RSP_OFF(off), REGISTER(FASTCALL_REGISTERS[j]), SIZE_QWORD));
      da_append(&vars, ((Var) {
	    .name = param->name,
	    .type = param->type,
	    .off  = -off,
	  }));

    }

    boolean needs_to_return = !type_equal(f->return_type, _VOID);
    boolean returned = false;
    for(u64 j=0;j<f->stmts.len;j++) {

      if(returned) {
	panic("Remove dead code in function: '"str_fmt"'\n", str_arg(f->name));
      }
      
      Type compiled_return_type;
      bool stmt_returns = stmt_compile(f->return_type,
				       &f->stmts.data[j],
				       &instrs,
				       &stack_ptr,
				       &label_count,
				       &vars,
				       &p->functions,
				       &p->constants,
				       &compiled_return_type);
      if(stmt_returns) {
	returned = true;
      }
      
    }
    if(needs_to_return && !returned) {
      panic("Not all paths in function: '"str_fmt"' return", str_arg(f->name));
    }
    
    vars.len = 0;
    if(type_equal(f->return_type, _VOID)) {
      if(stack_ptr > 0) {
	da_append(&instrs, ADD(RSP, LITERAL(stack_ptr), SIZE_QWORD));	   
      }
      da_append(&instrs, RET);
    }

    for(u64 j=0;j<instrs.len;j++) {
      instr_append(&instrs.data[j], sb);
    }
    instrs.len = 0;
    
  }
  
}

void appendFunctionsWinApi(Program *p) {
  
  Function exitProcess = {0};
  exitProcess.external = true;
  exitProcess.return_type = _VOID;
  exitProcess.name = string_from_cstr("ExitProcess");
  da_append(&exitProcess.params, ((Param) {
	.type = U8,
	.name = string_from_cstr("exitCode"),
      }));  
  da_append(&p->functions, exitProcess);

  Function createFileA = {0};
  createFileA.external = true;
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
  da_append(&p->functions, createFileA);

  Function getLastError = {0};
  getLastError.external = true;
  getLastError.return_type = U32;
  getLastError.name = string_from_cstr("GetLastError");
  da_append(&p->functions, getLastError);

  Function getProcessHeap = {0};
  getProcessHeap.external = true;
  getProcessHeap.return_type  = PTR(TYPE_U64);
  getProcessHeap.name = string_from_cstr("GetProcessHeap");
  da_append(&p->functions, getProcessHeap);

  Function getFileSize = {0};
  getFileSize.external = true;
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
  da_append(&p->functions, getFileSize);

  Function heapAlloc = {0};
  heapAlloc.external = true;
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
  da_append(&p->functions, heapAlloc);

  Function readFile = {0};
  readFile.external = true;
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
  da_append(&p->functions, readFile);

  Function writeFile = {0};
  writeFile.external = true;
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
  da_append(&p->functions, writeFile);

  Function getStdHandle = {0};
  getStdHandle.external = true;
  getStdHandle.return_type = PTR(TYPE_U64);
  getStdHandle.name = string_from_cstr("GetStdHandle");
  da_append(&getStdHandle.params, ((Param){
	.type = U32,
	.name = string_from_cstr("stdHandle")
      }));
  da_append(&p->functions, getStdHandle);

  Function closeHandle = {0};
  closeHandle.external = true;
  closeHandle.return_type = U8;
  closeHandle.name = string_from_cstr("CloseHandle");
  da_append(&closeHandle.params, ((Param) {
	.type = PTR(TYPE_U64),
	.name = string_from_cstr("object"),
      }));
  da_append(&p->functions, closeHandle);

  Function heapFree = {0};
  heapFree.external = true;
  heapFree.return_type = U8;
  heapFree.name = string_from_cstr("HeapFree");
  da_append(&heapFree.params, ((Param) {
	.type = PTR(TYPE_U64),
	.name = string_from_cstr("heap")
      }));
  da_append(&heapFree.params, ((Param) {
	.type = U32,
	.name = string_from_cstr("flags")
      }));
  da_append(&heapFree.params, ((Param) {
	.type = PTR(TYPE_U8),
	.name = string_from_cstr("mem")
      }));
  da_append(&p->functions, heapFree);
}

void appendFunctionSub(Program *p) {
  
  Function f = {0};
  f.name = string_from_cstr("sub");
  f.return_type = U8;
  da_append(&f.params, ((Param) {
	.type = U8,
	.name = string_from_cstr("a")
      }));
  da_append(&f.params, ((Param) {
	.type = U8,
	.name = string_from_cstr("b")
      }));
  
  Exprs *es = &p->exprs;
  Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;

  // return a - b;
  stmts_append_return(ss,
		      exprs_append_sub(es,
				       exprs_append_variable(es, string_from_cstr("a")),
				       exprs_append_variable(es, string_from_cstr("b"))));

  da_append(&p->functions, f);

}

void appendFunctionAdd(Program *p) {
  
  Function f = {0};
  f.name = string_from_cstr("add");
  f.return_type = U8;
  da_append(&f.params, ((Param) {
	.type = U8,
	.name = string_from_cstr("a")
      }));
  da_append(&f.params, ((Param) {
	.type = U8,
	.name = string_from_cstr("b")
      }));
  
  Exprs *es = &p->exprs;
  Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;

  // return a + b;
  stmts_append_return(ss,
		      exprs_append_sum(es,
				       exprs_append_variable(es, string_from_cstr("a")),
				       exprs_append_variable(es, string_from_cstr("b"))));

  da_append(&p->functions, f);

}

void appendFunctionFoo(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("foo");
  f.return_type = U8;
  
  Exprs *es = &p->exprs;
  Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;

  // return 35;
  stmts_append_return(ss, exprs_append_cast(es, exprs_append_value(es, 35), U8));

  da_append(&p->functions, f);

}

void appendFunctionMain(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("main");
  f.return_type = _VOID;
  
  Exprs *es = &p->exprs;
  Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;

  // if(dump() == 0) {
  //     Exit(GetLastError());
  // }
  Stmts *stmts = stmtss_append(sss);
  memset(stmts, 0, sizeof(Stmts));
  stmts_append_funccall(stmts,
			string_from_cstr("ExitProcess"),
			exprs_append_cast(es,
					  exprs_append_funccall(es,
								string_from_cstr("GetLastError")),
					  U8));
  stmts_append_if(ss,
		  exprs_append_funccall(es,
					string_from_cstr("dump")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_cast(es, exprs_append_value(es, 0), U8),
		  stmts);  

  // Exitprocess(foo());
  stmts_append_funccall(ss,
			string_from_cstr("ExitProcess"),
			exprs_append_funccall(es,
					      string_from_cstr("sub"),
					      exprs_append_funccall(es, string_from_cstr("foo")),
					      exprs_append_cast(es, exprs_append_value(es, 34), U8)
					      )
			);

  
  da_append(&p->functions, f);
}

void appendFunctionDump(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("dump");
  f.return_type = U8;
  
  Exprs *es = &p->exprs;
  Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;  

  // handle : u64* = CreateFileA("main.asm", (u32) GENERIC_READ, (u32) FILE_SHARE_READ, (u64*) 0, (u32) OPEN_EXISTING, (u32) FILE_ATTRIBUTE_NORMAL, (u64*) NULL);
  stmts_append_declaration(ss, string_from_cstr("handle"), PTR(TYPE_U64));
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("handle")),
			  exprs_append_funccall(es,
						string_from_cstr("CreateFileA"),
						exprs_append_constant(es, constants_append_cstr(&p->constants, "main.fe")),
						exprs_append_cast(es, exprs_append_value(es, GENERIC_READ), U32),
						exprs_append_cast(es, exprs_append_value(es, FILE_SHARE_READ), U32),
						exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)),
						exprs_append_cast(es, exprs_append_value(es, OPEN_EXISTING), U32),
						exprs_append_cast(es, exprs_append_value(es, FILE_ATTRIBUTE_NORMAL), U32),
						exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64))
						)
			  );

  // if(handle == INVALID_HANDLE_VALUE) {
  //     ExitProcess(GetLastError());
  // }
  Stmts *error_body = stmtss_append(sss);
  memset(error_body, 0, sizeof(Stmts));
  {
    stmts_append_return(error_body, exprs_append_cast(es, exprs_append_value(es, 0), U8));
  }
  stmts_append_if(ss,
		  exprs_append_variable(es, string_from_cstr("handle")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_cast(es, exprs_append_value(es, (s64) INVALID_HANDLE_VALUE), PTR(TYPE_U64)),
		  error_body);

  // process_heap : u64* = GetProcessHeap
  stmts_append_declaration(ss, string_from_cstr("process_heap"), PTR(TYPE_U64));
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("process_heap")),
			  exprs_append_funccall(es, string_from_cstr("GetProcessHeap"))
			  );
  // if(process_heap == 0) {
  //     ExitProcess(GetLastError());
  // }
  stmts_append_if(ss,
		  exprs_append_variable(es, string_from_cstr("process_heap")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)),
		  error_body);

  // size : u32 = GetFileSize(handle);
  stmts_append_declaration(ss, string_from_cstr("size"), U32);
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("size")),
			  exprs_append_funccall(es,
						string_from_cstr("GetFileSize"),
						exprs_append_variable(es,
								      string_from_cstr("handle")),
						exprs_append_cast(es,
								  exprs_append_value(es, 0),
								  PTR(TYPE_U32)))
			  );  
  // if(size == INVALID_FILE_SIZE) {
  //     ExitProcess(GetLastError());
  // }
  stmts_append_if(ss,
		  exprs_append_variable(es, string_from_cstr("size")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_cast(es, exprs_append_value(es, INVALID_FILE_SIZE), U32),
		  error_body);

  // space : u8* = HeapAlloc(process_heap, (u32) 0, (u64) size);
  stmts_append_declaration(ss, string_from_cstr("space"), PTR(TYPE_U8));
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("space")),
			  exprs_append_funccall(es,
						string_from_cstr("HeapAlloc"),
					        exprs_append_variable(es, string_from_cstr("process_heap")),
						exprs_append_cast(es, exprs_append_value(es, 0), U32),
						exprs_append_cast(es, exprs_append_variable(es, string_from_cstr("size")), U64))
			  );
  // if(space == 0) {
  //     ExitProcess(GetLastError());
  // }
  stmts_append_if(ss,
		  exprs_append_variable(es, string_from_cstr("space")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U8)),
		  error_body);

  // written : u32;
  stmts_append_declaration(ss, string_from_cstr("written"), U32);  

  // ReadFile(handle, space, size, &written, (u64*) 0);
  stmts_append_funccall(ss,
			string_from_cstr("ReadFile"),
			exprs_append_variable(es, string_from_cstr("handle")),
			exprs_append_variable(es, string_from_cstr("space")),
			exprs_append_variable(es, string_from_cstr("size")),
			exprs_append_pointer(es, string_from_cstr("written")),
			exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)));

  // WriteFile(GetStdHandle((u32) -11), space, size, &written, (u64) 0);
  stmts_append_funccall(ss,
			string_from_cstr("WriteFile"),
		        exprs_append_funccall(es,
					      string_from_cstr("GetStdHandle"),
					      exprs_append_cast(es, exprs_append_value(es, -11), U32)),
			exprs_append_variable(es, string_from_cstr("space")),
			exprs_append_variable(es, string_from_cstr("size")),
			exprs_append_pointer(es, string_from_cstr("written")),
			exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)));

  // HeapFree(process_heap, (u32) 0, space);
  stmts_append_funccall(ss,
			string_from_cstr("HeapFree"),
			exprs_append_variable(es, string_from_cstr("process_heap")),
		        exprs_append_cast(es, exprs_append_value(es, 0), U32),
			exprs_append_variable(es, string_from_cstr("space")));

  // CloseHandle(handle);
  stmts_append_funccall(ss,
			string_from_cstr("CloseHandle"),
			exprs_append_variable(es, string_from_cstr("handle")));
  
  // return 1;
  stmts_append_return(ss, exprs_append_cast(es, exprs_append_value(es, 1), U8));

  da_append(&p->functions, f);
  
}

int main() {

  Program program = {0};
  appendFunctionsWinApi(&program);
  appendFunctionDump(&program);
  appendFunctionFoo(&program);
  appendFunctionAdd(&program);
  appendFunctionMain(&program);
  appendFunctionSub(&program);
  
  string_builder sb = {0};  
  program_append(&program, &sb);
  printf("%.*s", (int) sb.len, sb.data);

  if(!io_write_file("main.asm", (u8 *) sb.data, sb.len)) {
    return 1;
  }
  
  return 0;
}
