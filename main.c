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

// https://en.wikibooks.org/wiki/X86_Assembly/X86_Architecture
// https://en.wikipedia.org/wiki/X86_instruction_listings

char *REGISTER_NAMES[][5] = {
//  8-lo    8-hi  16-lo   32-lo   64-lo
  { "dil",  NULL, "di",   "edi",  "rdi" },  
  { "bl",   "bh", "bx",   "ebx",  "rbx" },
  { "sil",  NULL, "si",   "esi",  "rsi" },
  { "r10b", NULL, "r10w", "r10d", "r10" },
  { "r11b", NULL, "r11w", "r11d", "r11" },
  { "r12b", NULL, "r12w", "r12d", "r12" },
  { "r13b", NULL, "r13w", "r13d", "r13" },
  { "r14b", NULL, "r14w", "r14d", "r14" },
  { "r15b", NULL, "r15w", "r15d", "r15" },

  { "cl",   "ch", "cx",   "ecx",  "rcx" },
  { "r8b",  NULL, "r8w",  "r8d",  "r8" },
  { "r9b",  NULL, "r9w",  "r9d",  "r9" },

  { "al",   "ah",  "ax",  "eax",  "rax" },
  { "dl",   "dh",  "dx",  "edx",  "rdx" },
  { "spl",  NULL,  "sp",  "esp",  "rsp" },
  { "bpl",  NULL,  "bp", "ebp",    "rbp" },
};

u64 REGISTER_NAMES_COUNT =
  sizeof(REGISTER_NAMES)/sizeof(REGISTER_NAMES[0]);

u64 FASTCALL_REGISTERS[] = {
  9, // rcx
  13, // rdx
  10, // r8
  11  // r9
};
u64 FASTCALL_REGISTER_COUNT =
  sizeof(FASTCALL_REGISTERS)/sizeof(FASTCALL_REGISTERS[0]);

typedef struct{
  bool used[sizeof(REGISTER_NAMES)/sizeof(REGISTER_NAMES[0]) - 4];
}Registers;

s32 registers_lock(Registers *rs) {
  for(u64 i=0;i<REGISTER_NAMES_COUNT;i++) {
    if(!rs->used[i]) {
      rs->used[i] = true;
      return (s32) i;
    }
  }

  panic("todo");
  //return -1;
}

void registers_release(Registers *rs, s32 index) {
  rs->used[index] = false;
}

#define REGISTER(n) ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (n) })
#define REGISTER_OFF(n, o) ((Value) { .type = VALUE_TYPE_REGISTER_OFF, .as.sval = (n), .off = (o) })
#define _RAX ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (12) })
#define _RCX ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (9) })
#define _RDX ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (13) })
#define _RSP ((Value) { .type = VALUE_TYPE_REGISTER, .as.sval = (14) })
#define _RSP_OFF(n) ((Value) { .type = VALUE_TYPE_REGISTER_OFF, .as.sval = (14), .off = (n) })

#define LITERAL(n) ((Value) { .type = VALUE_TYPE_LITERAL, .as.sval = (n) })
#define WORD(s) ((Value) { .type = VALUE_TYPE_WORD, .as.stringval = (s) })
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
  SIZE_BYTE_HI,
  SIZE_WORD,       // 16 bits 2 bytes
  SIZE_DWORD,      // 32 bits 4 bytes
  SIZE_QWORD,      // 64 bits 8 bytes
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
  case SIZE_BYTE_HI:
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
  INSTR_TYPE_CDQ,
  INSTR_TYPE_DIV,
  INSTR_TYPE_IDIV,
  INSTR_TYPE_IMUL,
  INSTR_TYPE_SHL,
}Instr_Type;

typedef struct{
  Instr_Type type;
  Value lhs;
  Value rhs;
  Size size;
}Instr;

#define MOV(l, r, s) ((Instr) { .type = INSTR_TYPE_MOV, .lhs = (l), .rhs = (r), .size = (s)})
#define LEA(l, r) ((Instr) { .type = INSTR_TYPE_LEA, .lhs = (l), .rhs = (r), .size = (4) })
#define AND(l, r, s) ((Instr) { .type = INSTR_TYPE_AND, .lhs = (l), .rhs = (r), .size = (s)})
#define SUB(l, r, s) ((Instr) { .type = INSTR_TYPE_SUB, .lhs = (l), .rhs = (r), .size = (s)})
#define ADD(l, r, s) ((Instr) { .type = INSTR_TYPE_ADD, .lhs = (l), .rhs = (r), .size = (s)})
#define CMP(l, r, s) ((Instr) { .type = INSTR_TYPE_CMP, .lhs = (l), .rhs = (r), .size = (s)})
#define SHL(l, r, s) ((Instr) { .type = INSTR_TYPE_SHL, .lhs = (l), .rhs = (r), .size = (s)})
#define CALL(l) ((Instr) { .type = INSTR_TYPE_CALL, .lhs = (l), .size = (4) })
#define JNE(l) ((Instr) { .type = INSTR_TYPE_JNE, .lhs = (l), .size = (4) })
#define JGE(l) ((Instr) { .type = INSTR_TYPE_JGE, .lhs = (l), .size = (4)  })
#define JLE(l) ((Instr) { .type = INSTR_TYPE_JLE, .lhs = (l), .size = (4)  })
#define JL(l) ((Instr) { .type = INSTR_TYPE_JL, .lhs = (l), .size = (4)  })
#define JE(l) ((Instr) { .type = INSTR_TYPE_JE, .lhs = (l), .size = (4)  })
#define JMP(l) ((Instr) { .type = INSTR_TYPE_JMP, .lhs = (l), .size = (4)  })
#define LABEL(l) ((Instr) { .type = INSTR_TYPE_LABEL, .lhs = (l), .size = (4)  })
#define RET ((Instr) { .type = INSTR_TYPE_RET, .size = (4) })
#define PUSH(l) ((Instr) { .type = INSTR_TYPE_PUSH, .lhs = (l), .size = (4) })
#define POP(l) ((Instr) { .type = INSTR_TYPE_POP, .lhs = (l), .size = (4) })
#define CDQ ((Instr) { .type = INSTR_TYPE_CDQ, .size = (4)})
#define IDIV(l, s) ((Instr) { .type = INSTR_TYPE_IDIV, .lhs = (l), .size = (s) })
#define IMUL(l, s) ((Instr) { .type = INSTR_TYPE_IMUL, .lhs = (l), .size = (s) })
#define DIV(l, s) ((Instr) { .type = INSTR_TYPE_DIV, .lhs = (l), .size = (s) })

void instr_append(Instr* instr, string_builder *sb) {
  Value lhs = instr->lhs;
  Value rhs = instr->rhs;

  Size s = instr->size;
  if(s < SIZE_BYTE || COUNT_SIZES <= s) {
    panic("Invalid size: %d", s);
  }  

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
			       REGISTER_NAMES[rhs.as.sval][4], rhs.off);	  
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
			       REGISTER_NAMES[lhs.as.sval][4], lhs.off,
			       rhs.as.sval);
      } break;

      case VALUE_TYPE_REGISTER: {
        string_builder_appendf(sb, "        mov [%s + %lld], %s\n",
			       REGISTER_NAMES[lhs.as.sval][4], lhs.off,
			       REGISTER_NAMES[rhs.as.sval][s]);
      } break;
	
      default:
	panic("unimplemented value.type (rhs): %d", rhs.type);
      }
      
    } break;


    default:
      panic("unimplemented value.type (lhs)");
      
    }

  } break;

  case INSTR_TYPE_SUB: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {

      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        sub %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][4], rhs.as.sval);
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
			       REGISTER_NAMES[lhs.as.sval][4], rhs.as.sval);	
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
			     REGISTER_NAMES[lhs.as.sval][4],
			     REGISTER_NAMES[rhs.as.sval][4],
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
			     REGISTER_NAMES[lhs.as.sval][4]);
    } break;

      
    default:
      panic("unimplemented lhs_type");
    }
    
  } break;

  case INSTR_TYPE_POP: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {
      string_builder_appendf(sb, "        pop %s\n",
			     REGISTER_NAMES[lhs.as.sval][4]);
    } break;
      
    default:
      panic("unimplemented lhs_type");
    }
    
  } break;

  case INSTR_TYPE_CDQ: {

    string_builder_appendf(sb, "        cdq\n");
    
  } break;

  case INSTR_TYPE_DIV: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {
      string_builder_appendf(sb, "        div %s\n",
			     REGISTER_NAMES[lhs.as.sval][s]);
    } break;
      
    default:
      panic("unimplemented lhs_type");
    }
    
  } break;


  case INSTR_TYPE_IDIV: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {
      string_builder_appendf(sb, "        idiv %s\n",
			     REGISTER_NAMES[lhs.as.sval][s]);
    } break;
      
    default:
      panic("unimplemented lhs_type");
    }
    
  } break;

  case INSTR_TYPE_SHL: {

    switch(lhs.type) {

    case VALUE_TYPE_REGISTER: {
      
      switch(rhs.type) {

      case VALUE_TYPE_LITERAL: {
	string_builder_appendf(sb, "        shl %s, %lld\n",
			       REGISTER_NAMES[lhs.as.sval][s],
			       rhs.as.sval);
	
      } break;

      default:
	panic("unimplemented rhs_type");
	
      }
      
    } break;
      
    default:
      panic("unimplemented lhs_type");
    }

    case INSTR_TYPE_IMUL: {

      switch(lhs.type) {

      case VALUE_TYPE_REGISTER: {
	string_builder_appendf(sb, "        imul %s\n",
			       REGISTER_NAMES[lhs.as.sval][s]);
      } break;
      
      default:
	panic("unimplemented lhs_type");
      }
    
    } break;

    
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

void instrs_combine_consecutive_stack_manipulations(Instrs *is) {
  if(is->len == 0) return;

  Value rsp = _RSP;

  Instr last = {0};
  bool has_last = false;
  
  for(u64 i=0;i<is->len;i++) {
    Instr curr = is->data[i];
    if(!has_last) {      
      last = curr;
      has_last = true;
      continue;
    }

    // sub rsp, 34
    // sub rsp, 35    
    //      |
    //      v
    // sub rsp, 69
    if(
       // last is SUB or ADD
       (last.type == INSTR_TYPE_SUB || last.type == INSTR_TYPE_ADD ) &&
       // last.lhs is RSP
       last.lhs.type == rsp.type &&
       last.lhs.as.sval == rsp.as.sval &&
       // last.rhs is LITERAL
       last.rhs.type == VALUE_TYPE_LITERAL &&

       // curr is SUB or ADD
       (curr.type == INSTR_TYPE_SUB || curr.type == INSTR_TYPE_ADD ) &&
       // curr.lhs is RSP
       curr.lhs.type == rsp.type &&
       curr.lhs.as.sval == rsp.as.sval
       // curr.rhs is a LITERAL       
       ) {

      // Patch last
      s64 diff = 0;
      if(last.type == INSTR_TYPE_SUB) {
	diff -= (last.rhs.as.sval);
      } else {
	diff += (last.rhs.as.sval);
      }
      if(curr.type == INSTR_TYPE_SUB) {
	diff -= (curr.rhs.as.sval);
      } else {
	diff += (curr.rhs.as.sval);
      }

      if(diff == 0) {
	
        // Remove last + curr
	memmove(&is->data[i - 1], &is->data[i + 1], (is->len - i - 1) * sizeof(Instr));
	is->len -= 2;
	// The next index must be the same
	i -= 2;

	has_last = false;
      } else {
	// write it back
	if(diff > 0) {
	  last.type = INSTR_TYPE_ADD;
	  last.rhs.as.sval = diff;
	} else {
	  last.type = INSTR_TYPE_SUB;
	  last.rhs.as.sval = -1 * diff;
	}
	is->data[i - 1] = last;

	// Remove curr
	memmove(&is->data[i], &is->data[i + 1], (is->len - i - 1) * sizeof(Instr));
	is->len -= 1;
	// The next index must be the same
	i -= 1;
      }
      
      continue;
    }

    last = curr;
  }

  
}

void instrs_optmize(Instrs *is) {
  instrs_combine_consecutive_stack_manipulations(is);
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
  EXPR_TYPE_STRUCT_FIELD,
  EXPR_TYPE_DEREF,
  EXPR_TYPE_SUM,
  EXPR_TYPE_SUBTRACTION,
  EXPR_TYPE_DIV,
  EXPR_TYPE_MOD,
  EXPR_TYPE_MUL,
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

typedef struct{
  Expr *struct_expr;
  string field_name;
}Expr_Struct_Field;

struct Expr{
  Expr_Type type;
  union{
    s64 sval;
    Expr_Funccall funccall;
    Expr_Bin_Op bin_op;
    Expr_Strings strings;
    Expr_Cast cast;
    Expr_Struct_Field struct_field;
    Expr *expr;
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

Expr *exprs_append_deref(Exprs *es, Expr *expr) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_DEREF;
  e->as.expr = expr;

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

Expr *exprs_append_div(Exprs *es, Expr *lhs, Expr *rhs) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_DIV;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

Expr *exprs_append_mod(Exprs *es, Expr *lhs, Expr *rhs) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_MOD;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

Expr *exprs_append_mul(Exprs *es, Expr *lhs, Expr *rhs) {
  Expr *e = exprs_append(es);
  
  e->type = EXPR_TYPE_MUL;
  e->as.bin_op = (Expr_Bin_Op) { lhs, rhs };

  return e;
}

Expr *exprs_append_struct_field(Exprs *es, Expr *struct_expr, string field_name) {
  Expr *e = exprs_append(es);

  Expr_Struct_Field struct_field = {
    .struct_expr = struct_expr,
    .field_name = field_name
  };
  
  e->type = EXPR_TYPE_STRUCT_FIELD;
  e->as.struct_field = struct_field;

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
  STMT_TYPE_DECLARATION_ARRAY,
  STMT_TYPE_WHILE,
  STMT_TYPE_DECLARATION_STRUCT,
}Stmt_Type;

typedef struct{
  string name;
  Type type;
  u64 count;
}Stmt_Declaration;

typedef struct{
  Expr *lhs;
  Expr *rhs;
}Stmt_Assignment;

typedef struct Stmts Stmts;

typedef enum {
  STMT_IF_TYPE_NONE = 0,
  STMT_IF_TYPE_EQUALS,  
  STMT_IF_TYPE_NOT_EQUALS,
  STMT_IF_TYPE_LESS,
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
    Expr_Strings strings;
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

void stmts_append_declaration_array(Stmts *ss, string name, Type type, u64 count) {

  Stmt_Declaration declaration = { .name = name, .type = type, .count = count };
  
  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_DECLARATION_ARRAY,
	.as.declaration = declaration,
      }));
}

void stmts_append_declaration_struct(Stmts *ss, string name, string struct_name) {

  Expr_Strings strings = { .fst = name, .snd = struct_name };

  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_DECLARATION_STRUCT,
	.as.strings = strings,
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

void stmts_append_while(Stmts *ss, Expr *lhs, Stmt_If_Type op, Expr *rhs, Stmts *body) {

  Stmt_If iff = { .lhs = lhs, .op = op, .rhs = rhs , .body = body };

  da_append(ss, ((Stmt) {
	.type = STMT_TYPE_WHILE,
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

  /* case EXPR_TYPE_STRUCT_FIELD: */
  /*   return true; */

  case EXPR_TYPE_VARIABLE:
    return true;

  case EXPR_TYPE_VALUE:
    return true;

  case EXPR_TYPE_DEREF:
    return expr_is_static(e->as.expr);

  case EXPR_TYPE_SUM:
  case EXPR_TYPE_SUBTRACTION:
  case EXPR_TYPE_DIV:
  case EXPR_TYPE_MOD:
  case EXPR_TYPE_MUL:
    return expr_is_static(e->as.bin_op.lhs) && expr_is_static(e->as.bin_op.rhs);

  case EXPR_TYPE_FUNCCALL:
    return false;
    
  case EXPR_TYPE_CAST:
    return expr_is_static(e->as.cast.expr);

  case EXPR_TYPE_STRUCT_FIELD:
    return expr_is_static(e->as.struct_field.struct_expr);
  }

  panic("unimplemented expr_type");
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
  u64 total_size;
  string name;
  Structure_Fields fields;
}Structure;

u64 structure_size(Structure *s) {
  if(s->total_size == 0) {
    for(u64 i=0;i<s->fields.len;i++) {
      Structure_Field *f = &s->fields.data[i];
      assert(f->type.type != TYPE_STRUCT);

      s->total_size += size_in_bytes(type_size(f->type));
    }
  }

  return s->total_size;  
}

u64 structure_field_off(Structure *s, string field_name) {

  u64 acc = 0;
  bool found = false;
  
  for(u64 i=0;i<s->fields.len;i++) {
    Structure_Field *f = &s->fields.data[i];
    assert(f->type.type != TYPE_STRUCT);
    
    if(string_eq(f->name, field_name)) {
      found = true;
      break;
    }    

    acc += size_in_bytes(type_size(f->type));
  }
  if(!found) {
    panic("field_name should be inside structure");
  }

  return acc;
}

typedef struct{
  Structure *data;
  u64 len;
  u64 cap;
}Structures;

u64 structures_to_index(Structures *structs, Structure *s) {
  return ((u8 *) s - (u8 *) structs->data) / sizeof(Structure);
}

Structure *structures_find(Structures *structs, string name) {
  
  for(u64 i=0;i<structs->len;i++) {
    Structure *s = &structs->data[i];    
    if(string_eq(s->name, name)) {
      return s;
    }
  }

  return NULL;
}

Type expr_to_type(Expr *e, Vars *vars, Functions *fs, Constants *cs, Structures *structs);

bool type_check(Expr *lhs, Expr *rhs, Type *type,
		Vars *vs, Functions *fs, Constants *cs, Structures *structs) {

  Type lhs_type = expr_to_type(lhs, vs, fs, cs, structs);
  Type rhs_type = expr_to_type(rhs, vs, fs, cs, structs);
  
  bool okay = type_equal(lhs_type, rhs_type);

  if(okay) {
    if(type) *type = rhs_type;
    return true;
  } else {
    printf(type_fmt", "type_fmt, type_arg(lhs_type), type_arg(rhs_type));
    return false;
  }
  
}

Type expr_to_type(Expr *e, Vars *vs, Functions *fs, Constants *cs, Structures *structs) {
  
  switch(e->type) {

  case EXPR_TYPE_CAST:
    return e->as.cast.to_type;

  case EXPR_TYPE_VALUE:
    return S64;

  case EXPR_TYPE_DEREF: {
    Type type = expr_to_type(e->as.expr, vs, fs, cs, structs);
    if(type.ptr_degree <= 0) {
      panic("Can not derefernce type: "type_fmt, type_arg(type));
    }

    return type_deref(type);
  } break;
    
  case EXPR_TYPE_VARIABLE_PTR: {

    string name = e->as.strings.fst;

    Var *var = vars_find(vs, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    return type_ptr(var->type);
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD: {

    Type struct_type = expr_to_type(e->as.struct_field.struct_expr, vs, fs, cs, structs);
    if(struct_type.type != TYPE_STRUCT) {
      panic("can not access field of type: "type_fmt, type_arg(struct_type));
    }

    if(struct_type.ptr_degree != 0) {
      panic("todo");
    }

    assert(struct_type.struct_index < structs->len);
    Structure *structure = &structs->data[struct_type.struct_index];
    
    Structure_Field *field = structure_fields_find(&structure->fields, e->as.struct_field.field_name);
    if(field == NULL) {
      panic("struct: '"str_fmt"' has not field named: '"str_fmt"'\n",
	    str_arg(structure->name), str_arg(e->as.struct_field.field_name));
    }

    return field->type;
    
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
    if(!type_check(lhs, rhs, &type, vs, fs, cs, structs)) {
      panic("Can not add expressions with different types");
    }

    return type;
    
  } break;    
    
  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type, vs, fs, cs, structs)) {
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

  case EXPR_TYPE_DIV: {
    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type, vs, fs, cs, structs)) {
      panic("Can not divide expressions with different types");
    }

    return type;

  } break;

  case EXPR_TYPE_MOD: {
    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type, vs, fs, cs, structs)) {
      panic("Can not calculate modulo between expressions with different types");
    }

    return type;

  } break;

  case EXPR_TYPE_MUL: {
    Expr *lhs = e->as.bin_op.lhs;
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type, vs, fs, cs, structs)) {
      panic("Can not multiply expressions with different types");
    }

    return type;

  } break;

    
  }

  panic("unimplemented expr_type: %d", e->type);

}

Type funccall_compile(Expr_Funccall *funccall,
		      Registers *rs, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs, Structures *structs);

void expr_compile(Expr *e, Value location, Size cast_size,
		  Registers *rs, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs, Structures *structs);

typedef struct{
  Value value;
  bool is_dynamic;
}DynamicValue;

DynamicValue expr_location(Expr *e, Type *out_type,
		    Registers *rs, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs, Structures *structs) {
  
  Type type = expr_to_type(e, vs, fs, cs, structs);
  //Size size = type_size(type);
  
  DynamicValue result;
  switch(e->type) {

  case EXPR_TYPE_VARIABLE_PTR:
  case EXPR_TYPE_VARIABLE: {
    string name = e->as.strings.fst;

    Var *var = vars_find(vs, name);
    if(!var) {
      panic("can not find variable with the name: \""str_fmt"\"\n", str_arg(name));
    }

    result = (DynamicValue) {
      .value = _RSP_OFF((s64) (*stack_ptr) - var->off),
      .is_dynamic = false,
    };
  } break;

  case EXPR_TYPE_DEREF: {

    s32 index = registers_lock(rs);
    expr_compile(e->as.expr, REGISTER(index), SIZE_NONE,
		 rs, is, stack_ptr, vs, fs, cs, structs);

    result = (DynamicValue) {
      .value = REGISTER_OFF(index, 0),
      .is_dynamic = true,
    };
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD: {

    Type struct_type;
    DynamicValue dynamicValue = expr_location(e->as.struct_field.struct_expr, &struct_type,
				   rs, is, stack_ptr, vs, fs, cs, structs);
    Value location = dynamicValue.value;
    assert(location.type == VALUE_TYPE_REGISTER_OFF);

    if(struct_type.type != TYPE_STRUCT) {
      panic("can not access field of type: "type_fmt, type_arg(type));
    }
    if(struct_type.ptr_degree != 0) {
      panic("todo");
    }
    assert(struct_type.struct_index < structs->len);
    
    Structure *structure = &structs->data[struct_type.struct_index];
    location.off += structure_field_off(structure, e->as.struct_field.field_name);    
    
    result = (DynamicValue) {
      .value = location,
      .is_dynamic = dynamicValue.is_dynamic,
    };
    
  } break;
    
  default:
    panic("unimplemented expr_type: %d", e->type);
  }

  if(out_type) {
    *out_type = type;
  }

  return result;

}

void expr_compile_smart(Expr *lhs, Value lhs_loc,
			Expr *rhs, Value rhs_loc,
			Size size,
			Registers *rs, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs, Structures *structs) {
  
  bool is_static = expr_is_static(rhs);
  
  if(is_static) {
    expr_compile(lhs, lhs_loc, size,
		 rs, is, stack_ptr, vs, fs, cs, structs);
    expr_compile(rhs, rhs_loc, size,
		 rs, is, stack_ptr, vs, fs, cs, structs);
  } else {
    u64 size_bytes = size_in_bytes(size);
    
    da_append(is, SUB(_RSP, LITERAL(size_bytes), SIZE_QWORD));
    (*stack_ptr) += size_bytes;
    expr_compile(lhs, _RSP_OFF(0), size,
		 rs, is, stack_ptr, vs, fs, cs, structs);
    
    expr_compile(rhs, rhs_loc, size,
		 rs, is, stack_ptr, vs, fs, cs, structs);
    
    da_append(is, MOV(lhs_loc, _RSP_OFF(0), size));
    da_append(is, ADD(_RSP, LITERAL(size_bytes), SIZE_WORD));
    (*stack_ptr) -= size_bytes;
    
  }

}

void expr_compile(Expr *e, Value location, Size cast_size,
		  Registers *rs, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs, Structures *structs) {

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
       s32 index = registers_lock(rs);
       
       da_append(is, MOV(REGISTER(index), LITERAL(value), size));
       da_append(is, MOV(location, REGISTER(index), size));

       registers_release(rs, index);
    }
    
  } break;

  case EXPR_TYPE_FUNCCALL: {

    Type type = funccall_compile(&e->as.funccall,
				 rs, is, stack_ptr, vs, fs, cs, structs);

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }
    
    da_append(is, MOV(location, _RAX, size));
    
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
      s32 index = registers_lock(rs);
      
      da_append(is, MOV(REGISTER(index), CONSTANT(e->as.sval), value_size));
      da_append(is, MOV(location, REGISTER(index), value_size));

      registers_release(rs, index);
    }
    
  } break;

  case EXPR_TYPE_STRUCT_FIELD:
  case EXPR_TYPE_VARIABLE: {

    Type variable_type;
    DynamicValue dynamicValue = expr_location(e, &variable_type,
					    rs, is, stack_ptr, vs, fs, cs, structs);
    Value variable_location = dynamicValue.value;
    
    Size variable_size;
    if(cast_size == SIZE_NONE) {
      variable_size = type_size(variable_type);
    } else {
      variable_size = cast_size;
    }

    if(location.type == VALUE_TYPE_REGISTER_OFF) {
      s32 index = registers_lock(rs);
      
      da_append(is, MOV(REGISTER(index), variable_location, variable_size));
      da_append(is, MOV(location, REGISTER(index), variable_size));

      registers_release(rs, index);
    } else {
      da_append(is, MOV(location, variable_location, variable_size));
    }

    if(dynamicValue.is_dynamic) {
      registers_release(rs, (s32) variable_location.as.sval);
    }
    
  } break;

  case EXPR_TYPE_VARIABLE_PTR: {

    Type variable_type;
    DynamicValue dynamicValue = expr_location(e, &variable_type,
					    rs, is, stack_ptr, vs, fs, cs, structs);
    Value variable_location = dynamicValue.value;
        
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
      s32 index = registers_lock(rs);
      
      da_append(is, LEA(REGISTER(index), variable_location));
      da_append(is, MOV(location, REGISTER(index), variable_ptr_size));

      registers_release(rs, index);
    }

    if(dynamicValue.is_dynamic) {
      registers_release(rs, (s32) variable_location.as.sval);
    }
    
  } break;

  case EXPR_TYPE_SUBTRACTION: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;

    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs, structs)) {
      panic("Can not subtract expressions with different types");
    }
    
    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    /* expr_compile_smart(lhs, RAX, */
    /* 		       rhs, RBX, */
    /* 		       type_size(type), */
    /* 		       rs, is, stack_ptr, vs, fs, cs);     */
    /* da_append(is, SUB(RAX, RBX, size));     */
    /* da_append(is, MOV(location, RAX, size)); */

    s32 lhs_index = registers_lock(rs);
    s32 rhs_index = registers_lock(rs);
    expr_compile_smart(lhs, REGISTER(lhs_index),
		       rhs, REGISTER(rhs_index),
		       size,
		       rs, is, stack_ptr, vs, fs, cs, structs);
    da_append(is, SUB(REGISTER(lhs_index), REGISTER(rhs_index), size));
    da_append(is, MOV(location, REGISTER(lhs_index), size));
  
    registers_release(rs, lhs_index);
    registers_release(rs, rhs_index);

  } break;

  case EXPR_TYPE_CAST: {

    Type type = expr_to_type(e->as.cast.expr, vs, fs, cs, structs);
    Size size = type_size(type);

    Type to_cast_type = e->as.cast.to_type;
    Size to_cast_size = type_size(to_cast_type);

    if(size == to_cast_size) {
      expr_compile(e->as.cast.expr, location, to_cast_size,
		   rs, is, stack_ptr, vs, fs, cs, structs);
    } else if(size > to_cast_size) {
      expr_compile(e->as.cast.expr, location, to_cast_size,
		   rs, is, stack_ptr, vs, fs, cs, structs);
    } else  { // size < to_cast_size
      
      s32 index = registers_lock(rs);      
      if(expr_is_static(e->as.cast.expr)) {
	da_append(is, MOV(REGISTER(index), LITERAL(0), SIZE_QWORD));
	expr_compile(e->as.cast.expr, REGISTER(index), size,
		     rs, is, stack_ptr, vs, fs, cs, structs);
	da_append(is, MOV(location, REGISTER(index), to_cast_size));
      } else {
	
	expr_compile(e->as.cast.expr, REGISTER(index), size,
		     rs, is, stack_ptr, vs, fs, cs, structs);		
	s32 snd_index = registers_lock(rs);
	da_append(is, MOV(REGISTER(snd_index), LITERAL(0), SIZE_QWORD));
	da_append(is, MOV(REGISTER(snd_index), REGISTER(index), size));	
	da_append(is, MOV(location, REGISTER(snd_index), to_cast_size));
	registers_release(rs, snd_index);
     }
      
      registers_release(rs, index);
      
    }        
    
  } break;

  case EXPR_TYPE_SUM: {

    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;
    
    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs, structs)) {
      panic("Can not add expressions with different types");
    }

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }
    
    s32 lhs_index = registers_lock(rs);
    s32 rhs_index = registers_lock(rs);  
    expr_compile_smart(lhs, REGISTER(lhs_index),
		       rhs, REGISTER(rhs_index),
		       size,
		       rs, is, stack_ptr, vs, fs, cs, structs);
    da_append(is, ADD(REGISTER(lhs_index), REGISTER(rhs_index), size));
    da_append(is, MOV(location, REGISTER(lhs_index), size));
  
    registers_release(rs, lhs_index);
    registers_release(rs, rhs_index);

    
  } break;

  case EXPR_TYPE_DEREF: {

    Type type;
    DynamicValue dynamicValue = expr_location(e, &type,
				 rs, is, stack_ptr, vs, fs, cs, structs);
    Value source = dynamicValue.value;

    Size size;
    if(cast_size != SIZE_NONE) {
      size = cast_size;
    } else {
      size = type_size(type);
    }

    if(location.type == VALUE_TYPE_REGISTER_OFF) {
      s32 index = registers_lock(rs);
      
      da_append(is, MOV(REGISTER(index), source, size));
      da_append(is, MOV(location, REGISTER(index), size));

      registers_release(rs, index);
    } else {
      da_append(is, MOV(location, source, size));
    }
    
    if(dynamicValue.is_dynamic) {
      registers_release(rs, (s32) source.as.sval);
    }

    
  } break;

  case EXPR_TYPE_DIV: {
    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;
    
    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs, structs)) {
      panic("Can not divide expressions with different types");
    }

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    // https://www.felixcloutier.com/x86/idiv
    if(size < SIZE_QWORD) {
      size = SIZE_DWORD;
    }

    s32 index = registers_lock(rs);    
    expr_compile_smart(lhs, _RAX,
		       rhs, REGISTER(index),
		       size,
		       rs, is, stack_ptr, vs, fs, cs, structs);    
    da_append(is, CDQ);
    da_append(is, IDIV(REGISTER(index), size));
    registers_release(rs, index);
    da_append(is, MOV(location, _RAX, size));    
    
  } break;

  case EXPR_TYPE_MOD: {
    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;
    
    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs, structs)) {
      panic("Can not divide expressions with different types");
    }

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    // https://www.felixcloutier.com/x86/idiv
    if(size < SIZE_QWORD) {
      size = SIZE_DWORD;
    }

    s32 index = registers_lock(rs);    
    expr_compile_smart(lhs, _RAX,
		       rhs, REGISTER(index),
		       size,
		       rs, is, stack_ptr, vs, fs, cs, structs);    
    da_append(is, CDQ);
    da_append(is, IDIV(REGISTER(index), size));
    registers_release(rs, index);
    da_append(is, MOV(location, _RDX, size));    
    
  } break;

  case EXPR_TYPE_MUL: {
    Expr *lhs = e->as.bin_op.lhs;    
    Expr *rhs = e->as.bin_op.rhs;
    
    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs, structs)) {
      panic("Can not divide expressions with different types");
    }

    Size size;
    if(cast_size == SIZE_NONE) {
      size = type_size(type);
    } else {
      size = cast_size;
    }

    // https://www.felixcloutier.com/x86/idiv
    if(size < SIZE_QWORD) {
      size = SIZE_DWORD;
    }

    s32 index = registers_lock(rs);    
    expr_compile_smart(lhs, _RAX,
		       rhs, REGISTER(index),
		       size,
		       rs, is, stack_ptr, vs, fs, cs, structs);    
    da_append(is, CDQ);
    da_append(is, IMUL(REGISTER(index), size));
    registers_release(rs, index);
    da_append(is, MOV(location, _RAX, size));    

  } break;
    
  default:
    panic("Unimplemented expr->type: %d", e->type);
    
  }  
}

Type funccall_compile(Expr_Funccall *funccall,
		      Registers *rs, Instrs *is, u64 *stack_ptr, Vars *vs, Functions *fs, Constants *cs, Structures *structs) {

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
    Type arg_type = expr_to_type(arg, vs, fs, cs, structs);

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
  da_append(is, SUB(_RSP, LITERAL(shadow_space), SIZE_QWORD));

  // calculate argument-expressions and save them in the shadowspace
  for(u64 i=0;i<funccall->args_len;i++) {
    Expr *e = funccall->args[i];

    if(expr_is_static(e)) {
      // pass
    } else {
      expr_compile(e, _RSP_OFF((*stack_ptr) - stack_ptr_before - shadow_space + i  * 8), SIZE_NONE,
		   rs, is, stack_ptr, vs, fs, cs, structs);
    }    
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
		     rs, is, stack_ptr, vs, fs, cs, structs);
      } else {
	da_append(is, MOV(REGISTER(FASTCALL_REGISTERS[i]), _RSP_OFF(target), param_size));
      }

    } else { // push ...

      if(expr_is_static(e)) {
	s32 index = registers_lock(rs);
	
	expr_compile(e, REGISTER(index), SIZE_NONE,
		     rs, is, stack_ptr, vs, fs, cs, structs);
	da_append(is, MOV(_RSP_OFF(target), REGISTER(index), param_size));

	registers_release(rs, index);
      } else {
	// variable already in the right place
      }
    }
    
    
  }  
  
  da_append(is, CALL(WORD(funccall->name)));

  da_append(is, ADD(_RSP, LITERAL((*stack_ptr) - stack_ptr_before), SIZE_QWORD));
  *stack_ptr = stack_ptr_before;

  return f->return_type;
}

bool stmt_compile(Type return_type,
		  Stmt *s,
		  Registers *rs,
		  Instrs *is,
		  u64 *stack_ptr,
		  u64 *label_count,
		  Vars *vs,		  
		  Functions *fs,
		  Constants *cs,
		  Structures *structs,
		  Type *out_type);

typedef struct{
  u64 vars_len;
  u64 stack_ptr;
  u64 label;
  
  bool result;
  bool returned;
}Program_State;

Program_State stmt_compile_if(Stmt_If *iff,
			      Registers *rs,
			      Instrs *is,
			      u64 *stack_ptr,
			      u64 *label_count,
			      Vars *vs,
			      Functions *fs,
			      Constants *cs,
			      Structures *structs,
			      Type return_type,
			      Type *out_type) {
  
  Expr *lhs = iff->lhs;
  Stmt_If_Type op = iff->op;
  Expr *rhs = iff->rhs;

  Type type;
  if(!type_check(lhs, rhs, &type,
		 vs, fs, cs, structs)) {
    panic("Can compare expressions with different types"); 
  }
  Size size = type_size(type);
  
  s32 lhs_index = registers_lock(rs);
  s32 rhs_index = registers_lock(rs);
  expr_compile_smart(lhs, REGISTER(lhs_index),
		     rhs, REGISTER(rhs_index),
		     size,
		     rs, is, stack_ptr, vs, fs, cs, structs);
  da_append(is, CMP(REGISTER(lhs_index), REGISTER(rhs_index), size));  
  registers_release(rs, lhs_index);
  registers_release(rs, rhs_index);
  
  Program_State state = {
    .stack_ptr = *stack_ptr,
    .vars_len = vs->len,
    .label = *label_count,
    .result = true,
  };  
  (*label_count) = (*label_count) + 1;

  switch(op) {
      
  case STMT_IF_TYPE_EQUALS: {
    da_append(is, JNE(LITERAL(state.label)));
  } break;

  case STMT_IF_TYPE_NOT_EQUALS: {
    da_append(is, JE(LITERAL(state.label)));
  } break;
    
  case STMT_IF_TYPE_LESS: {
    da_append(is, JGE(LITERAL(state.label)));
  } break;

  default:
    panic("Unimplemented stmt_if_type");
      
  }    

  Stmts *stmts = iff->body;
  bool returned = false;
  for(u64 i=0;i<stmts->len;i++) {

    if(returned) {
      panic("Found dead code in if");
    }
      
    if(stmt_compile(return_type,
		    &stmts->data[i],
		    rs,
		    is,
		    stack_ptr,
		    label_count,
		    vs,		  
		    fs,
		    cs,
		    structs,
		    out_type)) {
      returned = true;
    }
  }
  state.returned = returned;
  
  return state;  
}

// Return true, if stmt returns and write output into 'out_type'
bool stmt_compile(Type return_type,
		  Stmt *s,
		  Registers *rs,
		  Instrs *is,
		  u64 *stack_ptr,
		  u64 *label_count,
		  Vars *vs,		  
		  Functions *fs,
		  Constants *cs,
		  Structures *structs,
		  Type *out_type) {

  switch(s->type) {

  case STMT_TYPE_FUNCCALL: {

    // TODO: add warning of unused return_type
    funccall_compile(&s->as.funccall, rs, is, stack_ptr, vs, fs, cs, structs);
    
  } break;

  case STMT_TYPE_DECLARATION: {

    Stmt_Declaration *declaration = &s->as.declaration;

    if(vars_find(vs, declaration->name)) {
      panic("variable already declared");
    }
    
    u64 n = size_in_bytes(type_size(declaration->type));

    da_append(is, SUB(_RSP, LITERAL(n), SIZE_QWORD));
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

    Type type;
    if(!type_check(lhs, rhs, &type,
		   vs, fs, cs, structs)) {
      panic("Can not assign expressions with different type");
    }
    Size size = type_size(type);


    DynamicValue dynamicValue;
    Value location;
    if(expr_is_static(rhs)) {
      dynamicValue = expr_location(lhs, NULL,
			       rs, is, stack_ptr, vs, fs, cs, structs);
      location = dynamicValue.value;
      expr_compile(rhs, location, SIZE_NONE,
		   rs, is, stack_ptr, vs, fs, cs, structs);
    } else {
      s32 index = registers_lock(rs);

      expr_compile(rhs, REGISTER(index), SIZE_NONE,
		   rs, is, stack_ptr, vs, fs, cs, structs);
      dynamicValue = expr_location(lhs, NULL,
			       rs, is, stack_ptr, vs, fs, cs, structs);
      location = dynamicValue.value;
      da_append(is, MOV(location, REGISTER(index), size));

      registers_release(rs, index);
    }

    if(dynamicValue.is_dynamic) {
      registers_release(rs, (s32) location.as.sval);
    }    
    
  } break;

  case STMT_TYPE_IF: {

    // TODO: figure this logic out
    Program_State state = stmt_compile_if(&s->as.iff,
					  rs,
					  is,
					  stack_ptr,
					  label_count,
					  vs,
					  fs,
					  cs,
					  structs,
					  return_type,
					  out_type);
    if(!state.result) {
      return false;
    }
    
    vs->len = state.vars_len;
    if(!state.returned && (*stack_ptr) != state.stack_ptr) {
      da_append(is, ADD(_RSP, LITERAL((*stack_ptr) - state.stack_ptr), SIZE_QWORD));
    }
    (*stack_ptr) = state.stack_ptr;
    da_append(is, LABEL(LITERAL(state.label)));

    
    /* if(do_return) { */
    /*   return true; */
    /* } */
    
  } break;

  case STMT_TYPE_RETURN: {

    Expr *expr = s->as.expr;
    Type type = expr_to_type(expr, vs, fs, cs, structs);

    if(!type_equal(type, return_type)) {
      printf(type_fmt", "type_fmt, type_arg(type), type_arg(return_type));
      panic("Can not return with a different type");
    }

    if(type.ptr_degree > 0) {
      expr_compile(expr, _RAX, SIZE_NONE,
		   rs, is, stack_ptr, vs, fs, cs, structs);
    } else if(type.type == TYPE_STRUCT) {
      panic("todo");
    } else if(type.type == TYPE_VOID) {
      // pass
    } else {
      expr_compile(expr, _RAX, SIZE_NONE,
		   rs, is, stack_ptr, vs, fs, cs, structs);
    }    

    if((*stack_ptr) > 0) {
      da_append(is, ADD(_RSP, LITERAL((*stack_ptr)), SIZE_QWORD));
      *stack_ptr = 0;
    }
    da_append(is, RET);

    *out_type = type;
    return true;
  
  } break;

  case STMT_TYPE_DECLARATION_ARRAY: {

    Stmt_Declaration *declaration = &s->as.declaration;

    if(vars_find(vs, declaration->name)) {
      panic("variable already declared");
    }

    Size size = type_size(declaration->type);
    Type ptr_type = type_ptr(declaration->type);
    Size size_ptr = type_size(ptr_type);
  
    u64 n_ptr = size_in_bytes(size_ptr);
    u64 ns = declaration->count * size_in_bytes(size);
  
    da_append(is, SUB(_RSP, LITERAL(ns + n_ptr), SIZE_QWORD));

    s32 index = registers_lock(rs);
      
    da_append(is, LEA(REGISTER(index), _RSP_OFF(n_ptr)));
    da_append(is, MOV(_RSP_OFF(0), REGISTER(index), size_ptr));

    registers_release(rs, index);
    
    (*stack_ptr) += ns + n_ptr;
    
    da_append(vs, ((Var) {
	  .name = declaration->name,
	  .type = ptr_type,
	  .off = (*stack_ptr),
	}));
    
  } break;

  case STMT_TYPE_WHILE: {

    u64 label = (*label_count);
    *label_count = label + 1;
    
    da_append(is, LABEL(LITERAL(label)));
    Program_State state = stmt_compile_if(&s->as.iff,
					  rs,
					  is,
					  stack_ptr,
					  label_count,
					  vs,
					  fs,
					  cs,
					  structs,
					  return_type,
					  out_type);    
    if(!state.result) {
      return false;
    }
    da_append(is, JMP(LITERAL(label)));
    
    vs->len = state.vars_len;
    if(!state.returned && (*stack_ptr) != state.stack_ptr) {
      da_append(is, ADD(_RSP, LITERAL((*stack_ptr) - state.stack_ptr), SIZE_QWORD));
    }
    (*stack_ptr) = state.stack_ptr;
    da_append(is, LABEL(LITERAL(state.label)));
    
    
  } break;

  case STMT_TYPE_DECLARATION_STRUCT: {

    string name = s->as.strings.fst;
    string struct_name = s->as.strings.snd;

    if(vars_find(vs, name)) {
      panic("variable already declared");
    }

    Structure *structure = structures_find(structs, struct_name);
    if(structure == NULL) {
      panic("Can not find structure: '"str_fmt"'", str_arg(struct_name));
    }

    u64 n = structure_size(structure);

    da_append(is, SUB(_RSP, LITERAL(n), SIZE_QWORD));
    *stack_ptr += n;
  
    da_append(vs, ((Var) {
	  .name = name,
	  .type = STRUCT((u32) structures_to_index(structs, structure)),
	  .off  = (s64) (*stack_ptr),
	}));

    
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
  Structures structures;
}Program;

void program_append(Program *p, string_builder *sb, bool optimize) {

  Vars vars = {0};
  Instrs instrs = {0};
  Registers registers = {0};

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
      da_append(&instrs, MOV(_RSP_OFF(off), REGISTER(FASTCALL_REGISTERS[j]), SIZE_QWORD));
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
				       &registers,
				       &instrs,
				       &stack_ptr,
				       &label_count,
				       &vars,
				       &p->functions,
				       &p->constants,
				       &p->structures,
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
	da_append(&instrs, ADD(_RSP, LITERAL(stack_ptr), SIZE_QWORD));	   
      }
      da_append(&instrs, RET);
    }

    if(optimize) {
      instrs_optmize(&instrs);
    }
    for(u64 j=0;j<instrs.len;j++) {
      instr_append(&instrs.data[j], sb);
    }
    instrs.len = 0;

    memset(&registers, 0, sizeof(Registers));
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

  Function getCommandLineA = {0};
  getCommandLineA.external = true;
  getCommandLineA.return_type = PTR(TYPE_U8);
  getCommandLineA.name = string_from_cstr("GetCommandLineA");
  da_append(&p->functions, getCommandLineA);
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
  Stmts *ss = &f.stmts;

  // return 35;
  stmts_append_return(ss, exprs_append_cast(es, exprs_append_value(es, 35), U8));

  da_append(&p->functions, f);

}

void appendFunctionMain2(Program *p) {

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

void appendFunctionStrlen(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("strlen");
  f.return_type = U8;
  da_append(&f.params, ((Param) {
	.type = PTR(TYPE_U8),
	.name = string_from_cstr("cstr")
      }));
  
  Stmts *ss = &f.stmts;
  Exprs *es = &p->exprs;
  Stmtss *sss = &p->stmtss;

  // len : u32 = 0;
  stmts_append_declaration(ss,
			   string_from_cstr("len"),
			   U8);
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("len")),
			  exprs_append_cast(es, exprs_append_value(es, 0), U8));
  
  Stmts *while_body = stmtss_append(sss);
  memset(while_body, 0, sizeof(Stmts));
  {
    stmts_append_assignment(while_body,
			    exprs_append_variable(es, string_from_cstr("cstr")),
			    exprs_append_sum(es,
					     exprs_append_variable(es, string_from_cstr("cstr")),
					     exprs_append_cast(es, exprs_append_value(es, 1), PTR(TYPE_U8))));
    stmts_append_assignment(while_body,
			    exprs_append_variable(es, string_from_cstr("len")),
			    exprs_append_sum(es,
					     exprs_append_variable(es, string_from_cstr("len")),
					     exprs_append_cast(es, exprs_append_value(es, 1), U8)));

  }
  stmts_append_while(ss,
		     exprs_append_cast(es, exprs_append_value(es, 0), U8),
		     STMT_IF_TYPE_NOT_EQUALS,
		     exprs_append_deref(es, exprs_append_variable(es, string_from_cstr("cstr"))),
		     while_body);

  // return 0;
  stmts_append_return(ss, exprs_append_variable(es, string_from_cstr("len")));

  da_append(&p->functions, f);
}

void appendFunctionPrintNumber(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("printNumber");
  f.return_type = _VOID;

  da_append(&f.params, ((Param) {
	.type = S64,
	.name = string_from_cstr("n"),
      }));

  Stmts *ss = &f.stmts;
  Exprs *es = &p->exprs;  
  Stmtss *sss = &p->stmtss;

  // minus : u8 = 0
  stmts_append_declaration(ss,
			   string_from_cstr("minus"),
			   U8);
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("minus")),
			  exprs_append_cast(es, exprs_append_value(es, 0), U8));

  // if(n < 0) {
  Stmts *if_body = stmtss_append(sss);
  memset(if_body, 0, sizeof(Stmts));
  {
    // n = n * -1;
    stmts_append_assignment(if_body,
			    exprs_append_variable(es, string_from_cstr("n")),
			    exprs_append_mul(es,
					     exprs_append_variable(es, string_from_cstr("n")),
					     exprs_append_value(es, -1)));
    // minus = 1;
    stmts_append_assignment(if_body,
			    exprs_append_variable(es, string_from_cstr("minus")),
			    exprs_append_cast(es, exprs_append_value(es, 1), U8));
  }
  stmts_append_if(ss,
		  exprs_append_variable(es, string_from_cstr("n")),
		  STMT_IF_TYPE_LESS,
		  exprs_append_value(es, 0),
		  if_body);
  // }

  s64 buf_len = 32;
  // buf : u8[33];
  stmts_append_declaration_array(ss, string_from_cstr("buf"), U8, buf_len + 1);

  // i : u8 = 31
  stmts_append_declaration(ss, string_from_cstr("i"), U8);
  stmts_append_assignment(ss,
			  exprs_append_variable(es, string_from_cstr("i")),
			  exprs_append_cast(es, exprs_append_value(es, buf_len - 1), U8));

  // while(n != 0) {
  Stmts *while_body = stmtss_append(sss);
  memset(while_body, 0, sizeof(Stmts));
  {
    //*(buf + (u8*) i) = (u8) ((n % 10) + 48);
    stmts_append_assignment(while_body,
			    exprs_append_deref(es,
					       exprs_append_sum(es,
								exprs_append_variable(es, string_from_cstr("buf")),
								exprs_append_cast(es, exprs_append_variable(es, string_from_cstr("i")), PTR(TYPE_U8)))),
			    exprs_append_cast(es,
					      exprs_append_sum(es,
							       exprs_append_mod(es,
										exprs_append_variable(es, string_from_cstr("n")),
										exprs_append_value(es, 10)),
							       exprs_append_value(es, 48)
							       ),
					      U8));

    // n = n / 10
    stmts_append_assignment(while_body,
			    exprs_append_variable(es, string_from_cstr("n")),
			    exprs_append_div(es,
					     exprs_append_variable(es, string_from_cstr("n")),
					     exprs_append_value(es, 10))
			    );

    // i = i - 1
    stmts_append_assignment(while_body,
			    exprs_append_variable(es, string_from_cstr("i")),
			    exprs_append_sub(es,
					     exprs_append_variable(es, string_from_cstr("i")),
					     exprs_append_cast(es, exprs_append_value(es, 1), U8)));

  }
  stmts_append_while(ss,
		     exprs_append_cast(es, exprs_append_value(es, 0), S64),
		     STMT_IF_TYPE_NOT_EQUALS,
		     exprs_append_variable(es, string_from_cstr("n")),
		     while_body);
  // }

  //*(buf + buf_len) = '\n';
  stmts_append_assignment(ss,
			  exprs_append_deref(es,
					     exprs_append_sum(es,
							      exprs_append_variable(es, string_from_cstr("buf")),
							      exprs_append_cast(es, exprs_append_value(es, buf_len), PTR(TYPE_U8)))),
			  exprs_append_cast(es,
					    exprs_append_value(es, 10),
					    U8));

  // if(minus == 1) {
  Stmts *minus_body = stmtss_append(sss);
  memset(minus_body, 0, sizeof(Stmts));
  {
    //*(buf + (u8*) i) = '-';
    stmts_append_assignment(minus_body,
			    exprs_append_deref(es,
					       exprs_append_sum(es,
								exprs_append_variable(es, string_from_cstr("buf")),
								exprs_append_cast(es, exprs_append_variable(es, string_from_cstr("i")), PTR(TYPE_U8)))),
			    exprs_append_cast(es,
					      exprs_append_value(es, 45),
					      U8));

    
    // i = i - 1
    stmts_append_assignment(minus_body,
			    exprs_append_variable(es, string_from_cstr("i")),
			    exprs_append_sub(es,
					     exprs_append_variable(es, string_from_cstr("i")),
					     exprs_append_cast(es, exprs_append_value(es, 1), U8)));
  }
  stmts_append_if(ss,
		  exprs_append_variable(es, string_from_cstr("minus")),
		  STMT_IF_TYPE_EQUALS,
		  exprs_append_cast(es, exprs_append_value(es, 1), U8),
		  minus_body);
  // }

  // written : u32;
  stmts_append_declaration(ss, string_from_cstr("written"), U32);
 
  // WriteFile(GetStdHandle((u32) -11), buf + i + 1, buf_len - i, &written, (u64) 0);
  stmts_append_funccall(ss,
			string_from_cstr("WriteFile"),
		        exprs_append_funccall(es,
					      string_from_cstr("GetStdHandle"),
					      exprs_append_cast(es, exprs_append_value(es, -11), U32)),
			exprs_append_sum(es,
					 exprs_append_variable(es, string_from_cstr("buf")),
					 exprs_append_cast(es,
							   exprs_append_sum(es,
									    exprs_append_variable(es, string_from_cstr("i")),
									    exprs_append_cast(es, exprs_append_value(es, 1), U8)),
							   PTR(TYPE_U8))),
			exprs_append_cast(es,
					  exprs_append_sub(es,
							   exprs_append_cast(es, exprs_append_value(es, buf_len), U8),
							   exprs_append_variable(es, string_from_cstr("i"))),
					  U32),
			exprs_append_pointer(es, string_from_cstr("written")),
			exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)));
  
  da_append(&p->functions, f);
}

void appendFunctionMain3(Program *p) {
  
  Function f = {0};
  f.name = string_from_cstr("main");
  f.return_type = _VOID;

  Exprs *es = &p->exprs;
  //Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;  

  const char *message_cstr = "Foo\n";
  Expr *message = exprs_append_constant(es, constants_append_cstr(&p->constants, message_cstr));

  // written : u32;
  stmts_append_declaration(ss, string_from_cstr("written"), U32);

  // WriteFile(GetStdHandle((u32) -11), message, strlen(message), &written, (u64) 0);
  stmts_append_funccall(ss,
			string_from_cstr("WriteFile"),
		        exprs_append_funccall(es,
					      string_from_cstr("GetStdHandle"),
					      exprs_append_cast(es, exprs_append_value(es, -11), U32)),
			message,
			exprs_append_cast(es, exprs_append_funccall(es, string_from_cstr("strlen"), message), U32),
			exprs_append_pointer(es, string_from_cstr("written")),
			exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)));

  s64 value = -12345;
  stmts_append_funccall(ss,
			string_from_cstr("printNumber"),
			exprs_append_value(es, value));

  // Exitprocess(0);
  stmts_append_funccall(ss,
			string_from_cstr("ExitProcess"),
			exprs_append_cast(es, exprs_append_value(es, 0), U8));

  da_append(&p->functions, f);
}

void appendFunctionCstrToString(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("cstrToString");
  f.return_type = _VOID;
  da_append(&f.params, ((Param) {
	.type = PTR(TYPE_U8),
	.name = string_from_cstr("cstr"),
      }));
  Structure *s = structures_find(&p->structures, string_from_cstr("string"));
  assert(s);
  da_append(&f.params, ((Param) {
	.type = type_ptr(STRUCT((u32) structures_to_index(&p->structures, s))),
	.name = string_from_cstr("s"),
      }));

  Exprs *es = &p->exprs;
  //Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;
  
  // (*s).data = cstr
  stmts_append_assignment(ss,			  
			  exprs_append_struct_field(es,
						    exprs_append_deref(es, exprs_append_variable(es, string_from_cstr("s"))),
						    string_from_cstr("data")),
			  exprs_append_variable(es, string_from_cstr("cstr")));

  // (*s).len = 2;
  stmts_append_assignment(ss,			  
			  exprs_append_struct_field(es,
						    exprs_append_deref(es, exprs_append_variable(es, string_from_cstr("s"))),
						    string_from_cstr("len")),
			  exprs_append_cast(es,
					    exprs_append_funccall(es,
								  string_from_cstr("strlen"),
								  exprs_append_variable(es, string_from_cstr("cstr"))),
					    U64));

  da_append(&p->functions, f);
  
}

void appendFunctionMain(Program *p) {

  Function f = {0};
  f.name = string_from_cstr("main");
  f.return_type = _VOID;

  Exprs *es = &p->exprs;
  //Stmtss *sss = &p->stmtss;
  Stmts *ss = &f.stmts;


  // s : string;
  stmts_append_declaration_struct(ss,
				  string_from_cstr("s"),
				  string_from_cstr("string"));
  // cstrToString(GetCommandLineA(), &s);
  stmts_append_funccall(ss,
			string_from_cstr("cstrToString"),
			exprs_append_funccall(es,
						string_from_cstr("GetCommandLineA")),
			exprs_append_pointer(es, string_from_cstr("s")));

  // written : u32;
  stmts_append_declaration(ss, string_from_cstr("written"), U32);

  // WriteFile(GetStdHandle((u32) -11), s.data, s.len, &written, (u64) 0);
  stmts_append_funccall(ss,
			string_from_cstr("WriteFile"),
		        exprs_append_funccall(es,
					      string_from_cstr("GetStdHandle"),
					      exprs_append_cast(es, exprs_append_value(es, -11), U32)),
			exprs_append_struct_field(es,
						  exprs_append_variable(es, string_from_cstr("s")),
						  string_from_cstr("data")),
			exprs_append_cast(es,
					  exprs_append_struct_field(es,
								    exprs_append_variable(es, string_from_cstr("s")),
								    string_from_cstr("len")),
					  U32),
			exprs_append_pointer(es, string_from_cstr("written")),
			exprs_append_cast(es, exprs_append_value(es, 0), PTR(TYPE_U64)));
  
  // Exitprocess(0);
  stmts_append_funccall(ss,
			string_from_cstr("ExitProcess"),
			exprs_append_cast(es, exprs_append_value(es, 0), U8));

  da_append(&p->functions, f);
}

void appendStructureString(Program *p) {

  Structure structure = {0};
  structure.name = string_from_cstr("string");
  da_append(&structure.fields, ((Structure_Field) {
	.name = string_from_cstr("data"),
	.type = PTR(TYPE_U8)
      }));
  da_append(&structure.fields, ((Structure_Field) {
	.name = string_from_cstr("len"),
	.type = U64
      }));


  da_append(&p->structures, structure);
}

int main() {

  Program program = {0};
  appendFunctionsWinApi(&program);
  appendStructureString(&program);
  
  /* appendFunctionDump(&program); */
  /* appendFunctionFoo(&program); */
  /* appendFunctionAdd(&program); */
  /* appendFunctionMain2(&program); */
  /* appendFunctionSub(&program); */
  
  /* appendFunctionStrlen(&program); */
  /* appendFunctionMain3(&program); */
  /* appendFunctionPrintNumber(&program); */
  
  appendFunctionStrlen(&program);
  appendFunctionMain(&program);
  appendFunctionCstrToString(&program);
  
  string_builder sb = {0};  
  program_append(&program, &sb, false);
  printf("%.*s", (int) sb.len, sb.data);

  if(!io_write_file("foo.asm", (u8 *) sb.data, sb.len)) {
    return 1;
  }
  
  return 0;
}
