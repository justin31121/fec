#include <stdio.h>

#define IO_IMPLEMENTATION
#include "core/io.h"

#include "core/compiler.h"

#include "core/cmd.h"

s8 *next(s32 *argc, s8 ***argv) {
  if(*argc == 0) {
    return NULL;
  }

  s8 *arg = (*argv)[0];
  *argv = (*argv)+1;
  *argc = (*argc)-1;

  return arg;
}

void *global_alloc(void *userdata, u64 size_bytes) {
  (void) userdata;
  void *result = malloc(size_bytes);
  assert(result);
  return result;
}

s32 main(s32 argc, s8 **argv) {

  s8 *prog = next(&argc, &argv);

  string s = (string) {0};
  s8 *arg = next(&argc, &argv);
  if(!arg) {
    fprintf(stderr, "ERROR: Please provide an argument\n");
    fprintf(stderr, "USAGE: %s [-o output-name>] <input_name>\n", prog);
    return 1;
  }

  if(strcmp(arg, "-o") == 0) {
    arg = next(&argc, &argv);
    if(!arg) {
      fprintf(stderr, "ERROR: No output path specified\n");
      fprintf(stderr, "USAGE: %s [-o <output-name>] <input_name>\n", prog);
      return 1;
    }
    s = string_from_cstr(arg);
    arg = next(&argc, &argv);
  }
  
  if(!arg) {
    fprintf(stderr, "ERROR: No input specified\n");
    fprintf(stderr, "USAGE: %s [-o <output-name>] <input_name>\n", prog);
    return 1;
  }
  s8 *input = arg;

  if(!s.len) {
    s = string_from_cstr(input);
    s32 pos = string_last_index_of(s, ".");
    if(pos >= 0) {
      assert(string_substring(s, 0, pos, &s));
    }    
  }

  s8 *name_asm = cstrf(str_fmt".asm", str_arg(s));
  s8 *name_obj = cstrf(str_fmt".obj", str_arg(s));
  s8 *name_exe = cstrf(str_fmt".exe", str_arg(s));
  
  string_builder sb = {0};

  ////////////////////////////////////////////////////////////////

  // Compile

  Compiler compiler;
  if(!compiler_from(input, global_alloc, NULL, &compiler)) {
    return 1;
  }

  sb.len = 0;
  if(!compiler_compile(&compiler, &sb)) {
    return 1;
  }

  assert(sb.len);
  if(!io_write_file(name_asm, sb.items, sb.len)) {
    return 1;
  }

  ////////////////////////////////////////////////////////////////
  
  // Assemble

  Cmd cmd = {0};
  cmd_append(&cmd, "nasm", "-f", "win64", name_asm, "-o", name_obj);
  u8 code;
  sb.len = 0;
  if(!cmd_execute(&cmd, &code, &sb, NULL, NULL)) {
    fprintf(stderr, "ERROR: Can not execute 'nasm'. Make sure its on your PATH\n");
    return 1;
  }

  if(code != 0) {
    return 1;
  }

  ////////////////////////////////////////////////////////////////

  // Link

  cmd.len = 0;
#ifdef _MSC_VER
  name_exe = cstrf("/OUT:%s", name_exe);
  cmd_append(&cmd, "link", "/ENTRY:main", "/SUBSYSTEM:console", name_exe, name_obj, "kernel32.lib");
#else
  cmd_append(&cmd, "ld", "-LC:\\Windows\\System32", "-o", name_exe, name_obj, "-lkernel32");
#endif // _MSC_VER

  sb.len = 0;
  if(!cmd_execute(&cmd, &code, &sb, NULL, NULL)) {
    fprintf(stderr, "ERROR: Can not link. Make sure it to have either 'ld' or 'link'installed and in your PATH\n");
    return 1;    
  }

  if(code != 0) {
    return 1;
  }
  
  return 0;
}
