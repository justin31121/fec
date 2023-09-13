#include <stdio.h>

#define IO_IMPLEMENTATION
#include "io.h"

#include "compiler.h"

#include "cmd.h"

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
  s8 *input = next(&argc, &argv);
  if(!input) {
    fprintf(stderr, "ERROR: Please provide an argument\n");
    fprintf(stderr, "USAGE: %s <source>\n", prog);
    return 1;
  }

  string_builder sb = {0};

  ////////////////////////////////////////////////////////////////

  // Compile

  Compiler compiler;
  if(!compiler_from(input, global_alloc, NULL, &compiler)) {
    return 1;
  }

  if(!compiler_compile(&compiler, &sb)) {
    return 1;
  }
  
  assert(sb.len);
  if(!io_write_file("main.asm", sb.items, sb.len - 1)) {
    return 1;
  }
  sb.len = 0;

  ////////////////////////////////////////////////////////////////
  
  // Assemble

  Cmd cmd = {0};
  cmd_append(&cmd, "nasm", "-f", "win64", "main.asm", "-o", "main.obj");
  u8 code;
  if(!cmd_execute(&cmd, &code, &sb)) {
    fprintf(stderr, "ERROR: Can not execute 'nasm'. Make sure its on your PATH\n");
    return 1;
  }
  cmd.len = 0;

  if(code != 0) {
    return 1;
  }

  ////////////////////////////////////////////////////////////////

  // Link
  
  cmd_append(&cmd, "ld", "-LC:\\Windows\\System32", "-o", "main.exe", "main.obj", "-lkernel32");
  if(!cmd_execute(&cmd, &code, &sb)) {
    fprintf(stderr, "ERROR: Can not execute 'ld'. Make sure its on your PATH\n");
    return 1;
  }

  if(code != 0) {
    return 1;
  }
  
  return 0;
}
