#include <stdio.h>

#define IO_IMPLEMENTATION
#include "core/io.h"

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

s32 capture() {
  const s8 *dir_path = "tests";
  
  const s8 *src_path = cstrf("%s/src/", dir_path);
  const s8 *exitcode_path = cstrf("%s/exitcode/", dir_path);
  const s8 *stdout_path = cstrf("%s/stdout/", dir_path);
  const s8 *stderr_path = cstrf("%s/stderr/", dir_path);

  if(!io_delete_dir(exitcode_path)) {
    return 1;
  }
  if(!io_create_dir(exitcode_path, NULL)) {
    return 1;
  }

  if(!io_delete_dir(stdout_path)) {
    return 1;
  }
  if(!io_create_dir(stdout_path, NULL)) {    
    return 1;
  }
  
  if(!io_delete_dir(stderr_path)) {
    return 1;
  }
  if(!io_create_dir(stderr_path, NULL)) {
    return 1;
  }

  Io_Dir dir;
  if(!io_dir_open(&dir, src_path)) {
    fprintf(stderr, "ERROR: Can not open directory '%s': (%d) %s",
	    src_path, io_last_error(), io_last_error_cstr());
    return 1;
  }

  Cmd cmd = {0};
  string_builder sb = {0};
  string_builder out = {0};
  u8 exitcode;

  s8 exitcode_buf[MAX_PATH];
  s8 output_path[MAX_PATH];

  Io_Dir_Entry entry;
  while(io_dir_next(&dir, &entry)) {
    
    if(entry.is_dir) {
      continue;
    }

    string s = string_from_cstr(entry.name);
    s32 pos = string_last_index_of(s, ".");
    if(pos >= 0) {
      assert(string_substring(s, 0, pos, &s));
    }
    const char *exe_path = string_to_cstr(s);

    // COMPILE
    sb.len = 0;
    cmd.len = 0;
    out.len = 0;
    cmd_append(&cmd, "fec", "-o", exe_path, entry.abs_name);
    if(!cmd_execute(&cmd, &exitcode, &sb, NULL, &out)) {
      //log error
      return 1;
    }

    if(exitcode != 0) {

      // WRITE STDERR-FILE
      if(out.len) {
	bufsnprintf(output_path, "%s"str_fmt".txt", stderr_path, str_arg(s));
	if(!io_write_file(output_path, out.items, out.len)) {
	  return 1;
	} 	
      }

      continue;
    }

    // EXECUTE
    sb.len = 0;
    cmd.len = 0;
    out.len = 0;
    cmd_append(&cmd, exe_path);
    if(!cmd_execute(&cmd, &exitcode, &sb, &out, NULL)) {
      //log error
      return 1;
    }

    // WRITE EXITCODE-FILE
    bufsnprintf(exitcode_buf, "%u", exitcode);
    bufsnprintf(output_path, "%s"str_fmt".txt", exitcode_path, str_arg(s));
    if(!io_write_file(output_path, (u8 *) exitcode_buf, strlen(exitcode_buf))) {
      return 1;
    }

    // WRITE STDOUT-FILE
    if(out.len) {
      bufsnprintf(output_path, "%s"str_fmt".txt", stdout_path, str_arg(s));
      if(!io_write_file(output_path, out.items, out.len)) {
	return 1;
      }
    }
  }
  
  io_dir_close(&dir);

  return 0;
}

s32 compare() {

  if(true) return 0;
  
  const s8 *dir_path = "tests";
  
  const s8 *src_path = cstrf("%s/src/", dir_path);
  const s8 *exitcode_path = cstrf("%s/exitcode/", dir_path);
  const s8 *stdout_path = cstrf("%s/stdout/", dir_path);
  const s8 *stderr_path = cstrf("%s/stderr/", dir_path);

  Io_Dir dir;
  if(!io_dir_open(&dir, src_path)) {
    fprintf(stderr, "ERROR: Can not open directory '%s': (%d) %s",
	    src_path, io_last_error(), io_last_error_cstr());
    return 1;
  }

  Cmd cmd = {0};
  string_builder sb = {0};
  string_builder out = {0};
  u8 exitcode;

  Io_Dir_Entry entry;
  while(io_dir_next(&dir, &entry)) {
    
    if(entry.is_dir) {
      continue;
    }

    string s = string_from_cstr(entry.name);
    s32 pos = string_last_index_of(s, ".");
    if(pos >= 0) {
      assert(string_substring(s, 0, pos, &s));
    }
    const char *exe_path = string_to_cstr(s);

    // COMPILE
    sb.len = 0;
    cmd.len = 0;
    out.len = 0;
    cmd_append(&cmd, "fec", "-o", exe_path, entry.abs_name);
    if(!cmd_execute(&cmd, &exitcode, &sb, NULL, &out)) {
      //log error
      return 1;
    }
    
  }

  io_dir_close(&dir);
}

s32 main(s32 argc, s8 **argv) {

  const s8 *prog = next(&argc, &argv);
  s8 *arg = next(&argc, &argv);

  if(arg) {
    if(strcmp(arg, "capture") == 0) {
      return capture();
    } else {
      fprintf(stderr, "ERROR: Unknown command '%s'\n", arg);
      fprintf(stderr, "USAGE: %s [capture]\n", prog);
      return 1;
    }
  }

  return compare();
  
  return 0;
}
