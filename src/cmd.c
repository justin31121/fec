#include "cmd.h"

#include <stdio.h>
#include <string.h>

#include <windows.h>

void cmd_append_impl(Cmd *cmd, ...) {
  (void) cmd;

  va_list argv;
  va_start(argv, cmd);
  
  while(true) {
    char *cstr = va_arg(argv, char *);
    if(!cstr)
      break;

    da_append(cmd, cstr);
  }

  va_end(argv);
}

bool cmd_execute(Cmd *cmd, u8 *exit_code, string_builder *sb) {

  bool result = true;
  
  STARTUPINFO si;
  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  PROCESS_INFORMATION pi;
  memset(&pi, 0, sizeof(pi));

  string_builder _sb = {0};
  char *cstr;
  if(sb) {
    sb->len = 0;
    cstr = cmd_to_cstr(cmd, sb);
  } else {
    cstr = cmd_to_cstr(cmd, &_sb);
  }

  fprintf(stderr, "[CMD] %s\n", cstr); fflush(stderr);
  
  if(!CreateProcess(NULL, cstr, NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi))  {
    printf("CREATE PROCESS FAILED\n"); fflush(stdout);
    return_defer(false);
  }

  WaitForSingleObject(pi.hProcess, INFINITE);
  DWORD code;
  GetExitCodeProcess(pi.hProcess, &code);

  *exit_code = (u8) code;
  
 defer:
  if(_sb.cap) free(_sb.items);

  return result;
}

char *cmd_to_cstr(Cmd *cmd, string_builder *sb) {
  sb->len = 0;  
  for(u64 i=0;i<cmd->len;i++) {
    char *p = cmd->items[i];

    if(strchr(p, ' ')) {
      da_append(sb, '\"');
      da_append_many(sb, p, strlen(p));
      da_append(sb, '\"');
    } else {
      da_append_many(sb, p, strlen(p));
    }
    
    da_append(sb, ' ');
  }  
  da_append(sb, '\0');

  return (char *) sb->items;
}
