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

bool cmd_execute(Cmd *cmd, u8 *exit_code, string_builder *sb, string_builder *stdout_sb, string_builder *stderr_sb) {

  string_builder fallback_sb = {0};
  bool result = true;

  HANDLE stdout_read = INVALID_HANDLE_VALUE;
  HANDLE stdout_write = INVALID_HANDLE_VALUE;
  HANDLE stderr_read = INVALID_HANDLE_VALUE;
  HANDLE stderr_write = INVALID_HANDLE_VALUE;

  SECURITY_ATTRIBUTES attr;
  attr.nLength = sizeof(attr);
  attr.bInheritHandle = TRUE;
  attr.lpSecurityDescriptor = NULL;

  bool use_stdout = stdout_sb != NULL;
  bool use_stderr = stderr_sb != NULL;
 
  STARTUPINFO si;
  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  PROCESS_INFORMATION pi;
  memset(&pi, 0, sizeof(pi));

  if(use_stdout || use_stderr) {
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    si.dwFlags |= STARTF_USESTDHANDLES; 
  }
  
  if(use_stdout) {
    if(!CreatePipe(&stdout_read, &stdout_write, &attr, 0)) {
      return_defer(false);
    }

    if(!SetHandleInformation(stdout_read, HANDLE_FLAG_INHERIT, 0)) {
      return_defer(false);
    }
    si.hStdOutput = stdout_write;
  }

  if(use_stderr) {
    if(!CreatePipe(&stderr_read, &stderr_write, &attr, 0)) {
      return_defer(false);
    }

    if(!SetHandleInformation(stderr_read, HANDLE_FLAG_INHERIT, 0)) {
      return_defer(false);
    }
    si.hStdError = stderr_write;
  }  

  s8 *cstr;
  if(sb) {
    cstr = cmd_to_cstr(cmd, sb);
  } else {
    cstr = cmd_to_cstr(cmd, &fallback_sb);
  }
  
  fprintf(stderr, "[CMD] %s\n", cstr); fflush(stderr);
  
  if(!CreateProcess(NULL, cstr, NULL, NULL,
		    (use_stdout || use_stderr)
		    ? TRUE
		    : FALSE, 0, NULL, NULL, &si, &pi))  {
    return_defer(false);
  }

  //READ
  if(use_stdout || use_stderr) {
    
    CloseHandle(pi.hThread);

    if(use_stdout) {
      CloseHandle(stdout_write);
      stdout_write = INVALID_HANDLE_VALUE;
    }
    if(use_stderr) {
      CloseHandle(stderr_write);
      stderr_write = INVALID_HANDLE_VALUE;
    }    
    
    DWORD read;
    u8 buf[4096];
    bool success = false;

    bool finished[2];
    finished[0] = use_stdout ? false : true;
    finished[1] = use_stderr ? false : true;

    for(;;) {

      if(use_stdout) {	
	success = ReadFile(stdout_read, buf, sizeof(buf), &read, NULL);
	if(!success || read == 0) {
	  finished[0] = true;
	}

	string_builder_append(stdout_sb, buf, read);
      }

      if(use_stderr) {	
	success = ReadFile(stderr_read, buf, sizeof(buf), &read, NULL);
	if(!success || read == 0) {
	  finished[1] = true;
	}

	string_builder_append(stderr_sb, buf, read);
	
      }

      if(finished[0] && finished[1]) {
	break;
      }
    }
  }


  WaitForSingleObject(pi.hProcess, INFINITE);
  DWORD code;
  GetExitCodeProcess(pi.hProcess, &code);

  *exit_code = (u8) code;
  
 defer:
  if(fallback_sb.cap) free(fallback_sb.items);
  if(stdout_read != INVALID_HANDLE_VALUE) CloseHandle(stdout_read);
  if(stdout_write != INVALID_HANDLE_VALUE) CloseHandle(stdout_write);
  if(stderr_read != INVALID_HANDLE_VALUE) CloseHandle(stderr_read);
  if(stderr_write != INVALID_HANDLE_VALUE) CloseHandle(stderr_write);

  return result;
}

char *cmd_to_cstr(Cmd *cmd, string_builder *sb) {
  sb->len = 0;  
  for(u64 i=0;i<cmd->len;i++) {
    char *p = cmd->items[i];

    if(strchr(p, ' ')) {
      string_builder_append(sb, "\"", 1);
      string_builder_append_cstr(sb, p);
      string_builder_append(sb, "\"", 1);
    } else {
      string_builder_append_cstr(sb, p);
    }
    
    if(i != cmd->len-1) {
      string_builder_append(sb, " ", 1); 
    }
  }  
  string_builder_append(sb, "\0", 1);

  return (char *) sb->items;
}
