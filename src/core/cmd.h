#ifndef CMD_H
#define CMD_H

#include "common.h"
#include "string.h"
#include <stdarg.h>

typedef struct{
  s8 **items;
  u64 len;
  u64 cap;
}Cmd;

void cmd_append_impl(Cmd *cmd, ...);
bool cmd_execute(Cmd *cmd, u8 *exit_code, string_builder *sb, string_builder *stdout_sb, string_builder *stderr_sb);
char *cmd_to_cstr(Cmd *cmd, string_builder *sb);

#define cmd_append(cmd, ...) cmd_append_impl(cmd, __VA_ARGS__, NULL)

#endif //CMD_H
