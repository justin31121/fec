#include <stdio.h>
#include <windows.h>

void print(DWORD n) {
  char buf[32];
  char *buf_pos = buf + sizeof(buf) - 1;

  if(n == 0) {
    *buf_pos-- = '0';
  } else {
    while(n) {
      *buf_pos-- = n % 10 + '0';
      n /= 10;
    }
  }

  HANDLE hStdout = GetStdHandle(-11);

  DWORD buf_len = sizeof(buf) - (buf_pos - buf) - 1;

  DWORD written;
  WriteFile(hStdout,
	    buf_pos + 1,
	    buf_len,
	    &written,
	    NULL);
}

int main() {

  printf("GENERIC_READ: 0x%x\n", GENERIC_READ);
  printf("FILE_SHARE_READ: 0x%x\n", FILE_SHARE_READ);
  printf("OPEN_EXISTING: 0x%x\n", OPEN_EXISTING);
  printf("FILE_ATTRIBUTE_NORMAL: 0x%x\n", FILE_ATTRIBUTE_NORMAL);
  printf("INVALID_HANDLE_VALUE: 0x%x\n", INVALID_HANDLE_VALUE);

  printf("GENERIC_WRITE: 0x%x\n", GENERIC_WRITE);
  printf("CREATE_ALWAYS: 0x%x\n", CREATE_ALWAYS);
  printf("FILE_ATTRIBUTE_NORMAL: 0x%x\n", FILE_ATTRIBUTE_NORMAL);

  /* HANDLE handle = CreateFileA( */
  /* 			      "foo.txt", */
  /* 			      0x80000000, */
  /* 			      0x1, */
  /* 			      0, */
  /* 			      0x3, */
  /* 			      0x80, */
  /* 			      0); */
  /* if(handle == INVALID_HANDLE_VALUE) { */
  /*   printf("failed\n"); */
  /* } else { */
  /*   printf("succeeded\n"); */
  /* } */

  /* print(69); */

  return 0;
}
