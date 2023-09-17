gcc -Wall -Wextra -pedantic -ggdb -o fec src\main.c src\core\*.c
gcc -Wall -Wextra -pedantic -ggdb -o test src\test.c src\core\cmd.c src\core\common.c src\core\string.c
::cl /W4 /Fe:fec src\*.c
