::gcc -Wall -Wextra -pedantic -ggdb -o fec src\main.c src\core\*.c
::gcc -Wall -Wextra -pedantic -ggdb -o test src\test.c src\core\*.c
cl /W4 /Fe:fec src\main.c src\core\*.c
cl /W4 /Fe:test src\test.c src\core\*.c
