cl /Fe:main.exe /Zi /W4 main.c && main && nasm -f win64 foo.asm -o foo.obj && link /ENTRY:main /OUT:foo.exe /DRIVER:UPONLY /ALIGN:16 foo.obj kernel32.lib
::gcc -o main main.c && main && nasm -f win64 main.asm -o main.obj && ld -e main -o main.exe main.obj -lkernel32 && main.exe
