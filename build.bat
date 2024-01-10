cl /Fe:main.exe /Zi /W4 main.c && main && nasm -f win64 main.asm -o main.obj && link /ENTRY:main /OUT:main.exe /DRIVER:UPONLY /ALIGN:16 main.obj kernel32.lib
::gcc -o main main.c && main && nasm -f win64 main.asm -o main.obj && ld -e main -o main.exe main.obj -lkernel32 && main.exe
