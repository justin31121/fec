cl /W4 main.c && main.exe && nasm -f win64 main.asm -o main.obj && link /ENTRY:main /OUT:main.exe main.obj kernel32.lib && main.exe
