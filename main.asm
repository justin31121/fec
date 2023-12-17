	global main

	;; https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-exitprocess
	extern ExitProcess

	;; https://learn.microsoft.com/en-us/windows/win32/api/errhandlingapi/nf-errhandlingapi-getlasterror
	;; https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
	extern GetLastError	
	
	;; https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
	extern CreateFileA

	;; https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-getfilesize
	extern GetFileSize

	;; https://learn.microsoft.com/en-us/windows/win32/api/handleapi/nf-handleapi-closehandle
	extern CloseHandle

	;; https://learn.microsoft.com/en-us/windows/win32/api/heapapi/nf-heapapi-getprocessheap
	extern GetProcessHeap

	;; https://learn.microsoft.com/en-us/windows/win32/api/heapapi/nf-heapapi-heapalloc
	extern HeapAlloc

	;; https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-readfile
	extern ReadFile

	;; https://learn.microsoft.com/en-us/windows/console/getstdhandle
	extern GetStdHandle

	;; https://learn.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-writefile
	extern WriteFile

	;; https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-formatmessagea
	extern FormatMessageA

	;; 
	;;              ^
	;;              |
	;; 
	%define GENERIC_READ 0x80000000
	%define FILE_SHARE_READ 0x01
	%define OPEN_EXISTING 0x3
	%define FILE_ATTRIBUTE_NORMAL 0x80
	%define INVALID_HANDLE_VALUE -1
	%define INVALID_FILE_SIZE -1
	%define STD_OUTPUT_HANDLE -11
	%define FORMAT_MESSAGE_ALLOCATE_BUFFER 0x00000100
	%define FORMAT_MESSAGE_FROM_SYSTEM 0x00001000

	section .bss
written:	resq 1

	section .data
filepath:	dq "main.asm",0

	section .text
main:
	;; BUILD:
	;;     nasm -f win64 main.asm -o main.obj
	;;     link /ENTRY:main /OUT:main.exe main.obj kernel32.lib
	
	;; TODO:
	;;     heap_handle is leaked
	;;     accept cmd-line argument
	;;     format and print error-message
	;;     get rid of 'written' in .bss
	
	;; handle = CreateFileA(filepath,
	;;	       GENERIC_READ,
	;;	       FILE_SHARE_READ,
	;;             0,
	;;             OPEN_EXISTING,
	;;             FILE_ATTRIBUTE_NORMAL,
	;;             NULL);
	mov rcx, filepath
	mov qword rdx, GENERIC_READ
	mov qword r8, FILE_SHARE_READ
	mov qword r9, 0
	push qword 0
	push qword FILE_ATTRIBUTE_NORMAL
	push qword OPEN_EXISTING
	sub rsp, 32
	call CreateFileA
	add rsp, 56
	push qword rax		; PUSH handle
	;; if(handle == INVALID_HANDLE_VALUE) goto .error;
	mov qword rcx, INVALID_HANDLE_VALUE
	cmp rcx, rax
	je .error	

	;; size = GetFileSize(handle);
	mov rcx, rax
	sub rsp, 40
	call GetFileSize
	add rsp, 40
	push qword rax	 	; PUSH size
	;; if(size == INVALID_FILE_SIZE) goto .error;
	mov qword rcx, INVALID_FILE_SIZE
	cmp rcx, rax
	je .error

	;; heap_handle = GetProcessHeap();
	sub rsp, 40
	call GetProcessHeap
	add rsp, 40
	;; if(heap_handle == NULL) goto .error;
	test rax, rax
	jz .error

	;; space = HeapAlloc(heap_handle, 0, size);
	mov rcx, rax
	mov qword rdx, 0
	pop qword r8
	push qword r8
	sub rsp, 40
	call HeapAlloc
	add rsp, 40
	;; if(space == NULL) goto .error;
	test rax, rax
	jz .error

	;; stack:
	;;     size
	;;     handle
	;; reg:
	;;     rax: space
	;;     ...

	;; result = ReadFile(handle, space, size, &written, NULL);
	pop qword r8
	pop qword rcx
	push qword rcx		; PUSH handle
	push qword r8		; PUSH size
	mov rdx, rax
	push qword rdx		; PUSH space
	mov r9, written
	push qword 0
	sub rsp, 32
	call ReadFile
	add rsp, 40
	;; if(result == 0/False) goto .error;
	test rax, rax
	jz .error

	;; hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
	mov rcx, STD_OUTPUT_HANDLE
	sub rsp, 40
	call GetStdHandle
	add rsp, 40
	;; if(hStdout == INVALID_HANDLE_VALUE) goto .error;
	mov qword rcx, INVALID_HANDLE_VALUE
	cmp rcx, rax
	je .error

	;; stack:
	;;     space
	;;     size
	;;     handle
	;; reg:
	;;     rax: hStdout

	;; result = WriteFile(hStdout, space, size, &written, NULL);
	pop qword rdx
	pop qword r8
	mov rcx, rax
	mov r9, written
	push qword 0
	sub rsp, 32
	call WriteFile
	add rsp, 40
	;; if(result == 0/False) goto .error;
	test rax, rax
	jz .error

	;; result = CloseHandle(handle);
	pop qword rcx
	sub rsp, 40
	call CloseHandle
	add rsp, 40
	;; if(result == 0/False) goto .error;
	test rax, rax
	jz .error

	;; ExitProcess(0);
	mov qword rcx, 0
	sub rsp, 40
	call ExitProcess
	
.error:
	
	;; Exit(GetLastError());
	sub rsp, 40
	call GetLastError
	;; add rsp, 40
	
	mov rcx, rax
	;; sub rsp, 40
	call ExitProcess
