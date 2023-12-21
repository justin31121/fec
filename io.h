#ifndef IO_H
#define IO_H

#include <stdio.h>
#include <stdbool.h>

#ifdef _WIN32
#  include <windows.h>
#  define IO_MAX_PATH MAX_PATH
#  define IO_MAX_ENV 32768
#else
#  include <stdlib.h>
#  include <errno.h>
#  include <string.h>
#  include <fcntl.h>
#  include <unistd.h>
#  include <sys/stat.h>
#  include <dirent.h>
#  include <linux/limits.h>
#  define IO_MAX_PATH PATH_MAX
#endif //_WIN32

#ifndef IO_DEF
#  define IO_DEF static inline
#endif //IO_DEF

#ifndef IO_LOG
#  ifndef IO_QUIET
#    define IO_LOG(fmt, ...) fprintf(stderr, "IO_LOG: "fmt"\n", __VA_ARGS__)
#  else 
#    define IO_LOG(...)
#  endif // IO_VERBOSE
#endif //IO_LOG

////////////////////////////////////////////////////////////////////////////////////////

// Io_Util

typedef bool (*Io_Stream_Callback)(void *userdata, const unsigned char *buf, size_t buf_size);

IO_DEF bool io_slurp_file(const char *filepath, unsigned char **data, size_t *data_size);
IO_DEF bool io_write_file(const char *filepath, unsigned char *data, size_t data_size);
IO_DEF bool io_delete_file(const char *filepath);
IO_DEF bool io_stream_file(const char *filepath, Io_Stream_Callback callback, unsigned char *buf, size_t buf_size, void *userdata);

IO_DEF bool io_create_dir(const char *dir_path, bool *existed);
IO_DEF bool io_delete_dir(const char *dir_path);

IO_DEF bool io_exists(const char *file_path, bool *is_file);

IO_DEF bool io_getenv(const char *name, char *buffer, size_t buffer_cap, size_t *buffer_len);

////////////////////////////////////////////////////////////////////////////////////////

// Io_Dir

typedef struct{
#ifdef _WIN32
    WIN32_FIND_DATAW file_data;
    HANDLE handle;
    bool stop;
#else
    struct dirent *ent;
    DIR *handle;
    struct stat stats;
#endif //_WIN32

    const char *name;
}Io_Dir;

typedef struct{
  char abs_name[IO_MAX_PATH];
  char *name;
  bool is_dir;  
}Io_Dir_Entry;

IO_DEF bool io_dir_open(Io_Dir *dir, const char *dir_path);
IO_DEF bool io_dir_next(Io_Dir *dir, Io_Dir_Entry *entry);
IO_DEF void io_dir_close(Io_Dir *dir);

////////////////////////////////////////////////////////////////////////////////////////

// Io_File

typedef enum{
  IO_MODE_READ = 0,
  IO_MODE_WRITE,
  COUNT_IO_MODE,
}Io_Mode;

#ifdef _WIN32
typedef struct{ HANDLE handle; DWORD size; DWORD pos; }Io_File;
#else
typedef struct{ int fd; size_t size; size_t pos; }Io_File;
#endif //_WIN32

IO_DEF bool io_file_open(Io_File *f, const char *filepath, Io_Mode mode);
IO_DEF int io_file_seek(Io_File *f, long int offset, int whence);
IO_DEF long int io_file_tell(Io_File *f);
IO_DEF size_t io_file_read(Io_File *f, void *ptr, size_t size, size_t count);
IO_DEF size_t io_file_write(Io_File *f, const void *ptr, size_t size, size_t nmemb);
IO_DEF void io_file_close(Io_File *f);

////////////////////////////////////////////////////////////////////////////////////////

IO_DEF int io_last_error();
IO_DEF const char *io_last_error_cstr();

#ifdef IO_IMPLEMENTATION

#define io_file_write_cstr(f, cstr) io_file_write((f), (cstr), 1, (strlen(cstr)))

IO_DEF bool io_slurp_file(const char *filepath, unsigned char **data, size_t *data_size) {
  Io_File f;
  if(!io_file_open(&f, filepath, IO_MODE_READ)) {
    IO_LOG("Failed to open '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;
  }

  *data_size = f.size;
  
  unsigned char *result = malloc(*data_size);
  if(!result) {
    io_file_close(&f);
    IO_LOG("Failed to allocate enough memory. Tried to allocate: %zu bytes", *data_size);
    return false;
  }

  if(*data_size != io_file_read(&f, result, 1, *data_size)) {
    io_file_close(&f);
    IO_LOG("Failed to read: '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;
  }

  *data = result;
  io_file_close(&f);

  return true;
}

IO_DEF bool io_write_file(const char *filepath, unsigned char *data, size_t data_size) {
  Io_File f;
  if(!io_file_open(&f, filepath, IO_MODE_WRITE)) {
    IO_LOG("Failed to open '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;
  }

  if(data_size != io_file_write(&f, data, 1, data_size)) {
    io_file_close(&f);
    IO_LOG("Failed to write '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;    
  }

  io_file_close(&f);
  return true;
}

IO_DEF bool io_delete_file(const char *filepath) {
#ifdef _WIN32
  if(!DeleteFile(filepath)) {
    IO_LOG("Failed to delete file '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;
  }

  return true;
#else

  if(remove(filepath) != 0) {
    IO_LOG("Failed to delete file '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;
  }
  
  return true;
#endif // _WIN32
}

IO_DEF bool io_stream_file(const char *filepath, Io_Stream_Callback callback, unsigned char *buf, size_t buf_size, void *userdata) {
  Io_File f;
  if(!io_file_open(&f, filepath, IO_MODE_READ)) {
    IO_LOG("Failed to open '%s': (%d) %s",
	   filepath, io_last_error(), io_last_error_cstr());
    return false;
  }

  while(true) {
    size_t read = io_file_read(&f, buf, 1, buf_size);
    if(read == 0) {
      break;
    }

    if(!callback(userdata, buf, read)) {
      io_file_close(&f);
      return false;
    }    
  }

  io_file_close(&f);
  return true;
}

IO_DEF bool io_create_dir(const char *dir_path, bool *_existed) {
#ifdef _WIN32
  if(CreateDirectory(dir_path, NULL)) {
    if(_existed) *_existed = false;
    return true;
  } else {
    bool existed = io_last_error() == ERROR_ALREADY_EXISTS;
    if(!existed) {
      IO_LOG("Failed to create direcory '%s': (%d) %s",
	     dir_path, io_last_error(), io_last_error_cstr());
      return false;
    }
  
    if(_existed) *_existed = existed;    
    return true;    
  }
#else

  if(mkdir(dir_path, S_IRWXU | S_IRWXG | S_IRWXO) == 0) {
    if(_existed) *_existed = false;
    return true;
  } else {
    bool existed = errno == EEXIST;
    if(!existed) {
      IO_LOG("Failed to create direcory '%s': (%d) %s",
	     dir_path, io_last_error(), io_last_error_cstr());
      return false;
    }

    if(_existed) *_existed = existed;
    return true;
  }
  

#endif // _WIN32
}

IO_DEF bool io_delete_dir(const char *dir_path) {
  
  Io_Dir dir;
  if(!io_dir_open(&dir, dir_path)) {
    return true;
  }

  Io_Dir_Entry entry;
  while(io_dir_next(&dir, &entry)) {

    if(strncmp(entry.name, ".", 1) == 0 ||
       strncmp(entry.name, "..", 2) == 0) {
      continue;
    }
    
    if(entry.is_dir) {
      if(!io_delete_dir(entry.abs_name)) {
	io_dir_close(&dir);
	return false;
      }
    } else {
      if(!io_delete_file(entry.abs_name)) {
	io_dir_close(&dir);
	IO_LOG("Failed to delete directory '%s': Can not delete file: '%s'", dir_path, entry.name);
	return false;
      }
    }
  }

  io_dir_close(&dir);

#ifdef _WIN32
  if(!RemoveDirectory(dir_path)) {
    IO_LOG("Failed to remove direcory '%s': (%d) %s",
	   dir_path, io_last_error(), io_last_error_cstr()); 
    return false;
  }
#else
  if(remove(dir_path) != 0) {
    IO_LOG("Failed to delete file '%s': (%d) %s",
	   dir_path, io_last_error(), io_last_error_cstr());
    return false;
  }
#endif // _WIN32
  
  return true;
}

IO_DEF bool io_exists(const char *file_path, bool *is_file) {
#ifdef _WIN32
  DWORD attribs = GetFileAttributes(file_path);
  if(is_file) *is_file = !(attribs & FILE_ATTRIBUTE_DIRECTORY);
  return attribs != INVALID_FILE_ATTRIBUTES;
#else
  int result = access(file_path, F_OK);
  if(result < 0) {
    return false;
  }

  if(is_file) {
    struct stat path_stat;
    stat(file_path, &path_stat);
    *is_file = S_ISREG(path_stat.st_mode) != 0;
  }

  return true;
#endif // _WIN32
}

IO_DEF bool io_getenv(const char *name, char *buffer, size_t buffer_cap, size_t *buffer_len) {
#ifdef _WIN32
  if(buffer_len) *buffer_len = 0;
  
  DWORD size = GetEnvironmentVariable(name, buffer, (DWORD) buffer_cap);
  if(size == 0) {
    IO_LOG("Environment-Variable: '%s' does not exist", name);
    return false;
  }

  if(buffer_len) *buffer_len = size;

  if(size > buffer_cap) {
    IO_LOG("Environment-Variable: '%s' does not fit into %llu chars. %lu needed", name, buffer_cap, size);
    return false;
  }

  return true;
#else
  if(buffer_len) *buffer_len = 0;

  char *variable = getenv(name);
  if(!variable) {
    IO_LOG("Environment-Variable: '%s' does not exist", name);
    return false;
  }

  size_t variable_len = strlen(variable) + 1;
  if(variable_len > buffer_cap) {
    IO_LOG("Environment-Variable: '%s' does not fit into %zu chars. %zu needed", name, buffer_cap, variable_len);
    return false;    
  }

  memcpy(buffer, variable, variable_len);
  
  return true;
#endif // _WIN32
}

////////////////////////////////////////////////////////////////////////////////////////

IO_DEF bool io_dir_open(Io_Dir *dir, const char *dir_path) {
#ifdef _WIN32
  int num_wchars = MultiByteToWideChar(CP_UTF8, 0, dir_path, -1, NULL, 0); 
  wchar_t *my_wstring = (wchar_t *)malloc((num_wchars+1) * sizeof(wchar_t));
  MultiByteToWideChar(CP_UTF8, 0, dir_path, -1, my_wstring, num_wchars);
  my_wstring[num_wchars-1] = '*';
  my_wstring[num_wchars] = 0;

  // Use my_wstring as a const wchar_t *
  dir->handle = FindFirstFileExW(my_wstring, FindExInfoStandard, &dir->file_data, FindExSearchNameMatch, NULL, 0);
  if(dir->handle == INVALID_HANDLE_VALUE) {
    free(my_wstring);
    return false;
  }

  bool is_dir = (dir->file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) > 0;
  if(!is_dir) {
    free(my_wstring);
    return false;
  }

  dir->name = dir_path;
  dir->stop = false;

  free(my_wstring);
  return true;
#else

  dir->name = dir_path;
  dir->handle = opendir(dir_path);
  if(dir->handle == NULL) {
    return false;
  }
  
  return true;
#endif //_WIN32
}

IO_DEF bool io_dir_next(Io_Dir *dir, Io_Dir_Entry *entry) {
#ifdef _WIN32
  if(dir->stop) {
    return false;
  }

  entry->is_dir = (dir->file_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) > 0;

  size_t len = strlen(dir->name);
  memcpy(entry->abs_name, dir->name, len);
  int len2 = WideCharToMultiByte(CP_ACP, 0, dir->file_data.cFileName, -1, NULL, 0, NULL, NULL);
  WideCharToMultiByte(CP_ACP, 0, dir->file_data.cFileName, -1, entry->abs_name + len, len2, NULL, NULL);

  //WHAT IS THIS
  if(entry->is_dir) {
    entry->abs_name[len + len2-1] = '/';
    entry->abs_name[len + len2] = 0;       
  } else {
    entry->abs_name[len + len2-1] = 0;
  }

  entry->name = (char *) &entry->abs_name[len];

  if(FindNextFileW(dir->handle, &dir->file_data) == 0) {
    dir->stop = true;
  }

  return true;
#else

  dir->ent = readdir(dir->handle);
  if(dir->ent == NULL) {
    return false;
  }

  size_t d_name_len = strlen(dir->ent->d_name);
  entry->name = dir->ent->d_name;

  size_t dir_name_len = strlen(dir->name);
  memcpy(entry->abs_name, dir->name, dir_name_len);
  entry->abs_name[dir_name_len++] = '/';
  
  memcpy(entry->abs_name + dir_name_len, dir->ent->d_name, d_name_len);
  dir_name_len += d_name_len;
  entry->abs_name[dir_name_len] = '\0';

  if(stat(entry->abs_name, &dir->stats) < 0) {
    return false;
  }
  entry->is_dir = S_ISDIR(dir->stats.st_mode);

  return true;
#endif //_WIN32
}

IO_DEF void io_dir_close(Io_Dir *dir) {
#ifdef _WIN32
  FindClose(dir->handle);
#else
  closedir(dir->handle);
#endif //_WIN32
}

////////////////////////////////////////////////////////////////////////////////////////

IO_DEF bool io_file_open(Io_File *f, const char *filepath, Io_Mode mode) {
#ifdef _WIN32
  if(mode < 0 || COUNT_IO_MODE <= mode)
    return false;

  f->handle = INVALID_HANDLE_VALUE;

  if(mode == IO_MODE_READ) {
    f->handle = CreateFile(filepath,
			   GENERIC_READ,
			   FILE_SHARE_READ,
			   NULL,
			   OPEN_EXISTING,
			   FILE_ATTRIBUTE_NORMAL,
			   NULL);
    if(f->handle == INVALID_HANDLE_VALUE)
      goto error;

    f->size = GetFileSize(f->handle, NULL);
    if(f->size == INVALID_FILE_SIZE)
      goto error;

    f->pos = 0;
  } else {
    f->handle = CreateFile(filepath,
			   GENERIC_WRITE, 0, NULL,
			   CREATE_ALWAYS,
			   FILE_ATTRIBUTE_NORMAL,
			   NULL);
    if(f->handle == INVALID_HANDLE_VALUE)
      goto error;
    
    f->pos = 0;
    f->size = 0;
  }
  
  return true;
 error:

  if(f->handle != INVALID_HANDLE_VALUE)
    CloseHandle(f->handle);
  return false;
#else
  
  if(mode < 0 || COUNT_IO_MODE <= mode)
    return false;

  f->fd = -1;

  if(mode == IO_MODE_READ) {
    f->fd = open(filepath, O_RDONLY);
    if(f->fd < 0)
      goto error;

    struct stat stats;
    if(stat(filepath, &stats) < 0)
      goto error;

    f->size = (size_t) stats.st_size;
    f->pos  = 0;
  } else {

    f->fd = open(filepath, O_CREAT|O_WRONLY|O_TRUNC, S_IRWXU | S_IRWXG | S_IRWXO);
    if(f->fd < 0)
      goto error;

    f->size = 0;
    f->pos  = 0;
  }

  return true;
 error:
  if(f->fd >= 0) close(f->fd);
  
  return false;
#endif //_WIN32  
}

IO_DEF int io_file_seek(Io_File *f, long int offset, int whence) {
#ifdef _WIN32
  DWORD moveMethod;

  switch (whence) {
  case SEEK_SET: {
    moveMethod = FILE_BEGIN;    
  } break;
  case SEEK_CUR: {
    moveMethod = FILE_CURRENT;    
  } break;
  case SEEK_END: {
    moveMethod = FILE_END;    
  } break;
  default: {
    return -1;  // Invalid whence
  } break;
  }

  f->pos = SetFilePointer(f->handle, offset, NULL, moveMethod);
  if(f->pos == INVALID_SET_FILE_POINTER)
    return -1;

  return 0;
#else
  f->pos = (size_t) lseek(f->fd, offset, whence);
  
  return (int) f->pos;
#endif //_WIN32
}

IO_DEF long int io_file_tell(Io_File *f) {
  return (long int) f->pos;
}

IO_DEF void io_file_close(Io_File *f) {
#ifdef _WIN32
  CloseHandle(f->handle);
#else
  close(f->fd);
#endif //_WIN32
}


IO_DEF size_t io_file_read(Io_File *f, void *ptr, size_t size, size_t count) {
#ifdef _WIN32
  DWORD bytes_read;
  DWORD bytes_to_read = (DWORD) (size * count);

  if(!ReadFile(f->handle, ptr, bytes_to_read, &bytes_read, NULL))
    return 0;
  f->pos += bytes_read;

  return (size_t) (bytes_read / size);
#else

  size_t bytes_to_read = size * count;
  ssize_t bytes_read = read(f->fd, ptr, bytes_to_read);
  if(bytes_read < 0) {
    return 0;
  }

  return bytes_read / size;
#endif //_WIN32
}

IO_DEF size_t io_file_write(Io_File *f, const void *ptr, size_t size, size_t nmemb) {
#ifdef _WIN32
  DWORD bytes_written;
  DWORD bytes_to_write = (DWORD) (size * nmemb);
  
  if(!WriteFile(f->handle, ptr, bytes_to_write, &bytes_written, NULL)) {
    return 0;
  }

  f->pos += bytes_to_write;
  f->size += bytes_to_write;
  
  return (size_t) (bytes_written / size);
#else

  size_t bytes_to_write = size * nmemb;  
  ssize_t bytes_written = write(f->fd, ptr, bytes_to_write);
  if(bytes_written < 0) {
    return 0;
  }

  return bytes_written / size;
  
#endif //_WIN32
}

////////////////////////////////////////////////////////////////////////////////////////

IO_DEF int io_last_error() {
#ifdef _WIN32
  return GetLastError();
#else
  return errno;
#endif //_WIN32
}

IO_DEF const char *io_last_error_cstr() {
#ifdef _WIN32
  DWORD error = GetLastError();
  static char buffer[1024];

  FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM,
		 NULL,
		 error,
		 MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		 (LPSTR) &buffer,
		 sizeof(buffer),
		 NULL);
  
  return buffer;
#else
  return strerror(errno);
#endif //_WIN32
}

#endif //IO_IMPLEMENTATION

#endif //IO_H
