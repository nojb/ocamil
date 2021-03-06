/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Unix-specific stuff */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "config.h"
#ifdef SUPPORT_DYNAMIC_LINKING
#include <dlfcn.h>
#endif
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include "memory.h"
#include "misc.h"
#include "osdeps.h"

#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif

char * decompose_path(struct ext_table * tbl, char * path)
{
  char * p, * q;
  int n;

  if (path == NULL) return NULL;
  p = stat_alloc(strlen(path) + 1);
  strcpy(p, path);
  q = p;
  while (1) {
    for (n = 0; q[n] != 0 && q[n] != ':'; n++) /*nothing*/;
    ext_table_add(tbl, q);
    q = q + n;
    if (*q == 0) break;
    *q = 0;
    q += 1;
  }
  return p;
}

char * search_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;
  struct stat st;

  for (p = name; *p != 0; p++) {
    if (*p == '/') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = stat_alloc(strlen((char *)(path->contents[i])) +
                          strlen(name) + 2);
    strcpy(fullname, (char *)(path->contents[i]));
    if (fullname[0] != 0) strcat(fullname, "/");
    strcat(fullname, name);
    if (stat(fullname, &st) == 0 && S_ISREG(st.st_mode)) return fullname;
    stat_free(fullname);
  }
 not_found:
  fullname = stat_alloc(strlen(name) + 1);
  strcpy(fullname, name);
  return fullname;
}
  
#ifdef __CYGWIN32__

/* Cygwin needs special treatment because of the implicit ".exe" at the
   end of executable file names */

static int cygwin_file_exists(char * name)
{
  int fd;
  /* Cannot use stat() here because it adds ".exe" implicitly */
  fd = open(name, O_RDONLY);
  if (fd == -1) return 0;
  close(fd);
  return 1;
}

static char * cygwin_search_exe_in_path(struct ext_table * path, char * name)
{
  char * p, * fullname;
  int i;

  for (p = name; *p != 0; p++) {
    if (*p == '/' || *p == '\\') goto not_found;
  }
  for (i = 0; i < path->size; i++) {
    fullname = stat_alloc(strlen((char *)(path->contents[i])) +
                          strlen(name) + 6);
    strcpy(fullname, (char *)(path->contents[i]));
    strcat(fullname, "/");
    strcat(fullname, name);
    if (cygwin_file_exists(fullname)) return fullname;
    strcat(fullname, ".exe");
    if (cygwin_file_exists(fullname)) return fullname;
    stat_free(fullname);
  }
 not_found:
  fullname = stat_alloc(strlen(name) + 5);
  strcpy(fullname, name);
  if (cygwin_file_exists(fullname)) return fullname;
  strcat(fullname, ".exe");
  if (cygwin_file_exists(fullname)) return fullname;
  strcpy(fullname, name);
  return fullname;
}
  
#endif

char * search_exe_in_path(char * name)
{
  struct ext_table path;
  char * tofree;
  char * res;

  ext_table_init(&path, 8);
  tofree = decompose_path(&path, getenv("PATH"));
#ifndef __CYGWIN32__
  res = search_in_path(&path, name);
#else
  res = cygwin_search_exe_in_path(&path, name);
#endif
  stat_free(tofree);
  ext_table_free(&path, 0);
  return res;
}

char * search_dll_in_path(struct ext_table * path, char * name)
{
  char * dllname = stat_alloc(strlen(name) + 4);
  char * res;
  strcpy(dllname, name);
  strcat(dllname, ".so");
  res = search_in_path(path, dllname);
  stat_free(dllname);
  return res;
}

#ifdef SUPPORT_DYNAMIC_LINKING

#ifndef RTLD_GLOBAL
#define RTLD_GLOBAL 0
#endif
#ifndef RTLD_NODELETE
#define RTLD_NODELETE 0
#endif

void * caml_dlopen(char * libname)
{
  return dlopen(libname, RTLD_NOW|RTLD_GLOBAL|RTLD_NODELETE);
}

void caml_dlclose(void * handle)
{
  dlclose(handle);
}

void * caml_dlsym(void * handle, char * name)
{
  return dlsym(handle, name);
}

char * caml_dlerror(void)
{
  return dlerror();
}

#else

void * caml_dlopen(char * libname)
{
  return NULL;
}

void caml_dlclose(void * handle)
{
}

void * caml_dlsym(void * handle, char * name)
{
  return NULL;
}

char * caml_dlerror(void)
{
  return "dynamic loading not supported on this platform";
}

#endif

#ifdef USE_MMAP_INSTEAD_OF_MALLOC

/* The code below supports the use of mmap() rather than malloc()
   for allocating the chunks composing the major heap.
   This code is needed for the IA64 under Linux, where the native
   malloc() implementation can return pointers several *exabytes* apart,
   (some coming from mmap(), other from sbrk()); this makes the
   page table *way* too large.
   No other tested platform requires this hack so far.  However, it could
   be useful for other 64-bit platforms in the future. */

#include <sys/mman.h>

char *aligned_mmap (asize_t size, int modulo, void **block)
{
  char *raw_mem;
  unsigned long aligned_mem;
  Assert (modulo < Page_size);
  raw_mem = (char *) mmap(NULL, size + Page_size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (raw_mem == MAP_FAILED) return NULL;
  *block = raw_mem;
  raw_mem += modulo;                /* Address to be aligned */
  aligned_mem = (((unsigned long) raw_mem / Page_size + 1) * Page_size);
#ifdef DEBUG
  {
    unsigned long *p;
    unsigned long *p0 = (void *) *block,
                  *p1 = (void *) (aligned_mem - modulo),
                  *p2 = (void *) (aligned_mem - modulo + size),
                  *p3 = (void *) ((char *) *block + size + Page_size);

    for (p = p0; p < p1; p++) *p = Debug_filler_align;
    for (p = p1; p < p2; p++) *p = Debug_uninit_align;
    for (p = p2; p < p3; p++) *p = Debug_filler_align;
  }
#endif
  return (char *) (aligned_mem - modulo);
}

void aligned_munmap (char * addr, asize_t size)
{
  int retcode = munmap (addr, size + Page_size);
  Assert(retcode == 0);
}

#endif
