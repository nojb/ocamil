/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: printexc.c,v 1.13 2001/12/07 13:39:34 xleroy Exp $ */

/* Print an uncaught exception and abort */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "backtrace.h"
#include "callback.h"
#include "debugger.h"
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#ifdef HAS_UI
#include "ui.h"
#endif
#include "printexc.h"

struct stringbuf {
  char * ptr;
  char * end;
  char data[256];
};

static void add_char(struct stringbuf *buf, char c)
{
  if (buf->ptr < buf->end) *(buf->ptr++) = c;
}

static void add_string(struct stringbuf *buf, char *s)
{
  int len = strlen(s);
  if (buf->ptr + len > buf->end) len = buf->end - buf->ptr;
  if (len > 0) memmove(buf->ptr, s, len);
  buf->ptr += len;
}
  
CAMLexport char * format_caml_exception(value exn)
{
  mlsize_t start, i;
  value bucket, v;
  struct stringbuf buf;
  char intbuf[64];
  char * res;

  buf.ptr = buf.data;
  buf.end = buf.data + sizeof(buf.data) - 1;
  add_string(&buf, String_val(Field(Field(exn, 0), 0)));
  if (Wosize_val(exn) >= 2) {
    /* Check for exceptions in the style of Match_failure and Assert_failure */
    if (Wosize_val(exn) == 2 &&
        Is_block(Field(exn, 1)) &&
        Tag_val(Field(exn, 1)) == 0) {
      bucket = Field(exn, 1);
      start = 0;
    } else {
      bucket = exn;
      start = 1;
    }
    add_char(&buf, '(');
    for (i = start; i < Wosize_val(bucket); i++) {
      if (i > start) add_string(&buf, ", ");
      v = Field(bucket, i);
      if (Is_long(v)) {
        sprintf(intbuf, "%ld", Long_val(v));
        add_string(&buf, intbuf);
      } else if (Tag_val(v) == String_tag) {
        add_char(&buf, '"');
        add_string(&buf, String_val(v));
        add_char(&buf, '"');
      } else {
        add_char(&buf, '_');
      }
    }
    add_char(&buf, ')');
  }
  *buf.ptr = 0;              /* Terminate string */
  i = buf.ptr - buf.data + 1;
  res = malloc(i);
  if (res == NULL) return NULL;
  memmove(res, buf.data, i);
  return res;
}


void fatal_uncaught_exception(value exn)
{
  char * msg;
  value * at_exit;
#ifndef NATIVE_CODE
  int saved_backtrace_active, saved_backtrace_pos;
#endif
  /* Build a string representation of the exception */
  msg = format_caml_exception(exn);
  /* Perform "at_exit" processing, ignoring all exceptions that may
     be triggered by this */
#ifndef NATIVE_CODE
  saved_backtrace_active = backtrace_active;
  saved_backtrace_pos = backtrace_pos;
  backtrace_active = 0;
#endif
  at_exit = caml_named_value("Pervasives.do_at_exit");
  if (at_exit != NULL) callback_exn(*at_exit, Val_unit);
#ifndef NATIVE_CODE
  backtrace_active = saved_backtrace_active;
  backtrace_pos = saved_backtrace_pos;
#endif
  /* Display the uncaught exception */
#ifdef HAS_UI
  ui_print_stderr("Fatal error: exception %s\n", msg);
#else
  fprintf(stderr, "Fatal error: exception %s\n", msg);
#endif
  free(msg);
  /* Display the backtrace if available */
#ifndef NATIVE_CODE
  if (backtrace_active && !debugger_in_use) print_exception_backtrace();
#endif
  /* Terminate the process */
#ifdef HAS_UI
  ui_exit(2);
#else
  exit(2);
#endif
}
