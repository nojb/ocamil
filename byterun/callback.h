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

/* $Id: callback.h,v 1.8 2001/12/07 13:39:22 xleroy Exp $ */

/* Callbacks from C to Caml */

#ifndef _callback_
#define _callback_

#include "mlvalues.h"

CAMLextern value callback (value closure, value arg);
CAMLextern value callback2 (value closure, value arg1, value arg2);
CAMLextern value callback3 (value closure, value arg1, value arg2, value arg3);
CAMLextern value callbackN (value closure, int narg, value args[]);

CAMLextern value callback_exn (value closure, value arg);
CAMLextern value callback2_exn (value closure, value arg1, value arg2);
CAMLextern value callback3_exn (value closure,
                                value arg1, value arg2, value arg3);
CAMLextern value callbackN_exn (value closure, int narg, value args[]);

#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)

CAMLextern char * format_caml_exception(value exn); /* in [printexc.c] */

CAMLextern value * caml_named_value (char * name);

CAMLextern void caml_main (char ** argv);
CAMLextern void caml_startup (char ** argv);

CAMLextern int callback_depth;

#endif
