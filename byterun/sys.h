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

/* $Id: sys.h,v 1.11 2002/02/11 13:51:40 xleroy Exp $ */

#ifndef _sys_
#define _sys_

#include "misc.h"

#define NO_ARG Val_int(0)

CAMLextern void sys_error (value);
extern void sys_init (char * exe_name, char ** argv);
CAMLextern value sys_exit (value);

extern char * caml_exe_name;

#endif /* _sys_ */
