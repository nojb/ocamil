/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2001 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: printexc.h,v 1.3 2001/12/07 13:39:34 xleroy Exp $ */

#ifndef _printexc_
#define _printexc_


#include "misc.h"
#include "mlvalues.h"

CAMLextern char * format_caml_exception (value);
void fatal_uncaught_exception (value) Noreturn;


#endif /* _printexc_ */
