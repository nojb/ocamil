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

/* $Id: interp.h,v 1.8 2001/12/07 13:39:30 xleroy Exp $ */

/* The bytecode interpreter */

#ifndef _interp_
#define _interp_


#include "misc.h"
#include "mlvalues.h"

value interprete (code_t prog, asize_t prog_size);


#endif
