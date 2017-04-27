/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: compact.h,v 1.5 2001/12/07 13:39:23 xleroy Exp $ */

#ifndef _compact_
#define _compact_


#include "config.h"
#include "misc.h"

extern void compact_heap (void);
extern void compact_heap_maybe (void);


#endif /* _compact_ */
