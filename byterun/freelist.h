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

/* $Id: freelist.h,v 1.8 2001/12/07 13:39:27 xleroy Exp $ */

/* Free lists of heap blocks. */

#ifndef _freelist_
#define _freelist_


#include "misc.h"
#include "mlvalues.h"

extern asize_t fl_cur_size;

char *fl_allocate (mlsize_t);
void fl_init_merge (void);
void fl_reset (void);
char *fl_merge_block (char *);
void fl_add_block (char *);


#endif /* _freelist_ */
