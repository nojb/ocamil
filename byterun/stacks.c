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

/* $Id: stacks.c,v 1.18 2001/12/07 13:39:36 xleroy Exp $ */

/* To initialize and resize the stacks */

#include <string.h>
#include "config.h"
#include "fail.h"
#include "misc.h"
#include "mlvalues.h"
#include "stacks.h"

CAMLexport value * stack_low;
CAMLexport value * stack_high;
CAMLexport value * stack_threshold;
CAMLexport value * extern_sp;
CAMLexport value * trapsp;
CAMLexport value * trap_barrier;
value global_data;

unsigned long max_stack_size;            /* also used in gc_ctrl.c */

void init_stack (long unsigned int initial_max_size)
{
  stack_low = (value *) stat_alloc(Stack_size);
  stack_high = stack_low + Stack_size / sizeof (value);
  stack_threshold = stack_low + Stack_threshold / sizeof (value);
  extern_sp = stack_high;
  trapsp = stack_high;
  trap_barrier = stack_high + 1;
  max_stack_size = initial_max_size;
  gc_message (0x08, "Initial stack limit: %luk bytes\n",
              max_stack_size / 1024 * sizeof (value));
}

void realloc_stack(asize_t required_space)
{
  asize_t size;
  value * new_low, * new_high, * new_sp;
  value * p;

  Assert(extern_sp >= stack_low);
  size = stack_high - stack_low;
  do {
    if (size >= max_stack_size) raise_stack_overflow();
    size *= 2;
  } while (size < stack_high - extern_sp + required_space);
  gc_message (0x08, "Growing stack to %luk bytes\n",
              (unsigned long) size * sizeof(value) / 1024);
  new_low = (value *) stat_alloc(size * sizeof(value));
  new_high = new_low + size;

#define shift(ptr) \
    ((char *) new_high - ((char *) stack_high - (char *) (ptr)))

  new_sp = (value *) shift(extern_sp);
  memmove((char *) new_sp,
          (char *) extern_sp,
          (stack_high - extern_sp) * sizeof(value));
  stat_free(stack_low);
  trapsp = (value *) shift(trapsp);
  trap_barrier = (value *) shift(trap_barrier);
  for (p = trapsp; p < new_high; p = Trap_link(p))
    Trap_link(p) = (value *) shift(Trap_link(p));
  stack_low = new_low;
  stack_high = new_high;
  stack_threshold = stack_low + Stack_threshold / sizeof (value);
  extern_sp = new_sp;

#undef shift
}

CAMLprim value ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (extern_sp - req < stack_low) realloc_stack(req);
  return Val_unit;
}

void change_max_stack_size (long unsigned int new_max_size)
{
  asize_t size = stack_high - extern_sp + Stack_threshold / sizeof (value);

  if (new_max_size < size) new_max_size = size;
  if (new_max_size != max_stack_size){
    gc_message (0x08, "Changing stack limit to %luk bytes\n",
                new_max_size * sizeof (value) / 1024);
  }
  max_stack_size = new_max_size;
}
