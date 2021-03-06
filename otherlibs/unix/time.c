/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: time.c,v 1.9 2001/12/07 13:40:36 xleroy Exp $ */

#include <time.h>
#include <mlvalues.h>
#include <alloc.h>
#include "unixsupport.h"

CAMLprim value unix_time(void)
{
  return copy_double((double) time((time_t *) NULL));
}
