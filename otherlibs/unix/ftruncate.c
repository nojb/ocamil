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

/* $Id: ftruncate.c,v 1.9 2002/03/02 09:16:36 xleroy Exp $ */

#include <sys/types.h>
#include <mlvalues.h>
#include <io.h>
#include "unixsupport.h"
#ifdef HAS_UNISTD
#include <unistd.h>
#endif

#ifdef HAS_TRUNCATE

CAMLprim value unix_ftruncate(value fd, value len)
{
  if (ftruncate(Int_val(fd), Long_val(len)) == -1)
    uerror("ftruncate", Nothing);
  return Val_unit;
}

CAMLprim value unix_ftruncate_64(value fd, value len)
{
  if (ftruncate(Int_val(fd), File_offset_val(len)) == -1)
    uerror("ftruncate", Nothing);
  return Val_unit;
}

#else

CAMLprim value unix_ftruncate(value fd, value len)
{ invalid_argument("ftruncate not implemented"); }

#endif
