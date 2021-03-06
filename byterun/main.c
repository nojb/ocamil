/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: main.c,v 1.33 2002/06/07 09:49:37 xleroy Exp $ */

/* Main entry point (can be overridden by a user-provided main()
   function that calls caml_main() later). */

#include "misc.h"
#include "mlvalues.h"
#include "sys.h"

CAMLextern void caml_main (char **);

#ifdef _WIN32
CAMLextern void expand_command_line (int *, char ***);
#endif

#if macintosh
#include "rotatecursor.h"
#include "signals.h"
#endif

int main(int argc, char **argv)
{
#ifdef _WIN32
  /* Expand wildcards and diversions in command line */
  expand_command_line(&argc, &argv);
#endif
#if macintosh
  rotatecursor_options (&something_to_do, 0, NULL);
#endif /* macintosh */
  caml_main(argv);
  sys_exit(Val_int(0));
  return 0; /* not reached */
}
