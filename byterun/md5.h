/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1999 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: md5.h,v 1.10 2002/04/18 07:27:38 garrigue Exp $ */

/* MD5 message digest */

#ifndef _md5
#define _md5


#include "mlvalues.h"
#include "io.h"

CAMLextern value md5_string (value str, value ofs, value len);
CAMLextern value md5_chan (value vchan, value len);

struct MD5Context {
        uint32 buf[4];
        uint32 bits[2];
        unsigned char in[64];
};

CAMLextern void MD5Init (struct MD5Context *context);
CAMLextern void MD5Update (struct MD5Context *context, unsigned char *buf, 
                           unsigned long len);
CAMLextern void MD5Final (unsigned char *digest, struct MD5Context *ctx);
CAMLextern void MD5Transform (uint32 *buf, uint32 *in);


#endif
