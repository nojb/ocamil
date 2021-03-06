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

/* $Id: dump_img.c,v 1.10 2001/12/07 13:39:53 xleroy Exp $ */

#include "libgraph.h"
#include "image.h"
#include <alloc.h>
#include <memory.h>

value gr_dump_image(value image)
{
  int width, height, i, j;
  XImage * idata, * imask;
  value m = Val_unit;

  Begin_roots2(image, m);
    gr_check_open();
    width = Width_im(image);
    height = Height_im(image);
    m = alloc(height, 0);
    for (i = 0; i < height; i++) {
      value v = alloc(width, 0);
      modify(&Field(m, i), v);
    }

    idata =
      XGetImage(grdisplay, Data_im(image), 0, 0, width, height, (-1), ZPixmap);
    for (i = 0; i < height; i++)
      for (j = 0; j < width; j++)
        Field(Field(m, i), j) = Val_int(gr_rgb_pixel(XGetPixel(idata, j, i)));
    XDestroyImage(idata);

    if (Mask_im(image) != None) {
      imask =
        XGetImage(grdisplay, Mask_im(image), 0, 0, width, height, 1, ZPixmap);
      for (i = 0; i < height; i++)
        for (j = 0; j < width; j++)
          if (XGetPixel(imask, j, i) == 0)
            Field(Field(m, i), j) = Val_int(Transparent);
      XDestroyImage(imask);
    }
  End_roots();
  return m;
}
