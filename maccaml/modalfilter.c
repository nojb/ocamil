/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: modalfilter.c,v 1.3 2001/12/07 13:39:47 xleroy Exp $ */

#include "main.h"

/* See ocaml.r before modifying this. */
typedef struct {
  char mod;
  char chr;
  char item;
  char filler;
} KeyEquRecord, **KeyEquHandle;

short modalkeys;
ModalFilterUPP myModalFilterUPP;

/* Before calling ModalDialog with myModalFilter, set the dialog
   window's refcon to the resource number of the key equivalence
   list for the dialog.
*/
static pascal Boolean myModalFilter (DialogPtr d, EventRecord *evt,
                                     DialogItemIndex *item)
{
  Boolean result = false;
  char key;
  int cmdflag;
  KeyEquHandle equivlist;
  int equivcount, i;
  short itemtype;
  Handle itemhandle;
  Rect itemrect;
  unsigned long ticks;

  switch (evt->what){
  case updateEvt:
    if ((WindowPtr) evt->message != d) WinUpdate ((WindowPtr) evt->message);
    break;
  case activateEvt:
    if ((WindowPtr) evt->message != d){
      WinActivateDeactivate (evt->modifiers & activeFlag,
                             (WindowPtr) evt->message);
    }
    break;
  case keyDown: case autoKey:
    key = evt->message & charCodeMask;
    cmdflag = !!(evt->modifiers & cmdKey);
    equivlist = (KeyEquHandle) GetResource ('Kequ', modalkeys);
    if (equivlist != NULL){
      equivcount = GetHandleSize ((Handle) equivlist) / sizeof (KeyEquRecord);
      for (i = 0; i < equivcount; i++){
        if ((*equivlist)[i].chr == key && (!(*equivlist)[i].mod || cmdflag)){
          result = true;
          *item = (*equivlist)[i].item;
          GetDialogItem (d, *item, &itemtype, &itemhandle, &itemrect);
          HiliteControl ((ControlHandle) itemhandle, kControlButtonPart);
          Delay (kVisualDelay, &ticks);
          HiliteControl ((ControlHandle) itemhandle, 0);
        }
      }
    }
    break;
  default: break;
  }
  return result;
}

OSErr InitialiseModalFilter (void)
{
  myModalFilterUPP = NewModalFilterProc (myModalFilter);
  return noErr;
}
