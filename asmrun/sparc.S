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

/* $Id: sparc.S,v 1.18 2002/02/08 16:55:33 xleroy Exp $ */

/* Asm part of the runtime system for the Sparc processor.  */
/* Must be preprocessed by cpp */

/* SunOS 4 and BSD prefix identifiers with _, Solaris and Linux do not */

#if defined(SYS_sunos) || defined(SYS_bsd)

        .common _caml_required_size, 4, "bss"

#define Young_limit _young_limit
#define Young_ptr _young_ptr
#define Caml_bottom_of_stack _caml_bottom_of_stack
#define Caml_last_return_address _caml_last_return_address
#define Caml_gc_regs _caml_gc_regs
#define Caml_exception_pointer _caml_exception_pointer
#define Caml_required_size _caml_required_size
#define Caml_alloc _caml_alloc
#define Caml_call_gc _caml_call_gc
#define Garbage_collection _garbage_collection
#define Caml_c_call _caml_c_call
#define Caml_start_program _caml_start_program
#define Caml_program _caml_program
#define Raise_caml_exception _raise_caml_exception
#define Callback_exn _callback_exn
#define Callback2_exn _callback2_exn
#define Callback3_exn _callback3_exn
#define Caml_apply2 _caml_apply2
#define Caml_apply3 _caml_apply3
#define Mlraise _mlraise
#define System_frametable _system__frametable

#else

        .common caml_required_size, 4, 4

#define Young_limit young_limit
#define Young_ptr young_ptr
#define Caml_bottom_of_stack caml_bottom_of_stack
#define Caml_last_return_address caml_last_return_address
#define Caml_gc_regs caml_gc_regs
#define Caml_exception_pointer caml_exception_pointer
#define Caml_required_size caml_required_size
#define Caml_alloc caml_alloc
#define Caml_call_gc caml_call_gc
#define Garbage_collection garbage_collection
#define Caml_c_call caml_c_call
#define Caml_start_program caml_start_program
#define Caml_program caml_program
#define Raise_caml_exception raise_caml_exception
#define Callback_exn callback_exn
#define Callback2_exn callback2_exn
#define Callback3_exn callback3_exn
#define Caml_apply2 caml_apply2
#define Caml_apply3 caml_apply3
#define Mlraise mlraise
#define System_frametable system__frametable

#endif

#define Exn_ptr %l5
#define Alloc_ptr %l6
#define Alloc_limit %l7

#define Load(symb,reg)  sethi %hi(symb), %g1; ld [%g1 + %lo(symb)], reg
#define Store(reg,symb) sethi %hi(symb), %g1; st reg, [%g1 + %lo(symb)]
#define Address(symb,reg) sethi %hi(symb), reg; or reg, %lo(symb), reg

/* Allocation functions */

        .text
        .global Caml_alloc
        .global Caml_call_gc

/* Required size in %g2 */
Caml_alloc:
        ld      [Alloc_limit], %g1
        sub     Alloc_ptr, %g2, Alloc_ptr
        cmp     Alloc_ptr, %g1
        blu     Caml_call_gc
        nop
        retl
        nop

/* Required size in %g2 */
Caml_call_gc:
    /* Save %g2 (required size) */
        Store(%g2, Caml_required_size)
    /* Save exception pointer if GC raises */
        Store(Exn_ptr, Caml_exception_pointer)
    /* Save current allocation pointer for debugging purposes */
        Store(Alloc_ptr, Young_ptr)
    /* Record lowest stack address */
        Store(%sp, Caml_bottom_of_stack)
    /* Record last return address */
        Store(%o7, Caml_last_return_address)
    /* Allocate space on stack for caml_context structure and float regs */
        sub     %sp, 20*4 + 15*8, %sp
    /* Save int regs on stack and save it into caml_gc_regs */
L100:   add     %sp, 96 + 15*8, %g2
        std     %o0, [%g2]
        std     %o2, [%g2 + 0x8]
        std     %o4, [%g2 + 0x10]
        std     %i0, [%g2 + 0x18]
        std     %i2, [%g2 + 0x20]
        std     %i4, [%g2 + 0x28]
        std     %l0, [%g2 + 0x30]
        std     %l2, [%g2 + 0x38]
        st      %l4, [%g2 + 0x40]
        st      %g3, [%g2 + 0x44]
        st      %g4, [%g2 + 0x48]
        Store(%g2, Caml_gc_regs)
    /* Save the floating-point registers */
        add     %sp, 96, %g1
        std     %f0, [%g1]
        std     %f2, [%g1 + 0x8]
        std     %f4, [%g1 + 0x10]
        std     %f6, [%g1 + 0x18]
        std     %f8, [%g1 + 0x20]
        std     %f10, [%g1 + 0x28]
        std     %f12, [%g1 + 0x30]
        std     %f14, [%g1 + 0x38]
        std     %f16, [%g1 + 0x40]
        std     %f18, [%g1 + 0x48]
        std     %f20, [%g1 + 0x50]
        std     %f22, [%g1 + 0x58]
        std     %f24, [%g1 + 0x60]
        std     %f26, [%g1 + 0x68]
        std     %f28, [%g1 + 0x70]
    /* Call the garbage collector */
        call    Garbage_collection
        nop
    /* Restore all regs used by the code generator */
        add     %sp, 96 + 15*8, %g2
        ldd     [%g2], %o0
        ldd     [%g2 + 0x8], %o2
        ldd     [%g2 + 0x10], %o4
        ldd     [%g2 + 0x18], %i0
        ldd     [%g2 + 0x20], %i2
        ldd     [%g2 + 0x28], %i4
        ldd     [%g2 + 0x30], %l0
        ldd     [%g2 + 0x38], %l2
        ld      [%g2 + 0x40], %l4
        ld      [%g2 + 0x44], %g3
        ld      [%g2 + 0x48], %g4
        add     %sp, 96, %g1
        ldd     [%g1], %f0
        ldd     [%g1 + 0x8], %f2
        ldd     [%g1 + 0x10], %f4
        ldd     [%g1 + 0x18], %f6
        ldd     [%g1 + 0x20], %f8
        ldd     [%g1 + 0x28], %f10
        ldd     [%g1 + 0x30], %f12
        ldd     [%g1 + 0x38], %f14
        ldd     [%g1 + 0x40], %f16
        ldd     [%g1 + 0x48], %f18
        ldd     [%g1 + 0x50], %f20
        ldd     [%g1 + 0x58], %f22
        ldd     [%g1 + 0x60], %f24
        ldd     [%g1 + 0x68], %f26
        ldd     [%g1 + 0x70], %f28
    /* Reload alloc ptr */
        Load(Young_ptr, Alloc_ptr)
    /* Allocate space for block */
        Load(Caml_required_size, %g2)
        ld      [Alloc_limit], %g1
        sub     Alloc_ptr, %g2, Alloc_ptr
        cmp     Alloc_ptr, %g1      /* Check that we have enough free space */
        blu     L100                /* If not, call GC again */
        nop
    /* Return to caller */
        Load(Caml_last_return_address, %o7)
        retl
        add     %sp, 20*4 + 15*8, %sp       /* in delay slot */

/* Call a C function from Caml */

        .global Caml_c_call
/* Function to call is in %g2 */
Caml_c_call:
    /* Record lowest stack address and return address */
        Store(%sp, Caml_bottom_of_stack)
        Store(%o7, Caml_last_return_address)
    /* Save the exception handler and alloc pointer */
        Store(Exn_ptr, Caml_exception_pointer)
        sethi   %hi(Young_ptr), %g1
    /* Call the C function */
        call    %g2
        st      Alloc_ptr, [%g1 + %lo(Young_ptr)]        /* in delay slot */
    /* Reload return address */
        Load(Caml_last_return_address, %o7)
    /* Reload alloc pointer */
        sethi   %hi(Young_ptr), %g1
    /* Return to caller */
        retl
        ld      [%g1 + %lo(Young_ptr)], Alloc_ptr        /* in delay slot */

/* Start the Caml program */

        .global Caml_start_program
Caml_start_program:
    /* Save all callee-save registers */
        save    %sp, -96, %sp
    /* Address of code to call */
        Address(Caml_program, %l2)

    /* Code shared with callback* */
L108:
    /* Set up a callback link on the stack. */
        sub     %sp, 16, %sp
        Load(Caml_bottom_of_stack, %l0)
        Load(Caml_last_return_address, %l1)
        Load(Caml_gc_regs, %l3)
        std     %l0, [%sp + 96]
        st      %l3, [%sp + 104]
    /* Set up a trap frame to catch exceptions escaping the Caml code */
        call    L111
        nop
        b       L110
        nop
L111:   sub     %sp, 8, %sp
        Load(Caml_exception_pointer, Exn_ptr)
        st      %o7, [%sp + 96]
        st      Exn_ptr, [%sp + 100]
        mov     %sp, Exn_ptr
    /* Reload allocation pointers */
        Load(Young_ptr, Alloc_ptr)
        Address(Young_limit, Alloc_limit)
    /* Call the Caml code */
L109:   call    %l2
        nop
    /* Pop trap frame and restore caml_exception_pointer */
        ld      [%sp + 100], Exn_ptr
        add     %sp, 8, %sp
        Store(Exn_ptr, Caml_exception_pointer)
    /* Pop callback link, restoring the global variables */
L112:   ldd     [%sp + 96], %l0
        ld      [%sp + 104], %l2
        Store(%l0, Caml_bottom_of_stack)
        Store(%l1, Caml_last_return_address)
        Store(%l2, Caml_gc_regs)
        add     %sp, 16, %sp
    /* Save allocation pointer */
        Store(Alloc_ptr, Young_ptr)
    /* Move result where the C function expects it */
        mov     %o0, %i0        /* %i0 will become %o0 after restore */
    /* Reload callee-save registers and return */
        ret
        restore
L110:
    /* The trap handler */
        Store(Exn_ptr, Caml_exception_pointer)
    /* Encode exception bucket as an exception result */
        or      %o0, 2, %o0
        b       L112
        nop

/* Raise an exception from C */

        .global Raise_caml_exception
Raise_caml_exception:
    /* Save exception bucket in a register outside the reg windows */
        mov     %o0, %g2
    /* Load exception pointer in a register outside the reg windows */
        Load(Caml_exception_pointer, %g3)
    /* Pop some frames until the trap pointer is in the current frame. */
        cmp     %g3, %fp
        blt     L107                    /* if Exn_ptr < %fp, over */
        nop
L106:   restore
        cmp     %fp, %g3                /* if %fp <= Exn_ptr, loop */
        ble     L106
        nop
L107:
    /* Reload allocation registers */
        Load(Young_ptr, Alloc_ptr)
        Address(Young_limit, Alloc_limit)
    /* Branch to exception handler */
        mov     %g3, %sp
        ld      [%sp + 96], %g1
        ld      [%sp + 100], Exn_ptr
        add     %sp, 8, %sp
        jmp     %g1 + 8
    /* Restore bucket, in delay slot */
        mov     %g2, %o0

/* Callbacks C -> ML */

        .global Callback_exn
Callback_exn:
    /* Save callee-save registers and return address */
        save    %sp, -96, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %g1, %i1        /* environment */
        b       L108
        ld      [%g1], %l2      /* code pointer */

        .global Callback2_exn
Callback2_exn:
    /* Save callee-save registers and return address */
        save    %sp, -104, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %i2, %i1        /* second arg */
        mov     %g1, %i2        /* environment */
        sethi   %hi(Caml_apply2), %l2
        b       L108
        or      %l2, %lo(Caml_apply2), %l2

        .global Callback3_exn
Callback3_exn:
    /* Save callee-save registers and return address */
        save    %sp, -104, %sp
    /* Initial shuffling of arguments */
        mov     %i0, %g1
        mov     %i1, %i0        /* first arg */
        mov     %i2, %i1        /* second arg */
        mov     %i3, %i2        /* third arg */
        mov     %g1, %i3        /* environment */
        sethi   %hi(Caml_apply3), %l2
        b       L108
        or      %l2, %lo(Caml_apply3), %l2

        .data
        .global System_frametable
System_frametable:
        .word   1               /* one descriptor */
        .word   L109            /* return address into callback */
        .half   -1              /* negative frame size => use callback link */
        .half   0               /* no roots */
