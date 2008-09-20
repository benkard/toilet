/* "continue-ia64.S" continuation support for ia64.
 * Copyright (C) 2006 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/* Author: Richard E. Harke */

/*
struct Continuation {jump_buf jmpbuf;
		     long thrwval;
		     long length;
		     STACKITEM *stkbse;
#ifdef __ia64__
                     long *bspbse;
                     long bsplength;
                     long rnat;
#endif
		     CONTINUATION_OTHER other;
		     struct Continuation *parent;
		   };
*/
/* Define offsets for elements of a Continuation structure */
#include "contoffset-ia64.S"


	.global	must_malloc
	.text
	.align	32
	.global	make_root_continuation
	.proc	make_root_continuation
make_root_continuation:
	.prologue
	.save	ar.pfs,r33
	alloc	r33 = ar.pfs,1,3,2,0
	.save	rp,r34
	mov	r34 = b0
	.body
	addl	r14 = @ltoffx(s_call_cc), r1
	mov	out0 = cont_size
	mov	loc2 = gp
	;;
	ld8.mov r14 = [r14], s_call_cc
	;;
	adds	out1 = 18, r14
	;;
	br.call.sptk.many	b0=must_malloc
	;;
	mov	gp = r35
	cmp.eq	p6,p0 = r8,r0
	adds	r14 = stkbse_off,r8
	adds	r15 = bspbse_off,r8
  (p6)  br.cond.dpnt	mrcexit
	;;
	flushrs
	st8	[r14] = r12
	;;
	mov	r31 = ar.bsp
	;;
	adds	r14 = length_off,r8
	st8	[r15] = r31
	adds	r16 = bsplength_off,r8
	;;
	st8	[r14] = r0
	st8	[r16] = r0
	adds	r15 = parent_off,r8
	;;
	st8	[r15] = r8
mrcexit:
	mov	ar.pfs = r33
	mov	b0 = r34
	;;
	br.ret.sptk.many	b0
	.endp	make_root_continuation

	/*
	register usage
	r32 - r39 used in modulo loop (requires multiple of 8)
	r40	save r32 from input
	r41	save return - b0
	r42	ar.pfs
	r43	save gp (r1)
	r44	ar.bsp
	r45	out0
	r46	out1
	*/
	.global make_continuation
	.proc make_continuation
make_continuation:
	.prologue
	.save ar.pfs, r42
	alloc	r42 = ar.pfs, 1,12, 2, 8
	mov	r43 = r1
	.save rp, r41
	mov	r41 = b0
	mov	r40 = r32
	;;
	.body
	adds	r14 = bspbse_off,r40
	adds	r17 = stkbse_off,r40
	;;
	mov	r44 = ar.bsp
	ld8	r15 = [r14]	// bspbse from parent
	ld8	r18 = [r17]	// stkbse from parent
	;;
	sub	r16 = r44,r15	// length of bsp to save
	sub	r19 = r18,r12	// length of stack to save
	addl	r15 = @ltoffx(s_call_cc), r1
	;;
	add	r45 = r16,r19	// bsp len plus stack len
	ld8.mov r14 = [r15], s_call_cc
	;;
	adds	r14 = 18, r14
	adds	r45 = cont_size, r45	// add in length of continuation struct
	;;
	mov	r46 = r14
	br.call.sptk.many	b0 = must_malloc
	mov	r1 = r43
	cmp.eq	p6, p7 = 0, r8
	(p6) br.cond.dptk .L5
	;;
.L1:
	flushrs
	adds	r14 = bspbse_off,r40
	adds	r17 = stkbse_off,r40
	;;
	mov	r31 = ar.rsc
	ld8	r15 = [r14]	// bsp in parent
	ld8	r18 = [r17]	// stack base in parent
	;;
	and	r30 = ~0x3,r31
	sub	r16 = r44,r15	// length of bsp to save
	sub	r19 = r18,r12	// length of stack to save
	;;
	mov	ar.rsc = r30	// set enforced idle
	shr	r16 = r16,3	// number of longs not bytes
	adds	r21 = length_off,r8
	adds	r22 = bsplength_off,r8
	shr	r19 = r19,3	// number of longs not bytes
	;;
	mov	r30 = ar.rnat
	add	r20 = r16,r19	// total length to save
	st8	[r22] = r16	// store the bsp length
	adds	r14 = bspbse_off,r8
	adds	r17 = stkbse_off,r8
	;;
	st8	[r14] = r44	// save current bsp
	st8	[r17] = r18	// stkbse same as parent stkbse
	adds	r22 = parent_off,r8
	st8	[r21] = r20	// store the length
	;;
	adds	r21 = rnat_off,r8
	st8	[r22] = r40	// store parent continuation
	mov	r29 = ar.lc	// need to preserve ar.lc
	mov	r28 = pr	// need to preserve pr.rot
	adds	r16 = -1,r16
	;;
	st8	[r21] = r30	// store rnat's
	mov	ar.lc = r16
	mov	ar.ec = 3
	mov	pr.rot = 0x10000
	adds	r27 = cont_size,r8
	adds	r19 = -1,r19
	;;
.L6:
  (p16)	ld8	r32 = [r15],8
  (p18)	st8	[r27] = r34,8
	br.ctop.sptk.few	.L6
	;;
	mov	r26 = r12
	clrrrb
	;;
	mov	ar.ec = 3
	mov	pr.rot = 0x10000
	mov	ar.lc = r19
	;;
.L7:
  (p16)	ld8	r32 = [r26],8
  (p18)	st8	[r27] = r34,8
	br.ctop.sptk.few	.L7
	;;
	mov	ar.lc = r29	// restore ar.lc
	mov	pr = r28,0x1003e	// restore pr
	mov	ar.rsc = r31	// restore ar.rsc
	;;
.L5:
	mov ar.pfs = r42
	mov b0 = r41
	br.ret.sptk.many b0
	.endp make_continuation


	.global	thrown_value
	.global	longjmp
	.global dynthrow
	.proc dynthrow
dynthrow:
	.prologue
	.save ar.pfs, r42
	alloc	r42 = ar.pfs, 1,12, 2, 8
	mov	r43 = r1
	.save rp, r44
	mov	r44 = b0
	ld8	r40 = [r32],8
	mov	r31 = ar.rsc
	movl	r2 = ~0x3fff0003
	;;
.L3:
	flushrs
	adds	r14 = bspbse_off,r40
	adds	r17 = stkbse_off,r40
	and	r30 = r2,r31
	;;
	ld8	r41 = [r32]
	ld8	r15 = [r14]	// bsp
	ld8	r18 = [r17]	// stack base
	mov	ar.rsc = r30	// set enforced idle
	;;
.L2:
	loadrs
	adds	r21 = length_off,r40
	adds	r22 = bsplength_off,r40
	;;
	mov	ar.bspstore = r15
	ld8	r16 = [r21]	// get total length (number of longs)
	ld8	r17 = [r22]	// get bsp length (number of longs)
	;;
	sub	r20 = r16,r17	// compute stack length
	shl	r25 = r17,3
	;;
	mov	r29 = ar.lc	// need to preserve ar.lc
	mov	r28 = pr	// need to preserve pr.rot
	sub	r15 = r15,r25	// adjust bsp beginning
	shl	r14 = r20,3
	adds	r17 = -1,r17
	adds	r21 = rnat_off,r40
	;;
	sub	r18 = r18,r14	// adjust stack to lowest
	mov	ar.lc = r17
	mov	ar.ec = 3
	mov	pr.rot = 0x10000
	adds	r27 = cont_size,r40
	adds	r20 = -1,r20
	;;
.L8:
  (p16)	ld8	r32 = [r27],8
  (p18)	st8	[r15] = r34,8
	br.ctop.sptk.few	.L8
	;;
	ld8	r14 = [r21]	// get the rnat's
	clrrrb
	;;
	mov	ar.ec = 3
	mov	pr.rot = 0x10000
	mov	ar.lc = r20
	;;
.L9:
  (p16)	ld8	r32 = [r27],8
  (p18)	st8	[r18] = r34,8
	br.ctop.sptk.few	.L9
	;;
	mov	ar.rnat = r14
	mov	ar.lc = r29	// restore ar.lc
	mov	pr = r28,0x1003e	// restore pr
	addl	r26 = @gprel(thrown_value),gp
	;;
	mov	ar.rsc = r31	// restore ar.rsc
	st8	[r26] = r41
	mov	r45 = r40
	mov	r46 = 1
	;;
	br.call.sptk.many	b0 = longjmp
// the following should not be executed
	mov	r1 = r43
	mov	ar.pfs = r42
	mov	b0 = r44
	br.ret.sptk.many b0
	.endp	dynthrow

	.global mark_locations
	.global	mark_regs_ia64
	.proc	mark_regs_ia64
mark_regs_ia64:
	.prologue
	.save	ar.pfs, r35
	alloc	r35 = ar.pfs, 1, 4, 2, 0
	.save	rp, r33
	mov	r33 = b0
	mov	r36 = r1
	mov	r34 = r12
	adds	r17 = stkbse_off, r32
	;;
	adds	r12 = -32, r12
	ld8	r19 = [r17]
	;;
	adds	r18 = 16,r12
        ;;
	sub	r38 = r19, r18
	;;
	st8	[r18] = r4, 8
	shr	r38 = r38, 3
	;;
	st8	[r18] = r5, 8
	;;
	st8	[r18] = r6, 8
	;;
	st8	[r18] = r7
	mov	r37 = r12
	br.call.sptk.many	b0 = mark_locations
	flushrs
	mov	r1 = r36
	adds	r17 = bspbse_off, r32
	;;
	mov	r20 = ar.bsp
	;;
	ld8	r37 = [r17]
	;;
	sub	r38 = r20, r37
	;;
	shr	r38 = r38, 3
	br.call.sptk.many	b0 = mark_locations
	mov	r1 = r36
	mov	r12 = r34
	mov	ar.pfs = r35
	mov	b0 = r33
	br.ret.sptk.many	b0
	.endp	mark_regs_ia64
