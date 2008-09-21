/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 2006 Free Software Foundation, Inc.
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

/* "continue.h" Scheme Continuations for C.
   Author: Aubrey Jaffer. */

#ifndef STDC_HEADERS
#define STDC_HEADERS 1
#endif

#include <stdlib.h>

/* If stack is not longword aligned then */

/* #define SHORT_ALIGN */
#ifdef THINK_C
# define SHORT_ALIGN
#endif
#ifdef __MWERKS__
# ifdef __MC68K__
#  define SHORT_ALIGN
# endif
#endif
#ifdef MSDOS
# ifndef _M_ARM
/* arm processors need DWORD aligned data access */
#  define SHORT_ALIGN
# endif
#endif
#ifdef atarist
# define SHORT_ALIGN
#endif

#ifdef SHORT_ALIGN
typedef short STACKITEM;
#else
typedef long STACKITEM;
#endif

/* If stacks grow up then */

/* #define STACK_GROWS_UP */
#ifdef hp9000s800
# define STACK_GROWS_UP
#endif
#ifdef pyr
# define STACK_GROWS_UP
#endif
#ifdef nosve
# define STACK_GROWS_UP
#endif
#ifdef _UNICOS
# define STACK_GROWS_UP
#endif

/* James Clark came up with this neat one instruction fix for
   continuations on the SPARC.  It flushes the register windows so
   that all the state of the process is contained in the stack. */

#ifdef sparc
# define FLUSH_REGISTER_WINDOWS asm("ta 3")
#else
# define FLUSH_REGISTER_WINDOWS /* empty */
#endif

#ifdef vax
# ifndef CHEAP_CONTINUATIONS

typedef int jump_buf[17];
extern int setjump(jump_buf env);
extern int longjump(jump_buf env, int ret);

# else
#  include <setjmp.h>
#  define jump_buf jmp_buf
#  define setjump setjmp
#  define longjump longjmp
# endif
#else				/* ndef vax */
# ifdef _CRAY1

typedef int jump_buf[112];
extern int setjump(jump_buf env);
extern int longjump(jump_buf env, int ret);

# else				/* ndef _CRAY1 */
#  ifndef PLAN9
#   include <setjmp.h>
#   include <signal.h>
#  endif
#  ifdef SIG_UNBLOCK
#   define jump_buf sigjmp_buf
#   define setjump(buf) sigsetjmp((buf), !0)
#   define longjump siglongjmp
#  else
#   define jump_buf jmp_buf
#   define setjump setjmp
#   define longjump longjmp
#  endif                       /* ndef SIG_UNBLOCK */
# endif				/* ndef _CRAY1 */
#endif				/* ndef vax */

/* `other' is a CONTINUATION slot for miscellaneous data of type
   CONTINUATION_OTHER.  */

#ifndef CONTINUATION_OTHER
# define CONTINUATION_OTHER int
#endif

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
typedef struct Continuation CONTINUATION;

#ifndef P
# ifdef USE_ANSI_PROTOTYPES
#  define P(s) s
# else
#  define P(s) ()
# endif
#endif

extern long thrown_value;
long stack_size P((STACKITEM *start));
CONTINUATION *make_root_continuation P((STACKITEM *stack_base));
CONTINUATION *make_continuation P((CONTINUATION *parent_cont));
void free_continuation P((CONTINUATION *cont));
void dynthrow P((long *a));
void grow_throw P((long *a));
void throw_to_continuation P((CONTINUATION *cont, long val,
			      CONTINUATION *root_cont));

/* how to get the local definition for malloc */

#ifndef STDC_HEADERS
# ifndef malloc
	char *malloc P((sizet size));
# endif
	char *realloc P((char *ptr, sizet size));
#endif

/* PTR_LT defines how to compare two addresses (which may not be in
   the same array).  */

#if defined(__TURBOC__) && !defined(__TOS__)
# ifdef PROT386
#  define PTR_LT(x, y) (((long)(x)) < ((long)(y)))
# else
#  define PTR_LT(x, y) ((x) < (y))
# endif
#else /* not __TURBOC__ */
# ifdef nosve
#  define PTR_MASK 0xffffffffffff
#  define PTR_LT(x, y) (((int)(x)&PTR_MASK) < ((int)(y)&PTR_MASK))
# else
#  define PTR_LT(x, y) ((x) < (y))
# endif
#endif

#define PTR_GT(x, y) PTR_LT(y, x)
#define PTR_LE(x, y) (!PTR_GT(x, y))
#define PTR_GE(x, y) (!PTR_LT(x, y))
