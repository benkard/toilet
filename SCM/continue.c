/* "continue.c" Scheme Continuations for C.
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997 Free Software Foundation, Inc.
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

/* Author: Aubrey Jaffer */

/* "setjump.h" contains definitions for the `other' field (type
   CONTINUATION_OTHER) the struct Continuation.  "setjump.h" must
   #include "continue.h".  CONTINUATION_OTHER defaults to `long' */

#define IN_CONTINUE_C
#ifdef USE_CONTINUE_H
# include "continue.h"
#else
# include "setjump.h"
#endif

/* For platforms with short integers, we use thrown_value instead of
   the value returned from setjump so that any (long) value can be
   returned.  */

#ifdef SHORT_INT
long thrown_value;
#endif

/* stack_size() returns the number of units of size STACKITEM which
   fit between @var{start} and the current top of stack.  No check is
   done in this routine to ensure that @var{start} is actually in the
   current stack segment.  */

long stack_size(start)
     STACKITEM *start;
{
  STACKITEM stack;
#ifdef STACK_GROWS_UP
  return &stack - start;
#else
  return start - &stack;
#endif /* def STACK_GROWS_UP */
}

/* make_root_continuation() allocates (malloc) storage for a
   CONTINUATION near the current extent of stack.  This newly
   allocated CONTINUATION is returned if successful, 0 if not.  After
   make_root_continuation() returns, the calling routine still needs
   to `setjump(new_continuation->jmpbuf)' in order to complete the
   capture of this continuation.  */

#ifndef __ia64__
CONTINUATION *make_root_continuation(stack_base)
     STACKITEM *stack_base;
{
  CONTINUATION *cont;
  cont = (CONTINUATION *)malloc(sizeof(CONTINUATION));
  if (!cont) return 0;
  cont->length = 0;
  cont->stkbse = stack_base;
  cont->parent = cont;
  return cont;
}

/* make_continuation() allocates storage for the current continuation,
   copying (or encapsulating) the stack state from parent_cont->stkbse
   to the current top of stack.  The newly allocated CONTINUATION is
   returned if successful, 0 if not.  After make_continuation()
   returns, the calling routine still needs to
   `setjump(new_continuation->jmpbuf)' in order to complete the capture
   of this continuation.  */

/* Note: allocating local (stack) storage for the CONTINUATION would
	 not work; Think about it.  */

CONTINUATION *make_continuation(parent_cont)
     CONTINUATION *parent_cont;
{
  CONTINUATION *cont;
# ifdef CHEAP_CONTINUATIONS
  cont = (CONTINUATION *)malloc(sizeof(CONTINUATION));
  if (!cont) return 0;
  cont->length = 0;
  cont->stkbse = parent_cont->stkbse;
# else
  long j;
  register STACKITEM *src, *dst;
  FLUSH_REGISTER_WINDOWS;
  j = stack_size(parent_cont->stkbse);
  cont = (CONTINUATION *)malloc((sizeof(CONTINUATION) + j*sizeof(STACKITEM)));
  if (!cont) return 0;
  cont->length = j;
  cont->stkbse = parent_cont->stkbse;
  src = cont->stkbse;
#  ifdef STACK_GROWS_UP
  src += parent_cont->length;
#  else
  src -= parent_cont->length + cont->length;
#  endif/* ndef STACK_GROWS_UP */
  dst = (STACKITEM *)(cont + 1);
  for (j = cont->length; 0 <= --j; ) *dst++ = *src++;
# endif /* ndef CHEAP_CONTINUATIONS */
  cont->parent = parent_cont;
  return cont;
}
#endif

/* free_continuation() is trivial, but who knows what the future
   holds.  */

void free_continuation(cont)
     CONTINUATION *cont;
{
  free(cont);
}

/* Final routine involved in throw()ing to a continuation.  After
   ensuring that there is sufficient room on the stack for the saved
   continuation, dynthrow() copies the continuation onto the stack and
   longjump()s into it.  The routine does not return.  */

/* If you use conservative GC and your Sparc(SUN-4) heap is growing
   out of control:

   You are experiencing a GC problem peculiar to the Sparc.  The
   problem is that SCM doesn't know how to clear register windows.
   Every location which is not reused still gets marked at GC time.
   This causes lots of stuff which should be collected to not be.
   This will be a problem with any *conservative* GC until we find
   what instruction will clear the register windows.  This problem is
   exacerbated by using lots of make-CONTINUATION.

   Possibly adding the following before the thrown_value = val; line
   might help to clear out unused stack above the continuation (a
   small part of the problem).

#ifdef sparc
  bzero((void *)&a, sizeof(STACKITEM) *
	(((STACKITEM *)&a) - (dst - cont->length)))
#endif

   Let me know if you try it.  */

/* SCM_GROWTH is how many `long's to grow the stack by when we need room. */
#define SCM_GROWTH 100

#ifndef __ia64__
void dynthrow(a)
     long *a;
{
  register CONTINUATION *cont = (CONTINUATION *)(a[0]);
  long val = a[1];
# ifndef CHEAP_CONTINUATIONS
  register long j;
  register STACKITEM *src, *dst = cont->stkbse;
#  ifdef STACK_GROWS_UP
#   ifndef hpux
  if (a[2] && (a - ((long *)a[3]) < SCM_GROWTH))
    puts("grow_throw: check if long growth[]; being optimized out");
#   endif
  /* if (a[2]) fprintf(stderr, " ct = %ld, dist = %ld\n", a[2], (((long *)a[3]) - a)); */
  if (PTR_GE(dst + (cont->length), (STACKITEM *)&a)) grow_throw(a);
#  else
#   ifndef hpux
  if (a[2] && (((long *)a[3]) - a < SCM_GROWTH))
    puts("grow_throw: check if long growth[]; being optimized out");
#   endif
  /* if (a[2]) fprintf(stderr, " ct = %ld, dist = %ld\n", a[2], (((long *)a[3]) - a)); */
  dst -= cont->length;
  if (PTR_LE(dst, (STACKITEM *)&a)) grow_throw(a);
#  endif/* def STACK_GROWS_UP */
  FLUSH_REGISTER_WINDOWS;
  src = (STACKITEM *)(cont + 1);
  for (j = cont->length;0 <= --j;) *dst++ = *src++;
# endif /* ndef CHEAP_CONTINUATIONS */
# ifdef SHORT_INT
  thrown_value = val;
  longjump(cont->jmpbuf, 1);
# else
  longjump(cont->jmpbuf, val);
# endif
}

/* grow_throw() grows the stack by SCM_GROWTH long words.  If the
   "sizeof growth" assignment is not sufficient to restrain your
   overly optimistic compiler, the stack will grow by much less and
   grow_throw() and dynthrow() will waste time calling each other.  To
   fix this you will have to compile grow_throw() in a separate file
   so the compiler won't be able to guess that the growth array isn't
   all used.  */

# ifndef CHEAP_CONTINUATIONS
void grow_throw(a)		/* Grow the stack so that there is room */
     long *a;			/* to copy in the continuation.  Then */
{				/* retry the throw.  */
  long growth[SCM_GROWTH];
  growth[0] = a[0];
  growth[1] = a[1];
  growth[2] = a[2] + 1;
  growth[3] = (long) a;
  growth[SCM_GROWTH-1] = sizeof growth;
  dynthrow(growth);
}
# endif /* ndef CHEAP_CONTINUATIONS */
#endif

/* throw_to_continuation() restores the stack in effect when
   @var{cont} was made and resumes @var{cont}'s processor state.  If
   the stack cannot be resotred because @var{cont} and @var{root_cont}
   do not have the same stkbase, @code{throw_to_continuation()
   returns.  */

/* Note: If 2 or more @var{cont}s share a parent continuation and if
   the values of stack allocated variables in that parent continuation
   are changed, the results are unspecified.  This is because the
   parent continuation may or may not be reloaded, depending on what
   other throws have intervened.  */

void throw_to_continuation(cont, val, root_cont)
     CONTINUATION *cont;
     long val;
     CONTINUATION *root_cont;
{
  long a[3];
  a[0] = (long)cont;
  a[1] = val;
  a[2] = 0;
  if (cont->stkbse != root_cont->stkbse)
    return;			/* Stale continuation */
  dynthrow(a);
}
