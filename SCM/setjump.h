/* "setjump.h" memory and stack parameters.
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

/* CELL_UP and CELL_DN are used by init_heap_seg to find cell aligned inner
   bounds for allocated storage */

#ifdef PROT386
/*in 386 protected mode we must only adjust the offset */
# define CELL_UP(p) MK_FP(FP_SEG(p), ~7&(FP_OFF(p)+7))
# define CELL_DN(p) MK_FP(FP_SEG(p), ~7&FP_OFF(p))
#else
# ifdef _UNICOS
#  define CELL_UP(p) (CELLPTR)(~1L & ((long)(p)+1L))
#  define CELL_DN(p) (CELLPTR)(~1L & (long)(p))
# else
#  define CELL_UP(p) (CELLPTR)(~(sizeof(cell)-1L) & ((long)(p)+sizeof(cell)-1L))
#  define CELL_DN(p) (CELLPTR)(~(sizeof(cell)-1L) & (long)(p))
# endif				/* UNICOS */
#endif				/* PROT386 */

/* These are parameters for controlling memory allocation.  The heap
   is the area out of which cons and object headers is allocated.
   Each heap object is 8 bytes on a 32 bit machine and 16 bytes on a
   64 bit machine.  The units of the _SIZE parameters are bytes.

   INIT_HEAP_SIZE is the initial size of heap.  If this much heap is
   allocated initially the heap will grow by half its current size
   each subsequent time more heap is needed.

   If INIT_HEAP_SIZE heap cannot be allocated initially, HEAP_SEG_SIZE
   will be used, and the heap will grow by HEAP_SEG_SIZE when more
   heap is needed.  HEAP_SEG_SIZE must fit into type sizet.  This code
   is in init_storage() and alloc_some_heap() in sys.c

   If INIT_HEAP_SIZE can be allocated initially, the heap will grow by
   EXPHEAP(heap_cells) when more heap is needed.

   MIN_HEAP_SEG_SIZE is minimum size of heap to accept when more heap
   is needed.

   INIT_MALLOC_LIMIT is the initial amount of malloc usage which will
   trigger a GC. */

#define INIT_HEAP_SIZE (25000L*sizeof(cell))
#define MIN_HEAP_SEG_SIZE (2000L*sizeof(cell))
#ifdef _QC
# define HEAP_SEG_SIZE 32400L
#else
# ifdef sequent
#  define HEAP_SEG_SIZE (7000L*sizeof(cell))
# else
#  define HEAP_SEG_SIZE (8100L*sizeof(cell))
# endif
#endif
#define EXPHEAP(heap_cells) (heap_cells*2)
#define INIT_MALLOC_LIMIT 100000

/* ECACHE_SIZE is the number of cells in the copy-collected environment
   cache used for environment frames */
#define ECACHE_SIZE 2000

/* If fewer than MIN_GC_YIELD cells are recovered during a
   cell-requested garbage collection (GC), then another heap segment
   is allocated. */

#define MIN_GC_YIELD (heap_cells / 4)

/* If fewer than MIN_MALLOC_YIELD cells are free after a
   malloc-requested garbage collection (GC), then the mtrigger limit
   is raised. */

#define MIN_MALLOC_YIELD (mtrigger / 8)

/* NUM_HASH_BUCKETS is the number of symbol hash table buckets.  */

#define NUM_HASH_BUCKETS 137

#ifdef IN_CONTINUE_C
# include "scm.h"
# define malloc(size) must_malloc((long)(size), s_cont)
# define free(obj) must_free((char *)(obj), 0)
#endif

/* other.dynenv and other.parent get GCed just by being there.  */
struct scm_other {SCM dynenv;
		  SCM parent;
#ifdef RECKLESS
		  SCM stkframe[2];
#else
		  SCM stkframe[4];
#endif
                  SCM estk;
		  SCM *estk_ptr;
		};
#define CONTINUATION_OTHER struct scm_other
#define CONT(x) ((CONTINUATION *)CDR(x))
#define SETCONT SETCDR
void dowinds P((SCM to));

#include "continue.h"

typedef struct safeport {
  SCM port;
  jmp_buf jmpbuf;		/* The usual C jmp_buf, not SCM's jump_buf */
  int ccnt;
} safeport;

#define SAFEP_JMPBUF(sfp) (((safeport *)STREAM(sfp))->jmpbuf)
