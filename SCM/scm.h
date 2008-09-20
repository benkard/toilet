/* "scm.h" SCM data types and external functions.
 * Copyright (C) 1990-2006 Free Software Foundation, Inc.
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

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
# include <windows.h>
#endif

#ifdef _WIN32_WCE
# include <windows.h>
#endif

#ifdef hpux
# ifndef __GNUC__
#  define const /**/
# endif
#endif

#ifdef PLAN9
# include <u.h>
# include <libc.h>
/* Simple imitation of some Unix system calls */
# define exit(val) exits("")
# define getcwd getwd
/* we have our own isatty */
int isatty (int);
#endif

typedef long SCM;
typedef struct {SCM car, cdr;} cell;
typedef struct {long sname;SCM (*cproc)();} subr;
typedef struct {long sname;double (*dproc)();} dsubr;
typedef struct {const char *string;SCM (*cproc)();} iproc;
typedef struct {const char *name;} subr_info;

#include <stdio.h>
#include "scmfig.h"

#ifdef _WIN32
# ifdef SCM_WIN_DLL
#  define SCM_DLL_EXPORT __declspec(dllexport)
#  define SCM_EXPORT SCM_DLL_EXPORT
# else
#  define SCM_DLL_EXPORT /**/
#  define SCM_EXPORT extern
# endif
#else
# define SCM_DLL_EXPORT /**/
# define SCM_EXPORT extern
#endif


typedef struct {
  sizet eltsize;
  sizet len;
  sizet alloclen;
  sizet maxlen;
  const char *what;
  char *elts;} scm_gra;

#ifdef USE_ANSI_PROTOTYPES
# define P(s) s
#else
# define P(s) ()
#endif

#ifndef STDC_HEADERS
        int isatty P((int));
#endif

typedef struct {
  SCM   (*mark)P((SCM));
  sizet (*free)P((CELLPTR));
  int   (*print)P((SCM exp, SCM port, int writing));
  SCM   (*equalp)P((SCM, SCM));
} smobfuns;

typedef struct {
  char  *name;
  SCM   (*mark)P((SCM ptr));
  int   (*free)P((FILE *p));
  int   (*print)P((SCM exp, SCM port, int writing));
  SCM   (*equalp)P((SCM, SCM));
  int   (*fputc)P((int c, FILE *p));
/*    int       (*fputs)P((char *s, FILE *p)); */
/*    sizet     (*fwrite)P((char *s, sizet siz, sizet num, FILE *p)); */
  int   (*fputs)P((const char *s, FILE *p));
  sizet (*fwrite)P((const void *s, sizet siz, sizet num, FILE *p));
  int   (*fflush)P((FILE *stream));
  int   (*fgetc)P((FILE *p));
  int   (*fclose)P((FILE *p));
  int   (*ungetc)P((int c, SCM p));
} ptobfuns;

typedef struct {
  SCM data;
  SCM port;
  long flags;
  long line;
  int unread;
  short col;
  short colprev;
} port_info;

typedef struct {
  SCM v;
  sizet base;
} array;
typedef struct {
  long lbnd;
  long ubnd;
  long inc;
} array_dim;

#ifdef FLOATS
typedef struct {char *string;double (*cproc)P((double));} dblproc;
# ifdef SINGLES
#  ifdef CDR_DOUBLES
typedef struct {SCM type;double num;} flo;
#  else
typedef struct {SCM type;float num;} flo;
#  endif
# endif
typedef struct {SCM type;double *real;} dbl;
#endif

  /* Conditionals should always expect immediates */
  /* GCC __builtin_expect() is stubbed in scmfig.h */
#define IMP(x) SCM_EXPECT_TRUE(6 & PTR2INT(x))
#define NIMP(x) (!IMP(x))

#define INUMP(x) SCM_EXPECT_TRUE(2 & PTR2INT(x))
#define NINUMP(x) (!INUMP(x))
#define INUM0 ((SCM) 2)
#define ICHRP(x) ((0xff & PTR2INT(x))==0xf4)
#define ICHR(x) ((unsigned char)((x)>>8))
#define MAKICHR(x) (((x)<<8)+0xf4L)

#define ILOC00  (0x000000fcL)
#define ILOCP(n) ((0xff & PTR2INT(n))==PTR2INT(ILOC00))
#define MAKILOC(if, id) (ILOC00 + (((long)id)<<8) + (((long)if)<<16))
#define IDIST(n) ((PTR2INT(n)>>8) & 0x7f)
#define IFRAME(n) ((PTR2INT(n)>>16))
#define ICDRP(n) (ICDR & (n))
#define ICDR (1L<<15)

/* ISYMP tests for ISPCSYM and ISYM */
#define ISYMP(n) ((0x187 & PTR2INT(n))==4)
/* IFLAGP tests for ISPCSYM, ISYM and IFLAG */
#define IFLAGP(n) ((0x87 & PTR2INT(n))==4)
#define ISYMNUM(n) ((PTR2INT((n)>>9)) & 0x7f)
#define ISYMVAL(n) (PTR2INT((n)>>16))
#define MAKISYMVAL(isym, val) ((isym) | ((long)(val) <<16))
#define ISYMCHARS(n) (isymnames[ISYMNUM(n)])
#define MAKSPCSYM(n) (((n)<<9)+((n)<<3)+4L)
#define MAKISYM(n) (((n)<<9)+0x74L)
#define MAKIFLAG(n) (((n)<<9)+0x174L)
/* This is to make the print representation of some evaluated code,
   as in backtraces, make a little more sense. */
#define MAKSPCSYM2(work, look) ((127L & (work)) | ((127L<<9) & (look)))

SCM_EXPORT char *isymnames[];
#define NUM_ISPCSYM 14
#define IM_AND MAKSPCSYM(0)
#define IM_BEGIN MAKSPCSYM(1)
#define IM_CASE MAKSPCSYM(2)
#define IM_COND MAKSPCSYM(3)
#define IM_DO MAKSPCSYM(4)
#define IM_IF MAKSPCSYM(5)
#define IM_LAMBDA MAKSPCSYM(6)
#define IM_LET MAKSPCSYM(7)
#define IM_LETSTAR MAKSPCSYM(8)
#define IM_LETREC MAKSPCSYM(9)
#define IM_OR MAKSPCSYM(10)
#define IM_QUOTE MAKSPCSYM(11)
#define IM_SET MAKSPCSYM(12)
#define IM_FUNCALL MAKSPCSYM(13)

#define s_and (ISYMCHARS(IM_AND)+2)
#define s_begin (ISYMCHARS(IM_BEGIN)+2)
#define s_case (ISYMCHARS(IM_CASE)+2)
#define s_cond (ISYMCHARS(IM_COND)+2)
#define s_do (ISYMCHARS(IM_DO)+2)
#define s_if (ISYMCHARS(IM_IF)+2)
#define s_lambda (ISYMCHARS(IM_LAMBDA)+2)
#define s_let (ISYMCHARS(IM_LET)+2)
#define s_letstar (ISYMCHARS(IM_LETSTAR)+2)
#define s_letrec (ISYMCHARS(IM_LETREC)+2)
#define s_or (ISYMCHARS(IM_OR)+2)
#define s_quote (ISYMCHARS(IM_QUOTE)+2)
#define s_set (ISYMCHARS(IM_SET)+2)
#define s_define (ISYMCHARS(IM_DEFINE)+2)
#define s_delay (ISYMCHARS(IM_DELAY)+2)
#define s_quasiquote (ISYMCHARS(IM_QUASIQUOTE)+2)
#define s_let_syntax (ISYMCHARS(IM_LET_SYNTAX)+2)

SCM_EXPORT SCM i_dot, i_quote, i_quasiquote, i_unquote, i_uq_splicing;
#define s_apply (ISYMCHARS(IM_APPLY)+2)

/* each symbol defined here must have a unique number which
   corresponds to it's position in isymnames[] in repl.c */
  /* These are used for dispatch in eval.c */
#define IM_APPLY MAKISYM(14)
#define IM_FARLOC_CAR MAKISYM(15)
#define IM_FARLOC_CDR MAKISYM(16)
#define IM_DELAY MAKISYM(17)
#define IM_QUASIQUOTE MAKISYM(18)
#define IM_EVAL_FOR_APPLY MAKISYM(19)
#define IM_LET_SYNTAX MAKISYM(20)
#define IM_ACRO_CALL MAKISYM(21)
#define IM_LINUM MAKISYM(22)
#define IM_DEFINE MAKISYM(23)
#define IM_EVAL_VALUES MAKISYM(24)

  /* These are not used for dispatch. */
#define IM_UNQUOTE MAKISYM(25)
#define IM_UQ_SPLICING MAKISYM(26)
#define IM_ELSE MAKISYM(27)
#define IM_ARROW MAKISYM(28)
#define IM_VALUES_TOKEN MAKISYM(29)
#define IM_KEYWORD MAKISYM(30)

#define NUM_ISYMS 31

#define SCM_MAKE_LINUM(n) (IM_LINUM | ((unsigned long)(n))<<16)
#define SCM_LINUM(x) ((unsigned long)(x)>>16)
#define SCM_LINUMP(x) ((0xffffL & (x))==IM_LINUM)

#define BOOL_F MAKIFLAG(NUM_ISYMS+0)
#define BOOL_T MAKIFLAG(NUM_ISYMS+1)
#define UNDEFINED MAKIFLAG(NUM_ISYMS+2)
#define EOF_VAL MAKIFLAG(NUM_ISYMS+3)
#ifdef SICP
# define EOL BOOL_F
#else
# define EOL MAKIFLAG(NUM_ISYMS+4)
#endif
#define UNSPECIFIED MAKIFLAG(NUM_ISYMS+5)
#define NUM_IFLAGS NUM_ISYMS+6

/* Now some unnamed flags used as magic cookies by scm_top_level. */
/* Argument n can range from -4 to 16 */
#ifdef SHORT_INT
# define COOKIE(n) (n)
# define UNCOOK(f) (f)
#else
# define COOKIE(n) MAKIFLAG(NUM_IFLAGS+4+n)
# define UNCOOK(f) (ISYMNUM(f)-(NUM_IFLAGS+4))
#endif

#define FALSEP(x) (BOOL_F==(x))
#define NFALSEP(x) (BOOL_F != (x))
/* BOOL_NOT returns the other boolean.  The order of ^s here is
   important for Borland C++. */
#define BOOL_NOT(x)  ((x) ^ (BOOL_T ^ BOOL_F))
#define NULLP(x) (EOL==(x))
#define NNULLP(x) (EOL != (x))
#define UNBNDP(x) (UNDEFINED==(x))
#define CELLP(x) (!NCELLP(x))
#define NCELLP(x) ((sizeof(cell)-1) & PTR2INT(x))

#define GCMARKP(x) (1 & PTR2INT(CDR(x)))
#define GC8MARKP(x) (0x80 & PTR2INT(CAR(x)))
#define SETGCMARK(x) CDR(x) |= 1;
#define CLRGCMARK(x) CDR(x) &= ~1L;
#define SETGC8MARK(x) CAR(x) |= 0x80;
#define CLRGC8MARK(x) CAR(x) &= ~0x80L;
#define TYP3(x) (7 & PTR2INT(CAR(x)))
#define TYP7(x) (0x7f & PTR2INT(CAR(x)))
#define TYP7S(x) (0x7d & PTR2INT(CAR(x)))
#define TYP16(x) (0xffff & PTR2INT(CAR(x)))
#define TYP16S(x) (0xfeff & PTR2INT(CAR(x)))
#define GCTYP16(x) (0xff7f & PTR2INT(CAR(x)))

#define NCONSP(x) (1 & PTR2INT(CAR(x)))
#define CONSP(x) (!NCONSP(x))
#define ECONSP(x) (CONSP(x) || (1==TYP3(x)))
#define NECONSP(x) (NCONSP(x) && (1 != TYP3(x)))
#define SCM_GLOCP(x) (tc3_cons_gloc==(7 & PTR2INT(x)))

#define CAR(x) (((cell *)(SCM2PTR(x)))->car)
#define CDR(x) (((cell *)(SCM2PTR(x)))->cdr)
#define GCCDR(x) (~1L & CDR(x))
#define SETCDR(x, v) CDR(x) = (SCM)(v)

#ifdef _M_ARM
/* MS CLARM compiler bug workaround.  */
volatile SCM MS_CLARM_dumy;
# define CODE(x) (MS_CLARM_dumy = (CAR(x)-tc3_closure))
#else
# define CODE(x) (CAR(x)-tc3_closure)
#endif
#define CLOSUREP(x) (TYP3(x)==tc3_closure)
#define SETCODE(x, e) CAR(x) = (e)+tc3_closure
#define ENV(x) ((~7L & CDR(x)) ? (~7L & CDR(x)) : EOL)
#define GCENV ENV
#define ARGC(x) ((6L & CDR(x))>>1)
#ifdef CAUTIOUS
# define SCM_ESTK_FRLEN 4
#else
# define SCM_ESTK_FRLEN 3
#endif
#define SCM_ESTK_BASE 4
#define SCM_ESTK_PARENT(v) (VELTS(v)[0])
#define SCM_ESTK_PARENT_WRITABLEP(v) (VELTS(v)[1])
#define SCM_ESTK_PARENT_INDEX(v) (VELTS(v)[2])
SCM_EXPORT long tc16_env, tc16_ident;
#define ENVP(x) (tc16_env==TYP16(x))
#define SCM_ENV_FORMALS CAR
#ifdef MACRO
# define M_IDENTP(x) (tc16_ident==TYP16(x))
# define M_IDENT_LEXP(x) ((tc16_ident | (1L<<16))==CAR(x))
# define IDENTP(x) (SYMBOLP(x) || M_IDENTP(x))
# define IDENT_PARENT(x) (M_IDENT_LEXP(x) ? CAR(CDR(x)) : CDR(x))
# define IDENT_ENV(x) (M_IDENT_LEXP(x) ? CDR(CDR(x)) : BOOL_F)
#else
# define IDENTP SYMBOLP
# define M_IDENTP(x) (0)
#endif

  /* markers for various static environment frame types */
  /* FIXME these need to be exported somehow to Scheme */
#ifdef CAUTIOUS
# define SCM_ENV_FILENAME MAKINUM(1)
# define SCM_ENV_PROCNAME MAKINUM(2)
#endif
#define SCM_ENV_DOC MAKINUM(3)
#define SCM_ENV_ANNOTATION MAKINUM(4)
#define SCM_ENV_CONSTANT MAKINUM(5)
#define SCM_ENV_SYNTAX MAKINUM(6)
#define SCM_ENV_END MAKINUM(7)

#define PORTP(x) (TYP7(x)==tc7_port)
#define OPPORTP(x) (((0x7f | OPN) & CAR(x))==(tc7_port | OPN))
#define OPINPORTP(x) (((0x7f | OPN | RDNG) & CAR(x))==(tc7_port | OPN | RDNG))
#define OPOUTPORTP(x) (((0x7f | OPN | WRTNG) & CAR(x))==(tc7_port | OPN | WRTNG))
#define OPIOPORTP(x) (((0x7f | OPN | RDNG | WRTNG) & CAR(x))==(tc7_port | OPN | RDNG | WRTNG))
#define FPORTP(x) (TYP16S(x)==tc7_port)
#define OPFPORTP(x) (((0xfeff | OPN) & CAR(x))==(tc7_port | OPN))
#define OPINFPORTP(x) (((0xfeff | OPN | RDNG) & CAR(x))==(tc7_port | OPN | RDNG))
#define OPOUTFPORTP(x) (((0xfeff | OPN | WRTNG) & CAR(x))==(tc7_port | OPN | WRTNG))

#define INPORTP(x) (((0x7f | RDNG) & CAR(x))==(tc7_port | RDNG))
#define OUTPORTP(x) (((0x7f | WRTNG) & CAR(x))==(tc7_port | WRTNG))
#define OPENP(x) (OPN & CAR(x))
#define CLOSEDP(x) (!OPENP(x))
#define STREAM(x) ((FILE *)(CDR(x)))
#define SETSTREAM SETCDR
#define CRDYP(port) ((CAR(port) & CRDY) && (EOF != CGETUN(port)))
#define CLRDY(port) (CAR(port) &= (SCM_PORTFLAGS(port) | (~0xf0000)))
#define CGETUN(port) (scm_port_table[SCM_PORTNUM(port)].unread)

#define tc_socket (tc7_port | OPN)
#define SOCKP(x) (((0x7f | OPN | RDNG | WRTNG) & CAR(x))==(tc_socket))
#define SOCKTYP(x) (INUM(SCM_PORTDATA(x)))

#define DIRP(x) (NIMP(x) && (TYP16(x)==(tc16_dir)))
#define OPDIRP(x) (NIMP(x) && (CAR(x)==(tc16_dir | OPN)))

#ifdef FLOATS
# define INEXP(x) (TYP16(x)==tc16_flo)
# define CPLXP(x) (CAR(x)==tc_dblc)
# define REAL(x) (*(((dbl *) (SCM2PTR(x)))->real))
# define IMAG(x) (*((double *)(CHARS(x)+sizeof(double))))
/* ((&REAL(x))[1]) */
# ifdef SINGLES
#  define REALP(x) ((~REAL_PART & CAR(x))==tc_flo)
#  define SINGP(x) SCM_EXPECT_TRUE(CAR(x)==tc_flo)
#  define FLO(x) (((flo *)(SCM2PTR(x)))->num)
#  define REALPART(x) (SINGP(x)?0.0+FLO(x):REAL(x))
# else /* SINGLES */
#  define REALP(x) (CAR(x)==tc_dblr)
#  define REALPART REAL
# endif /* SINGLES */
#endif

#ifdef FLOATS
# define NUMBERP(x) (INUMP(x) || (NIMP(x) && NUMP(x)))
#else
# ifdef BIGDIG
#  define NUMBERP(x) (INUMP(x) || (NIMP(x) && NUMP(x)))
# else
#  define NUMBERP INUMP
# endif
#endif
#define NUMP(x) ((0xfcff & PTR2INT(CAR(x)))==tc7_smob)
#define BIGP(x) (TYP16S(x)==tc16_bigpos)
#define BIGSIGN(x) (0x0100 & PTR2INT(CAR(x)))
#define BDIGITS(x) ((BIGDIG *)(CDR(x)))
#define NUMDIGS(x) ((sizet)(((unsigned long)CAR(x))>>16))
#define MAKE_NUMDIGS(v, t) ((((v)+0L)<<16)+(t))
#define SETNUMDIGS(x, v, t) CAR(x) = MAKE_NUMDIGS(v, t)

#define SNAME(x) ((char *)(subrs[NUMDIGS(x)].name))
#define SUBRF(x) (((subr *)(SCM2PTR(x)))->cproc)
#define DSUBRF(x) (((dsubr *)(SCM2PTR(x)))->dproc)
#define CCLO_SUBR(x) (VELTS(x)[0])
#define CCLO_LENGTH NUMDIGS
#define CXR_OP SMOBNUM

#define SYMBOLP(x) (TYP7S(x)==tc7_ssymbol)
#define STRINGP(x) (TYP7(x)==tc7_string)
#define NSTRINGP(x) (!STRINGP(x))
#define BYTESP(x) (TYP7(x)==tc7_VfixN8)
#define VECTORP(x) (TYP7(x)==tc7_vector)
#define NVECTORP(x) (!VECTORP(x))
#define LENGTH(x) (((unsigned long)CAR(x))>>8)
#define LENGTH_MAX (((unsigned long)-1L)>>8)
#define MAKE_LENGTH(v, t) ((((v)+0L)<<8) + (t))
#define SETLENGTH(x, v, t) CAR(x) = MAKE_LENGTH(v, t)
#define CHARS(x) ((char *)(CDR(x)))
#define UCHARS(x) ((unsigned char *)(CDR(x)))
#define VELTS(x) ((SCM *)CDR(x))
#define SETCHARS SETCDR
#define SETVELTS SETCDR

SCM_EXPORT long tc16_array;
#define ARRAYP(a) (tc16_array==TYP16(a))
#define ARRAY_V(a) (((array *)CDR(a))->v)
/*#define ARRAY_NDIM(x) NUMDIGS(x)*/
#define ARRAY_NDIM(x) ((sizet)(CAR(x)>>17))
#define ARRAY_CONTIGUOUS 0x10000
#define ARRAY_CONTP(x) (ARRAY_CONTIGUOUS & PTR2INT(CAR(x)))
#define ARRAY_BASE(a) (((array *)CDR(a))->base)
#define ARRAY_DIMS(a) ((array_dim *)(CHARS(a)+sizeof(array)))

#define FREEP(x) (CAR(x)==tc_free_cell)
#define NFREEP(x) (!FREEP(x))

#define SMOBNUM(x) (0x0ff & (CAR(x)>>8))
#define PTOBNUM(x) (0x0ff & (CAR(x)>>8))
#define SCM_PORTNUM(x) ((int)(((unsigned long)CAR(x))>>20))
#define SCM_PORTNUM_MAX ((int)((0x7fffUL<<20)>>20))
#define SCM_PORTFLAGS(x) (scm_port_table[SCM_PORTNUM(x)].flags)
#define SCM_PORTDATA(x) (scm_port_table[SCM_PORTNUM(x)].data)
#define SCM_SETFLAGS(x, flags) (CAR(x) = (CAR(x) & ~0x0f0000L) | (flags))
/* This is used (only) for closing ports. */
#define SCM_SET_PTOBNUM(x, typ) (CAR(x)=(typ)|(CAR(x) & ~0x0ffffL))

#define DIGITS '0':case '1':case '2':case '3':case '4':\
                case '5':case '6':case '7':case '8':case '9'

/* Aggregated types for dispatch in switch statements. */

#define tcs_cons_inum 2: case 6:case 10:case 14:\
                 case 18:case 22:case 26:case 30:\
                 case 34:case 38:case 42:case 46:\
                 case 50:case 54:case 58:case 62:\
                 case 66:case 70:case 74:case 78:\
                 case 82:case 86:case 90:case 94:\
                 case 98:case 102:case 106:case 110:\
                 case 114:case 118:case 122:case 126
#define tcs_cons_iloc 124
#define tcs_cons_ispcsym 4:case 12:case 20:case 28:\
                   case 36:case 44:case 52:case 60:\
                   case 68:case 76:case 84:case 92:\
                   case 100:case 108
#define tcs_cons_chflag 116     /* char *or* flag */
#define tcs_cons_imcar tcs_cons_inum:\
                   case tcs_cons_iloc:\
                   case tcs_cons_ispcsym:\
                   case tcs_cons_chflag

#define tcs_cons_nimcar 0:case 8:case 16:case 24:\
                 case 32:case 40:case 48:case 56:\
                 case 64:case 72:case 80:case 88:\
                 case 96:case 104:case 112:case 120
#define tcs_cons_gloc 1:case 9:case 17:case 25:\
                 case 33:case 41:case 49:case 57:\
                 case 65:case 73:case 81:case 89:\
                 case 97:case 105:case 113:case 121

#define tcs_closures   3:case 11:case 19:case 27:\
                 case 35:case 43:case 51:case 59:\
                 case 67:case 75:case 83:case 91:\
                 case 99:case 107:case 115:case 123
#define tcs_subrs tc7_asubr:case tc7_subr_0:case tc7_subr_1:case tc7_cxr:\
        case tc7_subr_3:case tc7_subr_2:case tc7_rpsubr:case tc7_subr_1o:\
        case tc7_subr_2o:case tc7_lsubr_2:case tc7_lsubr
#define tcs_symbols tc7_ssymbol:case tc7_msymbol
#define tcs_bignums tc16_bigpos:case tc16_bigneg
#define tcs_uves tc7_string:case tc7_Vbool:case tc7_VfixN8:case tc7_VfixZ8:\
  case tc7_VfixN16:case tc7_VfixZ16:case tc7_VfixN32:case tc7_VfixZ32:\
  case tc7_VfloR32:case tc7_VfloC32:case tc7_VfloR64:case tc7_VfloC64

#define tc3_cons_nimcar 0
#define tc3_cons_imcar  2:case 4:case 6
#define tc3_cons_gloc   1
#define tc3_closure     3
#define tc3_tc7_types   5:case 7

#define tc7_ssymbol     5
#define tc7_msymbol     7
#define tc7_string      13
#define tc7_vector      15
#define tc7_Vbool       21

/* 23 */

#define tc7_VfixN8      29
#define tc7_VfixZ8      31
#define tc7_VfixN16     37
#define tc7_VfixZ16     39
#define tc7_VfixN32     45
#define tc7_VfixZ32     47

#define tc7_VfloR32     53
#define tc7_VfloC32     55
#define tc7_VfloR64     61
#define tc7_VfloC64     63

/* 69 */

#define tc7_port        71
#define tc7_contin      77
#define tc7_specfun     79

#define tc7_subr_0      85
#define tc7_subr_1      87
#define tc7_cxr         93
#define tc7_subr_3      95
#define tc7_subr_2      101
#define tc7_asubr       103
#define tc7_subr_1o     109
#define tc7_subr_2o     111
#define tc7_lsubr_2     117
#define tc7_lsubr       119
#define tc7_rpsubr      125

#define tc7_smob        127
#define tc_free_cell    127
#define tc_broken_heart (tc_free_cell+0x10000)

#define tc16_apply      (tc7_specfun | (0L<<8))
#define tc16_call_cc    (tc7_specfun | (1L<<8))
#define tc16_cclo       (tc7_specfun | (2L<<8))
#define tc16_eval       (tc7_specfun | (3L<<8))
#define tc16_values     (tc7_specfun | (4L<<8))
#define tc16_call_wv    (tc7_specfun | (5L<<8))

#define tc16_flo        0x017f
#define tc_flo          0x017fL

#define REAL_PART       (1L<<16)
#define IMAG_PART       (2L<<16)
#define tc_dblr         (tc16_flo|REAL_PART)
#define tc_dblc         (tc16_flo|REAL_PART|IMAG_PART)

#define tc16_bigpos     0x027f
#define tc16_bigneg     0x037f

  /* The first four flags fit in the car of a port cell, remaining
   flags only in the port table */
#define OPN             (1L<<16)
#define RDNG            (2L<<16)
#define WRTNG           (4L<<16)
#define CRDY            (8L<<16)

#define TRACKED         (16L<<16)
#define BINARY          (32L<<16)
#define BUF0            (64L<<16)
#define EXCLUSIVE       (128L<<16)
                /* LSB is used for gc mark */

SCM_EXPORT scm_gra subrs_gra;
#define subrs ((subr_info *)(subrs_gra.elts))
/* SCM_EXPORT sizet numsmob, numptob;
  SCM_EXPORT smobfuns *smobs;
  SCM_EXPORT ptobfuns *ptobs;
  SCM_EXPORT ptobfuns pipob; */
SCM_EXPORT scm_gra smobs_gra;
#define numsmob (smobs_gra.len)
#define smobs ((smobfuns *)(smobs_gra.elts))
SCM_EXPORT scm_gra ptobs_gra;
#define numptob (ptobs_gra.len)
#define ptobs ((ptobfuns *)(ptobs_gra.elts))
SCM_EXPORT port_info *scm_port_table;

#define tc16_fport (tc7_port + 0*256L)
#define tc16_pipe (tc7_port + 1*256L)
#define tc16_strport (tc7_port + 2*256L)
#define tc16_sfport (tc7_port + 3*256L)
SCM_EXPORT long tc16_dir;
SCM_EXPORT long tc16_clport;

SCM_EXPORT SCM sys_protects[];
#define cur_inp          sys_protects[0]
#define cur_outp         sys_protects[1]
#define cur_errp         sys_protects[2]
#define def_inp          sys_protects[3]
#define def_outp         sys_protects[4]
#define def_errp         sys_protects[5]
#define sys_errp         sys_protects[6]
#define sys_safep        sys_protects[7]
#define listofnull       sys_protects[8]
#define undefineds       sys_protects[9]
#define nullvect         sys_protects[10]
#define nullstr          sys_protects[11]
#define progargs         sys_protects[12]
#define loadports        sys_protects[13]
#define rootcont         sys_protects[14]
#define dynwinds         sys_protects[15]
#define list_unspecified sys_protects[16]
#define f_evapply        sys_protects[17]
#define eval_env         sys_protects[18]
#define f_apply_closure  sys_protects[19]
#define flo0             sys_protects[20]
#define scm_uprotects    sys_protects[21]
#define scm_narn         sys_protects[22]
#define NUM_PROTECTS 23

/* now for connects between source files */

/* SCM_EXPORT sizet num_finals;
   SCM_EXPORT void (**finals)P((void));
   SCM_EXPORT sizet num_finals; */
SCM_EXPORT scm_gra finals_gra;
#define num_finals (finals_gra.len)
#define finals ((void (**)())(finals_gra.elts))

SCM_EXPORT unsigned char upcase[], downcase[];
SCM_EXPORT SCM symhash;
SCM_EXPORT int symhash_dim;
SCM_EXPORT long heap_cells;
SCM_EXPORT CELLPTR heap_org;
SCM_EXPORT VOLATILE SCM freelist;
SCM_EXPORT long gc_cells_collected, gc_malloc_collected, gc_ports_collected;
SCM_EXPORT long gc_syms_collected;
SCM_EXPORT long cells_allocated, lcells_allocated, mallocated, lmallocated;
SCM_EXPORT long mtrigger;
SCM_EXPORT SCM *loc_loadpath;
SCM_EXPORT SCM *loc_errobj;
SCM_EXPORT SCM loadport;
SCM_EXPORT char *errjmp_bad;
SCM_EXPORT VOLATILE int ints_disabled;
SCM_EXPORT int output_deferred, gc_hook_pending, gc_hook_active;
SCM_EXPORT unsigned long SIG_deferred;
SCM_EXPORT SCM exitval;
SCM_EXPORT int cursinit;
SCM_EXPORT unsigned int poll_count, tick_count;
SCM_EXPORT int dumped;
SCM_EXPORT char *execpath;
SCM_EXPORT char s_no_execpath[];
SCM_EXPORT int scm_verbose;
#define verbose (scm_verbose+0)

SCM_EXPORT const char dirsep[];

/* strings used in several source files */

SCM_EXPORT char s_write[], s_newline[], s_system[];
SCM_EXPORT char s_make_string[], s_make_vector[], s_list[], s_op_pipe[];
#define s_string (s_make_string+5)
#define s_vector (s_make_vector+5)
#define s_pipe (s_op_pipe+5)
SCM_EXPORT char s_make_sh_array[];
SCM_EXPORT char s_array_fill[];
#define s_array (s_make_sh_array+12)
SCM_EXPORT char s_ccl[];
#define s_limit (s_ccl+10)
SCM_EXPORT char s_close_port[];
#define s_port_type (s_close_port+6)
SCM_EXPORT char s_call_cc[];
#define s_cont (s_call_cc+18)
SCM_EXPORT char s_try_create_file[];
SCM_EXPORT char s_badenv[];

SCM_EXPORT void (*init_user_scm) P((void));

/* function prototypes */

SCM_EXPORT void (* deferred_proc) P((void));
SCM_EXPORT void process_signals P((void));
SCM_EXPORT int  handle_it P((int i));
SCM_EXPORT SCM  must_malloc_cell P((long len, SCM c, const char *what));
SCM_EXPORT void must_realloc_cell P((SCM z, long olen, long len, const char *what));
SCM_EXPORT char *must_malloc P((long len, const char *what));
SCM_EXPORT char *must_realloc P((char *where, long olen, long len, const char *what));
SCM_EXPORT void must_free P((char *obj, sizet len));
SCM_EXPORT void scm_protect_temp P((SCM *ptr));
SCM_EXPORT long ilength P((SCM sx));
SCM_EXPORT SCM  hash P((SCM obj, SCM n));
SCM_EXPORT SCM  hashv P((SCM obj, SCM n));
SCM_EXPORT SCM  hashq P((SCM obj, SCM n));
SCM_EXPORT SCM  obhash P((SCM obj));
SCM_EXPORT SCM  obunhash P((SCM obj));
SCM_EXPORT unsigned long strhash P((unsigned char *str, sizet len, unsigned long n));
SCM_EXPORT unsigned long hasher P((SCM obj, unsigned long n, sizet d));
SCM_EXPORT SCM  lroom P((SCM args));
SCM_EXPORT void lfflush P((SCM port));
SCM_EXPORT SCM  scm_force_output P((SCM port));
SCM_EXPORT void scm_init_gra P((scm_gra *gra, sizet eltsize, sizet len,
                        sizet maxlen, const char *what));
SCM_EXPORT int  scm_grow_gra P((scm_gra *gra, char *elt));
SCM_EXPORT void scm_trim_gra P((scm_gra *gra));
SCM_EXPORT void scm_free_gra P((scm_gra *gra));
SCM_EXPORT long newsmob P((smobfuns *smob));
SCM_EXPORT long newptob P((ptobfuns *ptob));
SCM_EXPORT SCM  scm_port_entry P((FILE *stream, long ptype, long flags));
SCM_EXPORT SCM  scm_open_ports P((void));
SCM_EXPORT void prinport P((SCM exp, SCM port, char *type));
SCM_EXPORT SCM  repl P((void));
SCM_EXPORT void repl_report P((void));
SCM_EXPORT void growth_mon P((char *obj, long size, char *units, int grewp));
SCM_EXPORT void gc_start P((const char *what));
SCM_EXPORT void gc_end P((void));
SCM_EXPORT void gc_mark P((SCM p));
SCM_EXPORT void scm_gc_hook P((void));
SCM_EXPORT SCM     scm_gc_protect P((SCM obj));
SCM_EXPORT SCM  scm_add_finalizer P((SCM value, SCM finalizer));
SCM_EXPORT void scm_run_finalizers P((int exiting));
SCM_EXPORT void    scm_egc_start P((void));
SCM_EXPORT void    scm_egc_end P((void));
SCM_EXPORT void heap_report P((void));
SCM_EXPORT void gra_report P((void));
SCM_EXPORT void exit_report P((void));
SCM_EXPORT void stack_report P((void));
SCM_EXPORT SCM  scm_stack_trace P((SCM contin));
SCM_EXPORT SCM  scm_scope_trace P((SCM env));
SCM_EXPORT SCM  scm_frame_trace P((SCM contin, SCM nf));
SCM_EXPORT SCM  scm_frame2env P((SCM contin, SCM nf));
SCM_EXPORT SCM  scm_frame_eval P((SCM contin, SCM nf, SCM expr));
SCM_EXPORT void scm_iprin1 P((SCM exp, SCM port, int writing));
SCM_EXPORT void scm_intprint P((long n, int radix, SCM port));
SCM_EXPORT void scm_iprlist P((char *hdr, SCM exp, int tlr, SCM port, int writing));
SCM_EXPORT SCM  scm_env_lookup P((SCM var, SCM stenv));
SCM_EXPORT SCM  scm_env_rlookup P((SCM addr, SCM stenv, const char *what));
SCM_EXPORT SCM  scm_env_getprop P((SCM prop, SCM env));
SCM_EXPORT SCM  scm_env_addprop P((SCM prop, SCM val, SCM env));
SCM_EXPORT long num_frames P((SCM estk, int i));
SCM_EXPORT SCM  *estk_frame P((SCM estk, int i, int nf));
SCM_EXPORT SCM  *cont_frame P((SCM contin, int nf));
SCM_EXPORT SCM  stacktrace1 P((SCM estk, int i));
SCM_EXPORT void scm_princode P((SCM code, SCM env, SCM port, int writing));
SCM_EXPORT void scm_princlosure P((SCM proc, SCM port, int writing));
SCM_EXPORT void lputc P((int c, SCM port));
SCM_EXPORT void lputs P((const char *s, SCM port));
SCM_EXPORT sizet        lfwrite P((char *ptr, sizet size, sizet nitems, SCM port));
SCM_EXPORT int  lgetc P((SCM port));
SCM_EXPORT void lungetc P((int c, SCM port));
SCM_EXPORT char *grow_tok_buf P((SCM tok_buf));
SCM_EXPORT long mode_bits P((char *modes, char *cmodes));
SCM_EXPORT long time_in_msec P((long x));
SCM_EXPORT SCM  my_time P((void));
SCM_EXPORT SCM  your_time P((void));
SCM_EXPORT void init_iprocs P((iproc *subra, int type));

SCM_EXPORT void final_scm P((int));
SCM_EXPORT void init_sbrk P((void));
SCM_EXPORT int  init_buf0 P((FILE *inport));
SCM_EXPORT void scm_init_from_argv P((int argc, const char * const *argv, char *script_arg,
                              int iverbose, int buf0stdin));
SCM_EXPORT void init_signals P((void));
SCM_EXPORT SCM  scm_top_level P((char *initpath, SCM (*toplvl_fun)()));
SCM_EXPORT void restore_signals P((void));
SCM_EXPORT void free_storage P((void));
SCM_EXPORT char *dld_find_executable P((const char* command));
SCM_EXPORT char *scm_find_execpath P((int argc, const char * const *argv, const char *script_arg));
SCM_EXPORT void init_scm P((int iverbose, int buf0stdin, long init_heap_size));
SCM_EXPORT void    scm_init_INITS P((void));
SCM_EXPORT SCM  scm_init_extensions P((void));
SCM_EXPORT void ignore_signals P((void));
SCM_EXPORT void unignore_signals P((void));

SCM_EXPORT void add_feature P((char *str));
SCM_EXPORT int  raprin1 P((SCM exp, SCM port, int writing));
SCM_EXPORT SCM  markcdr P((SCM ptr));
#define mark0 (0)    /*SCM mark0 P((SCM ptr)); */
SCM_EXPORT SCM  equal0 P((SCM ptr1, SCM ptr2));
SCM_EXPORT sizet        free0 P((CELLPTR ptr));
SCM_EXPORT void scm_warn P((char *str1, char *str2, SCM obj));
SCM_EXPORT void everr P((SCM exp, SCM env, SCM arg, const char *pos, const char *s_subr, int codep));
SCM_EXPORT void wta P((SCM arg, const char *pos, const char *s_subr));
SCM_EXPORT void scm_experr P((SCM arg, const char *pos, const char *s_subr));
SCM_EXPORT SCM  intern P((char *name, sizet len));
SCM_EXPORT SCM  sysintern P((const char *name, SCM val));
SCM_EXPORT SCM  sym2vcell P((SCM sym));
SCM_EXPORT SCM  makstr P((long len));
SCM_EXPORT SCM  scm_maksubr P((const char *name, int type, SCM (*fcn)()));
SCM_EXPORT SCM  make_subr P((const char *name, int type, SCM (*fcn)()));
SCM_EXPORT SCM  make_synt P((const char *name, long flags, SCM (*fcn)()));
SCM_EXPORT SCM  make_gsubr P((const char *name, int req, int opt, int rst,
                      SCM (*fcn)()));
SCM_EXPORT SCM  closure P((SCM code, int nargs));
SCM_EXPORT SCM  makprom P((SCM code));
SCM_EXPORT SCM  force P((SCM x));
SCM_EXPORT SCM  makarb P((SCM name));
SCM_EXPORT SCM  tryarb P((SCM arb));
SCM_EXPORT SCM  relarb P((SCM arb));
SCM_EXPORT SCM  ceval P((SCM x, SCM static_env, SCM env));
SCM_EXPORT SCM  scm_wrapcode P((SCM code, SCM env));
SCM_EXPORT SCM  scm_current_env P((void));
SCM_EXPORT SCM  prolixity P((SCM arg));
SCM_EXPORT SCM  gc_for_newcell P((void));
SCM_EXPORT void gc_for_open_files P((void));
SCM_EXPORT SCM  gc P((SCM arg));
SCM_EXPORT SCM  tryload P((SCM filename, SCM reader));
SCM_EXPORT SCM  acons P((SCM w, SCM x, SCM y));
SCM_EXPORT SCM  cons2 P((SCM w, SCM x, SCM y));
SCM_EXPORT SCM  resizuve P((SCM vect, SCM len));
SCM_EXPORT SCM  lnot P((SCM x));
SCM_EXPORT SCM  booleanp P((SCM obj));
SCM_EXPORT SCM  eq P((SCM x, SCM y));
SCM_EXPORT SCM  equal P((SCM x, SCM y));
SCM_EXPORT SCM  consp P((SCM x));
SCM_EXPORT SCM  cons P((SCM x, SCM y));
SCM_EXPORT SCM  nullp P((SCM x));
SCM_EXPORT SCM  setcar P((SCM pair, SCM value));
SCM_EXPORT SCM  setcdr P((SCM pair, SCM value));
SCM_EXPORT SCM  listp P((SCM x));
SCM_EXPORT SCM  list P((SCM objs));
SCM_EXPORT SCM  length P((SCM x));
SCM_EXPORT SCM  append P((SCM args));
SCM_EXPORT SCM  reverse P((SCM lst));
SCM_EXPORT SCM  list_ref P((SCM lst, SCM k));
SCM_EXPORT SCM  memq P((SCM x, SCM lst));
SCM_EXPORT SCM  member P((SCM x, SCM lst));
SCM_EXPORT SCM  memv P((SCM x, SCM lst));
SCM_EXPORT SCM  assq P((SCM x, SCM alist));
SCM_EXPORT SCM  assoc P((SCM x, SCM alist));
SCM_EXPORT SCM  symbolp P((SCM x));
SCM_EXPORT SCM  symbol2string P((SCM s));
SCM_EXPORT SCM  string2symbol P((SCM s));
SCM_EXPORT SCM  string_copy P((SCM s));
SCM_EXPORT SCM  numberp P((SCM x));
SCM_EXPORT SCM  exactp P((SCM x));
SCM_EXPORT SCM  inexactp P((SCM x));
SCM_EXPORT SCM  eqp P((SCM x, SCM y));
SCM_EXPORT SCM  lessp P((SCM x, SCM y));
SCM_EXPORT SCM  greaterp P((SCM x, SCM y));
SCM_EXPORT SCM  leqp P((SCM x, SCM y));
SCM_EXPORT SCM  greqp P((SCM x, SCM y));
SCM_EXPORT SCM  zerop P((SCM z));
SCM_EXPORT SCM  positivep P((SCM x));
SCM_EXPORT SCM  negativep P((SCM x));
SCM_EXPORT SCM  oddp P((SCM n));
SCM_EXPORT SCM  evenp P((SCM n));
SCM_EXPORT SCM  lmax P((SCM x, SCM y));
SCM_EXPORT SCM  lmin P((SCM x, SCM y));
SCM_EXPORT SCM  sum P((SCM x, SCM y));
SCM_EXPORT SCM  difference P((SCM x, SCM y));
SCM_EXPORT SCM  product P((SCM x, SCM y));
SCM_EXPORT SCM  divide P((SCM x, SCM y));
SCM_EXPORT SCM  lquotient P((SCM x, SCM y));
SCM_EXPORT SCM  scm_iabs P((SCM x));
SCM_EXPORT SCM  scm_abs P((SCM x));
SCM_EXPORT SCM  lremainder P((SCM x, SCM y));
SCM_EXPORT SCM  modulo P((SCM x, SCM y));
SCM_EXPORT SCM  lgcd P((SCM x, SCM y));
SCM_EXPORT SCM  llcm P((SCM n1, SCM n2));
SCM_EXPORT SCM  number2string P((SCM x, SCM radix));
SCM_EXPORT SCM  istring2number P((char *str, long len, long radix));
SCM_EXPORT SCM  string2number P((SCM str, SCM radix));
SCM_EXPORT SCM  istr2flo P((char *str, long len, long radix));
SCM_EXPORT SCM  mkbig P((sizet nlen, int sign));
SCM_EXPORT SCM  mkstrport P((SCM pos, SCM str, long modes, char *caller));
SCM_EXPORT SCM  mksafeport P((int maxlen, SCM port));
SCM_EXPORT int  reset_safeport P((SCM sfp, int maxlen, SCM port));
SCM_EXPORT SCM  long2big P((long n));
SCM_EXPORT SCM  ulong2big P((unsigned long n));
SCM_EXPORT SCM  big2inum P((SCM b, sizet l));
SCM_EXPORT sizet iint2str P((long num, int rad, char *p));
SCM_EXPORT SCM  floequal P((SCM x, SCM y));
SCM_EXPORT SCM  uve_equal P((SCM u, SCM v));
SCM_EXPORT SCM  uve_read P((SCM v, SCM port));
SCM_EXPORT SCM  uve_write P((SCM v, SCM port));
SCM_EXPORT SCM  raequal P((SCM ra0, SCM ra1));
SCM_EXPORT SCM  array_equal P((SCM u, SCM v));
SCM_EXPORT SCM  array_rank P((SCM ra));
SCM_EXPORT int  rafill P((SCM ra, SCM fill, SCM ignore));
SCM_EXPORT SCM  uve_fill P((SCM uve, SCM fill));
SCM_EXPORT SCM  array_fill P((SCM ra, SCM fill));
SCM_EXPORT SCM  array_prot P((SCM ra));
SCM_EXPORT SCM  array_rank P((SCM ra));
SCM_EXPORT SCM  array_contents P((SCM ra, SCM strict));
SCM_EXPORT int  bigprint P((SCM exp, SCM port, int writing));
SCM_EXPORT int  floprint P((SCM sexp, SCM port, int writing));
SCM_EXPORT SCM  istr2int P((char *str, long len, long radix));
SCM_EXPORT SCM  istr2bve P((char *str, long len));
SCM_EXPORT void scm_ipruk P((char *hdr, SCM ptr, SCM port));
SCM_EXPORT SCM  charp P((SCM x));
SCM_EXPORT SCM  char_lessp P((SCM x, SCM y));
SCM_EXPORT SCM  chci_eq P((SCM x, SCM y));
SCM_EXPORT SCM  chci_lessp P((SCM x, SCM y));
SCM_EXPORT SCM  char_alphap P((SCM chr));
SCM_EXPORT SCM  char_nump P((SCM chr));
SCM_EXPORT SCM  char_whitep P((SCM chr));
SCM_EXPORT SCM  char_upperp P((SCM chr));
SCM_EXPORT SCM  char_lowerp P((SCM chr));
SCM_EXPORT SCM  char2int P((SCM chr));
SCM_EXPORT SCM  int2char P((SCM n));
SCM_EXPORT SCM  char_upcase P((SCM chr));
SCM_EXPORT SCM  char_downcase P((SCM chr));
SCM_EXPORT SCM  stringp P((SCM x));
SCM_EXPORT SCM  string P((SCM chrs));
SCM_EXPORT SCM  make_string P((SCM k, SCM chr));
SCM_EXPORT SCM  string2list P((SCM str));
SCM_EXPORT SCM  st_length P((SCM str));
SCM_EXPORT SCM  st_ref P((SCM str, SCM k));
SCM_EXPORT SCM  st_set P((SCM str, SCM k, SCM chr));
SCM_EXPORT SCM  st_equal P((SCM s1, SCM s2));
SCM_EXPORT SCM  stci_equal P((SCM s1, SCM s2));
SCM_EXPORT SCM  st_lessp P((SCM s1, SCM s2));
SCM_EXPORT SCM  stci_lessp P((SCM s1, SCM s2));
SCM_EXPORT SCM  substring P((SCM str, SCM start, SCM end));
SCM_EXPORT SCM  st_append P((SCM args));
SCM_EXPORT SCM  vectorp P((SCM x));
SCM_EXPORT SCM  vector_length P((SCM v));
SCM_EXPORT SCM  vector P((SCM l));
SCM_EXPORT SCM  vector_ref P((SCM v, SCM k));
SCM_EXPORT SCM  vector_set P((SCM v, SCM k, SCM obj));
SCM_EXPORT SCM  make_vector P((SCM k, SCM fill));
SCM_EXPORT SCM  vector2list P((SCM v));
SCM_EXPORT SCM  for_each P((SCM proc, SCM arg1, SCM args));
SCM_EXPORT SCM  procedurep P((SCM obj));
SCM_EXPORT SCM  apply P((SCM proc, SCM arg1, SCM args));
SCM_EXPORT SCM  scm_cvapply P((SCM proc, long n, SCM *argv));
SCM_EXPORT int  scm_arity_check P((SCM proc, long argc, const char *what));
SCM_EXPORT SCM  map P((SCM proc, SCM arg1, SCM args));
SCM_EXPORT SCM  scm_make_cont P((void));
SCM_EXPORT SCM  copytree P((SCM obj));
SCM_EXPORT SCM  eval P((SCM obj));
SCM_EXPORT SCM  scm_values P((SCM arg1, SCM arg2, SCM rest, const char *what));
SCM_EXPORT SCM  scm_eval_values P((SCM x, SCM static_env, SCM env));
SCM_EXPORT SCM  identp P((SCM obj));
SCM_EXPORT SCM  ident2sym P((SCM id));
SCM_EXPORT SCM  ident_eqp P((SCM id1, SCM id2, SCM env));
SCM_EXPORT int  scm_nullenv_p P((SCM env));
SCM_EXPORT SCM  env2tree P((SCM env));
SCM_EXPORT SCM  renamed_ident P((SCM id, SCM env));
SCM_EXPORT SCM  scm_check_linum P((SCM x, SCM *linum));
SCM_EXPORT SCM  scm_add_linum P((SCM linum, SCM x));
SCM_EXPORT SCM  input_portp P((SCM x));
SCM_EXPORT SCM  output_portp P((SCM x));
SCM_EXPORT SCM  cur_input_port P((void));
SCM_EXPORT SCM  cur_output_port P((void));
SCM_EXPORT SCM  i_setbuf0 P((SCM port));
SCM_EXPORT SCM  try_open_file P((SCM filename, SCM modes));
SCM_EXPORT SCM  open_file P((SCM filename, SCM modes));
SCM_EXPORT SCM  open_pipe P((SCM pipestr, SCM modes));
SCM_EXPORT SCM  close_port P((SCM port));
SCM_EXPORT SCM	scm_file_position P((SCM port, SCM pos));
#define file_position(port) scm_file_position(port, BOOL_F)
#define file_set_position scm_file_position
SCM_EXPORT SCM  scm_read P((SCM port));
SCM_EXPORT SCM  scm_read_char P((SCM port));
SCM_EXPORT SCM  scm_peek_char P((SCM port));
SCM_EXPORT SCM  eof_objectp P((SCM x));
SCM_EXPORT int  scm_io_error P((SCM port, const char *what));
SCM_EXPORT SCM  scm_write P((SCM obj, SCM port));
SCM_EXPORT SCM  scm_display P((SCM obj, SCM port));
SCM_EXPORT SCM  scm_newline P((SCM port));
SCM_EXPORT SCM  scm_write_char P((SCM chr, SCM port));
SCM_EXPORT SCM  scm_port_line P((SCM port));
SCM_EXPORT SCM  scm_port_col P((SCM port));
SCM_EXPORT void scm_line_msg P((SCM file, SCM linum, SCM port));
SCM_EXPORT void scm_err_line P((const char *what, SCM file, SCM linum, SCM port));
SCM_EXPORT SCM  lgetenv P((SCM nam));
SCM_EXPORT SCM  prog_args P((void));
SCM_EXPORT SCM  makacro P((SCM code));
SCM_EXPORT SCM  makmacro P((SCM code));
SCM_EXPORT SCM  makmmacro P((SCM code));
SCM_EXPORT SCM  makidmacro P((SCM code));
SCM_EXPORT void poll_routine P((void));
SCM_EXPORT void tick_signal P((void));
SCM_EXPORT void stack_check P((void));
SCM_EXPORT SCM  list2ura P((SCM ndim, SCM prot, SCM lst));
SCM_EXPORT SCM  make_ra P((int ndim));
SCM_EXPORT SCM  makflo P((float x));
SCM_EXPORT SCM  arrayp P((SCM v, SCM prot));
SCM_EXPORT SCM  aset P((SCM v, SCM obj, SCM args));
SCM_EXPORT SCM  aref P((SCM v, SCM args));
SCM_EXPORT SCM     scm_array_ref P((SCM args));
SCM_EXPORT SCM  cvref P((SCM v, sizet pos, SCM last));
SCM_EXPORT SCM  quit P((SCM n));
#ifdef CAREFUL_INTS
SCM_EXPORT void ints_viol P((ints_infot *info, int sense));
SCM_EXPORT void    ints_warn P((char *s1, char* s2, char *fname, int linum));
#endif
SCM_EXPORT void add_final P((void (*final)(void)));
SCM_EXPORT SCM  makcclo P((SCM proc, long len));
SCM_EXPORT SCM  make_uve P((long k, SCM prot));
SCM_EXPORT long scm_prot2type P((SCM prot));
SCM_EXPORT long aind P((SCM ra, SCM args, const char *what));
SCM_EXPORT SCM  scm_eval_string P((SCM str));
SCM_EXPORT SCM  scm_load_string P((SCM str));
SCM_EXPORT SCM  scm_unexec P((const SCM pathname));
SCM_EXPORT SCM     scm_logbitp  P((SCM index, SCM j1));
SCM_EXPORT SCM     scm_logtest  P((SCM x, SCM y));
SCM_EXPORT SCM     scm_logxor P((SCM x, SCM y));
SCM_EXPORT SCM     scm_logand P((SCM x, SCM y));
SCM_EXPORT SCM     scm_logior P((SCM x, SCM y));
SCM_EXPORT SCM     scm_lognot P((SCM n));
SCM_EXPORT SCM     scm_intexpt P((SCM z1, SCM z2));
SCM_EXPORT SCM     scm_ash P((SCM n, SCM cnt));
SCM_EXPORT SCM     scm_bitfield P((SCM n, SCM start, SCM end));
SCM_EXPORT SCM     scm_logcount P((SCM n));
SCM_EXPORT SCM     scm_intlength P((SCM n));
SCM_EXPORT SCM     scm_copybit P((SCM index, SCM j1, SCM bit));
SCM_EXPORT SCM  scm_bitif P((SCM mask, SCM n0, SCM n1));
SCM_EXPORT SCM  scm_copybitfield P((SCM to, SCM start, SCM rest));

                                /* Defined in "rope.c" */
SCM_EXPORT SCM   long2num P((long n));
SCM_EXPORT SCM  ulong2num P((unsigned long n));
SCM_EXPORT unsigned char  num2uchar  P((SCM num, char *pos, char *s_caller));
SCM_EXPORT   signed char  num2char   P((SCM num, char *pos, char *s_caller));
SCM_EXPORT unsigned short num2ushort P((SCM num, char *pos, char *s_caller));
SCM_EXPORT          short num2short  P((SCM num, char *pos, char *s_caller));
SCM_EXPORT unsigned long  num2ulong  P((SCM num, char *pos, char *s_caller));
SCM_EXPORT          long  num2long   P((SCM num, char *pos, char *s_caller));
SCM_EXPORT         double num2dbl    P((SCM num, char *pos, char *s_caller));
SCM_EXPORT SCM  makfromstr  P((const char *src, sizet len));
SCM_EXPORT SCM  makfromstrs P((int argc, const char * const *argv));
SCM_EXPORT SCM  makfrom0str P((const char *scr));
SCM_EXPORT char **makargvfrmstrs P((SCM args, const char *s_v));
SCM_EXPORT void must_free_argv P((char **argv));
SCM_EXPORT SCM  scm_evstr  P((char *str));
SCM_EXPORT void scm_ldstr  P((char *str));
SCM_EXPORT int  scm_ldfile P((char *path));
SCM_EXPORT int  scm_ldprog P((char *path));
SCM_EXPORT unsigned long scm_addr P((SCM args, const char *name));
SCM_EXPORT unsigned long scm_base_addr P((SCM v, const char *name));
SCM_EXPORT int  scm_cell_p P((SCM x));

#ifdef FLOATS
SCM_EXPORT SCM  makdbl P((double x, double y));
SCM_EXPORT SCM  dbl2big P((double d));
SCM_EXPORT double       big2dbl P((SCM b));
SCM_EXPORT double       scm_truncate P((double x));
SCM_EXPORT double       scm_round P((double x));
SCM_EXPORT double       floident P((double x));
#endif

#ifdef BIGDIG
SCM_EXPORT void longdigs P((long x, BIGDIG digs[DIGSPERLONG]));
SCM_EXPORT SCM  adjbig P((SCM b, sizet nlen));
SCM_EXPORT SCM  normbig P((SCM b));
SCM_EXPORT SCM  copybig P((SCM b, int sign));
SCM_EXPORT SCM  addbig P((BIGDIG *x, sizet nx, int xsgn, SCM bigy, int sgny));
SCM_EXPORT SCM  mulbig P((BIGDIG *x, sizet nx, BIGDIG *y, sizet ny, int sgn));
SCM_EXPORT unsigned int divbigdig P((BIGDIG *ds, sizet h, BIGDIG div));
SCM_EXPORT SCM  divbigint P((SCM x, long z, int sgn, int mode));
SCM_EXPORT SCM  divbigbig P((BIGDIG *x, sizet nx, BIGDIG *y, sizet ny, int sgn,
                   int modes));
SCM_EXPORT long  pseudolong P((long x));
#endif
SCM_EXPORT int  bigcomp P((SCM x, SCM y));
SCM_EXPORT SCM  bigequal P((SCM x, SCM y));
SCM_EXPORT int  scm_bigdblcomp P((SCM b, double d));

/* "script.c" functions */
SCM_EXPORT char *       scm_cat_path P((char *str1, const char *str2, long n));
SCM_EXPORT char *       scm_try_path P((char *path));
SCM_EXPORT char *       script_find_executable P((const char *command));
SCM_EXPORT char **      script_process_argv P((int argc, const char **argv));
SCM_EXPORT int  script_count_argv P((const char **argv));
SCM_EXPORT char *       find_impl_file P((const char *exec_path, const char *generic_name,
                          const char *initname, const char *sep));

/* environment cache functions */
SCM_EXPORT void scm_ecache_report P((void));
SCM_EXPORT void scm_estk_reset P((sizet size));
SCM_EXPORT void scm_env_cons P((SCM x, SCM y));
SCM_EXPORT void scm_env_cons2 P((SCM w, SCM x, SCM y));
SCM_EXPORT void scm_env_cons3 P((SCM v, SCM w, SCM x, SCM y));
SCM_EXPORT void scm_env_v2lst P((long argc, SCM *argv));
SCM_EXPORT void scm_extend_env P((void));
SCM_EXPORT void scm_egc P((void));

/* Global state for environment cache */
SCM_EXPORT CELLPTR scm_ecache;
SCM_EXPORT VOLATILE long scm_ecache_index, scm_ecache_len;
SCM_EXPORT SCM scm_env, scm_env_tmp;
SCM_EXPORT SCM scm_egc_roots[];
SCM_EXPORT VOLATILE long scm_egc_root_index;
SCM_EXPORT SCM scm_estk;
SCM_EXPORT SCM *scm_estk_v, *scm_estk_ptr;
SCM_EXPORT long scm_estk_size;
#ifndef RECKLESS
SCM_EXPORT SCM scm_trace, scm_trace_env;
#endif

#ifdef RECKLESS
# define ASRTER(_cond, _arg, _pos, _subr) ;
# define ASRTGO(_cond, _label) ;
#else
# define ASRTER(_cond, _arg, _pos, _subr) if (SCM_EXPECT_FALSE(!(_cond))) wta(_arg, (char *)(_pos), _subr);
# define ASRTGO(_cond, _label) if (SCM_EXPECT_FALSE(!(_cond))) goto _label;
#endif

#define ARGn            1
#define ARG1            2
#define ARG2            3
#define ARG3            4
#define ARG4            5
#define ARG5            6
  /* following must match entry indexes in errmsgs[] */
#define WNA             7
#define OVFLOW          8
#define OUTOFRANGE      9
#define NALLOC          10
#define THRASH          11
#define EXIT            12
#define HUP_SIGNAL      13
#define INT_SIGNAL      14
#define FPE_SIGNAL      15
#define BUS_SIGNAL      16
#define SEGV_SIGNAL     17
#define ALRM_SIGNAL     18
#define VTALRM_SIGNAL   19
#define PROF_SIGNAL     20

#define EVAL(x, env, venv) (IMP(x)?(x):ceval((x), (SCM)(env), (SCM)(venv)))
#define SIDEVAL(x, env, venv) if (NIMP(x)) ceval((x), (SCM)(env), (SCM)(venv))

#define NEWCELL(_into) {if (IMP(freelist)) _into = gc_for_newcell();\
        else {_into = freelist;freelist = CDR(freelist);++cells_allocated;}}
/*
#define NEWCELL(_into) {DEFER_INTS;if (IMP(freelist)) _into = gc_for_newcell();\
        else {_into = freelist;freelist = CDR(freelist);++cells_allocated;}\
        ALLOW_INTS;}
*/

#ifdef __cplusplus
}
#endif
