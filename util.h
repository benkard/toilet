#import "runtime-compatibility.h"
#import "functions.h"
#import <Foundation/NSException.h>
#import <Foundation/NSNull.h>
#import "MLKSymbol.h"

#define DEFINE_GMP_OPERATION(SIGNATURE, TYPE, GMPOP, RETTYPE, OBJTYPE, CONSTRUCTOR) \
  -(RETTYPE *) SIGNATURE                                                \
  {                                                                     \
    TYPE##_t mpval;                                                     \
    RETTYPE *result;                                                    \
                                                                        \
    TYPE##_init (mpval);                                                \
    GMPOP;                                                              \
    result = [OBJTYPE CONSTRUCTOR mpval];                               \
    TYPE##_clear (mpval);                                               \
                                                                        \
    return result;                                                      \
  }


// Read this as “Lisp ASSIGN” etc..
#define LASSIGN(VAR, VALUE)                             \
  ({ LRELEASE (VAR); VAR = VALUE; LRETAIN (VAR); })

#define LASSIGN_COPY(VAR, VALUE)                                \
  ({                                                            \
    id ___object = VALUE;                                       \
    LRELEASE (VAR);                                             \
    if (MLKInstanceP (___object))                               \
      {                                                         \
        VAR = [___object copy];                                 \
        RETAIN (VAR);                                           \
      }                                                         \
    else                                                        \
      VAR = ___object;                                          \
  })

#define LAUTORELEASE(VALUE)                                             \
  ({ id __object = VALUE;                                               \
     MLKInstanceP (__object) ? (id)AUTORELEASE(__object) : (id)__object; })

#define LDESTROY(VAR)                           \
  ({ LRELEASE (VAR); VAR = nil; })

#define LRELEASE(VALUE)                         \
  ({ id __object = VALUE;                       \
     if (__object) RELEASE(__object); })

#define LRETAIN(VALUE)                                                  \
  ({ id __object = VALUE;                                               \
     MLKInstanceP (__object) ? (id)RETAIN(__object) : (id)__object; })


static id nullify (id value) __attribute__ ((pure, unused));
static id denullify (id value) __attribute__ ((pure, unused));
static id stringify (id value) __attribute__ ((pure, unused));

static id nullify (id value)
{
  if (value)
    return value;
  else
    return [NSNull null];
}

static id denullify (id value)
{
  if (value == [NSNull null])
    return nil;
  else
    return value;
}

static id stringify (id thing)
{
  // FIXME: Some cases may be missing.
  if (!thing)
    return @"NIL";
  if ([thing isKindOfClass:[NSString class]])
    return thing;
  else if ([thing isKindOfClass:[MLKSymbol class]])
    return [thing name];

  [NSException raise:@"MLKTypeError" format:@"Can't coerce %@ to a string.",
                                            MLKPrintToString(thing)];

  return nil;
}
