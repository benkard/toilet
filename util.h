#include "runtime-compatibility.h"
#include <Foundation/NSNull.h>

#define DEFINE_GMP_OPERATION(SIGNATURE, TYPE, GMPOP, OBJTYPE, CONSTRUCTOR) \
  -(OBJTYPE *) SIGNATURE                                                \
  {                                                                     \
    TYPE##_t mpval;                                                     \
    OBJTYPE *result;                                                    \
                                                                        \
    TYPE##_init (mpval);                                                \
    GMPOP;                                                              \
    result = [OBJTYPE CONSTRUCTOR mpval];                               \
    TYPE##_clear (mpval);                                               \
                                                                        \
    return result;                                                      \
  }


static id nullify (id value) __attribute__ ((pure, unused));
static id denullify (id value) __attribute__ ((pure, unused));

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
