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
