#include "softposit.h"
#include <stdint.h>
#include <stdbool.h>

#define DEFINE_BIN_OP(NAME, F) \
uint32_t NAME (uint32_t a, uint32_t b) \
{ \
    posit32_t pa, pb; \
\
    pa = castP32(a); \
    pb = castP32(b); \
\
    return castUI(F (pa, pb)); \
}

#define DEFINE_COMP_OP(NAME, F) \
bool NAME (uint32_t a, uint32_t b) \
{ \
    posit32_t pa, pb; \
\
    pa = castP32(a); \
    pb = castP32(b); \
\
    return F (pa, pb); \
}

uint32_t posit_neg(uint32_t a)
{
    posit32_t pa = castP32(a);
    return castUI(negP32(pa));
}

uint32_t int_to_posit(int64_t x)
{
    return castUI(i64_to_p32(x));
}

DEFINE_BIN_OP(posit_add, p32_add)
DEFINE_BIN_OP(posit_sub, p32_sub)
DEFINE_BIN_OP(posit_mul, p32_mul)
DEFINE_BIN_OP(posit_div, p32_div)

DEFINE_COMP_OP(posit_eq, p32_eq)
DEFINE_COMP_OP(posit_le, p32_le)
DEFINE_COMP_OP(posit_lt, p32_lt)

