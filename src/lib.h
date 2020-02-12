#include "softposit.h"
#include <stdint.h>
#include <stdbool.h>

#define DEFINE_INT_TO_POSIT(N_BITS) \
uint##N_BITS##_t int_to_posit##N_BITS (int64_t x);

#define DEFINE_BIN_OP(N_BITS, OP) \
uint##N_BITS##_t posit##N_BITS##_##OP (uint##N_BITS##_t a, uint##N_BITS##_t b);

#define DEFINE_NEG(N_BITS) \
uint##N_BITS##_t posit##N_BITS##neg (uint##N_BITS##_t a);

#define DEFINE_ARITH_OPS(N_BITS) \
DEFINE_BIN_OP(N_BITS, add) \
DEFINE_BIN_OP(N_BITS, sub) \
DEFINE_BIN_OP(N_BITS, mul) \
DEFINE_BIN_OP(N_BITS, div) \
DEFINE_NEG(N_BITS)

#define DEFINE_COMP_OP(N_BITS, OP);

#define DEFINE_COMP_OPS(N_BITS) \
DEFINE_COMP_OP(N_BITS, eq) \
DEFINE_COMP_OP(N_BITS, le) \
DEFINE_COMP_OP(N_BITS, lt)

#define DEFINE_ALL_OPS(N_BITS) \
DEFINE_INT_TO_POSIT(N_BITS) \
DEFINE_ARITH_OPS(N_BITS) \
DEFINE_COMP_OPS(N_BITS)

DEFINE_ALL_OPS(8)
DEFINE_ALL_OPS(16)
DEFINE_ALL_OPS(32)

