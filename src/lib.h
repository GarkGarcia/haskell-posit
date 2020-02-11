uint32_t int_to_posit(int64_t x);

uint32_t posit_add(uint32_t a, uint32_t b);
uint32_t posit_sub(uint32_t a, uint32_t b);
uint32_t posit_mul(uint32_t a, uint32_t b);
uint32_t posit_div(uint32_t a, uint32_t b);
uint32_t posit_neg(uint32_t a);

bool posit_eq(uint32_t a, uint32_t b);
bool posit_le(uint32_t a, uint32_t b);
bool posit_lt(uint32_t a, uint32_t b);

