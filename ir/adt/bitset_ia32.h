

#undef _bitset_inside_clear
#undef _bitset_inside_set
#undef _bitset_inside_flip
#undef _bitset_inside_is_set

#undef _bitset_inside_nlz
#undef _bitset_inside_ntz
#undef _bitset_inside_ntz_value

#define _bitset_inside_set(unit,bit) \
	__asm__( "btsl %1,%0" :"=m" (unit) :"Ir" (bit))

#define _bitset_inside_clear(unit,bit) \
	__asm__( "btrl %1,%0" :"=m" (unit) :"Ir" (bit))

#define _bitset_inside_flip(unit,bit) \
	__asm__( "btcl %1,%0" :"=m" (unit) :"Ir" (bit))

#define _bitset_inside_is_set(unit,bit) _bitset_ia32_inside_is_set(unit, bit)
#define _bitset_inside_nlz(unit) _bitset_ia32_inside_nlz(unit)
#define _bitset_inside_ntz(unit) _bitset_ia32_inside_ntz(unit)
#define _bitset_inside_ntz_value(unit) _bitset_ia32_inside_ntz_value(unit)

static INLINE int _bitset_ia32_inside_is_set(unsigned long *unit, unsigned bit)
{
	int res = 0;
	__asm__("mov $0,%0\n\tbtl %1,%2\n\tadc $0,%0"
			: "=r" (res)
			: "Ir" (bit), "m" (unit)
			: "cc");
	return res;
}

static INLINE unsigned _bitset_ia32_inside_nlz(unsigned long *unit)
{
	unsigned res = 0;
	__asm__("bsrl %1,%0" :"=r" (res) :"m" (unit));
	return *unit == 0 ? 32 : res;
}

static INLINE unsigned _bitset_ia32_inside_ntz(unsigned long *unit) {
	unsigned res = 0;
	__asm__("bsfl %1,%0" :"=r" (res) :"m" (unit));
	return *unit == 0 ? 32 : res;
}

static INLINE unsigned _bitset_ia32_inside_ntz_value(unsigned long unit) {
	unsigned res = 0;
	__asm__("bsfl %1,%0" :"=r" (res) :"rm" (unit));
	return unit == 0 ? 32 : res;
}

#if defined(__GNUC__) && defined(__SSE2__)

#include <stddef.h>
#include <xmmintrin.h>

#undef _bitset_units
#undef _bitset_overall_size
#undef _bitset_data_ptr

#undef _BITSET_BINOP_UNITS_INC

#undef _bitset_inside_binop_and
#undef _bitset_inside_binop_andnot
#undef _bitset_inside_binop_or
#undef _bitset_inside_binop_xor

#undef _bitset_inside_binop_with_zero_and
#undef _bitset_inside_binop_with_zero_andnot
#undef _bitset_inside_binop_with_zero_or
#undef _bitset_inside_binop_with_zero_xor

#define _bitset_units(highest_bit) (round_up2(highest_bit, 128) / BS_UNIT_SIZE_BITS)

#define _bitset_overall_size(bitset_base_size,highest_bit) \
	((bitset_base_size) + 16 + _bitset_units(highest_bit) * BS_UNIT_SIZE)

#define _bitset_data_ptr(data,bitset_base_size,highest_bit) \
  _bitset_sse_data_ptr(data, bitset_base_size, highest_bit)

static INLINE unsigned long *_bitset_sse_data_ptr(void *data, size_t bitset_base_size,
		unsigned long highest_bit)
{
	ptrdiff_t diff;
	char *units = data;

	diff = (units - (char *) 0) + bitset_base_size;
	diff = round_up2(diff, 16);
	units = (char *) 0 + diff;
	return (unsigned long *) units;
}

#define _BITSET_BINOP_UNITS_INC 4
#define _bitset_inside_binop_and(tgt,src) _bitset_sse_inside_binop_and(tgt,src)
#define _bitset_inside_binop_andnot(tgt,src) _bitset_sse_inside_binop_andnot(tgt,src)
#define _bitset_inside_binop_or(tgt,src) _bitset_sse_inside_binop_or(tgt,src)
#define _bitset_inside_binop_xor(tgt,src) _bitset_sse_inside_binop_xor(tgt,src)

/* and with zero sets everything to zero. */
#define _bitset_inside_binop_with_zero_and(tgt) _bitset_sse_inside_binop_with_zero_and(tgt)

/* And Not with 0 is the identity (its like and with 1) */
#define _bitset_inside_binop_with_zero_andnot(tgt)

/* Or with zero is also the identity */
#define _bitset_inside_binop_with_zero_or(tgt)

/* Xor with 0 is like negation, we have to do it. */
#define _bitset_inside_binop_with_zero_xor(tgt) _bitset_sse_inside_binop_with_zero_xor(tgt)

#define _BITSET_SSE_BINOP(name) \
static INLINE void _bitset_sse_inside_binop_ ## name(unsigned long *tgt, unsigned long *src) \
{ \
	__m128i src_op = _mm_load_si128((__m128i *) src); \
	__m128i tgt_op = _mm_load_si128((__m128i *) tgt); \
	__m128i res = _mm_ ## name ## _si128(tgt_op, src_op); \
	_mm_store_si128((void *) tgt, res); \
}


static INLINE void _bitset_sse_inside_binop_with_zero_and(unsigned long *tgt)
{
	tgt[0] = 0;
	tgt[1] = 0;
	tgt[2] = 0;
	tgt[3] = 0;
}

static INLINE void _bitset_sse_inside_binop_with_zero_xor(unsigned long *tgt)
{
	__m128i src_op = _mm_setzero_si128();
	__m128i tgt_op = _mm_load_si128((void *) tgt);
	__m128i res = _mm_xor_si128(tgt_op, src_op);
	_mm_store_si128((__m128i *) tgt, res);
}

static INLINE void _bitset_sse_inside_binop_andnot(unsigned long *tgt, unsigned long *src)
{
	__m128i src_op = _mm_load_si128((void *) src);
	__m128i tgt_op = _mm_load_si128((void *) tgt);
	__m128i res = _mm_andnot_si128(src_op, tgt_op);
	_mm_store_si128((__m128i *) tgt, res);
}

_BITSET_SSE_BINOP(and)
_BITSET_SSE_BINOP(or)
_BITSET_SSE_BINOP(xor)


#endif
