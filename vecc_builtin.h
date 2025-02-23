// Hand-written target for VecC C code compilation backend.
//
// TODO:
// https://github.com/reyoung/avx_mathfun/blob/master/avx_mathfun.h
#ifndef VECC_BUILTIN_DEFINED
#define VECC_BUILTIN_DEFINED 1

#ifdef _MSC_VER
#define VECC_MSVC 1
#endif

#include <stdint.h>
#include <math.h>

typedef int8_t   I8;
typedef int16_t  I16;
typedef int32_t  I32;
typedef int64_t  I64;

typedef uint8_t  U8;
typedef uint16_t U16;
typedef uint32_t U32;
typedef uint64_t U64;

typedef int8_t   B8;
typedef int16_t  B16;
typedef int32_t  B32;
typedef int64_t  B64;

typedef float    F32;
typedef double   F64;

typedef struct {
    const char* data;
    int64_t     len;
} String;

#define b8_false  (B8)0
#define b16_false (B16)0
#define b32_false (B32)0
#define b64_false (b64)0
#define b8_true  (B8)0xff
#define b16_true (B16)0xffff
#define b32_true (B32)0xffffffff
#define b64_true (b64)0xffffffffffffffff

#define vecc_big_op static
#define vecc_op static inline

const I32 vector_width = 8;
#define vector_index v8i32_set(0, 1, 2, 3, 4, 5, 6, 7)

#define VECC_LEN(arr) (sizeof(arr) / sizeof(arr[0]))


vecc_op F32 f32_sqrt(F32 a) { return sqrtf(a); }
vecc_op F64 f64_sqrt(F64 a) { return sqrt(a); }
vecc_op F32 f32_sin(F32 a) { return sinf(a); }
vecc_op F64 f64_sin(F64 a) { return sin(a); }
vecc_op F32 f32_cos(F32 a) { return cosf(a); }
vecc_op F64 f64_cos(F64 a) { return cos(a); }
vecc_op F32 f32_rcp(F32 a) { return 1.0 / a; }
vecc_op F64 f64_rcp(F64 a) { return 1.0 / a; }
vecc_op F32 f32_pow(F32 a, F32 b) { return powf(a, b); }

vecc_op F32 f32_round(F32 a) { return roundf(a); }
vecc_op F32 f32_floor(F32 a) { return floorf(a); }
vecc_op F32 f32_ceil (F32 a) { return ceilf(a); }
vecc_op F32 f32_trunc(F32 a) { return truncf(a); }
vecc_op F32 f32_fract(F32 a) { return a - floorf(a); }

vecc_op F64 f64_round(F64 a) { return round(a); }
vecc_op F64 f64_floor(F64 a) { return floor(a); }
vecc_op F64 f64_ceil (F64 a) { return ceil(a); }
vecc_op F64 f64_trunc(F64 a) { return trunc(a); }
vecc_op F64 f64_fract(F64 a) { return a - floor(a); }


vecc_op I8  i8_min (I8  a, I8  b) { return a < b ? a : b; }
vecc_op I16 i16_min(I16 a, I16 b) { return a < b ? a : b; }
vecc_op I32 i32_min(I32 a, I32 b) { return a < b ? a : b; }
vecc_op I64 i64_min(I64 a, I64 b) { return a < b ? a : b; }
vecc_op U8  u8_min (U8  a, U8  b) { return a < b ? a : b; }
vecc_op U16 u16_min(U16 a, U16 b) { return a < b ? a : b; }
vecc_op U32 u32_min(U32 a, U32 b) { return a < b ? a : b; }
vecc_op U64 u64_min(U64 a, U64 b) { return a < b ? a : b; }
vecc_op B8  b8_min (B8  a, B8  b) { return a < b ? a : b; }
vecc_op B16 b16_min(B16 a, B16 b) { return a < b ? a : b; }
vecc_op B32 b32_min(B32 a, B32 b) { return a < b ? a : b; }
vecc_op B64 b64_min(B64 a, B64 b) { return a < b ? a : b; }
vecc_op F32 f32_min(F32 a, F32 b) { return a < b ? a : b; }
vecc_op F64 f64_min(F64 a, F64 b) { return a < b ? a : b; }

vecc_op I8  i8_max (I8  a, I8  b) { return a > b ? a : b; }
vecc_op I16 i16_max(I16 a, I16 b) { return a > b ? a : b; }
vecc_op I32 i32_max(I32 a, I32 b) { return a > b ? a : b; }
vecc_op I64 i64_max(I64 a, I64 b) { return a > b ? a : b; }
vecc_op U8  u8_max (U8  a, U8  b) { return a > b ? a : b; }
vecc_op U16 u16_max(U16 a, U16 b) { return a > b ? a : b; }
vecc_op U32 u32_max(U32 a, U32 b) { return a > b ? a : b; }
vecc_op U64 u64_max(U64 a, U64 b) { return a > b ? a : b; }
vecc_op B8  b8_max (B8  a, B8  b) { return a > b ? a : b; }
vecc_op B16 b16_max(B16 a, B16 b) { return a > b ? a : b; }
vecc_op B32 b32_max(B32 a, B32 b) { return a > b ? a : b; }
vecc_op B64 b64_max(B64 a, B64 b) { return a > b ? a : b; }
vecc_op F32 f32_max(F32 a, F32 b) { return a > b ? a : b; }
vecc_op F64 f64_max(F64 a, F64 b) { return a > b ? a : b; }

vecc_op I8  i8_abs (I8  a) { return a < 0 ? -a : a; }
vecc_op I16 i16_abs(I16 a) { return a < 0 ? -a : a; }
vecc_op I32 i32_abs(I32 a) { return a < 0 ? -a : a; }
vecc_op I64 i64_abs(I64 a) { return a < 0 ? -a : a; }
vecc_op U8  u8_abs (U8  a) { return a < 0 ? -a : a; }
vecc_op U16 u16_abs(U16 a) { return a < 0 ? -a : a; }
vecc_op U32 u32_abs(U32 a) { return a < 0 ? -a : a; }
vecc_op U64 u64_abs(U64 a) { return a < 0 ? -a : a; }
vecc_op B8  b8_abs (B8  a) { return a < 0 ? -a : a; }
vecc_op B16 b16_abs(B16 a) { return a < 0 ? -a : a; }
vecc_op B32 b32_abs(B32 a) { return a < 0 ? -a : a; }
vecc_op B64 b64_abs(B64 a) { return a < 0 ? -a : a; }
vecc_op F32 f32_abs(F32 a) { return a < 0 ? -a : a; }
vecc_op F64 f64_abs(F64 a) { return a < 0 ? -a : a; }

vecc_op I8  i8_clamp (I8  a, I8  lo, I8  hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op I16 i16_clamp(I16 a, I16 lo, I16 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op I32 i32_clamp(I32 a, I32 lo, I32 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op I64 i64_clamp(I64 a, I64 lo, I64 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op U8  u8_clamp (U8  a, U8  lo, U8  hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op U16 u16_clamp(U16 a, U16 lo, U16 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op U32 u32_clamp(U32 a, U32 lo, U32 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op U64 u64_clamp(U64 a, U64 lo, U64 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op B8  b8_clamp (B8  a, B8  lo, B8  hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op B16 b16_clamp(B16 a, B16 lo, B16 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op B32 b32_clamp(B32 a, B32 lo, B32 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op B64 b64_clamp(B64 a, B64 lo, B64 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op F32 f32_clamp(F32 a, F32 lo, F32 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }
vecc_op F64 f64_clamp(F64 a, F64 lo, F64 hi) { if(a < lo) return lo; if(a > hi) return hi; return a; }

vecc_op I8  i8_blend (I8  a, I8  b, B8 mask) { return mask ? b : a; }
vecc_op I16 i16_blend(I16 a, I16 b, B8 mask) { return mask ? b : a; }
vecc_op I32 i32_blend(I32 a, I32 b, B8 mask) { return mask ? b : a; }
vecc_op I64 i64_blend(I64 a, I64 b, B8 mask) { return mask ? b : a; }
vecc_op U8  u8_blend (U8  a, U8  b, B8 mask) { return mask ? b : a; }
vecc_op U16 u16_blend(U16 a, U16 b, B8 mask) { return mask ? b : a; }
vecc_op U32 u32_blend(U32 a, U32 b, B8 mask) { return mask ? b : a; }
vecc_op U64 u64_blend(U64 a, U64 b, B8 mask) { return mask ? b : a; }
vecc_op B8  b8_blend (B8  a, B8  b, B8 mask) { return mask ? b : a; }
vecc_op B16 b16_blend(B16 a, B16 b, B8 mask) { return mask ? b : a; }
vecc_op B32 b32_blend(B32 a, B32 b, B8 mask) { return mask ? b : a; }
vecc_op B64 b64_blend(B64 a, B64 b, B8 mask) { return mask ? b : a; }
vecc_op F32 f32_blend(F32 a, F32 b, B8 mask) { return mask ? b : a; }
vecc_op F64 f64_blend(F64 a, F64 b, B8 mask) { return mask ? b : a; }

#ifdef VECC_MSVC
vecc_op I32 i8_count_trailing_zeros (I8  a) { unsigned long i; _BitScanForward(&i, a); return i; }
vecc_op I32 i16_count_trailing_zeros(I16 a) { unsigned long i; _BitScanForward(&i, a); return i; }
vecc_op I32 i32_count_trailing_zeros(I32 a) { unsigned long i; _BitScanForward(&i, a); return i; }
vecc_op I32 i64_count_trailing_zeros(I64 a) { unsigned long i; _BitScanForward64(&i, a); return i; }
vecc_op I32 u8_count_trailing_zeros (U8  a) { unsigned long i; _BitScanForward(&i, a); return i; }
vecc_op I32 u16_count_trailing_zeros(U16 a) { unsigned long i; _BitScanForward(&i, a); return i; }
vecc_op I32 u32_count_trailing_zeros(U32 a) { unsigned long i; _BitScanForward(&i, a); return i; }
vecc_op I32 u64_count_trailing_zeros(U64 a) { unsigned long i; _BitScanForward64(&i, a); return i; }

vecc_op I32 i8_count_leading_zeros (I8  a) { unsigned long i; _BitScanReverse(&i, a); return i; }
vecc_op I32 i16_count_leading_zeros(I16 a) { unsigned long i; _BitScanReverse(&i, a); return i; }
vecc_op I32 i32_count_leading_zeros(I32 a) { unsigned long i; _BitScanReverse(&i, a); return i; }
vecc_op I32 i64_count_leading_zeros(I64 a) { unsigned long i; _BitScanReverse64(&i, a); return i; }
vecc_op I32 u8_count_leading_zeros (U8  a) { unsigned long i; _BitScanReverse(&i, a); return i; }
vecc_op I32 u16_count_leading_zeros(U16 a) { unsigned long i; _BitScanReverse(&i, a); return i; }
vecc_op I32 u32_count_leading_zeros(U32 a) { unsigned long i; _BitScanReverse(&i, a); return i; }
vecc_op I32 u64_count_leading_zeros(U64 a) { unsigned long i; _BitScanReverse64(&i, a); return i; }
#endif

vecc_big_op void string_print   (String a) { printf("%.*s"  , int(a.len), a.data); }
vecc_big_op void string_println (String a) { printf("%.*s\n", int(a.len), a.data); }
vecc_big_op void i8_print       (I8     a) { printf("%i"  , (I32)a); }
vecc_big_op void i8_println     (I8     a) { printf("%i\n", (I32)a); }
vecc_big_op void i16_print      (I16    a) { printf("%hi"  , a); }
vecc_big_op void i16_println    (I16    a) { printf("%hi\n", a); }
vecc_big_op void i32_print      (I32    a) { printf("%i"  , a); }
vecc_big_op void i32_println    (I32    a) { printf("%i\n", a); }
vecc_big_op void i64_print      (I64    a) { printf("%lli"  , a); }
vecc_big_op void i64_println    (I64    a) { printf("%lli\n", a); }
vecc_big_op void u8_print       (U8     a) { printf("%u"  , (U32)a); }
vecc_big_op void u8_println     (U8     a) { printf("%u\n", (U32)a); }
vecc_big_op void u16_print      (U16    a) { printf("%hu"  , a); }
vecc_big_op void u16_println    (U16    a) { printf("%hu\n", a); }
vecc_big_op void u32_print      (U32    a) { printf("%u"  , a); }
vecc_big_op void u32_println    (U32    a) { printf("%u\n", a); }
vecc_big_op void u64_print      (U64    a) { printf("%llu"  , a); }
vecc_big_op void u64_println    (U64    a) { printf("%llu\n", a); }
vecc_big_op void b8_print       (B8     a) { printf("%s"  , a ? "true" : "false"); }
vecc_big_op void b8_println     (B8     a) { printf("%s\n", a ? "true" : "false"); }
vecc_big_op void b16_print      (B16    a) { printf("%s"  , a ? "true" : "false"); }
vecc_big_op void b16_println    (B16    a) { printf("%s\n", a ? "true" : "false"); }
vecc_big_op void b32_print      (B32    a) { printf("%s"  , a ? "true" : "false"); }
vecc_big_op void b32_println    (B32    a) { printf("%s\n", a ? "true" : "false"); }
vecc_big_op void b64_print      (B64    a) { printf("%s"  , a ? "true" : "false"); }
vecc_big_op void b64_println    (B64    a) { printf("%s\n", a ? "true" : "false"); }
vecc_big_op void f32_print      (F32    a) { printf("%f"  , a); }
vecc_big_op void f32_println    (F32    a) { printf("%f\n", a); }
vecc_big_op void f64_print      (F64    a) { printf("%g"  , a); }
vecc_big_op void f64_println    (F64    a) { printf("%g\n", a); }

#define VECC_AVX2 1

#ifdef VECC_AVX2
#include <immintrin.h>

// TODO: all underlying vector types should be padded up to at least one SSE vector size?

typedef struct { I8      data[2]; } V2I8;
typedef struct { I8      data[4]; } V4I8;
typedef struct { I8      data[8]; } V8I8;
typedef struct { __m128i data[1]; } V16I8;
typedef struct { __m256i data[1]; } V32I8;
typedef struct { __m256i data[2]; } V64I8;

typedef struct { U8      data[2]; } V2U8;
typedef struct { U8      data[4]; } V4U8;
typedef struct { U8      data[8]; } V8U8;
typedef struct { __m128i data[1]; } V16U8;
typedef struct { __m256i data[1]; } V32U8;
typedef struct { __m256i data[2]; } V64U8;

typedef struct { B8      data[2]; } V2B8;
typedef struct { B8      data[4]; } V4B8;
typedef struct { B8      data[8]; } V8B8;
typedef struct { __m128i data[1]; } V16B8;
typedef struct { __m256i data[1]; } V32B8;
typedef struct { __m256i data[2]; } V64B8;


typedef struct { I16     data[2]; } V2I16;
typedef struct { I16     data[4]; } V4I16;
typedef struct { __m128i data[1]; } V8I16;
typedef struct { __m256i data[1]; } V16I16;
typedef struct { __m256i data[2]; } V32I16;
typedef struct { __m256i data[4]; } V64I16;

typedef struct { U16     data[2]; } V2U16;
typedef struct { U16     data[4]; } V4U16;
typedef struct { __m128i data[1]; } V8U16;
typedef struct { __m256i data[1]; } V16U16;
typedef struct { __m256i data[2]; } V32U16;
typedef struct { __m256i data[4]; } V64U16;

typedef struct { B16     data[2]; } V2B16;
typedef struct { B16     data[4]; } V4B16;
typedef struct { __m128i data[1]; } V8B16;
typedef struct { __m256i data[1]; } V16B16;
typedef struct { __m256i data[2]; } V32B16;
typedef struct { __m256i data[4]; } V64B16;


typedef struct { __m128  data[1]; } V2F32;
typedef struct { __m128  data[1]; } V4F32;
typedef struct { __m256  data[1]; } V8F32;
typedef struct { __m256  data[2]; } V16F32;
typedef struct { __m256  data[4]; } V32F32;
typedef struct { __m256  data[8]; } V64F32;

typedef struct { I32     data[2]; } V2I32;
typedef struct { __m128i data[1]; } V4I32;
typedef struct { __m256i data[1]; } V8I32;
typedef struct { __m256i data[2]; } V16I32;
typedef struct { __m256i data[4]; } V32I32;
typedef struct { __m256i data[8]; } V64I32;

typedef struct { U32     data[2]; } V2U32;
typedef struct { __m128i data[1]; } V4U32;
typedef struct { __m256i data[1]; } V8U32;
typedef struct { __m256i data[2]; } V16U32;
typedef struct { __m256i data[4]; } V32U32;
typedef struct { __m256i data[8]; } V64U32;

typedef struct { B32     data[2]; } V2B32;
typedef struct { __m128i data[1]; } V4B32;
typedef struct { __m256i data[1]; } V8B32;
typedef struct { __m256i data[2]; } V16B32;
typedef struct { __m256i data[4]; } V32B32;
typedef struct { __m256i data[8]; } V64B32;


typedef struct { __m128d data[1];  } V2F64;
typedef struct { __m256d data[1];  } V4F64;
typedef struct { __m256d data[2];  } V8F64;
typedef struct { __m256d data[4];  } V16F64;
typedef struct { __m256d data[8];  } V32F64;
typedef struct { __m256d data[16]; } V64F64;

typedef struct { __m128d data[1];  } V2I64;
typedef struct { __m256d data[1];  } V4I64;
typedef struct { __m256d data[2];  } V8I64;
typedef struct { __m256d data[4];  } V16I64;
typedef struct { __m256d data[8];  } V32I64;
typedef struct { __m256d data[16]; } V64I64;

typedef struct { __m128d data[1];  } V2U64;
typedef struct { __m256d data[1];  } V4U64;
typedef struct { __m256d data[2];  } V8U64;
typedef struct { __m256d data[4];  } V16U64;
typedef struct { __m256d data[8];  } V32U64;
typedef struct { __m256d data[16]; } V64U64;

typedef struct { __m128d data[1];  } V2B64;
typedef struct { __m256d data[1];  } V4B64;
typedef struct { __m256d data[2];  } V8B64;
typedef struct { __m256d data[4];  } V16B64;
typedef struct { __m256d data[8];  } V32B64;
typedef struct { __m256d data[16]; } V64B64;

vecc_op F32 f32_rsqrt(F32 a) { return _mm_cvtss_f32(_mm_rsqrt_ss(_mm_set_ss(a))); }
vecc_op F64 f64_rsqrt(F64 a) { return 1.0 / sqrt(a); }

vecc_op V8I32 v8f32_to_v8i32(V8F32 a) {
    return {{_mm256_cvtps_epi32(a.data[0])}};
}

vecc_op V8U32 v8f32_to_v8u32(V8F32 a) {
    return {{_mm256_cvtps_epi32(a.data[0])}};
}

vecc_big_op V8U16 v8f32_to_v8u16(V8F32 a) {
    __m256i a32 = _mm256_cvtps_epi32(a.data[0]);
    return {{_mm256_castsi256_si128(_mm256_packus_epi32(a32, a32))}};
}

vecc_big_op V8I16 v8f32_to_v8i16(V8F32 a) {
    __m256i a32 = _mm256_cvtps_epi32(a.data[0]);
    return {{_mm256_castsi256_si128(_mm256_packs_epi32(a32, a32))}};
}

vecc_op V8F32 v8i32_to_v8f32(V8I32 a) { return {{_mm256_cvtepi32_ps(a.data[0])}}; }

vecc_op V8U32 v8i32_to_v8u32(V8I32 a) { return {{a.data[0]}}; }
vecc_op V8I32 v8u32_to_v8i32(V8U32 a) { return {{a.data[0]}}; }

vecc_op V8F32 v8f32_set1(F32 a) { return {{_mm256_set1_ps(a)}}; }
vecc_op V4F64 v4f64_set1(F64 a) { return {{_mm256_set1_pd(a)}}; }
vecc_op V8I32 v8i32_set1(I32 a) { return {{_mm256_set1_epi32(a)}}; }
vecc_op V8U32 v8u32_set1(U32 a) { return {{_mm256_set1_epi32(a)}}; }
vecc_op V8B32 v8b32_set1(B32 a) { return {{_mm256_set1_epi32(a)}}; }
vecc_op V8U16 v8u16_set1(U16 a) { return {{_mm_set1_epi16(a)}}; }
vecc_op V8U8  v8u8_set1(U8 a) { return {{a,a,a,a,a,a,a,a}}; }

vecc_op V8F32 v8f32_set(F32 a,F32 b,F32 c,F32 d,F32 e,F32 f,F32 g,F32 h) { return {{_mm256_setr_ps(a,b,c,d,e,f,g,h)}}; }
vecc_op V8I32 v8i32_set(I32 a,I32 b,I32 c,I32 d,I32 e,I32 f,I32 g,I32 h) { return {{_mm256_setr_epi32(a,b,c,d,e,f,g,h)}}; }
vecc_op V8U32 v8u32_set(U32 a,U32 b,U32 c,U32 d,U32 e,U32 f,U32 g,U32 h) { return {{_mm256_setr_epi32(a,b,c,d,e,f,g,h)}}; }
vecc_op V8B32 v8b32_set(B32 a,B32 b,B32 c,B32 d,B32 e,B32 f,B32 g,B32 h) { return {{_mm256_setr_epi32(a,b,c,d,e,f,g,h)}}; }

vecc_op V8F32 v8f32_load(const F32* data) { return {{_mm256_loadu_ps(data)}}; }
vecc_op V8I32 v8i32_load(const I32* data) { return {{_mm256_loadu_si256((__m256i*)data)}}; }
vecc_op V8U32 v8u32_load(const U32* data) { return {{_mm256_loadu_si256((__m256i*)data)}}; }
vecc_op V8B32 v8b32_load(const B32* data) { return {{_mm256_loadu_si256((__m256i*)data)}}; }

vecc_op void v8f32_store(F32* dst, V8F32 a) { _mm256_storeu_ps(dst, a.data[0]); }
vecc_op void v8i32_store(I32* dst, V8I32 a) { _mm256_storeu_si256((__m256i*)dst, a.data[0]); }
vecc_op void v8u32_store(U32* dst, V8U32 a) { _mm256_storeu_si256((__m256i*)dst, a.data[0]); }
vecc_op void v8b32_store(B32* dst, V8B32 a) { _mm256_storeu_si256((__m256i*)dst, a.data[0]); }

vecc_op V8F32 v8f32_add (V8F32 a, V8F32 b)  { return {{_mm256_add_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_sub (V8F32 a, V8F32 b)  { return {{_mm256_sub_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_mul (V8F32 a, V8F32 b)  { return {{_mm256_mul_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_div (V8F32 a, V8F32 b)  { return {{_mm256_div_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_and (V8F32 a, V8F32 b)  { return {{_mm256_and_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_or  (V8F32 a, V8F32 b)  { return {{_mm256_or_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_xor (V8F32 a, V8F32 b)  { return {{_mm256_xor_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_neg (V8F32 a)           { return {{_mm256_xor_ps(a.data[0], _mm256_set1_ps(-1.0f))}}; }

vecc_op V4F64 v4f64_add (V4F64 a, V4F64 b)  { return {{_mm256_add_pd(a.data[0], b.data[0])}}; }
vecc_op V4F64 v4f64_sub (V4F64 a, V4F64 b)  { return {{_mm256_sub_pd(a.data[0], b.data[0])}}; }
vecc_op V4F64 v4f64_mul (V4F64 a, V4F64 b)  { return {{_mm256_mul_pd(a.data[0], b.data[0])}}; }
vecc_op V4F64 v4f64_div (V4F64 a, V4F64 b)  { return {{_mm256_div_pd(a.data[0], b.data[0])}}; }
vecc_op V4F64 v4f64_and (V4F64 a, V4F64 b)  { return {{_mm256_and_pd(a.data[0], b.data[0])}}; }
vecc_op V4F64 v4f64_or  (V4F64 a, V4F64 b)  { return {{_mm256_or_pd(a.data[0], b.data[0])}}; }
vecc_op V4F64 v4f64_xor (V4F64 a, V4F64 b)  { return {{_mm256_xor_pd(a.data[0], b.data[0])}}; }

vecc_op V8I32 v8i32_add (V8I32 a, V8I32 b)  { return {{_mm256_add_epi32(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_sub (V8I32 a, V8I32 b)  { return {{_mm256_sub_epi32(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_mul (V8I32 a, V8I32 b)  { return {{_mm256_mullo_epi32(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_and (V8I32 a, V8I32 b)  { return {{_mm256_and_si256(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_or  (V8I32 a, V8I32 b)  { return {{_mm256_or_si256(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_xor (V8I32 a, V8I32 b)  { return {{_mm256_xor_si256(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_andnot(V8I32 a, V8I32 b){ return {{_mm256_andnot_si256(b.data[0], a.data[0])}}; }
vecc_op V8I32 v8i32_neg (V8I32 a)           { return {{_mm256_sub_epi32(_mm256_set1_epi32(0), a.data[0])}}; }

vecc_op V8U32 v8u32_add (V8U32 a, V8U32 b)  { return {{_mm256_add_epi32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_sub (V8U32 a, V8U32 b)  { return {{_mm256_sub_epi32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_mul (V8U32 a, V8U32 b)  { return {{_mm256_mullo_epi32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_and (V8U32 a, V8U32 b)  { return {{_mm256_and_si256(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_or  (V8U32 a, V8U32 b)  { return {{_mm256_or_si256(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_xor (V8U32 a, V8U32 b)  { return {{_mm256_xor_si256(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_sl  (V8U32 a, int b)    { return {{_mm256_slli_epi32(a.data[0], b)}}; }
vecc_op V8U32 v8u32_sr  (V8U32 a, int b)    { return {{_mm256_srli_epi32(a.data[0], b)}}; }
vecc_op V8U32 v8u32_slv (V8U32 a, V8U32 b)  { return {{_mm256_sllv_epi32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_srv (V8U32 a, V8U32 b)  { return {{_mm256_srlv_epi32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_andnot(V8U32 a, V8U32 b){ return {{_mm256_andnot_si256(b.data[0], a.data[0])}}; }
vecc_op V8U32 v8u32_not (V8U32 a)           { return {{_mm256_xor_si256(a.data[0], _mm256_set1_epi32(0xffffffff))}}; }

vecc_op V8B32 v8b32_and (V8B32 a, V8B32 b)  { return {{_mm256_and_si256(a.data[0], b.data[0])}}; }
vecc_op V8B32 v8b32_or  (V8B32 a, V8B32 b)  { return {{_mm256_or_si256(a.data[0], b.data[0])}}; }
vecc_op V8B32 v8b32_xor (V8B32 a, V8B32 b)  { return {{_mm256_xor_si256(a.data[0], b.data[0])}}; }
vecc_op V8B32 v8b32_andnot(V8B32 a, V8B32 b)  { return {{_mm256_andnot_si256(b.data[0], a.data[0])}}; }
vecc_op V8B32 v8b32_not(V8B32 a)  { return {{_mm256_xor_si256(a.data[0], _mm256_set1_epi32(0xffffffff))}}; }

vecc_op V8U16 v8u16_add (V8U16 a, V8U16 b)  { return {{_mm_add_epi16(a.data[0], b.data[0])}}; }
vecc_op V8U16 v8u16_sub (V8U16 a, V8U16 b)  { return {{_mm_sub_epi16(a.data[0], b.data[0])}}; }
// vecc_op V8U16 v8u16_mul (V8U16 a, V8U16 b)  { return {{_mm_mul_epi16(a.data[0], b.data[0])}}; }
vecc_op V8U16 v8u16_and (V8U16 a, V8U16 b)  { return {{_mm_and_si128(a.data[0], b.data[0])}}; }
vecc_op V8U16 v8u16_or  (V8U16 a, V8U16 b)  { return {{_mm_or_si128(a.data[0], b.data[0])}}; }
vecc_op V8U16 v8u16_xor (V8U16 a, V8U16 b)  { return {{_mm_xor_si128(a.data[0], b.data[0])}}; }
vecc_op V8U16 v8u16_sl  (V8U16 a, int b)    { return {{_mm_slli_epi16(a.data[0], b)}}; }
vecc_op V8U16 v8u16_sr  (V8U16 a, int b)    { return {{_mm_srli_epi16(a.data[0], b)}}; }
vecc_op V8U16 v8u16_andnot(V8U16 a, V8U16 b)  { return {{_mm_andnot_si128(b.data[0], a.data[0])}}; }

// Not cool.

vecc_op V8U8 v8u8_add(V8U8 a, V8U8 b) {
    return {{
        (U8)(a.data[0] + b.data[0]),
        (U8)(a.data[1] + b.data[1]),
        (U8)(a.data[2] + b.data[2]),
        (U8)(a.data[3] + b.data[3]),
        (U8)(a.data[4] + b.data[4]),
        (U8)(a.data[5] + b.data[5]),
        (U8)(a.data[6] + b.data[6]),
        (U8)(a.data[7] + b.data[7]),
    }};
}
vecc_op V8U8 v8u8_mul(V8U8 a, V8U8 b) {
    return {{
        (U8)(a.data[0] + b.data[0]),
        (U8)(a.data[1] + b.data[1]),
        (U8)(a.data[2] + b.data[2]),
        (U8)(a.data[3] + b.data[3]),
        (U8)(a.data[4] + b.data[4]),
        (U8)(a.data[5] + b.data[5]),
        (U8)(a.data[6] + b.data[6]),
        (U8)(a.data[7] + b.data[7]),
    }};
}
vecc_op V8U8 v8u8_and(V8U8 a, V8U8 b) {
    return {{
        (U8)(a.data[0] & b.data[0]),
        (U8)(a.data[1] & b.data[1]),
        (U8)(a.data[2] & b.data[2]),
        (U8)(a.data[3] & b.data[3]),
        (U8)(a.data[4] & b.data[4]),
        (U8)(a.data[5] & b.data[5]),
        (U8)(a.data[6] & b.data[6]),
        (U8)(a.data[7] & b.data[7]),
    }};
}
vecc_op V8U8 v8u8_or(V8U8 a, V8U8 b) {
    return {{
        (U8)(a.data[0] | b.data[0]),
        (U8)(a.data[1] | b.data[1]),
        (U8)(a.data[2] | b.data[2]),
        (U8)(a.data[3] | b.data[3]),
        (U8)(a.data[4] | b.data[4]),
        (U8)(a.data[5] | b.data[5]),
        (U8)(a.data[6] | b.data[6]),
        (U8)(a.data[7] | b.data[7]),
    }};
}
vecc_op V8U8 v8u8_xor(V8U8 a, V8U8 b) {
    return {{
        (U8)(a.data[0] ^ b.data[0]),
        (U8)(a.data[1] ^ b.data[1]),
        (U8)(a.data[2] ^ b.data[2]),
        (U8)(a.data[3] ^ b.data[3]),
        (U8)(a.data[4] ^ b.data[4]),
        (U8)(a.data[5] ^ b.data[5]),
        (U8)(a.data[6] ^ b.data[6]),
        (U8)(a.data[7] ^ b.data[7]),
    }};
}
vecc_op V8U8 v8u8_blend(V8U8 a, V8U8 b, V8B32 mask) {
    // this is dumb
    return {{
        u8_blend(a.data[0], b.data[0], _mm256_extract_epi32(mask.data[0], 0)),
        u8_blend(a.data[1], b.data[1], _mm256_extract_epi32(mask.data[0], 1)),
        u8_blend(a.data[2], b.data[2], _mm256_extract_epi32(mask.data[0], 2)),
        u8_blend(a.data[3], b.data[3], _mm256_extract_epi32(mask.data[0], 3)),
        u8_blend(a.data[4], b.data[4], _mm256_extract_epi32(mask.data[0], 4)),
        u8_blend(a.data[5], b.data[5], _mm256_extract_epi32(mask.data[0], 5)),
        u8_blend(a.data[6], b.data[6], _mm256_extract_epi32(mask.data[0], 6)),
        u8_blend(a.data[7], b.data[7], _mm256_extract_epi32(mask.data[0], 7)),
    }};
}


vecc_op V8I32 v8i32_min (V8I32 a, V8I32 b)  { return {{_mm256_min_epi32(a.data[0], b.data[0])}}; }
vecc_op V8I32 v8i32_max (V8I32 a, V8I32 b)  { return {{_mm256_max_epi32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_min (V8U32 a, V8U32 b)  { return {{_mm256_min_epu32(a.data[0], b.data[0])}}; }
vecc_op V8U32 v8u32_max (V8U32 a, V8U32 b)  { return {{_mm256_max_epu32(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_min  (V8F32 a, V8F32 b) { return {{_mm256_min_ps(a.data[0], b.data[0])}}; }
vecc_op V8F32 v8f32_max  (V8F32 a, V8F32 b) { return {{_mm256_max_ps(a.data[0], b.data[0])}}; }

vecc_op V8F32 v8f32_clamp(V8F32 a, V8F32 lo, V8F32 hi) { return v8f32_min(v8f32_max(a, lo), hi); }
vecc_op V8I32 v8i32_clamp(V8I32 a, V8I32 lo, V8I32 hi) { return v8i32_min(v8i32_max(a, lo), hi); }
vecc_op V8U32 v8u32_clamp(V8U32 a, V8U32 lo, V8U32 hi) { return v8u32_min(v8u32_max(a, lo), hi); }

vecc_op V8F32 v8f32_abs  (V8F32 a) { return {{_mm256_and_ps(a.data[0], _mm256_castsi256_ps(_mm256_set1_epi32(0x7FFFFFFF)))}}; }
// vecc_op V8F32 v8f32_sign (V8F32 a) { return {{_mm256_sign_ps  (a.data[0])}}; }
vecc_op V8F32 v8f32_trunc(V8F32 a) { return {{_mm256_round_ps(a.data[0],_MM_FROUND_TO_ZERO|_MM_FROUND_NO_EXC)}}; }
vecc_op V8F32 v8f32_floor(V8F32 a) { return {{_mm256_round_ps(a.data[0],_MM_FROUND_TO_NEG_INF|_MM_FROUND_NO_EXC)}}; }
vecc_op V8F32 v8f32_round(V8F32 a) { return {{_mm256_round_ps(a.data[0],_MM_FROUND_TO_NEAREST_INT|_MM_FROUND_NO_EXC)}}; }
vecc_op V8F32 v8f32_ceil (V8F32 a) { return {{_mm256_round_ps(a.data[0],_MM_FROUND_TO_POS_INF|_MM_FROUND_NO_EXC)}}; }
vecc_op V8F32 v8f32_fract(V8F32 a) { return v8f32_sub(a, v8f32_floor(a)); }
vecc_op V8F32 v8f32_rcp  (V8F32 a) { return {{_mm256_rcp_ps   (a.data[0])}}; }
vecc_op V8F32 v8f32_sqrt (V8F32 a) { return {{_mm256_sqrt_ps  (a.data[0])}}; }
vecc_op V8F32 v8f32_rsqrt(V8F32 a) { return {{_mm256_rsqrt_ps (a.data[0])}}; }

vecc_op V8B32 v8f32_gt(V8F32 a, V8F32 b) { return {{_mm256_castps_si256(_mm256_cmp_ps(a.data[0], b.data[0], _CMP_GT_OQ))}}; }
vecc_op V8B32 v8f32_ge(V8F32 a, V8F32 b) { return {{_mm256_castps_si256(_mm256_cmp_ps(a.data[0], b.data[0], _CMP_GE_OQ))}}; }
vecc_op V8B32 v8f32_lt(V8F32 a, V8F32 b) { return {{_mm256_castps_si256(_mm256_cmp_ps(a.data[0], b.data[0], _CMP_LT_OQ))}}; }
vecc_op V8B32 v8f32_le(V8F32 a, V8F32 b) { return {{_mm256_castps_si256(_mm256_cmp_ps(a.data[0], b.data[0], _CMP_LE_OQ))}}; }

vecc_op V8I32 v8i32_abs(V8I32 a) { return {{_mm256_abs_epi32(a.data[0])}}; }

vecc_op V8B32 v8i32_eq(V8I32 a, V8I32 b) { return {{_mm256_cmpeq_epi32(a.data[0], b.data[0])}}; }
vecc_op V8B32 v8i32_neq(V8I32 a, V8I32 b){ return {{_mm256_xor_si256(_mm256_cmpeq_epi32(a.data[0], b.data[0]), _mm256_set1_epi32(0xffffffff))}}; }
vecc_op V8B32 v8i32_gt(V8I32 a, V8I32 b) { return {{_mm256_cmpgt_epi32(a.data[0], b.data[0])}}; }
vecc_op V8B32 v8i32_ge(V8I32 a, V8I32 b) { return {{_mm256_xor_si256(_mm256_cmpgt_epi32(b.data[0], a.data[0]), _mm256_set1_epi32(0xffffffff))}}; }
vecc_op V8B32 v8i32_lt(V8I32 a, V8I32 b) { return {{_mm256_cmpgt_epi32(b.data[0], a.data[0])}}; }
vecc_op V8B32 v8i32_le(V8I32 a, V8I32 b) { return {{_mm256_xor_si256(_mm256_cmpgt_epi32(a.data[0], b.data[0]), _mm256_set1_epi32(0xffffffff))}}; }

vecc_op V8B32 v8u32_eq(V8U32 a, V8U32 b) { return {{_mm256_cmpeq_epi32(a.data[0], b.data[0])}}; }
vecc_op V8B32 v8u32_neq(V8U32 a, V8U32 b){ return {{_mm256_xor_si256(_mm256_cmpeq_epi32(a.data[0], b.data[0]), _mm256_set1_epi32(0xffffffff))}}; }
// TODO: no epu32 instructions, needs sign trick
// vecc_op V8B32 v8u32_gt(V8I32 a, V8I32 b) { return {{_mm256_cmpgt_epi32(a.data[0], b.data[0])}}; }
// vecc_op V8B32 v8u32_ge(V8I32 a, V8I32 b) { return {{_mm256_xor_si256(_mm256_cmpgt_epi32(b.data[0], a.data[0]), _mm256_set1_epi32(0xffffffff))}}; }
// vecc_op V8B32 v8u32_lt(V8I32 a, V8I32 b) { return {{_mm256_cmpgt_epi32(b.data[0], a.data[0])}}; }
// vecc_op V8B32 v8u32_le(V8I32 a, V8I32 b) { return {{_mm256_xor_si256(_mm256_cmpgt_epi32(a.data[0], b.data[0]), _mm256_set1_epi32(0xffffffff))}}; }


vecc_op V8F32 v8f32_blend(V8F32 a, V8F32 b, V8B32 mask) {
    return {{_mm256_blendv_ps(a.data[0], b.data[0], _mm256_castsi256_ps(mask.data[0]))}};
}
vecc_op V8I32 v8i32_blend(V8I32 a, V8I32 b, V8B32 mask) {
    return {{_mm256_castps_si256(_mm256_blendv_ps(
        _mm256_castsi256_ps(a.data[0]), _mm256_castsi256_ps(b.data[0]), _mm256_castsi256_ps(mask.data[0])
    ))}};
}
vecc_op V8U32 v8u32_blend(V8U32 a, V8U32 b, V8B32 mask) {
    return {{_mm256_castps_si256(_mm256_blendv_ps(
        _mm256_castsi256_ps(a.data[0]), _mm256_castsi256_ps(b.data[0]), _mm256_castsi256_ps(mask.data[0])
    ))}};
}

vecc_op V8B32 v8b32_blend(V8B32 a, V8B32 b, V8B32 mask) {
    return {{_mm256_castps_si256(_mm256_blendv_ps(
        _mm256_castsi256_ps(a.data[0]), _mm256_castsi256_ps(b.data[0]), _mm256_castsi256_ps(mask.data[0])
    ))}};
}

// Slow scalar fallback for now

#define VECC_ALIGNED(n) __declspec(align(n))

vecc_op V8F32 v8f32_sin(V8F32 a) {
    VECC_ALIGNED(32) F32 data[8];
    _mm256_store_ps(data, a.data[0]);
    for (int i = 0; i < VECC_LEN(data); i++) {
        data[i] = sinf(data[i]);
    }
    return {{_mm256_load_ps(data)}};
}

vecc_op V8F32 v8f32_cos(V8F32 a) {
    VECC_ALIGNED(32) F32 data[8];
    _mm256_store_ps(data, a.data[0]);
    for (int i = 0; i < VECC_LEN(data); i++) {
        data[i] = cosf(data[i]);
    }
    return {{_mm256_load_ps(data)}};
}

vecc_op V8F32 v8f32_tan(V8F32 a) {
    VECC_ALIGNED(32) F32 data[8];
    _mm256_store_ps(data, a.data[0]);
    for (int i = 0; i < VECC_LEN(data); i++) {
        data[i] = tanf(data[i]);
    }
    return {{_mm256_load_ps(data)}};
}

vecc_op V8F32 v8f32_log(V8F32 a) {
    VECC_ALIGNED(32) F32 data[8];
    _mm256_store_ps(data, a.data[0]);
    for (int i = 0; i < VECC_LEN(data); i++) {
        data[i] = logf(data[i]);
    }
    return {{_mm256_load_ps(data)}};
}

vecc_op V8F32 v8f32_exp(V8F32 a) {
    VECC_ALIGNED(32) F32 data[8];
    _mm256_store_ps(data, a.data[0]);
    for (int i = 0; i < VECC_LEN(data); i++) {
        data[i] = expf(data[i]);
    }
    return {{_mm256_load_ps(data)}};
}

vecc_op V8F32 v8f32_pow(V8F32 a, V8F32 b) {
    VECC_ALIGNED(32) F32 a_data[8];
    VECC_ALIGNED(32) F32 b_data[8];
    _mm256_store_ps(a_data, a.data[0]);
    _mm256_store_ps(b_data, b.data[0]);
    for (int i = 0; i < VECC_LEN(a_data); i++) {
        a_data[i] = powf(a_data[i], b_data[i]);
    }
    return {{_mm256_load_ps(a_data)}};
}

vecc_op B32 v8b32_reduce_all(V8B32 a) { return _mm256_testc_si256(a.data[0], _mm256_set1_epi32(-1)) != 0; }
vecc_op B32 v8b32_reduce_any(V8B32 a) { return _mm256_testz_si256(a.data[0], a.data[0]) == 0; }

// HACK
vecc_op U8 v8b32_to_bitmask(V8B32 a) { return _mm256_movemask_ps(_mm256_castsi256_ps(a.data[0])); }

// HACK
vecc_op F32 v8f32_extract(V8F32 a, int index) { F32 data[8]; _mm256_store_ps(data, a.data[0]); return data[index]; }
vecc_op I32 v8i32_extract(V8I32 a, int index) { I32 data[8]; _mm256_store_si256((__m256i*)data, a.data[0]); return data[index]; }
vecc_op U32 v8u32_extract(V8U32 a, int index) { U32 data[8]; _mm256_store_si256((__m256i*)data, a.data[0]); return data[index]; }
vecc_op U8 v8u8_extract(V8U8 a, int index) { return a.data[index]; }

#endif // VECC_AVX2

#endif // VECC_BUILTIN_DEFINED