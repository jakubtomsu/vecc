#ifndef VECC_BUILTIN_DEFINED
#define VECC_BUILTIN_DEFINED 1

#include <stdint.h>

typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t   b8;
typedef int16_t  b16;
typedef int32_t  b32;
typedef int64_t  b64;

typedef float    f32;
typedef double   f64;

#define b8_false  (b8)0
#define b16_false (b16)0
#define b32_false (b32)0
#define b64_false (b64)0
#define b8_true  (b8)0xff
#define b16_true (b16)0xffff
#define b32_true (b32)0xffffffff
#define b64_true (b64)0xffffffffffffffff

#define vecc_func static
#define vecc_op static __forceinline

#define VECC_AVX2 1

#ifdef VECC_AVX2
#include <immintrin.h>


typedef struct { i8      data[2]; } v2i8;
typedef struct { i8      data[4]; } v4i8;
typedef struct { __m128i data[1]; } v16i8;
typedef struct { __m256i data[1]; } v32i8;
typedef struct { __m256i data[2]; } v64i8;

typedef struct { u8      data[2]; } v2u8;
typedef struct { u8      data[4]; } v4u8;
typedef struct { __m128i data[1]; } v16u8;
typedef struct { __m256i data[1]; } v32u8;
typedef struct { __m256i data[2]; } v64u8;

typedef struct { b8      data[2]; } v2b8;
typedef struct { b8      data[4]; } v4b8;
typedef struct { __m128i data[1]; } v16b8;
typedef struct { __m256i data[1]; } v32b8;
typedef struct { __m256i data[2]; } v64b8;


typedef struct { i16     data[2]; } v2i16;
typedef struct { i16     data[4]; } v4i16;
typedef struct { __m128i data[1]; } v8i16;
typedef struct { __m256i data[1]; } v16i16;
typedef struct { __m256i data[2]; } v32i16;
typedef struct { __m256i data[4]; } v64i16;

typedef struct { u16     data[2]; } v2u16;
typedef struct { u16     data[4]; } v4u16;
typedef struct { __m128i data[1]; } v8u16;
typedef struct { __m256i data[1]; } v16u16;
typedef struct { __m256i data[2]; } v32u16;
typedef struct { __m256i data[4]; } v64u16;

typedef struct { b16     data[2]; } v2b16;
typedef struct { b16     data[4]; } v4b16;
typedef struct { __m128i data[1]; } v8b16;
typedef struct { __m256i data[1]; } v16b16;
typedef struct { __m256i data[2]; } v32b16;
typedef struct { __m256i data[4]; } v64b16;


typedef struct { __m128  data[1]; } v2f32;
typedef struct { __m128  data[1]; } v4f32;
typedef struct { __m256  data[1]; } v8f32;
typedef struct { __m256  data[2]; } v16f32;
typedef struct { __m256  data[4]; } v32f32;
typedef struct { __m256  data[8]; } v64f32;

typedef struct { i32     data[2]; } v2i32;
typedef struct { __m128i data[1]; } v4i32;
typedef struct { __m256i data[1]; } v8i32;
typedef struct { __m256i data[2]; } v16i32;
typedef struct { __m256i data[4]; } v32i32;
typedef struct { __m256i data[8]; } v64i32;

typedef struct { u32     data[2]; } v2u32;
typedef struct { __m128i data[1]; } v4u32;
typedef struct { __m256i data[1]; } v8u32;
typedef struct { __m256i data[2]; } v16u32;
typedef struct { __m256i data[4]; } v32u32;
typedef struct { __m256i data[8]; } v64u32;

typedef struct { b32     data[2]; } v2b32;
typedef struct { __m128i data[1]; } v4b32;
typedef struct { __m256i data[1]; } v8b32;
typedef struct { __m256i data[2]; } v16b32;
typedef struct { __m256i data[4]; } v32b32;
typedef struct { __m256i data[8]; } v64b32;


typedef struct { __m128d data[1];  } v2f64;
typedef struct { __m256d data[1];  } v4f64;
typedef struct { __m256d data[2];  } v8f64;
typedef struct { __m256d data[4];  } v16f64;
typedef struct { __m256d data[8];  } v32f64;
typedef struct { __m256d data[16]; } v64f64;

typedef struct { __m128d data[1];  } v2i64;
typedef struct { __m256d data[1];  } v4i64;
typedef struct { __m256d data[2];  } v8i64;
typedef struct { __m256d data[4];  } v16i64;
typedef struct { __m256d data[8];  } v32i64;
typedef struct { __m256d data[16]; } v64i64;

typedef struct { __m128d data[1];  } v2u64;
typedef struct { __m256d data[1];  } v4u64;
typedef struct { __m256d data[2];  } v8u64;
typedef struct { __m256d data[4];  } v16u64;
typedef struct { __m256d data[8];  } v32u64;
typedef struct { __m256d data[16]; } v64u64;

typedef struct { __m128d data[1];  } v2b64;
typedef struct { __m256d data[1];  } v4b64;
typedef struct { __m256d data[2];  } v8b64;
typedef struct { __m256d data[4];  } v16b64;
typedef struct { __m256d data[8];  } v32b64;
typedef struct { __m256d data[16]; } v64b64;


vecc_func __m128i __m256_conv_uint8_t(__m256 x) {
    __m256i x_i32 = _mm256_cvtps_epi32(x);
    __m256i x_i16 = _mm256_packs_epi32(x_i32, x_i32);
    return _mm256_castsi256_si128(_mm256_packus_epi16(x_i16, x_i16));
}

vecc_func v8i32 v8f32_to_v8i32(v8f32 a) {
    return {{_mm256_cvtps_epi32(a.data[0])}};
}

vecc_func v8u32 v8f32_to_v8u32(v8f32 a) {
    return {{_mm256_cvtps_epi32(a.data[0])}};
}

vecc_func v8u16 v8f32_to_v8u16(v8f32 a) {
    __m256i a32 = _mm256_cvtps_epi32(a.data[0]);
    return {{_mm256_castsi256_si128(_mm256_packus_epi32(a32, a32))}};
}

vecc_func v8i16 v8f32_to_v8i16(v8f32 a) {
    __m256i a32 = _mm256_cvtps_epi32(a.data[0]);
    return {{_mm256_castsi256_si128(_mm256_packs_epi32(a32, a32))}};
}

vecc_func v8f32 v8i32_to_v8f32(v8i32 a) { return {{_mm256_castsi256_ps(a.data[0])}}; }

vecc_op v8f32 v8f32_set1(f32 a) { return {{_mm256_set1_ps(a)}}; }
vecc_op v4f64 v4f64_set1(f64 a) { return {{_mm256_set1_pd(a)}}; }
vecc_op v8i32 v8i32_set1(i32 a) { return {{_mm256_set1_epi32(a)}}; }
vecc_op v8u32 v8u32_set1(u32 a) { return {{_mm256_set1_epi32(a)}}; }
vecc_op v8b32 v8b32_set1(b32 a) { return {{_mm256_set1_epi32(a)}}; }
vecc_op v8u16 v8u16_set1(u16 a) { return {{_mm_set1_epi16(a)}}; }

vecc_op v8f32 v8f32_set(f32 a,f32 b,f32 c,f32 d,f32 e,f32 f,f32 g,f32 h) { return {{_mm256_setr_ps(a,b,c,d,e,f,g,h)}}; }
vecc_op v8i32 v8i32_set(i32 a,i32 b,i32 c,i32 d,i32 e,i32 f,i32 g,i32 h) { return {{_mm256_setr_epi32(a,b,c,d,e,f,g,h)}}; }
vecc_op v8u32 v8u32_set(u32 a,u32 b,u32 c,u32 d,u32 e,u32 f,u32 g,u32 h) { return {{_mm256_setr_epi32(a,b,c,d,e,f,g,h)}}; }
vecc_op v8b32 v8b32_set(b32 a,b32 b,b32 c,b32 d,b32 e,b32 f,b32 g,b32 h) { return {{_mm256_setr_epi32(a,b,c,d,e,f,g,h)}}; }

vecc_op v8f32 v8f32_load(const f32* data) { return {{_mm256_loadu_ps(data)}}; }
vecc_op v8i32 v8i32_load(const i32* data) { return {{_mm256_loadu_si256((__m256i*)data)}}; }
vecc_op v8u32 v8u32_load(const u32* data) { return {{_mm256_loadu_si256((__m256i*)data)}}; }
vecc_op v8b32 v8b32_load(const b32* data) { return {{_mm256_loadu_si256((__m256i*)data)}}; }

vecc_op void v8f32_store(f32* dst, v8f32 a) { _mm256_storeu_ps(dst, a.data[0]); }
vecc_op void v8i32_store(i32* dst, v8i32 a) { _mm256_storeu_si256((__m256i*)dst, a.data[0]); }
vecc_op void v8u32_store(u32* dst, v8u32 a) { _mm256_storeu_si256((__m256i*)dst, a.data[0]); }
vecc_op void v8b32_store(b32* dst, v8b32 a) { _mm256_storeu_si256((__m256i*)dst, a.data[0]); }

vecc_op v8f32 v8f32_add (v8f32 a, v8f32 b)  { return {{_mm256_add_ps(a.data[0], b.data[0])}}; }
vecc_op v8f32 v8f32_sub (v8f32 a, v8f32 b)  { return {{_mm256_sub_ps(a.data[0], b.data[0])}}; }
vecc_op v8f32 v8f32_mul (v8f32 a, v8f32 b)  { return {{_mm256_mul_ps(a.data[0], b.data[0])}}; }
vecc_op v8f32 v8f32_div (v8f32 a, v8f32 b)  { return {{_mm256_div_ps(a.data[0], b.data[0])}}; }
vecc_op v8f32 v8f32_and (v8f32 a, v8f32 b)  { return {{_mm256_and_ps(a.data[0], b.data[0])}}; }
vecc_op v8f32 v8f32_or  (v8f32 a, v8f32 b)  { return {{_mm256_or_ps(a.data[0], b.data[0])}}; }
vecc_op v8f32 v8f32_xor (v8f32 a, v8f32 b)  { return {{_mm256_xor_ps(a.data[0], b.data[0])}}; }

vecc_op v4f64 v4f64_add (v4f64 a, v4f64 b)  { return {{_mm256_add_pd(a.data[0], b.data[0])}}; }
vecc_op v4f64 v4f64_sub (v4f64 a, v4f64 b)  { return {{_mm256_sub_pd(a.data[0], b.data[0])}}; }
vecc_op v4f64 v4f64_mul (v4f64 a, v4f64 b)  { return {{_mm256_mul_pd(a.data[0], b.data[0])}}; }
vecc_op v4f64 v4f64_div (v4f64 a, v4f64 b)  { return {{_mm256_div_pd(a.data[0], b.data[0])}}; }
vecc_op v4f64 v4f64_and (v4f64 a, v4f64 b)  { return {{_mm256_and_pd(a.data[0], b.data[0])}}; }
vecc_op v4f64 v4f64_or  (v4f64 a, v4f64 b)  { return {{_mm256_or_pd(a.data[0], b.data[0])}}; }
vecc_op v4f64 v4f64_xor (v4f64 a, v4f64 b)  { return {{_mm256_xor_pd(a.data[0], b.data[0])}}; }

vecc_op v8i32 v8i32_add (v8i32 a, v8i32 b)  { return {{_mm256_add_epi32(a.data[0], b.data[0])}}; }
vecc_op v8i32 v8i32_sub (v8i32 a, v8i32 b)  { return {{_mm256_sub_epi32(a.data[0], b.data[0])}}; }
vecc_op v8i32 v8i32_mul (v8i32 a, v8i32 b)  { return {{_mm256_mul_epi32(a.data[0], b.data[0])}}; }
vecc_op v8i32 v8i32_and (v8i32 a, v8i32 b)  { return {{_mm256_and_si256(a.data[0], b.data[0])}}; }
vecc_op v8i32 v8i32_or  (v8i32 a, v8i32 b)  { return {{_mm256_or_si256(a.data[0], b.data[0])}}; }
vecc_op v8i32 v8i32_xor (v8i32 a, v8i32 b)  { return {{_mm256_xor_si256(a.data[0], b.data[0])}}; }

vecc_op v8u32 v8u32_add (v8u32 a, v8u32 b)  { return {{_mm256_add_epi32(a.data[0], b.data[0])}}; }
vecc_op v8u32 v8u32_sub (v8u32 a, v8u32 b)  { return {{_mm256_sub_epi32(a.data[0], b.data[0])}}; }
vecc_op v8u32 v8u32_mul (v8u32 a, v8u32 b)  { return {{_mm256_mul_epi32(a.data[0], b.data[0])}}; }
vecc_op v8u32 v8u32_and (v8u32 a, v8u32 b)  { return {{_mm256_and_si256(a.data[0], b.data[0])}}; }
vecc_op v8u32 v8u32_or  (v8u32 a, v8u32 b)  { return {{_mm256_or_si256(a.data[0], b.data[0])}}; }
vecc_op v8u32 v8u32_xor (v8u32 a, v8u32 b)  { return {{_mm256_xor_si256(a.data[0], b.data[0])}}; }
vecc_op v8u32 v8u32_sl  (v8u32 a, int b)    { return {{_mm256_slli_epi32(a.data[0], b)}}; }
vecc_op v8u32 v8u32_sr  (v8u32 a, int b)    { return {{_mm256_srli_epi32(a.data[0], b)}}; }

vecc_op v8b32 v8b32_and (v8b32 a, v8b32 b)  { return {{_mm256_and_si256(a.data[0], b.data[0])}}; }
vecc_op v8b32 v8b32_or  (v8b32 a, v8b32 b)  { return {{_mm256_or_si256(a.data[0], b.data[0])}}; }
vecc_op v8b32 v8b32_xor (v8b32 a, v8b32 b)  { return {{_mm256_xor_si256(a.data[0], b.data[0])}}; }

vecc_op v8u16 v8u16_add (v8u16 a, v8u16 b)  { return {{_mm_add_epi16(a.data[0], b.data[0])}}; }
vecc_op v8u16 v8u16_sub (v8u16 a, v8u16 b)  { return {{_mm_sub_epi16(a.data[0], b.data[0])}}; }
// vecc_op v8u16 v8u16_mul (v8u16 a, v8u16 b)  { return {{_mm_mul_epi16(a.data[0], b.data[0])}}; }
vecc_op v8u16 v8u16_and (v8u16 a, v8u16 b)  { return {{_mm_and_si128(a.data[0], b.data[0])}}; }
vecc_op v8u16 v8u16_or  (v8u16 a, v8u16 b)  { return {{_mm_or_si128(a.data[0], b.data[0])}}; }
vecc_op v8u16 v8u16_xor (v8u16 a, v8u16 b)  { return {{_mm_xor_si128(a.data[0], b.data[0])}}; }
vecc_op v8u16 v8u16_sl  (v8u16 a, int b)    { return {{_mm_slli_epi16(a.data[0], b)}}; }
vecc_op v8u16 v8u16_sr  (v8u16 a, int b)    { return {{_mm_srli_epi16(a.data[0], b)}}; }

vecc_op v8u32 v8u32_min (v8u32 a, v8u32 b)  { return {{_mm256_min_epi32(a.data[0], b.data[0])}}; }

#endif // VECC_AVX2

#endif // VECC_BUILTIN_DEFINED