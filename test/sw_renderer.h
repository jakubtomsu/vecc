#ifndef VECC_DEFINED
#define VECC_DEFINED 1

// WARNING: this file was generated by the VecC compiler.

#include <stdint.h>
#include <stdio.h>
#include <immintrin.h>
#ifdef __cplusplus
#else // __cplusplus
#define false 0
#define true 1
#endif // __cplusplus

// VECC type definitions
typedef int8_t bool8_t;
typedef int16_t bool16_t;
typedef int32_t bool32_t;
typedef int64_t bool64_t;
static __m256i __m256i_add(__m256i a, __m256i b);
static __m256i __m256i_sub(__m256i a, __m256i b);
static __m256i __m256i_mul(__m256i a, __m256i b);
static __m256i __m256i_div(__m256i a, __m256i b);
static __m256i __m256i_mod(__m256i a, __m256i b);
static __m256i __m256i_and(__m256i a, __m256i b);
static __m256i __m256i_or(__m256i a, __m256i b);
static __m256i __m256i_xor(__m256i a, __m256i b);
static __m256i __m256i_broadcast(int32_t a);
typedef struct Aos4_uint8_t { uint8_t data[4]; } Aos4_uint8_t;
static Aos4_uint8_t aos4_uint8_t_add(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_sub(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_mul(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_div(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_mod(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_and(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_or(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_xor(Aos4_uint8_t a, Aos4_uint8_t b);
static Aos4_uint8_t aos4_uint8_t_broadcast(uint8_t a);
typedef struct Aos2_int32_t { int32_t data[2]; } Aos2_int32_t;
static Aos2_int32_t aos2_int32_t_add(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_sub(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_mul(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_div(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_mod(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_and(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_or(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_xor(Aos2_int32_t a, Aos2_int32_t b);
static Aos2_int32_t aos2_int32_t_broadcast(int32_t a);
typedef struct Aos2_float { float data[2]; } Aos2_float;
static Aos2_float aos2_float_add(Aos2_float a, Aos2_float b);
static Aos2_float aos2_float_sub(Aos2_float a, Aos2_float b);
static Aos2_float aos2_float_mul(Aos2_float a, Aos2_float b);
static Aos2_float aos2_float_div(Aos2_float a, Aos2_float b);
static Aos2_float aos2_float_broadcast(float a);
static __m256 __m256_add(__m256 a, __m256 b);
static __m256 __m256_sub(__m256 a, __m256 b);
static __m256 __m256_mul(__m256 a, __m256 b);
static __m256 __m256_div(__m256 a, __m256 b);
static __m256 __m256_broadcast(float a);
typedef struct Aos2___m256 { __m256 data[2]; } Aos2___m256;
static Aos2___m256 aos2___m256_broadcast(__m256 a);
typedef struct Aos4_float { float data[4]; } Aos4_float;
static Aos4_float aos4_float_add(Aos4_float a, Aos4_float b);
static Aos4_float aos4_float_sub(Aos4_float a, Aos4_float b);
static Aos4_float aos4_float_mul(Aos4_float a, Aos4_float b);
static Aos4_float aos4_float_div(Aos4_float a, Aos4_float b);
static Aos4_float aos4_float_broadcast(float a);
typedef struct Aos4___m256 { __m256 data[4]; } Aos4___m256;
static Aos4___m256 aos4___m256_broadcast(__m256 a);
typedef struct Aos4_uint16_t { uint16_t data[4]; } Aos4_uint16_t;
static Aos4_uint16_t aos4_uint16_t_add(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_sub(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_mul(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_div(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_mod(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_and(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_or(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_xor(Aos4_uint16_t a, Aos4_uint16_t b);
static Aos4_uint16_t aos4_uint16_t_broadcast(uint16_t a);
static __m128i __m128i_add(__m128i a, __m128i b);
static __m128i __m128i_sub(__m128i a, __m128i b);
static __m128i __m128i_mul(__m128i a, __m128i b);
static __m128i __m128i_div(__m128i a, __m128i b);
static __m128i __m128i_mod(__m128i a, __m128i b);
static __m128i __m128i_and(__m128i a, __m128i b);
static __m128i __m128i_or(__m128i a, __m128i b);
static __m128i __m128i_xor(__m128i a, __m128i b);
static __m128i __m128i_broadcast(uint16_t a);
typedef struct Aos4___m128i { __m128i data[4]; } Aos4___m128i;
static Aos4___m128i aos4___m128i_broadcast(__m128i a);

// VECC exported function declarations
void compute_frame(Aos4_uint8_t* framebuffer, Aos2_int32_t resolution, float time, float delta, int32_t frame);
#endif // VECC_DEFINED


#ifdef VECC_IMPL
#include "vecc_builtin.h"

// VECC private function declarations

static __m256i __m256i_add(__m256i a, __m256i b) { return _mm256_add_epi32(a, b); }
static __m256i __m256i_sub(__m256i a, __m256i b) { return _mm256_sub_epi32(a, b); }
static __m256i __m256i_mul(__m256i a, __m256i b) { return _mm256_mul_epi32(a, b); }
static __m256i __m256i_and(__m256i a, __m256i b) { return _mm256_and_si256(a, b); }
static __m256i __m256i_or(__m256i a, __m256i b) { return _mm256_or_si256(a, b); }
static __m256i __m256i_xor(__m256i a, __m256i b) { return _mm256_xor_si256(a, b); }
static __m256i __m256i_broadcast(int32_t a) {
	return _mm256_set1_epi32(a);
}
static Aos4_uint8_t aos4_uint8_t_add(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] + b.data[0];
	result.data[1] = a.data[1] + b.data[1];
	result.data[2] = a.data[2] + b.data[2];
	result.data[3] = a.data[3] + b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_sub(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] - b.data[0];
	result.data[1] = a.data[1] - b.data[1];
	result.data[2] = a.data[2] - b.data[2];
	result.data[3] = a.data[3] - b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_mul(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] * b.data[0];
	result.data[1] = a.data[1] * b.data[1];
	result.data[2] = a.data[2] * b.data[2];
	result.data[3] = a.data[3] * b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_div(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] / b.data[0];
	result.data[1] = a.data[1] / b.data[1];
	result.data[2] = a.data[2] / b.data[2];
	result.data[3] = a.data[3] / b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_mod(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] % b.data[0];
	result.data[1] = a.data[1] % b.data[1];
	result.data[2] = a.data[2] % b.data[2];
	result.data[3] = a.data[3] % b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_and(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] & b.data[0];
	result.data[1] = a.data[1] & b.data[1];
	result.data[2] = a.data[2] & b.data[2];
	result.data[3] = a.data[3] & b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_or(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] | b.data[0];
	result.data[1] = a.data[1] | b.data[1];
	result.data[2] = a.data[2] | b.data[2];
	result.data[3] = a.data[3] | b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_xor(Aos4_uint8_t a, Aos4_uint8_t b) {
	Aos4_uint8_t result;
	result.data[0] = a.data[0] ^ b.data[0];
	result.data[1] = a.data[1] ^ b.data[1];
	result.data[2] = a.data[2] ^ b.data[2];
	result.data[3] = a.data[3] ^ b.data[3];
	return result;
}
static Aos4_uint8_t aos4_uint8_t_broadcast(uint8_t a) {
	Aos4_uint8_t result;
	result.data[0] = a;
	result.data[1] = a;
	result.data[2] = a;
	result.data[3] = a;
}
static Aos2_int32_t aos2_int32_t_add(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] + b.data[0];
	result.data[1] = a.data[1] + b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_sub(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] - b.data[0];
	result.data[1] = a.data[1] - b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_mul(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] * b.data[0];
	result.data[1] = a.data[1] * b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_div(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] / b.data[0];
	result.data[1] = a.data[1] / b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_mod(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] % b.data[0];
	result.data[1] = a.data[1] % b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_and(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] & b.data[0];
	result.data[1] = a.data[1] & b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_or(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] | b.data[0];
	result.data[1] = a.data[1] | b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_xor(Aos2_int32_t a, Aos2_int32_t b) {
	Aos2_int32_t result;
	result.data[0] = a.data[0] ^ b.data[0];
	result.data[1] = a.data[1] ^ b.data[1];
	return result;
}
static Aos2_int32_t aos2_int32_t_broadcast(int32_t a) {
	Aos2_int32_t result;
	result.data[0] = a;
	result.data[1] = a;
}
static Aos2_float aos2_float_add(Aos2_float a, Aos2_float b) {
	Aos2_float result;
	result.data[0] = a.data[0] + b.data[0];
	result.data[1] = a.data[1] + b.data[1];
	return result;
}
static Aos2_float aos2_float_sub(Aos2_float a, Aos2_float b) {
	Aos2_float result;
	result.data[0] = a.data[0] - b.data[0];
	result.data[1] = a.data[1] - b.data[1];
	return result;
}
static Aos2_float aos2_float_mul(Aos2_float a, Aos2_float b) {
	Aos2_float result;
	result.data[0] = a.data[0] * b.data[0];
	result.data[1] = a.data[1] * b.data[1];
	return result;
}
static Aos2_float aos2_float_div(Aos2_float a, Aos2_float b) {
	Aos2_float result;
	result.data[0] = a.data[0] / b.data[0];
	result.data[1] = a.data[1] / b.data[1];
	return result;
}
static Aos2_float aos2_float_broadcast(float a) {
	Aos2_float result;
	result.data[0] = a;
	result.data[1] = a;
}
static __m256 __m256_add(__m256 a, __m256 b) { return _mm256_add_ps(a, b); }
static __m256 __m256_sub(__m256 a, __m256 b) { return _mm256_sub_ps(a, b); }
static __m256 __m256_mul(__m256 a, __m256 b) { return _mm256_mul_ps(a, b); }
static __m256 __m256_div(__m256 a, __m256 b) { return _mm256_div_ps(a, b); }
static __m256 __m256_broadcast(float a) {
	return _mm256_set1_ps(a);
}
static Aos2___m256 aos2___m256_broadcast(__m256 a) {
	Aos2___m256 result;
	result.data[0] = a;
	result.data[1] = a;
}
static Aos4_float aos4_float_add(Aos4_float a, Aos4_float b) {
	Aos4_float result;
	result.data[0] = a.data[0] + b.data[0];
	result.data[1] = a.data[1] + b.data[1];
	result.data[2] = a.data[2] + b.data[2];
	result.data[3] = a.data[3] + b.data[3];
	return result;
}
static Aos4_float aos4_float_sub(Aos4_float a, Aos4_float b) {
	Aos4_float result;
	result.data[0] = a.data[0] - b.data[0];
	result.data[1] = a.data[1] - b.data[1];
	result.data[2] = a.data[2] - b.data[2];
	result.data[3] = a.data[3] - b.data[3];
	return result;
}
static Aos4_float aos4_float_mul(Aos4_float a, Aos4_float b) {
	Aos4_float result;
	result.data[0] = a.data[0] * b.data[0];
	result.data[1] = a.data[1] * b.data[1];
	result.data[2] = a.data[2] * b.data[2];
	result.data[3] = a.data[3] * b.data[3];
	return result;
}
static Aos4_float aos4_float_div(Aos4_float a, Aos4_float b) {
	Aos4_float result;
	result.data[0] = a.data[0] / b.data[0];
	result.data[1] = a.data[1] / b.data[1];
	result.data[2] = a.data[2] / b.data[2];
	result.data[3] = a.data[3] / b.data[3];
	return result;
}
static Aos4_float aos4_float_broadcast(float a) {
	Aos4_float result;
	result.data[0] = a;
	result.data[1] = a;
	result.data[2] = a;
	result.data[3] = a;
}
static Aos4___m256 aos4___m256_broadcast(__m256 a) {
	Aos4___m256 result;
	result.data[0] = a;
	result.data[1] = a;
	result.data[2] = a;
	result.data[3] = a;
}
static Aos4_uint16_t aos4_uint16_t_add(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] + b.data[0];
	result.data[1] = a.data[1] + b.data[1];
	result.data[2] = a.data[2] + b.data[2];
	result.data[3] = a.data[3] + b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_sub(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] - b.data[0];
	result.data[1] = a.data[1] - b.data[1];
	result.data[2] = a.data[2] - b.data[2];
	result.data[3] = a.data[3] - b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_mul(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] * b.data[0];
	result.data[1] = a.data[1] * b.data[1];
	result.data[2] = a.data[2] * b.data[2];
	result.data[3] = a.data[3] * b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_div(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] / b.data[0];
	result.data[1] = a.data[1] / b.data[1];
	result.data[2] = a.data[2] / b.data[2];
	result.data[3] = a.data[3] / b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_mod(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] % b.data[0];
	result.data[1] = a.data[1] % b.data[1];
	result.data[2] = a.data[2] % b.data[2];
	result.data[3] = a.data[3] % b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_and(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] & b.data[0];
	result.data[1] = a.data[1] & b.data[1];
	result.data[2] = a.data[2] & b.data[2];
	result.data[3] = a.data[3] & b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_or(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] | b.data[0];
	result.data[1] = a.data[1] | b.data[1];
	result.data[2] = a.data[2] | b.data[2];
	result.data[3] = a.data[3] | b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_xor(Aos4_uint16_t a, Aos4_uint16_t b) {
	Aos4_uint16_t result;
	result.data[0] = a.data[0] ^ b.data[0];
	result.data[1] = a.data[1] ^ b.data[1];
	result.data[2] = a.data[2] ^ b.data[2];
	result.data[3] = a.data[3] ^ b.data[3];
	return result;
}
static Aos4_uint16_t aos4_uint16_t_broadcast(uint16_t a) {
	Aos4_uint16_t result;
	result.data[0] = a;
	result.data[1] = a;
	result.data[2] = a;
	result.data[3] = a;
}
static __m128i __m128i_and(__m128i a, __m128i b) { return _mm_and_si128(a, b); }
static __m128i __m128i_or(__m128i a, __m128i b) { return _mm_xor_si128(a, b); }
static __m128i __m128i_xor(__m128i a, __m128i b) { return _mm_xor_si128(a, b); }
static __m128i __m128i_broadcast(uint16_t a) {
	return _mm_set1_epi16(a);
}
static Aos4___m128i aos4___m128i_broadcast(__m128i a) {
	Aos4___m128i result;
	result.data[0] = a;
	result.data[1] = a;
	result.data[2] = a;
	result.data[3] = a;
}

// VECC global variable declarations

const int32_t W = 8;
const int32_t vector_width = 8;
const __m256i vector_index = _mm256_set_epi32(7, 6, 5, 4, 3, 2, 1, 0);

// VECC function definitions

void compute_frame(Aos4_uint8_t* framebuffer, Aos2_int32_t resolution, float time, float delta, int32_t frame) {
	for (int32_t y = 0; y < resolution.data[1]; y = y + 1) {
		for (int32_t x = 0; x < resolution.data[0]; x = x + vector_width) {
			const int32_t index = x + y * resolution.data[0];
			Aos2___m256 uv = {0};
			uv.data[0] = __m256_broadcast((float)x / (float)resolution.data[0]);
			uv.data[1] = __m256_broadcast((float)y / (float)resolution.data[1]);
			uv.data[0] = __m256_mul(uv.data[0], __m256_broadcast(2.0f));
			uv.data[0] = __m256_add(uv.data[0], __m256_broadcast(1.0f));
			Aos4___m256 col = {0};
			col.data[0] = uv.data[0];
			col.data[1] = uv.data[1];
			col.data[3] = __m256_broadcast(1.0f);
			Aos4___m128i col_int = {0};
			col_int.data[2] = __m128i_and(__m256_conv_uint16_t(__m256_mul(col.data[0], __m256_broadcast(255.0f))), __m128i_broadcast(255));
			col_int.data[1] = __m128i_and(__m256_conv_uint16_t(__m256_mul(col.data[1], __m256_broadcast(255.0f))), __m128i_broadcast(255));
			col_int.data[0] = __m128i_and(__m256_conv_uint16_t(__m256_mul(col.data[2], __m256_broadcast(255.0f))), __m128i_broadcast(255));
			col_int.data[3] = __m128i_and(__m256_conv_uint16_t(__m256_mul(col.data[3], __m256_broadcast(255.0f))), __m128i_broadcast(255));
			__m128i idk = __m128i_shl(col_int.data[0], __m128i_broadcast(24));
			for (int32_t i = 0; i < vector_width; i = i + 1) {
			};
		};
	};
}

#endif // VECC_IMPL
