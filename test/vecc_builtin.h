#ifndef VECC_BUILTIN_DEFINED
#define VECC_BUILTIN_DEFINED 1

// NOTE: the ugly ass name mangling will be changed later hopefully

static __m128i __m256_conv_uint8_t(__m256 x) {
    __m256i x_i32 = _mm256_cvtps_epi32(x);
    __m256i x_i16 = _mm256_packs_epi32(x_i32, x_i32);
    return _mm256_castsi256_si128(_mm256_packus_epi16(x_i16, x_i16));
}

static __m128i __m256_conv_uint16_t(__m256 x) {
    __m256i x_i32 = _mm256_cvtps_epi32(x);
    return _mm256_castsi256_si128(_mm256_packus_epi32(x_i32, x_i32));
}

#endif // VECC_BUILTIN_DEFINED