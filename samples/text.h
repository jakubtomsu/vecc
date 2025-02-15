#ifndef VECC_DEFINED
#define VECC_DEFINED 1

// WARNING: this file was generated by the VecC compiler.

#include <stdint.h>
#include <stdio.h>
#include <vecc_builtin.h>

typedef struct { U32 data[95]; } Aos95U32;
typedef struct { I32 data[2]; } Aos2I32;
typedef struct { U8 data[6]; } Aos6U8;
static Aos2I32 aos2i32_set(I32 v0, I32 v1) { return {{v0, v1}}; }
static Aos2I32 aos2i32_set1(I32 a) { return {{a, a}}; }
static Aos2I32 aos2i32_add(Aos2I32 a, Aos2I32 b) { return {{a.data[0] + b.data[0], a.data[1] + b.data[1]}}; }
static Aos2I32 aos2i32_sub(Aos2I32 a, Aos2I32 b) { return {{a.data[0] - b.data[0], a.data[1] - b.data[1]}}; }
static Aos2I32 aos2i32_mul(Aos2I32 a, Aos2I32 b) { return {{a.data[0] * b.data[0], a.data[1] * b.data[1]}}; }
static Aos2I32 aos2i32_div(Aos2I32 a, Aos2I32 b) { return {{a.data[0] / b.data[0], a.data[1] / b.data[1]}}; }
static Aos2I32 aos2i32_and(Aos2I32 a, Aos2I32 b) { return {{a.data[0] & b.data[0], a.data[1] & b.data[1]}}; }
static Aos2I32 aos2i32_or(Aos2I32 a, Aos2I32 b) { return {{a.data[0] | b.data[0], a.data[1] | b.data[1]}}; }
static Aos2I32 aos2i32_xor(Aos2I32 a, Aos2I32 b) { return {{a.data[0] ^ b.data[0], a.data[1] ^ b.data[1]}}; }
static Aos2I32 aos2i32_neg(Aos2I32 a) { return {{-a.data[0], -a.data[1]}}; }

// VECC exported constants

const String SAMPLE_NAME = {"120", 3};
const I32 RESOLUTION_X = (8 * 24);
const I32 RESOLUTION_Y = (8 * 24);
const I32 RESOLUTION_SCALE = 3;
const I32 KEY_LEFT_BIT = (1 << 0);
const I32 KEY_RIGHT_BIT = (1 << 1);
const I32 KEY_UP_BIT = (1 << 2);
const I32 KEY_DOWN_BIT = (1 << 3);
const I32 KEY_Z_BIT = (1 << 4);
const I32 KEY_X_BIT = (1 << 5);

// VECC exported function declarations
void compute_frame(V8U32* framebuffer, Aos2I32 resolution, F32 time, F32 delta, I32 frame, U32 keys);
#endif // VECC_DEFINED


#ifdef VECC_IMPL

// VECC private function declarations

static void print_glyph(U32 glyph);
static void draw_glyph(U32* framebuffer, Aos2I32 resolution, U32 glyph, Aos2I32 pos, U32 color);

// VECC global variable declarations

const I32 GLYPH_WIDTH = 5;
const I32 GLYPH_HEIGHT = 6;
const Aos95U32 g_glyphs = {{0, 134353028, 10570, 368389098, 150616254, 866266720, 781506726, 4228, 272765064, 71438466, 10631296, 4657152, 71434240, 458752, 207618048, 35791360, 490395438, 1044518084, 1042424366, 488124943, 554682504, 488127551, 488080460, 35791391, 488159790, 554649150, 207624384, 71307264, 272699648, 14694400, 71569472, 134361646, 1008662062, 589284676, 521715247, 1007715886, 521717295, 1041284159, 34651199, 488408126, 588840497, 1044517023, 211034399, 588553521, 1041269793, 588961467, 597481073, 488162862, 35112495, 820692526, 580372015, 487856686, 138547359, 488162865, 138757681, 928699953, 581046609, 138547537, 1042424351, 406982796, 545392672, 205656198, 17732, 3187671040, 130, 479703040, 244620321, 470857728, 479508744, 1008715200, 1109466188, 1317479726, 311729185, 203489344, 2353139716, 307336481, 203491394, 593144832, 311729152, 211064832, 1108583718, 1887905070, 34904064, 243494912, 203504706, 244622336, 73704448, 358269952, 910322688, 1317479721, 505560064, 407050380, 138547332, 205926534, 448512}};

// VECC function definitions

void compute_frame(V8U32* framebuffer, Aos2I32 resolution, F32 time, F32 delta, I32 frame, U32 keys) {
	const I32 num_pixel_blocks = ((resolution.data[0] * resolution.data[1]) / vector_width);
	for (I32 i = 0; (i < num_pixel_blocks); i = i + 1) {
		framebuffer[i] = v8u32_set1(286331153);
	};
	const Aos6U8 chars = {{72, 69, 76, 76, 79, 33}};
	for (I32 i = 0; (i < 6); i = i + 1) {
		draw_glyph((*(U32**)&framebuffer), resolution, g_glyphs.data[((I32)chars.data[i] - 32)], {{(10 + (i * (GLYPH_WIDTH + 1))), 10}}, 1432813567);
	};
}

static void print_glyph(U32 glyph) {
	for (I32 y = 0; (y < GLYPH_HEIGHT); y = y + 1) {
		for (I32 x = 0; (x < GLYPH_WIDTH); x = x + 1) {
			const U32 shift = (U32)((y * GLYPH_WIDTH) + x);
			const U32 bit = ((glyph >> shift) & 1);
			if ((bit != 0)) {
				string_print({"#", 1});
			} else {
				string_print({" ", 1});
			};
		};
		string_println({" ", 1});
	};
}

static void draw_glyph(U32* framebuffer, Aos2I32 resolution, U32 glyph, Aos2I32 pos, U32 color) {
	for (I32 y = 0; (y < GLYPH_HEIGHT); y = y + 1) {
		for (I32 x = 0; (x < GLYPH_WIDTH); x = x + 1) {
			const U32 shift = (U32)((y * GLYPH_WIDTH) + x);
			const U32 bit = ((glyph >> shift) & 1);
			if ((bit != 0)) {
				framebuffer[((pos.data[0] + x) + ((pos.data[1] + y) * resolution.data[0]))] = color;
			};
		};
	};
}

#endif // VECC_IMPL
