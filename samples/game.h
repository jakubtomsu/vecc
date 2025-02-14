#ifndef VECC_DEFINED
#define VECC_DEFINED 1

// WARNING: this file was generated by the VecC compiler.

#include <stdint.h>
#include <stdio.h>
#include <vecc_builtin.h>

typedef struct Hit {
	F32 tmin;
	F32 tmax;
	B8 hit;
} Hit;
typedef struct { F32 data[2]; } Aos2F32;
typedef struct Bullet {
	B8 used;
	Aos2F32 pos;
	Aos2F32 vel;
	F32 timer;
	U8 level;
	B8 explode;
} Bullet;
typedef struct { U8 data[4]; } Aos4U8;
typedef struct Effect {
	Aos2F32 pos;
	F32 rad;
	F32 timer;
	F32 dur;
	Aos4U8 color;
} Effect;
typedef struct Enemy {
	Aos2F32 pos;
	Aos2F32 vel;
	F32 speed;
	F32 health;
	F32 damage_timer;
	U8 size;
	U8 state;
	U8 kind;
} Enemy;
typedef struct { F32 data[3]; } Aos3F32;
typedef struct Player {
	Aos2F32 pos;
	Aos2F32 vel;
	Aos2F32 dir;
	F32 gun_timer;
	Aos3F32 powerup_timer;
	F32 particle_timer;
} Player;
typedef struct Item {
	Aos2F32 pos;
	I32 powerup;
	F32 timer;
} Item;
typedef struct { I32 data[2]; } Aos2I32;
typedef struct { Effect data[128]; } Aos128Effect;
typedef struct { Enemy data[64]; } Aos64Enemy;
typedef struct { Bullet data[64]; } Aos64Bullet;
typedef struct { Item data[64]; } Aos64Item;
typedef struct { V8I32 data[2]; } Aos2V8I32;
typedef struct { B8 data[3]; } Aos3B8;
static Aos2F32 aos2f32_set(F32 v0, F32 v1) { return {{v0, v1}}; }
static Aos2F32 aos2f32_set1(F32 a) { return {{a, a}}; }
static Aos2I32 aos2f32_to_aos2i32(Aos2F32 a) { return {{(I32)a.data[0], (I32)a.data[1]}}; }
static Aos2F32 aos2f32_add(Aos2F32 a, Aos2F32 b) { return {{a.data[0] + b.data[0], a.data[1] + b.data[1]}}; }
static Aos2F32 aos2f32_sub(Aos2F32 a, Aos2F32 b) { return {{a.data[0] - b.data[0], a.data[1] - b.data[1]}}; }
static Aos2F32 aos2f32_mul(Aos2F32 a, Aos2F32 b) { return {{a.data[0] * b.data[0], a.data[1] * b.data[1]}}; }
static Aos2F32 aos2f32_div(Aos2F32 a, Aos2F32 b) { return {{a.data[0] / b.data[0], a.data[1] / b.data[1]}}; }
static Aos2F32 aos2f32_neg(Aos2F32 a) { return {{-a.data[0], -a.data[1]}}; }
static Aos4U8 aos4u8_set(U8 v0, U8 v1, U8 v2, U8 v3) { return {{v0, v1, v2, v3}}; }
static Aos4U8 aos4u8_set1(U8 a) { return {{a, a, a, a}}; }
static Aos3F32 aos3f32_set(F32 v0, F32 v1, F32 v2) { return {{v0, v1, v2}}; }
static Aos3F32 aos3f32_set1(F32 a) { return {{a, a, a}}; }
static Aos3F32 aos3f32_add(Aos3F32 a, Aos3F32 b) { return {{a.data[0] + b.data[0], a.data[1] + b.data[1], a.data[2] + b.data[2]}}; }
static Aos3F32 aos3f32_sub(Aos3F32 a, Aos3F32 b) { return {{a.data[0] - b.data[0], a.data[1] - b.data[1], a.data[2] - b.data[2]}}; }
static Aos3F32 aos3f32_mul(Aos3F32 a, Aos3F32 b) { return {{a.data[0] * b.data[0], a.data[1] * b.data[1], a.data[2] * b.data[2]}}; }
static Aos3F32 aos3f32_div(Aos3F32 a, Aos3F32 b) { return {{a.data[0] / b.data[0], a.data[1] / b.data[1], a.data[2] / b.data[2]}}; }
static Aos3F32 aos3f32_neg(Aos3F32 a) { return {{-a.data[0], -a.data[1], -a.data[2]}}; }
static Aos2I32 aos2i32_set(I32 v0, I32 v1) { return {{v0, v1}}; }
static Aos2I32 aos2i32_set1(I32 a) { return {{a, a}}; }
static Aos2F32 aos2i32_to_aos2f32(Aos2I32 a) { return {{(F32)a.data[0], (F32)a.data[1]}}; }
static Aos2I32 aos2i32_add(Aos2I32 a, Aos2I32 b) { return {{a.data[0] + b.data[0], a.data[1] + b.data[1]}}; }
static Aos2I32 aos2i32_sub(Aos2I32 a, Aos2I32 b) { return {{a.data[0] - b.data[0], a.data[1] - b.data[1]}}; }
static Aos2I32 aos2i32_mul(Aos2I32 a, Aos2I32 b) { return {{a.data[0] * b.data[0], a.data[1] * b.data[1]}}; }
static Aos2I32 aos2i32_div(Aos2I32 a, Aos2I32 b) { return {{a.data[0] / b.data[0], a.data[1] / b.data[1]}}; }
static Aos2I32 aos2i32_and(Aos2I32 a, Aos2I32 b) { return {{a.data[0] & b.data[0], a.data[1] & b.data[1]}}; }
static Aos2I32 aos2i32_or(Aos2I32 a, Aos2I32 b) { return {{a.data[0] | b.data[0], a.data[1] | b.data[1]}}; }
static Aos2I32 aos2i32_xor(Aos2I32 a, Aos2I32 b) { return {{a.data[0] ^ b.data[0], a.data[1] ^ b.data[1]}}; }
static Aos2I32 aos2i32_neg(Aos2I32 a) { return {{-a.data[0], -a.data[1]}}; }
static Aos2V8I32 aos2v8i32_set(V8I32 v0, V8I32 v1) { return {{v0, v1}}; }
static Aos2V8I32 aos2v8i32_set_scalar(Aos2I32 a) { return {{v8i32_set1(a.data[0]), v8i32_set1(a.data[1])}}; }
static Aos2V8I32 aos2v8i32_set1(V8I32 a) { return {{a, a}}; }
static Aos2V8I32 aos2v8i32_add(Aos2V8I32 a, Aos2V8I32 b) { return {{v8i32_add(a.data[0], b.data[0]), v8i32_add(a.data[1], b.data[1])}}; }
static Aos2V8I32 aos2v8i32_sub(Aos2V8I32 a, Aos2V8I32 b) { return {{v8i32_sub(a.data[0], b.data[0]), v8i32_sub(a.data[1], b.data[1])}}; }
static Aos2V8I32 aos2v8i32_mul(Aos2V8I32 a, Aos2V8I32 b) { return {{v8i32_mul(a.data[0], b.data[0]), v8i32_mul(a.data[1], b.data[1])}}; }
static Aos2V8I32 aos2v8i32_and(Aos2V8I32 a, Aos2V8I32 b) { return {{v8i32_and(a.data[0], b.data[0]), v8i32_and(a.data[1], b.data[1])}}; }
static Aos2V8I32 aos2v8i32_or(Aos2V8I32 a, Aos2V8I32 b) { return {{v8i32_or(a.data[0], b.data[0]), v8i32_or(a.data[1], b.data[1])}}; }
static Aos2V8I32 aos2v8i32_xor(Aos2V8I32 a, Aos2V8I32 b) { return {{v8i32_xor(a.data[0], b.data[0]), v8i32_xor(a.data[1], b.data[1])}}; }
static Aos3B8 aos3b8_set(B8 v0, B8 v1, B8 v2) { return {{v0, v1, v2}}; }
static Aos3B8 aos3b8_set1(B8 a) { return {{a, a, a}}; }
static Aos3B8 aos3b8_not(Aos3B8 a) { return {{!a.data[0], !a.data[1], !a.data[2]}}; }

// VECC exported constants

const I32 RESOLUTION_X = (8 * 30);
const I32 RESOLUTION_Y = (8 * 30);
const I32 RESOLUTION_SCALE = 4;
const U32 KEY_LEFT_BIT = (1 << 0);
const U32 KEY_RIGHT_BIT = (1 << 1);
const U32 KEY_UP_BIT = (1 << 2);
const U32 KEY_DOWN_BIT = (1 << 3);
const U32 KEY_Z_BIT = (1 << 4);
const U32 KEY_X_BIT = (1 << 5);

// VECC exported function declarations
void compute_frame(V8U32* framebuffer, Aos2I32 resolution, F32 time, F32 delta, I32 frame, U32 keys);
#endif // VECC_DEFINED


#ifdef VECC_IMPL

// VECC private function declarations

static U32 hash(U32 n);
static U32 rand_u32();
static F32 rand_f32();
static U32 color_to_u32(Aos4U8 col);
static void spawn_item(Aos2F32 pos, I32 powerup);
static void spawn_effect(Aos2F32 pos, F32 rad, F32 dur, Aos4U8 col);
static void reset_game();
static void draw_rect(V8U32* framebuffer, Aos2I32 resolution, Aos2I32 pos, Aos2I32 size, Aos4U8 color);
static void draw_octagon(V8U32* framebuffer, Aos2I32 resolution, Aos2I32 pos, I32 size, I32 rad, Aos4U8 color, V8B32 mask);
static Aos2F32 normalize(Aos2F32 x);
static F32 length2(Aos2F32 x);
static F32 length(Aos2F32 x);
static F32 dot(Aos2F32 a, Aos2F32 b);
static Hit intersect_ray_aabb(Aos2F32 pos, Aos2F32 dir, Aos2F32 box_min, Aos2F32 box_max);

// VECC global variable declarations

const I32 PLAYER_POWERUP_FAST_FIRE = 0;
const I32 PLAYER_POWERUP_SHOTGUN = 1;
const I32 PLAYER_POWERUP_EXPLOSIVE = 2;
Player player = {0};
Aos64Item g_items = {0};
Aos64Bullet g_bullets = {0};
Aos128Effect g_effects = {0};
const U8 ENEMY_STATE_DEAD = 0;
const U8 ENEMY_STATE_ALIVE = 1;
const Aos4U8 ENEMY_COLOR_NORMAL = {{255, 50, 200, 0}};
const Aos4U8 ENEMY_COLOR_FAST = {{200, 0, 255, 0}};
const Aos4U8 ENEMY_COLOR_BIG = {{255, 10, 100, 0}};
const F32 GAME_MAX_TIME = 100.0f;
Aos64Enemy g_enemies = {0};
F32 g_enemy_spawn_timer = 5.0f;
F32 g_game_timer = 0.0f;
U32 g_seed = 1;
Aos2I32 g_camera = {0};
F32 g_shake = 0.0f;
F32 g_hit_pause = 0.0f;

// VECC function definitions

static U32 hash(U32 n) {
	n = ((n << 13) ^ n);
	n = n * (((n * n) * 15731) + 789221);
	n = n + 1376312589;
	return n;
}

static U32 rand_u32() {
	g_seed = ((g_seed * 214013) + 2531011);
	return ((g_seed >> 16) & 32767);
}

static F32 rand_f32() {
	return ((F32)rand_u32() / 32767.0f);
}

static U32 color_to_u32(Aos4U8 col) {
	U32 result = 0;
	result = (U32)col.data[2];
	result = result | ((U32)col.data[1] << 8);
	result = result | ((U32)col.data[0] << 16);
	result = result | ((U32)col.data[3] << 24);
	return result;
}

static void spawn_item(Aos2F32 pos, I32 powerup) {
	for (I32 i = 0; (i < 64); i = i + 1) {
		Item item = g_items.data[i];
		if ((item.powerup == -1)) {
			item.pos = pos;
			item.powerup = powerup;
			item.timer = 0.0f;
			g_items.data[i] = item;
			break;
		};
	};
}

static void spawn_effect(Aos2F32 pos, F32 rad, F32 dur, Aos4U8 col) {
	for (I32 i = 0; (i < 128); i = i + 1) {
		Effect effect = g_effects.data[i];
		if ((effect.timer > effect.dur)) {
			effect.pos = pos;
			effect.rad = rad;
			effect.timer = 0.0f;
			effect.dur = dur;
			effect.color = col;
			g_effects.data[i] = effect;
			break;
		};
	};
}

static void reset_game() {
	player.pos = aos2f32_div({{(F32)RESOLUTION_X, (F32)RESOLUTION_Y}}, aos2f32_set1(2.0f));
	player.vel = aos2f32_set1(0.0f);
	player.gun_timer = 0.0f;
	player.powerup_timer = {{0.0f, 0.0f, 0.0f}};
	for (I32 i = 0; (i < 64); i = i + 1) {
		g_items.data[i].powerup = -1;
	};
	for (I32 i = 0; (i < 64); i = i + 1) {
		g_enemies.data[i].pos = aos2f32_neg(aos2f32_set1(100.0f));
		g_enemies.data[i].state = ENEMY_STATE_DEAD;
	};
	for (I32 i = 0; (i < 64); i = i + 1) {
		g_effects.data[i].dur = -1.0f;
	};
	g_enemy_spawn_timer = 0.0f;
	g_game_timer = 0.0f;
}

void compute_frame(V8U32* framebuffer, Aos2I32 resolution, F32 time, F32 delta, I32 frame, U32 keys) {
	if ((frame == 0)) {
		reset_game();
	};
	B32 should_hit_pause = (g_hit_pause > 0.0f);
	g_hit_pause = g_hit_pause - delta;
	if (should_hit_pause) {
		return ;
	};
	g_game_timer = g_game_timer + delta;
	B8 end_game = false;
	U32 clear_mask = (*(U32*)&delta);
	clear_mask = ~clear_mask;
	const I32 num_pixel_blocks = ((resolution.data[0] * resolution.data[1]) / vector_width);
	for (I32 i = 0; (i < num_pixel_blocks); i = i + 1) {
		const V8U32 c = framebuffer[i];
		framebuffer[i] = v8u32_sl(c, 5);
	};
	g_camera = {{(I32)f32_round(((rand_f32() - 0.5f) * g_shake)), (I32)f32_round(((rand_f32() - 0.5f) * g_shake))}};
	g_shake = f32_max(0.0f, (g_shake - (delta * 50.0f)));
	for (I32 i = 0; (i < 64); i = i + 1) {
		Enemy enemy = g_enemies.data[i];
		if ((enemy.state != ENEMY_STATE_DEAD)) {
			continue;
		};
		draw_octagon(framebuffer, resolution, aos2f32_to_aos2i32(enemy.pos), 5, 6, {{40, 10, 10, 0}}, v8b32_set1(b32_true));
	};
	for (I32 i = 0; (i < 64); i = i + 1) {
		Item item = g_items.data[i];
		if ((item.powerup == -1)) {
			continue;
		};
		item.timer = item.timer + delta;
		if ((item.timer > 11.0f)) {
			item.powerup = -1;
		};
		if ((length2(aos2f32_sub(item.pos, player.pos)) < (10.0f * 10.0f))) {
			if ((player.powerup_timer.data[item.powerup] < 0.0f)) {
				player.powerup_timer.data[item.powerup] = 0.0f;
			};
			player.powerup_timer.data[item.powerup] = player.powerup_timer.data[item.powerup] + 7.0f;
			item.powerup = -1;
		};
		g_items.data[i] = item;
		F32 rad = f32_round((2.2f + (f32_sin(((time + (F32)(i * 13)) * 8.0f)) * 0.69999999f)));
		Aos4U8 color = {0};
		if ((item.powerup == PLAYER_POWERUP_FAST_FIRE)) {
			color = {{0, 255, 0, 0}};
		};
		if ((item.powerup == PLAYER_POWERUP_SHOTGUN)) {
			color = {{255, 255, 0, 0}};
		};
		if ((item.powerup == PLAYER_POWERUP_EXPLOSIVE)) {
			color = {{0, 255, 255, 0}};
		};
		if ((item.timer > 7.0f)) {
			if ((f32_sin((item.timer * 30.0f)) > 0.0f)) {
				color = {{0, 0, 0, 0}};
			};
			rad = 3.0f;
		};
		draw_octagon(framebuffer, resolution, aos2f32_to_aos2i32(item.pos), (I32)rad, 100, color, v8b32_set1(b32_true));
	};
	F32 max_enemy_damage_timer = 0.0f;
	I32 oldest_corpse = -1;
	for (I32 i = 0; (i < 64); i = i + 1) {
		Enemy enemy = g_enemies.data[i];
		enemy.damage_timer = enemy.damage_timer + delta;
		if ((enemy.state == ENEMY_STATE_DEAD)) {
			if ((enemy.damage_timer > max_enemy_damage_timer)) {
				max_enemy_damage_timer = enemy.damage_timer;
				oldest_corpse = i;
			};
			g_enemies.data[i] = enemy;
			continue;
		};
		if ((enemy.state == ENEMY_STATE_ALIVE)) {
			enemy.vel = aos2f32_add(enemy.vel, aos2f32_mul(normalize(aos2f32_sub(player.pos, enemy.pos)), aos2f32_set1((delta * (enemy.speed * 5.0f)))));
			enemy.vel = aos2f32_mul(normalize(enemy.vel), aos2f32_set1(f32_clamp(length(enemy.vel), 0.0f, enemy.speed)));
			enemy.pos = aos2f32_add(enemy.pos, aos2f32_mul(enemy.vel, aos2f32_set1(delta)));
			F32 rad = ((F32)enemy.size + 2.0f);
			if ((length2(aos2f32_sub(enemy.pos, player.pos)) < (rad * rad))) {
				end_game = b8_true;
			};
		};
		if ((enemy.health <= 0.0f)) {
			enemy.state = ENEMY_STATE_DEAD;
			spawn_effect(enemy.pos, 9.0f, 0.05f, {{255, 255, 255, 255}});
			g_shake = g_shake + 5.0f;
			g_hit_pause = 0.02f;
			U32 p = (rand_u32() % 10);
			if ((p < 3)) {
				spawn_item(enemy.pos, (I32)p);
			};
		};
		Aos4U8 color = {0};
		color = ENEMY_COLOR_NORMAL;
		if ((enemy.kind == 1)) {
			color = ENEMY_COLOR_FAST;
		};
		if ((enemy.kind == 2)) {
			color = ENEMY_COLOR_BIG;
		};
		if ((enemy.damage_timer < 0.05f)) {
			color = {{255, 255, 255, 255}};
		};
		draw_octagon(framebuffer, resolution, aos2f32_to_aos2i32(enemy.pos), (I32)enemy.size, (I32)(enemy.size + 1), color, v8b32_set1(b32_true));
		g_enemies.data[i] = enemy;
	};
	g_enemy_spawn_timer = g_enemy_spawn_timer + delta;
	const F32 spawn_rate = (0.2f + (1.8f * f32_max(0.0f, (1.0f - (g_game_timer / GAME_MAX_TIME)))));
	if (((g_enemy_spawn_timer > spawn_rate) & (oldest_corpse != -1))) {
		g_enemy_spawn_timer = 0.0f;
		Enemy enemy = g_enemies.data[oldest_corpse];
		enemy.state = ENEMY_STATE_ALIVE;
		const F32 RAD = 0.0f;
		if (((rand_u32() & 1) == 0)) {
			if (((rand_u32() & 1) == 0)) {
				enemy.pos.data[0] = -RAD;
			} else {
				enemy.pos.data[0] = ((F32)RESOLUTION_X + RAD);
			};
			enemy.pos.data[1] = (F32)((I32)rand_u32() % RESOLUTION_Y);
		} else {
			if (((rand_u32() & 1) == 0)) {
				enemy.pos.data[1] = -RAD;
			} else {
				enemy.pos.data[1] = ((F32)RESOLUTION_Y + RAD);
			};
			enemy.pos.data[0] = (F32)((I32)rand_u32() % RESOLUTION_X);
		};
		if (((g_game_timer > 10.0f) & ((rand_u32() % 4) == 0))) {
			enemy.size = 8;
			enemy.health = 6.0f;
			enemy.speed = 8.0f;
			enemy.kind = 2;
		} else {
			if (((g_game_timer > 5.0f) & ((rand_u32() % 2) == 0))) {
				enemy.size = 5;
				enemy.health = 0.5f;
				enemy.speed = 30.0f;
				enemy.kind = 1;
			} else {
				enemy.size = 6;
				enemy.health = 2.0f;
				enemy.speed = 20.0f;
				enemy.kind = 0;
			};
		};
		g_enemies.data[oldest_corpse] = enemy;
	};
	for (I32 i = 0; (i < 128); i = i + 1) {
		Effect effect = g_effects.data[i];
		effect.timer = effect.timer + delta;
		if ((effect.timer < effect.dur)) {
			draw_octagon(framebuffer, resolution, aos2f32_to_aos2i32(effect.pos), (I32)effect.rad, (I32)(effect.rad * 1.5f), effect.color, v8b32_set1(b32_true));
		};
		g_effects.data[i] = effect;
	};
	{
		Aos3B8 powerups = {0};
		for (I32 i = 0; (i < 3); i = i + 1) {
			player.powerup_timer.data[i] = player.powerup_timer.data[i] - delta;
			if ((player.powerup_timer.data[i] > 0.0f)) {
				powerups.data[i] = b8_true;
			};
		};
		Aos2F32 dir = {0};
		B8 aiming = false;
		if (((keys & KEY_LEFT_BIT) != 0)) {
			dir.data[0] = dir.data[0] + -1.0f;
		};
		if (((keys & KEY_RIGHT_BIT) != 0)) {
			dir.data[0] = dir.data[0] + 1.0f;
		};
		if (((keys & KEY_UP_BIT) != 0)) {
			dir.data[1] = dir.data[1] + -1.0f;
		};
		if (((keys & KEY_DOWN_BIT) != 0)) {
			dir.data[1] = dir.data[1] + 1.0f;
		};
		aiming = (B8)(length2(dir) > 0.0f);
		F32 cooldown_time = 0.1f;
		I32 num_shots = 1;
		F32 shot_spread = 0.25f;
		U8 bullet_level = 0;
		F32 bullet_speed = 300.0f;
		if (powerups.data[PLAYER_POWERUP_FAST_FIRE]) {
			cooldown_time = cooldown_time * 0.4f;
			bullet_speed = bullet_speed * 1.5f;
			bullet_level = bullet_level + 1;
		};
		if (powerups.data[PLAYER_POWERUP_SHOTGUN]) {
			cooldown_time = cooldown_time * 2.0f;
			num_shots = num_shots * 4;
			shot_spread = shot_spread * 3.0f;
			bullet_level = bullet_level + 1;
		};
		if (powerups.data[PLAYER_POWERUP_EXPLOSIVE]) {
			bullet_speed = bullet_speed * 0.69999999f;
			cooldown_time = cooldown_time * 2.0f;
			bullet_level = bullet_level + 1;
		};
		player.gun_timer = player.gun_timer + delta;
		F32 shoot_dir = 1.0f;
		B32 shooting = false;
		if (((keys & KEY_X_BIT) != 0)) {
			shoot_dir = 1.0f;
			shooting = b32_true;
		};
		if (((keys & KEY_Z_BIT) != 0)) {
			shoot_dir = -1.0f;
			shooting = b32_true;
		};
		if ((shooting & (player.gun_timer > cooldown_time))) {
			player.gun_timer = 0.0f;
			player.vel = aos2f32_sub(player.vel, aos2f32_mul(player.dir, aos2f32_set1(38.0f)));
			I32 bullet_num = 0;
			g_shake = 1.0f;
			for (I32 i = 0; (i < 64); i = i + 1) {
				if (!g_bullets.data[i].used) {
					Aos2F32 bdir = aos2f32_mul(player.dir, aos2f32_set1(shoot_dir));
					bdir.data[0] = bdir.data[0] + ((rand_f32() - 0.5f) * shot_spread);
					bdir.data[1] = bdir.data[1] + ((rand_f32() - 0.5f) * shot_spread);
					bdir = normalize(bdir);
					g_bullets.data[i] = {b8_true, aos2f32_add(player.pos, aos2f32_mul(player.dir, aos2f32_set1(3.0f))), aos2f32_mul(bdir, aos2f32_set1(bullet_speed)), 0.0f, bullet_level, powerups.data[PLAYER_POWERUP_EXPLOSIVE]};
					bullet_num = bullet_num + 1;
					if ((bullet_num == num_shots)) {
						break;
					};
				};
			};
		};
		if (aiming) {
			if (!shooting) {
				player.dir = dir;
			};
		};
		if (aiming) {
			player.particle_timer = player.particle_timer + delta;
			if ((player.particle_timer > 0.1f)) {
				player.particle_timer = player.particle_timer - 0.1f;
				spawn_effect(player.pos, 1.0f, (rand_f32() * 2.0f), {{20, 20, 20, 0}});
			};
		};
		player.vel = aos2f32_mul(player.vel, aos2f32_set1(0.89999998f));
		player.vel = aos2f32_add(player.vel, aos2f32_mul(dir, aos2f32_set1((delta * 1000.0f))));
		player.pos = aos2f32_add(player.pos, aos2f32_mul(player.vel, aos2f32_set1(delta)));
		const F32 RAD = 5.0f;
		player.pos.data[0] = f32_clamp(player.pos.data[0], RAD, ((F32)RESOLUTION_X - RAD));
		player.pos.data[1] = f32_clamp(player.pos.data[1], RAD, ((F32)RESOLUTION_Y - RAD));
		Aos2F32 player_pos = player.pos;
		player_pos.data[1] = player_pos.data[1] - f32_round(((0.5f + (0.5f * f32_sin((time * 25.0f)))) * f32_clamp(length(player.vel), 0.0f, 1.0f)));
		draw_octagon(framebuffer, resolution, aos2f32_to_aos2i32(player_pos), 3, 5, {{255, 150, 0, 0}}, v8b32_set1(b32_true));
		Aos2I32 gun_pos = aos2f32_to_aos2i32(player.pos);
		gun_pos.data[0] = gun_pos.data[0] + (I32)(((player.dir.data[0] * shoot_dir) * 3.0f) * f32_min((player.gun_timer * 10.0f), 1.0f));
		gun_pos.data[1] = gun_pos.data[1] + (I32)(((player.dir.data[1] * shoot_dir) * 3.0f) * f32_min((player.gun_timer * 10.0f), 1.0f));
		gun_pos.data[1] = gun_pos.data[1] + 1;
		Aos2I32 gun_size = {{7, 2}};
		if ((player.dir.data[0] == 0.0f)) {
			gun_size = {{(gun_size.data[1] + 1), (gun_size.data[0] - 1)}};
		};
		draw_rect(framebuffer, resolution, aos2i32_sub(aos2i32_sub(gun_pos, {{(gun_size.data[0] / 2), (gun_size.data[1] / 2)}}), g_camera), gun_size, {{255, 255, 255, 255}});
	};
	for (I32 i = 0; (i < 64); i = i + 1) {
		Bullet bullet = g_bullets.data[i];
		if ((bullet.timer > 0.4f)) {
			bullet.timer = 0.0f;
			bullet.used = b8_false;
		};
		if (bullet.used) {
			bullet.timer = bullet.timer + delta;
			const Aos2F32 move = aos2f32_mul(bullet.vel, aos2f32_set1(delta));
			const Aos2F32 move_dir = normalize(move);
			F32 tmin = length(move);
			I32 hit_enemy = -1;
			for (I32 ei = 0; (ei < 64); ei = ei + 1) {
				const Enemy enemy = g_enemies.data[ei];
				if ((enemy.state != ENEMY_STATE_ALIVE)) {
					continue;
				};
				const Hit hit = intersect_ray_aabb(bullet.pos, move_dir, aos2f32_sub(enemy.pos, aos2f32_set1((F32)(enemy.size + 1))), aos2f32_add(enemy.pos, aos2f32_set1((F32)(enemy.size + 1))));
				if ((hit.hit & (B8)(hit.tmin < tmin))) {
					tmin = hit.tmin;
					hit_enemy = ei;
				};
			};
			if ((hit_enemy == -1)) {
				bullet.pos = aos2f32_add(bullet.pos, move);
			} else {
				bullet.pos = aos2f32_add(bullet.pos, aos2f32_mul(move_dir, aos2f32_set1(tmin)));
				bullet.used = b8_false;
				if (bullet.explode) {
					spawn_effect(bullet.pos, 24.0f, 0.05f, {{0, 255, 255, 0}});
					g_hit_pause = 0.09f;
					for (I32 ei = 0; (ei < 64); ei = ei + 1) {
						Enemy enemy = g_enemies.data[ei];
						if ((enemy.state != ENEMY_STATE_ALIVE)) {
							continue;
						};
						if ((length2(aos2f32_sub(enemy.pos, bullet.pos)) < (28.0f * 28.0f))) {
							enemy.health = enemy.health - 6.0f;
							enemy.damage_timer = 0.0f;
							enemy.vel = aos2f32_set1(0.0f);
							g_enemies.data[hit_enemy] = enemy;
						};
					};
				} else {
					spawn_effect(bullet.pos, 4.0f, 0.1f, {{0, 255, 255, 0}});
				};
				Enemy enemy = g_enemies.data[hit_enemy];
				enemy.health = enemy.health - 1.0f;
				enemy.damage_timer = 0.0f;
				enemy.vel = bullet.vel;
				g_enemies.data[hit_enemy] = enemy;
			};
			draw_octagon(framebuffer, resolution, aos2f32_to_aos2i32(bullet.pos), 2, (2 + (I32)bullet.level), {{0, 255, 255, 0}}, v8b32_set1(b32_true));
		};
		g_bullets.data[i] = bullet;
	};
	draw_rect(framebuffer, resolution, {{0, 1}}, {{(RESOLUTION_X - 1), 1}}, {{100, 100, 100, 100}});
	draw_rect(framebuffer, resolution, {{0, 1}}, {{i32_max(0, (I32)((F32)RESOLUTION_X * (1.0f - (g_game_timer / GAME_MAX_TIME)))), 1}}, {{255, 255, 255, 255}});
	draw_rect(framebuffer, resolution, {{2, (RESOLUTION_Y - 5)}}, {{i32_max(0, (I32)(3.0f * f32_ceil(player.powerup_timer.data[PLAYER_POWERUP_FAST_FIRE]))), 3}}, {{0, 255, 0, 0}});
	draw_rect(framebuffer, resolution, {{2, (RESOLUTION_Y - 10)}}, {{i32_max(0, (I32)(3.0f * f32_ceil(player.powerup_timer.data[PLAYER_POWERUP_SHOTGUN]))), 3}}, {{255, 255, 0, 0}});
	draw_rect(framebuffer, resolution, {{2, (RESOLUTION_Y - 15)}}, {{i32_max(0, (I32)(3.0f * f32_ceil(player.powerup_timer.data[PLAYER_POWERUP_EXPLOSIVE]))), 3}}, {{0, 255, 255, 0}});
	if (end_game) {
		reset_game();
		for (I32 i = 0; (i < num_pixel_blocks); i = i + 1) {
			framebuffer[i] = v8u32_set1(0);
		};
	};
}

static void draw_rect(V8U32* framebuffer, Aos2I32 resolution, Aos2I32 pos, Aos2I32 size, Aos4U8 color) {
	const U32 col = color_to_u32(color);
	const I32 x_end = (pos.data[0] + size.data[0]);
	const Aos2I32 x_range = {{(i32_clamp(pos.data[0], 0, resolution.data[0]) / vector_width), (((i32_clamp(x_end, 0, resolution.data[0]) + vector_width) - 1) / vector_width)}};
	const Aos2I32 y_range = {{i32_clamp(pos.data[1], 0, resolution.data[1]), i32_clamp((pos.data[1] + size.data[1]), 0, resolution.data[1])}};
	for (I32 y = y_range.data[0]; (y < y_range.data[1]); y = y + 1) {
		const I32 y_offset = (y * (resolution.data[0] / vector_width));
		for (I32 xv = x_range.data[0]; (xv < x_range.data[1]); xv = xv + 1) {
			const V8I32 x = v8i32_add(vector_index, v8i32_set1((xv * vector_width)));
			V8B32 vecc_mask5 = v8i32_gt(x, v8i32_set1(pos.data[0])); { // vector if
				V8B32 vecc_mask6 = v8b32_and(vecc_mask5, v8i32_lt(x, v8i32_set1(x_end))); { // vector if
					framebuffer[(y_offset + xv)] = v8u32_blend(framebuffer[(y_offset + xv)], v8u32_set1(col), vecc_mask6);
				};
			};
		};
	};
}

static void draw_octagon(V8U32* framebuffer, Aos2I32 resolution, Aos2I32 pos, I32 size, I32 rad, Aos4U8 color, V8B32 mask) {
	pos = aos2i32_sub(pos, g_camera);
	const U32 col = color_to_u32(color);
	const I32 x_start = (pos.data[0] - size);
	const I32 x_end = (pos.data[0] + size);
	const Aos2I32 x_range = {{(i32_clamp(x_start, 0, resolution.data[0]) / vector_width), (((i32_clamp(x_end, 0, resolution.data[0]) + vector_width) - 1) / vector_width)}};
	const Aos2I32 y_range = {{i32_clamp((pos.data[1] - (size - 1)), 0, resolution.data[1]), i32_clamp((pos.data[1] + size), 0, resolution.data[1])}};
	for (I32 y = y_range.data[0]; (y < y_range.data[1]); y = y + 1) {
		const I32 y_offset = (y * (resolution.data[0] / vector_width));
		for (I32 xv = x_range.data[0]; (xv < x_range.data[1]); xv = xv + 1) {
			const V8I32 x = v8i32_add(vector_index, v8i32_set1((xv * vector_width)));
			const Aos2V8I32 rel = aos2v8i32_sub({{x, v8i32_set1(y)}}, aos2v8i32_set_scalar(pos));
			const V8I32 dist = v8i32_add(v8i32_abs(rel.data[0]), v8i32_abs(rel.data[1]));
			V8B32 vecc_mask5 = mask; { // vector if
				V8B32 vecc_mask6 = v8b32_and(vecc_mask5, v8i32_lt(dist, v8i32_set1(rad))); { // vector if
					V8B32 vecc_mask7 = v8b32_and(vecc_mask6, v8i32_gt(x, v8i32_set1(x_start))); { // vector if
						V8B32 vecc_mask8 = v8b32_and(vecc_mask7, v8i32_lt(x, v8i32_set1(x_end))); { // vector if
							framebuffer[(y_offset + xv)] = v8u32_blend(framebuffer[(y_offset + xv)], v8u32_set1(col), vecc_mask8);
						};
					};
				};
			};
		};
	};
}

static Aos2F32 normalize(Aos2F32 x) {
	return aos2f32_mul(x, aos2f32_set1(f32_rsqrt(length2(x))));
}

static F32 length2(Aos2F32 x) {
	return ((x.data[0] * x.data[0]) + (x.data[1] * x.data[1]));
}

static F32 length(Aos2F32 x) {
	return f32_sqrt(((x.data[0] * x.data[0]) + (x.data[1] * x.data[1])));
}

static F32 dot(Aos2F32 a, Aos2F32 b) {
	return ((a.data[0] * b.data[0]) + (a.data[1] * b.data[1]));
}

static Hit intersect_ray_aabb(Aos2F32 pos, Aos2F32 dir, Aos2F32 box_min, Aos2F32 box_max) {
	const Aos2F32 inv_dir = aos2f32_div(aos2f32_set1(1.0f), dir);
	const Aos2F32 t1 = aos2f32_mul(aos2f32_sub(box_min, pos), inv_dir);
	const Aos2F32 t2 = aos2f32_mul(aos2f32_sub(box_max, pos), inv_dir);
	Hit result = {0};
	result.tmin = f32_max(f32_min(t1.data[0], t2.data[0]), f32_min(t1.data[1], t2.data[1]));
	result.tmax = f32_min(f32_max(t1.data[0], t2.data[0]), f32_max(t1.data[1], t2.data[1]));
	result.hit = (B8)(result.tmax >= f32_max(0.0f, result.tmin));
	return result;
}

#endif // VECC_IMPL
