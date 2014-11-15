#ifndef __DISPLAY_H
#define __DISPLAY_H

#include <kernel.h>

typedef unsigned char SCREEN;

unsigned char left_margin = 0;

inline void get_lcd_lock() __naked {
	__asm
	PCALL(GETLCDLOCK)
	__endasm;
}

SCREEN *screen_allocate() __naked {
	__asm
	PCALL(ALLOCSCREENBUFFER)
	PUSH IY
	POP HL
	RET
	__endasm;
}

void screen_clear(SCREEN *screen) {
	__asm
	POP IX
	POP IY
	PCALL(CLEARBUFFER)
	PUSH IY
	PUSH IX
	__endasm;
	screen;
}

void screen_draw(SCREEN *screen) {
	__asm
	POP IX
	POP IY
	PCALL(FASTCOPY)
	PUSH IY
	PUSH IX
	__endasm;
	screen;
}

inline void set_left_margin(unsigned char margin) {
	left_margin = margin;
}

void draw_string(SCREEN *screen, unsigned char x, unsigned char y, const char *string) {
	__asm
	POP IX
	POP IY
	POP DE
	ld hl, _left_margin
	ld b, (hl)
	POP HL
		ld a, d
		ld d, e
		ld e, a
		PCALL(DRAWSTR)
	PUSH HL
	PUSH DE
	PUSH IY
	PUSH IX
	__endasm;
	screen; x; y; string;
}

#endif
