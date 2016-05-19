;
; F15 D'GAMMA CLONE
;

; Code and graphics by TMR


; A quick, written-from-scratch copy of the Apple II intros F-15
; Strike Eagle by Black Bag and The Six Pack's Lantern Of D'Gamma.
; Coded for C64CrapDebunk.Wordpress.com

; Notes: this source is formatted for the ACME cross assembler from
; http://sourceforge.net/projects/acme-crossass/

; build.bat will call ACME to create an assembled file but something
; like CiderPress is needed to inject the file into a bootable disk
; image.


; Memory Map
; $0800 - $09ff		work buffers for the text effect
; $0a00 - $17fd		program code/data
; $2000 - $3fff		bitmap data


; Select an output filename
		!to "fdclone#060a00",plain


; Yank in binary data
		* = $2000
char_data	!binary "binary/c64cd_screen.raw"


; Constants
rd_vbl_bar	= $c019		; top bit clear means vblank

txtclr		= $c050		; select bitmap mode
txtset		= $c051		; select text mode
mixclr		= $c052		; full screen
mixset		= $c053		; split screen (last four lines are text)

txtpage1	= $c054		; select text buffer 1 ($0400)
txtpage2	= $c055		; select text buffer 2 ($0x00)

lores		= $c056		; display low res graphics
hires		= $c057		; display high res graphics


; Label assignments
sine_at_1	= $50
sine_temp	= $51

logo_x		= $52
logo_timer	= $53
logo_count	= $54

rnd_1		= $55
rnd_2		= $56

scroll_count	= $57
scroll_timer	= $58
scroll_pos	= $59		; two bytes used


; Work spaces for the text effect
text_buffer	= $0200
effect_buffer	= $0300


; Set origin point (must be the same as the last four bytes of !to)
		* = $0a00


; Entry point for the code
entry		sei


; Configure display mode
		lda #$00
		sta txtclr
		sta hires
		sta mixset

; Initialise the logo movement routine
		lda #$00
		sta logo_x
		sta logo_timer
		sta logo_count

; Clear the work spaces
		ldx #$00
work_clear	lda #$20
		sta text_buffer,x
		lda #$00
		sta effect_buffer,x
		inx
		bne work_clear

; Reset the text
		jsr scroll_reset
		lda #$00
		sta scroll_count
		sta scroll_timer

; Main loop
main_loop	lda rd_vbl_bar
		bmi *-$03


; Update the upper colour bars
		lda sine_at_1
		clc
		adc #$02
		sta sine_at_1
		sta sine_temp

		ldx #$00
bar_loop_1	lda sine_temp
		clc
		adc #$09
		sta sine_temp
		tay
		lda bar_sinus,y
		tay

		lda bar_data_left+$00,y
		sta $2000,x
		lda bar_data_right+$00,y
		sta $2001,x

		lda bar_data_left+$01,y
		sta $2400,x
		lda bar_data_right+$01,y
		sta $2401,x

		lda bar_data_left+$02,y
		sta $2800,x
		lda bar_data_right+$02,y
		sta $2801,x

		lda bar_data_left+$03,y
		sta $2c00,x
		lda bar_data_right+$03,y
		sta $2c01,x

		lda bar_data_left+$04,y
		sta $3000,x
		lda bar_data_right+$04,y
		sta $3001,x

		lda bar_data_left+$05,y
		sta $3400,x
		lda bar_data_right+$05,y
		sta $3401,x

		lda bar_data_left+$06,y
		sta $3800,x
		lda bar_data_right+$06,y
		sta $3801,x

		lda bar_data_left+$07,y
		sta $3c00,x
		lda bar_data_right+$07,y
		sta $3c01,x

		inx
		inx
		cpx #$28
		bne bar_loop_1

; Move one of the letters in the rolling logo
		ldx logo_x
		cpx #$21
		bcc *+$05
		jmp logo_move_skip

		ldy #$00
logo_loop_1	lda $2100,x
		sta $2080,x
		lda $2500,x
		sta $2480,x
		lda $2900,x
		sta $2880,x
		lda $2d00,x
		sta $2c80,x
		lda $3100,x
		sta $3080,x
		lda $3500,x
		sta $3480,x
		lda $3900,x
		sta $3880,x
		lda $3d00,x
		sta $3c80,x

		lda $2180,x
		sta $2100,x
		lda $2580,x
		sta $2500,x
		lda $2980,x
		sta $2900,x
		lda $2d80,x
		sta $2d00,x
		lda $3180,x
		sta $3100,x
		lda $3580,x
		sta $3500,x
		lda $3980,x
		sta $3900,x
		lda $3d80,x
		sta $3d00,x

		lda $2200,x
		sta $2180,x
		lda $2600,x
		sta $2580,x
		lda $2a00,x
		sta $2980,x
		lda $2e00,x
		sta $2d80,x
		lda $3200,x
		sta $3180,x
		lda $3600,x
		sta $3580,x
		lda $3a00,x
		sta $3980,x
		lda $3e00,x
		sta $3d80,x

		lda $2280,x
		sta $2200,x
		lda $2680,x
		sta $2600,x
		lda $2a80,x
		sta $2a00,x
		lda $2e80,x
		sta $2e00,x
		lda $3280,x
		sta $3200,x
		lda $3680,x
		sta $3600,x
		lda $3a80,x
		sta $3a00,x
		lda $3e80,x
		sta $3e00,x

		lda $2300,x
		sta $2280,x
		lda $2700,x
		sta $2680,x
		lda $2b00,x
		sta $2a80,x
		lda $2f00,x
		sta $2e80,x
		lda $3300,x
		sta $3280,x
		lda $3700,x
		sta $3680,x
		lda $3b00,x
		sta $3a80,x
		lda $3f00,x
		sta $3e80,x

		lda $2380,x
		sta $2300,x
		lda $2780,x
		sta $2700,x
		lda $2b80,x
		sta $2b00,x
		lda $2f80,x
		sta $2f00,x
		lda $3380,x
		sta $3300,x
		lda $3780,x
		sta $3700,x
		lda $3b80,x
		sta $3b00,x
		lda $3f80,x
		sta $3f00,x

		lda $2028,x
		sta $2380,x
		lda $2428,x
		sta $2780,x
		lda $2828,x
		sta $2b80,x
		lda $2c28,x
		sta $2f80,x
		lda $3028,x
		sta $3380,x
		lda $3428,x
		sta $3780,x
		lda $3828,x
		sta $3b80,x
		lda $3c28,x
		sta $3f80,x

		lda $2080,x
		sta $2028,x
		lda $2480,x
		sta $2428,x
		lda $2880,x
		sta $2828,x
		lda $2c80,x
		sta $2c28,x
		lda $3080,x
		sta $3028,x
		lda $3480,x
		sta $3428,x
		lda $3880,x
		sta $3828,x
		lda $3c80,x
		sta $3c28,x

		inx
		iny
		cpy #$08
		beq logo_move_skip
		jmp logo_loop_1

; Update the rolling logo effect counters
logo_move_skip	ldx logo_timer
		inx
		cpx #$07
		bne lt_xb

		ldx logo_count
		inx
		stx logo_count

logo_xp_read	lda logo_x_pos,x
		cmp #$ff
		bne logo_xp_okay

		ldx #$00
		stx logo_count
		jmp logo_xp_read

logo_xp_okay	sta logo_x

		ldx #$00
lt_xb		stx logo_timer

; Update the lower colour bars
		ldx #$00
		ldy #$26
bar_loop_2	lda $2000,x
		sta $20a8,y
		lda $2400,x
		sta $24a8,y
		lda $2800,x
		sta $28a8,y
		lda $2c00,x
		sta $2ca8,y
		lda $3000,x
		sta $30a8,y
		lda $3400,x
		sta $34a8,y
		lda $3800,x
		sta $38a8,y
		lda $3c00,x
		sta $3ca8,y

		lda $2001,x
		sta $20a9,y
		lda $2401,x
		sta $24a9,y
		lda $2801,x
		sta $28a9,y
		lda $2c01,x
		sta $2ca9,y
		lda $3001,x
		sta $30a9,y
		lda $3401,x
		sta $34a9,y
		lda $3801,x
		sta $38a9,y
		lda $3c01,x
		sta $3ca9,y
		dey
		dey
		inx
		inx
		cpx #$28
		bne bar_loop_2

; Update the text area
		ldx #$00
text_update	lda text_buffer+$00,x
		ora effect_buffer+$00,x
		sta $0650,x
		lda text_buffer+$28,x
		ora effect_buffer+$28,x
		sta $06d0,x
		lda text_buffer+$50,x
		ora effect_buffer+$50,x
		sta $0750,x
		lda text_buffer+$78,x
		ora effect_buffer+$78,x
		sta $07d0,x
		inx
		cpx #$28
		bne text_update

; Fetch a character of text
		ldx scroll_timer
		inx
		cpx #$02
		bne st_xb

		ldx scroll_count
		cpx #$a0
		bcs scroll_skip
		ldy #$00
scroll_mread	lda (scroll_pos),y
		cmp #$ff
		bne scroll_okay
		jsr scroll_reset
		jmp scroll_mread

scroll_okay	sta text_buffer,x
		inc scroll_pos+$00
		bne *+$04
		inc scroll_pos+$01

scroll_skip	inx
		stx scroll_count

		ldx #$00
st_xb		stx scroll_timer

; Randomly toggle some of the character inversions
		jsr random
		jsr random

; Wait for the start of the screen
		lda rd_vbl_bar
		bpl *-$03

		jmp main_loop


; Randomly select a character and invert it
random		lda rnd_1
		clc
		adc #$01
		eor rnd_2
		lsr
		bcc *+$04
		ora #$80
		ldx rnd_2
		sta rnd_2
		stx rnd_1

		and #$7f
		lda effect_buffer+$00,x
		eor #$80
		sta effect_buffer+$00,x
		sta effect_buffer+$80,x

		rts


; Subroutine to reset the scrolling message
scroll_reset	lda #<scroll_text
		sta scroll_pos+$00
		lda #>scroll_text
		sta scroll_pos+$01
		rts


; Labels for the colour bar data to make it easier to read
black_l		= %00000000
black_r		= %00000000

purple_l	= %01010101
purple_r	= %00101010

green_l		= %00101010
green_r		= %01010101

blue_l		= %11010101
blue_r		= %10101010

orange_l	= %10101010
orange_r	= %11010101

white_l		= %01111111
white_r		= %01111111

; Data for the colour bars (two tables for odd and even bytes)
bar_data_left	!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte blue_l
		!byte black_l
		!byte blue_l
		!byte blue_l

		!byte blue_l
		!byte purple_l
		!byte blue_l
		!byte purple_l
		!byte purple_l
		!byte purple_l
		!byte white_l
		!byte purple_l

		!byte white_l
		!byte white_l
		!byte white_l
		!byte orange_l
		!byte white_l
		!byte orange_l
		!byte orange_l
		!byte orange_l

		!byte green_l
		!byte orange_l
		!byte green_l
		!byte green_l
		!byte green_l
		!byte black_l
		!byte green_l
		!byte black_l

		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte blue_l
		!byte black_l
		!byte blue_l

		!byte blue_l
		!byte blue_l
		!byte purple_l
		!byte blue_l
		!byte purple_l
		!byte purple_l
		!byte purple_l
		!byte white_l

		!byte purple_l
		!byte white_l
		!byte white_l
		!byte white_l
		!byte orange_l
		!byte white_l
		!byte orange_l
		!byte orange_l

		!byte orange_l
		!byte green_l
		!byte orange_l
		!byte green_l
		!byte green_l
		!byte green_l
		!byte black_l
		!byte green_l

		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l
		!byte black_l

bar_data_right	!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte blue_r
		!byte black_r
		!byte blue_r
		!byte blue_r

		!byte blue_r
		!byte purple_r
		!byte blue_r
		!byte purple_r
		!byte purple_r
		!byte purple_r
		!byte white_r
		!byte purple_r

		!byte white_r
		!byte white_r
		!byte white_r
		!byte orange_r
		!byte white_r
		!byte orange_r
		!byte orange_r
		!byte orange_r

		!byte green_r
		!byte orange_r
		!byte green_r
		!byte green_r
		!byte green_r
		!byte black_r
		!byte green_r
		!byte black_r

		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte blue_r
		!byte black_r
		!byte blue_r

		!byte blue_r
		!byte blue_r
		!byte purple_r
		!byte blue_r
		!byte purple_r
		!byte purple_r
		!byte purple_r
		!byte white_r

		!byte purple_r
		!byte white_r
		!byte white_r
		!byte white_r
		!byte orange_r
		!byte white_r
		!byte orange_r
		!byte orange_r

		!byte orange_r
		!byte green_r
		!byte orange_r
		!byte green_r
		!byte green_r
		!byte green_r
		!byte black_r
		!byte green_r

		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r
		!byte black_r

; Sine curve for the bar movement
bar_sinus	!byte $3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d
		!byte $3d,$3d,$3d,$3c,$3c,$3c,$3c,$3b
		!byte $3b,$3b,$3b,$3a,$3a,$39,$39,$39
		!byte $38,$38,$37,$37,$36,$36,$35,$35
		!byte $34,$34,$33,$33,$32,$32,$31,$30
		!byte $30,$2f,$2e,$2e,$2d,$2c,$2c,$2b
		!byte $2a,$2a,$29,$28,$27,$27,$26,$25
		!byte $25,$24,$23,$22,$21,$21,$20,$1f

		!byte $1e,$1e,$1d,$1c,$1b,$1b,$1a,$19
		!byte $18,$18,$17,$16,$15,$15,$14,$13
		!byte $13,$12,$11,$10,$10,$0f,$0e,$0e
		!byte $0d,$0d,$0c,$0b,$0b,$0a,$0a,$09
		!byte $09,$08,$07,$07,$06,$06,$06,$05
		!byte $05,$04,$04,$03,$03,$03,$02,$02
		!byte $02,$02,$01,$01,$01,$01,$00,$00
		!byte $00,$00,$00,$00,$00,$00,$00,$00

		!byte $00,$00,$00,$00,$00,$00,$00,$00
		!byte $00,$00,$00,$01,$01,$01,$01,$02
		!byte $02,$02,$03,$03,$03,$04,$04,$04
		!byte $05,$05,$06,$06,$07,$07,$08,$08
		!byte $09,$09,$0a,$0a,$0b,$0c,$0c,$0d
		!byte $0d,$0e,$0f,$0f,$10,$11,$11,$12
		!byte $13,$13,$14,$15,$16,$16,$17,$18
		!byte $19,$19,$1a,$1b,$1c,$1c,$1d,$1e

		!byte $1f,$1f,$20,$21,$22,$22,$23,$24
		!byte $25,$25,$26,$27,$28,$28,$29,$2a
		!byte $2b,$2b,$2c,$2d,$2d,$2e,$2f,$2f
		!byte $30,$31,$31,$32,$32,$33,$33,$34
		!byte $35,$35,$36,$36,$37,$37,$38,$38
		!byte $38,$39,$39,$3a,$3a,$3a,$3b,$3b
		!byte $3b,$3b,$3c,$3c,$3c,$3c,$3d,$3d
		!byte $3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d

; X offsets for the rolling logo effect
logo_x_pos	!byte $00,$fe,$08,$fe,$10,$fe,$18,$fe
		!byte $20,$fe,$fe,$fe,$20,$fe,$10,$fe
		!byte $00,$fe,$18,$fe,$08,$fe,$fe,$fe
		!byte $10,$fe,$08,$fe,$18,$fe,$00,$fe
		!byte $20,$fe,$fe,$fe,$fe,$20,$fe,$18
		!byte $fe,$10,$fe,$08,$fe,$00,$fe,$fe
		!byte $fe,$20,$18,$10,$08,$00,$08,$10
		!byte $18,$20,$18,$10,$08,$00,$fe,$fe

		!byte $ff		; end of data marker

		!scr "0123456789012345678901234567890123456789"
scroll_text	!scr " welcome to   -=- f15 d'gamma clone -=- "
		!scr "                                        "
		!scr "      coding and graphics by t.m.r      "
		!scr " released by c64cd on the 19th may 2016 "

		!scr " based on the apple ii crack intros for "
		!scr "   f-15 strike eagle by black bag and   "
		!scr "  lantern of d'gamma from the six pack  "
		!scr " (with a few extra c64 influences too!) "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!scr "this code uses $c019 to synchronise with"
		!scr " the vertical blank - that may not work "
		!scr "on all models of apple ii, but if anyone"
		!scr "tests on real hardware i'd like to know!"

		!scr " email me at tmr.c64cd@gmail.com to say "
		!scr "   if the code ran, which model of a2   "
		!scr "  and which cards are present, please!  "
		!scr "                                        "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!scr "c64cd alphabetically sorted greetings to"
		!scr "                                        "
		!scr "  1001 crew   ash and dave   black bag  "
		!scr "         borderzone dezign team         "

		!scr "    four horsemen  of the apocalypse    "
		!scr "happy demomaker  harlow cracking service"
		!scr "  mean team     paul, shandor and matt  "
		!scr "pulse productions  reset 86  rob hubbard"

		!scr "   scoop     stoat and tim    tangent   "
		!scr " thalamus     the commandos     the gps "
		!scr "  the six pack   we music   xess   yak  "
		!scr "                                        "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!scr "and we'll finish on a quick website plug"
		!scr "                                        "
		!scr "   visit  c64crapdebunk.wordpress.com   "
		!scr "        because.... well, stuff!        "

		!scr "                                        "
		!scr "                                        "
		!scr "                                        "
		!scr "                                        "

		!byte $ff		; end of text marker
