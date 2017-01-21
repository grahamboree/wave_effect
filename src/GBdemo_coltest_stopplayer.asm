; Hello window!
; Luigi Guatieri
; with stuff translated from David Pello

INCLUDE "gbhw.inc"				; hardware definitions


; constants
; define sprite location 
; sprite 0
_SPR0_Y		EQU			_OAMRAM   ; store sprite 0, first object attribute memory ram location
					              ; 1st byte is y location
_SPR0_X		EQU			_OAMRAM+1
_SPR0_NUM	EQU			_OAMRAM+2	; tile number stored here
_SPR0_ATT	EQU			_OAMRAM+3

; sprite 1, 1 sprite is 4 bytes long
_SPR1_Y		EQU			_OAMRAM+4
_SPR1_X		EQU			_OAMRAM+5
_SPR1_NUM	EQU			_OAMRAM+6
_SPR1_ATT	EQU			_OAMRAM+7

; sprite 2, sprite is 4 bytes
_SPR2_Y		EQU			_OAMRAM+8
_SPR2_X		EQU			_OAMRAM+9
_SPR2_NUM	EQU			_OAMRAM+10
_SPR2_ATT	EQU			_OAMRAM+11

; sprite 3
_SPR3_Y		EQU			_OAMRAM+12
_SPR3_X		EQU			_OAMRAM+13
_SPR3_NUM	EQU			_OAMRAM+14
_SPR3_ATT	EQU			_OAMRAM+15

; sprite 4 - bird
_SPR4_Y		EQU			_OAMRAM+16
_SPR4_X		EQU			_OAMRAM+17
_SPR4_NUM	EQU			_OAMRAM+18
_SPR4_ATT	EQU			_OAMRAM+19

; Variables/Equates
; Variables for saving pad state
_PAD		EQU			_RAM  ; Save at beginning of internal ram
; Sprite control variables
_POS_MAR_2	EQU			_RAM+1	; position to place 2nd sprites
_SPR_MAR_SUM	EQU		_RAM+2	; number to add the sprites to alternate them? CHECK THIS
_SPR4_ANIM_SPEED	EQU	_RAM+3	; var for animation speed of bird

; collision variables
_COLL_TILE_1	EQU	_RAM+4	; tile number for top tile
_COLL_TILE_2	EQU	_RAM+5	; tile number for bottom tile
_COLL_TILE_3	EQU	_RAM+11	; tile number for bottom tile
_COLL_TILE_4	EQU	_RAM+12	; tile number for bottom tile
_MOVABLE		EQU	_RAM+6	; use for moving
_TILE_CHECK_1	EQU	_RAM+7	; store checked tile
_TILE_CHECK_2	EQU	_RAM+8	; store checked tile 2
_NODIAG			EQU	_RAM+13	; stop diagonal movement

; math variables
_MD16temp    EQU _RAM+9
_MD16count   EQU _RAM+10



; Program Begins
SECTION "start",HOME[$0100] ; location to begin memory (< $0100 is saved for interupts)
							; HOME is memory bank 0 
	nop	; no operation
	jp	start
	
; ROM Header (Macro defined in gbhw.inc)
; defines ROM without mapper, 32K without RAM, the basics
;(like Tetris)
	ROM_HEADER ROM_NOMBC, ROM_SIZE_32KBYTE, RAM_SIZE_0KBYTE



; Our program begins
start:
	nop
	di			; disable interupts
	ld	sp, $ffff	; load stack pointer into highest ram location

init:
	
	; starting variable values
	ld	hl, _POS_MAR_2	; sprites looking to the right
	ld	[hl], -8
	
	ld	hl, _SPR_MAR_SUM	; start with 0100
	ld	[hl], 0
	
	ld	hl, _SPR4_ANIM_SPEED	; speed of bird animation
	ld	[hl], 15
	
	ld	hl, _COLL_TILE_1	; tile number for coll. check
	ld	[hl], $0
	
	ld	hl, _COLL_TILE_2
	ld	[hl], $0
	
	ld	hl, _MOVABLE
	ld	[hl], %00000000
	
	ld	hl, _NODIAG
	ld	[hl], %00000000
	
	ld	hl, _TILE_CHECK_1
	ld	[hl], $0
	
	ld	hl, _TILE_CHECK_2
	ld	[hl], $0
	
	; palletes
	ld	a, %11100100	; pallete colors, darkest to lightest
	ld	[rBGP], a		; load colors into contents of pallete register
	ld	[rOBP0], a		; load contents of pallete into sprite pallete
	
	; create another pallete for other sprites
	ld	a, %11010000	; for Mario
	ld	[rOBP1], a		; into location 1
	
	; scroll variables
	ld	a, 0	; write 0 for scroll x and y
	ld	[rSCX], a	; 0 into location of scroll x (visible location)
	ld	[rSCY], a	; 0 into location of scroll y (top left)
	
	; video
	call	stopLCD		; routine to shut off LCD
						; LCD must be off to load tiles
						
	; Load tiles into memory
	ld	hl, Tiles	; HL loaded with sprite data
	ld	de, _VRAM	; address for video memory into de
	ld	bc, FinTiles-Tiles	; number of bytes to copy

	call	CopyMemory
	
	; Load the tile map
	ld	hl, Mapa
	ld	de, _SCRN0		; map 0 loaction
	ld	bc, 32*32		; 32 by 32 tiles
	call	CopyMemory
	
	; load window tile map
	ld	hl, Window
	ld	de, _SCRN1		; map 1 location
	ld	bc, 32*32		; map size
	call CopyMemory
	
	; Loaded all map tiles
	; clean sprite memory
	ld	de, _OAMRAM		; Sprite attribut memory
	ld	bc, 43*4		; 42 sprites, 4 bytes each
	ld	l, 0			; put everything to zero
	call	FillMemory	; Unused sprites remain off-screen
	
	; Now we wil create the sprites
	ld	a, 136
	ld	[_SPR0_Y], a	; y position of sprite
	ld	a, 80
	ld	[_SPR0_X], a	; x position of sprite
	ld	a, 0			; select sprite 0
	ld	[_SPR0_NUM], a	; load a into contents of _SPR0_NUM
	ld	a, 16 | 32	
	ld	[_SPR0_ATT], a	; special attribute, pallet 1
	
	ld	a, 136+8
	ld	[_SPR1_Y], a	; y position of sprite
	ld	a, 80
	ld	[_SPR1_X], a	; x position of sprite
	ld	a, 1			; select sprite 0
	ld	[_SPR1_NUM], a	; load a into contents of _SPR0_NUM
	ld	a, 16 | 32	
	ld	[_SPR1_ATT], a	; special attribute, pallet 1
	
	ld	a, 136
	ld	[_SPR2_Y], a	; y position of sprite
	ld	a, [_POS_MAR_2]
	add	80
	ld	[_SPR2_X], a	; x position of sprite
	ld	a, 2			; select sprite 0
	ld	[_SPR2_NUM], a	; load a into contents of _SPR0_NUM
	ld	a, 16 | 32	
	ld	[_SPR2_ATT], a	; special attribute, pallet 1
	
	ld	a, 136+8
	ld	[_SPR3_Y], a	; y position of sprite
	ld	a, [_POS_MAR_2]
	add	80
	ld	[_SPR3_X], a	; x position of sprite
	ld	a, 3			; select sprite 0
	ld	[_SPR3_NUM], a	; load a into contents of _SPR0_NUM
	ld	a, 16 | 32	
	ld	[_SPR3_ATT], a	; special attribute, pallet 1
	
	; bird sprite location
	ld	a, 100
	ld	[_SPR4_Y], a	; y position of sprite
	ld	a, 80
	ld	[_SPR4_X], a	; x position of sprite
	ld	a, 33			; select sprite 33
	ld	[_SPR4_NUM], a	; load a into contents of _SPR0_NUM
	;ld	a, 16 | 32
	ld	a, %00110000
	ld	[_SPR4_ATT], a	; special attribute, pallet 1
	
	; configure and activate display
	ld	a, LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON|LCDCF_OBJ8|LCDCF_OBJON|LCDCF_WIN9C00
	ld	[rLCDC], a
	
; main loop
movement:
	
	; read the pad
	call	read_pad
	; first, we wait for VBlank to update sprites
	; Cannot change VRAM out of VBlank
.wait:
	ld	a, [rLY]	; check scanline
	cp	145	; compare to final scanline
	jr	nz, .wait	; if not, loop again

	call	move_bird
	call	anim_bird
; Now we move sprite depending on buttons
	ld	a, [_PAD]	; load status of pad
	and	%00010000	; right
	call	nz, move_right ; if pressed, call routine move right
	
	ld	a, [_PAD]
	and	%00100000	; left
	call	nz, move_left
	
	ld a, [_PAD]
	and	%01000000	; up
	call	nz, move_up
	
	ld	a, [_PAD]
	and	%10000000	; down
	call	nz, move_down
	
	ld	a, [_PAD]
	and	%00001000	; start
	call	nz, sample_window
	
	ld	a, [_PAD]
	and	%11111111
	call	z, no_press
	
	ld	a, [_PAD]
	and	%00000100	; select
	call	nz, change_bgtile
	
	ld	a, [_PAD]
	and	%0000010	; A?
	call	nz, change_bgtile1
	
	; reset movable
	call reset_movable
	call reset_NODIAG
	
	; a small delay
	ld	bc, 2000
	call	delay
	
	; restart loop
	jr	movement
	
; move bird
move_bird:
	
	ld	a, [_SPR4_X]	; load x pos
	add	1				; speed
	cp 168	; see if its at edge
	
	jr	z, .wrap_bird	; if it is, wrap it
	ld	[_SPR4_X], a	; save x pos
	
	
	
	
	ret

; wrap the bird on the screen
.wrap_bird:
	
	ld	a, 0	; return bird
	ld	[_SPR4_X], a	; save x pos
	
	ret
	
; animate bird
anim_bird:
	ld	a, [_SPR4_ANIM_SPEED]	; load anim speed var into a
	dec	a	; decrement
	ld	[_SPR4_ANIM_SPEED], a	; save value
	cp	0	; is it 0 yet?
	ret	nz	; if it is not, return (this is the delay)
	ld	a, 15	; reset when 0
	ld	[_SPR4_ANIM_SPEED], a	; save original value
	
	
	ld	a, [_SPR4_NUM]	; load tile number
	inc	a	; increment
	ld	[_SPR4_NUM], a	; save tile number
	cp	36	; check to see if it's over
	ret	nz	; if not, return
	

	
	ld	a, 33	; retart aniamtion loop
	ld	[_SPR4_NUM], a	; save sprite
	
	ret


	
; movement routines
move_right:
	
	 call	coll_right
	
	ld	a, [_MOVABLE]	; check if can move
	cp	0
	ret	z	; if can't return 0
	
	
	
	ld	a, 1
	ld	[_NODIAG], a	; set _NODIAG on
	
	
	
	ld	a, [_SPR0_X]	; grab current x position
	cp	120				; at the corner?
	jp	nz, .ar			; if we are not at the corner, move
	
	
	ld	a, [rSCX]	; if we are on the edge, we move the scroll
	inc	a
	ld	[rSCX], a	; load incremented scroll x
	
	
	; modify sprites
	call	num_spr_mario
	call	mario_walk
	
	ret

.ar:
	; the second sprites must be behind the first
	push	af	; push af to stack
	ld	a, -8
	ld	[_POS_MAR_2], a
	pop	af	; pop from stack
	; movement
	inc	a	; advance
	ld	[_SPR0_X], a	; save position
	ld	[_SPR1_X], a	
	
	ld	hl, _POS_MAR_2	; the first shift
	add	a, [hl]			; add
	ld	[_SPR2_X], a	; save sprite position
	ld	[_SPR3_X], a
	; right, therefor sprites must be flipped on x
	ld	a, [_SPR0_ATT]
	set	5, a	; set bit 5 to 0
	ld	[_SPR0_ATT], a
	ld	[_SPR1_ATT], a
	ld	[_SPR2_ATT], a
	ld	[_SPR3_ATT], a
	
	; modify sprites
	call	num_spr_mario
	call	mario_walk
	
	ret		; return

; check collision (test $99EC)
coll_right:
	
	; find tile lcoation: [((y/8)*31)+(y/8)]+(x/8)
	ld	a, [_SPR0_X]	; find sprite position
	;sub 8
	ld	b, a
	ld	a, [rSCX]
	add	b	; add scroll value to pos
	
	ld b, a	; divide by 8
	srl	b
	srl	b
	srl	b
	
	ld	d, $00
	ld	e, b	; load de with a
	
	
	
	
	ld	a, d
	ld	[_COLL_TILE_1], a	; save x location
	ld	a, e
	ld	[_COLL_TILE_2],	a
	

	
	ld	a, [_SPR0_Y]	; find y pos
	sub 16
	
	ld	b, a	; divide by 8
	srl b
	srl b
	srl b
	
	ld	d, $00
	ld	e, b	; load de with a
	
	
	
	
	ld	a,	d
	ld	[_COLL_TILE_3], a	; save y location
	ld	a, e
	ld	[_COLL_TILE_4],	a
	
	;ld	e, 15	; checking without the divide, begin
	;ld	d, 0
	
	ld	h, $00
	ld	l, $1F	; multiply by 31
	
	call	mul_DE_HL_BCDEu	; multiplication, BCDE is result checking if multiplying first will do anything
	
	
	ld	a, [_COLL_TILE_3]
	ld	h, a	; re-load y block into hl
	ld	a, [_COLL_TILE_4]
	ld	l, a
	

	
	add	hl, de	; add the second y
	
	ld	d, h	; move hl into de
	ld	e, l
	
	ld	a, [_COLL_TILE_1]	; retrieve x block location
	ld	h, a
	ld	a, [_COLL_TILE_2]
	ld	l, a
	

	
	add	hl, de	; add the final x, should be tile location now
	
	
	ld	d, $98	; setting up: HL + DE
	ld	e, $00

	add	hl, de		; add to map location
	
	
	; compare tile in front
	ld	a, [hl]	; just testing if it changes
	
	cp	40	; check if it's a collidable tile coin
	
	jr	z, .changetile_coll
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable
	
	
	; check next tile
	; just checking next tile down
	ld	d, $00	; get next row down if collided
	ld	e, $20
	add	hl, de
	ld	a, [hl]	; check tile number if coin:
	cp	40
	
	jr	z, .changetile_coll
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable
	
	;check next one
	ld	d, $00	; get next row down if collided
	ld	e, $20
	add	hl, de
	ld	a, [hl]	; check tile number if coin:
	
	cp	40
	jr	z, .changetile_coll
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable
	
		
	ret
	
.changetile_coll:
	ld	a, 41
	ld	[hl], a
	ret
	
.set_movable:
	ld	a, 0
	ld	[_MOVABLE], a ; make movable
	ret
	


reset_movable:
	ld	a, 1
	ld	[_MOVABLE], a
	ret


	
; Multiply
mul_DE_HL_BCDEu::
        push    hl              ; Save multiplier.
        ld      c,h             ; Save MSBs of multiplier.
        ld      a,l             ; LSBs to A for an 8 x 16 multiply.

        ld      b,0             ; Handy 0 to B for carry propagation.
        ld      h,b             ; Init LSBs of product.
        ld      l,b

        add     a,a             ; Test multiplier bit.
        jr      nc,.mul2
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul2   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul3
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul3   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul4
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul4   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul5
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul5   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul6
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul6   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul7
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul7   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul8
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)
.mul8   add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul9
        add     hl,de           ; Add multiplicand to product.
        adc     a,b             ; (Product in AHL)

.mul9   push    hl              ; Save LSBs in stack.
        ld      h,b             ; Zero second product.
        ld      l,b             ; .
        ld      b,a             ; Save MSBs of first product in B
        ld      a,c             ; Get MSBs of multiplier.
        ld      c,h             ; Handy 0 in C this time.

        add     a,a             ; Test multiplier bit.
        jr      nc,.mul20
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul20  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul21
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul21  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul22
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul22  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul23
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul23  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul24
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul24  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul25
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul25  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul26
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)
.mul26  add     hl,hl           ; Shift product left.
        adc     a,a             ; Test multiplier bit.
        jr      nc,.mul27
        add     hl,de           ; Add multiplicand to product.
        adc     a,c             ; (Product in AHL)

.mul27  pop     de              ; Fetch LSBs of 1st product.
        ld      c,a             ; Add partial products.
        ld      a,d             ; .
        add     a,l             ; .
        ld      d,a             ; .
        ld      a,b             ; .
        adc     a,h             ; .
        ld      h,a             ; .
        ld      a,c             ; .
        adc     a,0             ; .
        ld      b,a             ; .
        ld      c,h             ; .
        pop     hl              ; Restore multiplier.
        ret
 
move_left:
	
	ld	a, 1
	ld	[_NODIAG], a	; set _NODIAG on

	
	call	coll_left
	
	ld	a, [_MOVABLE]	; check if can move
	cp	0
	ret	z	; if can't return 0
	

	
	ld	a, [_SPR0_X]	; grab current x position
	cp	16				; at the corner?
	jp	nz, .al			; if we are not at the corner, move
	
	ld	a, [rSCX]	; if we are on the left edge, we move the scroll
	dec	a
	ld	[rSCX], a	; load incremented scroll x
	
	; modify sprites
	call	num_spr_mario
	call	mario_walk
	
	ret

.al:
	; the second sprites must be behind the first
	push	af	; push af to stack
	ld	a, 8
	ld	[_POS_MAR_2], a
	pop	af	; pop from stack
	; movement
	dec	a	; step back
	ld	[_SPR0_X], a	; save position
	ld	[_SPR1_X], a	
	
	ld	hl, _POS_MAR_2	; the first shift
	add	a, [hl]			; add
	ld	[_SPR2_X], a	; save sprite position
	ld	[_SPR3_X], a
	; right, therefor sprites must be flipped on x
	ld	a, [_SPR0_ATT]
	res	5, a	; set bit 5 to 0
	ld	[_SPR0_ATT], a
	ld	[_SPR1_ATT], a
	ld	[_SPR2_ATT], a
	ld	[_SPR3_ATT], a
	
	; modify sprites
	call	num_spr_mario
	call	mario_walk
	
	ret		; return
	
; check collision left
coll_left:
	
	; find tile lcoation: [((y/8)*31)+(y/8)]+(x/8)
	ld	a, [_SPR0_X]	; find sprite position
	sub 8
	ld	b, a
	ld	a, [rSCX]
	add	b	; add scroll value to pos
	
	ld b, a	; divide by 8
	srl	b
	srl	b
	srl	b
	
	ld	d, $00
	ld	e, b	; load de with a
	;ld	b, $00
	;ld	c, $08	; number to divide by
	
	;call div_DE_BC_DEBCu	; divide, result is in DE
	
	
	
	ld	a, d
	ld	[_COLL_TILE_1], a	; save x location
	ld	a, e
	ld	[_COLL_TILE_2],	a
	

	
	ld	a, [_SPR0_Y]	; find y pos
	sub 16
	
	ld	b, a	; divide by 8
	srl b
	srl b
	srl b
	
	ld	d, $00
	ld	e, b	; load de with a
	;ld	b, $00
	;ld	c, $08	; number to divide by
	
	;call div_DE_BC_DEBCu	; divide, result is in DE
	
	
	
	ld	a,	d
	ld	[_COLL_TILE_3], a	; save y location
	ld	a, e
	ld	[_COLL_TILE_4],	a
	
	;ld	e, 15	; checking without the divide, begin
	;ld	d, 0
	
	ld	h, $00
	ld	l, $1F	; multiply by 31
	
	call	mul_DE_HL_BCDEu	; multiplication, BCDE is result checking if multiplying first will do anything
	
	
	ld	a, [_COLL_TILE_3]
	ld	h, a	; re-load y block into hl
	ld	a, [_COLL_TILE_4]
	ld	l, a
	
	;ld	l, 15
	;ld	h, 0
	
	add	hl, de	; add the second y
	
	ld	d, h	; move hl into de
	ld	e, l
	
	ld	a, [_COLL_TILE_1]	; retrieve x block location
	ld	h, a
	ld	a, [_COLL_TILE_2]
	ld	l, a
	
	;ld	l, 13
	;ld	h, 0
	
	add	hl, de	; add the final x, should be tile location now
	
	
	ld	d, $98	; setting up: HL + DE
	ld	e, $00
	;ld	h, a	
	;ld	a, $00
	;ld	l, a
	;ld	b, $98	; get high nibble of memory location
	add	hl, de		; add to map location
	;ld	d, a	; put it back in there
	
	;ld	a, e	; move result to de
	;ld	[_TILE_CHECK_2], a	; save result
	;ld	a, d	; 
	;ld	[_TILE_CHECK_1], a	; save result
	
	; compare tile in front
	ld	a, [hl]	; just testing if it changes
	
	cp	40	; check if it's a collidable tile coin
	
	jr	z, .changetile_coll2
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable2
	
	
	; check next tile
	; just checking next tile down
	ld	d, $00	; get next row down if collided
	ld	e, $20
	add	hl, de
	ld	a, [hl]	; check tile number if coin:
	cp	40
	
	jr	z, .changetile_coll2
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable2
	
	;check next one
	ld	d, $00	; get next row down if collided
	ld	e, $20
	add	hl, de
	ld	a, [hl]	; check tile number if coin:
	
	cp	40
	jr	z, .changetile_coll2
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable2
	
	
	ret

.changetile_coll2:
	ld	a, 41
	ld	[hl], a
	ret
	
.set_movable2:
	ld	a, 0
	ld	[_MOVABLE], a ; make unmovable
	ret
	

move_up:

	ld	a, [_NODIAG]	; check if can go diagonally
	cp	1
	ret	z	; if can't return 0
	
		ld	a, [_SPR0_Y]	; load current position
		cp	16	; at top?
		ret	z	; return if true
		
	;	dec	a	; step back
	;	ld	[_SPR0_Y], a	; save position
		
	;	ret
		
		; the second sprites must be behind the first
	push	af	; push af to stack
	ld	a, 8
	ld	[_POS_MAR_2], a
	pop	af	; pop from stack
	; movement
	dec	a	; step back
	ld	[_SPR0_Y], a	; save position
	ld	[_SPR2_Y], a	
	
	ld	hl, _POS_MAR_2	; the first shift
	add	a, [hl]			; add
	ld	[_SPR1_Y], a	; save sprite position
	ld	[_SPR3_Y], a
	; right, therefor sprites must be flipped on x
	;ld	a, [_SPR0_ATT]
	;set	5, a	; set bit 5 to 0
	;ld	[_SPR0_ATT], a
	;ld	[_SPR1_ATT], a
	;ld	[_SPR2_ATT], a
	;ld	[_SPR3_ATT], a
	
	; modify sprites
	call	num_spr_mario
	call	mario_walk
	
	ret		; return

move_down:
	
	call coll_down
	
	; check if can move
	ld	a, [_MOVABLE]	; check if can move
	cp	0
	ret	z	; if can't return 0
	
	ld	a, [_NODIAG]	; check if can go diagonally
	cp	1
	ret	z	; if can't return 0
	
	ld	a, [_SPR0_Y]	; get position
	cp	152	; at bottom?
	ret	z	; return if true
		
		;inc	a	; advance
		;ld	[_SPR0_Y], a	; save position
		
		;ret
		
			; the second sprites must be behind the first
	push	af	; push af to stack
	ld	a, 8
	ld	[_POS_MAR_2], a
	pop	af	; pop from stack
	; movement
	inc	a	; step back
	ld	[_SPR0_Y], a	; save position
	ld	[_SPR2_Y], a	
	
	ld	hl, _POS_MAR_2	; the first shift
	add	a, [hl]			; add
	ld	[_SPR1_Y], a	; save sprite position
	ld	[_SPR3_Y], a
	; right, therefor sprites must be flipped on x
	;ld	a, [_SPR0_ATT]
	;set	5, a	; set bit 5 to 0
	;ld	[_SPR0_ATT], a
	;ld	[_SPR1_ATT], a
	;ld	[_SPR2_ATT], a
	;ld	[_SPR3_ATT], a
	
	; modify sprites
	call	num_spr_mario
	call	mario_walk
	
	ret		; return
	
; check collision (test $99EC)
coll_down:
	
	; find tile lcoation: [((y/8)*31)+(y/8)]+(x/8)
	ld	a, [_SPR0_X]	; find sprite position
	sub 8
	ld	b, a
	ld	a, [rSCX]
	add	b	; add scroll value to pos
	
	ld b, a	; divide by 8
	srl	b
	srl	b
	srl	b
	
	ld	d, $00
	ld	e, b	; load de with a
	
	
	
	
	ld	a, d
	ld	[_COLL_TILE_1], a	; save x location
	ld	a, e
	ld	[_COLL_TILE_2],	a
	

	
	ld	a, [_SPR0_Y]	; find y pos
	;add 8
	
	ld	b, a	; divide by 8
	srl b
	srl b
	srl b
	
	ld	d, $00
	ld	e, b	; load de with a
	
	
	
	
	ld	a,	d
	ld	[_COLL_TILE_3], a	; save y location
	ld	a, e
	ld	[_COLL_TILE_4],	a
	
	;ld	e, 15	; checking without the divide, begin
	;ld	d, 0
	
	ld	h, $00
	ld	l, $1F	; multiply by 31
	
	call	mul_DE_HL_BCDEu	; multiplication, BCDE is result checking if multiplying first will do anything
	
	
	ld	a, [_COLL_TILE_3]
	ld	h, a	; re-load y block into hl
	ld	a, [_COLL_TILE_4]
	ld	l, a
	

	
	add	hl, de	; add the second y
	
	ld	d, h	; move hl into de
	ld	e, l
	
	ld	a, [_COLL_TILE_1]	; retrieve x block location
	ld	h, a
	ld	a, [_COLL_TILE_2]
	ld	l, a
	

	
	add	hl, de	; add the final x, should be tile location now
	
	
	ld	d, $98	; setting up: HL + DE
	ld	e, $00

	add	hl, de		; add to map location
	
	
	; compare tile underneath
	ld	a, [hl]	; just testing if it changes
	
	cp	40	; check if it's a collidable tile coin
	
	jr	z, .changetile_coll3
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable3
	
	
	; check next tile
	; just checking next tile across
	;ld	d, $00	; get next row down if collided
	;ld	e, $20
	;add	hl, de
	
	; check tile to left
	dec	hl
	ld	a, [hl]	; check tile number if coin:
	cp	40
	
	jr	z, .changetile_coll3
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable3
	
	;check next one
	;ld	d, $00	; get next row down if collided
	;ld	e, $20
	;add	hl, de
	
	; check tile to right
	inc	hl
	inc hl
	ld	a, [hl]	; check tile number if coin:
	
	cp	40
	jr	z, .changetile_coll3
	
	cp	42		; if not coin, check if block
	jr	z, .set_movable3
	
		
	ret
	
.changetile_coll3:
	ld	a, 41
	ld	[hl], a
	ret
	
.set_movable3:
	ld	a, 0
	ld	[_MOVABLE], a ; make movable
	ret
	

sample_window:
	ld	a, 8
	ld	[rWX], a	; window register with 8
	
	ld	a, 144
	ld	[rWY], a	; load window height
	
	; activate windows and deactivate sprites
	ld	a, [rLCDC]	; load LCD control contents
	or	LCDCF_WINON	; check if window is on
	res	1, a	; bit 1 to 0
	ld	[rLCDC], a
	
	; animation
	ld	a, 144
.anim_open_window
	push	af
	ld	bc, 1000
	call	delay
	pop	af
	dec	a
	ld	[rWY], a
	jr	nz, .anim_open_window
	
	; check if select is pressed to exit
.check_exit
	call	read_pad
	and		%00001000	; start button
	jr	z, .check_exit

.anim_close_window
	push	af
	ld	bc, 1000
	call	delay
	pop	af
	inc	a
	ld	[rWY],a
	cp	144
	jr	nz, .anim_close_window
	
	; deactivate the window and activate the sprites
	ld	a, [rLCDC]
	res	5, a	; reset window sprites to 0
	or	LCDCF_OBJON	; turn on objects
	ld	[rLCDC], a	; apply changes
	ret
	
; change a single BG tile with pressed
change_bgtile:
	ld	hl, $9883 ; get tile location
	ld	a, [hl]	; load contents into a
	ld	b, [hl]	; load b with tile info as well
	cp	2
	jr	nz, .changetile
	ret

.changetile:
	ld	a, 2
	ld	[hl], a
	ret
	
; change a single BG tile with pressed A btn
change_bgtile1:
	ld	hl, $9883 ; get tile location
	ld	a, [hl]	; load contents into a
	ld	b, [hl]	; load b with tile info as well
	cp	4
	jr	nz, .changetile1
	ret

.changetile1:
	ld	a, 4
	ld	[hl], a
	ret

; clear moving diagonally
reset_NODIAG:
	ld	a, 0
	ld	[_NODIAG], a
	ret
	
; if nothing is pressed, set mario to idle
no_press:
	
;	call reset_NODIAG
	
	ld	hl, _SPR_MAR_SUM ; start with 0
	ld	[hl], 0
	ld	a, 0
	ld	[_SPR0_NUM], a
	inc	a
	ld	[_SPR1_NUM], a
	inc	a
	ld	[_SPR2_NUM], a
	inc	a
	ld	[_SPR3_NUM], a
	ret

; modify this variable to change mario sprites walking
num_spr_mario:
	ld	a, [_SPR_MAR_SUM]
	add	4
	ld	[_SPR_MAR_SUM], a	; save a
	cp	12	; less than 12?
	ret	nz	; return if less than
	
	ld	a, 4	; 12, then back to 4
	ld	[_SPR_MAR_SUM], a
	ret

; walk animation
mario_walk:
	ld	a, [_SPR_MAR_SUM]
	ld	[_SPR0_NUM], a
	inc	a
	ld	[_SPR1_NUM], a
	inc	a
	ld	[_SPR2_NUM], a
	inc	a
	ld	[_SPR3_NUM], a
	ret
	
	
; Read pad routine
read_pad:
	; check the d-pad
	ld	a, %00100000	; bit 4 to 0, 5 to 1 (Activate d-pad, not buttons)
	ld	[rP1], a	; button register
	
	; now we read the state of the d-pad, and avoid bouncing
	ld	a, [rP1]
	ld	a, [rP1]
	ld	a, [rP1]
	ld	a, [rP1]
	
	and	$0F	; only care about the lower 4 bits
	swap	a	; lower and upper combined
	ld	b, a	; save state in b
	
	; check buttons
	ld	a, %00010000	; bit 4 to 1, 5 to 0 (activated buttons, no d-pad)
	ld	[rP1], a
	
	; read several times to avoid bouncing
	ld	a, [rP1]
	ld	a, [rP1]
	ld	a, [rP1]
	ld	a, [rP1]
	
	; check A against buttons
	and $0F	; only care about bottom 4 bits
	or b	; or with b to 'meter' the d-pad status
	
	; now we have in A the state of all buttons, compliment and store variable
	cpl
	ld	[_PAD], a
	ret
	
stopLCD:
	ld	a,[rLCDC]
	rlca	; rotate high bit into the carry
	ret	nc	; the screen is already off

.wait_VBlank
	ld	a, [rLY]
	cp	145
	jr	nz, .wait_VBlank
	
	; we are in VBlank, turn off LCD
	ld	a, [rLCDC]	; load rLCDC in a
	res	7, a	; reset bit 7 to 0 in LCD
	ld	[rLCDC], a	; save changes
	
	ret
; delay routine
; bc = number of iterations
delay:
.slow:
	dec	bc	; decrement
	ld	a, b	; see if zero, load b into a, check with c
	or	c
	jr	z, .fin_delay
	nop
	jr	.slow
.fin_delay:
	ret

; memory copy routine
; copy number of bytes from one directory to another
; expects parameters:
; hl: copying data address
; de: destination address
; bc: number of data to be copied
; destroys contents of about
CopyMemory:
	ld	a, [hl]	; load data to be copied in a
	ld	[de], a	; load copied to data to new address
	dec	bc	; moving to next copy
	; check if bc is zero
	ld	a, c
	or	b
	ret	z ; if zero, return
	; no? continue
	inc hl
	inc de
	jr	CopyMemory
	
; fill memory routine
; fill a number of bytes of memory with data
; expects the parameters:
; de: destination address
; bc: number of data to fill
; l: data to fill
FillMemory:
	ld	a, 1
	ld	[de], a	; puts data in destination
	dec	bc	; next fill
	
	ld	a, c
	or b
	ret	z	; return if zero
	inc	de	; keep going
	jr	FillMemory

;Tiles:
;INCLUDE "mario_sprites.z80"
;FinTiles:

Tiles:
INCLUDE "luigilarge.z80"
FinTiles:

;screen size 20x17
Mapa:
;INCLUDE"mapa_mario.z80"
INCLUDE"luigilargemap.z80"
FinMapa:

Window:
;INCLUDE "ventana.z80"
INCLUDE "luigilarge_window.z80"
FinVentana:
	