; Blank GB base file
; Luigi Guatieri
; superluigiland.net
; based on David Pello's tutorial

; hardware definitions
INCLUDE "gbhw.inc"

;user data
;**********************************************************************

;Constants
;**********************************************************************
; define sprites and attributes

; EXAMPLE:
;_SPR0_Y	EQU			_OAMRAM   ; store sprite 0, first object attribute memory ram location
;					              ; 1st byte is y location
;_SPR0_X	EQU			_OAMRAM+1
;_SPR0_NUM	EQU			_OAMRAM+2	; tile number stored here
;_SPR0_ATT	EQU			_OAMRAM+3	; byte for sprite atttributes

; More OAM data for sprites goes here


;Variables
;**********************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BLOCK 0 is mostly for global data like buttons and world state
_RAM_BLOCK_0 EQU	_RAM						;RAM is 8K without bank switching

padInput		EQU		_RAM_BLOCK_0			;The input from the d-pad and buttons
currentWorld	EQU		_RAM_BLOCK_0+1			;Which world are we in
currentLevel	EQU		_RAM_BLOCK_0+2			;Which level are we on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BLOCK 1 is mostly for player data, including direction, animation states, sprite tile, etc.
_RAM_BLOCK_1			EQU	_RAM_BLOCK_0+128

playerLightXFrame		EQU _RAM_BLOCK_1		;player x coordinate (page in world)
playerLightXPixel		EQU _RAM_BLOCK_1+1		;player x coordinate (relative to page)
playerLightYFrame		EQU _RAM_BLOCK_1+2		;player y coordinate (page in world)
playerLightYPixel		EQU _RAM_BLOCK_1+3		;player y coordinate (relative to world)
playerLightFrame		EQU _RAM_BLOCK_1+4		;what frame of the animation
playerLightDirection 	EQU _RAM_BLOCK_1+5		;bit 0 = up/down, up = 0;  bit 1 = left/right, left = 0

; reserved for future player data

playerDarkXFrame		EQU _RAM_BLOCK_1+16		;player x coordinate (page in world)
playerDarkXPixel		EQU _RAM_BLOCK_1+17		;player x coordinate (relative to page)
playerDarkYFrame		EQU _RAM_BLOCK_1+18		;player y coordinate (page in world)
playerDarkYPixel		EQU _RAM_BLOCK_1+19		;player y coordinate (relative to world)
playerDarkFrame		EQU _RAM_BLOCK_1+20		;what frame of the animation
playerDarkDirection 	EQU _RAM_BLOCK_1+21		;bit 0 = up/down, up = 0;  bit 1 = left/right, left = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_RAM_BLOCK_2 EQU	_RAM_BLOCK_1+128
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_RAM_BLOCK_3 EQU	_RAM_BLOCK_2+128
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_RAM_BLOCK_4 EQU	_RAM_BLOCK_3+128
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_RAM_BLOCK_5 EQU	_RAM_BLOCK_4+128
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_RAM_BLOCK_6 EQU	_RAM_BLOCK_5+128
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_RAM_BLOCK_7 EQU	_RAM_BLOCK_6+128
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CARTRIDGE HEADER
;**********************************************************************
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
	

; initialization
; load pallets for sprites, windows and backgrounds here
; load map location and scroll variables
; remember to stop LCD before copying tiles to memory
.Init:
	; pallet data
	
	;scroll variables
	
	call StopLCD
	
	; copy tiles
	
	; copy tile maps
	
	; copy	window tile map
	
	;	erase sprite memory
	ld	de, _OAMRAM		; Sprite attribut memory
	ld	bc, 40*4		; 40 sprites, 4 bytes each
	ld	l, 0			; put everything to zero
	call	FillMemory	; Unused sprites remain off-screen
	
	;	create sprites

	; configure and activate display
	ld	a, LCDCF_ON|LCDCF_BG8000|LCDCF_BG9800|LCDCF_BGON|LCDCF_OBJ8|LCDCF_OBJON|LCDCF_WIN9C00
	ld	[rLCDC], a
	
;	MAIN LOOP

; GAMEPLAY CODE


; End of gameplay code
.waitForVBlank:
	ld	a, [rLY]	; check scanline
	cp	145	; compare to final scanline
	jr	nz, .waitForVBlank	; if not, loop again
	
	; we are in VBlank, turn off LCD
	ld	a, [rLCDC]	; load rLCDC in a
	res	7, a	; reset bit 7 to 0 in LCD
	ld	[rLCDC], a	; save changes

; RENDERING CODE

; Choose which world to show
	ld a, [currentWorld]
	add 0
	jr z, .RenderLight

.RenderDark:


	xor a
	jr z, .RenderOthers

.RenderLight:

; Render the appropriate sprites


; Subroutines here:

.RenderOthers:




; read input pad and store the state into a
ReadPad:
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
	ld	[padInput], a
	ret

;Turn off the LCD
;destroys a
StopLCD:
	ld	a,[rLCDC]
	rlca	; rotate high bit into the carry
	ret	nc	; the screen is already off

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