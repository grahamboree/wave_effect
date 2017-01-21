; Blank GB base file
; Luigi Guatieri
; superluigiland.net
; based on David Pello's tutorial

; hardware definitions
INCLUDE "gbhw.inc"

;user data
;**********************************************************************

;Equates
;**********************************************************************
; define sprites and attributes
; example
_SPR0_Y		EQU			_OAMRAM   ; store sprite 0, first object attribute memory ram location
					              ; 1st byte is y location
_SPR0_X		EQU			_OAMRAM+1
_SPR0_NUM	EQU			_OAMRAM+2	; tile number stored here
_SPR0_ATT	EQU			_OAMRAM+3	; byte for sprite atttributes



; Variables
; Variables for saving pad state
_PAD	EQU	_RAM	; save pad at beginning of internal ram

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
init:
	; pallet data
	
	;scroll variables
	
	call stopLCD
	
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




; SUbroutines here:	
; read pad routine
Read_pad:
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