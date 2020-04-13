
; -------------------------------< JOYSTICK >-------------------------------

joysticks	move.w	joy0dat,d1
		bsr	scan_ports
		btst.b	#6,$bfe001	; Ciaapra
		bne.s	.no_fire1
		or.b	#16,d0
.no_fire1	move.b	d0,joyval1

		move.w	joy1dat,d1
		bsr	scan_ports
		btst.b	#7,$bfe001	; Ciaapra
		bne.s	.no_fire2
		or.b	#16,d0
.no_fire2	move.b	d0,joyval2
		rts

scan_ports	move.w	d1,d2
		lsl.w	#1,d2
		eor.w	d1,d2
		moveq.l	#0,d0	; Initialize joybyte.

		btst.l	#9,d2
		beq.s	.no_up
		or.b	#1,d0
	
.no_up		btst.l	#1,d2
		beq.s	.no_down
		or.b	#2,d0

.no_down	btst.l	#9,d1
		beq.s	.no_left
		or.b	#4,d0

.no_left	btst.l	#1,d1
		beq.s	.no_right
		or.b	#8,d0
.no_right	
fourplayer_adaptor
play3		moveq.l	#0,d0
		moveq.l	#0,d1
		move.b	$bfe101,d0
		not.b	d0
		and.b	#15,d0
		move.b	$bfd000,d1
		btst.l	#2,d1
		bne.s	.no_fire
		or.b	#16,d0
.no_fire	move.b	d0,joyval3

play4		moveq.l	#0,d0
		moveq.l	#0,d1
		move.b	$bfe101,d0
		lsr.b	#4,d0
		not.b	d0
		and.b	#15,d0
		move.b	$bfd000,d1
		btst.l	#0,d1
		bne.s	.no_fire
		or.b	#16,d0
.no_fire	move.b	d0,joyval4
		rts
