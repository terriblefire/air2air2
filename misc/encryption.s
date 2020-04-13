; -----------------------------------------------------------------------------------

decode_file	move.l	a0,a5
		move.l	(a0)+,d0
		lsr.l	#1,d0
		sub.l	#2,d0
		lea	encode_table,a3

.loop		moveq.l	#0,d2
		moveq.l	#0,d1
		move.w	(a0),d1
		move.w	(a3)+,d2
		eor.w	d2,d1
		move.w	d1,(a0)+
		cmp.w	#-1,(a3)
		bne.s	.okay
		lea	encode_table,a3
.okay		dbra	d0,.loop
		
		move.l	a5,a0
		addq.l	#4,a0
		rts
		
; -------------------------------------------------------------------------
	
re_encode_file	move.l	a5,a0
		move.l	(a0)+,d0
		lsr.l	#1,d0
		sub.l	#2,d0
		lea	encode_table,a3

.loop		moveq.l	#0,d2
		moveq.l	#0,d1
		move.w	(a0),d1
		move.w	(a3)+,d2
		eor.w	d2,d1
		move.w	d1,(a0)+
		cmp.w	#-1,(a3)
		bne.s	.okay
		lea	encode_table,a3
.okay		dbra	d0,.loop
		
		rts

encode_table	dc.w	26374	; scramble table
		dc.w	31341
		dc.w	23441
		dc.w	34554
		dc.w	23452
		dc.w	13455
		dc.w	64333
		dc.w	53323
		dc.w	43531
		dc.w	32435
		dc.w	-1
	
; ------------------------------------------------------------------------------
