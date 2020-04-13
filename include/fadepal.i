
fadepalin	lea	colours,a0
		
		move.w	#16,d4	; loop so that we update all 16 colours
		

red1		move.w	(a0),d0 ; move a system colour to a data register
		lsr.w	#8,d0	; Strip it down so that we only have the red
				; nibble (4 bit number)
		move.w	(a1),d1 ;
		lsr.w	#8,d1   ; Do the same with the hiiden colour
		
		cmp.w	d1,d0	; compare them
		
		blt	addr	; If the red value is too high decrement it.
		bgt	subr	; If the red value is too low increment it.

				; If its equal leave it as it is
green		move.w	(a0),d0 
		lsl.w	#colequ1,d0
		
		move.w	#colequ2,d3
		lsr.w	d3,d0
		
		move.w	(a1),d1
		lsl.w	#colequ1,d1
		
		move.w	#colequ2,d3
		lsr.w	d3,d1
		
		cmp.w	d1,d0
		
		blt	addg
		bgt	subg

blue		move.w	(a0),d0
		move.w	#colequ2,d3
		
		lsl.w	d3,d0
		lsr.w	d3,d0
		
		move.w	(a1),d1
		move.w	#colequ2,d3
		
		lsl.w	d3,d1
		lsr.w	d3,d1
		
		cmp.w	d1,d0
		
		blt	addb
		bgt	subb
		
nextcol		add.l	#2,a0
		add.l	#2,a1

		sub.w	#1,d4
		bne	red1

		lea	colours,a0
		bsr	set_colours

		rts
		
addr		add.w	#$100,(a0) ; inrcrement the red value.
		bra	green

addg		add.w	#$010,(a0) ; inrcrement the green value.
		bra	blue
		
addb		add.w	#$001,(a0) ; increment the blue value.
		bra	nextcol
		
subr		sub.w	#$100,(a0) ; decrement the red value.
		bra	green

subg		sub.w	#$010,(a0) ; decrement the green value.
		bra	blue
		
subb		sub.w	#$001,(a0) ; decrement the blue value.
		bra	nextcol
	
; ---------------------------< ERROR TRAPS >----------------------------------
pal_error1	lea	pal_error,a0
		move.w	#1,(pal_ecode)

		rts
	
Pal_Error	dc.b	"Pallette Error",0
Pal_Ecode	dc.w	0
pal_ok		dc.b	"Pallette Ok",0

; --------------------------------< END >------------------------------------
