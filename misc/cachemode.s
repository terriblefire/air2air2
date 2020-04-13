		move.l	d0,store1
		move.b	($2f6),d0
		eor.b	#1,d0
		move.b	d0,($2f6)
		move.l	store1,d0
		rts
		even
Store1		dc.l	0
store2		dc.l	0
	dc.b	"This program toggles between cache and not-cache mode "
	dc.b	"for Air2Air2"
	even
