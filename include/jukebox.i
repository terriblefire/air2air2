; --------------------------< JUKEBOX SYSTEM >-------------------------------

kill_audio	macro
		move.w	#%0000000000001111,dmacon
		clr.w	aud0dat
		clr.w	aud1dat
		clr.w	aud2dat
		clr.w	aud3dat
		endm

music_flag	dc.w	0

jukebox		lea	playing_list,a1

		tst.w	mt_endflag
		bne.s	.newtrack	

		tst.w	playing_pos
		bne	terminate

		move.w	playing_pos,d0
		add.w	#8,playing_pos
		add.w	d0,a1
		move.l	(a1),a0
		bsr	mt_init
		
		rts

.newtrack	bsr	mt_end
		move.w	#1,music_flag
		kill_audio
		addq.w	#1,mt_counter2
		cmp.w	#4,mt_counter2
		ble	terminate

		clr.w	mt_counter2

		clr.w	mt_endflag
		bsr	mt_end
		move.w	#1,music_flag
		kill_audio
		
		lea	playing_list,a1
		
		move.w	playing_pos,d0
		add.w	#8,playing_pos
		add.w	d0,a1

		cmp.l	#-1,(a1)
		bne.s	.no_restart	
		
		move.w	#1,music_flag
		kill_audio
		bsr	mt_end

		clr.w	playing_pos
		lea	playing_list,a1

		move.w	playing_pos,d0
		add.w	d0,a1

.no_restart	
		move.l	(a1)+,a0
		move.l	(a1),d0
		move.b	d0,mt_auxspeedo
		move.b	d0,mt_auxspeede

		clr.w	music_flag
		bsr	mt_init
		
.exit		rts

mt_counter2	dc.w	0
restart		equ	-1
repeat		equ	-2
jumpb1		equ	-3
jumpb2		equ	-4
		
playing_list	dc.l	song1
		dc.l	6
		dc.l	song2
		dc.l	5
		dc.l	-1
		dc.l	-1
playing_pos	dc.w	0
repeat_flag	dc.w	0
	
;------------------------------< END JUKEBOX >-----------------------------
