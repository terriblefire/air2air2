menu		lea	colours,a1
		clear_pal
		lea	bak_colours,a1
		clear_pal
		bsr	cls_both
		lea	menu_sprites,a0
		bsr	tasking
		lea	colours,a1
		clear_pal
		lea	menu_sprites,a0
.init		tst.w	spr_type(a0)
		bmi.s	.done
		move.w	spr_xstart(a0),spr_xpos(a0)
		move.w	spr_ystart(a0),spr_ypos(a0)
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		
		move.w	#50,spr_vel(a0)
		
		clr.l	spr_long1(a0)
		clr.l	spr_long2(a0)
		
		lea	sprlist_len(a0),a0
		bra.s	.init
.done
		lea	bak_colours,a1
		move.l	bak_screen,a2
		lea	menu_pic,a0
		
		bsr	show_pic
		
		lea	blocks1,a0
		bsr	grab_blocks
		
light_menu	bsr	halt

		move.l	bak_screen,d2
		bsr	de_block
		
		lea	menu_sprites,a0
		bsr	blit_sprites
		
		
		bsr	swap_screen
		
		lea	bak_colours,a1
		bsr	fadepalin
		
		add.w	#1,menu_counter
		
		cmp.w	#menu_count,menu_counter
		bmi	light_menu
		

		clr.w	menu_counter

; -----------------------------< MENU MAIN LOOP > ----------------------------------

menu_loop	bsr	halt
		
		move.l	bak_screen,d2
		bsr	de_block
		
		lea	menu_sprites,a0
		bsr	tasking
		
		lea	menu_sprites,a0
		bsr	blit_sprites
		
		bsr	swap_screen
		
		cmp.b	#KB_SPACE,keycode
		bne.s	menu_loop
		
		lea	bak_colours,a1
		clear_pal
		
		clr.w	count

seed_loop	bsr 	halt

		cmp.b	#KB_SPACE,keycode
		bne.s	fade_menu
	
		add.w	#1,seed

		bra	seed_loop
		
fade_menu	;cmp.w	#1,seed
		;beq	go_asm


		bsr	halt
		
		move.l	bak_screen,d2
		bsr	de_block
		
		bsr	swap_screen

		lea	bak_colours,a1
		bsr	fadepalin
		
		add.w	#1,count
		cmp.w	#gap_count,count
		bne.s	fade_menu

		rts

; -----------------------------------------------------------------------------------

menu_counter	dc.l	0

menu_sprites
		dc.w	16*147
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	timer_score
		dc.w	145
		dc.w	90
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		
		
		dc.w	16*136
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	score_tens
		dc.w	177
		dc.w	87
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0


		dc.w	16*139
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	score_units
		dc.w	177+16
		dc.w	87
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0


		dc.w	16*44
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	terminate
		dc.w	190
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*51
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	terminate
		dc.w	190+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*148
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	terminate
		dc.w	172
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*3
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	players2
		dc.w	110
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	1
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		
		dc.w	16*11
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	players2
		dc.w	110+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	1
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*3
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	players3
		dc.w	110
		dc.w	112
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		
		dc.w	16*11
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	players3
		dc.w	110+32
		dc.w	112
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*101
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	players3
		dc.w	110
		dc.w	128
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		
		dc.w	16*109
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	players3
		dc.w	110+32
		dc.w	128
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*88
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	anti_grav
		dc.w	130
		dc.w	160
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		
		dc.w	16*151
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	night_day
		dc.w	180
		dc.w	160
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
	
		dc.w	16*154
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	terminate
		dc.w	180+32
		dc.w	160
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
	
		dc.w	16*149
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	cursor
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0 ; cursor x pos
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		
		dc.w	16*153
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	follow_pl
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
	
	
		dc.w	-1

; -----------------------------------------------------------------------------
		rsreset
csr_xpos	rs.w	1
csr_ypos	rs.w	1
csr_type1	rs.w	1
csr_type2	rs.w	1
csr_operand	rs.l	1
csr_min		rs.w	1
csr_max		rs.w	1
csrlist_len	rs.w	0


; --------------------------------< TASKS >------------------------------------

timer_score
		tst.w	timerQuest
		bne.s	.score

		move.w	#146*16,spr_type(a0)
		
		rts

.score		move.w	#147*16,spr_type(a0)

		rts

; -------------------------------------------------------------------------------
score_units	move.w	time,d0

		cmp.w	#9,d0
		bgt.s	.strip		
.stripped	
		
		add.w	#136,d0
		lsl.w	#4,d0
		
		move.w	d0,spr_type(a0)
		
		clr.l	d0
		move.w	time,d0
		
		mulu.w	#50,d0
		mulu.w	#60,d0
		
		move.w	d0,counter

		rts
		
.strip		add.w	#1,d1
		sub.w	#10,d0
		cmp.w	#9,d0
		
		bgt.s	.strip
		bra.s	.stripped

score_tens	clr.w	d1
		move.w	time,d0

		cmp.w	#9,d0
		bgt.s	.strip		
.stripped	
		
		add.w	#136,d1
		lsl.w	#4,d1
		
		move.w	d1,spr_type(a0)
		
		rts

.strip		add.w	#1,d1
		sub.w	#10,d0
		cmp.w	#9,d0
		
		bgt.s	.strip
		bra.s	.stripped

		
; -------------------------------------------------------------------------------

night_day	tst.w	dayQuest
		beq.s	.night

.day		lea	block_pic,a1
		move.l	a1,which_background
		
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		
		move.w	#16*150,spr_type(a0)
		move.w	#16*154,spr_type(a1)
		
		rts

.night		lea	block_pic2,a1
		move.l	a1,which_background
		
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		
		move.w	#16*151,spr_type(a0)
		move.w	#16*155,spr_type(a1)
		
		rts

; -------------------------------------------------------------------------------

anti_grav	tst.w	gravQuest
		beq.s	.anti

.grav		move.w	#88*16,spr_type(a0)

		rts

.anti		move.w	#89*16,spr_type(a0)

		rts

; -------------------------------------------------------------------------------
players2	cmp.w	#3,players
		beq.s	.kill

.revive		clr.w	spr_status(a0)
		
		rts
		
.kill		move.w	#1,spr_status(a0)
		
		rts
		
; -------------------------------------------------------------------------------
players3	cmp.w	#3,players
		bne.s	.kill

.revive		clr.w	spr_status(a0)
		
		rts
		
.kill		move.w	#1,spr_status(a0)
		
		rts

; -------------------------------------------------------------------------------
cursor_width	equ	1
cursor_depth	equ	2

cursor		tst.w	spr_word1(a0)
		beq.s	.no_left

		btst.b	#joy_left,joyval2
		beq.s	.no_left
		
		sub.w	#1,spr_word1(a0)

.no_left	cmp.w	#cursor_width,spr_word1(a0)
		beq.s	.no_right
		
		btst.b	#joy_right,joyval2
		beq.s	.no_right
		
		add.w	#1,spr_word1(a0)

.no_right	
		btst.b	#joy_down,joyval2
		beq.s	.no_down
	
		tst.w	d_move
		bne.s	.no_down2

		move.w	#1,d_move

		cmp.w	#cursor_depth,spr_word2(a0)
		beq.s	.no_down2
		
		add.w	#1,spr_word2(a0)
	
		bra.s	.no_down2
.no_down	
		clr.w	d_move

.no_down2	btst.b	#joy_up,joyval2
		beq.s	.no_up

		tst.w	u_move
		bne.s	.no_up2
		
		tst.w	spr_word2(a0)
		beq.s	.no_up
		
		move.w	#1,u_move
		sub.w	#1,spr_word2(a0)

		bra.s	.no_up2

.no_up		clr.w	u_move	

.no_up2
		
.move_cur	move.l	a0,a1
		lea	sprlist_len(a1),a1

		lea	cursor_table,a2
		clr.l	d0
		
		move.w	spr_word1(a0),d0
		mulu.w	#csrlist_len,d0
		
		add.w	d0,a2
		
		move.w	spr_word2(a0),d0
		mulu.w	#2*csrlist_len,d0
		
		add.w	d0,a2
	
		move.w	csr_xpos(a2),spr_xpos(a0)
		move.w	csr_ypos(a2),spr_ypos(a0)
		move.w	csr_type1(a2),spr_type(a0)
		move.w	csr_type2(a2),spr_type(a1)
		
.func		btst.b	#joy_fire,joyval2
		beq.s	.no_fire
		
		tst.w	fireQuest
		bne	terminate
		
		move.w	#1,fireQuest
		
		move.l	csr_operand(a2),a1
		add.w	#1,(a1)
		
		move.w	csr_max(a2),d0
		move.w	(a1),d1
		
		cmp.w	d0,d1
		ble	terminate
		
		move.w	csr_min(a2),(a1)

		rts
		
.no_fire	clr.w	fireQuest

		rts
		
fireQuest		dc.w	0
d_move		dc.w	0
u_move		dc.w	0

cursor_table	dc.w	135
		dc.w	90
		dc.w	152*16
		dc.w	156*16
		dc.l	timerQuest
		dc.w	0
		dc.w	1
		
		dc.w	173
		dc.w	88
		dc.w	152*16
		dc.w	156*16
		dc.l	time
		dc.w	1
		dc.w	20
		
		dc.w	120
		dc.w	120
		dc.w	152*16
		dc.w	156*16
		dc.l	players
		dc.w	2
		dc.w	3
		
		dc.w	181
		dc.w	158
		dc.w	149*16
		dc.w	153*16
		dc.l	dayQuest
		dc.w	0
		dc.w	1
		
		dc.w	126
		dc.w	160
		dc.w	152*16
		dc.w	156*16
		dc.l	gravQuest
		dc.w	0
		dc.w	1
		
		dc.w	181
		dc.w	158
		dc.w	149*16
		dc.w	153*16
		dc.l	dayQuest
		dc.w	0
		dc.w	1
		
		
		dc.w	-1
		
; -------------------------------------------------------------------------------

blank		dc.w	0
gravQuest		dc.w	1
dayQuest		dc.w	0
timerQuest		dc.w	1
time		dc.w	1	; Max 20
players		dc.w	3 ; 3 for 3 players anything else is 2 player
; -------------------------------------------------------------------------------
