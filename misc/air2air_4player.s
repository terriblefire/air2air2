; ----------------------------< HARDWARE EQUATES >-----------------------------
		include	include/hardware.i
		include	include/what_key.i ; include file of all the key equates. AND NO I DID`NT TYPE THEM IN. I STOLE IT.

usea		equ	2048
useb		equ	1024
usec		equ	512
used		equ	256

border		equ	colour00	; What do you think?
no_colours	equ	32
joy_up		equ	0
joy_down	equ	1
joy_left	equ	2
joy_right	equ	3
joy_fire	equ	4
halt_flag	equ	0
scroll_flag	equ	1
cr		equ	13
mouse_left	equ	7
mouse_right	equ	6

plane_len	equ	44
line_len	equ	4*plane_len ;160
no_lines	equ	232
act_lines	equ	no_lines+32
sprite_lines	equ	16*13		; 12 Blocks down...


; Sprite structure equates...

		rsreset
spr_type	rs.w	1 ; Guess.
spr_xvel	rs.l	1 ; Guess.
spr_xpos	rs.l	1 ; Guess.
spr_yvel	rs.l	1 ; Guess.
spr_ypos	rs.l	1 ; Guess.
spr_task	rs.l	1 ; Guess.
spr_xstart	rs.w	1 ; Guess.
spr_ystart	rs.w	1 ; Guess.
spr_vel		rs.w	1 ; Number of right shifts of the speed. 0 = full speed. 8 = full stop.
spr_clock	rs.w	1 ; Clockwise delay counter. only used for the first sprite in a AEROPLANE.
spr_anti	rs.w	1 ; Anti-clockwise delay counter. only used for the first sprite in a AEROPLANE.
spr_throttle	rs.w	1 ; Throttle delay
spr_status	rs.w	1 ; If not zero then the sprite is NOT displayed (except with HWsprites).
spr_long1	rs.l	1 ; Used to tell whether a sprite is exploded or not 1 = exploded 0 = fine
spr_long2	rs.l	1 ; Various uses.
spr_word1	rs.w	1 ; Used as a counter.
spr_word2	rs.w	1 ; Various uses.
spr_lastx1	rs.w	1 ; The xpos 1 frame ago.
spr_lastx2	rs.w	1 ; The xpos 2 frames ago.
spr_lastt2	rs.w	1 ; The Type of sprite 2 frames ago.
spr_lasty1	rs.w	1 ; The ypos a frame ago.
spr_lasty2	rs.w	1 ; The ypos 2 frames ago.
spr_lastt1	rs.w	1 ; The Type of sprite a frame ago
spr_lasty3	rs.w	1 ; The ypos 3 frames ago.
spr_lastx3	rs.w	1 ; The xpos 3 frames ago.
spr_lastt3	rs.w	1 ; The Type of sprite 3 frames ago
spr_stall	rs.w	1 ; Zero if engines on. 1 if plane is stalling.
sprlist_len	rs.w	0 ; Guess.

; Sprite info equates...

		rsreset
spr_address	rs.l	1 ; You did these, so you tell me!
spr_xdis	rs.w	1
spr_ydis	rs.w	1
spr_width	rs.w	1
spr_height	rs.w	1
spr_space	rs.w	1
sprinfo_len	rs.w	0

blank_line	ds.l	160	;two spare blank_lines for the copper to repeat

; ------------------------------< SET SCREEN >------------------------------

; > D0.L = Address of screen to make.

set_screen	macro
		move.l	d0,d1
		move.w	d0,2(a1)	; Updates the copper pointers...
		swap	d0
		move.w	d0,6(a1)
		swap	d0
		add.l	#plane_len,d0

		move.w	d0,10(a1)
		swap	d0
		move.w	d0,14(a1)
		swap	d0
		add.l	#plane_len,d0

		move.w	d0,18(a1)
		swap	d0
		move.w	d0,22(a1)
		swap	d0
		add.l	#plane_len,d0

		move.w	d0,26(a1)
		swap	d0
		move.w	d0,30(a1)
		move.l	d1,d0
		endm
; ------------------------------------------------------------------------

diskoff		macro ; Stops the disk motor
		move.b	#255,$bfd100 ; disk motor off bit7* AL.
		move.b	#255-8,$bfd100 ; select int disk bit 3 AL.
		endm
		
change_copper	macro
		move.w	#%0000000010000000,dmacon
		move.l	copper,cop1lch	; My copperlist.
		move.w	#%1000000010000000,dmacon
		move.w	copjmp1,d0			
		endm

new_cop_colours macro
		move.w	(a0)+,day_ln1
		move.w	(a0)+,day_ln2
		move.w	(a0)+,day_ln3
		move.w	(a0)+,day_ln4
		move.w	(a0)+,day_ln5
		move.w	(a0)+,day_ln6
		move.w	(a0)+,day_ln7
		move.w	(a0)+,night_ln1
		move.w	(a0)+,night_ln2
		move.w	(a0)+,night_ln3
		move.w	(a0)+,night_ln4
		move.w	(a0)+,night_ln5
		move.w	(a0)+,night_ln6
		move.w	(a0)+,night_ln7
		endm

; ---------------------------------< SHELL >------------------------------


go		diskoff

		move.w	intenar,asm_intena
		move.w	dmaconr,asm_dmacon
		or.w	#32768,asm_intena
		or.w	#32768,asm_dmacon
		move.w	#%0111111111111111,intena  	; Disable interupts.
		move.w	#%0111110111101111,dmacon  	; Disable DMA. ; leave disk going to let motor stop

		movem.l	d0-d7/a0-a6,-(sp)
		move.l	sp,asm_stack

		lea	stack+4*64,sp

		move.l	#bus_error,08
		move.l	#address_err,12
		move.l	#illegal_inst,16
		move.l	#divide_zero,20

		move.l	$68,asm_keyboard
		move.l	$6c,asm_vblank
		move.l	#keyboard,$68
		move.l	#vblank,$6c

		clr.b	keycode

		lea	song1,a0
		bsr	mt_init

		lea	h_sprites,a0
		lea	intro_cop_sprites,a1
		lea	day_cop_sprites,a2
		moveq.l	#16-1,d7
.loop		addq.w	#2,a1
		addq.w	#2,a2
		move.w	(a0),(a1)+
		move.w	(a0)+,(a2)+
		dbra	d7,.loop
		
		move.l	#intro_copperlist,copper
		move.l	copper,cop1lch	; My copperlist.

		move.w	#%1000011111100000,dmacon	; DMA
		move.w	#%1100000000011000,intena	; Enable interupts
		
		move.w	copjmp1,d0
		
		lea	lpt01,a0				; Make an LPT.
		move.l	#scr01-2,d0
		move.w	#act_lines*3-1,d7
.lpt		move.l	d0,(a0)+
		add.l	#line_len,d0
		dbra	d7,.lpt

		bsr	swap_screen	; Bodge to initialize the screen...

		tst.w	.map
		bne.s	.ok

		lea	map,a0

		move.w	#39*22,d0
		
.loop2		move.w	(a0),d2
		divs	#32,d2
		lsl.w	#6,d2
		move.w	d2,(a0)+
		
		dbra	d0,.loop2
		move.w	#1,.map	
.ok
		lea	cop_screen2,a1
		move.l	#blank_line,d0
		set_screen
		
		jmp	the_code
		
.map		dc.w	0

; -------------------------------< BLITWAIT >-------------------------------

blitwait	macro
		btst.b	#14,dmaconr
.\.blitwait	btst.b	#14,dmaconr
 		bne.s	.\.blitwait
		endm

blit_block	macro
		addq.l	#2,d2
		move.l	d0,a4
		clr.l	d5
		move.w	(a0)+,d5 ; Kev U made one spectacular FUCK UP HERE MATE!
		lsl.l	#1,d5
		
		add.l	d5,a4
		move.l	a4,(a1)
		move.l	d2,(a2)
		move.w	d3,(a3)
		endm

kill_sprites	macro
		move.w	#%0000000000100000,dmacon	; DMA
		clr.w	spr0data
		clr.w	spr0datb
		clr.w	spr1data
		clr.w	spr1datb
		clr.w	spr2data
		clr.w	spr2datb
		clr.w	spr3data
		clr.w	spr3datb
		clr.w	spr4data
		clr.w	spr4datb
		clr.w	spr5data
		clr.w	spr5datb
		clr.w	spr6data
		clr.w	spr6datb
		clr.w	spr7data
		clr.w	spr7datb
		endm

reincarnate_sprites	macro
		move.w	#%1000000000100000,dmacon	; DMA
		clr.w	spr0data
		clr.w	spr0datb
		clr.w	spr1data
		clr.w	spr1datb
		clr.w	spr2data
		clr.w	spr2datb
		clr.w	spr3data
		clr.w	spr3datb
		clr.w	spr4data
		clr.w	spr4datb
		clr.w	spr5data
		clr.w	spr5datb
		clr.w	spr6data
		clr.w	spr6datb
		clr.w	spr7data
		clr.w	spr7datb
		endm

		
; ------------------------------< COPPERLIST >------------------------------

intro_copperlist	
		dc.w	bplcon0-custom,%0100000000000000	; It's safe here.
		dc.w	bplcon1-custom,$FFFF
		dc.w	bplcon2-custom,%0000000000100000
		dc.w	ddfstrt-custom,$28	; No scroll thank you.
		dc.w	ddfstop-custom,$d0
		dc.w	diwstrt-custom,56*256+128
		dc.w	diwstop-custom,255*256+193
		dc.w	bpl1mod-custom,line_len-plane_len
		dc.w	bpl2mod-custom,line_len-plane_len
intro_cop_screen
		dc.w	bpl1ptl-custom,0		; Screen.
		dc.w	bpl1pth-custom,0
		dc.w	bpl2ptl-custom,0
		dc.w	bpl2pth-custom,0
		dc.w	bpl3ptl-custom,0
		dc.w	bpl3pth-custom,0
		dc.w	bpl4ptl-custom,0
		dc.w	bpl4pth-custom,0
intro_cop_colours	
		dc.w	colour00-custom,0	; Perminat colours.
		dc.w	colour01-custom,0
		dc.w	colour02-custom,0
		dc.w	colour03-custom,0
		dc.w	colour04-custom,0
		dc.w	colour05-custom,0
		dc.w	colour06-custom,0
		dc.w	colour07-custom,0
		dc.w	colour08-custom,0
		dc.w	colour09-custom,0
		dc.w	colour10-custom,0
		dc.w	colour11-custom,0
		dc.w	colour12-custom,0
		dc.w	colour13-custom,0
		dc.w	colour14-custom,0
		dc.w	colour15-custom,0
		dc.w	colour16-custom,0	; Perminat colours.
		dc.w	colour17-custom,0
		dc.w	colour18-custom,0
		dc.w	colour19-custom,0
		dc.w	colour20-custom,0
		dc.w	colour21-custom,0
		dc.w	colour22-custom,0
		dc.w	colour23-custom,0
		dc.w	colour24-custom,0
		dc.w	colour25-custom,0
		dc.w	colour26-custom,0
		dc.w	colour27-custom,0
		dc.w	colour28-custom,0
		dc.w	colour29-custom,0
		dc.w	colour30-custom,0
		dc.w	colour31-custom,0
intro_cop_sprites	
		dc.w	spr0pth-custom,0
		dc.w	spr0ptl-custom,0
		dc.w	spr1pth-custom,0
		dc.w	spr1ptl-custom,0
		dc.w	spr2pth-custom,0
		dc.w	spr2ptl-custom,0
		dc.w	spr3pth-custom,0
		dc.w	spr3ptl-custom,0
		dc.w	spr4pth-custom,0
		dc.w	spr4ptl-custom,0
		dc.w	spr5pth-custom,0
		dc.w	spr5ptl-custom,0
		dc.w	spr6pth-custom,0
		dc.w	spr6ptl-custom,0
		dc.w	spr7pth-custom,0
		dc.w	spr7ptl-custom,0
		dc.w	intreq-custom,%1000000000010000
pal1		dc.b	255,01,255,00
		dc.w	bpl1mod-custom,-2*line_len-plane_len
		dc.w	bpl2mod-custom,-2*line_len-plane_len
ripple		dc.b	03,01,255,00
		dc.w	bpl1mod-custom,-1*line_len-plane_len
		dc.w	bpl2mod-custom,-1*line_len-plane_len		
pal2		dc.b	06,01,255,00
		dc.w	bpl1mod-custom,-2*line_len-plane_len
		dc.w	bpl2mod-custom,-2*line_len-plane_len

ripple_end 	dc.b	09,01,255,00
cop_screen2	dc.w	bpl1ptl-custom,0		; Screen.
		dc.w	bpl1pth-custom,0
		dc.w	bpl2ptl-custom,0
		dc.w	bpl2pth-custom,0
		dc.w	bpl3ptl-custom,0
		dc.w	bpl3pth-custom,0
		dc.w	bpl4ptl-custom,0
		dc.w	bpl4pth-custom,0
		dc.w	bpl1mod-custom,-1*plane_len
		dc.w	bpl2mod-custom,-1*plane_len
		dc.w	$ffff,$fffe
		dc.w	$ffff,$fffe
day_copperlist	
		dc.w	bplcon0-custom,%0100000000000000	; It's safe here.
		dc.w	bplcon1-custom,$FFFF
		dc.w	bplcon2-custom,%0000000000100000
		dc.w	ddfstrt-custom,$28	; No scroll thank you.
		dc.w	ddfstop-custom,$d0
		dc.w	diwstrt-custom,56*256+128
		dc.w	diwstop-custom,254*256+193
		dc.w	bpl1mod-custom,line_len-plane_len
		dc.w	bpl2mod-custom,line_len-plane_len
day_cop_screen
		dc.w	bpl1ptl-custom,0		; Screen.
		dc.w	bpl1pth-custom,0
		dc.w	bpl2ptl-custom,0
		dc.w	bpl2pth-custom,0
		dc.w	bpl3ptl-custom,0
		dc.w	bpl3pth-custom,0
		dc.w	bpl4ptl-custom,0
		dc.w	bpl4pth-custom,0
day_cop_colours	
		dc.w	colour01-custom,0	; Perminat colours.
		dc.w	colour01-custom,0
		dc.w	colour02-custom,0
		dc.w	colour03-custom,0
		dc.w	colour04-custom,0
		dc.w	colour05-custom,0
		dc.w	colour06-custom,0
		dc.w	colour07-custom,0
		dc.w	colour08-custom,0
		dc.w	colour09-custom,0
		dc.w	colour10-custom,0
		dc.w	colour11-custom,0
		dc.w	colour12-custom,0
		dc.w	colour13-custom,0
		dc.w	colour14-custom,0
		dc.w	colour15-custom,0
		dc.w	colour16-custom,0	; Perminat colours.
		dc.w	colour17-custom,0
		dc.w	colour18-custom,0
		dc.w	colour19-custom,0
		dc.w	colour20-custom,0
		dc.w	colour21-custom,0
		dc.w	colour22-custom,0
		dc.w	colour23-custom,0
		dc.w	colour24-custom,0
		dc.w	colour25-custom,0
		dc.w	colour26-custom,0
		dc.w	colour27-custom,0
		dc.w	colour28-custom,0
		dc.w	colour29-custom,0
		dc.w	colour30-custom,0
		dc.w	colour31-custom,0
day_cop_sprites	
		dc.w	spr0pth-custom,0
		dc.w	spr0ptl-custom,0
		dc.w	spr1pth-custom,0
		dc.w	spr1ptl-custom,0
		dc.w	spr2pth-custom,0
		dc.w	spr2ptl-custom,0
		dc.w	spr3pth-custom,0
		dc.w	spr3ptl-custom,0
		dc.w	spr4pth-custom,0
		dc.w	spr4ptl-custom,0
		dc.w	spr5pth-custom,0
		dc.w	spr5ptl-custom,0
		dc.w	spr6pth-custom,0
		dc.w	spr6ptl-custom,0
		dc.w	spr7pth-custom,0
		dc.w	spr7ptl-custom,0

		dc.w	$0201,$ff00,colour13-custom
day_ln1		dc.w	$8ae
		dc.w	$6e01,$ff00,colour13-custom
day_ln2		dc.w	$79d
		dc.w	$9c01,$ff00,colour13-custom
day_ln3		dc.w	$68d
		dc.w	$c001,$ff00,colour13-custom
day_ln4		dc.w	$57b
		dc.w	$d801,$ff00,colour13-custom
day_ln5		dc.w	$46a
		dc.w	$ea01,$ff00,colour13-custom
day_ln6		dc.w	$359
		dc.w	$f201,$ff00,colour13-custom
day_ln7		dc.w	$248
		dc.w	$f301,$ff00
		dc.w	intreq-custom,%1000000000010000
		dc.w	$ffff,$fffe
		dc.w	$ffff,$fffe
night_copperlist	
		dc.w	bplcon0-custom,%0100000000000000	; It's safe here.
		dc.w	bplcon1-custom,$FFFF
		dc.w	bplcon2-custom,%0000000000100000
		dc.w	ddfstrt-custom,$28	; No scroll thank you.
		dc.w	ddfstop-custom,$d0
		dc.w	diwstrt-custom,56*256+128
		dc.w	diwstop-custom,255*256+193
		dc.w	bpl1mod-custom,line_len-plane_len
		dc.w	bpl2mod-custom,line_len-plane_len
night_cop_screen
		dc.w	bpl1ptl-custom,0		; Screen.
		dc.w	bpl1pth-custom,0
		dc.w	bpl2ptl-custom,0
		dc.w	bpl2pth-custom,0
		dc.w	bpl3ptl-custom,0
		dc.w	bpl3pth-custom,0
		dc.w	bpl4ptl-custom,0
		dc.w	bpl4pth-custom,0
night_cop_colours	
		dc.w	colour01-custom,0	; Perminat colours.
		dc.w	colour01-custom,0
		dc.w	colour02-custom,0
		dc.w	colour03-custom,0
		dc.w	colour04-custom,0
		dc.w	colour05-custom,0
		dc.w	colour06-custom,0
		dc.w	colour07-custom,0
		dc.w	colour08-custom,0
		dc.w	colour09-custom,0
		dc.w	colour10-custom,0
		dc.w	colour11-custom,0
		dc.w	colour12-custom,0
		dc.w	colour13-custom,0
		dc.w	colour14-custom,0
		dc.w	colour15-custom,0
		dc.w	colour16-custom,0	; Perminat colours.
		dc.w	colour17-custom,0
		dc.w	colour18-custom,0
		dc.w	colour19-custom,0
		dc.w	colour20-custom,0
		dc.w	colour21-custom,0
		dc.w	colour22-custom,0
		dc.w	colour23-custom,0
		dc.w	colour24-custom,0
		dc.w	colour25-custom,0
		dc.w	colour26-custom,0
		dc.w	colour27-custom,0
		dc.w	colour28-custom,0
		dc.w	colour29-custom,0
		dc.w	colour30-custom,0
		dc.w	colour31-custom,0
night_cop_sprites	
		dc.w	spr0pth-custom,0
		dc.w	spr0ptl-custom,0
		dc.w	spr1pth-custom,0
		dc.w	spr1ptl-custom,0
		dc.w	spr2pth-custom,0
		dc.w	spr2ptl-custom,0
		dc.w	spr3pth-custom,0
		dc.w	spr3ptl-custom,0
		dc.w	spr4pth-custom,0
		dc.w	spr4ptl-custom,0
		dc.w	spr5pth-custom,0
		dc.w	spr5ptl-custom,0
		dc.w	spr6pth-custom,0
		dc.w	spr6ptl-custom,0
		dc.w	spr7pth-custom,0
		dc.w	spr7ptl-custom,0

		dc.w	$0201,$ff00,colour13-custom
night_ln1	dc.w	$000
		dc.w	$6e01,$ff00,colour13-custom
night_ln2	dc.w	$002
		dc.w	$9c01,$ff00,colour13-custom
night_ln3	dc.w	$003
		dc.w	$c001,$ff00,colour13-custom
night_ln4	dc.w	$004
		dc.w	$d801,$ff00,colour13-custom
night_ln5	dc.w	$006
		dc.w	$ea01,$ff00,colour13-custom
night_ln6	dc.w	$016
		dc.w	$f201,$ff00,colour13-custom
night_ln7	dc.w	$026
		dc.w	$f301,$ff00
		dc.w	intreq-custom,%1000000000010000
		dc.w	$ffff,$fffe
		dc.w	$ffff,$fffe
		
; ------------------------< ROLL ON EFFECT HANDLER >---------------------------

ripplecon	move.w	ripst,d0
		move.w	d0,d1
		sub.w	#6,d1
		cmp.w	#256,d0
		blt.s	.singleinst
		move.w	#255,d1
		sub.w	#255,d0

.singleinst	mulu.w	#256,d0
		add.w	#1,d0
		mulu.w	#256,d1
		add.w	#1,d1
		move.w	d0,ripple
		move.w	d1,pal1
		move.w	ripst,d0
		add.w	ripsize,d0
		move.w	d0,d1
		
		sub.w	#6,d1
		cmp.w	#256,d0
		blt.s	.singleinst2		
		move.w	#255,d1
		sub.w	#255,d0
		move.w	ripsize,d3
		add.w	#256,d3
		cmp.w	d3,d0
		blt.s	.singleinst2
		move.w	d0,d1
		sub.w	#1,d1

.singleinst2	mulu.w	#256,d0
		add.w	#1,d0
		mulu.w	#256,d1
		add.w	#1,d1
		move.w	d0,ripple_end
		move.w	d1,pal2
		add.l	#32768,ripst
		cmp.w	#312,ripst
		bne.s	.quit
		move.w	#16,ripst
.quit		rts

ripst		dc.l	16*65536
ripsize		dc.w	20

; -----------------------------< SWAP SCREEN >------------------------------

swap_screen	move.l	bak_lpt,d0		; Swaps LPT
		move.l	the_lpt,bak_lpt
		move.l  d0,the_lpt

		move.l	bak_screen,d0		; Swaps the...
		move.l	the_screen,bak_screen
		move.l  d0,the_screen		; Drop through with D0.L.
		move.l	copper,a1
		add.l	#36,a1
		set_screen
		
		rts
		
; -----------------------------< SET COLOURS >------------------------------
; > A0.L = Source of colours.

set_colours	move.l	copper,a1		; Where to put colours...
		add.l	#68,a1
		moveq.l	#16-1,d0		; Only 16.
		
.loop		addq.w	#2,a1
		move.w	(a0)+,(a1)+
		dbra	d0,.loop

		rts

; ---------------------------------< CLS >---------------------------------

cls		move.l	the_screen,a0
		move.w	#plane_len*no_lines-1,d0
.loop		clr.l	(a0)+
		dbra	d0,.loop
		rts

cls_both	move.l	#scr01DB,a0
		move.w	#plane_len*no_lines*3-1,d0
.loop		clr.l	(a0)+
		dbra	d0,.loop
		rts

; -------------------------------< SHOW PIC >-------------------------------

; A0.L > Address of picture file.
; A1.L > Destination of colours.
; A2.L > Destination for picture.

show_pic	moveq.l	#16-1,d2		; Number of colours.
		tst.w	(a0)+
		beq.s	.pi1			; What format?
		bmi.s	.pc1
		cmp.w	#'RM',(a0)+
		beq.s	.iff
		jsr	pic_failed

.pi1		move.w	(a0)+,d0		; PI1...
		add.w	d0,d0
		move.w	d0,(a1)+
		dbra	d2,.pi1
		
		move.w	#200-1,d0
.down		moveq.l	#20-1,d1
.across		move.w	(a0)+,(a2)+
		move.w	(a0)+,plane_len-2(a2)
		move.w	(a0)+,2*plane_len-2(a2)
		move.w	(a0)+,3*plane_len-2(a2)
		dbra	d1,.across
		add.w	#plane_len-line_len,a2
		dbra	d0,.down
		rts
		
.pc1		move.w	(a0)+,d0		; PC1...
		add.w	d0,d0
		move.w	d0,(a1)+
		dbra	d2,.pc1
		bra.s	.decompact
.iff		cmp.l	#'CMAP',(a0)+		; IFF...
		bne.s	.iff
		addq.l	#4,a0
.rgb		move.b	(a0)+,d0
		lsl.w	#4,d0
		move.b	(a0)+,d0
		and.w	#$ff0,d0
		move.b	(a0)+,d1
		lsr.b	#4,d1
		or.b	d1,d0
		move.w	d0,(a1)+
		dbra	d2,.rgb
		
.body		cmp.w	#'DY',(a0)+		; Find "BODY".
		bne.s	.body
		addq.l	#4,a0
.decompact	move.l	a2,a1
		add.l	#32000,a1
.loop		move.b	(a0)+,d0		; Counter...
		bmi.s	.copy
.1		move.b	(a0)+,(a2)+
		subq.b	#1,d0
		bpl.s	.1
		cmp.l	a1,a2		; Branch if less than screen end.
		blt.s	.loop
		rts
.copy		neg.b	d0		; Convert counter...
		move.b	(a0)+,d1
.2		move.b	d1,(a2)+
		subq.b	#1,d0
		bpl.s	.2
		cmp.l	a1,a2		; Branch if less than screen end.
		blt.s	.loop
		rts
		
filter		dc.w	1	;filter on
master_vol	dc.w	32
; --------------------------------< VBLANK >--------------------------------

vblank		movem.l	a0-a7/d0-d7,-(sp)
		addq.b	#1,flag
	
		bsr	mt_music
		bsr	jukebox
; -------------------------< FILTER CONTROL >-------------------------------------

		cmp.b	#KB_EQUAL,keycode
		bne.s	.no_on
		clr.w	filter	;	
.no_on
		cmp.b	#KB_MINUS,keycode
		bne.s	.filter
		move.w	#1,filter
.filter		tst.w	filter	
		bne.s	.filter_off
		or.b	#%00000010,$bfe001 ; CIA LED BRIGHT/FILTER ON.
		bra.s	.finished
.filter_off	and.b	#%11111101,$bfe001
.finished	
		lea	colours,a0
		move.l	copper,a1
		add.l	#68,a1
		moveq.l	#32-1,d0
.loop		addq.w	#2,a1
		move.w	(a0)+,(a1)+
		dbra	d0,.loop
		bsr	joysticks		; Create port1 and port2.
		bsr	mice
.continue	movem.l	(sp)+,a0-a7/d0-d7
		move.w	#%0000000000010000,intreq	; Clear pending bit.
		move.w	#%1000000000010000,intena	; Clear pending bit.
		rte

; ---------------------------------< HALT >---------------------------------

halt		clr.b	flag			; It's a spectrum...
.halt		tst.b	flag			; Sad.
		beq.s	.halt
		rts

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
		bsr	fourplayer_adaptor
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
.no_right	rts

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
keyboard_joyval
		moveq.l	#0,d0	
		tst.b	key_a
		beq.s	.no_up
		add.w	#1,d0
.no_up		tst.b	key_q
		beq.s	.no_down
		add.w	#2,d0
.no_down	tst.b	key_p
		beq.s	.no_left
		add.w	#4,d0
.no_left	tst.b	key_o
		beq.s	.no_right
		add.w	#8,d0
.no_right	tst.b	key_space
		beq.s	.no_fire
		add.w	#16,d0
.no_fire	move.b	d0,joyval5	
		rts

; ---------------------------------< MICE >---------------------------------

mice		move.b	mouseval1+1,d1		; Scan mouse port 1.
		move.b	mouseval1+0,d2
		move.w	joy0dat,mouseval1
		move.b	mouseval1+1,d0
		sub.b	d1,d0
		ext.w	d0
		move.w	d0,xmouse1
		move.b	mouseval1+0,d0
		sub.b	d2,d0
		ext.w	d0
		move.w	d0,ymouse1

		move.b	mouseval2+1,d1		; Scan mouse port 2.
		move.b	mouseval2+0,d2
		move.w	joy1dat,mouseval2
		move.b	mouseval2+1,d0
		sub.b	d1,d0
		ext.w	d0
		move.w	d0,xmouse2
		move.b	mouseval2+0,d0
		sub.b	d2,d0
		ext.w	d0
		move.w	d0,ymouse2

		btst.b	#6,$bfe001		; Left buttons...
		bne.s	.1
		bset.b	#mouse_left,joyval1
.1		btst.b	#7,$bfe001
		bne.s	.2
		bset.b	#mouse_left,joyval2

.2		move.w	#%1100110000000001,$dff034
		moveq.l	#100,d0
.charge		dbra	d0,.charge

		move.w	$dff016,d0		; Right buttons...
		btst.l	#10,d0
		bne.s	.3
		bset.b	#mouse_right,joyval1
.3		btst.l	#14,d0
		bne.s	.4
		bset.b	#mouse_right,joyval2
.4		rts
; -------------------------------< KEYBOARD >-------------------------------


keyboard	move.w	#8,$dff09a		; Disable interupts
		movem.l	d0-d2/a0,-(sp)
		move.b	$bfed01,d0		; Is it a keyboard interupt
		btst	#3,d0
		beq	keyintexit
		clr.l	d0
		move.b	$bfec01,d0		; Read keycode
		or.b	#$40,$bfee01
		not.b	d0
		
		move.b	#0,d1			; D1=key up/down code
		lea	unpressedctr,a0
		lsr.w	#1,d0
		bcs	keypressed
		move.b	#$ff,d1
		cmp.b	#$59,d0
		lea	pressedctr,a0
		beq	keypressed		; Ignore f10
keypressed	addq.b	#1,(a0)			; Update key count
		and.w	#$7f,d0

		move.b	d0,lastkeycode
		cmp.b	#$68,d0
		bge.s	keyclear
		lea	keytable,a0		; Set keytable entry
		move.b	d1,0(a0,d0.w)

keyclear	move.w	#100,d1			; Delay for ikbd controller
keydelay	dbra	d1,keydelay
		and.b	#$bf,$bfee01
keyintexit	movem.l	(sp)+,d0-d2/a0
		move.w	#8,$dff09c		;clear interupt pending bit
		move.w	#$8008,$dff09a		;reenable interupts
		rte

keytable	ds.b	128	; Key pressed table - zero if not pressed.
lastkeycode	dc.b	0
		even

pressedctr	dc.w	0
unpressedctr	dc.w	0
last_pressed	dc.w	0

; --------------------------------------------------------------------------
; Used for reading keyboard...

key_0		equ	keytable+$0a
key_1		equ	keytable+$01
key_2		equ	keytable+$02
key_3		equ	keytable+$03
key_4		equ	keytable+$04
key_5		equ	keytable+$05
key_6		equ	keytable+$06
key_7		equ	keytable+$07
key_8		equ	keytable+$08
key_9		equ	keytable+$09
key_a		equ	keytable+$20
key_b		equ	keytable+$35
key_c		equ	keytable+$33
key_d		equ	keytable+$22
key_e		equ	keytable+$12
key_f		equ	keytable+$23
key_g		equ	keytable+$24
key_h		equ	keytable+$25
key_i		equ	keytable+$17
key_j		equ	keytable+$26
key_k		equ	keytable+$27
key_l		equ	keytable+$28
key_m		equ	keytable+$37
key_n		equ	keytable+$36
key_o		equ	keytable+$18
key_p		equ	keytable+$19
key_q		equ	keytable+$10
key_r		equ	keytable+$13
key_s		equ	keytable+$21
key_t		equ	keytable+$14
key_u		equ	keytable+$16
key_v		equ	keytable+$34
key_w		equ	keytable+$11
key_x		equ	keytable+$32
key_y		equ	keytable+$15
key_z		equ	keytable+$31
key_f01		equ	keytable+$50
key_f02		equ	keytable+$51
key_f03		equ	keytable+$52
key_f04		equ	keytable+$53
key_f05		equ	keytable+$54
key_f06		equ	keytable+$55
key_f07		equ	keytable+$56
key_f08		equ	keytable+$57
key_f09		equ	keytable+$58
key_f10		equ	keytable+$59
key_space	equ	keytable+$40
key_tab		equ	keytable+$42
key_enter	equ	keytable+$44
key_esc		equ	keytable+$45
key_del		equ	keytable+$46
key_shift	equ	keytable+$60
key_ctrl	equ	keytable+$63
key_up		equ	keytable+$4c
key_down	equ	keytable+$4d
key_left	equ	keytable+$4f
key_right	equ	keytable+$4e

; ------------------------------< BLIT TEXT >-------------------------------
; > A0.L = Address of text string to be blitted.
; < A0.L = Currupt.
; < A1.L = Currupt.
; < D0.L = Currupt.
; < D1.L = Currupt.
; < D2.L = Currupt.
; < D3.L = Currupt.

blit_text	move.w	(a0)+,d1	; Read initial X and Y.
		move.w	(a0)+,d2

blit_string	move.w	d1,d3		; Use entry D1 - D2...

text_ride	move.b	(a0)+,d0	; The main text loop.
		beq.s	.done
		cmp.b	#cr,d0
		beq.s	.cr
		cmp.b	#32,d0
		beq.s	.space
.no_space	bsr	blit_ascii
.space		addq.w	#8,d1
		bra.s	text_ride
.cr		move.w	d3,d1
		addq.w	#8,d2
		bra.s	text_ride
.done		rts

; ------------------------------< BLIT ASCII >------------------------------

; > D0.B = ASCII code.
; > D1.W = X positon of character.
; > D2.W = Y positon of character.

blit_ascii	sub.b	#48,d0

blit_didgit	lea	font,a1

		blitwait

		and.w	#%0000000011111111,d0
		asl.w	#6,d0
		add.w	d0,a1
		move.l	a1,bltbpth	; Address of character.

		move.l	the_lpt,a1	; Calculate screen address.
		move.w	d2,d0
		add.w	d0,d0
		add.w	d0,d0
		move.l	(a1,d0),a1
	
		moveq.l	#0,d0
		move.w	d1,d0
		and.w	#%1111111111110000,d0
		asr.w	#3,d0
		add.l	d0,a1
		move.l	a1,bltcpth
		move.l	a1,bltdpth
	
		move.w	d1,d0
		and.w	#15,d0
		ror.w	#4,d0

		move.w	d0,bltcon1
		or.w	#useb+usec+used+$ca,d0
		move.w	d0,bltcon0

		move.w	#%1111111100000000,bltafwm
		move.w	#%0000000000000000,bltalwm

		move.w	#%1111111100000000,bltadat

		move.w	#plane_len-4,bltcmod
		move.w	#plane_len-4,bltdmod

		move.w	#-2,bltbmod

		move.w	#4*8<<6+2,bltsize		; Go...
		rts

; -------------------------------< DECIMAL >--------------------------------

; > A0.L = Address for numbrer string.
; > D0.L = Number to convert.

decimal_10	moveq.l	#-1,d1
.10		addq.w	#1,d1
		sub.l	#1000000000,d0
		bcc.s	.10
		add.l	#1000000000,d0
		move.b	d1,(a0)+

decimal_9	moveq.l	#-1,d1
.9		addq.w	#1,d1
		sub.l	#100000000,d0
		bcc.s	.9
		add.l	#100000000,d0
		move.b	d1,(a0)+

decimal_8	moveq.l	#-1,d1
.8		addq.w	#1,d1
		sub.l	#10000000,d0
		bcc.s	.8
		add.l	#10000000,d0
		move.b	d1,(a0)+

decimal_7	moveq.l	#-1,d1
.7		addq.w	#1,d1
		sub.l	#1000000,d0
		bcc.s	.7
		add.l	#1000000,d0
		move.b	d1,(a0)+

decimal_6	moveq.l	#-1,d1
.6		addq.w	#1,d1
		sub.l	#100000,d0
		bcc.s	.6
		add.l	#100000,d0
		move.b	d1,(a0)+

decimal_5	moveq.l	#-1,d1
.5		addq.w	#1,d1
		sub.l	#10000,d0
		bcc.s	.5
		add.l	#10000,d0
		move.b	d1,(a0)+

decimal_4	moveq.l	#-1,d1
.4		addq.w	#1,d1
		sub.w	#1000,d0
		bcc.s	.4
		add.w	#1000,d0
		move.b	d1,(a0)+

decimal_3	moveq.l	#-1,d1
.3		addq.w	#1,d1
		sub.w	#100,d0
		bcc.s	.3
		add.w	#100,d0
		move.b	d1,(a0)+

decimal_2	moveq.l	#-1,d1
.2		addq.w	#1,d1
		sub.w	#10,d0
		bcc.s	.2
		add.w	#10,d0
		move.b	d1,(a0)+

decimal_1	moveq.l	#-1,d1
.1		addq.w	#1,d1
		sub.w	#1,d0
		bcc.s	.1
		move.b	d1,(a0)+

		move.b	#-1,(a0)+	; -1 Terminates.
		rts

; -----------------------------< BLIT NUMBER >------------------------------

; > A0.L = Address of number string.
; > D1.W = X positon of character.
; > D2.W = Y positon of character.

blit_number	move.b	(a0)+,d0	; Get first didgit.
		bmi.s	.zero		; If it's Minus then dump a zero.
		bne.s	.loop
		addq.w	#8,d1
		bra.s	blit_number

		move.b	(a0)+,d0	; No space.
		bmi.s	.zero
		beq.s	blit_number

.loop		bsr	blit_didgit
		addq.w	#8,d1

		move.b	(a0)+,d0
		bpl.s	.loop
		rts

.zero		subq.w	#8,d1
		moveq.l	#0,d0
		jmp	blit_didgit

; ------------------------------< EXCEPTIONS >------------------------------

address_err	move.l	2(sp),excep_addr
		move.l	10(sp),excep_pc
		move.l	#excep_ae,excep_error
		move.b	#1,excep_type
		btst.b	#4,1(sp)
		bne.s	.no_read
		move.b	#-1,excep_type
.no_read	bra	exception

bus_error	move.l	2(sp),excep_pc
		move.l	#excep_be,excep_error
		move.b	#0,excep_type
		bra	exception

illegal_inst	move.l	2(sp),excep_pc
		move.l	#excep_ii,excep_error
		move.b	#0,excep_type
		bra.s	exception

divide_zero	move.l	2(sp),excep_pc
		move.l	#excep_dz,excep_error
		move.b	#0,excep_type
		bra.s	exception

grab_error	move.l	(sp)+,excep_pc
		subq.l	#4,excep_pc
		move.l	#excep_ge,excep_error
		move.b	#0,excep_type
		bra.s	exception

pic_failed	move.l	(sp)+,excep_pc
		subq.l	#4,excep_pc
		move.l	#excep_pf,excep_error
		move.b	#0,excep_type
		bra.s	exception

go_asm		move.l	(sp)+,excep_pc	; Jsr this code to get back...
		subq.l	#4,excep_pc
		move.l	#excep_pds,excep_error
		move.b	#0,excep_type

exception	move.l	sp,excep_sp
		movem.l	a0-a6/d0-d7,-(sp)

		;move.w	#$000,cop_colours+2	; Make sure you can see text.
		;move.w	#$fff,cop_colours+6
		;move.w	#$000,colours+0
		;move.w	#$fff,colours+2

		lea	excep_number,a0
		move.l	excep_pc,d0		; PC.
		bsr	decimal_10

		lea	excep_text,a0
		bsr	blit_text

		move.l	excep_error,a0		; Error string.
		move.w	excep_xpos,d1
		move.w	excep_ypos,d2
		bsr	blit_string

		lea	excep_number,a0
		move.w	excep_xpos,d1
		move.w	excep_ypos,d2
		add.w	#56,d1
		add.w	#24,d2
		move.w	d1,d3

		bsr	blit_number	; Show PC.

		add.w	#16,d2
		move.l	sp,a3
		moveq.l	#16-1,d7	; Don't bother with the stack.
.loop
		move.l	(a3)+,d0
		cmp.w	#0,d7
		bne.s	.no_stack

		move.l	excep_sp,d0

.no_stack	lea	excep_number,a0
		bsr	decimal_10
		move.w	d3,d1
		lea	excep_number,a0
		bsr	blit_number
		addq.w	#8,d2
		cmp.w	#8,d7
		bne.s	.no
		addq.w	#8,d2
.no		dbra	d7,.loop


		addq.w	#8,d2
		lea	excep_wr,a1
		tst.b	excep_type
		beq.s	.pc
		bmi.s	.no_read
		lea	excep_rd,a1

.no_read	lea	excep_number,a0
		move.l	excep_addr,d0
		bsr	decimal_10
		move.l	a1,a0

		move.w	excep_xpos,d1
		bsr	blit_string

		lea	excep_number,a0
		bsr	blit_number

.pc		movem.l	(sp)+,a0-a6/d0-d7

.wait		cmp.b	#127,keycode
		beq.s	.wait
.wait1		cmp.b	#127,keycode
		bne.s	.wait1



; This code will get you back to the assembler... Allways use "JSR GO_ASM"

exit		bsr	mt_end

		move.w	#%0111111111111111,intena  	; Disable interupts.
		move.w	#%0111111111111111,dmacon  	; Disable DMA.

		move.l	asm_keyboard,$68
		move.l	asm_vblank,$6c

		moveq.l	#10-1,d0	; Flush unused keys...
.key		move.b	$bfed01,d1
		move.b	$bfec01,d1
		or.b	#$40,$bfee01
		moveq.l	#100-1,d1
.delay		dbra	d1,.delay
		and.b	#$bf,$bfee01
		dbra	d0,.key

		move.l	4,a6			; Reset copper.
		lea	gfx_name,a1
		jsr	-408(a6)
		move.l	d0,a0
		move.l  38(a0),d0
		move.l  d0,cop1lch
		move.w  copjmp1,d0

		move.l	asm_stack,sp
		movem.l	(sp)+,d0-d7/a0-a6
		move.w	asm_dmacon,dmacon
		move.w	asm_intena,intena
		rts


; -----------------------------< BLIT SPRITES >-----------------------------
terminate	rts

mask_sprites	;lea	sprite_structure-sprlist_len,a0
		
		sub.w	#sprlist_len,a0
		
		lea	bltapth,a1			; A
		lea	bltbpth,a2			; B
		lea	bltcpth,a3			; C
		lea	bltdpth,a4			; D
		lea	sprite_info_structure,a5
		move.l	bak_lpt,a6

		blitwait
		move.w	#-1,bltafwm
		clr.w	bltalwm

mask_loop	lea	sprlist_len(a0),a0

		tst.w	spr_type(a0)
		bmi	terminate

		move.w	spr_lastt1(a0),d7		; Address...
		
		
		;tst.w	spr_status(a0) ; if LONG1 <> 0 THEN DONT SHOW SPRITE
		;bne.s	mask_loop
		
		clr.l	d1
		clr.l	d6
		
		add.w	spr_height(a5,d7),d6
		add.w	spr_ydis(a5,d7),d6
		
		move.w	spr_lastt2(a0),spr_lastt3(a0)
		move.w	spr_lastt1(a0),spr_lastt2(a0)
		move.w	spr_type(a0),spr_lastt1(a0)
			
		move.w	spr_lastx3(a0),d1
		move.w	spr_lasty3(a0),d2
	
		move.w	spr_lastx2(a0),spr_lastx3(a0)
		move.w	spr_lasty2(a0),spr_lasty3(a0)
		
		move.w	spr_lastx1(a0),spr_lastx2(a0)
		move.w	spr_lasty1(a0),spr_lasty2(a0)
		
		move.w	spr_xpos(a0),spr_lastx1(a0)
		move.w	spr_ypos(a0),spr_lasty1(a0)
		
		move.l	spr_xvel(a0),d0		
		add.l	d0,spr_xpos(a0)		
		
		move.l	spr_yvel(a0),d0
		add.l	d0,spr_ypos(a0)
		
		asr.w	#1,d6
		sub.w	d6,d2
	
		sub.w	#8,d2
		sub.w	#2,d1
	
		and.w	#%1111111111110000,d7
		add.w	spr_xdis(a5,d7),d1
		add.w	spr_ydis(a5,d7),d2
	
		bgt	mask32
		bra	mask16

		bra	mask_loop

		rts

test_counterd	dc.w	0

blit_sprites	;lea	sprite_structure-sprlist_len,a0

		sub.w	#sprlist_len,a0

		lea	bltapth,a1			; A
		lea	bltbpth,a2			; B
		lea	bltcpth,a3			; C
		lea	bltdpth,a4			; D
		lea	sprite_info_structure,a5
		move.l	bak_lpt,a6

		blitwait
		move.w	#-1,bltafwm
		clr.w	bltalwm

blit_loop	lea	sprlist_len(a0),a0

		move.w	spr_type(a0),d7		; Address...
		bmi	terminate
		
		tst.w	spr_status(a0) ; if LONG1 <> 0 THEN DONT SHOW SPRITE
		bne.s	blit_loop
		
		clr.l	d1
		clr.l	d6
		
		add.w	spr_height(a5,d7),d6
		add.w	spr_ydis(a5,d7),d6
		
		move.w	spr_xpos(a0),d1
		move.w	spr_ypos(a0),d2	
				
		asr.w	#1,d6
		sub.w	d6,d2
	
		and.w	#%1111111111110000,d7
		add.w	spr_xdis(a5,d7),d1
		add.w	spr_ydis(a5,d7),d2


		cmp.w	#16,spr_width(a5,d7)

		bgt	blit32
		
		bra	blit16

		bra	blit_loop
		
func		equ	$aa
backQuest		dc.w	1
right_width	equ	400


; -------------------------------< BLIT 16 >--------------------------------
mask16		move.w	spr_height(a5,d7),d5	; Height...
		move.l	spr_address(a5,d7),d7

		add.w	#16,d5

		tst.w	d1
		blt	mask_loop
		cmp.w	#right_width,d1
		bge	mask_loop

		tst.w	d2
		bpl.s	.top_okay
		add.w	d5,d2
		beq	mask_loop
		bmi	mask_loop

		sub.w	d2,d5
		lsl.w	#4,d5		; Find lenght of sprite line...
		add.w	d5,d7
		move.w	d2,d5
		move.l	(a6),d2
		bra.s	.clipped2

.top_okay	move.w	#sprite_lines,d0	; "192" is the bottom line +1.
		sub.w	d5,d0
		sub.w	d2,d0
		bpl.s	.clipped1
		add.w	d0,d5
		beq	mask_loop
		bmi	mask_loop

.clipped1	add.w	d2,d2		; Find Screen position.
		add.w	d2,d2
		move.l	(a6,d2),d2
.clipped2	move.w	d1,d0
		lsr.w	#4,d0
		add.w	d0,d0
		add.w	d0,d2

		and.w	#15,d1		; Find shift count.	
		ror.w	#4,d1
		lsl.w	#8,d5
		addq.w	#2,d5		; 1 Word wide +1.

		blitwait
		
		move.w	#-1,bltalwm		
		move.w	#plane_len-4,bltcmod
		move.w	#plane_len-4,bltdmod

		clr.w	bltcon1
		move.w	#usec+used+$aa,d1	; Arrrh...
		move.w	d1,bltcon0
		
		move.l	d2,(a4)
		
		move.l	bak_screen,d3
		sub.l	d3,d2
		
		move.l	ref_screen,d3
		add.l	d3,d2
		
		move.l	d2,(a3)
		
		
		move.w	d5,bltsize		; Go...
		bra	mask_loop

		
; -------------------------------< BLIT 32 >--------------------------------

mask32		move.w	spr_height(a5,d7),d5	; Height...
		move.l	spr_address(a5,d7),d7
		
		add.w	#16,d5
		
		cmp.w	#0,d1
		bmi	mask_loop
		cmp.w	#right_width,d1
		bge	mask_loop

		tst.w	d2		
		bpl	.top_okay
		add.w	d5,d2
		beq	mask_loop
		bmi	mask_loop
		sub.w	d2,d5
		lsl.w	#5,d5		; Find lenght of sprite line...
		add.w	d5,d7
		move.w	d2,d5
		move.l	(a6),d2
		bra.s	.clipped2

.top_okay	move.w	#sprite_lines,d0	; "192" is the bottom line +1.
		sub.w	d5,d0
		sub.w	d2,d0
		bpl.s	.clipped1
		add.w	d0,d5
		beq	mask_loop
		bmi	mask_loop

.clipped1	add.w	d2,d2		; Find Screen position.
		add.w	d2,d2
		move.l	(a6,d2),d2
.clipped2	move.w	d1,d0
		lsr.w	#4,d0
		add.w	d0,d0
		add.w	d0,d2

		and.w	#15,d1	; Find shift count.
		ror.w	#4,d1
		lsl.w	#8,d5
		addq.w	#3,d5		; 2 Words wide +1.

		blitwait
		
		move.w	#-1,bltalwm		
		move.w	#plane_len-6,bltcmod
		move.w	#plane_len-6,bltdmod

		clr.w	bltcon1
		
		move.w	#usec+used+$aa,d1	; Arrrh...
		move.w	d1,bltcon0
		move.l	d2,(a4)
		
		move.l	bak_screen,d4
		sub.l	d4,d2
		
		move.l	ref_screen,d3
		add.l	d3,d2
	
		move.l	d2,(a3)
		
		move.w	d5,bltsize		; Go...
		bra	mask_loop
	
; ------------------------< BLIT 16 >-----------------------------------------
		
blit16		move.w	spr_height(a5,d7),d5	; Height...
		move.l	spr_address(a5,d7),d7

		tst.w	d1
		blt	blit_loop
		cmp.w	#right_width,d1
		bge	blit_loop

		tst.w	d2
		bpl	.top_okay
		add.w	d5,d2
		beq	blit_loop
		bmi	blit_loop

		sub.w	d2,d5
		lsl.w	#4,d5		; Find lenght of sprite line...
		add.w	d5,d7
		move.w	d2,d5
		move.l	(a6),d2
		bra.s	.clipped2

.top_okay	move.w	#sprite_lines,d0	; "192" is the bottom line +1.
		sub.w	d5,d0
		sub.w	d2,d0
		bpl.s	.clipped1
		add.w	d0,d5
		beq	blit_loop
		bmi	blit_loop

.clipped1	add.w	d2,d2		; Find Screen position.
		add.w	d2,d2
		move.l	(a6,d2),d2
.clipped2	move.w	d1,d0
		lsr.w	#4,d0
		add.w	d0,d0
		add.w	d0,d2

		and.w	#15,d1		; Find shift count.	
		ror.w	#4,d1
		lsl.w	#8,d5
		addq.w	#2,d5		; 1 Word wide +1.

		blitwait

		clr.w	bltamod
		clr.w	bltbmod
		move.w	#plane_len-4,bltcmod
		move.w	#plane_len-4,bltdmod

		move.w	#0,bltalwm		
		move.w	d1,bltcon1
		or.w	#usea+useb+usec+used+%11001010,d1	; Arrrh...
		move.w	d1,bltcon0

		move.l	d7,(a1)
		addq.w	#2,d7
		move.l	d7,(a2)
		move.l	d2,(a3)
		move.l	d2,(a4)
		move.w	d5,bltsize		; Go...
		bra	blit_loop

; -------------------------------< BLIT 32 >--------------------------------

blit32		move.w	spr_height(a5,d7),d5	; Height...
		move.l	spr_address(a5,d7),d7

		tst.w	d1		
		bmi	blit_loop
		cmp.w	#right_width,d1
		bge	blit_loop

		tst.w	d2		
		bpl	.top_okay
		add.w	d5,d2
		beq	blit_loop
		bmi	blit_loop
		sub.w	d2,d5
		lsl.w	#5,d5		; Find lenght of sprite line...
		add.w	d5,d7
		move.w	d2,d5
		move.l	(a6),d2
		bra.s	.clipped2

.top_okay	move.w	#sprite_lines,d0	; "192" is the bottom line +1.
		sub.w	d5,d0
		sub.w	d2,d0
		bpl.s	.clipped1
		add.w	d0,d5
		beq	blit_loop
		bmi	blit_loop

.clipped1	add.w	d2,d2		; Find Screen position.
		add.w	d2,d2
		move.l	(a6,d2),d2
.clipped2	move.w	d1,d0
		lsr.w	#4,d0
		add.w	d0,d0
		add.w	d0,d2

		and.w	#15,d1		; Find shift count.
		ror.w	#4,d1
		lsl.w	#8,d5
		addq.w	#3,d5		; 2 Words wide +1.

		blitwait
		
		move.w	#0,bltalwm		

		move.w	#2,bltamod
		move.w	#2,bltbmod
		move.w	#plane_len-6,bltcmod
		move.w	#plane_len-6,bltdmod

		move.w	d1,bltcon1
		or.w	#usea+useb+usec+used+%11001010,d1	; Arrrh...
		move.w	d1,bltcon0

		move.l	d7,(a1)
		addq.l	#4,d7
		move.l	d7,(a2)
		move.l	d2,(a3)
		move.l	d2,(a4)
		move.w	d5,bltsize		; Go...
		bra	blit_loop



; -----------------------------< DE BLOCK >------------------------------
; -----------------------------< DE BLOCK >------------------------------

; D2 > Address of screen

de_block	
		
		sub.l	#16*line_len,d2 ; Scroller
		
		clr.l	d3
		clr.l	d4
		
		move.w	yscroll,d3
		move.w	d3,d4
		
		not.w	d3
		and.w	#15,d3
		
		mulu.w	#line_len,d3
		add.l	d3,d2
		
		lsr.w	#4,d4
		mulu.w	#44,d4
		
		lea	map,a0
		add.l	d4,a0
		
		lea	bltapth,a1		; A.
		lea	bltdpth,a2		; D.
		lea	bltsize,a3		; Blitsize.

		move.l	#blocks1,d0
		moveq.l	#14-1,d1		; Number of rows...
		
		sub.w	#2,d2
		
		move.w	#4*16<<6+1,d3

		blitwait			; Don't move...
		move.w	#-1,bltafwm
		move.w	#-1,bltalwm
		move.w	#usea+used+%11110000,bltcon0
		clr.w	bltamod
		move.w	#plane_len-2,bltdmod
		
.line		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		blit_block
		add.l	#16*line_len-38,d2
		sub.l	#6,d2
		
		dbra	d1,.line
		
		rts

; -----------------------------< GRAB SPRITES >-----------------------------

; > A0.L = Sprite info list.
; > A1.L = Destination.
; > A2.L = Address of screen.

; < d7.l = no_of_sprites
; < D6.L = Amount of memory used...

grab_sprites	clr.l	d7

		lea	-4(a2),a3	
		moveq.l	#0,d6

.across		addq.w	#4,a3
		move.l	a3,a2

		tst.w	40*3(a2)		; Look for control bar...
		bne.s	.not_found
		tst.w	40*2(a2)
		bne.s	.not_found
		tst.w	40*1(a2)
		bne.s	.not_found
		move.w	40*0(a2),d0

		cmp.w	#%1111111111111110,d0
		beq.s	.found
		rts


.grab		tst.w	40*3(a2)		; Look for control bar...
		bne.s	.not_found
		tst.w	40*2(a2)
		bne.s	.not_found
		tst.w	40*1(a2)
		bne.s	.not_found
		move.w	40*0(a2),d0

		cmp.w	#%1111111111111110,d0
		beq.s	.found

.not_found	jsr	grab_error



.found	; Found sprite - so must know height, width, xdis y dis...

		move.w	#-1,d2	; Now scan for Y dis...
		move.l	the_screen,d4
		add.l	#32000,d4

.ydis		add.w	#160,a2
		cmp.l	d4,a2
		bge.s	.across
		addq.w	#1,d2
		move.l	40*0(a2),d0
		or.l	40*1(a2),d0
		or.l	40*2(a2),d0
		or.l	40*3(a2),d0
		beq.s	.ydis

; D2 = Y displacement - A2.L = Address of line sprite starts on...

		move.l	a2,a6
		moveq.l	#0,d4

.height		add.w	#160,a6
		addq.w	#1,d4
		tst.w	40*3(a6)
		bne.s	.height
		tst.w	40*2(a6)
		bne.s	.height
		tst.w	40*1(a6)
		bne.s	.height
		move.w	40*0(a6),d0
		cmp.w	#%1111111111111110,d0
		bne.s	.height

; D4 = Height...

		move.l	a2,a6
		move.w	d4,d1	; Hgt...
		moveq.l	#0,d0

.scan1		or.l	40*0(a6),d0
		or.l	40*1(a6),d0
		or.l	40*2(a6),d0
		or.l	40*3(a6),d0
		add.w	#160,a6
		subq.w	#1,d1
		bne.s	.scan1

		move.w	#-1,d1

.xdis		addq.w	#1,d1
		lsl.l	#1,d0
		bcc.s	.xdis

; D1 = X displacement...

		move.w	d1,d3
		beq.s	.no_shift
		move.l	a2,a6
		move.w	d4,d5

.shift		move.l	(a6),d0
		lsl.l	d3,d0
		move.l	d0,(a6)
		add.w	#40,a6

		move.l	(a6),d0
		lsl.l	d3,d0
		move.l	d0,(a6)
		add.w	#40,a6

		move.l	(a6),d0
		lsl.l	d3,d0
		move.l	d0,(a6)
		add.w	#40,a6

		move.l	(a6),d0
		lsl.l	d3,d0
		move.l	d0,(a6)
		add.w	#40,a6

		subq.w	#1,d5
		bne.s	.shift

.no_shift	; Sprite is now on in correct place...

		move.l	a2,a6
		move.w	d4,d3
		moveq.l	#0,d0

.scan2		or.l	40*0(a6),d0
		or.l	40*1(a6),d0
		or.l	40*2(a6),d0
		or.l	40*3(a6),d0
		add.w	#160,a6
		subq.w	#1,d3
		bne.s	.scan2

		moveq.l	#33,d3

.width		subq.w	#1,d3
		lsr.l	#1,d0
		bcc.s	.width	

; D3.W = Width...

		move.l	a1,(a0)+
		move.w	d1,(a0)+
		move.w	d2,(a0)+
		move.w	d3,(a0)+
		move.w	d4,(a0)+
		addq.w	#4,a0		; This is spare...

		addq.l	#1,d7
		
		cmp.w	#16,d3
		bgt.s	.long

.word		move.w	40*0(a2),d0
		or.w	40*1(a2),d0
		or.w	40*2(a2),d0
		or.w	40*3(a2),d0

		move.w	d0,(a1)+
		move.w	(a2),(a1)+
		add.w	#40,a2
		move.w	d0,(a1)+
		move.w	(a2),(a1)+
		add.w	#40,a2
		move.w	d0,(a1)+
		move.w	(a2),(a1)+
		add.w	#40,a2
		move.w	d0,(a1)+
		move.w	(a2),(a1)+
		add.w	#40,a2

		add.l	#16,d6
		subq.w	#1,d4
		bne.s	.word

		bra	.grab

.long		move.l	40*0(a2),d0
		or.l	40*1(a2),d0
		or.l	40*2(a2),d0
		or.l	40*3(a2),d0

		move.l	d0,(a1)+
		move.l	(a2),(a1)+
		add.w	#40,a2
		move.l	d0,(a1)+
		move.l	(a2),(a1)+
		add.w	#40,a2
		move.l	d0,(a1)+
		move.l	(a2),(a1)+
		add.w	#40,a2
		move.l	d0,(a1)+
		move.l	(a2),(a1)+
		add.w	#40,a2

		add.l	#32,d6
		subq.w	#1,d4
		bne.s	.long

		bra	.grab

; -----------------------------< GRAB BLOCKS >-----------------------------

grab_blocks	;lea	blocks,a0
		move.l	bak_screen,a2
		moveq.l	#13-1,d5

.down		move.l	a2,a3
		moveq.l	#20-1,d6

.across		move.l	a2,a4
		moveq.l	#16-1,d7

.line		move.w	40*0(a2),(a0)+
		move.w	40*1(a2),(a0)+
		move.w	40*2(a2),(a0)+
		move.w	40*3(a2),(a0)+
		lea	160(a2),a2
		dbra	d7,.line

		lea	2(a4),a2
		dbra	d6,.across

		lea	16*160(a3),a2
		dbra	d5,.down
		rts

; -------------------------------< TASKING >-------------------------------

tasking		;lea	sprite_structure,a0 ; Sorry but it's nessessary
		
.loop		tst.w	spr_type(a0)
		bmi.s	.done
		move.l	spr_task(a0),a1
		jsr	(a1)
		lea	sprlist_len(a0),a0
		bra.s	.loop
.done		rts

;---------------------------------------------------------------------------

colequ1		equ	8
colequ2		equ	12

fadepalin	lea	colours,a0
		move.w	#32,d4	; loop so that we update all 32 colours
red1		move.w	(a0),d0 ; move a system colour to a data register
		lsr.w	#8,d0	; Strip it down so that we only have the red
		move.w	(a1),d1 ; nibble..
		lsr.w	#8,d1   ; Do the same with the hidden colour
		cmp.w	d1,d0	; compare them
		blt	addr	; If the red value is too high decrement it.
		bgt	subr	; If the red value is too low increment it.
green		move.w	(a0),d0 ; If its equal leave it as it is
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
		bne	red1	; Hopefully the vblank will update the colours

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
		
pal_error	dc.b	"Pallette Error",0
pal_ecode	dc.w	0
pal_ok		dc.b	"Pallette Ok",0

clear_pal	macro 	; 	Very useful macro (N O T!)
		move.w	#16-1,d0
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		clr.l	(a1)+
		endm
; --------------------------------< INTRO > --------------------------------
map_line	equ	2560
holo_count	equ	500
menu_count	equ	25
pres_count	equ	350
a2a_count	equ	350
gap_count	equ	25
scroll_count	equ	100

intro		bsr	cls_both
		lea	blocks1,a0
		bsr	grab_blocks
		clr.l	yscroll		
		move.l	ref_screen,d2
		bsr	de_block
		lea	colours,a1
		clear_pal

		lea	bak_colours,a1
		move.l	bak_screen,a2
		lea	hologenics_pic,a0		
		bsr	show_pic
		lea	blocks1,a0
		bsr	grab_blocks
		clr.w	count
		
		move.l	bak_screen,d2
		bsr	de_block
		bsr	swap_screen
		move.l	bak_screen,d2
		bsr	de_block
		lea	cop_screen2,a1
		move.l	#blank_line,d0
		set_screen
		move.l	#50*65536,ripst
		
holo_loop	bsr	halt
		bsr	swap_screen
		bsr	ripplecon
		lea	bak_colours,a1
		bsr	fadepalin
		add.w	#1,count
		
		btst.b	#joy_fire,joyval2
		bne	end_loop
		cmp.w	#holo_count,count
		bmi.s	holo_loop
		clr.w	count
		lea	bak_colours,a1
		clear_pal

.gap_loop	bsr	halt		
		lea	bak_colours,a1
		bsr	fadepalin
		add.w	#1,count		
		btst.b	#joy_fire,joyval2
		bne	end_loop
		cmp.w	#gap_count,count
		bmi.s	.gap_loop
		clr.w	count
		bsr	cls_both
		lea	colours,a1
		clear_pal
		lea	bak_colours,a1
		move.l	bak_screen,a2
		lea	presents_pic,a0 
		bsr	show_pic

		lea	blocks1,a0
		bsr	grab_blocks
		move.l	bak_screen,d2
		bsr	de_block
		bsr	swap_screen
		lea	cop_screen2,a1
		move.l	#blank_line,d0
		set_screen
		move.l	bak_screen,d2
		bsr	de_block
		move.l	#100*65536,ripst

pres_loop	bsr	halt
		bsr	ripplecon
		bsr	swap_screen
		lea	bak_colours,a1
		bsr	fadepalin	
		add.w	#1,count
		btst.b	#joy_fire,joyval2
		bne	end_loop
		cmp.w	#pres_count,count
		bmi.s	pres_loop
		clr.w	count
		lea	bak_colours,a1
		clear_pal
		move.l	bak_screen,d2
		bsr	de_block
		bsr	swap_screen
		move.l	bak_screen,d2
		bsr	de_block

.gap_loop	bsr	halt		
		lea	bak_colours,a1
		bsr	fadepalin
		add.w	#1,count
		cmp.w	#gap_count,count
		bmi.s	.gap_loop
		clr.w	count
		bsr	cls_both
		lea	colours,a1
		clear_pal
		lea	bak_colours,a1
		move.l	bak_screen,a2
		lea	title_pic,a0 
		bsr	show_pic

		lea	blocks1,a0
		bsr	grab_blocks
		move.l	bak_screen,d2
		bsr	de_block
		bsr	swap_screen
		move.l	bak_screen,d2
		bsr	de_block
		move.l	#90*65536,ripst

a2a_loop	bsr	halt
		bsr	ripplecon
		bsr	swap_screen
		lea	bak_colours,a1
		bsr	fadepalin
		add.w	#1,count
		btst.b	#joy_fire,joyval2
		bne	end_loop
		cmp.w	#a2a_count,count
		bmi.s	a2a_loop
		clr.w	count
		lea	bak_colours,a1
		clear_pal
		
.gap_loop	bsr	halt		
		move.l	bak_screen,d2
		bsr	de_block
		lea	bak_colours,a1
		bsr	fadepalin
		add.w	#1,count
		cmp.w	#gap_count,count
		bmi.s	.gap_loop
		clr.w	count
		bsr	cls_both	
		lea	blocks1,a0
		bsr	grab_blocks
		lea	colours,a1
		move.l	bak_screen,a2
		lea	credits_pic,a0 
		bsr	show_pic
		lea	blocks1,a0
		add.l	#13*map_line,a0
		bsr	grab_blocks
		
		lea	colours,a1
		move.l	bak_screen,a2
		lea	credits_pic2,a0 
		bsr	show_pic
		lea	blocks1,a0
		add.l	#26*map_line,a0
		bsr	grab_blocks
		lea	bak_colours,a1
		clear_pal
		move.l	bak_screen,d2
		bsr	de_block		
		bsr	swap_screen
		move.l	bak_screen,d2
		bsr	de_block
		move.l	#230*65536,ripst
		bsr	ripplecon
		
scroll_loop	clr.b	flag			
.halt		tst.b	flag			
		beq.s	.halt	
		move.l	bak_screen,d2	
		bsr	de_block
		bsr	swap_screen
		add.w	#1,yscroll
		cmp.w	#448,yscroll
		bmi.s	.okay
		move.w	#300,ripst
		bsr	ripplecon
		move.w	#448,yscroll
		addq.w	#1,count
		cmp.w	#scroll_count,count
		bmi.s	.okay
		clr.w	count
		bra.s	end_loop
.okay		bra.s	scroll_loop
end_loop	clr.w	count
.end_loop	bsr	halt		
		lea	bak_colours,a1
		clear_pal
		lea	bak_colours,a1
		bsr	fadepalin
		
		add.w	#1,count
		
		cmp.w	#gap_count,count
		bmi.s	.end_loop

		move.w	#300,ripst
		bsr	ripplecon

		clr.w	count
		
		bsr	cls_both
		
		clr.l	yscroll

		rts

count		dc.l	0

; ---------------------------------< DATA >---------------------------------

colours		dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000

bak_colours 	dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000

extra_pal	dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff ; extra pallette for HW sprites
		dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff


copper		dc.l	intro_copperlist
which_background
		dc.l	block_pic2

asm_intena	dc.w	0
asm_dmacon	dc.w	0
asm_vblank	dc.l	0
asm_keyboard	dc.l	0
asm_stack	dc.l	0

gfx_name	dc.b	"graphics.library",0
		even

the_screen	dc.l	scr01	; Address of the screen on display.
bak_screen	dc.l	scr02	; Address of the back screen.
ref_screen	dc.l	scr03	; Address of the Refresh Screen.

the_lpt		dc.l	lpt01
bak_lpt		dc.l	lpt02
ref_lpt		dc.l	lpt03

joyval1		dc.b	0
joyval2		dc.b	0
joyval3		dc.b	0
joyval4		dc.b	0
joyval5		dc.b	0
xmouse1		dc.w	0
ymouse1		dc.w	0
xmouse2		dc.w	0
ymouse2		dc.w	0
mouseval1	dc.w	0
mouseval2	dc.w	0

flag		dc.b	0	; System flags...
keycode		dc.b	0

hrz_pixels	dc.l	0	; Length of map in pixels.
vrt_pixels	dc.l	0	; Height of map in pixels.
yscroll		dc.w	0
		dc.w	0

init_flag	dc.w	0
spodd_flag	dc.w	0

master_sprite	dc.l	0	; Sprite structure that moves scroll.
memory_used	dc.l	0	; Amount of memory used by the GSprite
sprites_avail	dc.l	0	; Number of Available sprites

excep_number	dc.b	"0123456789",-1
excep_type	dc.b	0

excep_pc	dc.l	0		; Program counter.
excep_error	dc.l	0		; Address of text string.
excep_sp	dc.l	0		; Usefull...
excep_addr	dc.l	0		; Very usefull...

excep_text

excep_xpos	dc.w	92		; X position of box.
excep_ypos	dc.w	0		; Y position of box.

		dc.b	"abbbbbbbbbbbbbbbbbbbc",cr
		dc.b	"h^^^^^^^^^^^^^^^^^^^d",cr
		dc.b	"h-------------------d",cr
		dc.b	"h^^^PC^^^^^^^^^^^^^^d",cr
		dc.b	"h-------------------d",cr
		dc.b	"h^^^D0^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D1^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D2^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D3^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D4^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D5^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D6^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D7^^^^^^^^^^^^^^d",cr
		dc.b	"h-------------------d",cr
		dc.b	"h^^^A0^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A1^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A2^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A3^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A4^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A5^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A6^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A7^^^^^^^^^^^^^^d",cr
		dc.b	"h-------------------d",cr
		dc.b	"hSPACE<<^^^^^^^^^^^^d",cr
		dc.b	"gfffffffffffffffffffe",0
		even



excep_be	dc.b	cr,"      BUS ERROR      ",0
		even

excep_ii	dc.b	cr," ILLEGAL INSTRUCTION ",0
		even

excep_ae	dc.b	cr,"    ADDRESS ERROR    ",0
		even

excep_dz	dc.b	cr,"   DIVIDE BY ZERO    ",0
		even

excep_pds	dc.b	cr,"   ALL SYSTEMS GO    ",0
		even

excep_ge	dc.b	cr,"     GRAB ERROR      ",0
		even

excep_pf	dc.b	cr," SHOW PIC HAS FAILED ",0
		even

excep_rd	dc.b	" READING  ",0
		even

excep_wr	dc.b	" WRITING  ",0
		even


font		incbin	data/sys_font.bin

		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0111111000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0

		ds.w	4*8*4

		dc.w	%0000000000000000,0,0,0
		dc.w	%0011111100000000,0,0,0
		dc.w	%0111111100000000,0,0,0
		dc.w	%0111000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0

		dc.w	%0000000000000000,0,0,0
		dc.w	%1111111100000000,0,0,0
		dc.w	%1111111100000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0

		dc.w	%0000000000000000,0,0,0
		dc.w	%1111110000000000,0,0,0
		dc.w	%1111111000000000,0,0,0
		dc.w	%0000110000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0

		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0

		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000010000000000,0,0,0
		dc.w	%0000110000000000,0,0,0
		dc.w	%1111100000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0

		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%1111111100000000,0,0,0
		dc.w	%0000000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0

		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0111000000000000,0,0,0
		dc.w	%0111111100000000,0,0,0
		dc.w	%0010000000000000,0,0,0
		dc.w	%0000000000000000,0,0,0

		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0
		dc.w	%0110000000000000,0,0,0

number		dc.b	"0123456789",-1	; A place to create nßßumber string.
		even

h_sprites	dc.l	dummy ;hsprites
		dc.l	dummy
		dc.l	dummy
		dc.l	dummy
		dc.l	dummy
		dc.l	dummy
		dc.l	dummy
		dc.l	dummy

dummy		dc.l	0

lpt01		ds.l	act_lines
lpt02		ds.l	act_lines
lpt03		ds.l	act_lines

stack		ds.l	64



hard_info_structure	ds.b	16*8	; 8 Hardware sprites
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
		dc.l	6
		dc.l	-1
		dc.l	-1
playing_pos	dc.w	0
repeat_flag	dc.w	0
	
		include	music/replay.s ; music player
		
; ---------------------------------< MENU >----------------------------------
		
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
		jsr	cls
		
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
		
		tst.b	key_space
		beq.s	menu_loop
		
		lea	bak_colours,a1
		clear_pal
		
		clr.w	count

seed_loop	bsr 	halt

		tst.b	key_space
		beq.s	fade_menu
	
		add.w	#1,seed

		bra	seed_loop
		
fade_menu	bsr	halt
		move.l	bak_screen,d2
		bsr	de_block
		bsr	swap_screen

		lea	bak_colours,a1
		bsr	fadepalin
		
		add.w	#1,count
		cmp.w	#gap_count,count
		bne.s	fade_menu
		move.l	mcopper,copper
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
		move.l	#day_copperlist,mcopper
		
		rts

.night		lea	block_pic2,a1
		move.l	a1,which_background
		
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		
		move.w	#16*151,spr_type(a0)
		move.w	#16*155,spr_type(a1)
		move.l	#night_copperlist,mcopper
		
		rts
		
mcopper		dc.l	0

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
		

; --------------------------< INIT HARDWARE SPRITES >------------------------
init_hard	tst.w	init_flag
		bne	terminate

		lea	hsprite_structures,a5
		lea	Hardware_sps,a1
		lea	Haddress5,a3
		lea	Haddress1,a6

.init		tst.w	spr_type(a5)
		bmi	.done
		
		move.w	spr_xstart(a5),spr_xpos(a5)
		move.w	spr_ystart(a5),spr_ypos(a5)
		lea	sprlist_len(a5),a5
		bra.s	.init
.done		
		lea	hsprite_structures,a5
		move.w	#2-1,d5 ; Only four Hardware sprites left.

.init_loop	lea	hard_info_structure,a0 ; bodge to transform the sprites into hw sprites
		move.l	a1,a4
		move.w	spr_type(a5),d7
		add.w	d7,a0
		
		clr.l	d0
		clr.l	d1
		clr.l	d2
		
		move.w	spr_height(a0),d0
		move.w	spr_xpos(a5),d1
		move.w	spr_ypos(a5),d2
		
		btst.l	#0,d1	; Check if X-POS EVEN
		beq.s	.even
		move.w	#1,spodd_flag
		bra.s	.cont
.even		clr.w	spodd_flag
.cont		
		add.w	#56,d2
		add.w	#$60,d1
		add.w	d2,d0
		lsl.w	#8,d0
		lsl.w	#8,d2
		lsr.w	#1,d1
		add.l	d1,d2
		swap	d2
		add.l	d0,d2
		
		tst.w	spodd_flag
		beq.s	.nodd
		add.l	#1,d2
.nodd		
		move.w	spr_height(a0),d0
		move.l	a1,d3	; Store the addresses of the pointers in a table
		move.l	d3,(a6)+
		add.l	#Hsprite2,d3
		move.l	d3,(a6)+
		
		move.l	d2,Hsprites(a1)
		add.l	#8*65536,d2
		move.l	d2,Hsprite2(a1)
		
		addq.l	#4,a1
		move.l	spr_address(a0),a2
		move.w	d0,d6
		
.line		move.w	(a2),Hsprites+2(a1)
		move.w	4(a2),Hsprites(a1)
		move.w	2(a2),Hsprite2+2(a1)
		move.w	6(a2),Hsprite2(a1)
		add.l	#32,a2
		addq.l	#4,a1
		
		dbra	d0,.line

.create_extras	move.l	a1,d3	; Store the addresses of the pointers in a table
		move.l	d3,(a3)+
		add.l	#Hsprite2,d3
		move.l	d3,(a3)+
		addq.l	#4,a1
		move.w	d6,d0
		move.l	spr_address(a0),a2
		move.w	d0,d6
		
.copy_loop	move.w	(a2),Hsprites+2(a1)
		move.w	4(a2),Hsprites(a1)
		move.w	2(a2),Hsprite2+2(a1)
		move.w	6(a2),Hsprite2(a1)
		
		add.l	#32,a2
		addq.l	#4,a1
		
		dbra	d0,.copy_loop
		move.l	a4,a1
		lea	Hsprite3(a1),a1

		lea	sprlist_len(a5),a5
		dbra	d5,.init_loop

		lea	Hardware_sps,a1 ; Store The Hardware sprites
		move.l	a1,h_sprites
		lea	Hsprite2(a1),a1
		move.l	a1,h_sprites+4
		lea	Hsprite2(a1),a1
		move.l	a1,h_sprites+8
		lea	Hsprite2(a1),a1
		move.l	a1,h_sprites+12
		
		lea	h_sprites,a0	; Tell the copper about the sprites
		lea	day_cop_sprites,a1
		lea	night_cop_sprites,a2
		
		moveq.l	#16-1,d7
.loop		addq.w	#2,a1
		addq.w	#2,a2
		move.w	(a0),(a1)+
		move.w	(a0)+,(a2)+
		dbra	d7,.loop
		
		sub.l	#4,Haddress6 ; allow for a bug i can`t find
		sub.l	#4,Haddress7
		sub.l	#4,Haddress8

		move.w	#1,init_flag
		rts
		
; ------------------------< HARDWARE SPRIRE STRUCTURES >--------------------

hsprite_structures		; Hardware sprite structures
		dc.w	16*0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	cloud1
		dc.w	160
		dc.w	40
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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

		dc.w	16*2
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	cloud1
		dc.w	192
		dc.w	40
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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

		dc.w	16*0 
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	cloud2
		dc.w	160
		dc.w	100
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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

		dc.w	16*2
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	cloud2
		dc.w	192
		dc.w	100
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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

		dc.w	-1 ; What do you think!

; ------------------------------------------------------------------------	
		
service_hardware ; Hardware sprite service routine.
		lea	hsprite_structures,a5
		lea	Hardware_sps,a1
		lea	Haddress1,a2
.serv_loop	
		lea	hard_info_structure,a0 ; bodge to transform the sprites into hw sprites
		move.l	a1,a4
		tst.w	spr_type(a5)
		bmi	terminate
		
		move.w	spr_type(a5),d7
		add.w	d7,a0
		move.l	spr_xvel(a5),d0
		add.l	d0,spr_xpos(a5)
		move.l	spr_yvel(a5),d0
		add.l	d0,spr_xpos(a5)
		
		clr.l	d0
		clr.l	d1
		clr.l	d2
		
		move.w	spr_height(a0),d0
		move.w	spr_xpos(a5),d1
		move.w	spr_ypos(a5),d2
		
		btst.l	#0,d1	; Check if X-POS EVEN
		beq.s	.even
		move.w	#1,spodd_flag
		bra.s	.cont
.even		clr.w	spodd_flag
.cont		
		add.w	#56,d2
		add.w	#$60,d1
		add.w	d2,d0
		lsl.w	#8,d0
		lsl.w	#8,d2
		lsr.w	#1,d1
		add.l	d1,d2
		
		swap	d2
		add.l	d0,d2
		tst.w	spodd_flag
		beq.s	.nodd
		add.l	#1,d2
.nodd		
		move.w	spr_height(a0),d0
		move.l	(a2)+,a3
		move.l	d2,(a3)
		move.l	(a2)+,a3		
		
		add.l	#8*65536,d2
		move.l	d2,(a3)
		lea	sprlist_len(a5),a5
		bra	.serv_loop	

		rts
		
Haddress1	dc.l	0 ; pointers to sprite position and control data
Haddress2	dc.l	0
Haddress3	dc.l	0
Haddress4	dc.l	0
Haddress5	dc.l	0
Haddress6	dc.l	0
Haddress7	dc.l	0
Haddress8	dc.l	0
		
		rsreset
Hsprites	rs.b	400
Hsprite2	rs.b	400
Hsprite3	rs.b	400
Hsprite4	rs.b	400

Hardware_sps	ds.b	3200

; -------------------------------< THAT'S IT >------------------------------

the_code	; What can I sayQuest
		
		
		bsr	cls_both
		lea	sprite_pic,a0
		lea	bak_colours,a1
		move.l	the_screen,a2
		bsr	show_pic

		lea	sprite_info_structure,a0
		lea	sprites,a1
		move.l	the_screen,a2
		bsr	grab_sprites
	
		move.l	d6,memory_used
		move.l	d7,sprites_avail
		lea	sprite_pic2,a0
		lea	bak_colours,a1
		move.l	the_screen,a2
		bsr	show_pic
		
		move.l	memory_used,d6		
		move.l	sprites_avail,d7
		lsl.w	#4,d7
		lea	sprite_info_structure,a0
		lea	sprites,a1
		add.l	d7,a0 	; Start after First lot
		add.l	d6,a1   ; Find end of last sprite bin
		move.l	the_screen,a2
		bsr	grab_sprites
		
		add.l	d6,memory_used
		add.l	d7,sprites_avail
		lea	hardware_pic,a0
		move.l	bak_screen,a2
		lea	bak_colours,a1 ; Not the actual pallette.
		bsr	show_pic
		
		lea	hard_info_structure,a0	; Same for HW sprites.
		lea	hards,a1
		
		move.l	bak_screen,a2
		bsr	grab_sprites		
		lea	sprite_pic3,a0
		lea	bak_colours,a1
		move.l	the_screen,a2
		bsr	show_pic

		move.l	memory_used,d6		
		move.l	sprites_avail,d7
		lsl.w	#4,d7

		lea	sprite_info_structure,a0
		lea	sprites,a1
		add.l	d7,a0 	; Start after First lot
		add.l	d6,a1   ; Find end of last sprite bin
		
		move.l	the_screen,a2
		bsr	grab_sprites
		add.l	d6,memory_used
		add.l	d7,sprites_avail
		
		lea	sprite_pic4,a0
		lea	bak_colours,a1
		move.l	the_screen,a2
		bsr	show_pic

		move.l	memory_used,d6		
		move.l	sprites_avail,d7
		lsl.w	#4,d7

		lea	sprite_info_structure,a0
		lea	sprites,a1
		add.l	d7,a0 	; Start after First lot
		add.l	d6,a1   ; Find end of last sprite bin
		
		move.l	the_screen,a2
		bsr	grab_sprites
		add.l	d6,memory_used
		add.l	d7,sprites_avail


		lea	sprite_pic5,a0
		lea	bak_colours,a1
		move.l	the_screen,a2
		bsr	show_pic

		move.l	memory_used,d6		
		move.l	sprites_avail,d7
		lsl.w	#4,d7

		lea	sprite_info_structure,a0
		lea	sprites,a1
		add.l	d7,a0 	; Start after First lot
		add.l	d6,a1   ; Find end of last sprite bin
		
		move.l	the_screen,a2
		bsr	grab_sprites
		add.l	d6,memory_used
		add.l	d7,sprites_avail


		kill_sprites	; Kill all Hardware sprites
		bsr	init_hard
		bsr	intro
		bsr	menu
		bsr	cls_both
		
		lea	colours,a1
		move.l	bak_screen,a2
		move.l	which_background,a0
		bsr	show_pic
		lea	blocks1,a0
		bsr	grab_blocks
	
		cmp.w	#3,players	; 2 or 3 Players.
		bne.s	.2
	
.3		clr.w	out3 ; Yeah lets bump start plane3.
		lea	plane_3,a0
		move.l	#tower3,a1
		move.l	a1,spr_task(a0)
		
		move.w	#1,spr_status(a0)
		lea	sprlist_len(a0),a0
		move.w	#1,spr_status(a0)
		
		lea	plane_1,a0
		move.l	#tower1,a1
		move.l	a1,spr_task(a0)
		
		clr.w	spr_status(a0)
		lea	sprlist_len(a0),a0
		clr.w	spr_status(a0)
		bra.s	.cont ; Continue with program.
		
.2		move.w	#1,out3	; Sorry, plane 3 left in the hangar this time.
		lea	plane_3,a0
		move.l	#tower3,a1
		move.l	a1,spr_task(a0)
		move.w	#1,spr_status(a0)
		lea	sprlist_len(a0),a0
		move.w	#1,spr_status(a0)
		
.cont		lea	runway1stack,a2
		move.l	#plane_3,(a2)+
		move.l	#plane_4,(a2)+
		move.l	#0,(a2)
		move.w	#1,runway1clearQuest

		lea	plane_1,a0
		move.l	#takeoff1,a1
		move.w	#1,runway1clearQuest
		move.l	a1,spr_task(a0)
		clr.w	spr_status(a0)
		lea	sprlist_len(a0),a0
		clr.w	spr_status(a0)
		
		lea	plane_4,a0
		move.l	#tower4,a1
		move.l	a1,spr_task(a0)
		move.w	#1,spr_status(a0)
		lea	sprlist_len(a0),a0
		move.w	#1,spr_status(a0)
		
		
		lea	plane_2,a0
		move.l	#tower2,a1
		move.l	a1,spr_task(a0)
		move.w	#1,spr_status(a0)
		lea	sprlist_len(a0),a0
		move.w	#1,spr_status(a0)
	
		lea	plane_5,a0
		move.l	#tower5,a1
		move.l	a1,spr_task(a0)
		move.w	#1,spr_status(a0)
		lea	sprlist_len(a0),a0
		move.w	#1,spr_status(a0)
		
		
		lea	plane_3,a0
		move.l	#tower3,a1
		move.l	a1,spr_task(a0)
		move.w	#1,spr_status(a0)
		lea	sprlist_len(a0),a0
		move.w	#1,spr_status(a0)
		
	
		lea	missile1,a0	;Kill Missiles
		move.w	#1,spr_status(a0)
		lea	missile2,a0	;Kill Missiles
		move.w	#1,spr_status(a0)
		lea	missile3,a0	;Kill Missiles
		move.w	#1,spr_status(a0)
		lea	missile4,a0	;Kill Missiles
		move.w	#1,spr_status(a0)
		lea	missile5,a0	;Kill Missiles
		move.w	#1,spr_status(a0)

		
.planes		lea	plane_1,a0
		clr.w	spr_word1(a0)
		move.w	#2*16,spr_type(a0)
		lea	sprlist_len(a0),a0
		move.w	#10*16,spr_type(a0)
		
		lea	plane_2,a0
		move.w	#8,spr_word1(a0)
		move.w	#36*16,spr_type(a0)
		lea	sprlist_len(a0),a0
		move.w	#43*16,spr_type(a0)
		
		lea	plane_3,a0 	;reset	planes - AeroPlanes, that is.
		clr.w	spr_word1(a0)
		move.w	#100*16,spr_type(a0)
		lea	sprlist_len(a0),a0
		move.w	#108*16,spr_type(a0)
		lea	sprite_structure,a0

; ----------------------------------------------------------------------------------

start_game
.init		tst.w	spr_type(a0)
		bmi.s	.done
		move.w	spr_xstart(a0),spr_xpos(a0)
		move.w	spr_ystart(a0),spr_ypos(a0)
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		;move.w	#,spr_status(a0)
		move.w	#50,spr_vel(a0) ; The speed is shifted this many times..
		clr.l	spr_long1(a0)   ; ..so that you stay still before take off
		clr.l	spr_long2(a0)
		lea	sprlist_len(a0),a0
		bra.s	.init
.done		
		bsr	init_hard
		bsr	service_hardware
		jsr	swap_screen
		bsr	set_colours
		bsr	cls_both
		move.l	ref_screen,d2 ; Set up The Screens
		bsr	de_block
		move.l	the_screen,d2
		bsr	de_block
		move.l	bak_screen,d2
		bsr	de_block		
		change_copper
		lea	colours,a1
		lea	32(a1),a1
		lea	extra_pal,a0
		move.w	#16-1,d0
.0		move.w	(a0)+,(a1)+
		dbra	d0,.0
		reincarnate_sprites ; reactivate Hardware sprites		
game		
		
		lea	sprite_structure,a0		 
		bsr	mask_sprites
		lea	sprite_structure,a0		 
		bsr	tasking
		lea	sprite_structure,a0		 
		bsr	blit_sprites
		bsr	swap_screen
		
		clr.w	score1
		clr.w	score2
		clr.w	score3
		clr.w	out1 ; reset the game
		clr.w	out2
		clr.w	count
		clr.w	game_fin
		
; ------------------------------< MAIN LOOP >-------------------------------
main_loop	bsr	halt
		;bsr	swap_screen
		lea	sprite_structure,a0		 
		bsr	mask_sprites
		lea	sprite_structure,a0
		bsr	tasking
		lea	sprite_structure,a0
		bsr	blit_sprites
		bsr	swap_screen
		lea	hsprite_structures,a0
		bsr	tasking
		bsr	service_hardware
		bsr	monitor
		tst.w	game_fin	; Bodge flag to signal that the game is OVER
		bne.s	.end_game
		tst.b	key_esc
		beq.s	.no_escape
		move.w	#1,game_fin
.no_escape	tst.b	key_f09
		beq.s	main_loop
		clr.b	key_f09
		jsr	exit
; --------------------------------------------------------------------------
.end_game	clr.w	count
		lea	bak_colours,a1
		clear_pal 

.fade_menu	bsr	halt
		lea	bak_colours,a1
		bsr	fadepalin

		lea	day_pal,a0
		lea	bak_colours,a1
		move.w	#16,d4
		bsr	red1
		
		lea	day_pal,a0
		new_cop_colours
			
		add.w	#1,count
		cmp.w	#120,count
		blt	.fade_menu
		clr.w	count
		
		move.l	#intro_copperlist,copper
		jsr	swap_screen
		change_copper
		bsr	halt
		
		lea	day_palst,a0
		new_cop_colours
	
		lea	day_palst,a0
		lea	day_pal,a1
		rept	14
		move.w	(a0)+,(a1)+
		endr
			
		bra	the_code ; Restart program.

counter		dc.w	10*3000	; Delay counter

day_palst	dc.w	$8ae,$79d,$68c,$57b,$46a,$359,$248
night_palst	dc.w	$000,$002,$003,$004,$006,$016,$026

day_pal		dc.w	$8ae,$79d,$68c,$57b,$46a,$359,$248
night_pal	dc.w	$000,$002,$003,$004,$006,$016,$026
		ds.w	4

; --------------------------------< TASKS >--------------------------------

monitor		tst.w	timerQuest
		bne	max_score
		tst.w	counter
		beq	end
		sub.w	#1,counter
		rts
		
end		clr.l	d0
		clr.l	d1
		clr.l	d2
		move.w	score1,d0
		move.w	score2,d1
		move.w	score3,d2
		cmp.w	d0,d1
		bgt	.2up_u_loose
				
.1up_u_loose	move.w	#1,out1
		bra	.p2vp3
.2up_u_loose	move.w	#1,out2
.p1vp3		tst.w	out3
		bne	choose
		cmp.w	d0,d2
		bpl	.3up_u_loose
		move.w	#1,out1
		bra	choose
		
.3up_u_loose	move.w	#1,out3
		bra	choose
.p2vp3		tst.w	out3
		bne	choose
		cmp.w	d1,d2
		bpl	.3up_u_loose2
		
		move.w	#1,out2
		bra	choose
.3up_u_loose2	move.w	#1,out3
		bra	choose

; --------------------------------------------------------------------------------		
		
max_score	move.w	time,d0
		move.w	score1,d1
		
		cmp.w	d0,d1
		blt.s	.okay1
		lea	plane_1,a0
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		
		move.l	#terminate,spr_task(a0)
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)
		move.w	#1,out1

.okay1		move.w	score2,d1
		cmp.w	d0,d1
		blt.s	.okay1a
		lea	plane_2,a0
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		
		move.l	#terminate,spr_task(a0)
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)
		move.w	#1,out2
	
.okay1a		move.w	score3,d1
		cmp.w	d0,d1
		blt.s	.okay3
		lea	plane_3,a0
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		
		move.l	#terminate,spr_task(a0)
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)
		move.w	#1,out3
.okay3
choose		tst.w	out2
		beq.s	.okay2
		tst.w	out3
		beq.s	.okay2
		add.w	#1,count
		cmp.w	#150,count
		blt	.cont
		move.w	#1,game_fin	

.cont		lea	plane_1,a0
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		move.l	#terminate,spr_task(a0)
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		
		tst.w	spr_status(a0)
		beq.s	.bit0
.bit1		clr.w	spr_status(a0)
		clr.w	spr_status(a1)
		
		bra	.okay2
.bit0		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)

.okay2		tst.w	out1
		beq.s	.okay4
		tst.w	out3
		beq.s	.okay4

		add.w	#1,count
		cmp.w	#150,count
		blt	.cont2
		move.w	#1,game_fin	
.cont2
		lea	plane_2,a0
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		move.l	#terminate,spr_task(a0)
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		
		tst.w	spr_status(a0)
		beq.s	.bit02

.bit12		clr.w	spr_status(a0)
		clr.w	spr_status(a1)
		bra	.okay4

.bit02		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)

.okay4		tst.w	out1
		beq.s	.okay5
		tst.w	out2
		beq.s	.okay5

		add.w	#1,count
		cmp.w	#150,count
 		blt	.cont3
		move.w	#1,game_fin	
.cont3
		lea	plane_3,a0
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		move.l	#terminate,spr_task(a0)
		
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		tst.w	spr_status(a0)
		beq.s	.bit03
		
.bit13		clr.w	spr_status(a0)
		clr.w	spr_status(a1)	
		bra	.okay5

.bit03		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)
.okay5

		rts

; ---------------------------< REGISTERS >------------------------------------
		
game_fin	dc.w	0
out1		dc.w	0
out2		dc.w	0
out3		dc.w	0
out4		dc.w	0
out5		dc.w	0

; ---------------------------------< TASKS > ---------------------------------------

; A0.L = Address of sprite structure...

test_struct	move.l	#32768,spr_xvel(a0)

		cmp.w	#-16,spr_xpos(a0)
		bpl.s	.fine

		move.w	#334,spr_xpos(a0)
		
.fine		cmp.w	#336,spr_xpos(a0)
		bmi	.enif
		
		move.w	#-15,spr_xpos(a0)
.enif		
		rts

; -----------------------------< BLIMP >----------------------------------
		
blimp		tst.w	spr_status(a0)
		beq	blimp_control ; if the Sprite is on then control it!
		
		move.w	#87*16,spr_type(a0)
		cmp.w	#4,seed
		not	seed
		blt.s	.left
		bgt.s	.right

.centre		clr.l	horiz
		bra.s	.random
.left		move.l	#-4096,horiz
		bra.s	.random
.right		move.l	#4096,horiz

.random		clr.l	d0
		clr.l	d1
		move.w	seed,d0
		and.w	#7,d0
		
		move.w	score1,d1
		add.w	score2,d1
		add.w	score3,d1
		and.w	#7,d1
		cmp.w	d0,d1
		beq	start_blimp

		rts

blimp_control	add.l	#1,spr_long2(a0)
		lea	missile1,a1
		
		tst.w	blimp_hit
		bne.s	.no_hit
		
		tst.l	spr_status(a1)
		bne.s	.no_hit
		move.w	spr_xpos(a1),d0
		move.w	spr_ypos(a1),d1
		
		move.w	spr_xpos(a0),d2
		sub.w	#8,d2
		move.w	spr_ypos(a0),d3
		sub.w	#10,d3
		sub.w	d2,d0
		cmp.w	#32,d0
		
		bpl.s	.no_hit
		tst.w	d0
		bmi.s	.no_hit
		sub.w	d3,d1
		cmp.w	#10,d1
		
		bpl.s	.no_hit
		tst.w	d1
		bmi.s	.no_hit
		
		move.l	#1,spr_long1(a0)
		tst.w	score1
		beq.s	.no_zero
		sub.w	#1,score1
		move.w	#1,blimp_hit
.no_zero	
		move.w	#1,spr_status(a1)
		move.l	blimp_counter,d0
		not.w	d0
		lsr.w	#4,d0
		add.w	d0,seed
		add.w	#4,seed
		clr.w	hit_bottom		
.no_hit		
		lea	missile2,a1
		tst.w	blimp_hit
		bne.s	.no_hit2
		
		tst.l	spr_status(a1)
		bne.s	.no_hit2
		
		move.w	spr_xpos(a1),d0
		move.w	spr_ypos(a1),d1
		move.w	spr_xpos(a0),d2
		sub.w	#8,d2
		move.w	spr_ypos(a0),d3
		sub.w	#10,d3
		sub.w	d2,d0
		cmp.w	#32,d0
		bpl.s	.no_hit2
		tst.w	d0
		bmi.s	.no_hit2
		
		sub.w	d3,d1
		cmp.w	#10,d1
		
		bpl.s	.no_hit2
		tst.w	d1
		bmi.s	.no_hit2
		
		move.l	#1,spr_long1(a0)
		tst.w	score2
		beq.s	.no_zero2
		sub.w	#1,score2
		move.w	#1,blimp_hit
.no_zero2		
		move.w	#1,spr_status(a1)
		move.l	blimp_counter,d0
		not.w	d0
		lsr.w	#4,d0
		add.w	d0,seed
		add.w	#4,seed 
		clr.w	hit_bottom		

.no_hit2	lea	missile3,a1
		
		tst.w	blimp_hit
		bne.s	.no_hit3
		
		tst.l	spr_status(a1)
		bne.s	.no_hit3
		move.w	spr_xpos(a1),d0
		move.w	spr_ypos(a1),d1
		
		move.w	spr_xpos(a0),d2
		sub.w	#8,d2
		move.w	spr_ypos(a0),d3
		sub.w	#10,d3
		sub.w	d2,d0
		cmp.w	#32,d0
		
		bpl.s	.no_hit3
		tst.w	d0
		bmi.s	.no_hit3
		sub.w	d3,d1
		cmp.w	#10,d1
		
		bpl.s	.no_hit3
		tst.w	d1
		bmi.s	.no_hit3
		
		move.l	#1,spr_long1(a0)
		tst.w	score3
		beq.s	.no_zero3
		sub.w	#1,score3
		move.w	#1,blimp_hit
.no_zero3
		move.w	#1,spr_status(a1)
		move.l	blimp_counter,d0
		not.w	d0
		lsr.w	#4,d0
		add.w	d0,seed
		add.w	#4,seed
		clr.w	hit_bottom		
		

.no_hit3	tst.l	spr_long1(a0)
		bne	blow_blimp
		
		cmp.w	#130,spr_ypos(a0)
		bgt.s	.move_up
		
		tst.w	hit_bottom
		bne.s	.move_up
		
.move_down	move.l	#16384,spr_yvel(a0)
		move.l	horiz,spr_xvel(a0)

		bra	blow_blimp

.move_up	add.w	#1,hit_bottom
		clr.l	spr_yvel(a0)
		cmp.w	#25,hit_bottom
		blt	blow_blimp
		move.l	#-10000,spr_yvel(a0)
		cmp.w	#-8,spr_ypos(a0)
		bgt	blow_blimp
		
		move.w	#1,spr_status(a0)
		move.l	blimp_counter,d0
		not.w	d0
		
		add.w	d0,seed
		clr.w	hit_bottom
		move.w	#170,spr_xpos(a0)
		sub.w	#3,seed		
		
		
blow_blimp	tst.l	spr_long1(a0)
		beq	terminate
		bra	missle_fire ; bodge an explosion.

start_blimp	move.w	#87*16,spr_type(a0)
		clr.w	blimp_hit
		move.w	#160,spr_xpos(a0)
		clr.w	spr_status(a0)
		clr.w	spr_ypos(a0)
		
		rts

blimp_hit	dc.w	0				
seed		dc.w	0
blimp_counter	dc.l	0
hit_bottom	dc.w	0
horiz		dc.l	4096


term		rts ; used as a second exit branch as `Terminate` is now over 32K away

; ---------------------------< AEROPLANES >----------------------------------
boundarys	macro
		cmp.w	#-16,spr_xpos(a0) ; So that if the plane goes out off screen (X) then it is returned on the other side
		bpl	.okay1
		move.w	#334,spr_xpos(a0)
.okay1		cmp.w	#336,spr_xpos(a0)
		bmi	.okay2
		move.w	#-15,spr_xpos(a0)
.okay2		tst.w	gravQuest
		bne.s	.okay3
		cmp.w	#0,spr_ypos(a0)
		bpl	.no_top	
		clr.l	spr_yvel(a0)
		move.w	#0,spr_ypos(a0)
		bra.s	.no_top
.okay3		cmp.w	#-16,spr_ypos(a0)
		bpl	.no_top		
		clr.l	spr_yvel(a0)
		move.w	#1,spr_stall(a0)
		move.w	#-16,spr_ypos(a0)
.no_top
		endm
;---------------------------------------------------------------------------
missiles	macro
		tst.l	spr_long1(a0)
		bne	.no_hit\1
		lea	\1,a1
		tst.l	spr_status(a1)
		bne.s	.no_hit\1
		move.w	spr_xpos(a1),d0
		move.w	spr_ypos(a1),d1
		move.w	spr_xpos(a0),d2
		move.w	spr_ypos(a0),d3
		sub.w	d2,d0
		cmp.w	#16,d0
		bpl.s	.no_hit\1
		tst.w	d0
		bmi.s	.no_hit\1
		sub.w	d3,d1
		cmp.w	#16,d1
		bpl.s	.no_hit\1
		tst.w	d1
		bmi.s	.no_hit\1
		move.l	#1,spr_long1(a0)
		move.w	#1,spr_status(a1)
		clr.l	spr_yvel(a0)
		lea	\2,a2
		move.l	a2,spr_task(a0)
.no_hit\1
		endm
;------------------------------------------------------------------------------
planes		macro	
		lea	plane\1,a1
		tst.l	spr_status(a1)
		bne.s	.no_hitplane\1
		
		cmp.l	#takeoff\2,spr_task(a1)
		beq.s	.no_hitplane\1
		
		cmp.l	#takeoff_\2,spr_task(a1)
		beq.s	.no_hitplane\1

		move.w	spr_xpos(a1),d0
		move.w	spr_ypos(a1),d1
		
		move.w	spr_xpos(a0),d2
		move.w	spr_ypos(a0),d3
		
		sub.w	d2,d0
		cmp.w	#16,d0
		
		bpl.s	.no_hitplane\1
		tst.w	d0
		bmi.s	.no_hitplane\1
		sub.w	d3,d1
		cmp.w	#8,d1
		
		bpl.s	.no_hitplane\1
		tst.w	d1
		bmi.s	.no_hitplane\1
		
		move.l	#1,spr_long1(a0)
		move.w	#1,spr_long1(a1)
		
		clr.l	spr_yvel(a0)
		clr.l	spr_yvel(a1)
		lea	plane\3,a2
		move.l	a2,spr_task(a0)
		lea	plane\2,a2
		move.l	a2,spr_task(a1)
.no_hitplane\1		
		endm
;---------------------------------------------------------------------------

blimp		macro
		lea	the_blimp,a1
		tst.l	spr_status(a1)
		bne	.no_hitbl
		move.w	spr_xpos(a1),d0
		move.w	spr_ypos(a1),d1
		sub.w	#16,d1
	
		move.w	spr_xpos(a0),d2
		move.w	spr_ypos(a0),d3
		sub.w	#10,d3
		sub.w	d2,d0
		cmp.w	#32,d0
		bpl.s	.no_hitbl
		tst.w	d0
		bmi.s	.no_hitbl		
		sub.w	d3,d1
		cmp.w	#10,d1

		bpl.s	.no_hitbl
		tst.w	d1
		bmi.s	.no_hitbl
		
		clr.l	spr_yvel(a0)
		clr.l	spr_yvel(a1)
		lea	\1,a2
		move.l	a2,spr_task(a0)
		tst.l	spr_long1(a1)
		bne.s	.no_zero		
		tst.w	score2
		beq.s	.no_zero
		sub.w	#1,score2
.no_zero	
		move.l	#1,spr_long1(a0)
		move.w	#1,spr_long1(a1)
		move.l	blimp_counter,d0
		not.w	d0
		lsr.w	#4,d0
		add.w	d0,seed
		add.w	#4,seed
		clr.w	hit_bottom						
.no_hitbl	
		endm
;--------------------------------------------------------------------------------------		
croabh		macro
		tst.l	spr_long1(a0)
		bne.s	.no_croabh
		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#144,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
.no_croabh
		endm
; ------------------------------------------------------------------------------------
control		macro
		btst.b	#joy_up,\1
		beq.s	.no_up
		sub.w	#1,spr_clock(a0)
		tst.w	spr_clock(a0)
		bne	.no_up
		move.w	#4,spr_clock(a0)
		add.w	#1,spr_word1(a0)
		
.no_up		btst.b	#joy_down,\1 ; Rotate Anticlockwise
		beq.s	.no_down
		sub.w	#1,spr_anti(a0)
		tst.w	spr_anti(a0)
		bne	.no_down
		move.w	#4,spr_anti(a0)
		sub.w	#1,spr_word1(a0)

.no_down	btst.b	#joy_right,\1
		beq.s	.no_left
		sub.w	#3,spr_throttle(a0)
		tst.w	spr_throttle(a0)
		bmi	.no_left
		move.w	#3,spr_throttle(a0)
		cmp.w	#2,spr_vel(a0)
		beq	.no_left
		add.w	#1,spr_vel(a0)
	
.no_left	btst.b	#joy_left,\1
		beq.s	.no_right
		
		sub.w	#1,spr_throttle(a0)
		tst.w	spr_throttle(a0)
		bmi	.no_right
		move.w	#6,spr_throttle(a0)
		cmp.w	#0,spr_vel(a0)
		beq	.no_right
		sub.w	#1,spr_vel(a0)
		
.no_right	tst.l	spr_long1(a0)
		bne.s	.no_recover
		tst.w	spr_stall(a0)
		bne.s	.no_stall
		cmp.w	#1,spr_vel(a0)
		bmi.s	.no_stall	
		cmp.w	#9,spr_word1(a0)
		bmi.s	.no_stall
		tst.w	gravQuest
		beq.s	.no_stall
		clr.l	spr_yvel(a0)
		move.w	#1,spr_stall(a0)
.no_stall	cmp.w	#0,spr_vel(a0)
		bne.s	.no_recover
		cmp.w	#4,spr_word1(a0)
		bne.s	.no_recover
		clr.w	spr_stall(a0)
	
.no_recover	tst.w	spr_stall(a0)
		bne.s	.no_fire
		tst.l	spr_long1(a0) ; Can't Shoot if on Fire
		bne.s	.no_fire

		btst.b	#joy_fire,\1
		beq.s	.no_fire
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		lea	sprlist_len(a1),a1
		tst.w	spr_status(a1)
		beq.s	.no_fire
		clr.w	spr_status(a1)
		clr.l	spr_xvel(a1)
		clr.l	spr_yvel(a1)
		move.l	spr_xpos(a0),d0
		add.l	#16*65536,d0
		move.l	d0,spr_xpos(a1)
		move.l	spr_ypos(a0),d0
		move.l	d0,spr_ypos(a1)
.no_fire
		cmp.w	#16,spr_word1(a0)
		bne.s	.no_clear
		clr.w	spr_word1(a0)	
.no_clear	cmp.w	#-1,spr_word1(a0)
		bne.s	.no_minus
		move.w	#15,spr_word1(a0)		
.no_minus	tst.l	spr_long1(a0)
		bne	.okay4
		tst.w	gravQuest
		beq.s	.calc_speed
		tst.w	spr_stall(a0)
		bne	.okay4
.calc_speed	lea	sine_table,a1
		move.w	spr_word1(a0),d0
		lsl.w	#3,d0
		
		add.w	d0,a1
		move.l	(a1)+,d1
		move.w	spr_vel(a0),d3
		asr.l	d3,d1
		move.l	d1,spr_xvel(a0)		
		move.l	(a1),d2
		move.w	spr_vel(a0),d3
		asr.l	d3,d2
		move.l	d2,spr_yvel(a0)
.okay4		move.w	spr_word1(a0),d0
		lea	which_\4,a4
		mulu.w	#4,d0
		add.w	d0,a4
		move.l	a0,a5
		lea	sprlist_len(a5),a5
		
		move.w	(a4)+,spr_type(a0)
		move.w	(a4)+,spr_type(a5)
		tst.l	spr_long1(a0)
		bne.s	.grav
		tst.w	gravQuest
		beq.s	.floor
		tst.w	spr_stall(a0)
		bne.s	.grav
		tst.l	spr_yvel(a0)	
		beq.s	.floor
		bpl.s	.grav
		tst.l	spr_xvel(a0)
		bne.s	.floor
.grav		add.l	#512,spr_yvel(a0)
	
.floor		cmp.w	#188,spr_ypos(a0)
		bmi	term
		clr.l	spr_yvel(a0)
		clr.l	spr_xvel(a0)
		move.l	#1,spr_long1(a0)
		add.w	#1,spr_long2(a0)
		cmp.w	#150,spr_long2(a0)
		bmi	term
		clr.l	spr_long1(a0)
		add.w	#1,\2
		clr.l	spr_long2(a0)
		move.w	spr_xstart(a0),spr_xpos(a0)
		move.w	spr_ystart(a0),spr_ypos(a0)
		lea	\3,a1
		move.l	a1,spr_task(a0)
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		move.w	#start_sprite\5,spr_type(a0)
		move.w	#start_sprite\6,spr_type(a1)
		move.w	#\7,spr_word1(a0)
		clr.l	spr_long1(a0)
		clr.w	spr_stall(a0)
		move.w	#50,spr_vel(a0)
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a1)
		endm
;-----------------------------------------------------------------------------
start_sprite1a	equ	02*16
start_sprite1b	equ	10*16
start_sprite2a	equ	36*16
start_sprite2b	equ	43*16
start_sprite3a	equ	100*16
start_sprite3b	equ	108*16
start_sprite4a	equ	157*16
start_sprite4b	equ	165*16
start_sprite5a	equ	193*16
start_sprite5b	equ	200*16
; ---------------------------------------------------------------------------------------------------------------------------
plane1		boundarys
		missiles missile2,plane1
		missiles missile3,plane1
		missiles missile4,plane1
		missiles missile5,plane1
		planes   _2,2,1
		planes	 _3,3,1
		planes	 _4,4,1
		planes	 _5,5,1
		blimp	 plane1
		croabh
		control	 joyval5,score1,tower1,sps,1a,1b,0
		lea	runway1stack,a1
		tst.l	8(a1)
		beq.s	.ok2
		tst.l	(a1)
		beq.s	.ok1
		tst.l	4(a1)
		beq.s	.ok3
		jsr	go_asm
.ok3		move.l	8(a1),4(a1)
		clr.l	8(a1)
		bra.s	.ok2
.ok1		move.l	4(a1),(A1)
		move.l	8(a1),4(A1) 
		clr.l	8(a1)
.ok2		move.l	a0,8(a1)
		bra	tower1
		rts

; ---------------------------------------------------------------------------------------------------------------------------
plane2		boundarys
		missiles missile1,plane2
		missiles missile3,plane2
		missiles missile4,plane2
		missiles missile5,plane2
		planes	 _3,3,2
		planes	 _4,4,2
		planes	 _5,5,2
		blimp	 plane2
		croabh
		control	 joyval2,score2,tower2,sps2,2a,2b,8
		rts
; ---------------------------------------------------------------------------------------------------------------------------
plane3		boundarys
		missiles missile1,plane3
		missiles missile2,plane3
		missiles missile4,plane3
		missiles missile5,plane3
		planes	 _4,4,3
		planes	 _5,5,3
		blimp	 plane3
		croabh
		control	 joyval1,score3,tower3,sps3,3a,3b,0
		lea	runway1stack,a1
		tst.l	8(a1)
		beq.s	.ok2
		tst.l	(a1)
		beq.s	.ok1
		tst.l	4(a1)
		beq.s	.ok3
		jsr	go_asm
.ok3		move.l	8(a1),4(a1)
		clr.l	8(a1)
		bra.s	.ok2
.ok1		move.l	4(a1),(A1)
		move.l	8(a1),4(A1)
		clr.l	8(a1)
.ok2		move.l	a0,8(a1)
		bra	tower3
		rts
; ---------------------------------------------------------------------------------------------------------------------------		
plane4		boundarys
		missiles missile1,plane4
		missiles missile2,plane4
		missiles missile3,plane4
		missiles missile5,plane4
		planes	 _5,5,4
		blimp	 plane4
		croabh
		control	 joyval3,score4,tower4,sps4,4a,4b,0
		lea	runway1stack,a1
		tst.l	8(a1)
		beq.s	.ok2
		tst.l	(a1)
		beq.s	.ok1
		tst.l	4(a1)
		beq.s	.ok3
		jsr	go_asm
.ok3		move.l	8(a1),4(a1)
		clr.l	8(a1)
		bra.s	.ok2
.ok1		move.l	4(a1),(A1)
		move.l	8(a1),4(A1)
		clr.l	8(a1)
.ok2		move.l	a0,8(a1)
		bra	tower4
		rts
; ---------------------------------------------------------------------------------------------------------------------------				
plane5		boundarys
		missiles missile1,plane5
		missiles missile2,plane5
		missiles missile3,plane5
		missiles missile4,plane5
		blimp	 plane5
		croabh
		control	 joyval4,score5,tower5,sps5,5a,5b,8
		rts	
; ---------------------------< BEFORE TAKEOFF >------------------------------
control_tower	macro
		move.l	a0,a2
		lea	sprlist_len(a2),a2
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a2)
		tst.w	runway1clearQuest
		beq.s	.clear
		bne.s	.denied
.clear
		lea	runway1stack,a1
.cont		tst.l	(a1)
		beq.s	.rotate
		move.l	(a1),d0
		cmp.l	d0,a0
		bne.s	.denied
.permision_granted		
		move.w	#1,runway1clearQuest
		clr.l	(a1)
		move.l	4(a1),(a1)
		move.l	8(a1),4(a1)
		clr.l	8(a1)
		
		lea	\1,a1
		move.l	a1,spr_task(a0)	
.denied		rts

.rotate		move.l	4(a1),(a1)
		move.l	8(a1),4(a1)
		clr.l	8(a1)
		bra.s	.cont
		endm
		
delay		dc.w	0
runway1clearQuest	dc.w	1
runway1stack	dc.l	plane_3
		dc.l	plane_4
		dc.l	0
		even

tower1		control_tower	takeoff1
tower3		control_tower	takeoff3
tower4		control_tower	takeoff4


		
takeoff1	add.w	#1,delay
		cmp.w	#20,delay
		bne.s	.na
		move.l	a0,a2
		lea	sprlist_len(a2),a2
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a2)
		clr.w	spr_status(a0)
		clr.w	spr_status(a2)
		clr.w	delay
		lea	takeoff_1,a1
		move.l	a1,spr_task(a0)
.na		rts
takeoff_1	
.no_hit4	tst.l	spr_long1(a0)
		bne.s	.no_croabh
		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#144,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
		lea	plane1,a1
		move.l	a1,spr_task(a0)
		clr.w	runway1clearQuest
.no_croabh
		tst.b	key_p
		beq.s	.no_right
		move.w	#16*0,spr_type(a0)
		cmp.w	#3,spr_vel(a0)
		bmi	.okay
		move.w	#16*1,spr_type(a0)
		move.w	#3,spr_vel(a0)
.okay		add.w	#1,spr_long2(a0)
		cmp.w	#10,spr_long2(a0)
		bmi.s	.no_right
		clr.w	spr_long2(a0)
		tst.w	spr_vel(a0)
		beq.s	.no_right
		sub.w	#1,spr_vel(a0)

.no_right       tst.b	key_o
		beq.s	.no_left
		move.w	#16*2,spr_type(a0)
		cmp.w	#3,spr_vel(a0)
		bmi	.okay2
.okay2		add.w	#1,spr_long2(a0)
		cmp.w	#10,spr_long2(a0)
		bmi.s	.no_left
		clr.w	spr_long2(a0)
		tst.w	spr_vel(a0)
		beq.s	.no_left
		add.w	#1,spr_vel(a0)
.no_left
		tst.b	key_q
		beq.s	.no_up
		
		sub.w	#2,spr_ypos(a0)
		move.w	#3*16,(a0)
		move.w	#11*16,sprlist_len(a0)
		tst.w	gravQuest
		bne.s	.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		blt.s	.fart
		move.w	#4,spr_vel(a0)
		bra.s	.fart
.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		ble.s	.fart	
		move.w	#1,spr_stall(a0)
.fart		lea	plane1,a1
		move.l	a1,spr_task(a0)
		clr.w	runway1clearQuest
.no_up
.calc_speed	lea	sine_table,a1
		move.w	spr_word1(a0),d0
		lsl.w	#3,d0
		add.w	d0,a1
		move.l	(a1)+,d1
		move.w	spr_vel(a0),d3
		asr.l	d3,d1
		move.l	d1,spr_xvel(a0)
		move.l	(a1),d2
		move.w	spr_vel(a0),d3
		asr.l	d3,d2
		move.l	d2,spr_yvel(a0)
		rts
		
takeoff3	add.w	#1,delay
		cmp.w	#20,delay
		bne.s	.na
		move.l	a0,a2
		lea	sprlist_len(a2),a2
		clr.w	spr_status(a0)
		clr.w	spr_status(a2)
		clr.w	delay
		lea	takeoff_3,a1
		move.l	a1,spr_task(a0)
.na		rts
		
takeoff_3	
		btst.b	#joy_left,joyval1
		beq.s	.no_left
		move.w	#98*16,spr_type(a0)
		cmp.w	#3,spr_vel(a0)
		bmi	.okay
		move.w	#3,spr_vel(a0)
		move.w	#99*16,spr_type(a0)
.okay		
		add.w	#1,spr_long2(a0)
		cmp.w	#10,spr_long2(a0)
		bmi.s	.no_left
		clr.w	spr_long2(a0)
		tst.w	spr_vel(a0)
		beq.s	.no_left
		sub.w	#1,spr_vel(a0)
.no_left        tst.l	spr_long1(a0)
		bne.s	.no_croabh
		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#144,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
		lea	plane3,a1
		move.l	a1,spr_task(a0)
		clr.w	runway1clearQuest
.no_croabh
		btst.b	#joy_down,joyval1
		beq.s	.no_up
		sub.w	#2,spr_ypos(a0)
		move.w	#101*16,(a0)
		move.w	#109*16,sprlist_len(a0)
		tst.w	gravQuest
		bne.s	.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		blt.s	.fart
		move.w	#4,spr_vel(a0)
		bra.s	.fart
.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		ble.s	.fart		
		move.w	#1,spr_stall(a0)
.fart		lea	plane3,a1
		move.l	a1,spr_task(a0)
		clr.w	runway1clearQuest
.no_up	
.calc_speed	lea	sine_table,a1
		move.w	spr_word1(a0),d0
		lsl.w	#3,d0
		add.w	d0,a1
		move.l	(a1)+,d1
		move.w	spr_vel(a0),d3
		asr.l	d3,d1
		move.l	d1,spr_xvel(a0)
		move.l	(a1),d2
		move.w	spr_vel(a0),d3
		asr.l	d3,d2
		move.l	d2,spr_yvel(a0)	
		rts

; -----------------------------------------------------------------------------

takeoff4	add.w	#1,delay
		cmp.w	#20,delay
		bne.s	.na
		move.l	a0,a2
		lea	sprlist_len(a2),a2
		clr.w	spr_status(a0)
		clr.w	spr_status(a2)
		clr.w	delay
		lea	takeoff_4,a1
		move.l	a1,spr_task(a0)
.na		rts
		
takeoff_4	btst.b	#joy_left,joyval3
		beq.s	.no_left
		move.w	#157*16,spr_type(a0)
		cmp.w	#3,spr_vel(a0)
		bmi	.okay
		move.w	#3,spr_vel(a0)
		move.w	#158*16,spr_type(a0)
.okay		
		add.w	#1,spr_long2(a0)
		cmp.w	#10,spr_long2(a0)
		bmi.s	.no_left
		clr.w	spr_long2(a0)
		tst.w	spr_vel(a0)
		beq.s	.no_left
		sub.w	#1,spr_vel(a0)
.no_left        tst.l	spr_long1(a0)
		bne.s	.no_croabh
		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#144,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
		lea	plane4,a1
		move.l	a1,spr_task(a0)
		clr.w	runway1clearQuest
.no_croabh
		btst.b	#joy_down,joyval3
		beq.s	.no_up
		sub.w	#2,spr_ypos(a0)
		move.w	#160*16,(a0)
		move.w	#168*16,sprlist_len(a0)
		tst.w	gravQuest
		bne.s	.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		blt.s	.fart
		move.w	#4,spr_vel(a0)
		bra.s	.fart
.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		ble.s	.fart		
		move.w	#1,spr_stall(a0)
.fart		lea	plane4,a1
		move.l	a1,spr_task(a0)
		clr.w	runway1clearQuest
.no_up	
.calc_speed	lea	sine_table,a1
		move.w	spr_word1(a0),d0
		lsl.w	#3,d0
		add.w	d0,a1
		move.l	(a1)+,d1
		move.w	spr_vel(a0),d3
		asr.l	d3,d1
		move.l	d1,spr_xvel(a0)
		move.l	(a1),d2
		move.w	spr_vel(a0),d3
		asr.l	d3,d2
		move.l	d2,spr_yvel(a0)
		rts
; --------------------< RIGHT HAND RUNWAY LOGIC >--------------------------

takeoff_2
takeoff2	tst.l	spr_long1(a0)
		bne.s	.no_croabh
		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#144,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
		lea	plane2,a1
		move.l	a1,spr_task(a0)
.no_croabh
		btst.b	#joy_left,joyval2
		beq.s	.no_left
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		move.w	#41*16,spr_type(a1)
		cmp.w	#3,spr_vel(a0)
		bmi	.okay
		move.w	#42*16,spr_type(a1)	
		move.w	#3,spr_vel(a0)
.okay		
		add.w	#1,spr_long2(a0)
		cmp.w	#10,spr_long2(a0)
		bmi.s	.no_left
		clr.w	spr_long2(a0)
		tst.w	spr_vel(a0)
		beq.s	.no_left
		sub.w	#1,spr_vel(a0)
.no_left
	        btst.b	#joy_up,joyval2
		beq.s	.no_up
		sub.w	#1,spr_ypos(a0)
		move.w	#44*16,(a0)
		move.w	#51*16,sprlist_len(a0)
		tst.w	gravQuest
		bne.s	.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		blt.s	.fart
		move.w	#4,spr_vel(a0)
		bra.s	.fart
.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		ble.s	.fart
		move.w	#1,spr_stall(a0)
.fart		lea	plane2,a1
		move.l	a1,spr_task(a0)
.no_up	
.calc_speed	lea	sine_table,a1
		move.w	spr_word1(a0),d0
		lsl.w	#3,d0	
		add.w	d0,a1
		move.l	(a1)+,d1
		move.w	spr_vel(a0),d3
		asr.l	d3,d1
		move.l	d1,spr_xvel(a0)
		move.l	(a1),d2
		move.w	spr_vel(a0),d3
		asr.l	d3,d2
		move.l	d2,spr_yvel(a0)
		rts
; ---------------------------------------------------------------------------
takeoff_5
takeoff5	tst.l	spr_long1(a0)
		bne.s	.no_croabh
		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#144,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
		lea	plane5,a1
		move.l	a1,spr_task(a0)
.no_croabh
		btst.b	#joy_left,joyval4
		beq.s	.no_left
		move.l	a0,a1
		lea	sprlist_len(a1),a1
		move.w	#198*16,spr_type(a1)
		cmp.w	#3,spr_vel(a0)
		bmi	.okay
		move.w	#199*16,spr_type(a1)	
		move.w	#3,spr_vel(a0)
.okay		
		add.w	#1,spr_long2(a0)
		cmp.w	#10,spr_long2(a0)
		bmi.s	.no_left
		clr.w	spr_long2(a0)
		tst.w	spr_vel(a0)
		beq.s	.no_left
		sub.w	#1,spr_vel(a0)
.no_left
	        btst.b	#joy_up,joyval4
		beq.s	.no_up
		sub.w	#1,spr_ypos(a0)
		move.w	#16*157+16*44,(a0)
		move.w	#16*157+16*51,sprlist_len(a0)			
		tst.w	gravQuest
		bne.s	.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		blt.s	.fart
		move.w	#4,spr_vel(a0)
		bra.s	.fart
.fall_ya_bast
		cmp.w	#4,spr_vel(a0)
		ble.s	.fart
		move.w	#1,spr_stall(a0)
.fart		lea	plane5,a1
		move.l	a1,spr_task(a0)	
.no_up
.calc_speed	lea	sine_table,a1
		move.w	spr_word1(a0),d0
		lsl.w	#3,d0
		add.w	d0,a1
		move.l	(a1)+,d1
		move.w	spr_vel(a0),d3
		asr.l	d3,d1
		move.l	d1,spr_xvel(a0)
		move.l	(a1),d2
		move.w	spr_vel(a0),d3
		asr.l	d3,d2
		move.l	d2,spr_yvel(a0)
		rts
		
tower2		move.l	a0,a2
		lea	sprlist_len(a2),a2
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a2)
		lea	plane_5,a1
		cmp.l	#takeoff5,spr_task(a1)
		beq.s	.denied
		lea	plane_5,a4
		tst.w	spr_stall(a4)
		bne.s	.denied
.permision_granted		
		clr.w	spr_status(a0)
		clr.w	spr_status(a2)
		lea	takeoff2,a1
		move.l	a1,spr_task(a0)			
.denied		rts

tower5		move.l	a0,a2
		lea	sprlist_len(a2),a2
		move.w	#1,spr_status(a0)
		move.w	#1,spr_status(a2)
		lea	plane_2,a1
		cmp.l	#takeoff2,spr_task(a1)
		beq.s	.denied
		lea	plane_2,a4
		tst.w	spr_stall(a4)
		bne.s	.denied
.permision_granted		
		clr.w	spr_status(a0)
		clr.w	spr_status(a2)
		lea	takeoff5,a1
		move.l	a1,spr_task(a0)			
.denied		rts



; --------------------------< MISSILE STRUCTURE >----------------------------

missle_struct	move.w	seed,d0
		add.w	d0,blimp_counter
		tst.l	spr_long1(a0)
		bne	missle_fire
		move.l	a0,a1 ;Solution To 64 Pixel Wide Sprites
		sub.w	#2*sprlist_len,a1
		tst.w	spr_status(a0)
		bne	term
		tst.w	spr_xpos(a0)
		bpl	.okay1
		move.w	#1,spr_status(a0)
.okay1		cmp.w	#350,spr_xpos(a0)
		bmi	.okay2
		move.w	#1,spr_status(a0)
.okay2		tst.w	spr_ypos(a0)
		bpl.s	.okay3
		move.w	#1,spr_status(a0)	
.okay3		tst.l	spr_long1(a1)
		bne.s	.hit
		tst.w	spr_stall(a1)
		bne.s	.hit
		move.w	spr_word1(a1),d0
		lea	missiles,a4
		mulu.w	#10,d0
		add.w	d0,a4
		move.l	(a4)+,spr_xvel(a0)
		move.l	(a4)+,spr_yvel(a0)
		move.w	(a4)+,spr_type(a0)
.hit		move.w	spr_xpos(a0),d0
		move.w	spr_ypos(a0),d1
		sub.w	#160,d0
		tst.w	d0
		bmi.s	.no_croabh
		cmp.w	#32,d0
		bpl.s	.no_croabh
		cmp.w	#153,d1
		bmi.s	.no_croabh
		bra.s	.boom
.no_croabh	cmp.w	#185,spr_ypos(a0)
		bmi	term 
.boom		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		move.l	#1,spr_long1(a0)
		rts
; -------------------------------< CLOUDS >-----------------------------------

cloud1		move.l	#16384,spr_xvel(a0)
.fine		cmp.w	#351,spr_xpos(a0)
		bmi	.enif
		move.w	#-64,spr_xpos(a0)
.enif		rts
		
cloud2		move.l	#32768,spr_xvel(a0)
.fine		cmp.w	#351,spr_xpos(a0)
		bmi	.enif
		move.w	#-64,spr_xpos(a0)
.enif		rts
		

; ---------------------< EXTRA SPRITE HANDLERS >----------------------------

follow_pl	move.l	a0,a1 ;Solution To 64 Pixel Wide SpritesSs
		sub.w	#sprlist_len,a1
		move.l	spr_ypos(a1),d0
		move.l	d0,spr_ypos(a0)
		move.l	spr_xpos(a1),d0
		add.l	#32*65536,d0
		move.l	d0,spr_xpos(a0)
		rts
; -----------------------------------------------------------------------------		

follow_cloud	move.l	a0,a1 ;Solution To 64 Pixel Wide SpritesSs
		sub.w	#sprlist_len,a1
		clr.w	spr_status(a0)
		move.l	spr_ypos(a1),d0
		move.l	d0,spr_ypos(a0)
		move.l	spr_xpos(a1),d0
		add.l	#32*65536,d0
		move.l	d0,spr_xpos(a0)
		cmp.w	#353,spr_xpos(a0)
		bmi	term
		move.w	#1,spr_status(a0)	
		rts		
; ---------------------------< EXPLOSION >---------------------------------

explode		move.l	a0,a1 ;Solution To 64 Pixel Wide SpritesSs
		sub.w	#3*sprlist_len,a1
		tst.l	spr_long1(a1)
		beq	.no_marabh
		
		clr.w	spr_status(a0)
		sub.w	#1,spr_word1(a0)
 		bpl.s	.okay
		
		move.w	#4,spr_word1(a0)
		add.w	#16,spr_type(a0)
		cmp.w	#78*16,spr_type(a0)
		bne.s	.okay
	
		move.w	#75*16,spr_type(a0)
.okay		move.w	spr_xpos(a1),d0
		add.w	#16,d0
		move.w	d0,spr_xpos(a0)
		
		move.w	spr_ypos(a1),d0
		move.w	d0,spr_ypos(a0)
		rts
		
.no_marabh	move.w	#1,spr_status(a0)
		rts
		
; ---------------------------------------------------------------------

missle_fire	
		cmp.w	#79*16,spr_type(a0)
		bmi	.okay
		move.w	#75*16,spr_type(a0)
.okay		
		add.w	#1,spr_long2(a0)
		cmp.w	#4,spr_long2(a0)
		bmi	term
		clr.w	spr_long2(a0)
		add.w	#16,spr_type(a0)
		cmp.w	#78*16,spr_type(a0)
		bne	term
		move.w	#1,spr_status(a0)

		clr.l	spr_long1(a0)
		clr.l	spr_xvel(a0)
		clr.l	spr_yvel(a0)
		
		move.w	spr_xstart(a0),spr_xpos(a0)
		move.w	spr_ystart(a0),spr_ypos(a0)

		clr.w	spr_word2(a0)

		rts

; --------------------------< SCOREBOARDS >--------------------------------
score1		dc.w	0
score2		dc.w	0
score3		dc.w	0
score4		dc.w	0
score5		dc.w	0		
; ------------------------------------------------------------------------

; Graphixs tables
		
which_sps	dc.w	16*3
		dc.w	16*11
		
		dc.w	16*33
		dc.w	16*40
		
		dc.w	16*32
		dc.w	16*39
		
		dc.w	16*31
		dc.w	16*38
		
		dc.w	16*30
		dc.w	16*37
		
		dc.w	16*22
		dc.w	16*29
		
		dc.w	16*21
		dc.w	16*28
		
		dc.w	16*20
		dc.w	16*27
		
		dc.w	16*19
		dc.w	16*26
		
		dc.w	16*18
		dc.w	16*25
		
		dc.w	16*17
		dc.w	16*24
		
		dc.w	16*16
		dc.w	16*23
		
		dc.w	16*7
		dc.w	16*15
		
		dc.w	16*6
		dc.w	16*14
		
		dc.w	16*5
		dc.w	16*13
		
		dc.w	16*4
		dc.w	16*12

		
which_sps2	dc.w	16*59
		dc.w	16*66
		
		dc.w	16*60
		dc.w	16*67
		
		dc.w	16*61
		dc.w	16*68
		
		dc.w	16*62
		dc.w	16*69
		
		dc.w	16*63
		dc.w	16*70
		
		dc.w	16*64
		dc.w	16*71
		
		dc.w	16*72
		dc.w	16*78
		
		dc.w	16*73
		dc.w	16*79
		
		dc.w	16*44
		dc.w	16*51
		
		dc.w	16*45
		dc.w	16*52
		
		dc.w	16*46
		dc.w	16*53
		
		dc.w	16*47
		dc.w	16*54
		
		dc.w	16*48
		dc.w	16*55
		
		dc.w	16*49
		dc.w	16*56
		
		dc.w	16*50
		dc.w	16*57
		
		dc.w	16*58
		dc.w	16*65

which_sps3	dc.w	16*101
		dc.w	16*109
		
		dc.w	16*131
		dc.w	16*135
		
		dc.w	16*130
		dc.w	16*134
		
		dc.w	16*129
		dc.w	16*133
		
		dc.w	16*128
		dc.w	16*132
		
		dc.w	16*120
		dc.w	16*127
		
		dc.w	16*119
		dc.w	16*126
		
		dc.w	16*118
		dc.w	16*125
		
		dc.w	16*117
		dc.w	16*124
		
		dc.w	16*116
		dc.w	16*123
		
		dc.w	16*115
		dc.w	16*122
		
		dc.w	16*114
		dc.w	16*121
		
		dc.w	16*105
		dc.w	16*113
		
		dc.w	16*104
		dc.w	16*112
		
		dc.w	16*103
		dc.w	16*111
		
		dc.w	16*102
		dc.w	16*110

which_sps4	dc.w	16*157+16*3
		dc.w	16*157+16*11
		dc.w	16*157+16*33
		dc.w	16*157+16*40
		dc.w	16*157+16*32
		dc.w	16*157+16*39
		dc.w	16*157+16*31
		dc.w	16*157+16*38
		dc.w	16*157+16*30
		dc.w	16*157+16*37
		dc.w	16*157+16*22
		dc.w	16*157+16*29
		dc.w	16*157+16*21
		dc.w	16*157+16*28
		dc.w	16*157+16*20
		dc.w	16*157+16*27
		dc.w	16*157+16*19
		dc.w	16*157+16*26
		dc.w	16*157+16*18
		dc.w	16*157+16*25
		dc.w	16*157+16*17
		dc.w	16*157+16*24
		dc.w	16*157+16*16
		dc.w	16*157+16*23
		dc.w	16*157+16*7
		dc.w	16*157+16*15
		dc.w	16*157+16*6
		dc.w	16*157+16*14
		dc.w	16*157+16*5
		dc.w	16*157+16*13		
		dc.w	16*157+16*4
		dc.w	16*157+16*12

which_sps5	dc.w	16*157+16*59
		dc.w	16*157+16*66
		
		dc.w	16*157+16*60
		dc.w	16*157+16*67
		
		dc.w	16*157+16*61
		dc.w	16*157+16*68
		
		dc.w	16*157+16*62
		dc.w	16*157+16*69
		
		dc.w	16*157+16*63
		dc.w	16*157+16*70
		
		dc.w	16*157+16*64
		dc.w	16*157+16*71
		
		dc.w	16*157+16*72
		dc.w	16*157+16*74
		
		dc.w	16*157+16*73
		dc.w	16*157+16*75
		
		dc.w	16*157+16*44
		dc.w	16*157+16*51
		
		dc.w	16*157+16*45
		dc.w	16*157+16*52
		
		dc.w	16*157+16*46
		dc.w	16*157+16*53
		
		dc.w	16*157+16*47
		dc.w	16*157+16*54
		
		dc.w	16*157+16*48
		dc.w	16*157+16*55
		
		dc.w	16*157+16*49
		dc.w	16*157+16*56
		
		dc.w	16*157+16*50
		dc.w	16*157+16*57
		
		dc.w	16*157+16*58
		dc.w	16*157+16*65


missiles	dc.l	2*deg90
		dc.l	2*0
		dc.w	16*94
		
		dc.l	2*deg90
		dc.l	2*deg90
		dc.w	16*95
		
		dc.l	2*deg90
		dc.l	2*deg90
		dc.w	16*95

		dc.l	2*deg90
		dc.l	2*deg90
		dc.w	16*95

		dc.l	2*0
		dc.l	2*deg90
		dc.w	16*96

		dc.l	-2*deg90
		dc.l	2*deg90
		dc.w	16*97
		
		dc.l	-2*deg90
		dc.l	2*deg90
		dc.w	16*97
		
		dc.l	-2*deg90
		dc.l	2*deg90
		dc.w	16*97
		
		dc.l	-2*deg90 ;<
		dc.l	0
		dc.w	16*90
		
		dc.l	-2*deg90
		dc.l	-2*deg90
		dc.w	16*91
		
		dc.l	-2*deg90
		dc.l	-2*deg90
		dc.w	16*91
		
		dc.l	-2*deg90
		dc.l	-2*deg90
		dc.w	16*91
	
		dc.l	2*0
		dc.l	-2*deg90
		dc.w	16*92
		
		dc.l	2*deg90
		dc.l	-2*deg90
		dc.w	16*93
			
		dc.l	2*deg90
 		dc.l	-2*deg90
 		dc.w	16*93
		
		dc.l	2*deg90
		dc.l	-2*deg90
		dc.w	16*93
		
		
		
; ---------------------------< SPRITE STRUCTURES >-------------------------

sprite_structure

plane_1		dc.w	16*2
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	tower1
		dc.w	32
		dc.w	184
		dc.w	8
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
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
		
		dc.w	16*10 ;
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	follow_pl
		dc.w	32+32
		dc.w	184
		dc.w	0
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

missile1	dc.w	16*94 ; Missile Sprite
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	missle_struct
		dc.w	165
		dc.w	120
		dc.w	0
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	0
		dc.l	0
		dc.w	1
		dc.w	6
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
		
		dc.w	16*75 ; Explosion Sprite
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	explode
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
				
plane_2		dc.w	16*36
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	tower2
		dc.w	288
		dc.w	184
		dc.w	8
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	8
		dc.w	6
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*43
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	follow_pl
		dc.w	320
		dc.w	185
		dc.w	0
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
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

missile2	dc.w	16*95
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	missle_struct
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
		
		dc.w	16*75 ; Explosion Sprite
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	explode
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
		
plane_3		dc.w	16*100
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	tower3 ; terminate ; for 2 PLayer Game
		dc.w	32
		dc.w	184
		dc.w	8
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*108
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	follow_pl
		dc.w	32+32
		dc.w	185
		dc.w	0
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
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

missile3	dc.w	16*95
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	missle_struct
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
		
		dc.w	16*75 ; Explosion Sprite
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	explode
		dc.w	161
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
	
		
plane_4		dc.w	16*157
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	tower4 ; terminate ; for 2 PLayer Game
		dc.w	32
		dc.w	184
		dc.w	8
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*165
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	follow_pl
		dc.w	32+32
		dc.w	184
		dc.w	0
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
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

missile4	dc.w	16*95
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	missle_struct
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
		
		dc.w	16*75 ; Explosion Sprite
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	explode
		dc.w	161
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
			
plane_5		dc.w	16*193
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	tower5
		dc.w	288
		dc.w	184
		dc.w	8
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	8
		dc.w	6
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	160+32
		dc.w	120
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0
		dc.w	0

		dc.w	16*200
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	follow_pl
		dc.w	320
		dc.w	100
		dc.w	0
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	0
		dc.l	0
		dc.l	0
		dc.w	0
		dc.w	6
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

missile5	dc.w	16*95
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	missle_struct
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
		
		dc.w	16*75 ; Explosion Sprite
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	explode
		dc.w	165
		dc.w	120
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
		
		
the_blimp	dc.w	16*87
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	0
		dc.l	blimp
		dc.w	170
		dc.w	0
		dc.w	1
		dc.w	6
		dc.w	6
		dc.w	6
		dc.w	1
		dc.l	1
		dc.l	1
		dc.w	1
		dc.w	6
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
	
; ---------------------------< DIRECTION SPEEDS >-------------------------------

deg45		equ	59316  ;46341
deg45B		equ	1092*45

deg90		equ	65536+32768
deg90b		equ	65536

grav	equ	10000 ; 

sine_table	dc.l	deg90 ; Very Basic sine table
		dc.l	0
		dc.l	deg90
		dc.l	deg45
		dc.l	deg90
		dc.l	deg90
		dc.l	deg45
		dc.l	deg90
		dc.l	0
		dc.l	deg90
		dc.l	-deg45
		dc.l	deg90
		dc.l	-deg90
		dc.l	deg90
		dc.l	-deg90
		dc.l	deg45
		dc.l	-deg90 ;<
		dc.l	0
		dc.l	-deg90
		dc.l	-deg45
		dc.l	-deg45
		dc.l	-deg45
		dc.l	-deg45
		dc.l	-deg90b
		dc.l	0
		dc.l	-deg90b
		dc.l	deg45
		dc.l	-deg90b
		dc.l	deg45
 		dc.l	-deg45
		dc.l	deg90
		dc.l	-deg45
		
		dc.l	-1	; This don't do much I hope.

; -------------------------------< INCLUDES >------------------------------

hologenics_pic	incbin	gfx/hologenics.iff
presents_pic	incbin	gfx/presents.iff
title_pic	incbin	gfx/a2a3.lbm
credits_pic	incbin	gfx/credits.iff
credits_pic2	incbin	gfx/credits2.iff
menu_pic	incbin	gfx/intro.lbm

sprite_pic	incbin	gfx/sprites4.lbm ; 1st Sprite Pic
sprite_pic2	incbin	gfx/sprites5.lbm ; 2nd Sprite Pic
sprite_pic3	incbin	gfx/sprites6.lbm ; 3rd Sprite Pic
sprite_pic4	incbin  gfx/sprites8.lbm ; 4th sprite pic
sprite_pic5	incbin	gfx/sprites9.lbm ; 5th sprite pic
hardware_pic	incbin	gfx/sprites7.lbm ; Hardware Sprite Pic

block_pic	incbin	gfx/new_background.iff ; Day Background
block_pic2	incbin	gfx/new_background2.iff ; Night BackGround
map		incbin	maps/scr3.map ; Sequencial map

blocks1		ds.w	128*20*20 ; Somwhere to store your blocks
sprites		ds.b	200000 ; Somewhere to store your sprites
sprite_info_structure	
		ds.b	16*300	; Only room for 200 sprites...
hards		ds.w	3000
song2		incbin	music/air2air.mod
song1		incbin	music/air2air2.mod

; -------------------------------< SCREENS >-------------------------------
		
		ds.b	32*line_len ; buffer to protect my music
scr01DB		ds.b	16*line_len		; Primary Screen
scr01		ds.b	line_len*no_lines
		ds.b	16*line_len

scr02DB		ds.b	16*line_len		; Secondary Screen
scr02		ds.b	line_len*no_lines
		ds.b	16*line_len
		
scr03DB		ds.b	16*line_len		; Refresh Screen
scr03		ds.b	line_len*no_lines
		dc.l	0,0,0 ; PS what the hell is this for.

; ---------------------------------< END >-------------------------------
