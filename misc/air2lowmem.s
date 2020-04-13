 
; ----------------------------< HARDWARE EQUATES >---------------------------------
		include	boot:project1/sourcecode/what_key.s ; include file of all the key equates. AND NO I DID`NT TYPE THEM IN. I STOLE IT.
		include	boot:project1/sourcecode/hardware.i
		
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
		jmp	go
		ds.b	192*line_len
scr01DB		ds.b	16*line_len		; Primary Screen
scr01		ds.b	line_len*no_lines
		ds.b	16*line_len
	
scr02DB		ds.b	16*line_len		; Primary Screen
scr02		ds.b	line_len*no_lines
		ds.b	16*line_len
	
scr03DB		ds.b	16*line_len		; Refresh Screen
scr03		ds.b	line_len*no_lines
		ds.b	16*line_len		
		dc.l	0,0,0 ; PS what the hell is this for.
sprites		incbin	boot:project2/graphix/sprites_data.bin ; graphics 
sprite_info_structure	incbin	boot:project2/graphix/sprites_info.bin ; graphics information 
song1		incbin	boot:project2/music/air2air.mod
song2		incbin	boot:project2/music/air2air2.mod
blocks1		ds.w	128*397 ; Somwhere to store your blocks		
normal		equ	1
turbo		equ	2
insane		equ	4
are_you_fucking_crazy?	equ	8
speed		equ	turbo
grav_val	equ	speed*512
turn_delay	equ	5-speed
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

diskoff		macro ; Stops the disk motor
		move.b	#255,$bfd100 ; disk motor off bit7* AL.
		move.b	#255-8,$bfd100 ; select int disk bit 3 AL.
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



		lea	h_sprites,a0
		lea	cop_sprites,a1
		moveq.l	#16-1,d7
.loop		addq.w	#2,a1
		move.w	(a0)+,(a1)+
		dbra	d7,.loop
		
		move.l	#copperlist,cop1lch		; My copperlist.

		move.w	#%1000011111100000,dmacon	; DMA
		move.w	#%1100000000101000,intena	; Enable interupts.

		move.w	copjmp1,d0			; Go copper...

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
		bsr	set_screen
				
		move.l	4,a6
		move.l	296(a6),d0 = Processor
		move.l	62(a6),d1 = Amount of chipram
		move.l	78(a6),d2 = Base of fastram
		move.l	d0,machine
		move.l	d1,chipmem
		move.l	d2,fastmem
		btst.b	#1,$2f6
		beq.s	.music
		or.l	#2,machine
.music		
		tst.l	d0
		beq.s	.cheese
		or.l	#1,machine	
.cheese
		btst.b	#1,(machine)
		bne.s	.no_music
		jsr	jukebox 
		move.b	#6,mt_auxspeede
		move.b	#6,mt_auxspeedo
		lea	song1,a0
		jsr	mt_init
.no_music				
		jmp	the_code
		
.map		dc.w	0

; -------------------------------< BLITWAIT >-------------------------------

blitwait	macro
		btst.b	#14,dmaconr
.\1.blitwait	btst.b	#14,dmaconr
 		bne.s	.\1.blitwait
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

copperlist	dc.w	bplcon0-custom,%0100000000000000	; It's safe here.
hrz_shift	dc.w	bplcon1-custom,$FFFF
		dc.w	bplcon2-custom,%0000000000100000

cop_dfst	dc.w	ddfstrt-custom,$28	; No scroll thank you.
cop_dfsp	dc.w	ddfstop-custom,$d0
cop_dwst	dc.w	diwstrt-custom,56*256+128
cop_dwsp	dc.w	diwstop-custom,255*256+193

cop_mod1	dc.w	bpl1mod-custom,line_len-plane_len

cop_mod2	dc.w	bpl2mod-custom,line_len-plane_len

cop_screen	dc.w	bpl1ptl-custom,0		; Screen.
		dc.w	bpl1pth-custom,0
		dc.w	bpl2ptl-custom,0
		dc.w	bpl2pth-custom,0
		dc.w	bpl3ptl-custom,0
		dc.w	bpl3pth-custom,0
		dc.w	bpl4ptl-custom,0
		dc.w	bpl4pth-custom,0

cop_colours	dc.w	colour00-custom,0	; Perminat colours.
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

	

cop_sprites	dc.w	spr0pth-custom,0
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

		
; ------------------------< ROLL ON EFFECT HANDLER >---------------------------

ripsize		dc.w	20
ripplecon	

		move.w	ripst,d0
		move.w	d0,d1
		
		sub.w	#6,d1

		cmp.w	#256,d0
		blt.s	.singleinst
		
		move.w	#255,d1
		sub.w	#255,d0
		
.singleinst	
		
		mulu.w	#256,d0
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
.singleinst2
		mulu.w	#256,d0
		add.w	#1,d0
		
		mulu.w	#256,d1
		add.w	#1,d1
		
		move.w	d0,ripple_end
		move.w	d1,pal2
		
		add.l	#32768,ripst

		cmp.w	#312,ripst
		bne.s	quit

		move.w	#16,ripst

quit		rts
ripst		dc.l	16*65536

; -----------------------------< SWAP SCREEN >------------------------------

swap_screen	move.l	bak_lpt,d0		; Swaps LPT
		move.l	the_lpt,bak_lpt
		move.l  d0,the_lpt

		move.l	bak_screen,d0		; Swaps the...
		move.l	the_screen,bak_screen
		move.l  d0,the_screen		; Drop through with D0.L.

		lea	cop_screen,a1
		
; ------------------------------< SET SCREEN >------------------------------

; > D0.L = Address of screen to make.

set_screen	move.w	d0,2(a1)	; Updates the copper pointers...
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
		
		rts

; -----------------------------< SET COLOURS >------------------------------

; > A0.L = Source of colours.

set_colours	lea	cop_colours,a1		; Where to put colours...
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
	
		btst.b	#1,(machine)
		bne.s	.no_music
		bsr	mt_music
		bsr	jukebox
.no_music
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
		lea	cop_colours,a1
		moveq.l	#32-1,d0
.loop		addq.w	#2,a1
		move.w	(a0)+,(a1)+
		dbra	d0,.loop
		bsr	joysticks		; Create port1 and port2.
		bsr	mice
.continue	movem.l	(sp)+,a0-a7/d0-d7
		move.w	#%0000000000100000,intreq	; Clear pending bit.
		move.w	#%1000000000100000,intena	; Clear pending bit.
		rte

; ---------------------------------< HALT >---------------------------------

halt		clr.b	flag			; It's a spectrum...

.halt		tst.b	flag			; Sad.
		beq.s	.halt
		rts

; -------------------------------< JOYSTICK >-------------------------------

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

; -------------------------------< KEYBOARD >--------------------------------
keyboard	move.w	#8,$dff09a		; Disable interupts
		movem.l	d0-d2/a0,-(sp)
		move.b	$bfed01,d0		; Is it a keyboard interupt
		btst	#3,d0
		beq	keyintexit
		clr.l	d0
		move.b	$bfec01,d0		; Read keycode
		or.b	#$40,$bfee01
		move.b	d0,keycode
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

		move.w	#$000,cop_colours+2	; Make sure you can see text.
		move.w	#$fff,cop_colours+6
		move.w	#$000,colours+0
		move.w	#$fff,colours+2

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
back?		dc.w	1
right_width	equ	400


; -------------------------------< BLIT 16 >--------------------------------
mask16		move.w	spr_height(a5,d7),d5	; Height...
		move.l	spr_address(a5,d7),d7
;		add.l	#sprites,d7

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
	;	add.l	#sprites,d7
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
		;add.l	#sprites,d7

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
		;add.l	#sprites,d7
		
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


; D2 > Address of screen

de_block_cache
		
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
		blitwait 1
		blit_block
		blitwait 2
		blit_block
		blitwait 3
		blit_block
		blitwait 4
		blit_block
		blitwait 5
		blit_block
		blitwait 6
		blit_block
		blitwait 7
		blit_block
		blitwait 8
		blit_block
		blitwait 9
		blit_block
		blitwait a
		blit_block
		blitwait b
		blit_block
		blitwait c
		blit_block
		blitwait d
		blit_block
		blitwait e
		blit_block
		blitwait f
		blit_block
		blitwait g
		blit_block
		blitwait h
		blit_block
		blitwait i
		blit_block
		blitwait j
		blit_block
		blitwait k
		blit_block
		blitwait l
		blit_block
		blitwait m
		add.l	#16*line_len-38,d2
		sub.l	#6,d2
		dbra	d1,.line
		rts

; -------------------------------------------------------------------------
de_block	btst.b	#0,(machine)
		bne	de_block_cache

de_block_nocache	; De_block for the A500 and A600

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

; ---------------------------------< DATA >---------------------------------

colours		dc.w	$fff,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000

bak_colours 	dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000
		dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff
		dc.w	$000,$000,$000,$000,$000,$000,$000,$000

extra_pal	dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff ; extra pallette for HW sprites
		dc.w	$000,$000,$888,$fff,$000,$000,$888,$fff



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
memory_used	dc.l	0	; Amount of memory used by the GSprite.
sprites_avail	dc.l	0	; Number of Available sprites.
machine		dc.l	0	; What Version of processor are we running.
chipmem		dc.l	0	; How much Chip memory does this machine have.
fastmem		dc.l	0	; How much FAST memory does this machine have (If any).

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
		dc.b	"h\\\\\\\\\\\\\\\\\\\d",cr
		dc.b	"h^^^PC^^^^^^^^^^^^^^d",cr
		dc.b	"h\\\\\\\\\\\\\\\\\\\d",cr
		dc.b	"h^^^D0^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D1^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D2^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D3^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D4^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D5^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D6^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^D7^^^^^^^^^^^^^^d",cr
		dc.b	"h\\\\\\\\\\\\\\\\\\\d",cr
		dc.b	"h^^^A0^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A1^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A2^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A3^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A4^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A5^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A6^^^^^^^^^^^^^^d",cr
		dc.b	"h^^^A7^^^^^^^^^^^^^^d",cr
		dc.b	"h\\\\\\\\\\\\\\\\\\\d",cr
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


font		incbin	fonts:sys_font.bin

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
; --------------------------< JUKEBOX SYSTEM >-------------------------------
kill_audio	macro
		move.w	#%0000000000001111,dmacon
		clr.w	aud0dat
		clr.w	aud1dat
		clr.w	aud2dat
		clr.w	aud3dat
		endm
music_flag	dc.w	0
jukebox		btst.b	#1,(machine)
		bne	.exit
		lea	playing_list,a1
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

		include	boot:project1/sourcecode/replay.s ; music player
		
; -------------------------------< THAT'S IT >------------------------------
the_code
	bsr	halt
	lea	vector,a1
	move.l	the_screen,a0
	move.w	(a1)+,d0 ; lets get our vector matrix
	move.w	(a1)+,d1
	move.w	(a1)+,d2
	move.w	(a1)+,d3
	move.l	#44,d4
	
	sub.w	d0,d2 ; dx
	bmi.s	.xneg
	sub.w	d1,d3 ; dy
	bmi.s	.yneg
	cmp.w	d3,d2
	bmi.s	.ygtx
	moveq.l	#octant1+linemode,d5
	bra.s	.lineagain
.ygtx
	exg	d2,d3
	moveq.l	#octant2+linemode,d5
	bra.s	.lineagain
.yneg	
	neg.w	d3
	cmp.w	d3,d2
	bmi.s	.ynygtx
	moveq.l	#octant8+linemode,d5
	bra.s	.lineagain
.ynygtx
	exg	d2,d3
	moveq.l	#octant7+linemode,d5
	bra.s	.lineagain
.xneg
	neg.w	d2
	sub.w	d1,d3
	bmi.s	.xyneg
	cmp.w	d3,d2	
	bmi.s	.xnygtx
	moveq.l	#octant4+linemode,d5
	bra.s	.lineagain
.xnygtx
	exg	d2,d3
	moveq.l	#octant3+linemode,d5
	bra.s	.lineagain
.xyneg	
	neg.w	d3
	cmp.w	d3,d2
	bmi.s	.xynygtx
	moveq.l	#octant5+linemode,d5
	bra.s	.lineagain
.xynygtx	
	exg	d2,d3
	moveq.l	#octant6+linemode,d5
.lineagain
	mulu.w	d4,d1
	ror.l	#4,d0
	add.w	d0,d0
	add.l	d1,a0
	add.w	d0,a0
	swap	d0
	or.w	#$bfa,d0
	lsl.l	#2,d3
	add.w	d2,d2
	move.w	d2,d1
	lsl.w	#5,d1
	add.w	#$42,d1
	blitwait
	move.w	d3,bltbmod
	sub.w	d2,d3
	ext.l	d3
	move.l	d3,bltapth
	bpl.s	.lineover
	or.w	#$40,d5
.lineover
	move.w	d0,bltcon0
	move.w	d5,bltcon1
	move.w	d4,bltcmod
	move.w	d4,bltcmod
	sub.w	d2,d3
	move.w	d3,bltamod
	move.w	#$8000,bltadat
	moveq.l	#-1,d5
	move.l	d5,bltafwm
	move.l	a0,bltcpth
	move.l	a0,bltdpth
	move.w	d1,bltsize ; go.....
	
main_loop
.no_escape	tst.b	key_f09
		beq.s	main_loop
		clr.b	key_f09
		jsr	exit

sprite_struncture	
		dc.w	-1

; ---------------------------< OBJECTS >----------------------------------

vector		dc.w	5,7
		dc.w	50,6
; --------------------------------------------------------------------------
sprite_structure	
		dc.w	-1
; -------------------------------< INCLUDES >------------------------------
hologenics_pic	incbin	boot:project2/encoded_data/holoscram.iff
presents_pic	incbin	boot:project2/encoded_data/presscram.iff
title_pic	incbin	boot:project2/encoded_data/titlescram.iff
credits_pic	incbin	boot:project2/encoded_data/cred1scram.iff
credits_pic2	incbin	boot:project2/encoded_data/cred2scram.iff
menu_pic	incbin	boot:project2/encoded_data/introscram.iff
block_pic	incbin	boot:project2/encoded_data/back1scram.iff ; Day Background
block_pic2	incbin	boot:project2/encoded_data/back2scram.iff ; Night BackGround
map		incbin	boot:project2/maps/scr3.map ; Sequencial map

hards		incbin	boot:project2/graphix/hards_data.bin
hard_info_structure	incbin boot:project2/graphix/hards_info.bin ; 8 Hardware sprites
end_of_sourcecode	ds.b	2000

;---------------------------------< END >---------------------------------
