; -------------------------< HARDWARE EQUATES >---------------------------------

; SJL - taken from one of the main source files

custom		equ	$dff000

dmaconr		equ	$002+custom
joy0dat		equ	$00a+custom
joy1dat		equ	$00c+custom
intenar		equ	$01c+custom
intreqr		equ	$01e+custom
bltcon0		equ	$040+custom
bltcon1		equ	$042+custom
bltafwm		equ	$044+custom
bltalwm		equ	$046+custom
bltcpth		equ	$048+custom
bltcptl		equ	$04a+custom
bltbpth		equ	$04c+custom
bltbptl		equ	$04e+custom
bltapth		equ	$050+custom
bltaptl		equ	$052+custom
bltdpth		equ	$054+custom
bltdptl		equ	$056+custom
bltsize		equ	$058+custom
bltcmod		equ	$060+custom
bltbmod		equ	$062+custom
bltamod		equ	$064+custom
bltdmod		equ	$066+custom
bltcdat		equ	$070+custom
bltbdat		equ	$072+custom
bltadat		equ	$074+custom
cop1lch		equ	$080+custom
cop1lcl		equ	$082+custom
cop2lch		equ	$084+custom
cop2lcl		equ	$086+custom
copjmp1		equ	$088+custom
copjmp2		equ	$08a+custom
diwstrt		equ	$08e+custom
diwstop		equ	$090+custom
ddfstrt		equ	$092+custom
ddfstop		equ	$094+custom
dmacon		equ	$096+custom
intena		equ	$09a+custom
intreq		equ	$09c+custom
bpl1pth		equ	$0e0+custom
bpl1ptl		equ	$0e2+custom
bpl2pth		equ	$0e4+custom
bpl2ptl		equ	$0e6+custom
bpl3pth		equ	$0e8+custom
bpl3ptl		equ	$0ea+custom
bpl4pth		equ	$0ec+custom
bpl4ptl		equ	$0ee+custom
bpl5pth		equ	$0f0+custom
bpl5ptl		equ	$0f2+custom
bpl6pth		equ	$0f4+custom
bpl6ptl		equ	$0f6+custom
bpl7pth		equ	$0f8+custom
bpl7ptl		equ	$0fa+custom
bpl8pth		equ	$0fc+custom
bpl8ptl		equ	$0fe+custom
bplcon0		equ	$100+custom
bplcon1		equ	$102+custom
bplcon2		equ	$104+custom
bplcon3		equ	$106+custom
bpl1mod		equ	$108+custom
bpl2mod		equ	$10a+custom
bplcon4		equ	$10C+custom
spr0pth		equ	$120+custom
spr0ptl		equ	$122+custom
spr1pth		equ	$124+custom
spr1ptl		equ	$126+custom
spr2pth		equ	$128+custom
spr2ptl		equ	$12a+custom
spr3pth		equ	$12c+custom
spr3ptl		equ	$12e+custom
spr4pth		equ	$130+custom
spr4ptl		equ	$132+custom
spr5pth		equ	$134+custom
spr5ptl		equ	$136+custom
spr6pth		equ	$138+custom
spr6ptl		equ	$13a+custom
spr7pth		equ	$13c+custom
spr7ptl		equ	$13e+custom
spr0pos		equ	$140+custom
spr0ctl		equ	$142+custom
spr0data	equ	$144+custom
spr0datb	equ	$146+custom
spr1pos		equ	$148+custom
spr1ctl		equ	$14a+custom
spr1data	equ	$14c+custom
spr1datb	equ	$14e+custom
spr2pos		equ	$150+custom
spr2ctl		equ	$152+custom
spr2data	equ	$154+custom
spr2datb	equ	$156+custom
spr3pos		equ	$158+custom
spr3ctl		equ	$15a+custom
spr3data	equ	$15c+custom
spr3datb	equ	$15e+custom
spr4pos		equ	$160+custom
spr4ctl		equ	$162+custom
spr4data	equ	$164+custom
spr4datb	equ	$166+custom
spr5pos		equ	$168+custom
spr5ctl		equ	$16a+custom
spr5data	equ	$16c+custom
spr5datb	equ	$16e+custom
spr6pos		equ	$170+custom
spr6ctl		equ	$172+custom
spr6data	equ	$174+custom
spr6datb	equ	$176+custom
spr7pos		equ	$178+custom
spr7ctl		equ	$17a+custom
spr7data	equ	$17c+custom
spr7datb	equ	$17e+custom
colour00	equ	$180+custom
colour01	equ	$182+custom
colour02	equ	$184+custom
colour03	equ	$186+custom
colour04	equ	$188+custom
colour05	equ	$18a+custom
colour06	equ	$18c+custom
colour07	equ	$18e+custom
colour08	equ	$190+custom
colour09	equ	$192+custom
colour10	equ	$194+custom
colour11	equ	$196+custom
colour12	equ	$198+custom
colour13	equ	$19a+custom
colour14	equ	$19c+custom
colour15	equ	$19e+custom
colour16	equ	$1a0+custom
colour17	equ	$1a2+custom
colour18	equ	$1a4+custom
colour19	equ	$1a6+custom
colour20	equ	$1a8+custom
colour21	equ	$1aa+custom
colour22	equ	$1ac+custom
colour23	equ	$1ae+custom
colour24	equ	$1b0+custom
colour25	equ	$1b2+custom
colour26	equ	$1b4+custom
colour27	equ	$1b6+custom
colour28	equ	$1b8+custom
colour29	equ	$1ba+custom
colour30	equ	$1bc+custom
colour31	equ	$1be+custom

diwhigh		equ	$1e4+custom
fmode		equ	$1fc+custom

aud0dat		equ	$0aa+custom
aud1dat		equ	$0ba+custom
aud2dat		equ	$0ca+custom
aud3dat		equ	$0da+custom