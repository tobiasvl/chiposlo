;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;                           C H I P O S
;
;  COMPACT HEXADECIMAL INTERPRETIVE PROGRAMMING AND OPERATING SYSTEM
;
;     DREAM-6800 OPERATING SYSTEM WITH CHIP8 LANGUAGE INTERPRETER 
;
;       ORIGINATED BY MICHAEL J BAUER, DEAKIN UNIVERSITY, 1978
;
;                  www.mjbauer.biz/DREAM6800.htm
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
; (1) UPON RELOCATION, THE DATA AT LOC $C133
;     MUST BE CHANGED ACCORDINGLY (SEE RANDOM)
; (2) IF DISPLAY AREA MOVED, THE DATA AT LOC $C227
;     IS CHANGED TO HIGH-ORDER BYTE OF BUFFER ADRS.
;
; SCRATCHPAD RAM ASSIGNMENTS (PAGE 0)
;

 processor 6803

IRQV	EQU		$0000		; INTERRUPT VECTOR
BEGA 	EQU		$0002		; BEGIN ADRS FOR LOAD/DUMP
ENDA 	EQU		$0004		; ENDING ADRS FOR LOAD/DUMP
ADRS 	EQU		$0006		; ADRS FOR GO AND MEMOD
;DDPAT 	EQU		$0008		; DIGIT PATTERN TEMP (5 BYTES)
VX		EQU		$000A		; VARIABLE X (ALSO X-COORD)
VY		EQU		$0009		; VARIABLE Y (ALSO Y-COORD)
RND		EQU		$000D		; RANDOM BYTE (SEED)
N		EQU 	$000E		; TEMP
ATEMP	EQU 	$000F		; TEMP
XTEMP 	EQU 	$0012		; 2-BYTE SAVE FOR X, SP
ZHI 	EQU 	$0014		; TEMP ADRS
ZLO		EQU		$0015		; 
KEYCOD	EQU		$0017		; KEYCODE TEMP
BADRED	EQU		$0018		; KEY BAD-READ FLAG
BLOC 	EQU		$001C		; DISPLAY POINTER (BYTE LOC'N)
PATNH 	EQU		$001E		; PATTERN TEMP
PATNL 	EQU		$001F		;
TIME 	EQU		$0020		; RTC TIMER VALUE
TONE 	EQU		$0021		; DURATION COUNT FOR TONE
PPC 	EQU		$0022		; PSEUDO PRGM-COUNTER
PSP 	EQU		$0024		; PSEUDO STACK-PTR
I		EQU		$0026		; CHIP8 MEMORY POINTER
PIR 	EQU		$0028		; PSEUDO INST-REG
VXLOC 	EQU		$002A		; POINTS TO VX
;RNDX 	EQU		$002C		; RANDOM POINTER
;VX		EQU		$002E		; VARIABLE X (ALSO X-COORD)
;VY		EQU		$002F		; VARIABLE Y (ALSO Y-COORD)
;
; CHIP8 VARIABLES (TABLE)
;
VO		EQU 	$0030
VF		EQU		$003F
RNDX 	EQU		$0043		; RANDOM POINTER
;
; CHIP8 SUBROUTINE STACK
;
STACK 	EQU 	$005F
;
; OPERATING-SYSTEM STACK
;
STOP 	EQU 	$007F		; STACK TOP (MONITOR)
DDPAT 	EQU		$0080		; DIGIT PATTERN TEMP (5 BYTES)
;
; CHIP8 GRAPHIC DISPLAY AREA
; ( 1/4K RAM BLOCK MAPPED ONTO T.V. SCREEN BY DMA.
; IN FORMAT 64X32 DOTS
;
DISBUF	EQU		$0100		; DISPLAY BUFFER AREA
ENDBUF	EQU		$0200
PIAA 	EQU 	$8010		; PORT-A FOR KEYPAD
PIAB 	EQU 	$8012		; PORT-B FOR TAPE, RTC, TONE
;
; CHIP8 INTERPRETER MAINLINE
;
		ORG 	 $C000
;
CHIP8:	BSR		ERASE		; NORMAL ENTRY POINT
		LDX		#$0200		; RESET PSEUDO-PC
		STX		PPC         
        LDX     #STACK		; RESET   STACK PTR
        STX     PSP
FETCH:	LDX    	PPC			; POINT TO NEXT INSTR
        LDX     0,X			; COPY TO PIR
        STX     PIR
        STX     ZHI			; SAVE ADRS (MMM)
        JSR		SKIP2		; BUMP PRGM-CTR
        LDAA   	ZHI			; MASK OFF ADRS
        ANDA  	#$0F
        STAA   	ZHI
        BSR     FINDV		; EXTRACT VX ALSO
        STAA   	VX          ; STASH VX
        STX     VXLOC       ; SAVE LOCATION OF VX
        LDAA   	PIR+1       ; FIND Y
        LSRA
        LSRA
        LSRA   
	LSRA
	BSR	FINDV		; EXTRACT VY
	STAA	VY			; STASH VY
EXEC:   LDX     #JUMTAB-2  	; POINT TO JUMP TABLE
        LDAA   	PIR         ; EXTRACT MSD
        ANDA  	#$F0
EXEl:   INX					; FlND ROUTINE ADRS
        INX
        SUBA  	#$10
        BCC     EXEl      	; BRANCH IF HIGHER OR SAME
	LDAA PIR+1 ; TODO
	;LDAB PIR+1 ; TODO
        LDX     0,X 		; LOAD ROUTINE ADRS
        JSR     0,X         ; PERFORM ROUTINE
        BRA     FETCH       ; NEXT INSTR...
FINDV:  LDX     #VO-1       ; POINT TO VARIABLES TABLE
FIND1:  INX                 ; FIND LOCN VX
        DECA
        BPL     FIND1
        LDAA    0,X         ; FETCH VX FROM TABLE
		RTS
;
; JUMP TABLE(ROUTINE ADDRESSES)
; 
JUMTAB:	.word     EXCALL      ; ERASE, RET, CALL, NOP
        .word     GOTO        ; GOTO MMM
        .word     DOSUB       ; DO MMM
        .word     SKFEK       ; SKF VX=KK
        .word     SKFNK       ; SKF VX#KK
        .word     SKFEV       ; SKF VX=VY
        .word     PUTVX;LETK        ; Vx=KK
        .word     LETVK       ; VX=VX+KK
        .word     LETVV       ; VX=[VX][+-&!]VY
        .word     SKFNV       ; SKF  VX#VY
        .word     LETI        ; I=MMM
	.word  	GOTOV       ; GOTO MMM+VO
        .word     RANDV       ; VX-RND.KK
        .word  	SHOW        ; SHOW  N@VX, VY
        .word  	SKFKEY      ; SKF VX[=#]KEY
        .word  	MISC        ; (MINOR JUMP TABL)
;
; ERASE, RETURN, CALL (MLS), OR NOP INTRN:
;
EXCALL: LDAB 	PIR         ; GET INSTR REG
        BNE     CALL
        ;LDAA    PIR+1
        CMPA  	#$E0
        BEQ     ERASE
        CMPA  	#$EE
        BEQ     RETDO
        RTS             	; NOP, FETCH
ERASE:  CLRA                ; WRITE ZEROS TO SCREEN
        LDX 	#DISBUF     ; POTNT TO DISPLAY BUFF
FILL:   STAA 	0,X         ; FILL SCREEN WITH ACC-A
        INX 
        CPX  	#ENDBUF    	; DONE?
        BNE     FILL
        RTS
RETDO:  TSX                 ; SAVE REAL SP
        LDS     PSP
		PULA
        STAA  	PPC         ; PULL PPC
        PULA
        STAA   	PPC+1
        STS  	PSP         ; SAVE CHIP8 SP
        TXS                 ; RESTORE SP
        RTS
CALL:   LDX  	ZHI         ; GET OPRND ADRS(MMM)
        JMP    	0,X         ; PERFORM MLS
GOTOV:  LDAA   	VO          ; 16-BIT ADD VO TO ADRS
        CLRB
        ADDA   	ZLO
        STAA    ZLO
        ADCB   	ZHI
		STAB	ZHI
GOTO:	LDX     ZHI         ; MOVE ADRS TO PPC
        STX     PPC
        RTS                 ; FETCH
LETI:   LDX  	ZHI         ; MOVE ADRS TO MI PTR
        STX     I
        RTS                 ; FETCH
DOSUB:  TSX                 ; SAVE SP
        LDS     PSP
        LDAA  	PPC+1       ; PUSH PPC
        PSHA
        LDAA   	PPC
        PSHA
        STS  	PSP         ; SAVE CHIP SP
        TXS                 ; RESTORE REAL SP
        BRA  	GOTO        ; JUMP TO ADRS(MMM)
;
; CONDITIONAL SKIP ROUTINES
;
SKFEK:	;LDAA   	PIR+1		; GET KK
SKFEQ:  CMPA    VX
        BEQ 	SKIP2
	RTS
SKFEV:  LDAA 	VY         	; GET VY
        BRA     SKFEQ
SKFNV:  LDAA    VY
        BRA     SKFNE
SKIP2:  LDX  	PPC         ; ADD 2 TO PPC
        INX
        INX
        STX     PPC
        RTS
SKFKEY: JSR  	KEYINP   	; INTERROGATE KEYBOARD
        TST     BADRED      ; KEY DOWN?
        BEQ     SKFK1
	LDAA	PIR+1
        ;LDAB	#$A1        ; WHAT INSTRN?
        ;CMPB	PIR+1       ; SKF VX#KEY
	CMPA	#$A1
        BEQ     SKIP2
        RTS                	; NO KEY GO FETCH
SKFK1:  ;LDAB  	#$9E
        ;CMPB  	PIR+1       ; WHAT INSTRN?
	CMPA	#$9E
        BEQ     SKFEQ
        ;BRA     SKFNE
SKFNK:  ;LDAA   	PIR+1       ; GET KK
SKFNE:  CMPA   	VX
        BNE     SKIP2
        RTS
;
; ARITHMETIC/LOGIC ROUTINES
;
LETK:   ;LDAA  	PIR+1     	; GET KK
        ;BRA     PUTVX
LETVK:  ;LDAA    PIR+1
        ADDA    VX
        BRA     PUTVX
RANDV:  BSR  	RANDOM     	; GET RANDOM BYTE
	;.byte $14 ;NBA
        ANDA    PIR+1
        BRA     PUTVX

LETVV:  TAB
	CLR VF
	LDAA    VY
        ANDB  	#$0F        ; EXTRACT N
	BEQ PUTVX


	LDX #JUMP8-2
loop:
	INX
	INX
	DECB
	BNE loop
	LDX 0,X
	JSR 0,X
	BRA PUTVX

JUMP8:
	.word LETV1
	.word LETV2
	.word LETV3
	.word LETV4
	.word LETV5
	.word LETV6
	.word LETV7


LETV1:  
        ORAA  	VX         	; VX=VX!VY (OR)
	RTS
LETV2:  
        ANDA  	VX          ; VX=VX.VY
	RTS
LETV3:	
	EORA	VX
	RTS

INCVF:  INC VF
	RTS

	.word LETVE

LETV4:  
        ADDA    VX          ; VX=VX+VY
	BCS INCVF          ; VF=1(OVERFLOW)
	RTS
LETV5:  
	LDAA VX
        SUBA    VY			; VX=VX-VY
        BCC INCVF          ; NO PUT VF=l
	RTS
LETV6:	
	LSRA
	BCS INCVF          ; VF=1(OVERFLOW)
	RTS
LETV7:  
	SUBA VX
	BCC INCVF
	RTS
LETVE:	
	ASLA
	BCS INCVF
	RTS

PUTVX:  LDX     VXLOC       ; REPLACE VX
        STAA    0,X
        RTS

;
; RANDOM BYTE GENERATOR
; 
RANDOM: LDAA  	#$C0        ; HIGH-ORDER BYTE OF RNDX =
        STAA  	RNDX        ; =MSB OF CHIP8 START ADRS
        INC     RNDX+1
		LDX		RNDX		; POINT TO NEXT PROGRAM BYTE
        LDAA  	RND         ; GET SEED (LAST VALUE)
        ADDA  	0,X         ; MANGLE IT
        EORA    $FF,X  
        STAA  	RND			; STASH IT
        RTS
;
; JUMP TABLE FOR MISCELLANEOUS INSTRNS [FXZZ]
;
MINJMP:	.byte		$07			; VX=TIME
		.word		VTIME
        .byte     $0A       	; VX=KEY
        .word     VKEY
        .byte		$15       	; TIME=VX
        .word     TIMEV
        .byte		$18         ; TONE=VX
        .word 	TONEV
        .byte  	$1E         ; I=I+VX
        .word     LETIV     
        .byte		$29			; I=DSPL,VX
        .word     LETDSP
        .byte		$33   		; MI=DEQ,VX
        .word		LETDEQ
        .byte  	$55         ; MI=VO:VX
        .word     STORV
        .byte		$65         ; VO:VX=MI
        .word     LOADV
;
MISC:	LDX   	#MINJMP    	; POINT TO TABLE
        LDAB	#9			; DO 9 TIMES		
MIS1:   LDAA    0,X         ; GET TABLE OPCODE
        CMPA	PIR+1
        BEQ     MIS2
        INX
        INX
        INX
        DECB
		BNE    	MIS1
        JMP     START    	; BAD OPCODE, RETURN TO MON.
MIS2:   LDX    	1,X         ; GET ROUTINE ADRS FROM TABLE
        LDAA    VX          ; GET VX
        JMP     0,X         ; GO TO ROUTINE
VTIME:  LDAA    TIME
        BRA     PUTVX
VKEY:   JSR     GETKEY
        BRA     PUTVX
TIMEV:  STAA   	TIME
        RTS
TONEV:  TAB					; SET DURATION=VX
		JMP   	BTONE
LETIV:  CLRB                ; 16-BIT ADD VX TO I
        ADDA   	I+1
        STAA    I+1
        ADCB    I
        STAB    I
        RTS
;
; COPY COMPRESSED DIGIT PATTERN (FROM TABLE)
; TO  5-BYTE ARRAY (DDPAT), & SET I FOR 'SHOW',
;
LETDSP: LDX   	#HEXTAB-2	; POINT TO HEX DIGIT PATTERNS ,
        ANDA  	#$0F        ; ISOLATE LS DIGIT
LDSP1:  INX                 ; SEARCH TABLE.....
        INX
        DECA                ; (A=VX)
        BPL		LDSP1
        LDX  	0,X      	; MOVE PATNH
        STX     PATNH
        LDX  	#DDPAT    	; POINT PATTERN ARRAY(5)
        STX  	I           ; SET MI POINTER
        LDAB  	#5          ; DO 5 TIMES
LDSP5:  LDAA    PATNH
        ANDA  	#$E0        ; EXTRACT 3 BITS
        STAA    4,X
        DEX
        LDAA  	#3          ; DO 3 TIMES
LDSP3:  ROL  	PATNL       ; MOVE NEXT 3 BITS
        ROL		PATNH
        DECA 
	BNE  	LDSP3      	; CONT (3)....
        DECB 
	BNE  	LDSP5		; CONT (5)....
        RTS
;
; HEXADECIMAL DIGIT PATTERNS (3X5  MATRIX)
;
HEXTAB:	.word  	$F6DF     	; 0
        .word 	$4925       ; 1
        .word  	$F39F       ; 2
        .word  	$E79F       ; 3
        .word  	$3ED9       ; 4
        .word  	$E7CF       ; 5
        .word  	$F7CF       ; 6
        .word  	$249F       ; 7
        .word     $F7DF       ; 8      
        .word  	$E7DF       ; 9
        .word  	$B7DF       ; A
        .word     $D7DD       ; B
        .word     $F24F   	; C
        .word  	$D6DD       ; D
        .word  	$F3CF       ; E
        .word  	$934F       ; F
;
LETDEQ: LDX     I           ; GET MI POINTER
        LDAB  	#100        ; N=100
        BSR  	DECI        ; CALC 100'S DIGIT
        LDAB  	#10         ; N=10
        BSR  	DECI        ; CALC l0'S DIGIT
        LDAB  	#1
DECI:   STAB    N			
        CLRB
LDEQ1:  CMPA  	N          	; DO UNTIL A<N  ...
        BCS  	LDEQ2       ; BRANCH IF LOWER NOT SAME.
        INCB
        SUBA    N
        BRA     LDEQ1       ; END-DO...
LDEQ2:  STAB  	0,X         ; STASH
        INX                 ; FOR NEXT DIGIT
        RTS
STORV:  SEI                 ; KILL IRQ FOR DATA STACK
        STS  	XTEMP       ; SAVE SP
        LDS  	#VO-1       ; POINT TO VARIABLES TABLE
        LDX  	I           ; FOINT MI
        BRA  	MOVX        ; TRANSFER NB BYTES
LOADV:  SEI                 ; KILL IRQ
        STS     XTEMP
        LDS     I           ; POINT MI
		DES
        LDX  	#VO         ; POINT TO VO
MOVX:   LDAB  	VXLOC+1  	; CALC. X  (AS IN VX)
        ANDB  	#$0F        ; LOOP (X+l) TIMES.....
MOVX1:  PULA                ; GET NEXT V
        STAA   	0,X         ; COPY IT
        INX
		INC 	I+1         ; I=I+X+1(ASSUMES SAME PAGE)
        DECB
        BPL  	MOVX1       ; CONTINUE...
        LDS     XTEMP       ; RESTORE SP
        CLI		        	; RESTORE IRQ
        RTS
;
; DISPLAY ROUTINES 
;   
SHOW:   ;LDAB    PIR+1       ; GET N (OPCODE LSB)
	TAB
        CLR     VF          ; CLEAR OVERLAP FLAG
SHOWI:  LDX     I           ; POINT TO PATTERN BYTES
SHOWX:  LDAA  	#$01        ; SET DISPLAY ADRS MSB =
        STAA    BLOC        ; = DISBUF HIGH-ORDER BYTE.
        ANDB    #$0F        ; COMPUTE NO. OF BYTES (N)
        BNE     SHOW2       ; IF N=0, MAKE N=16
        LDAB    #16           
SHOW2:  PSHB                ; DO N TIMES,...,.
        STX	ZHI         	; SAVE MI POINTER
        LDAA    0,X         ; FETCH NEW PATTERN BYTE
        STAA    PATNH
        CLR     PATNL
        LDAB    VX          ; DETERMINE OFFSET BIT COUNT
        ANDB  	#7
SHOW3:  BEQ     SHOW4       ; SHIFT INTO MATCHING POS'N
        LSR     PATNH         
        ROR     PATNL
        DECB
        BNE     SHOW3
SHOW4:  LDAB    VX          ; GET X COORD
        BSR     DISLOC      ; FIND WHERE IS FIRST DISP BYTE
		LDAA    PATNH       
        BSR     SHOWUP
        LDAB   	VX
        ADDB    #8          ; FIND WHERE IS ADJACENT BYTE                
		BSR     DISLOC      
        LDAA    PATNL
        BSR     SHOWUP
        INC		VY
        LDX     ZHI         ; POINT NEXT PATTERN BYTE
        INX
        PULB
        DECB
        BNE    	SHOW2       ; CONT.....
        RTS
SHOWUP: TAB                 ; UPDATE DISPLAY BYTE
        EORB   	0,X         ; X-OR WITH EXISTING DISPLAY
        ORAA    0,X         ; OR ALSO FOR OVERLAP TEST
        STAB    0,X         ; STORE XORED BYTE
        CBA
        BEQ     SHOWR       ; XOR SAME AS OR ELSE....
        LDAA  	#1          ; SET OVERLAP FLAG (VF)
		STAA	VF
SHOWR:  RTS
;
; COMPUTE ADRS OF DISPLAY BYTE AT COORDS(B, VY):
;
DISLOC: LDAA    VY          ; FETCH Y COORD
		ANDA  	#$1F        ; MASK TO 5 BITS FOR WRAP-ROUN
        ASLA                ; LEFT JUSTIFY
        ASLA
        ASLA
        ANDB  	#$3F        ; MASK X COORD TO 6 BITS
        LSRB                ; DROP 3 LS BITS
        LSRB
        LSRB
        ABA                 ; BUILD BYTE
        STAA   	BLOC+1      ; DISP LOC'N LSB COMPLETED
        LDX     BLOC        ; POINT TO DISP BYTE AT (VX,VY)
        RTS
;
; KEYPAD ROUTINES 
;
PAINZ:  LDAB  	#$F0        ; INITIALIZE PORT
PAINV:  LDX     #PIAA       ; (ENTRY PT FOR INV. DDR)
        CLR     1,X         ; RESET & SELECT DDR
        STAB   	0,X         ; SET DATA DIRECTION
        LDAB  	#$06        ; SET O/P REG & SETUP CTRL
        STAB    1,X          
        CLR     0,X         ; OUTPUT ZEROS & RE5ET FLAGS
        RTS
;
; KEYPAD INPUT SERVICE ROUTINE
;
KEYINP: BSR     PAINZ       ; RESET KEYPAD PORT
        CLR     BADRED     	; RESET BAD-READ FLAG
		BSR		DEL333		; DELAY FOR DEBOUNCE
        LDAB    0,X         ; INPUT ROW DATA
        BSR     KBILD      	; FORM CODE BITS 0,1
        STAA    KEYCOD
        LDAB  	#$0F        ; SET DDR FOR...
        BSR     PAINV       ; INVERSE ROW/COL  DIR N
        LDAB    0,X         ; INPUT COLUM DATA
        LSRB                ; RIGHT JUSTIFY
        LSRB
        LSRB
        LSRB
        BSR     KBILD       ; FORM CODE BITS 2,3
        ASLA					
        ASLA
        ADDA    KEYCOD
        STAA   	KEYCOD     	; BUILD COMPLETE KEYCODE
        RTS
KBILD:  CMPB  	#$0F        ; CHECK KEY STATUS
        BNE     KBILD0		; KEY IS DOWN, GO DECODE IT
        STAB	BADRED     	; NO KEY, SET BAD-READ FLAG
KBILD0: LDAA	#-1
KBILD1: INCA                ; (A=RESULT)
        LSRB                ; SHIFT DATA BIT TO CARRY
        BCS     KBILD1      ; FOUND ZERO BIT ?
        RTS
;
; GETKEY WAIT FOR KEYDOWN, THEN INPUTS
;
GETKEY: STX  	XTEMP       ; SAVE X FOR CALLING ROUTINE
GETK1:  BSR     PAINZ       ; RESET PORT, CLEAR FLAGS
GETK2:  LDAA    1,X         ; INPUT STATUS (HEX KEY DOWN?)
        BMI     HEXKEY  	; YES FETCH IT IN
        ASLA               	; TRY CA2 FLAG
        BPL  	GETK2		; FN  XEY DOWN? (A<0?)
FNKEY:  TST     0,X         ; YES: RESET FLAG IN PIA
        BRA  	HEXK1       ; RETURN WITHOUT CODE
HEXKEY: BSR  	KEYINP      ; DECODE THE KEYPAD
        TST  	BADRED      ; WAS IT A BAD READ?
        BNE  	GETK1       ; YES, TRY  AGAIN
HEXK1:  BSR  	BLEEP       ; O.K. ACKNOWLEDGE
        LDX  	XTEMP       ; RESTORE CALLER'S X-REG
        RTS                 ; RETURN (WITH A<O FOR FN KEY)
;
; TONE GENERATING ROUTINES
;
BLEEP:  LDAB	#4
BTONE:  STAB  	TONE      	; SET DURATION (RTC CYCLES)
        LDAB  	#$41        ; TURN AUDIO ON 
        STAB   	PIAB
BTON1:  TST     TONE        ; WAIT FOR RTC TIME-OUT
        BNE     BTON1
        LDAB  	#1          ; TURN AUDIO OFF
        STAB  	PIAB
        RTS
;
; SOFTWARE DELAY ROUTINE FOR SERIAL I/O:
;
DEL333: BSR  	DEL167  	; DELAY FOR 3.33 MILLISEC
DEL167: PSHB
        LDAB  	#200        ; DELAY FOR 1.67 MILLISEC
DEL:    DECB
        NOP
        BNE     DEL
        PULB
        RTS
;
; TAPE INPUT/OUTPUT ROUTINES
; INlTIALIZE TAPE, TONE, RTC, & DMA
; A=$3F FOR DISPLAY/DMA ON; A=$37 FOR OFF:
;
PBINZ:  LDX     #PIAB
		LDAB  	#$3B       	; SELECT DDR (DMA ON)
        STAB    1,X
        LDAB  	#$7F     	; WRITE DDR
        STAB	0,X
        STAA	1,X 		; WRITE CTRL REG
        LDAB  	#1          ; OUTPUT FOR T0NE OFF, AND...
        STAB   	0,X         ; TAPE DATA-OUT HIGH (MARKING)
        RTS
;
; INPUT ONE BYTE FROM TAPE PORT
;
INBYT:  BSR  	XCHG       	; EXCHANGE X FOR PIA ADRS
IN1:    LDAA   	0,X
        BMI   	IN1         ; LOOK FOR START BIT
        BSR  	DEL167      ; DELAY HALF BIT-TIME (300BD)
        LDAB  	#9          ; DO 9 TIMES....
IN2:    SEC                	; ENSURE PB0 MARKING
        ROL  	0,X         ; INPUT & SHIFT NEXT BIT
        RORA                ; INTO ACC-A
        BSR  	DEL333      ; WAIT 1 BIT-TIME
        DECB
        BNE  	IN2       	; CONT....
        BRA     OUTX        ; RESTORE X AND RETURN
XCHG:   STX  	XTEMP       ; SAVE   X-REG
        LDX     #PIAB
        RTS
;
; OUTPUT ONE BYTE TO TAPE PORT 
;
OUTBYT: BSR     XCHG
        PSHA
        DEC  	0,X        	; RESET START BIT
        LDAB	#10         ; DO 10 TIMES....
OUT1:  	BSR  	DEL333      ; DELAY 1 BIT-TIME
        STAA   	0,X         ; NEXT BIT TO OUT LINE (PB0)
        SEC
        RORA
        DECB
        BNE  	OUT1     	; CONT....
        PULA                ; RESTORE A
OUTX:   LDX   	XTEMP      	; RESTORE X
        RTS
GETKEE: BRA  	GETKEY      ; FOR INTERLINKING
;
; TAPE LOAD AND DUMP ROUTINES
;
LODUMX: LDAA  	#$37        ; KILL DISPLAY (DMA OFF)
        BSR     PBINZ
        LDX  	BEGA        ; POINT TO FIRST LOAD/DUMP ADR
        RTS
DUMP:   BSR     LODUMX
DUMP1:  LDAA    0,X      	; FETCH RAM BYTE
        BSR     OUTBYT
        INX
        CPX		ENDA       	; (ENDA = LAST ADRS+1)
        BNE     DUMP1
        BRA     START
LOAD:   BSR     LODUMX
LOAD1:  BSR		INBYT
        STAA  	0,X         ; STASH BYTE IN RAM
        INX
        CPX  	ENDA        ; DONE?
        BNE  	LOAD1       ; CONT....
;      (BRA START)
;
; MONITOR ENTRY POINT
;
START:  LDS     #STOP       ; RESET SP TO TOP
        LDX   	#RTC        ; SETUP IRQ VECTOR FOR RTC
        STX     IRQV
        LDAA  	#$3F        ; SETUP I/O PORT: DISPLAY ON.
        BSR     PBINZ
        BSR     SHOADR     	; PROMPT
		CLI                 ; ENABLE RELATIVE TIME CLOCK
COMAND: BSR    	GETKEE      ; INPUT SOMETHING
        TSTA  
        BPL     INADRS      ; IF HEX, GET AN ADDRESS
        BSR		GETKEE 		; IF FN, GET A COMMAND
		ANDA	#3      
        BEQ     MEMOD       ; 0 = MEM0RY MODIFY
        DECA
		BEQ		LOAD    	; 1 = TAPE LOAD
        DECA
        BEQ		DUMP       	; 2 = TAPE DUMP
GO:     LDX     ADRS        ; FETCH ADRS FOR GO
        JMP     0,X
INADRS: BSR     BYT1        ; BUILD ADRS MS BYTE
        STAA   	ADRS
        BSR     BYTIN       ; INPUT & BUILD LSB
        STAA   	ADRS+1 
        BSR     SHOADR      ; DISPLAY RESULTANT ADRS
        BRA     COMAND
BYTIN:  BSR     GETKEE      ; INPUT 2 HEX DIGITS
BYT1:   ASLA                ; LEFT JUSTIFY FIRST DIGIT
        ASLA
        ASLA
        ASLA
        STAA	ATEMP     	; HOLD IT
        BSR     GETKEE     	; INPUT ANOTHER DIGIT
        ADDA    ATEMP       ; BUILD A BYTE
        RTS
;
; MEMORY MODIFY ROUTINE
;
MEMOD:  BSR  	SHOADR      ; SHOW CURRENT ADRS
        LDX     ADRS        ; SHOW DATA AT ADRS
        BSR     SHODAT      ; 
        BSR  	GETKEE      ; WAIT FOR INPUT
        TSTA
        BMI  	MEM1        ; FN KEY; NEXT ADRS
        BSR  	BYT1        ; HEX KEY; NEW DATA BYTE
        STAA  	0,X         ; DEPOSIT IT
MEM1:   INX
        STX     ADRS		; BUMP ADRS
        BRA     MEMOD
SHOADR: LDAA    #$10        ; SET CURSOR HOME POSITION
        BSR     CURS1
        LDX     #DISBUF+200	; ILLUMINATE LAST 7 ROWS
        LDAA  	#$FF
        JSR     FILL
        LDX     #ADRS       ; POINT TO ADRS MS BYTE
        BSR     SHODAT
        INX                 ; FOINT TO ADRS LS BYTE
        BSR     SHODAT
        BSR     CURSR       ; MOVE CURSOR RIGHT
	RTS
SHODAT: LDAA   	0,X         ; FETCH DATA @ X
SHOBYT  PSHA
        LSRA           		; ISOLATE MS DIGIT
        LSRA
        LSRA
        LSRA
        BSR   	DIGOUT    	; 5HOw  ONE dIGIT
        PULA
DIGOUT: STX     XTEMP      	; SAVE X
        JSR     LETDSP      ; POINT TO DIGIT PATTERN
        LDAB	#5          ; SHOW 5-BYTE PATTERN
        JSR     SHOWI
CURSR:  LDAA    #4        	; SHIFT CURSOR RIGHT 4 DOTS
        ADDA    VX
CURS1:  STAA   	VX          ; SET X COORD
        LDAA  	#$1A        ; SET Y COORD
        STAA	VY
        LDX     XTEMP       ; RESTORE X_REG
        RTS
;
; REAL TIME CLOCK INTERRUPT SERVICE ROUTINE
;
RTC:	DEC		TIME
        DEC     TONE
        TST  	PIAB       	; CLEAR IRQ FLAG IN PIA
        RTI
IRQ:	LDX		IRQV		; INDIRECT JUMP VIA IRQV
        JMP     0,X
		.byte     0
;
; RESTART AND INTERRUPT TRAPS
;
        .word     IRQ         ; (ALLOWS USER-WRITTEN ISR)
        .word     $0080       ; SWI ROUIINE AT $0080 (OPTION
        .word		$0083		; NMI ROUTINE AT $0083 (OPTION
        .word     START
		END
