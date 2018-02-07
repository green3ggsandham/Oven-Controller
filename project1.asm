; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 1000    ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BAUD equ 115200
BRG_VAL equ (0x100-(CLK/(16*BAUD)))


;BUTTONS HERE
BOOT_BUTTON   equ P4.5
ADD_ONE       equ p0.1
SOUND_OUT 	  equ p4.4
NEXT		  equ p2.4




; Reset vector
org 0x0000
    ljmp main
; External interrupt 0 vector (not used in this code)
org 0x0003
	reti
; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
; External interrupt 1 vector (not used in this code)
org 0x0013
	reti
; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti
; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	
	
;VARIABLES HERE	
; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
second:		  ds 1
soaktemp:	  ds 2
soaktime:	  ds 2
reflowtemp:	  ds 2
reflowtime:	  ds 2
x: ds 4
y: ds 4
bcd: ds 5
result: ds 4
hun: ds 1
hun1: ds 1
hun2: ds 1
hun3: ds 1
hunsec: ds 1
switch: ds 1
currenttemp: ds 1
pwm:		  ds 2
state: ds 1
statealarm: ds 1



;FLAGS
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
no_alarm: dbit 0
mf: dbit 1



cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5


CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3


$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$include(macro1.inc)
$LIST

;add menu display messages here!
stemp: 					db 'SOAK TEMP:  xxxC',0
stime:					db 'SOAK TIME:  xxxs',0
rtemp:					db 'REFLOW TEMP xxxC',0
rtime:					db 'REFLOW TIME xxxs',0
selectyes:			    db '  > yes <  no   ',0
selectno:			    db '    yes  > no < ',0
save:					db '  SAVE CHANGES? ',0
clear:					db '                ',0
otemp:					db '   xxxC  xxxs   ',0
state1dis:				db '  RAMP TO SOAK  ',0
state2dis:				db '      SOAK      ',0
state3dis:				db ' RAMP TO REFLOW ',0 
state4dis:				db '     REFLOW     ',0
state5dis:				db '      COOL      ',0
coolenough:				db ' YOUVE GOT ONE  ',0
coolenough1:			db '    COOL BOY    ',0


;SET UP TIMERS!!!!!!!!!!!!!! (LAB 2)
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    ;setb TR0  ; Start timer 
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	clr TF0  ; According to the data sheet this is done for us already.
	jnb no_alarm, don
	cpl SOUND_OUT ; Connect speaker to P3.7!
	lcall delay
	clr SOUND_OUT
clr no_alarm
	don:
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	push acc
	push psw
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
	
Inc_Done:
	; Check if half second has passed
	clr c
	
	mov a, count1ms+0
	subb a, pwm+0
	mov a, count1ms+1
	subb a, pwm+1
	mov P0.3, c
	
	
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	increment1(second, hunsec, #1)
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a	
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
;;GET TEMPERATURE DATA (LAB 3)							
INIT_SPI:
 setb MY_MISO ; Make MISO an input pin
 clr MY_SCLK ; For mode (0,0) SCLK is zero
 ret

DO_SPI_G:
 push acc
 mov R1, #0 ; Received byte stored in R1
 mov R2, #8 ; Loop counter (8-bits)
 
DO_SPI_G_LOOP:
 mov a, R0 ; Byte to write is in R0
 rlc a ; Carry flag has bit to write
 mov R0, a
 mov MY_MOSI, c
 setb MY_SCLK ; Transmit
 mov c, MY_MISO ; Read received bit
 mov a, R1 ; Save received bit in R1
 rlc a
 mov R1, a
 clr MY_SCLK
 djnz R2, DO_SPI_G_LOOP
 pop acc
 ret
 
 
delay:
	Wait_Milli_Seconds(#190) 
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
    
Do_Something_With_Result:

	;CONVERT TO TEMPERATURE
	mov x, Result
	mov x+1, Result+1
	mov x+2, #0
	mov x+3, #0
	load_y(45)
	lcall mul32
	load_y(100)
	lcall div32
	load_y(26)
	lcall add32
	;load_y(7016)
	;lcall sub32
	mov a, x
	da a
	lcall hex2bcd
	
	;LCD DISPLAY
	Set_Cursor(1, 3)
	Display_BCD(bcd+1)					
	Set_Cursor(1, 5)
	Display_BCD(bcd)

	
	;PUTTY DISPLAY
	mov a, bcd+1
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	mov a, bcd+1
	anl a, #0fh
	orl a, #30h
	lcall putchar
	mov a, bcd+0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	mov a, bcd+0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
ret


beep:
	setb SOUND_OUT
    lcall delay
    lcall delay
    clr SOUND_OUT
    lcall delay
    ret
  

main:
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    lcall LCD_4BIT
    mov P0M0, #0
    mov P0M1, #0
    setb EA 
    setb half_seconds_flag
    mov soaktemp, #0x30
    mov soaktime, #0
    mov reflowtemp, #0
    mov reflowtime, #0
    mov hun, #0
    mov hun1, #0
    mov hun2, #0
    mov hun3, #0
    mov pwm+0, #low(0) 		;initialize pwm to 0% 
	mov pwm+1, #high(0)
	mov state, #0x1
    mov statealarm, #1
    clr no_alarm
    clr SOUND_OUT
    
    
    ;SET SOAK TEMPERATURE
	redo:
	Set_Cursor(1,1)
	Send_Constant_String(#clear)
	Set_Cursor(2,1)
	Send_Constant_String(#clear)
    Set_Cursor(1,1)
    Send_Constant_String(#stemp)
    setsoaktemp:
    buttonpress(ADD_ONE, scroll)
    increment5(soaktemp,hun,#5)
    scroll:
    displayvariable(14,soaktemp,13,hun)
    buttonpress(NEXT,setsoaktemp)
    
    
    ;SET SOAK TIME
    Set_Cursor(1,1)
    Send_Constant_String(#stime)
    setsoaktime:
	buttonpress(ADD_ONE, scroll1)
    increment5(soaktime, hun1,#5)
    scroll1:
    displayvariable(14,soaktime,13,hun1)
    buttonpress(NEXT,setsoaktime)
    
    
    ;SET REFLOW TEMPERATURE
    Set_Cursor(1,1)
    Send_Constant_String(#rtemp)
    setreflowtemp:
	buttonpress(ADD_ONE, scroll2)
    increment5(reflowtemp, hun2,#5)
    scroll2:
    displayvariable(14,reflowtemp,13,hun2)
    buttonpress(NEXT,setreflowtemp)
    
    
    ;SET REFLOW TIME
    Set_Cursor(1,1)
    Send_Constant_String(#rtime)
    setreflowtime:
	buttonpress(ADD_ONE, scroll3)
    increment1(reflowtime, hun3,#1)
    scroll3:
    displayvariable(14,reflowtime,13,hun3)
    buttonpress(NEXT,setreflowtime)
    
	
	;SAVING CHANGES
    final:
    Set_Cursor(1,1)
    Send_Constant_String(#save)
    buttonpress(ADD_ONE, change)
    mov a, switch
    cpl a
    mov switch, a
    change:
    cjne a, #0, yes
    Set_Cursor(2,1)
    Send_Constant_String(#selectno)
    buttonpress(NEXT, final)
    Set_Cursor(2,1)
    Send_Constant_String(#clear)
    ljmp redo
    yes:
	Set_Cursor(2,1)
    Send_Constant_String(#selectyes)
    buttonpress(NEXT, final)
    
    ;MOVING ON TO THE BAKING PROCESS
    Set_Cursor(1,1)
    Send_Constant_String(#otemp)
    Set_Cursor(2,1)
    Send_Constant_String(#clear)
    mov hunsec, #0
    mov second, #0
    
    
    ;LOOP UPDATING REAL TIME TEMPERATURE
    forever1:
	lcall forever
	

	;ONE BEEP EACH NEW STATE
	mov a, statealarm
    cjne a, state, nope
    add a, #1
    da a
    mov statealarm, a
    lcall beep
    nope:
	mov a, state
	cjne a, #1, soak
	
	;USING TIMER 2 FOR BAKE CLOCK
	displayvariable(11,second,10,hunsec)
	Set_Cursor(2,1)
	Send_Constant_String(#state1dis)

	;MAX POWER TO OVEN
	mov pwm+0, #low(1000) 
	mov pwm+1, #high(1000) 

	mov a, second			;safety case
	cjne a, #0x60, not60
	mov a, hun
	cjne a, #0, not60	
	mov a, #0x50
	subb a, bcd
	jnc escape			;basically checking for overflow
	sjmp not60

	;DOESNT REACH 50C WITHIN 60 SEC
	escape:
	mov statealarm, #1
	mov state, #1
	mov pwm+0, #low(0) 
	mov pwm+1, #high(0) 
	ljmp redo
	
	not60:
	mov a, hun
	cjne a, bcd+1, cont
	mov a, soaktemp		;passes safety check. or 60 seconds have not passes. now checking for ramp to soak temp set earlier
	cjne a, bcd, cont
	sjmp gogo
	cont:
	ljmp forever1
	gogo:
	mov state, #2
	mov second, #0
	mov hunsec, #0
	
	soak:
	mov a, state
	cjne a, #2, ramptoreflow
	;USING TIMER 2 FOR BAKE CLOCK
	displayvariable(11,second,10,hunsec)
	Set_Cursor(2,1)
	Send_Constant_String(#state2dis)
	;SET OVEN TO MEDIUM POWER
	mov pwm+0, #low(0) 
	mov pwm+1, #high(0)
	
	;CHECKING IF SOAK TIME MET
	mov a, hunsec
	cjne a, hun1, cont
	mov a, second
	cjne a, soaktime, cont
	
	;CONTINUE TO NEXT STATE RESET TIME
	mov state, #3
	mov second, #0
	mov hunsec, #0
	
	
	ramptoreflow:
	mov a, state
	cjne a, #3, reflow
	;DISPLAYING TIME VARIABLE ONCE AGAIN
	displayvariable(11,second,10,hunsec)
	Set_Cursor(2,1)
	Send_Constant_String(#state3dis)
	;SETTING OVEN TO MAX POWER
	mov pwm+0, #low(1000) 
	mov pwm+1, #high(1000) 
	;CHECKING TO SEE IF RAMP TEMPERATURE MET
	mov a, hun2
	cjne a, bcd+1, cont1
	mov a, bcd
	cjne a, reflowtemp, cont1
	sjmp gogo1
	cont1:
	ljmp forever1
	gogo1:

	;MOVING ON TO NEXT STATE
	mov state, #4
	mov second, #0
	mov hunsec, #0
	
	
	reflow:
	mov a, state
	cjne a, #4, coolmike
	;DISPLAY TIME VARIABLE
	displayvariable(11,second,10,hunsec)
	Set_Cursor(2,1)
	Send_Constant_String(#state4dis)
	;SET OVEN TO ABOVE MEDIUM POWER
	mov pwm+0, #low(0)
	mov pwm+1, #high(0) 
	;CHECK IF REFLOW TIME MET
	mov a, hun3
	cjne a, hunsec, cont1
	mov a, second
	cjne a, reflowtime, cont1
	
	;MOVING ON TO COOL STATE
	mov state, #5
	mov second, #0
	mov hunsec, #0
	
	coolmike:
	displayvariable(11,second,10,hunsec)
	Set_Cursor(2,1)
	Send_Constant_String(#state5dis)
	;TURN OFF OVEN
	mov pwm+0, #low(0) 
	mov pwm+1, #high(0) 
	
	
	;CHECKING IF COOL ENOUGH TO HANDEL
	mov a, hun2
	cjne a, bcd+1, cont2
	mov a, bcd
	cjne a, #0x30, cont2
	
	Set_Cursor(2,1)
	Send_Constant_String(#coolenough1)
	Set_Cursor(1,1)
	Send_Constant_String(#coolenough)
	lcall beep
	lcall beep
	lcall beep
	lcall beep
	lcall beep
	lcall beep

	
	ending:
	sjmp ending
	;add beep sounds for letting the person know that its cool enough, like 30 celcisu
	cont2:
    ljmp forever1 
    
    
  
	end