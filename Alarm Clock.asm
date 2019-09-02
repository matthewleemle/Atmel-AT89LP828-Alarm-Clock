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
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))



SOUND_OUT     equ P3.7
UPDOWN        equ P0.0

one equ P2.0
two equ P2.2
three equ P2.4
four equ P2.6
five equ P0.5
six equ P0.3
seven equ p0.1


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

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
second_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
minute_counter: ds 3;
hour_counter: ds 3;
day_counter: ds 4;

Alarmhour: ds 5
Alarmmin: ds 6
Alarmampm: ds 7

Alarmhour2: ds 8
Alarmmin2: ds 9
Alarmampm2: ds 0






; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
one_second_flag: dbit 1 ; Set to one in the ISR every time 1 s had passed

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
PM:  db 'PM', 0
AM: db 'AM', 0
colon: db ':', 0
Monday: db 'Mon', 0
Tuesday: db 'Tues', 0
Wednesday: db 'Wed', 0
Thursday: db 'Thurs', 0
Friday: db 'Fri', 0
Saturday: db 'Sat', 0
Sunday: db 'Sun', 0


 
	



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
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
      ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	

	mov a, second_counter
	cjne a, #0x59, check
	mov second_counter,#0

	
	mov a, second_counter
	add a, #0x99
	mov second_counter, a
	

		
	
	
	mov a, minute_counter
	inc a
	da a
	mov minute_counter, a
	cjne a, #0x60, check
	mov minute_counter, #0


	
	
	inc hour_counter
	mov a, hour_counter
	da a
	mov hour_counter, a
	
	
	mov a, hour_counter
	cjne a, #0x12, check
	cjne R1, #1, set_to_one
	cjne R1, #0, set_to_zero
	
	set_to_one: 
	mov R1, #1
	
	sjmp check



	set_to_zero:
	mov R1, #0
	mov a, day_counter
	inc a
	cjne a, #0x8, check3
	mov a, #1
	
	
check3: 		
	mov day_counter, a
	sjmp check	
	
	

check:	

		
	mov a, hour_counter
	cjne a, #0x13, check2
	mov hour_counter, #1
	
	mov a, day_counter
	add a, #0x1
	mov day_counter, a
	
	cjne a, #0x8, check2
	mov day_counter, #0x1

	

	


check2:
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000),Timer2_ISR_done
	
	
	; one second have passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know one second had passed
	;cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable

	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	mov a, second_counter
	inc a
	sjmp Timer2_ISR_da
	
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov second_counter, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
    Set_Cursor(1, 6)
    Send_Constant_String(#colon)
    Set_Cursor(1, 3)
    Send_Constant_String(#colon)
    setb one_second_flag
	mov second_counter, #0x30
	mov minute_counter, #0x19
	mov hour_counter, #0x2
	mov day_counter, #0x5 
	mov R1, #1 ; When R1 is 0 it is AM when R1 is 1 it is PM
	mov Alarmmin, #0x20
	mov Alarmhour, #0x4
	mov Alarmampm, #1
	
	mov Alarmmin2, #0x20
	mov Alarmhour2, #0x8
	mov Alarmampm2, #1

loop6: 
	jb seven, loop5
	Wait_Milli_Seconds(#100)
	jb seven, loop5
	ljmp alarmsetting2

loop5: 
	jb six, loop4
	Wait_Milli_Seconds(#100)
	jb six, loop4
	ljmp alarmsetting

loop4: 
	jb five, loop3
	Wait_Milli_Seconds(#200)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb five, loop3  ; if the 'BOOT' button is not pressed skip
	ljmp switch
	


loop3:
	jb one, loop2
	Wait_Milli_Seconds(#100)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb one, loop2  ; if the 'BOOT' button is not pressed skip
	ljmp incrementhour
	
	
loop2:

	jb two, loop1
	Wait_Milli_Seconds(#100)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb two, loop1  ; if the 'BOOT' button is not pressed skip
	ljmp decrementhour

	
	
loop1:
	jb three, loop
	Wait_Milli_Seconds(#100)
	jb three, loop
	ljmp incrementminute
	
	; After initialization the program stays in this 'forever' loop
loop:
	jb four, loop_a ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#100)
	jb four,loop_a
    ljmp decrementminute
      
  

loop_a:
	jnb one_second_flag, loop7

	
loop_b:
    clr one_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	

	mov a, Alarmmin
	cjne a, minute_counter, jump
	
	mov a, Alarmhour
	cjne a, hour_counter, jump
	
	mov a, R1
	cjne a, Alarmampm, jump
	cpl TR0



	
jump:	  

	mov a, Alarmmin2
	cjne a, minute_counter, jump2
	
	mov a, Alarmhour2
	cjne a, hour_counter, jump2
	
	mov a, R1
	cjne a, Alarmampm2, jump2
	cpl TR0
 


jump2: 
	Set_Cursor(1, 7)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(second_counter) ; This macro is also in 'LCD_4bit.inc'
	set_Cursor(1,4)
	Display_BCD(minute_counter)
	set_Cursor(1,1)
	Display_BCD(hour_counter)
	
	set_Cursor(1,12)
	mov a, day_counter
	
	cjne a, #1, display_rest1
	Send_Constant_String(#Monday)
	ljmp rest
loop7: ljmp loop6   
	
display_rest1:
	cjne a, #2, display_rest2
	Send_Constant_String(#Tuesday)
	sjmp rest

display_rest2:
	cjne a, #3, display_rest3
	Send_Constant_String(#Wednesday)
	
display_rest3:

	cjne a, #4, display_rest4
	Send_Constant_String(#Thursday)
	sjmp rest

display_rest4:
	cjne a, #5, display_rest5
	Send_Constant_String(#Friday)
	sjmp rest

display_rest5:
	cjne a, #6, display_rest6
	Send_Constant_String(#Saturday)
	sjmp rest

display_rest6:
	cjne a, #7, rest
	Send_Constant_String(#Sunday)
		
rest:	


	cjne R1, #0, pmcond   
    cjne R1, #1, amcond
    
    
amcond: 
	Set_Cursor(1, 9)
    Send_Constant_String(#AM)
    ljmp loop2
    
pmcond:
	Set_Cursor(1,9)
	Send_Constant_String(#PM) 
	ljmp loop2

incrementminute:
	mov a, minute_counter
	add a, #0x1
	da a
	mov minute_counter, a
	clr TR0
	ljmp loop_a
	
decrementminute:
	mov a, minute_counter
	add a, #0x99
	da a
	mov minute_counter, a
	clr TR0
	ljmp loop_a


    
incrementhour:
	mov a, hour_counter
	add a, #0x1
	da a
	mov hour_counter, a
	clr TR0
	ljmp loop_a
	
decrementhour:
	mov a, hour_counter
	add a, #0x99
	da a
	mov hour_counter, a
	clr TR0
	ljmp loop_a
	


switch:
	clr TR0
	mov a, R1
	cjne a, #0, setzeroversion2
	cjne a, #1, setoneversion2
	
setzeroversion2:
	mov R1, #0
	ljmp loop_a

setoneversion2:
	mov R1, #1
	ljmp loop_a
	
alarmsetting:
	setb TR0
	Wait_Milli_Seconds(#100)
	clr TR0
	mov a, minute_counter
	mov Alarmmin, a
	mov a, hour_counter
	mov Alarmhour, a
	mov Alarmampm, R1

	ljmp loop_a
	
alarmsetting2:
	setb TR0
	Wait_Milli_Seconds(#100)
	clr TR0
	mov a, minute_counter
	mov Alarmmin2, a
	mov a, hour_counter
	mov Alarmhour2, a
	mov Alarmampm2, R1

	ljmp loop_a

	
END
