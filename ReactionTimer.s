

                THUMB         ; Switch to Thumb instruction set for more efficient code
                AREA          My_code, CODE, READONLY  ; Define a new area for the code
                EXPORT        __MAIN  ; Make __MAIN function visible outside this file
                ENTRY         ; Mark the entry point for the code execution

__MAIN          ; Start of the main function

                ; Initialize and turn off all LEDs using a defined base address for ease of access
                LDR           R10, =LED_BASE_ADR  ; Load base address of LED memory into R10
                MOV           R6, #0xB0000000     ; Set up value to turn off LEDs on port 1
                STR           R6, [R10, #0x20]    ; Write the value to turn off LEDs on port 1
                MOV           R6, #0x0000007C     ; Set up value to turn off LEDs on port 2
                STR           R6, [R10, #0x40]    ; Write the value to turn off LEDs on port 2

                ; Setting up registers for a counter loop
                MOV           R4, #0
                MOV           R0, #0
                MOV           R1, #0x0            ; Initialize simple counter
                MOV           R2 , #0xFF          ; Set a limit for the counter


                ; Initialize random number generator seed
loop            MOV           R11, #0xDCBA        ; Initialize R11 with a non-zero seed value for random number generation
;map 1 to 65353 (2^16-1)
;random value in r1111
;to 20000 to 100, 000
;20,000*0.1ms= 2s and 100,000*0.1ms=10s
                BL            RandomNum           ; Generate a random number
                BL            TURN_OFF             ; Call subroutine to turn off LEDs

                ; Masking and scaling random number to create a delay
                MOV           R1, #0xFFFF        ; Extracting the least sig 16 bits of R11--- Mask out the most significant 16 bits 00000000000111111111111
                AND           R11, R1
                MOV           R1, #122           ; Scale the number between 0 and 8,000,000 (7994270)
                MUL           R11, R1            ; Multiplying it by 122
                MOV           R1, #100           
                UDIV          R11, R1            ;Divide by 100 (to make sure that the range is between 0 and 79942.7)
                MOV           R1, #20000         ; Shift the range to 20,000 - 100,000
                ADD           R11, R1            ; Add 20000 to it to ensure 20000 if random number happens to be 0
												; our range is now 20,000 - 99,942.7
												; +- 5% of 2 seconds is 1.9s - 2.1 s 
												; +- 5% of 10s is 9.5s to 10.5s 
												; our range is between 2s and 9.994s
												; and hence, we are between the range.
												; QED

smallDelay      MOV           R0, R11            ; Use the random number for delay
                BL            DELAY
                BL            TURN_ON              ; Call subroutine to turn on LEDs
                MOV           R1, #0
                MOV           R0, #1

                ; Polling loop to wait for a button press
poll            BL            DELAY
                ADD           R1, #1
                LDR           R9, [R10, #0x55]   ; Read the input register to check for button press 55+address of leds= data register, and r10 is address of led
				;r9 address of data reg
                TST           R9, #(1<<2)        ; Test if the button is pressed
				;r9 anded with 4, checking if 3rd bit- bit for button being pressed- is pressed 
                BNE           poll                ; Keep polling if button not pressed

                ; Displaying a number on LEDs in a loop
displayLoop     MOV           R7, #4             ; Set byte counter for display
                push          {R1}                ; Push R1 to stack to preserve its value

; r1 is a 32 bit number, number is amount of 0.1 ms. 
;if it took me 1ms, r1 will have 10. r1/10 = number of ms. 

display         BL            DISPLAY_NUM         ; Call subroutine to display the number
                MOV           R0, #0x4E20         ; Load delay value
                BL            DELAY               ; Delay between displays
                LSR           R1, #8              ; Shift out the displayed byte
                SUBS          R7, #1              ; Decrement byte counter
                BNE           display             ; Loop until all bytes displayed
                BL            TURN_OFF             ; Turn off LEDs after displaying
                MOV           R0, #0xC350         ; Load delay value
                BL            DELAY               ; Delay after display
                pop           {R1}                ; Pop R1 from stack to restore its value
                B             displayLoop         ; Loop to display numbers continuously
				
;COUNTER CODE 
;imagine R1 has 11011 - 010
;				87654321

;mask with 0x3 gives 10 that's why this is left shifted by 29
;mask with 0x4 gives 0 left 
;mask with (11111000)2 gives 11011
;11011 because port 2 LEDS are from bit 2 to bit 6

;bc LEDs are active low, whatever you store needs to be complimented
; Display the number in R6 onto the 8 LEDs

; Turn on LED by setting a particular memory-mapped register
TURN_ON
		push 		{r3-r4}		; Save the current values of R6 and R4 on the stack
		MOV R4, #0xC000			; Load the lower 16 bits of the address into R4
		MOVT R4,#0x2009			; Load the upper 16 bits of the address into R4 (forming 0x2009C000)
		MOV R6, #0x90000000		; Move the specific data value to R6 to turn on LED
		STR R6, [R4, #0x20]		; Store the value in R6 to the memory address computed by (R4 + 0x20)
		
		pop 		{r3-r4}		; Restore the values of R6 and R4 from the stack
		
		; Insert user code here
		
		BX 		LR				; Return from the subroutine by branching to the address in the Link Register

; Turn off LED by setting a particular memory-mapped register
TURN_OFF
		STMFD R13!, {R6, R14}	; Push the values of R6 and LR (Link Register, R14) onto the stack
		MOV R4, #0xC000			; Load the lower 16 bits of the address into R4
		MOVT R4,#0x2009			; Load the upper 16 bits of the address into R4 (forming 0x2009C000)
		MOV R6, #0xB0000000		; Move the specific data value to R6 to turn off LED
		STR R6, [R4, #0x20]		; Store the value in R6 to the memory address computed by (R4 + 0)
		MOV R6, #0x0000007C
		STR R6, [R4, #0x40]
		LDMFD R13!, {R6, R15} ; Pop the values off the stack

DISPLAY_NUM		STMFD		R13!,{R1, R2, R14}

				RBIT R1, R1                 ; Reverse the bit order of R1
				REV R1, R1                  ; Reverse the byte order of R1 (after bit reversal)
				AND R5 ,R1, #0x03           ; R5 = R1 ANDed with 0x03, isolating the last 2 bits of R1
				LSL R5, #28                 ; Left Shift R5 by 28 bits, moving the bits to the top of the register
				AND R6, R1 , #0x04          ; R6 = R1 ANDed with 0x04, isolating the 3rd bit from the LSB of R1
				LSL R6, R6 , #29            ; Left Shift R6 by 29 bits (after isolating the bit), moving it to bit 31
				ORR R8, R5, R6             ; R8 = R5 ORed with R6, combining the bits from R5 and R6
				EOR R8, R8, #0xFFFFFFFF   ; R8 = R8 XORed with all 1's, flipping all bits of R8
				STR R8, [R10, #0x20]       ; Store R8 into the memory address contained in R10 plus offset 0x20

				; Second set of operations on R1
				AND R8 ,R1, #0xF8          ; R8 = R1 ANDed with 0xF8, isolating the top 5 bits of R1
				LSR R8, #1                 ; Right Shift R8 by 1 bit
				EOR R8, R8, #0xFFFFFFFF   ; R8 = R8 XORed with all 1's, flipping all bits of R8
				STR R8, [R10, #0x40]       ; Store R8 into the memory address contained in R10 plus offset 0x40

; Usefull commaands:  RBIT (reverse bits), BFC (bit field clear), LSR & LSL to shift bits left and right, ORR & AND and EOR for bitwise operations

				LDMFD		R13!,{R1, R2, R15}

;
; R11 holds a 16-bit random number via a pseudo-random sequence as per the Linear feedback shift register (Fibonacci) on WikiPedia
; R11 holds a non-zero 16-bit number.  If a zero is fed in the pseudo-random sequence will stay stuck at 0
; Take as many bits of R11 as you need.  If you take the lowest 4 bits then you get a number between 1 and 15.
;   If you take bits 5..1 you'll get a number between 0 and 15 (assuming you right shift by 1 bit).
;
; R11 MUST be initialized to a non-zero 16-bit value at the start of the program OR ELSE!
; R11 can be read anywhere in the code but must only be written to by this subroutine
RandomNum		STMFD		R13!,{R1, R2, R6, R14}

					AND              R1, R11, #0x8000
					AND              R2, R11, #0x2000
					LSL              R2, #2
					EOR              R6, R1, R2
					AND              R1, R11, #0x1000
					LSL              R1, #3
					EOR              R6, R6, R1
					AND              R1, R11, #0x0400
					LSL              R1, #5
					EOR              R6, R6, R1   ; R6 now contains the new bit to be shifted into the LSB
					LSR              R6, #15
					LSL              R11, #1      ; Shift R11 to the left by 1 bit
					ORR              R11, R11, R6 ; Insert the new LSB into R11
					LDMFD            R13!,{R1, R2, R6, R15}

;
;
;		Delay 0.1ms (100us) * R0 times
; 		aim for better than 10% accuracy
;               The formula to determine the number of loop cycles is equal to Clock speed x Delay time / (#clock cycles)
;               where clock speed = 4MHz and if you use the BNE or other conditional branch command, the #clock cycles =
;               2 if you take the branch, and 1 if you don't.



DELAY			STMFD		R13!,{R2, R14}
				; making the 1 millisecond delay
                 push {R5}
                 MOV R5,  #0x85 ; Load R5 with constant (133), base value for multiplication
                 MUL R5,R5,R0   ; Multiply R5 by R0 to scale the delay

DELAYING 
                 SUBS R5, #0x1  ; Subtract 1 from R5 and set condition flags
                 BGT DELAYING  ; If R5 > 0, branch to DELAYING, continuing the delay
                 pop {R5}
		;
		; code to generate a delay of 0.1mS * R0 times
		;
exitDelay		LDMFD		R13!,{R2, R15}
				

LED_BASE_ADR	EQU 	0x2009c000 		; Base address of the memory that controls the LEDs 
PINSEL3			EQU 	0x4002c00c 		; Address of Pin Select Register 3 for P1[31:16]
PINSEL4			EQU 	0x4002c010 		; Address of Pin Select Register 4 for P2[15:0]
FIO2PIN         EQU     0x2009c054
;	Usefull GPIO Registers
;	FIODIR  - register to set individual pins as input or output
;	FIOPIN  - register to read and write pins
;	FIOSET  - register to set I/O pins to 1 by writing a 1
;	FIOCLR  - register to clr I/O pins to 0 by writing a 1

				ALIGN 
END 



; QUESTION 1
;If a 32-bit register is counting user reaction time in 0.1 milliseconds increments, what is the
;maximum amount of time which can be stored in 8 bits, 16-bits, 24-bits and 32-bits?


;Each bit can represent two values (0 or 1). Therefore, an n-bit number can represent 2^n unique values.
;If a register is counting in 0.1 millisecond increments, then to find the maximum amount of time it can store, we would multiply the number of unique values it can represent by the increment value.
;8-bit register: Can store 2^8 values, which is 256. Multiplied by 0.1 milliseconds gives us 25.6 milliseconds maximum. 
;16-bit register: Can store  2^16 values, which is 65,536. Multiplied by 0.1 milliseconds gives us 6,553.6 milliseconds, or 6.5536 seconds maximum.
;24-bit register: Can store 2^24 values, which is 16,777,216. Multiplied by 0.1 milliseconds gives us 1,677,721.6 milliseconds, or 1,677.7216 seconds (approximately 27.96 minutes) maximum.
;32-bit register: Can store 2^32 values, which is 4,294,967,296. Multiplied by 0.1 milliseconds gives us 429,496,729.6 milliseconds, or 429,496.7296 seconds (approximately 119.86 hours or almost 5 days) maximum.


; QUESTION 2
;Considering typical human reaction time, which size would be the best for this task (8, 16,
;24, or 32 bits)?
;Typical human reaction times are usually within the range of 150 to 300 milliseconds, but to accommodate a wider range including outliers, we might consider up to 1000 milliseconds (1 second) as the upper limit for a robust reaction timer.
;Using these principles, let's calculate:

;Given these calculations:
;An 8-bit register is too small, as it can't even cover the typical lower bound of human reaction time.
;A 16-bit register is quite sufficient to cover typical human reaction times, with plenty of room to spare for slower reactions.
;A 24-bit register and a 32-bit register provide more range than necessary for this application.
;Therefore, the 16-bit register would be the best choice for measuring human reaction times, as it provides an adequate range without being wastefully large.
