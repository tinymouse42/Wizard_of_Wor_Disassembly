; #INCLUDE ".\\WoW_Disassembly\\WoW_Disassembly_Include_Files\\WoW_Disassembly_Header.include"
; This code was written for the TASM assembler and will have to be converted to for ZMAC to compile it.
; Verified working 7/8/2018

;******************************************************************************
; Begin game initialization
;******************************************************************************
		.ORG	 $0000		; Beginning of ROM (see memory map)                           
					                                                             
		di			; No Interruptions                                            
		ld	 sp,$D400	; Set Stack at $D400 ($0400 stack size)
					; STATIC RAM is at $D000 - $D3FF
		ld	 a,$01		                              
L0006:		out	 ($08),a	; Set Video to HI-RES 

L0008:		ld	 a,%00101100	; 00xxxxxxb = Position to switch left and right			
L000A:		out	 ($09),a	; pallete. xx000000b vblank color 

L000C:		ld	 a,$CC		; Set to 204 screen height.                                                     
		out	 ($0A),a	; Note DNA manual says 0-203. Why 204?
		
L0010:		call	 L0093		; Set interrupt vector and color mapping

		ld	 a,%00001000	; 0000S0L0b = Enable/disable Screen/Lightpen interrupts   
L0015:		out	 ($0E),a	; 00000S0Lb = MODE for Screen/Lightpen interrupts 
		

L0017:		ld	 a,00000000b	; Disable coin counter 3                                                                                                     
		in	 a,($15)	                
		ld	 a,00000010b	; Disable coin counter 2                                          
		in	 a,($15)	            
		ld	 a,00001110b	; Disable ???  On gorf it is unused light transistor                                         
		in	 a,($15)	 
		
		call	 L06C8		; Set interrupt line and turn on all sparkle colors
		
L0026:		ld	 a,$00		; Set High order byte for Interrupt                                                            
		ld	 i,a		; Interrupts will be from $0000 to $00FF                          
		im	 2		; Explaination: A call is made to an address read from
					; address (register I × 256 + value from interrupting device)        
				         


;******************************************************************************
;
; This looks like the end of the hardware initialization code
; and the beginning of the code to run the game. So far, I have found that the 
; program has tokens associated with subroutines and are kept in a table and 
; called by indexing in IY.
;
; This is due to this being a Forth like language called TERSE. The tokens
; are really "Forth" words and the subroutines associated with it are
; what the "Forth" commands execute.
;
;******************************************************************************
	
	xor	 a		                                                        
	ld	 (LD2D3),a	; Zero ??? (location always seems to have a $D2 in it)
	
L0030:	ld	 a,(L8006)	; If l8006 = Jump Instruction, call it  
	cp	 $C3		                                                             
	call	 z,L8006	; Load ($D270)->($D272) with $18, $40, $87
				; Zero 14 locations above
				; Zero music output (8 zero bytes to music output block)
				; Zero 42 locations below
				; ??? (need to trace more)
;
; Temporary !!!
; At this point, memory between has the following:
; $D270 = $18, $40, $87
; $D281 = $01
; $D2AC = $58, $40, $87
; $D2BE = $01
; $D3F5 = $AF, $D2, $AF, $D2, $58, $08, $AB, $D2, $63, $82, $38
; 
;
		ld	 hl,LD03A	; Increment <??? variable>                                                                        
		ld	 c,(hl)		;                            
L003C:		inc	 c		;              
		call	 L0F6A		; Write to protected memory (HL)=C routine
		

L0040:		ld	 hl,(LD038)	; Load <??? variable>      
		call	 L00AA		; ...and ???

		ld	 hl,(LD03E)	; Load <??? variable>      
		call	 L00AA		; ...and ???

		ld	 a,(LD03C)	; If ($d03c)=$1f then
L004F:		cp	 $1F		; ...Go zero first $40 bytes of static RAM  ($D000-$D03F)                    
		call	 nc,L00B5	; 
		
L0054:		ld	 hl,Is_Speech_Active	;  
		call	 L00BA		; 256 bytes to $D245 ???     
		call	 L00B8		; 64 bytes to 
		
		ld	 a,r		; Load a with refresh register??? RND Number?  
		ld	 (LD34A),a	; ...and save it to <??? variable> 
		
		ld	 hl,LD000	; Move $20 bytes (32 decimal) from $D000 to $D300             
		ld	 de,LD300	;       
L0068:		ld	 bc,L0020	;                   
		ldir			;

		call	 L08AE		; Reset video and blank some static RAM ???

;
; Here is where all the real stuff starts happening. Details are few, but from what
; I can gather, this part runs the demo and game loop. It also checks to see if the
; diagnostic switch is set and jumps to diagnostic loop if so at the end of a game or 
; the demo loop. ???
;

	ld	 a,(Game_Mode)	; Load game status variable                      
	and	 a		; Is game in "demo" mode (i.e. no game going on)
	ld	 iy,$0F70	; Ready demo loop pointer                                   
L0078:	jr	 z,L007E	; Skip game loop - we are in demo mode                           
	ld	 iy,L10FD	; ...else ready game loop pointer
	
L007E:	ld	 hl,L007E	; Set up "return" from subroutine
L0081:	push	 hl		; 

	call	 L0875		; Load up HL with a saved address in IY and IY+1
				; ... and IY++
	
	push	 hl		; ...and push it to the stack (for return later?)
	
	in	 a,($10)	; Check status of Service Switch (active low)                  
L0088:	bit	 3,a		;                                     
	ret	 nz		; Diag switch off, continue to demo in progress
	
	ld	 a,(Game_Mode)	; Is a game in progress                               
	and	 a		;                             
	ret	 nz		; Yes, continue game in progress 
				; (... don't want to jump to diag's during a game... wait until after game is over)
;
	jp	 L00FB		; Otherwise, jump to diagnostics loop
				; ... note diag loop terminates with a RST $00
;
;*****************************************************
; Set Interrupt vector to $CA and map color pallete
;*****************************************************
;				
L0093:	ld	 a,$CA		;Interrupt vector at $CA (202)         
	out	 ($0D),a	;This is the upper byte when interrupt hits
	
	ld	 hl,L00C5	;Point to an color mapping table               
	ld	 bc,L080B	;Move $08 bytes to Port $0b                
	otir			;(C)<-(HL), HL+1, B-1 until B=0            
	ret			;Interrupt vector an color mapping complete
;
;*****************************************************
; Set Interrupt vector to $CC
;*****************************************************
;
L00A0:	ld	 a,$CC
	out	 ($0D),a
L00A4:	ret	
;
;*****************************************************
; Set Interrupt vector to $CE
;*****************************************************
;
L00A5:	ld	 a,$CE
	out	 ($0D),a
	ret	
;
;*****************************************************
; Checks HL to see if lower nybble is same as upper
; If yes, skip to clear memory
; If no, see if complement of H same as normal L
; If yes, then return.
; 
; In:	HL
; Out:	None
;
;*****************************************************
;
L00AA:	ld	 a,l		
L00AB:	rlca			 
	rlca	
	rlca	
	rlca	
	cp	 l		 
	jr	 nz,L00B5
	cpl			
	cp	 h		
	ret	 z		
;
;*****************************************************
; In:	None
; Out:	Zero $40 (64) bytes at the bottom of 
;	static RAM $D000 to $D040
;*****************************************************
;
L00B5:	ld	 hl,LD000	; Bottom of Static RAM

L00B8:	ld	 b,$40		; bytes to write

L00BA:	ld	 c,$00		; Fill with 0's
	ld	 a,$A5		; Unlock memory byte

L00BE:	out	 ($5B),a	; Memory Protection Port

L00C0:	ld	 (hl),c		; write byte
	inc	 hl		; next byte
	djnz	 L00BE		; Go until all byte written
	ret			; Done!
;
;*********************************************************
;
; Data area for routines at ???
; What is the data for ???
;
;*********************************************************
;

L00C5:  .db 	$51, $7c, $f3	
L00C8:  .db	$c7, $00, $56, $09, $9e, $09, $b4, $09 

;
;*****************************************************
;
; This is called from the memory fill routine when it
; gets an error. (See L00D4). It loads the refresh register
; (random number) and outputs it to a video port.
; This may be what makes the screen go crazy when video
; memory is bad.
;
;*****************************************************
;
L00D0:	ld	 a,r
	out	 ($09),a
;
;*****************************************************
;
; This routine is part of the memory test, or at least
; it fills the memory to solid values. It takes a byte,
; fills video memory, checks it. It then  complements
; the byte and fills from top of video to bottom, checks it
; then returns.
; One of the interesting things is that this is a type of
; worm test. It walks the byte through memory so it only has
; to check the last location for the correct value.
; If it's different than the pattern, then memory screwed
; it up somewhere. In that case, it calls the error routine.
; Another note, the "return" address is in HL, so if calling
; this routine, you have to set up HL with where you want
; it to "return" to.
;
;*****************************************************
;
L00D4:	exx			; Swap the main registers             
	ld	 hl,L4000	; Point to beginning of video memory  
	ld	 (hl),a		; Load the pattern to write in first  
				; byte of video memory                
	ld	 de,L4001	; point to one byte later               
	ld	 bc,L3FFF	; set count for all memory              
	ldir			; walk the bit through memory           
	cp	 (hl)		; did the pattern change?               
	jr	 nz,L00D0	; yes, then go to error routine         
	ex	 af,af'		; exchange AF                           
	in	 a,($10)	; Kick the dog              
	ex	 af,af'		; exchange again                        
L00E8:	dec	 de		; set up de to correct place            
	dec	 de		; ... for the next pass                 
	cpl			; complement the pattern                
	ld	 (hl),a		; load it at top of video memory        
	ld	 bc,L3FFF	; whole video memory as before          
	lddr			; work it down..                        
	cp	 (hl)		; did it change?                        
	jr	 nz,L00D0	; yes, go to error routine              
	ex	 af,af'		; exchange                              
	in	 a,($10)	; kick the dog       
	ex	 af,af'		; swap back                             
	cpl			; complement the pattern back           
	exx			; swap the main register back           
	jp	 (hl)		; Go back from whence it came... 
;
;*********************************************************
;
; Entry point to the game diagnostics. The diagnostics
; loop is terminated by a RST $00 when the diag
; switch is turned off. Otherwise, it loops forever.
;
;*********************************************************
;
L00FB:	di			; no interuptions 
	ld	 a,(L8006)	; Load from ROM   
L00FF:	cp	 $C3		; check ROM $8006 for a Jump
				; instruction... Why check???
L0101:	call	 z,L8006	; Well, call it if it's there...
	call	 L06CC		; I think this sets up sound to 
				; a known state. ???            
	ld	 a,$80		; Load a pattern for video        
				; memory test (Pattern is 1000 0000B)  
				; and fall through to the test...      
;
;*********************************************************
;
; Set up return to $010e and call memory test fill pass
; at l00d4. ???
;
;*********************************************************
;
L0109:	ld	 hl,L010E	; set the return location to l010e  
	jr	 L00D4		; Off to fill video memory... 
;
;*********************************************************
;
; This rotates the initial test pattern in a (which looks
; to be $80 (or 1000 0000 binary) to walk the bit to the
; right for each pass. When a=0, then it's done and moves on.
;
;*********************************************************
;
L010E:	and	 a		; Set the flags                                     
	jr	 z,L0114	; Jump if done...                                   
L0111:	rra			; Change the fill pattern by walking bit right      
	jr	 L0109		; Go to memory fill at l0109
;
;*********************************************************
;
; Entry point to the game after the Video RAM tests are
; complete. Let's go play Wizard of Wor! ???
;
;*********************************************************
;
L0114:	ld	 sp,L8000	; Set stack pointer right                          
				; below high ROM, but into                         
				; the top of video memory.                         
				; Turns out that video memory ends at $7fbf        
				; and from $7fc0 to $7fff ($40 bytes) is not       
				; displayed. That $40 bytes is used for the stack. 
L0117:	call	 L08AE		; Reset video and blank some static RAM ???
	ld	 hl,L042E	; String "SCREEN RAM OK"
	ld	 de,L001A	; Color (RED) ???
	ld	 b,$0D		; Length
	call	 L03B3		; Go write the string - check on details ???
;
;
;*****************************************************
;
; Static RAM test starts here
;
;*****************************************************
;
	ld	 hl,LD000	; Bottom of Static RAM
	ld	 bc,$0004	; Outer loop setup
	ld	 d,$FF		; Pattern
	ld	 a,$A5		; 
L012F:	out	 ($5B),a	; Enable write to protected memory
	ld	 (hl),d		; Store in Static RAM
	ld	 d,(hl)		; Read it back in
	inc	 hl		; Increment to next location to test
	djnz	 L012F		; ... and keep going until 255 bytes checked
	dec	 c		; count down the outer loop
	jr	 nz,L012F	; Outer loop is executed 4 times for 1K
	ld	 a,d		; Load test character again
	cp	 $FF		; Is it an $FF?
	jr	 nz,L016C	; No, jump off to error
	ld	 c,$04		; Set up outer loop again
L0140:	inc	 d		; Bump up test character...
	ld	 a,$A5		; 
L0143:	out	 ($5B),a	; Enable write to protected memory
	dec	 hl		; Going down through memory
	ld	 (hl),d		; Store test character
	ld	 d,(hl)		; Get it back from memory
	djnz	 L0143		; Go until bottom of static RAM reached
	dec	 c		; Outer loop ???
	jr	 nz,L0143	; go do ??? until ???
	ld	 a,d		; 
	and	 a		; Test if we got back test character correctly
	jr	 nz,L016C	; Jump to ??? if not
	ld	 c,$04		; Set up for next pass
L0153:	or	 (hl)		; change bit pattern in memory
L0154:	inc	 hl		; next (going up)
L0155:	djnz	 L0153		; Again until reach end of memory
	dec	 c		; Outer loop ???
	jr	 nz,L0153	; More until ???
	and	 a		; Did we get back correct byte? ???
	jr	 nz,L016C	; Off to the error routine if not...

	ld	 a,$55		; 
	ld	 (LD045),a	; This location seems to hold a $55 when ??? and static RAM tests OK
	ld	 a,(LD045)	; This location seems to hold a $55 when ??? and static RAM tests OK
	cp	 $55		; 

	ld	 hl,$043B	; String "STATIC RAM OK "
L016A:	jr	 z,L016F	; Skip if static RAM is good ???
L016C:	ld	 hl,L0449	; String "STATIC RAM BAD" 
L016F:	ld	 de,$051A	; Set color and ???
	ld	 b,$0E		; Length
	call	 L03B3		; Write the string
;
;*****************************************************
;
;Start checking ROMS
;
;*****************************************************
	ld	 hl,L0457	; String "ROM "
	ld	 de,L0A28	; Set color and ??? 
	call	 L03B1		; Sets length to $04 and drops
				; through to the write string
				; routine. 
	ld	 hl,L03D5	; String "ABCDEFGX"
	exx			; Swap the registers
	ld	 de,L03E0	; ???
	ld	 hl,$0000	; Point to ROM beginning
	ld	 a,(LD347)	; ???
	in	 a,($13)	; Check if Foriegn language switch                     
	bit	 3,a		; ... in bit 3 (DSW Switch 4 - English=On)                    
	ld	 b,$07		; B=initial number of ROMS                                         
	jr	 nz,L0196	; If it's English, leave ROM count at 7     
	inc	 b		; If Foreign, Set ROM count to 8                
L0196:	push	 bc		; Below is ROM check... details to come... ???      
	ld	 a,h		; 
	cp	 $40		; Is H=$40? ???
	jr	 nz,L019E	; No, skip ahead
	ld	 h,$80		; H=80 ???
L019E:	cp	 $B0		; Is old H=$B0? ???
	jr	 nz,L01A7	; No, skip ahead 
	ld	 de,LC00A	; DE is set to location in alt. char. ROM
	ld	 h,$C0		; H=$C0???
L01A7:	ld	 bc,L0010	; 
	xor	 a		; A=0
L01AB:	add	 a,(hl)		; 
	inc	 hl		;
	djnz	 L01AB		; Add 255 bytes starting at HL
	dec	 c		;
	jr	 nz,L01AB	; ... 16 times for a total of 4K
	ex	 de,hl		;
	cp	 (hl)		;
	inc	 hl
	ex	 de,hl
	exx	
	jr	 z,L01C1
	ld	 a,d
	ld	 (LD1D4),a
	call	 L0374
	dec	 hl
L01C1:	inc	 hl
	exx	
	pop	 bc
	djnz	 L0196
;
; End of ROM Test
;
	exx			; Restore registers after ROM test
	ld	 hl,$0446	; String "OK"
	ld	 a,(LD1D4)	; ???
	and	 a		;
	call	 z,L0356	; ???
	ld	 a,$01		;
	ld	 (LD1C5),a	; Save 01 in ??? 
	ld	 de,$1403	; Set color and ??? for string write
	call	 L03A7		; Go write "MOVE" and "FIRE" then drop to write routine 
	ld	 e,$30		; change color or ??? for string write
	call	 L03A7		; Go write "MOVE" and "FIRE" then drop to write routine
L01E1:	ld	 de,L1E0B	; Set color and ??? for string write
	call	 L03AE		; Write "FIRE" and drop to write string
	ld	 e,$38		; change color or ??? for string write
	call	 L03AE		; Write "FIRE" and drop to write string
	ld	 hl,L0408	; String "PL1"
	ld	 de,L230B	; Color and ??? for string write
	call	 L03A3		; B=3 and drop to write string
	ld	 hl,L040B	;
	ld	 e,$38
	call	 L03A3
L01FD:	ld	 de,L280B
L0200:	ld	 hl,L03DD
L0203:	call	 L03BA
L0206:	ld	 e,$38
	call	 L03BA
L020B:	ld	 e,$22
	call	 L03BA
	ld	 de,L2D0B
	ld	 hl,L0412
L0216:	call	 L03B1
	ld	 hl,L0416
	ld	 de,L2D22
	call	 L03C4		; Write a string and ???
	ld	 de,L2D38
	call	 L03C4		; Write a string and ???
L0228:	ei			; OK to interrupt me...
	ld	 de,LD1D6	; Player 2 control status save area
	in	 a,($11)	; Read player 2 controls
	cpl			; ???
	ld	 (de),a		; Save Player 2 control status
L0230:	ld	 de,LD1D7	; Player 1 control status save area
	in	 a,($12)	; Read Player 1 controls
	cpl			; ???
	ld	 (de),a		; Save Player 1 control status
	ld	 de,L190B	; ???
	ld	 hl,LD1CE	; ???
	ld	 a,(LD1D6)	; Load Player 2 control status
	call	 L0317		; ???
	ld	 de,L1939	; ???
	ld	 hl,LD1CF	; ???
	ld	 a,(LD1D7)	; Load Player 1 control status
	call	 L0317		; ???
	ld	 de,L1903	; ???
	ld	 hl,LD1D0	; ???
	ld	 a,(LD1D6)	; Load Player 2 control status
	and	 $10		; ???
	call	 L0397		; Test and write YES or NO
	ld	 e,$30		; ???
	ld	 hl,LD1D1	; ???
	ld	 a,(LD1D7)	; Load Player 1 control status	
	and	 $10		; ???
	call	 L0397		; Test and write YES or NO
	ld	 de,L1E03	; ???
	ld	 hl,LD1CC	; ???
	ld	 a,(LD1D6)	; Load Player 2 control status
	and	 $20		; ???
	call	 L0397		; Test and write YES or NO
	ld	 e,$30		; ???
	ld	 hl,LD1CD	; ???
	ld	 a,(LD1D7)	; Load Player 1 control status
	and	 $20		; ???
	call	 L0397		; Test and write YES or NO
	ld	 de,L2303	; ???
	ld	 hl,LD1D2	; ???
	in	 a,($10)	; Check IN0 for activity
	cpl			; ???
	and	 $20		; Check ???
	call	 L0397		; Test and write YES or NO
	ld	 e,$30		; Check ???
	ld	 hl,LD1D3	; ???
	in	 a,($10)	; Check IN0 for activity
	cpl			; ???	
	and	 $40		; Check ???
	call	 L0397		; Test and write YES or NO
L02A0:	ld	 de,L2803	; ???
	ld	 hl,LD1C9	; ???
	in	 a,($10)	
L02A8:	cpl	
	and	 $01
	call	 L0397
	ld	 e,$30
	ld	 hl,LD1CA
	in	 a,($10)
	cpl	
	and	 $02
	call	 L0397
	ld	 e,$1A
	ld	 hl,LD1CB
	in	 a,($10)
	cpl	
	and	 $04
	call	 L0397
	ld	 de,L2D03
	ld	 hl,LD1D5
	in	 a,($10)
	cpl	
L02D1:	and	 $10
	call	 L0397
	ld	 a,(LD347)
	in	 a,($13)
L02DB:	cpl	
	ld	 b,a
	ld	 de,L2D1A
	ld	 hl,LD1D8
	ld	 c,$01
L02E5:	push	 hl
	push	 bc
	ld	 a,b
	and	 c
	push	 de
	call	 L0397
	pop	 de
	pop	 bc
	ld	 hl,$0500
	add	 hl,de
	ex	 de,hl
	pop	 hl
	inc	 hl
	ld	 a,c
	cp	 $08
	jr	 nz,L02FE
	ld	 de,L2D30
L02FE:	sla	 c
L0300:	jr	 nz,L02E5
L0302:	in	 a,($10)
	and	 $60
L0306:	jr	 z,L0310
	in	 a,($10)	; 
	bit	 3,a		; Check for service switch on
	jp	 z,L0228	; Jump back to diagnostic screen if so
L030F:	rst	 00H		; Otherwise, restart everything! 
;
;*****************************************************
;
; ???
;
;*****************************************************
L0310:	di	
	call	 L08AE
L0314:	jp	 LAF80
L0317:	and	 $0F
	cp	 (hl)
	ret	 z
;
	ld	 (hl),a
	ld	 c,a
	ld	 b,$00
L031F:	push	 bc
L0320:	push	 de
	ld	 b,$05
	xor	 a
	call	 L03B5
	pop	 de
L0328:	pop	 bc
	ld	 hl,L0333
	add	 hl,bc
	add	 hl,bc
	ld	 a,(hl)
	inc	 hl
	ld	 h,(hl)
L0331:	ld	 l,a
	jp	 (hl)


L0333:	ld	 d,e
	inc	 bc
	ld	 e,d
	inc	 bc
	ld	 e,a
	inc	 bc
	ld	 h,h
L033A:	inc	 bc
	ld	 l,h
	inc	 bc
	ld	 a,b
	inc	 bc
	add	 a,b
L0340:	inc	 bc
	ld	 h,h
	inc	 bc
	adc	 a,b
L0344:	inc	 bc
	adc	 a,l
	inc	 bc
	sub	 d
	inc	 bc
	ld	 h,h
	inc	 bc
	ld	 h,h
	inc	 bc
	ld	 h,h
	inc	 bc
	ld	 h,h
	inc	 bc
	ld	 h,h
	inc	 bc

	ld	 hl,L03EC	; "NO" String
L0356:	ld	 b,$02
	jr	 L03B3		; Write String
L035A:	ld	 hl,L03EE
	jr	 L0356
L035F:	ld	 hl,L03F0
	jr	 L0356
	ld	 hl,L03F7	; String "ERROR"
	ld	 b,$05
	jp	 L03B3		; Write String
;
L036C:	ld	 hl,L03F2	; String "YES"
	jr	 L0356
L0371:	ld	 hl,L03F6	; String "_" May translate to other character ???
L0374:	ld	 b,$01
	jr	 L03B3		; Write string
;
	call	 L036C
L037B:	call	 L0371
	jr	 L035A
	call	 L036C
L0383:	call	 L0371
	jr	 L035F
L0388:	ld	 hl,L03F4
	jr	 L0356
	call	 L0388
	jr	 L037B
	call	 L0388
	jr	 L0383
;
;*****************************************************
;
; ??? test to see if specified control is off or on
; and write YES or NO accordingly.
;
;*****************************************************
;
L0397:	cp	 (hl)		; ???
	ret	 z		; ???
	ld	 (hl),a		; ???
	and	 a		; ???
	ld	 hl,$03EB	; String " NO"
	jr	 z,L03A3	; ???
	ld	 hl,L03E8	; String "YES"
L03A3:	ld	 b,$03		; Length
	jr	 L03B3		; Write string
;
;*****************************************************
; Write "MOVE"
;*****************************************************
;
L03A7:	ld	 hl,L0400	; String "MOVE"
	ld	 b,$08		; Length
	jr	 L03B3		; Write string
;
;*****************************************************
; Write "FIRE"
;*****************************************************
;
L03AE:	ld	 hl,L03FC	; String "FIRE"
L03B1:	ld	 b,$04		; Length
;
;*****************************************************
; Entry Point to "write string" command?
; Parameters:
;	HL=<start address of string (ASCII)
;	DE=<color> ???
;	B=<Length of string (ASCII Character count)
; Other:
;	A= Expand mode color ???
;	C=???
;*****************************************************
;
L03B3:	ld	 a,$0C		; Expand mode color ???
L03B5:	ld	 c,$FF		; ??? 
	jp	 L0460		; Go write the string...
;
;*****************************************************
; ???
;
;*****************************************************
L03BA:	push	 hl
	ld	 hl,L040E
	call	 L03B1
	pop	 hl
	jr	 L0374
;
;*****************************************************
; ??? Write string of some kind... strange stuff going on.
;
;*****************************************************
L03C4:	ld	 b,$04		; ???	
L03C6:	push	 bc		; Save ???
	call	 L03A3		; B=3 and drop to write string
	push	 hl		; save ???
	ld	 hl,$04FA	; HL is in graphic characters area ???
	add	 hl,de		; ???
	ex	 de,hl		; DE now has ???
	pop	 hl		; Restore ???
	pop	 bc		; Restore count
	djnz	 L03C6		; Loop until ???
	ret			; Go back
;*****************************************************
; Begin Data area for "Words" in diagnostics
; Note: "@" is used as a space. ??? Verify
;	"_" is used as ??? Verify
;*****************************************************

L03D5:	.db	"ABCDEFGX"
L03DD:	.db	"123"
L03E0:	.db	$00, $6d, $f4, $2e, $62, $9d, $d4, '*'	;Could this be ROM checksums???
L03E8:	.db	"YES@"
L03EC:	.db	"NO"
L03EE:	.db	"UP"
L03F0:	.db	"DN"
L03F2:	.db	"LF"
L03F4:	.db	"RT"
L03F6:	.db	"_"
L03F7:	.db	"ERROR"
L03FC:	.db	"FIRE"
L0400:	.db	"MOVE@DI"
L0407:	.db	'R'
L0408:	.db	"PL1PL2CO"
L0410:	.db	"IN"
L0412:	.db	"SLAM"
L0416:	.db	"SW1SW2SW3S"
L0420:	.db	"W4SW5SW6SW7SW8"
	.db	"SCREEN@RAM@O"
L043A:	.db	"KSTATIC@RAM@OK@"
L0449:	.db	"STATIC@RAM@BAD"
L0457:	.db	"ROM@"
	.db	$00		; ???
;
;*****************************************************
; ???
;*****************************************************
;
L045C:	ld	 c,$10
	in	 c,(c)
;
;*****************************************************
; Write string to screen.
; Set up "expand mode color in A
; and "???" in C
;*****************************************************
;
L0460:	di			; No interruptions
	out	 ($19),a	; Expand mode color ???
	bit	 7,c		; Check for???
	ld	 a,$08		; Setup for expand mode only... !!! change this to binary
	jr	 nz,L046B	;   and skip ahead.
	set	 6,a		; Otherwise, set ??? bit
L046B:	out	 ($0C),a	; Out to "Magic RAM control"
L046D:	ld	 a,(hl)		; Get the character of the string
	inc	 hl		; Set up for next character 
	push	 hl		; Save next character location
	push	 de		; Save the color of character
	call	 L04AB		; Translate ASCII into graphic location
	pop	 de		; Restore color of character
	bit	 7,c		; Check for cocktail again??? unless L04AB changed c... check it. ???
	ld	 a,$26		; Set normal line offset value
	jr	 nz,L047D	; ... skip the modification for ???
	xor	 $30		; Modification to ???
L047D:	out	 ($7A),a	; Set up line offset value???
	ld	 a,l		; 
	out	 ($78),a	; LSB of source	
L0482:	ld	 a,h
	out	 ($79),a	; MSB of source
	ld	 a,e
	out	 ($7B),a	; LSB of destination
	ld	 a,d
	out	 ($7C),a	; MSB of destination
	bit	 7,c		; Check for cocktail... ???
	ld	 a,$4F		; Set up for normal ???
	jr	 nz,L0493	; Skip the mod for ??? if not cocktail
	ld	 a,$B1		; Otherwise, set up for ???
L0493:	out	 ($7B),a	; ??? what is this ???
	ld	 a,$01		
	out	 ($7D),a	; Set width of pattern
	ld	 a,$09
	out	 ($7E),a	; Height of pattern and start transfer! 
	pop	 hl		; Restore the next character to HL
	inc	 de		; 
	inc	 de		; Why did we double DE???
	bit	 7,c		; cocktail???
	jr	 nz,L04A8	; ... yes, skip mod of DE for ???
	dec	 de
	dec	 de
	dec	 de
	dec	 de		; Mod for ???
L04A8:	djnz	 L046D		; Character finished, go back and do 
				; next character until all of string is finished.
	ret			; String is done, return to sender...
;
;********************************************************************
; Name:			L04AB
;
; Title:		ASCII to GRAPHICS Table lookup
;
; Function:		Take an ASCII character and translate it into
;			a graphic entry in a character table. It is not
;			true ASCII, but a subset with some modifications.
;			See character table at L04CE for details.
;			It also handles translation to alternate ROM set.
; 
; Entry:		A  = ASCII character
;			
; Exit:			HL = Address entry in table which corrosponds 
;			     with the ASCII character.
;
; Registers used:	A, DE, HL  ???verify 
;
;********************************************************************
;
L04AB:	sub	 $30		; Turn ASCII into table entry	
	cp	 $0A		; Check for number 0-9
	jr	 c,L04B3	; Jump if not a number...
	sub	 $06		; Adjust to take out unsupported characters 
				; ... between "9" and "A" (":;<=>?")
L04B3:	ld	 l,a		; L = table entry
	sub	 $2C		; Check for alternate characters???
	ld	 de,L04CE	; Entry to character table
	jr	 c,L04C2	; Skip alternate ROM set
	ld	 hl,LC00B	; Entry of table to alternate ROM character set
	ld	 e,(hl)		; \
	inc	 hl		;  Manipulations for Alternate ROM set.
	ld	 d,(hl)		;  Need to figure out what it does later... ???
	ld	 l,a		; /
L04C2:	ld	 h,$00		; 
	add	 hl,hl		; Begin multiplying HL * $0A
	push	 de		; Push beginning of table address
	ld	 d,h		; 
L04C7:	ld	 e,l		;
	add	 hl,hl		; 
	add	 hl,hl		; 
	add	 hl,de		; Multiply HL * $0A complete. This is table offset.
	pop	 de		; Restore beginning of table address
	add	 hl,de		; Add start of table address with offset to give table entry.
	ret			; Thou are done with thy translation!

;******************************************************************************
; Name:		Graphic_Character_Table
; Games:	Wizard of Wor (horizontal monitor)
; Purpose:	This is the Wizard of Wor character set in graphical format.
; Description:	This is the data area for alphabetic and a few special 
;		characters in graphic format. It has the following charastics:
;
;		   1. Character patterns are stored in a bitmap 1 by 5 bytes
;		   2. Every bit is doubled by theMagic Expand mode.
;		   3. The pixel color it is converted to is based on the ??? 
;		      register, which tells what to expand each set bit
;		      or reset bit into. 
;		   4. Each character is $0A apart. 
;		
;Example:	Character "A"
;			
;		Data	Expanded	Expanded two 
;			one bit		bits per byte
;			per byte
;
;		$18	---XX---	------XXXX------	
;		$3C	--XXXX--	----XXXXXXXX----
;		$7E	-XXXXXX-	--XXXXXXXXXXXX--
;		$66	-XX--XX-	--XXXX----XXXX--
;		$66	-XX--XX-	--XXXX----XXXX--
;		$66	-XX--XX-	--XXXX----XXXX--
;		$7E	-XXXXXX-	--XXXXXXXXXXXX--
;		$7E	-XXXXXX-	--XXXXXXXXXXXX--
;		$66	-XX--XX-	--XXXX----XXXX--
;		$66	-XX--XX-	--XXXX----XXXX--
;

;
;
;******************************************************************************
;
L04CE:

	.db	$3C,$7E,$66,$66,$66,$66,$66,$66,$7E,$3C		; "0" 
	.db	$18,$38,$18,$18,$18,$18,$18,$18,$3C,$3C		; "1"
	.db	$3C,$7E,$66,$06,$3E,$7C,$60,$60,$7E,$7E		; "2"
	.db	$3C,$7E,$66,$06,$1C,$1E,$06,$66,$7E,$3C		; "3"
	.db	$66,$66,$66,$66,$7E,$7E,$06,$06,$06,$06		; "4"
	.db	$7C,$7C,$60,$60,$7C,$7E,$06,$66,$7E,$3C		; "5"
	.db	$3C,$7C,$60,$60,$7C,$7E,$66,$66,$7E,$3C		; "6"
	.db	$7E,$7E,$06,$0E,$0C,$1C,$18,$38,$30,$30		; "7"
	.db	$3C,$7E,$66,$66,$3C,$7E,$66,$66,$7E,$3C		; "8"
	.db	$3C,$7E,$66,$66,$7E,$3E,$06,$06,$3E,$3C		; "9"
	.db	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		; "space"
	.db	$18,$3C,$7E,$66,$66,$66,$7E,$7E,$66,$66		; "A" 
	.db	$7C,$7E,$66,$66,$7C,$7E,$66,$66,$7E,$7C		; "B"
	.db	$3C,$7E,$66,$60,$60,$60,$60,$66,$7E,$3C		; "C"
	.db	$7C,$7E,$66,$66,$66,$66,$66,$66,$7E,$7C		; "D"
	.db	$7E,$7E,$60,$60,$7C,$7C,$60,$60,$7E,$7E		; "E"
	.db	$7E,$7E,$60,$60,$7C,$7C,$60,$60,$60,$60		; "F"
	.db	$3C,$7E,$60,$60,$60,$6E,$6E,$66,$7E,$3C		; "G"
	.db	$66,$66,$66,$66,$7E,$7E,$66,$66,$66,$66		; "H"
	.db	$3C,$3C,$18,$18,$18,$18,$18,$18,$3C,$3C		; "I"
	.db	$06,$06,$06,$06,$06,$06,$66,$66,$7E,$3C		; "J"
	.db	$66,$66,$6E,$7C,$78,$78,$6C,$6E,$66,$66		; "K"
	.db	$60,$60,$60,$60,$60,$60,$60,$60,$7E,$7E		; "L"
	.db	$C3,$E7,$E7,$DB,$DB,$C3,$C3,$C3,$C3,$C3		; "M"
	.db	$66,$66,$76,$7E,$7E,$6E,$66,$66,$66,$66		; "N"
	.db	$3C,$7E,$66,$66,$66,$66,$66,$66,$7E,$3C		; "O"
	.db	$7C,$7E,$66,$66,$7E,$7C,$60,$60,$60,$60		; "P"
	.db	$3C,$7E,$66,$66,$66,$66,$66,$6E,$64,$3A		; "Q"
	.db	$7C,$7E,$66,$66,$7E,$7C,$6E,$66,$66,$66		; "R"
	.db	$3C,$7E,$66,$60,$7C,$3E,$06,$66,$7E,$3C		; "S"
	.db	$7E,$7E,$18,$18,$18,$18,$18,$18,$18,$18		; "T"
	.db	$66,$66,$66,$66,$66,$66,$66,$66,$7E,$3C		; "U"
	.db	$66,$66,$66,$66,$66,$7E,$3C,$3C,$18,$18		; "V"
	.db	$C3,$C3,$C3,$DB,$DB,$DB,$FF,$E7,$C3,$C3		; "W"
	.db	$66,$66,$7E,$3C,$18,$18,$3C,$7E,$66,$66		; "X"
	.db	$66,$66,$7E,$3C,$18,$18,$18,$18,$18,$18		; "Y"
	.db	$7E,$7E,$06,$0E,$1C,$38,$70,$60,$7E,$7E		; "Z"
	.db	$3C,$42,$99,$A5,$A1,$A1,$A5,$99,$42,$3C		; "Copyright Symbol"
	.db	$38,$20,$60,$40,$FF,$FF,$40,$60,$20,$38		; "Left arrow"      
	.db	$18,$18,$3C,$3C,$7E,$5A,$DB,$99,$18,$18		; "Up arrow"        
	.db	$18,$18,$99,$DB,$5A,$7E,$3C,$3C,$18,$18		; "Down arrow"      
	.db	$00,$00,$00,$00,$7E,$7E,$00,$00,$00,$00		; "Dash"            
	.db	$1C,$1C,$1C,$08,$08,$00,$00,$00,$00,$00		; "Apostrophe"      
	.db	$1C,$04,$06,$02,$FF,$FF,$02,$06,$04,$1C		; "Right arrow"
	
	nop	; Not sure why there is a NOP here... left in just in case.
;
;*************************************************
;
;
;*************************************************
L0687:	call	 L06B5
	ld	 hl,LD1BF
	ld	 a,(hl)
	and	 a
	ret	 z
	inc	 hl
	dec	 (hl)
	ret	 nz
	ld	 (hl),$02
	dec	 hl
	dec	 (hl)
	ld	 a,(hl)
	sub	 $09
	cpl	
	ld	 e,a
	call	 L06D9
	ld	 a,e
	cp	 $08
	ret	 c
	ld	 a,(L00C8)
	out	 ($04),a
	xor	 a
	ld	 (LD1BA),a
	ld	 (LD050),a
	inc	 a
	ld	 (LD048),a
	jr	 L06C8
L06B5:	ld	 hl,LD1C1
	ld	 a,(hl)
	and	 a
	ret	 z
	inc	 hl
	dec	 (hl)
	ret	 nz
	ld	 (hl),$04
	dec	 hl
	dec	 (hl)
	ld	 e,(hl)
	call	 L06D9
	dec	 e
	ret	 p
;
;*********************************************************
; Set scan line interrupt and 
; turn on all three sparkle colors
;*********************************************************
;
L06C8:	ld	 a,$A8		; Set the interrupt line to 162
	out	 ($0F),a	 

L06CC:	ld	 a,00000111b	; Turn on sparkle color 1	
	in	 a,($15)	

	ld	 a,00001001b	; Turn on sparkle color 2
	in	 a,($15)

	ld	 a,00001011b	; Turn on sparkle color 3		
	in	 a,($15)

	ret
;
;*********************************************************
;
L06D9:	ld	 d,$00
	ld	 hl,$06F3
	add	 hl,de
	add	 hl,de
L06E0:	add	 hl,de
	ld	 a,(LD1BA)
	and	 a
	jr	 nz,L06E8
L06E7:	ld	 a,(hl)
L06E8:	out	 ($07),a
	inc	 hl
	ld	 a,(hl)
	out	 ($06),a
	inc	 hl
	ld	 a,(hl)
	out	 ($05),a
	ret
;
;*********************************************************
;
	ld	 d,c
	ld	 a,h
	di	
	ld	 d,c
	ld	 a,e
	di	
	ld	 d,c
	ld	 a,e
	di	
	ld	 d,c
	ld	 a,d
	jp	 p,L7A51
	jp	 p,L7950
	pop	 af
	ld	 d,b
	ld	 a,c
	pop	 af
	ld	 d,b
	ld	 a,b
	ret	 p
	ld	 d,b
L070C:	ld	 a,b
	ret	 p
L070E:	ld	 a,$04
	in	 a,($15)
	jr	 L06C8

L0714:	ld	 hl,LD1BB
	ld	 a,(hl)
	and	 a
	ret	 z
	ld	 a,$CC
	out	 ($0F),a
	inc	 hl
	ld	 a,(hl)
	and	 a
	jr	 z,L0725
	dec	 (hl)
	ret
;
;*********************************************************
;
L0725:	ld	 (hl),$01
	dec	 hl
	ld	 a,$05
	in	 a,($15)
	dec	 (hl)
	call	 z,L070E
	ld	 a,(hl)
L0731:	sub	 $10
	jr	 nc,L0731
	add	 a,$10
	ld	 c,a
	ld	 b,$00
	ld	 hl,L0742
	add	 hl,bc
L073E:	ld	 a,(hl)
	out	 ($04),a
	ret
;
;*********************************************************
;
;
; Data Below?
; 
L0742:	rst	 00H
	ld	 a,b
	ld	 c,e
	jr	 L0702
	ld	 l,b
	in	 a,($58)
	dec	 hl
	ex	 af,af'
	xor	 e
	adc	 a,b
	dec	 sp
	sbc	 a,b
	set	 7,e
L0752:	ld	 hl,$06F3
	call	 L06E7
	jp	 L06C8
;
; End Data?
;
L075B:	ld	 hl,LD1BD
	ld	 a,(hl)
	and	 a
	ret	 z
	ld	 a,$CC
	out	 ($0F),a
	inc	 hl
	ld	 a,(hl)
	and	 a
	jr	 z,L076C
	dec	 (hl)
	ret
;
;*********************************************************
;
L076C:	ld	 (hl),$03
	dec	 hl
	dec	 (hl)
	jr	 z,L0752
	bit	 0,(hl)
	ld	 a,$07
	jr	 z,L0779
	xor	 a
L0779:	out	 ($07),a
	out	 ($06),a
	out	 ($05),a
	ret
;
;*********************************************************
;
	nop	
L0781:	call	 L0894
	call	 L07A8
	xor	 a
	jp	 L045C
;
;*********************************************************
;
L078B:	ld	 a,(LD347)
	in	 a,($13)		; Check for language...
	bit	 3,a			; Off=Foreign, On=English (active HIGH)
	ld	 hl,L3131
	jr	 nz,L079A
	ld	 hl,LC00D
L079A:	ld	 a,(hl)
	inc	 hl
	cp	 $30
	jr	 nc,L079A
	djnz	 L079A
	ld	 b,a
	ld	 a,(LD1E2)
	add	 a,b
	ret
;
;*********************************************************
;
L07A8:	call	 L0875
	push	 hl
	call	 L0875
	in	 a,($10)		; Unknown what bit 7 does... 
	bit	 7,a
	jr	 nz,L07B6
	ex	 (sp),hl
L07B6:	pop	 hl
	ex	 de,hl
	ret
;
;*********************************************************
;
	call	 L0875
	call	 L0894
	call	 L0886
	call	 L0880
	ld	 c,$FF
	jp	 L0460
;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;L07CA:
	call	 L0886
	call	 L0894
	call	 L07A8
L07D3:	call	 L0880
	jp	 L045C
;
	call	 L0894
	call	 L07A8
	call	 L078B
	jr	 L07D3
;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;
L07E4:	call	 L0894
	call	 L078B
	sub	 $29
	cpl	
	ld	 e,a
	call	 L088B
	in	 a,($10)	
	bit	 7,a		;Unknown what bit 7 does on port $10
	call	 L0880
	jr	 nz,L07FF
	ld	 d,a
	ld	 a,$4F
	sub	 e
	ld	 e,a
L07FF:	push	 hl
L0800:	call	 L0947
	ex	 de,hl
	pop	 hl
	jr	 L07D3
;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;
L0807:	call	 L0872		
	ld	 (hl),a
L080B:	ret	

;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;
L080C:	call	 L0886
L080F:	call	 L0875
	ld	 (hl),e
	inc	 hl
	ld	 (hl),d
	ret
;
	call	 L0872
	call	 L0886
	cp	 (hl)
	ret	 z
	jr	 L083E
	call	 L0886
L0823:	call	 L0875
	ld	 a,(de)
	and	 a
	ret	 nz
	jr	 L086E
	call	 L0886
	call	 L0875
	ld	 a,(de)
	and	 a
	ret	 z
	jr	 L086E
L0836:	call	 L0872
	call	 L0886
	cp	 (hl)
	ret	 c
L083E:	push	 de
	pop	 iy
	ret	
	in	 a,($10)
	and	 (iy+$00)
	inc	 iy
	call	 L0875
	ret	 z
	jr	 L086E
L084F:	xor	 a
	ld	 (LD050),a
	call	 L0880
	ld	 (LD048),a
	ret	
	xor	 a
	ld	 (LD053),a
	call	 L0872
	ld	 (LD051),hl
	ld	 (LD042),a
	ret	
	ld	 l,(iy+$00)
	ld	 h,(iy+$01)
L086E:	push	 hl
	pop	 iy
	ret
;
;*********************************************************
; Sub to A=(IY), IY=IY+1
;*********************************************************
;
L0872:	call	 L0880
;
;*********************************************************
;
;  Load H=(IY+1), L=(IY), IY=IY+2
;  Why are we loading up HL with these values?
;
;*********************************************************
;
L0875:	ld	 l,(iy+$00)
	inc	 iy
	ld	 h,(iy+$00)
	inc	 iy
	ret
;
;*********************************************************
;
; Load A=(IY), IY=IY+1
;
;*********************************************************
;
L0880:	ld	 a,(iy+$00)
	inc	 iy
	ret
;
;*********************************************************
;
; Load E=(IY+1), D=(IY), IY=IY+2
;
;*********************************************************
;
L0886:	ld	 e,(iy+$00)
	inc	 iy
L088B:	ld	 d,(iy+$00)
	inc	 iy
	ret
;
;*********************************************************
;
; Load C=(IY), B=(IY+1)
;
;*********************************************************
;
	call	 L089A
L0894:	ld	 b,(iy+$00)
	inc	 iy
	ret
;
;*********************************************************
;
; Load C=(IY)
;
;*********************************************************
;
L089A:	ld	 c,(iy+$00)
	inc	 iy
	ret	
;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;
L08A0:	pop	 hl		; Return address in HL 
	call	 L0886		; Load E=(IY+1), D=(IY), IY=IY+2
	push	 iy		; Save old subroutine pointer ???
	push	 de
	pop	 iy		; Setup subroutine pointer to new area ???
	jp	 (hl)		; Return...
;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;L08AA:
	pop	 hl
	pop	 iy
	jp	 (hl)
;
;***********************************************************************
;
; I think this routine sets video for game play and fills some
; static RAM memory with 0's. 
; Note: In the pattern xfer below, the source destination regisers are 
; missing. This probably means they were set before the call ???
;
;
; Details and film at eleven...
;
;***********************************************************************
;
L08AE:	di	
	xor	 a		; Zero A
	out	 ($19),a	; Magic RAM expand mode color ???
	ld	 a,$08
	out	 ($0C),a	; Set vertical blanking (see description) ???
	ld	 a,$22
	out	 ($7A),a	; Mode control byte ???
	xor	 a		; A=0
	out	 ($7B),a	; LSB of destination address
	out	 ($7C),a	; MSB of destination address
	inc	 a		; A=1
L08C0:	out	 ($7B),a	; Line offset value ???
	ld	 a,$4F
	out	 ($7D),a	; Width of pattern	
	ld	 a,$CB
	out	 ($7E),a	; Height of pattern and start transfer

; NOTE: After the execution of the previous instruction, the screen is cleared!
; So is this a "clear screen" routine using the pattern board???
;
; What is this section of static RAM used for?
; This section of code blanks out $d040 to $d243 inclusive.
; Also note that the first $40 (64 decimal) bytes is not included.
;
	ld	 hl,LD040
	ld	 (hl),$00
	ld	 de,LD041
	ld	 bc,L0203
	ldir
	
	ld	 a,(L8006)
	cp	 $C3
	call	 z,L8006
	ret	
;
;************************************************************************
;
	call	 L089A
	call	 L0880
	ex	 af,af'
	call	 L0875
	exx	
	call	 L07A8
	ex	 de,hl
	exx	
L08F0:	ld	 a,(hl)
	inc	 hl
	exx	
	push	 hl
	call	 L04AB
	ex	 de,hl
	pop	 hl
	exx	
	ld	 b,$0A
L08FC:	exx	
	ld	 b,$08
	ex	 af,af'
	ld	 c,a
	ex	 af,af'
	ld	 a,(de)
	inc	 de
L0904:	rla	
	jr	 nc,L0908
	ld	 (hl),c
L0908:	inc	 hl
	push	 af
	in	 a,($10)
	bit	 7,a
	jr	 nz,L0912
	dec	 hl
	dec	 hl
L0912:	pop	 af
	djnz	 L0904
	in	 a,($10)
	bit	 7,a
	ld	 c,$E8
	jr	 nz,L0920
	ld	 bc,LFF18
L0920:	add	 hl,bc
	exx	
	djnz	 L08FC
	exx	
	ld	 bc,LF6A8
	in	 a,($10)
	bit	 7,a
	jr	 nz,L0931
	ld	 bc,L0958
L0931:	add	 hl,bc
	exx	
	dec	 c
	jr	 nz,L08F0
	ret	
	nop	
L0938:	push	 hl
	srl	 e
	srl	 e
	call	 L0947
	ld	 de,L0007
	add	 hl,de
	ex	 de,hl
	pop	 hl
	ret	
L0947:	ld	 l,d
	ld	 h,$00
	ld	 d,h
	add	 hl,hl
	add	 hl,hl
	add	 hl,hl
	add	 hl,hl
	push	 hl
	add	 hl,hl
	add	 hl,hl
	add	 hl,de
	pop	 de
	add	 hl,de
	ret	
	push	 af
L0957:	push	 bc
L0958:	push	 de
	push	 hl
	ex	 af,af'
	push	 af
	exx	
	push	 bc
	push	 de
	push	 hl
L0960:	push	 ix
	call	 L0E2B
	call	 L09D1
	call	 L0979
	pop	 ix
	pop	 hl
	pop	 de
	pop	 bc
	exx	
	pop	 af
	ex	 af,af'
	pop	 hl
	pop	 de
	pop	 bc
	pop	 af
	ei	
	ret	
L0979:	ld	 a,(LD1C5)
	and	 a
	ret	 nz
	ld	 hl,LD038
	ld	 a,(hl)
	inc	 a
	call	 L098B
	ld	 hl,LD03E
	ld	 a,r
L098B:	and	 $0F
	ld	 c,a
	rlca	
	rlca	
	rlca	
	rlca	
	or	 c
	ld	 c,a
	cpl	
	ld	 b,a
	call	 L0F6A		
	inc	 hl
	ld	 c,b
	jp	 L0F6A		
	push	 af
	ld	 a,(LD1C4)
	add	 a,$2C
	out	 ($0F),a
	call	 L00A5
	ld	 a,$0A
	in	 a,($15)
	ld	 a,$52
	out	 ($07),a
	pop	 af
	ei	
	ret
	
;
;************************************************************************
;
	push	 af
	ld	 a,(LD1C4)
	out	 ($0F),a
	call	 L00A0
	ld	 a,$0B
	in	 a,($15)
	ld	 a,$51
	out	 ($07),a
	jr	 L0957
	nop	
L09C8:	ld	 hl,LD003
	ld	 c,$00
	call	 L0F6A		
	rst	 00H
L09D1:	ld	 a,(LD1C5)
	and	 a
	jp	 nz,L0A68
	in	 a,($10)		; Check for TILT
	and	 $10			; Bit 4, active LOW
	jr	 z,L09C8		; Jump to TILT routine
	ld	 a,(LD03A)
	cp	 $05
	jr	 nc,L09C8
	ld	 a,(LD34E)
	rra	
	jr	 c,L0A25
	ld	 hl,LD05C
	call	 L0B62
	ld	 hl,LD09C
	call	 L0B62
	ld	 hl,LD0E8
	call	 L0B62
	ld	 hl,LD134
L0A00:	call	 L0B62
	ld	 hl,LD067
L0A06:	call	 L0AA6
	ld	 hl,LD0A7
	call	 L0AA6
	ld	 hl,LD0F3
	call	 L0AA6
L0A15:	ld	 hl,LD13F
	call	 L0AA6
	call	 L0BD7
	call	 L075B
	ld	 a,$05
	jr	 L0A5D
L0A25:	ld	 hl,LD07C
L0A28:	call	 L0B62
	ld	 hl,LD0C2
	call	 L0B62
	ld	 hl,LD10E
	call	 L0B62
	ld	 hl,LD15A
	call	 L0B62
	ld	 hl,LD087
	call	 L0AA6
	ld	 hl,LD0CD
	call	 L0AA6
	ld	 hl,LD119
	call	 L0AA6
	ld	 hl,LD165
	call	 L0AA6
	call	 L0BE7
	call	 L0714
	ld	 a,$0A
L0A5D:	ld	 hl,LD1C3
	or	 (hl)
	ld	 (hl),a
	call	 L0A72
	call	 L0687
L0A68:	ld	 a,(L8000)
	cp	 $C3
	call	 z,L8000
	ret	
	nop	
L0A72:	call	 L0A82
	ld	 hl,LD048
	inc	 de
	ld	 a,(de)
	and	 a
	ret	 nz
	ld	 b,$08
	call	 L0A95
	ret	
L0A82:	ld	 de,LD040
	ld	 a,(de)
	and	 a
	ret	 nz
	ld	 hl,LD34E
	dec	 (hl)
	ret	 nz
	ld	 (hl),$3C
	ld	 bc,L0601
	ld	 hl,LD042
L0A95:	ld	 a,(hl)
	and	 a
	jr	 z,L0A9D
	dec	 (hl)
	jr	 nz,L0A9D
	scf	
L0A9D:	rl	 c
	inc	 hl
L0AA0:	djnz	 L0A95
	ld	 a,c
	ld	 (de),a
	ret	
	nop	
L0AA6:	bit	 7,(hl)
L0AA8:	ret	 z
	ld	 a,$0C
	out	 ($19),a
	ld	 a,$28
	out	 ($0C),a
	bit	 3,(hl)
	jr	 z,L0AB9
	res	 7,(hl)
	jr	 L0AE8
L0AB9:	push	 hl
	call	 L0AE8
	pop	 hl
	bit	 5,(hl)
	ret	 nz
	push	 hl
	inc	 hl
	ld	 a,(hl)
	inc	 hl
	add	 a,(hl)
	jr	 z,L0AC9
	ld	 (hl),a
L0AC9:	ld	 e,a
	inc	 hl
	ld	 a,(hl)
	inc	 hl
	add	 a,(hl)
	ld	 (hl),a
	ld	 d,a
	call	 L0938
	inc	 hl
	ld	 (hl),e
	inc	 hl
	ld	 (hl),d
	in	 a,($08)
	pop	 hl
	ld	 a,(hl)
	push	 hl
	call	 L0AF5
	pop	 hl
	set	 6,(hl)
	in	 a,($08)
	and	 a
	ret	 z
	set	 5,(hl)
L0AE8:	bit	 6,(hl)
	ret	 z
	res	 6,(hl)
	ld	 a,(hl)
	ld	 de,L0005
	add	 hl,de
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
L0AF5:	ex	 de,hl
	ld	 de,L004F
	bit	 1,a
	jr	 nz,L0B2E
	bit	 2,a
	jr	 nz,L0B17
	ld	 a,$18
	call	 L0B06
L0B06:	ld	 (hl),a
L0B07:	inc	 hl
	ld	 (hl),a
	add	 hl,de
	ld	 (hl),a
	inc	 hl
	ld	 (hl),a
	add	 hl,de
	ld	 (hl),a
	inc	 hl
	ld	 (hl),a
	add	 hl,de
	ld	 (hl),a
	inc	 hl
	ld	 (hl),a
	add	 hl,de
	ret	
L0B17:	inc	 de
	ld	 bc,L2244
	ld	 a,(LD1C6)
	and	 a
	call	 z,L0B25
	call	 L0B25
L0B25:	ld	 (hl),b
	add	 hl,de
	ld	 (hl),b
	add	 hl,de
	ld	 (hl),c
	add	 hl,de
	ld	 (hl),c
	add	 hl,de
	ret	
L0B2E:	bit	 2,a
	jr	 nz,L0B3C
	ld	 a,$FF
	ld	 (hl),a
	inc	 hl
	ld	 (hl),a
	add	 hl,de
	ld	 (hl),a
	inc	 hl
	ld	 (hl),a
	ret	
L0B3C:	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L0B55
	dec	 de
	ld	 (hl),$99
	inc	 hl
	ld	 (hl),$99
	inc	 hl
	ld	 (hl),$9E
	add	 hl,de
	ld	 (hl),$9E
	inc	 hl
	ld	 (hl),$67
	inc	 hl
	ld	 (hl),$67
	ret	
L0B55:	ld	 (hl),$99
	inc	 hl
	ld	 (hl),$99
	add	 hl,de
	ld	 (hl),$E7
	inc	 hl
	ld	 (hl),$E7
	ret	
	nop	
L0B62:	bit	 7,(hl)
	ret	 z
	res	 7,(hl)
	push	 hl
	bit	 6,(hl)
	res	 6,(hl)
	call	 nz,L0B89
	pop	 hl
	bit	 5,(hl)
	ret	 z
	push	 hl
	pop	 ix
	ld	 a,(ix-$04)
	ld	 (ix+$16),a
	ld	 a,(ix-$02)
	ld	 (ix+$17),a
	push	 ix
	pop	 hl
	ld	 a,(hl)
	xor	 $70
	ld	 (hl),a
L0B89:	bit	 4,(hl)
	ld	 de,L0005
	jr	 z,L0B91
	add	 hl,de
L0B91:	inc	 hl
L0B92:	di	
	ld	 a,(hl)
	out	 ($0C),a
	ld	 de,L0005
	bit	 7,a
	jr	 z,L0B9F
	set	 4,d
L0B9F:	bit	 6,a
	jr	 nz,L0BA5
	set	 5,d
L0BA5:	ld	 a,d
	or	 $0C
	out	 ($7A),a
L0BAA:	bit	 5,a
	jr	 z,L0BB0
	ld	 e,$FB
L0BB0:	bit	 4,a
	ld	 a,$50
	jr	 z,L0BB8
	ld	 a,$B0
;
;*****************************************************************************
; First seen drawing the monsters on the demo screen! ???
;*****************************************************************************
;

L0BB8:	add	 a,e
	ld	 e,a
	inc	 hl
	ld	 a,(hl)
	out	 ($78),a
	inc	 hl
	ld	 a,(hl)
	out	 ($79),a
	inc	 hl
	ld	 a,(hl)
	out	 ($7B),a
	inc	 hl
	ld	 a,(hl)
	out	 ($7C),a
	ld	 a,e
	out	 ($7B),a
	ld	 a,$05
	out	 ($7D),a
	ld	 a,$11
	out	 ($7E),a
	ret	
	nop	
L0BD7:	push	 iy
	ld	 iy,LD054
	ld	 ix,LD074
	call	 L0C2B
	pop	 iy
	ret	
L0BE7:	push	 iy
	ld	 iy,LD074
	ld	 ix,LD054
	call	 L0C2B
	pop	 iy
	ret	
L0BF7:	push	 iy
	ld	 a,$06
L0BFB:	push	 af
	call	 L0F2C
	call	 L0C09
	pop	 af
L0C03:	dec	 a
	jr	 nz,L0BFB
	pop	 iy
	ret	
L0C09:	ld	 a,(iy+$13)
L0C0C:	bit	 5,a
	ret	 z
	ld	 ix,LD074
	call	 L0C49
L0C16:	ld	 ix,LD054
	call	 L0C49
	ld	 (iy+$13),$00
	ld	 a,(LD1C6)
	and	 a
	ret	 nz
	ld	 (iy+$1c),$0F
	ret	
L0C2B:	ld	 a,(iy+$13)
	bit	 5,a
	ret	 z
	call	 L0C49
	call	 L0E0B
	ld	 a,$06
L0C39:	push	 af
	call	 L0F1F
	call	 L0C49
	pop	 af
	dec	 a
	jr	 nz,L0C39
	ld	 (iy+$13),$00
	ret	
L0C49:	ld	 a,(ix+$00)
	and	 $9A
	cp	 $80
	ret	 nz
	bit	 1,(iy+$13)
L0C55:	jr	 z,L0C6F
	ld	 hl,L0D2C
	call	 L0D18
	bit	 2,(iy+$13)
	jr	 z,L0C85
	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L0C85
	inc	 b
	inc	 b
	inc	 b
	inc	 b
	jr	 L0C85
L0C6F:	ld	 hl,L0D3C
	call	 L0D18
	bit	 2,(iy+$13)
	jr	 z,L0C85
	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L0C85
	inc	 d
	inc	 d
L0C83:	inc	 d
	inc	 d
L0C85:	push	 bc
	xor	 a
	ld	 h,a
	ld	 b,a
	ld	 l,(ix+$1e)
	ld	 c,(iy+$15)
	sbc	 hl,bc
	pop	 bc
	ld	 a,l
	jr	 c,L0C98
	cp	 $EA
	ret	 nc
L0C98:	add	 a,c
	cp	 b
	ret	 nc
	ld	 a,(ix+$1f)
	sub	 (iy+$17)
	add	 a,e
	cp	 d
	ret	 nc
	call	 L0D4C
	bit	 2,(iy+$07)
	ret	 nz
	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L0CB6
	set	 3,(ix+$13)
L0CB6:	bit	 2,(iy+$08)
	ld	 hl,LD1E1
	ld	 a,$04
	jr	 nz,L0CC2
	rlca	
L0CC2:	or	 (hl)
	ld	 (hl),a
	bit	 3,(iy+$08)
	ld	 hl,LD31B
	jr	 z,L0CCF
	inc	 hl
	inc	 hl
L0CCF:	ld	 d,$00
	ld	 a,$10
	bit	 2,(ix+$07)
	jr	 z,L0CF3
	ld	 e,$05
	ld	 a,(ix+$08)
	and	 $0C
	jp	 pe,L0CFC
	ld	 e,$01
	bit	 2,a
	jr	 nz,L0CEA
	inc	 e
L0CEA:	ld	 a,(LD351)
	and	 a
	ld	 a,e
	jr	 z,L0CF3
	add	 a,a
	daa	
L0CF3:	add	 a,(hl)
	daa	
	ld	 (hl),a
	inc	 hl
	ld	 a,d
	adc	 a,(hl)
	daa	
	ld	 (hl),a
	ret	
L0CFC:	ld	 a,(LD1EB)
	and	 a
	jr	 z,L0CEA
L0D02:	ld	 (LD352),a
	ld	 a,(LD1C6)
	and	 a
	ld	 e,$10
	jr	 z,L0CEA
	ld	 (LD1C8),a
	xor	 a
	ld	 (LD1D8),a
	ld	 e,$25
	jr	 L0CEA
L0D18:	ld	 a,(ix+$07)
	and	 $03
	ld	 c,a
	ld	 b,$00
	add	 hl,bc
	add	 hl,bc
	add	 hl,bc
	add	 hl,bc
	ld	 c,(hl)
	inc	 hl
	ld	 b,(hl)
	inc	 hl
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	ret	
L0D2C:	inc	 d
	add	 hl,de
	ld	 d,$15
	inc	 d
	add	 hl,de
	inc	 d
	dec	 d
	ld	 d,$1B
	inc	 d
	inc	 de
	inc	 d
	dec	 de
	inc	 d
L0D3B:	inc	 de
L0D3C:	ld	 de,$1613
	dec	 de
	ld	 de,L1413
	dec	 de
	inc	 de
	dec	 d
	inc	 d
	add	 hl,de
	ld	 de,$1415
	add	 hl,de
L0D4C:	res	 7,(ix+$08)
	set	 3,(ix+$00)
	ld	 (ix+$1d),$01
	ret	
L0D59:	ld	 (ix+$1d),$00
	call	 L0DD6
	ld	 (ix+$03),$01
	ld	 (ix+$05),$06
	ld	 b,(ix+$07)
	ld	 a,b
	rrca	
	rrca	
	rrca	
	and	 $80
	ld	 (ix+$07),a
	ld	 c,$00
	ld	 a,b
	and	 $03
	cp	 $02
	jr	 c,L0D87
	ex	 af,af'
	in	 a,($10)
	bit	 7,a
	jr	 nz,L0D86
	set	 7,c
L0D86:	ex	 af,af'
L0D87:	cp	 $01
	jr	 nz,L0D8D
	set	 7,c
L0D8D:	cp	 $02
	jr	 z,L0D9D
	cp	 $03
L0D93:	jr	 z,L0D9B
	ld	 a,(LD1DA)
	and	 a
	jr	 z,L0D9D
L0D9B:	set	 6,c
L0D9D:	ld	 (ix+$01),c
	ld	 c,(ix+$08)
	bit	 2,b
	ld	 a,(LD1C6)
	jr	 nz,L0DCF
	and	 a
	jr	 z,L0DB0
	ld	 (LD1C8),a
L0DB0:	ld	 a,c
	rlca	
	rlca	
	rlca	
	and	 $20
	ld	 h,a
	ld	 a,b
	rlca	
	rlca	
	rlca	
	and	 $10
	or	 h
	ld	 (ix+$07),a
	ld	 (ix+$05),$0F
	bit	 3,c
	ld	 hl,LD301
	jr	 nz,L0DCD
	dec	 hl
L0DCD:	dec	 (hl)
	ret	
L0DCF:	and	 a
	ret	 z
	ld	 (ix+$05),$01
	ret	
L0DD6:	bit	 2,(ix+$07)
	ld	 hl,LD241
	jr	 z,L0DFA
	inc	 hl
	ld	 a,(LD1EB)
	and	 a
	ld	 a,$02
	jr	 z,L0DF7
	rra	
	ld	 b,a
	ld	 a,(LD1C6)
	and	 a
	ld	 a,b
	jr	 z,L0DF7
	ld	 hl,LD243
	set	 0,(hl)
	ret	
L0DF7:	or	 (hl)
	ld	 (hl),a
	ret	
L0DFA:	set	 0,(hl)
	ld	 a,(LD1C6)
	and	 a
	ret	 z
	call	 L0F39
	and	 $07
	or	 $38
	jp	 L8009
L0E0B:	ld	 a,(ix+$15)
	sub	 (iy+$15)
	add	 a,$08
	cp	 $11
	ret	 nc
	ld	 a,(ix+$17)
	sub	 (iy+$17)
	add	 a,$08
	cp	 $11
	ret	 nc
	set	 3,(ix+$13)
	ld	 (ix+$1c),$0F
	ret	
	nop	
L0E2B:	ld	 hl,LD341
	ld	 d,$00
	call	 L0F00
	ld	 hl,LD343
	ld	 d,$02
	call	 L0F00
	ld	 hl,LD345
L0E3E:	ld	 d,$0E
	call	 L0F00
	ld	 a,(LD347)
	in	 a,($13)
	ld	 b,a
	exx	
	ld	 de,L0F1A
	bit	 3,a
	jr	 nz,L0E54
	ld	 de,LC004
L0E54:	exx	
	xor	 a
	ex	 af,af'
	ld	 c,$01
	ld	 e,$00
	call	 L0EBB
	call	 L0EC0
	ld	 hl,LD342
	call	 c,L0EF4
	ld	 c,$02
	ld	 e,c
	call	 L0EBB
	bit	 3,b
	jr	 z,L0E77
	ld	 a,b
	cpl	
	and	 $06
	rra	
	ld	 e,a
L0E77:	call	 L0EC0
	ld	 hl,LD344
	call	 c,L0EE3
	ld	 a,(LD1C5)
L0E83:	and	 a
	jr	 nz,L0E8A
	bit	 3,b
	jr	 nz,L0E99
L0E8A:	ld	 c,$04
	ld	 e,c
	call	 L0EBB
	call	 L0EC0
	ld	 hl,LD346
	call	 c,L0EF4
L0E99:	ex	 af,af'
	ld	 hl,LD03D
	add	 a,(hl)
	push	 af
	and	 $0F
	ld	 c,a
L0EA2:	call	 L0F6A		
	pop	 af
	rrca	
	rrca	
	rrca	
	rrca	
	and	 $0F
	ret	 z
	dec	 hl
	add	 a,(hl)
	cp	 $1F
	ret	 nc
	ld	 c,a
	call	 L0F6A		
	ld	 hl,LD340
	inc	 (hl)
	ret	
L0EBB:	ld	 a,b
	and	 c
	ret	 nz
	inc	 e
	ret	
L0EC0:	ld	 hl,LD03B
	in	 a,($10)
	and	 c
	ld	 d,(hl)
	ld	 a,$A5
	out	 ($5B),a	
	jr	 nz,L0ED1
	ld	 a,d
	or	 c
	ld	 (hl),a
	ret	
L0ED1:	ld	 a,d
	and	 c
	ret	 z
	ld	 a,d
	xor	 c
	ld	 (hl),a
	ld	 a,e
	exx	
	ld	 l,a
	ld	 h,$00
	add	 hl,de
	ex	 af,af'
	add	 a,(hl)
	ex	 af,af'
	exx	
	scf	
	ret	
L0EE3:	bit	 3,b
	jr	 z,L0EF4
	bit	 2,b
	jr	 z,L0EF4
	ld	 a,b
	and	 $03
	jp	 po,L0EF4
	ld	 hl,LD342
L0EF4:	inc	 (hl)
	ld	 hl,LD240
	set	 5,(hl)
	ld	 a,$01
	ld	 (LD244),a		; Turn on sounds in attract mode variable
	ret	
L0F00:	ld	 a,(hl)
L0F01:	and	 a
	jr	 z,L0F0C
	dec	 (hl)
	cp	 $0F
L0F07:	ret	 nz
	ld	 a,d
	in	 a,($15)
L0F0B:	ret	
L0F0C:	inc	 hl
	ld	 a,(hl)
	and	 a
	ret	 z
	dec	 (hl)
	dec	 hl
	ld	 (hl),$1E
	ld	 a,d
	set	 0,a
	in	 a,($15)
	ret	
L0F1A:	djnz	 L0F24
	jr	 nc,L0F6E
	nop	
L0F1F:	ld	 ix,LD06E
	ld	 bc,L0026
L0F26:	add	 ix,bc
	dec	 a
	jr	 nz,L0F26
	ret	
L0F2C:	ld	 iy,LD06E
	ld	 bc,L0026
L0F33:	add	 iy,bc
	dec	 a
	jr	 nz,L0F33
	ret	
L0F39:	exx	
	ld	 bc,(LD34A)
	ld	 hl,$1321
	add	 hl,bc
	push	 hl
L0F43:	ld	 hl,L2776
L0F46:	adc	 hl,bc
	ld	 de,(LD34C)
	add	 hl,de
	ex	 (sp),hl
	add	 hl,bc
	ex	 (sp),hl
L0F50:	adc	 hl,de
	ex	 (sp),hl
	add	 hl,bc
	ex	 (sp),hl
	adc	 hl,de
L0F57:	ex	 (sp),hl
	ld	 d,e
	ld	 e,b
	ld	 b,c
	ld	 c,$00
	add	 hl,bc
	ld	 (LD34A),hl
	pop	 hl
	adc	 hl,de
	ld	 (LD34C),hl
	ld	 a,h
	exx	
	ret
;
;************************************************************
;
; Write protected memory byte
; HL=<location to be written>
; C =<byte to be written>
;
;
;************************************************************
;
L0F6A:	ld	 a,$A5
	out	 ($5B),a	; Protected memory port
L0F6E:	ld	 (hl),c
	ret
;
;************************************************************
;
;
;
;
;************************************************************
;
;

		.DB	$07,$08,$00,$49,$D3,$81,$17,$92
		.DB	$17,$84,$16,$A0,$08,$FE,$15,$0C
		.DB	$08,$00,$00,$00,$D3,$D5,$18,$07
		.DB	$08,$0C,$E1,$D1,$CA,$07,$DA,$32
		.DB	$14,$14,$00,$0B,$08,$04,$CA,$07
		.DB	$23,$33,$13,$15,$05,$0A,$03,$04
		.DB	$E4,$07,$02,$28,$A1,$0C,$97,$16
		.DB	$07,$08,$32,$CB,$D1,$20,$08,$4F
		.DB	$D3,$B8,$0F,$07,$08,$33,$CB,$D1
		.DB	$CA,$07,$CB,$D1,$0A,$89,$11,$6E
		.DB	$2D,$0C,$07,$08,$35,$CB,$D1,$20
		.DB	$08,$4F,$D3,$D2,$0F,$07,$08,$37
		.DB	$CB,$D1,$CA,$07,$CB,$D1,$0A,$B1
		.DB	$11,$96,$2D,$0C,$D4,$19,$5A,$08
		.DB	$08,$3E,$10,$20,$08,$3C,$D0,$F3
		.DB	$0F,$07,$08,$01,$44,$D2,$5A,$08
		.DB	$03,$3E,$10,$84,$16,$2B,$08,$3C
		.DB	$D0,$34,$10,$2B,$08,$48,$D3,$34
		.DB	$10,$E4,$07,$01,$B0,$B9,$0C,$8F
		.DB	$19,$A0,$08,$23,$15,$2B,$08,$03
		.DB	$D3,$E6,$10,$4F,$08,$1E,$61,$1F
		.DB	$81,$07,$0F,$19,$37,$06,$3A,$94
		.DB	$19,$A0,$08,$23,$15,$2B,$08,$03
		.DB	$D3,$E6,$10,$4F,$08,$1E,$61,$1F
		.DB	$68,$08,$F3,$0F,$E4,$07,$0F,$B0
		.DB	$B9,$0C,$68,$08,$07,$10,$2B,$08
		.DB	$3C,$D0,$52,$10,$A0,$08,$7B,$13
		.DB	$2B,$08,$03,$D3,$E6,$10,$68,$08
		.DB	$70,$0F,$73,$17,$93,$00,$84,$16
		.DB	$BC,$16,$61,$1F,$AE,$08,$0C,$08
		.DB	$00,$00,$1B,$D3,$0C,$08,$00,$00
		.DB	$1D,$D3,$07,$08,$00,$02,$D3,$52
		.DB	$17,$08,$07,$E4,$07,$0A,$20,$A2
		.DB	$0C,$E4,$07,$05,$38,$8A,$0C,$5A
		.DB	$08,$0F,$68,$13,$03,$17,$CA,$07
		.DB	$CB,$D1,$02,$22,$00,$CD,$3C,$08
		.DB	$D9,$07,$12,$28,$00,$C7,$3C,$0C
		.DB	$E4,$07,$03,$50,$72,$08,$E4,$07
		.DB	$05,$78,$4A,$0C,$36,$08,$01,$3C
		.DB	$D0,$B5,$10,$E4,$07,$04,$90,$32
		.DB	$04,$68,$08,$C7,$10,$E4,$07,$06
		.DB	$90,$32,$04,$E4,$07,$07,$A0,$22
		.DB	$04,$E4,$07,$17,$B0,$12,$04,$C7
		.DB	$16,$2F,$1E,$EC,$16,$20,$20,$08
		.DB	$53,$D3,$D7,$10,$EC,$16,$08,$A0
		.DB	$08,$23,$15,$4F,$08,$02,$61,$1F
		.DB	$20,$08,$03,$D3,$D7,$10,$07,$08
		.DB	$01,$44,$D2,$93,$00,$A0,$08,$FE
		.DB	$15,$0C,$08,$00,$00,$1B,$D3,$0C
		.DB	$08,$00,$00,$1D,$D3,$2B,$08,$D9
		.DB	$D1,$CD,$12,$75,$16,$84,$16,$A3
		.DB	$16,$E0,$08,$09,$AA,$61,$32,$C4
		.DB	$52,$FB,$6C,$07,$08,$01,$49,$D3
		.DB	$20,$08,$50,$D3,$2B,$11,$81,$07
		.DB	$14,$64,$2D,$5B,$30,$E4,$07,$14
		.DB	$91,$9A,$08,$EC,$16,$01,$4F,$08
		.DB	$78,$61,$1F,$E0,$08,$02,$AA,$C3
		.DB	$32,$80,$62,$3F,$5D,$4F,$08,$3C
		.DB	$61,$1F,$BC,$16,$61,$1F,$AE,$08
		.DB	$20,$08,$51,$D3,$74,$11,$07,$08
		.DB	$01,$EC,$D1,$EC,$16,$02,$E0,$08
		.DB	$06,$55,$C5,$32,$10,$40,$AF,$7F
		.DB	$E0,$08,$05,$AA,$43,$31,$14,$4F
		.DB	$AB,$70,$E0,$08,$07,$FF,$F1,$32
		.DB	$0C,$5E,$B3,$61,$42,$16,$20,$08
		.DB	$CA,$D1,$87,$11,$E4,$07,$09,$90
		.DB	$32,$0C,$07,$08,$01,$EC,$D1,$20
		.DB	$08,$EC,$D1,$AE,$11,$C7,$16,$2B
		.DB	$08,$18,$D3,$A3,$11,$4F,$08,$3C
		.DB	$61,$1F,$52,$17,$20,$07,$4F,$08
		.DB	$B4,$61,$1F,$4F,$08,$78,$61,$1F
		.DB	$BC,$16,$61,$1F,$AE,$08,$79,$1A
		.DB	$2F,$18,$A0,$08,$3D,$13,$07,$08
		.DB	$80,$41,$D2,$07,$08,$07,$45,$D0
		.DB	$36,$08,$00,$50,$D3,$E4,$11,$36
		.DB	$08,$01,$50,$D3,$DC,$11,$52,$17
		.DB	$48,$07,$E4,$07,$16,$91,$9A,$0C
		.DB	$68,$08,$12,$12,$52,$17,$40,$07
		.DB	$68,$08,$F8,$11,$52,$17,$10,$0F
		.DB	$2B,$08,$18,$D3,$F8,$11,$E4,$07
		.DB	$15,$91,$9A,$0C,$68,$08,$12,$12
		.DB	$A0,$08,$0F,$16,$1F,$17,$16,$08
		.DB	$01,$02,$D3,$12,$12,$81,$07,$14
		.DB	$64,$2D,$5B,$30,$E4,$07,$10,$91
		.DB	$9A,$0C,$61,$1F,$4F,$08,$1E,$61
		.DB	$1F,$68,$08,$FD,$10,$2B,$08,$F1
		.DB	$D1,$25,$12,$50,$1F,$2B,$08,$F2
		.DB	$D1,$2D,$12,$24,$1F,$C5,$17,$2B
		.DB	$08,$F1,$D1,$37,$12,$50,$1F,$2B
		.DB	$08,$F2,$D1,$3F,$12,$24,$1F,$81
		.DB	$07,$14,$64,$2D,$5B,$30,$CA,$07
		.DB	$0B,$33,$06,$72,$2D,$4D,$30,$08
		.DB	$61,$1F,$20,$08,$D8,$D1,$9D,$12
		.DB	$81,$07,$14,$64,$2D,$5B,$30,$E4
		.DB	$07,$11,$91,$9A,$08,$4F,$08,$0A
		.DB	$61,$1F,$2B,$08,$C6,$D1,$79,$12
		.DB	$4F,$08,$78,$61,$1F,$68,$08,$FD
		.DB	$10,$CA,$07,$11,$33,$0D,$6C,$2D
		.DB	$53,$30,$0C,$20,$08,$03,$D3,$FD
		.DB	$10,$61,$1F,$2B,$08,$D8,$D1,$B0
		.DB	$12,$07,$08,$20,$BD,$D1,$5A,$08
		.DB	$05,$FD,$10,$61,$1F,$CA,$07,$C5
		.DB	$32,$0C,$6D,$2D,$53,$30,$08,$07
		.DB	$08,$20,$BB,$D1,$68,$08,$65,$12
		.DB	$07,$08,$20,$BB,$D1,$81,$07,$14
		.DB	$64,$2D,$5B,$30,$E4,$07,$11,$91
		.DB	$9A,$08,$8C,$17,$4F,$08,$B4,$61
		.DB	$1F,$68,$08,$FD,$10,$84,$16,$07
		.DB	$08,$0C,$E1,$D1,$E0,$08,$09,$FA
		.DB	$D1,$32,$A4,$59,$1B,$66,$81,$07
		.DB	$14,$64,$2D,$5B,$30,$A0,$08,$0F
		.DB	$16,$1F,$17,$52,$17,$30,$07,$4F
		.DB	$08,$01,$61,$1F,$2B,$08,$45,$D2
		.DB	$EF,$12,$5A,$08,$07,$70,$0F,$EC
		.DB	$16,$10,$20,$08,$3C,$D0,$0E,$13
		.DB	$E4,$07,$0F,$B0,$B9,$0C,$A0,$08
		.DB	$23,$15,$2B,$08,$03,$D3,$E6,$10
		.DB	$4F,$08,$1E,$61,$1F,$20,$08,$3C
		.DB	$D0,$02,$13,$81,$07,$0F,$19,$37
		.DB	$06,$3A,$A0,$08,$23,$15,$2B,$08
		.DB	$03,$D3,$E6,$10,$4F,$08,$1E,$61
		.DB	$1F,$68,$08,$02,$13,$AA,$17,$CE
		.DB	$1C,$D5,$18,$07,$08,$0C,$E1,$D1
		.DB	$07,$08,$01,$47,$D0,$C7,$16,$4F
		.DB	$08,$03,$61,$1F,$2B,$08,$C1,$D1
		.DB	$4F,$13,$B3,$29,$07,$08,$01,$DB
		.DB	$D1,$07,$08,$01,$D7,$D1,$AA,$08
		.DB	$A0,$08,$7B,$13,$BC,$16,$61,$1F
		.DB	$AE,$08,$07,$08,$01,$53,$D3,$68
		.DB	$08,$84,$10,$93,$00,$42,$2D,$AE
		.DB	$08,$C7,$16,$ED,$1C,$CA,$07,$F8
		.DB	$32,$06,$56,$01,$69,$3E,$04,$CA
		.DB	$07,$36,$33,$03,$6E,$01,$51,$3E
		.DB	$04,$D9,$07,$08,$78,$01,$47,$3E
		.DB	$04,$CA,$07,$FE,$32,$06,$16,$0A
		.DB	$A9,$35,$08,$CA,$07,$39,$33,$03
		.DB	$2E,$0A,$91,$35,$08,$D9,$07,$08
		.DB	$38,$0A,$87,$35,$08,$CA,$07,$04
		.DB	$33,$07,$D4,$12,$EB,$2C,$0C,$CA
		.DB	$07,$3C,$33,$03,$EE,$12,$D1,$2C
		.DB	$0C,$D9,$07,$08,$F8,$12,$C7,$2C
		.DB	$0C,$CA,$07,$1B,$33,$07,$94,$1B
		.DB	$2B,$24,$04,$CA,$07,$3F,$33,$04
		.DB	$AC,$1B,$13,$24,$04,$D9,$07,$08
		.DB	$B8,$1B,$07,$24,$04,$CA,$07,$1B
		.DB	$33,$07,$54,$24,$6B,$1B,$08,$CA
		.DB	$07,$3F,$33,$04,$6C,$24,$53,$1B
		.DB	$08,$D9,$07,$08,$78,$24,$47,$1B
		.DB	$08,$CA,$07,$0B,$33,$06,$16,$2D
		.DB	$A9,$12,$0C,$CA,$07,$3F,$33,$04
		.DB	$2C,$2D,$93,$12,$0C,$D9,$07,$08
		.DB	$38,$2D,$87,$12,$0C,$CA,$07,$C5
		.DB	$32,$0C,$2C,$32,$93,$0D,$0C,$CA
		.DB	$07,$11,$33,$0D,$C8,$3A,$F7,$04
		.DB	$08,$CA,$07,$43,$33,$04,$EC,$3A
		.DB	$D3,$04,$08,$D9,$07,$08,$F8,$3A
		.DB	$C7,$04,$08,$63,$17,$5A,$08,$0A
		.DB	$5E,$14,$68,$08,$08,$15,$93,$00
		.DB	$AE,$08,$E4,$07,$0B,$40,$59,$0C
		.DB	$E4,$07,$0C,$50,$49,$0C,$B9,$07
		.DB	$F0,$32,$01,$27,$1E,$0C,$2F,$18
		.DB	$C7,$16,$5A,$08,$07,$83,$14,$68
		.DB	$08,$08,$15,$BC,$16,$61,$1F,$AE
		.DB	$08,$79,$1A,$B9,$07,$EF,$32,$01
		.DB	$27,$32,$0C,$E4,$07,$0D,$B0,$C9
		.DB	$0C,$E4,$07,$0E,$C0,$B9,$0C,$AA
		.DB	$17,$07,$08,$01,$47,$D0,$C7,$16
		.DB	$4F,$08,$03,$61,$1F,$2B,$08,$C1
		.DB	$D1,$A8,$14,$0C,$08,$04,$04,$00
		.DB	$D3,$20,$08,$4F,$D3,$C5,$14,$0C
		.DB	$08,$06,$06,$00,$D3,$CE,$1C,$1E
		.DB	$16,$B3,$29,$07,$08,$01,$DB,$D1
		.DB	$07,$08,$01,$D7,$D1,$07,$08,$01
		.DB	$C9,$D1,$07,$08,$01,$46,$D0,$5A
		.DB	$08,$0A,$E8,$14,$68,$08,$08,$15
		.DB	$BC,$16,$61,$1F,$AE,$08,$79,$1A
		.DB	$1E,$16,$2F,$18,$A0,$08,$3D,$13
		.DB	$E4,$07,$10,$91,$9A,$0C,$07,$08
		.DB	$01,$46,$D0,$5A,$08,$0A,$21,$15
		.DB	$07,$08,$04,$40,$D2,$A0,$08,$23
		.DB	$15,$4F,$08,$02,$61,$1F,$20,$08
		.DB	$03,$D3,$0D,$15,$07,$08,$01,$53
		.DB	$D0,$AA,$08,$42,$08,$40,$81,$15
		.DB	$2B,$08,$48,$D3,$35,$15,$36,$08
		.DB	$01,$3C,$D0,$DA,$15,$07,$08,$02
		.DB	$03,$D3,$0C,$08,$10,$10,$19,$D3
		.DB	$0C,$08,$02,$02,$00,$D3,$20,$08
		.DB	$4F,$D3,$52,$15,$0C,$08,$03,$03
		.DB	$00,$D3,$2B,$08,$48,$D3,$5F,$15
		.DB	$36,$08,$03,$3C,$D0,$7B,$15,$0C
		.DB	$08,$20,$20,$19,$D3,$0C,$08,$05
		.DB	$05,$00,$D3,$20,$08,$4F,$D3,$77
		.DB	$15,$0C,$08,$07,$07,$00,$D3,$AC
		.DB	$16,$AC,$16,$AC,$16,$AC,$16,$AA
		.DB	$08,$42,$08,$20,$DA,$15,$2B,$08
		.DB	$48,$D3,$93,$15,$36,$08,$00,$3C
		.DB	$D0,$DA,$15,$07,$08,$01,$03,$D3
		.DB	$0C,$08,$02,$02,$00,$D3,$20,$08
		.DB	$4F,$D3,$AA,$15,$0C,$08,$03,$03
		.DB	$00,$D3,$0C,$08,$00,$10,$19,$D3
		.DB	$2B,$08,$48,$D3,$BD,$15,$36,$08
		.DB	$01,$3C,$D0,$D6,$15,$07,$08,$20
		.DB	$1A,$D3,$0C,$08,$05,$05,$00,$D3
		.DB	$20,$08,$4F,$D3,$D4,$15,$0C,$08
		.DB	$07,$07,$00,$D3,$AC,$16,$AC,$16
		.DB	$AA,$08,$2B,$08,$3C,$D0,$FC,$15
		.DB	$DE,$16,$20,$08,$DE,$D1,$FC,$15
		.DB	$2B,$08,$E4,$D1,$FC,$15,$07,$08
		.DB	$01,$44,$D2,$07,$08,$01,$E4,$D1
		.DB	$52,$17,$00,$07,$AA,$08,$9E,$17
		.DB	$D2,$16,$AE,$08,$79,$17,$07,$08
		.DB	$00,$02,$D3,$42,$2D,$AA,$08,$F8
		.DB	$16,$E4,$07,$13,$91,$9A,$0C,$07
		.DB	$08,$00,$E2,$D1,$AA,$08
;
; Is this the end of the data (subroutine jumps)???
;
	xor	 a
L161F:	ld	 (LD302),a
	ld	 a,$06
	ld	 de,L0C0C
	call	 L162E
	ld	 a,$04
	ld	 e,$08
L162E:	push	 af
	call	 L0F1F
	call	 L26EB
	pop	 af
	dec	 a
	jr	 nz,L162E
	ret	
;
L163A:	ld	 a,(LD302)
	cp	 $0C
	jr	 z,L1657
	ret	


	ld	 a,(LD347)
	in	 a,($13)
	ld	 b,$03
	bit	 5,a			; Dipswitch: Bonus Lives active HIGH
					; Off = 4th Level,  On  = 3rd Level
	jr	 nz,L164E
	inc	 b
;
L164E:	ld	 a,(LD302)
	sub	 b
	jr	 nz,L163A
	ld	 (LD318),a
;
L1657:	ld	 hl,LD300
	ld	 a,(Game_Mode)
	ld	 (LD1CA),a
	dec	 a
	jr	 z,L166D
	ld	 a,(hl)
	and	 a
	jr	 z,L166D
	inc	 (hl)
	exx	
	call	 L1CFA
	exx
;
L166D:	inc	 hl
	ld	 a,(hl)
	and	 a
	ret	 z
	inc	 (hl)
	jp	 L1D06
	ld	 hl,LD354
	ld	 a,$FF
	ld	 b,$0C
L167C:	and	 (hl)
	inc	 hl
L167E:	djnz	 L167C
	jp	 nz,L179E
	ret	
;
;*****************************************************************************
; Called this routine from L007E routine
; Move $24 (36) bytes of data from $D300 to $D000 ???
;*****************************************************************************
;L1684:
	di			
	ld	 hl,LD000	
	ld	 de,LD300
	ld	 b,$24		; 36 decimal
L168D:	ld	 a,(de)		
	inc	 de
	ld	 c,a
	call	 L0F6A		; Write protected memory byte (HL)=C
	inc	 hl
	djnz	 L168D
	ret	

;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;L1697:

	ld	 hl,L331A
	ld	 de,LD1CC
	ld	 bc,L0009
	ldir	
	ret
;
	ld	 hl,LD352
	ld	 a,(hl)
	ld	 (hl),$00
	dec	 hl
	ld	 (hl),a
	ret	
	ld	 a,(LD347)
	in	 a,($13)		; Check for Free Play - Active HIGH ???
					; Bit 6: Free Play              
					; Off=No Free Play, On=Free Play
	bit	 6,a
	ret	 z
	ld	 hl,LD03C
	ld	 c,(hl)
	dec	 c
	jp	 L0F6A			;Write protected memory byte
	ld	 a,$CC
	out	 ($0F),a
	ld	 hl,L0109
	ld	 (LD1BF),hl
	ret	
L16C7:	ld	 a,$CC
L16C9:	out	 ($0F),a
	ld	 hl,L0109
	ld	 (LD1C1),hl
	ret	

;
;*****************************************************************************
; Called this routine from L007E routine
; Check dip switch for free play 
;*****************************************************************************
;
L16D2:	ld	 a,(LD347)
	in	 a,($13)
	cpl	
	and	 $40
	ld	 (LD348),a
	ret	
;
;*****************************************************************************
; This routine looks like it will check for any activity on player1 and
; player 2 controls and stores the result in $D1DE. Since P1 and P2
; control 'bits' are identical, I assume the result saved will tell the program
; if a certain control has been used by either P1 or P2. ???
; 
;*****************************************************************************
;
	in	 a,($11)		; Check P2 controls - all active LOW
	cpl				; Convert to active HIGH
	ld	 b,a			; 
	in	 a,($12)		; Now check P1 Controls - all active LOW	
	cpl				; Make active HIGH
	or	 b			; Combine with P2 controls
	and	 %00111111		; Mask unused input bits (see port description)	
	ld	 (LD1DE),a		; Save it ..
	ret
;
;*****************************************************************************
; 
; 
;*****************************************************************************
;
	ld	 a,(iy+$00)
	inc	 iy
	ld	 (LD240),a
	ld	 (LD244),a		; Dip switch - Bit 7 - "Sounds in Attract Mode"
	ret	
	in	 a,($10)		
	bit	 7,a			; Check to see if <function> is active
	ret	 nz
;
	ld	 a,$02
	ld	 (LD1E2),a
	ret
;
;*****************************************************************************
; 
; 
;*****************************************************************************
;
	ld	 a,(LD03C)
	ld	 e,$FF
L1708:	inc	 e
	sub	 $0A
	jr	 nc,L1708
	add	 a,$3A
	ld	 hl,LD1CC
L1712:	ld	 (hl),a
	ld	 a,e
	or	 $30
	cp	 $30
	jr	 nz,L171C
	add	 a,$10
L171C:	dec	 hl
	ld	 (hl),a
	ret	
	ld	 a,(LD302)
	ld	 b,a
	xor	 a
L1724:	inc	 a
	daa	
	djnz	 L1724
	push	 af
	and	 $0F
	or	 $30
	ld	 hl,LD1CC
	ld	 (hl),a
	pop	 af
	rrca	
	rrca	
	rrca	
	rrca	
	and	 $0F
	ld	 b,$01
	jr	 z,L1741
	dec	 hl
	inc	 b
	or	 $30
	ld	 (hl),a
L1741:	dec	 de
	dec	 de
	in	 a,($10)
	bit	 7,a
	jr	 nz,L174D
	inc	 de
	inc	 de
	inc	 de
	inc	 de
L174D:	ld	 a,$0C
	jp	 L045C
L1752:	ld	 b,(iy+$00)
	inc	 iy
	call	 L0F39
	and	 (iy+$00)
	inc	 iy
	or	 b
	jp	 L8009
	in	 a,($10)
	bit	 7,a
	ld	 a,$88
	jr	 nz,L176D
	ld	 a,$18
L176D:	ld	 (LD1C4),a
	jp	 L00A0
	ld	 sp,$D400
	jp	 L007E

;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;	
;L1779:
	ld	 hl,LD03A
	ld	 c,$00
	jp	 L0F6A		;Write protected memory byte
;
;*****************************************************************************
; Called this routine from L007E routine
; Check if dip switch set to "Demo Sounds Active"
;*****************************************************************************
;L1781:
	ld	 a,(LD347)	; Why load this? The next command wipes it out ???
	in	 a,($13)
	and	 $80		; Check bit 7 - Demo Sounds active high ???
	ld	 (LD244),a	; Save demo sound status, A=$80 if active, $00 if not
	ret	
;
;*****************************************************************************
; ???
;*****************************************************************************
;

	ld	 a,$08
	ld	 (LD243),a
	ret
	
;
;*****************************************************************************
; Called this routine from L007E routine
; Check how many lives at beginning of game (dip switch)
;*****************************************************************************
;L1792:
	ld	 a,(LD347)	; Why load this? The next command wipes it out ???
	in	 a,($13)
	cpl	
	and	 $10		; Normally active HIGH, but complemented is active LOW
	ld	 (LD34F),a	; $10=3/7 lives, $00=2/5 
	ret
;
;*****************************************************************************
; Called this routine from L007E routine
; Zero $D350 to $D36E
;*****************************************************************************
;
L179E:	ld	 hl,LD350
	ld	 bc,L1E00
L17A4:	ld	 (hl),c
	inc	 hl
	djnz	 L17A4
	ret	
;

	nop	
	call	 L17C5
	ld	 a,$0C
	out	 ($19),a
	ld	 a,$18
	out	 ($0C),a
	ld	 hl,L0F07
	ld	 a,$0B
	call	 L185F
	ld	 hl,L0F43
	ld	 a,$07
	jp	 L185F
L17C5:	call	 L181D
	ld	 hl,L0781
	call	 L1855
	ld	 hl,L07C9
	call	 L1855
	ld	 hl,L0F01
	call	 L1859
	ld	 hl,L0F49
	call	 L1859
	ld	 hl,L1681
	call	 L185D
	ld	 hl,L16C9
	call	 L185D
	ld	 c,$17
	ld	 hl,L0007
	exx	
	ld	 hl,LD178
	ld	 c,$06
L17F7:	ld	 b,$0B
L17F9:	ld	 a,(hl)
	inc	 hl
	exx	
	call	 L1813
	exx	
	djnz	 L17F9
	exx	
	ld	 de,L073E
	add	 hl,de
	exx	
	dec	 c
	jr	 nz,L17F7
	exx	
	call	 L1812
	ld	 hl,L2D43
L1812:	xor	 a
L1813:	push	 hl
	call	 L185F
	pop	 hl
	ld	 de,L0006
	add	 hl,de
	ret
;
L181D:	di	
	ld	 a,(LD1EB)
	and	 a
	ld	 a,$0C
	jr	 nz,L1828
	ld	 a,$04
L1828:	out	 ($19),a
	ld	 a,$18
	out	 ($0C),a
	ret	
	call	 L181D
	ld	 hl,L30DD
	ld	 de,L18D0
	ld	 c,$2F
	call	 L18A8
	ld	 hl,L30F4
	call	 L18A8
	ld	 hl,L30DD
	call	 L184C
	ld	 hl,L3F8D
L184C:	ld	 a,$FF
	ld	 b,$17
L1850:	ld	 (hl),a
	inc	 hl
	djnz	 L1850
	ret	
L1855:	ld	 a,$0D
	jr	 L185F
L1859:	ld	 a,$0C
	jr	 L185F
L185D:	ld	 a,$0E
L185F:	bit	 2,a
	jr	 nz,L186B
	ex	 af,af'
	ld	 de,L18D2
	call	 L18A8
	ex	 af,af'
L186B:	bit	 3,a
	jr	 nz,L187D
	ex	 af,af'
	push	 hl
	ld	 de,L0005
	add	 hl,de
	ld	 de,L18D3
	call	 L18A8
	pop	 hl
	ex	 af,af'
L187D:	bit	 0,a
	call	 z,L1889
	bit	 1,a
	ret	 nz
	ld	 de,L06E0
	add	 hl,de
L1889:	push	 hl
	call	 L1896
	ld	 de,L004B
	add	 hl,de
	call	 L1896
	pop	 hl
	ret	
L1896:	ld	 (hl),$FF
	inc	 hl
	ld	 (hl),$FF
	inc	 hl
	ld	 (hl),$FF
	inc	 hl
	ld	 (hl),$FF
	inc	 hl
	ld	 (hl),$FF
	inc	 hl
	ld	 (hl),$FF
	ret	
;
;*****************************************************************************
; This routine seems to be used to draw vertical lines on the screen
; First used in drawing the radar box in the demo screens.
; Also called when drawing the maze... more details as they are available ???
; Uses DE, HL, C for parameters for source, dest and height.
;*****************************************************************************
;

L18A8:	ld	 a,$22
	out	 ($7A),a
	ld	 a,e
	out	 ($78),a
	ld	 a,d
	out	 ($79),a
	ld	 a,l
	out	 ($7B),a
	ld	 a,h
	out	 ($7C),a
	ld	 a,$4F
	out	 ($7B),a
	ld	 a,$01
	out	 ($7D),a
	ld	 a,c
	out	 ($7E),a
	ret
;
L18C4:	ld	 a,h
	ld	 (hl),c
	call	 m,L7C73
	halt	
	call	 m,L7C78
	ld	 a,e
	call	 m,L807D
	djnz	 L1893
L18D3:	jr	 nc,L18D5

;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;

L18D5:	di	
	ld	 a,$08
	out	 ($0C),a
	ld	 de,L1937
	ld	 a,$04
	out	 ($19),a
	ld	 bc,L1107
	ld	 hl,L3522
	call	 L191B
	ld	 hl,L3D42
	call	 L191B
	ld	 bc,L0111
	ld	 hl,L37A2
	call	 L191B
	ld	 hl,L37B2
	call	 L191B
	ld	 a,$08
	out	 ($19),a
L1903:	ld	 hl,L37DC
	call	 L191B
	ld	 hl,L37EC
	call	 L191B
	ld	 bc,L1107
	ld	 hl,L355C
	call	 L191B
	ld	 hl,L3D7C
L191B:	ld	 a,$22
	out	 ($7A),a
	ld	 a,e
	out	 ($78),a
	ld	 a,d
	out	 ($79),a
	ld	 a,l
	out	 ($7B),a
	ld	 a,h
	out	 ($7C),a
	ld	 a,$50
	sub	 b
	out	 ($7B),a
	ld	 a,b
	out	 ($7D),a
	ld	 a,c
	out	 ($7E),a
	ret
;
L1937:	rst	 38H
	nop	
L1939:	ld	 hl,(LD31B)
	push	 hl
	ld	 de,LD319
	push	 de
	ld	 de,(LD31D)
	xor	 a
	sbc	 hl,de
	ld	 hl,LD31A
	ex	 de,hl
	jr	 c,L1953
	pop	 bc
	ex	 (sp),hl
	push	 de
	ld	 d,b
	ld	 e,c
L1953:	ld	 b,h
	ld	 c,l
	call	 L195A
	pop	 de
	pop	 bc
L195A:	ld	 a,(de)
	and	 a
	ret	 z
	sub	 $10
	ld	 hl,LD304
	jr	 z,L1967
	ld	 hl,LD30E
L1967:	push	 de
	exx	
	ld	 bc,$0500
L196C:	exx	
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	ex	 de,hl
	xor	 a
	sbc	 hl,bc
	ex	 de,hl
	jr	 nc,L1986
	ld	 a,(hl)
	ld	 (hl),b
	ld	 b,a
	dec	 hl
	ld	 a,(hl)
	ld	 (hl),c
	ld	 c,a
	inc	 hl
	exx	
	ld	 a,c
	and	 a
	jr	 nz,L1985
	ld	 c,b
L1985:	exx	
L1986:	inc	 hl
	exx	
	djnz	 L196C
	pop	 hl
	ld	 a,c
	or	 (hl)
	ld	 (hl),a
	ret	
	ld	 de,L32EE
	jr	 L1997
	ld	 de,L3138
L1997:	ld	 a,(LD319)
	ld	 b,$04
	push	 de
	call	 L19A6
	pop	 de
	ld	 a,(LD31A)
	ld	 b,$08
L19A6:	and	 a
	ret	 z
	bit	 4,a
	ld	 hl,L2F9C
	jr	 nz,L19B1
	ld	 l,$C4
L19B1:	and	 $07
	ret	 z
	push	 bc
	ld	 bc,LFB00
	ex	 af,af'
	in	 a,($10)
	bit	 7,a
	jr	 nz,L19C8
	ld	 h,$0A
	ld	 a,l
	sub	 $92
	ld	 l,a
	ld	 bc,$0500
L19C8:	ex	 af,af'
L19C9:	add	 hl,bc
	dec	 a
	jr	 nz,L19C9
	ex	 de,hl
	pop	 af
	ld	 b,$01
	jp	 L045C
	ld	 hl,L168D
	ld	 de,LD304
	call	 L19E3
	ld	 hl,L16B5
	ld	 de,LD30E
L19E3:	in	 a,($10)
	bit	 7,a
	jr	 nz,L19EB
	ld	 h,$20
L19EB:	ld	 b,$05
L19ED:	push	 bc
	push	 de
	push	 hl
	ld	 a,$0C
	call	 L1A2D
	pop	 hl
	pop	 de
	inc	 de
	inc	 de
	in	 a,($10)
	bit	 7,a
	ld	 bc,$0500
	jr	 nz,L1A05
	ld	 bc,LFB00
L1A05:	add	 hl,bc
	pop	 bc
	djnz	 L19ED
	ret	
	nop	
L1A0B:	ld	 hl,LD1E1
	bit	 2,(hl)
	res	 2,(hl)
	ld	 hl,L38E5
	ld	 de,LD31B
	ld	 a,$04
	call	 L1A2C
	ld	 hl,LD1E1
	bit	 3,(hl)
	res	 3,(hl)
	ld	 hl,L391F
	ld	 de,LD31D
	ld	 a,$08
L1A2C:	ret	 z
L1A2D:	ex	 af,af'
	in	 a,($10)
	bit	 7,a
	jr	 nz,L1A38
	ld	 bc,L02DB
	add	 hl,bc
L1A38:	push	 hl
	ld	 hl,LD1D0
	ld	 (hl),$30
	dec	 hl
	ld	 (hl),$30
	dec	 hl
	ld	 a,(de)
	call	 L1A6D
	ld	 a,(de)
	call	 L1A69
	inc	 de
	ld	 a,(de)
	call	 L1A6D
	ld	 a,(de)
	call	 L1A69
	inc	 hl
	push	 hl
	ld	 b,$05
L1A57:	ld	 a,(hl)
	cp	 $30
	jr	 nz,L1A61
	ld	 (hl),$40
	inc	 hl
	djnz	 L1A57
L1A61:	pop	 hl
	ex	 af,af'
	ld	 b,$06
	pop	 de
	jp	 L045C
L1A69:	rrca	
	rrca	
	rrca	
	rrca	
L1A6D:	and	 $0F
	add	 a,$90
	daa	
	adc	 a,$40
	daa	
	ld	 (hl),a
	dec	 hl
	ret	
	nop	
	ld	 hl,LD302
	inc	 (hl)
	ld	 a,(LD318)
	ld	 c,a
	ld	 b,$00
	ld	 hl,L1AED
	add	 hl,bc
	add	 hl,bc
	ld	 c,(hl)
	inc	 hl
	ld	 b,(hl)
	ld	 hl,LD172
	ld	 a,$06
L1A90:	ex	 af,af'
	ld	 de,L0006
	add	 hl,de
	push	 hl
	add	 hl,de
	add	 hl,de
	ex	 de,hl
	dec	 de
	pop	 hl
	call	 L1AB4
	call	 L1ABB
	call	 L1AB4
	call	 L1ABB
	call	 L1AB4
	ld	 a,(bc)
	inc	 bc
	and	 $0F
	ld	 (hl),a
	ex	 af,af'
	dec	 a
	jr	 nz,L1A90
	ret	
L1AB4:	ld	 a,(bc)
	rrca	
	rrca	
	rrca	
	rrca	
	jr	 L1ABD
L1ABB:	ld	 a,(bc)
	inc	 bc
L1ABD:	and	 $0F
	ld	 (hl),a
	and	 $0C
	ld	 a,(hl)
	inc	 hl
	jp	 pe,L1AC9
	xor	 $0C
L1AC9:	dec	 de
	ld	 (de),a
	ret	
L1ACC:	ld	 a,e
	ld	 e,$FF
L1ACF:	inc	 e
	sub	 $18
	jr	 nc,L1ACF
	ld	 a,d
	ld	 d,$00
L1AD7:	inc	 d
	sub	 $18
	jr	 nc,L1AD7
	ld	 l,$0B
	ld	 a,$F5
L1AE0:	add	 a,l
	dec	 d
	jr	 nz,L1AE0
	add	 a,e
	ld	 e,a
	ld	 d,$00
	ld	 hl,LD178
	add	 hl,de
	ret	
L1AED:	dec	 e
	dec	 de
	add	 hl,de
	inc	 e
	cpl	
	dec	 de
	ld	 b,c
	dec	 de
	ld	 d,e
	dec	 de
	ld	 h,l
	dec	 de
	ld	 (hl),a
	dec	 de
	adc	 a,c
	dec	 de
	sbc	 a,e
	dec	 de
	xor	 l
	dec	 de
	cp	 a
	dec	 de
	pop	 de
	dec	 de
	ex	 (sp),hl
	dec	 de
L1B07:	push	 af
	dec	 de
	rlca	
	inc	 e
	dec	 hl
	inc	 e
	dec	 a
	inc	 e
	ld	 c,a
	inc	 e
	ld	 h,c
	inc	 e
	ld	 (hl),e
	inc	 e
	add	 a,l
	inc	 e
	sub	 a
	inc	 e
	xor	 c
	inc	 e
	cp	 e
	inc	 e
	xor	 h
	call	 pe,LBCCE
	ld	 e,d
	rst	 28H
	cp	 h
	rst	 28H
	rst	 38H
	or	 (hl)
	sbc	 a,a
L1B28:	.db	  $dd,$3b
	rst	 28H
	call	 pe,L9595
	sbc	 a,h
	xor	 h
	xor	 $CE
	ld	 a,(LADD7)
	sub	 a
	xor	 a
	call	 m,L73AD
	sbc	 a,(hl)
	cp	 h
	ld	 a,e
	.db	  $ed,$9c
	push	 de
	sbc	 a,h
	xor	 h
	ld	 l,d
	adc	 a,$BC
	rst	 10H
	xor	 l
	sbc	 a,(hl)
	rst	 28H
	cp	 $A5
	inc	 sp
	inc	 sp
	cp	 h
	rst	 30H
	sbc	 a,a
	sbc	 a,h
	ld	 e,c
	call	 L6AAC
	adc	 a,$3A
	rst	 18H
	rst	 08H
	sub	 a
	xor	 a
	ld	 h,e
	xor	 l
	ld	 (hl),e
	inc	 sp
	cp	 h
	push	 af
	cp	 a
	sbc	 a,h
	call	 c,LAC51
	add	 a,$AE
	ld	 a,(L73CD)
	sub	 a
	xor	 (hl)
	rst	 18H
	and	 a
	dec	 sp
	call	 LFF3B
	adc	 a,$9D
	ld	 e,c
	call	 L6AAC
	adc	 a,$BC
	rst	 18H
	ld	 h,e
	sbc	 a,(hl)
	rst	 00H
	cp	 a
	xor	 l
	ld	 l,e
	ld	 d,e
	cp	 h
	ld	 a,e
	.db	  $ed,$9c
	push	 de
	sbc	 a,h
	and	 (hl)
	xor	 h
	call	 pe,L5A3B
	call	 m,LE79F
	cp	 h
	and	 a
	inc	 sp
	cp	 (hl)
	inc	 sp
	cp	 a
	ld	 d,e
	sbc	 a,l
	ld	 e,c
	call	 LEEAE
	xor	 $33
	inc	 sp
	inc	 sp
	sbc	 a,a
	ld	 a,c
	ld	 (hl),e
	and	 a
	sbc	 a,(hl)
	rst	 38H
	dec	 sp
	ld	 l,e
	ld	 d,e
	sub	 l
	sbc	 a,l
	call	 LCEAC
	call	 pe,LE5BC
	cp	 h
	sbc	 a,(hl)
	ld	 a,d
	sbc	 a,$A5
	or	 a
	xor	 a
	ld	 a,(L735B)
	sbc	 a,l
	push	 bc
	sbc	 a,l
	xor	 h
	ld	 l,d
	adc	 a,$BC
	rst	 38H
	rst	 28H
	sbc	 a,(hl)
	ld	 d,e
	inc	 sp
	and	 l
	and	 l
	cp	 a
	cp	 h
	ld	 e,d
	ld	 (hl),e
	sbc	 a,h
	push	 bc
	sbc	 a,l
	xor	 h
	ld	 l,d
	call	 pe,L73BC
	sbc	 a,(hl)
	sub	 (hl)
	cp	 a
	.db	  $ed,$ad
	ld	 d,e
	cp	 h
	cp	 h
	rst	 28H
	call	 m,LD59C
	sbc	 a,h
	and	 (hl)
	xor	 h
	call	 pe,LFC3B
	call	 m,LB697
	sbc	 a,(hl)
	and	 a
	sub	 a
	xor	 l
	dec	 sp
	rst	 28H
	sbc	 a,$9D
	ld	 e,c
	call	 L6AAC
	adc	 a,$3A
	.db	  $fd,$cf
	or	 a
	cp	 h
	ld	 h,e
	or	 l
	ld	 a,(L3ADF)
	rst	 18H
	ld	 h,e
	sbc	 a,l
	push	 bc
	sbc	 a,l
	and	 (hl)
	xor	 h
	call	 pe,L7A3B
	sbc	 a,$95
	add	 hl,sp
	ld	 h,e
	xor	 (hl)
	call	 c,L3B73
L1C14:	xor	 $FD
	sub	 l
	sub	 l
	sbc	 a,h
	xor	 (hl)
	xor	 $EE
	cp	 a
	rst	 38H
	rst	 38H
	cp	 a
	rst	 38H
	rst	 38H
	cp	 a
	rst	 38H
	rst	 38H
	cp	 a
	rst	 38H
	rst	 38H
	sbc	 a,l
	.db	  $dd,$dd
L1C2B:	xor	 h
	xor	 $EE
	or	 (hl)
	cp	 l
	ld	 d,e
	or	 l
L1C32:	ld	 a,(LBEEF)
	push	 de
	cp	 a
	or	 a
	xor	 (hl)
	rst	 38H
	sbc	 a,l
	.db	  $dd,$dd
	xor	 (hl)
L1C3E:	xor	 $CE
	cp	 a
	push	 af
	xor	 a
	cp	 a
	ld	 e,d
	rst	 38H
	or	 l
	xor	 a
	ld	 d,e
	cp	 (hl)
	push	 af
	xor	 a
	sbc	 a,l
	call	 c,LAEDD
	xor	 $EE
	dec	 sp
	ld	 (hl),e
	cp	 a
	inc	 sp
	or	 a
	inc	 sp
	or	 a
	dec	 sp
	ld	 (hl),e
	cp	 a
	ld	 (hl),e
	cp	 a
	sbc	 a,l
	.db	  $dd,$dd
	xor	 (hl)
	xor	 $EE
	add	 hl,sp
	push	 af
	cp	 a
	or	 (hl)
	cp	 (hl)
	ld	 (hl),e
	cp	 l
	ld	 a,c
	ld	 (hl),e
	ld	 a,(LBFF6)
	sbc	 a,l
	.db	  $dd,$dd
	xor	 (hl)
	xor	 $CE
	add	 hl,sp
	push	 af
	xor	 a
	or	 (hl)
	sbc	 a,(hl)
	rst	 38H
	cp	 a
	ld	 l,e
	rst	 18H
	cp	 a
	push	 af
	xor	 a
	sbc	 a,l
	call	 c,LAEDD
	xor	 $CE
	or	 l
	sbc	 a,a
	ld	 h,e
	or	 (hl)
	and	 l
	sbc	 a,a
	add	 hl,sp
	or	 $AF
	or	 (hl)
	sbc	 a,l
	rst	 38H
	sbc	 a,l
	call	 z,LACDD
	xor	 $EE
	or	 (hl)
	sbc	 a,a
	ld	 d,e
	cp	 a
	ld	 h,e
	xor	 a
	cp	 a
	ld	 d,e
	sbc	 a,a
	or	 l
	xor	 a
	ld	 h,e
	sbc	 a,h
	.db	  $dd,$dd
	xor	 h
	xor	 $CE
	or	 (hl)
	sbc	 a,a
	.db	  $ed,$bf
	ld	 l,c
	cp	 $BF
	or	 $9F
	or	 a
	cp	 a
	ld	 h,e
	sbc	 a,l
	.db	  $dd,$dd
	xor	 h
	xor	 $EC
	cp	 (hl)
	rst	 10H
	cp	 h
	dec	 sp
	.db	  $ed,$fe
	dec	 sp
	sbc	 a,$FD
	cp	 l
	rst	 20H
	cp	 h
	sbc	 a,h
	.db	  $dd,$dc
	nop	
	call	 L1D12
	ld	 a,(LD300)
	call	 L1CDD
	call	 L1D1A
	ld	 a,(LD301)
L1CDD:	and	 a
	ret	 z
	cp	 $08
	jr	 c,L1CE5
	ld	 a,$07
L1CE5:	ld	 b,a
L1CE6:	call	 L0B92
	inc	 hl
	djnz	 L1CE6
	ret	
	ld	 hl,L1DB3
	ld	 de,L1DD6
	call	 L1D20
	ld	 b,$07
	jr	 L1CE6
L1CFA:	ld	 hl,L1DF9
	ld	 de,L1DFE
	call	 L1D20
	jp	 L0B92
L1D06:	ld	 hl,L1E03
	ld	 de,L1E08
	call	 L1D20
	jp	 L0B92
L1D12:	ld	 hl,L1D27
	ld	 de,L1D4A
	jr	 L1D20
L1D1A:	ld	 hl,L1D6D
	ld	 de,L1D90
L1D20:	in	 a,($10)
	bit	 7,a
	ret	 nz
	ex	 de,hl
L1D26:	ret	
L1D27:	ld	 h,d
	sbc	 a,h
	jr	 c,L1D27
	dec	 l
	ld	 h,d
	sbc	 a,h
	jr	 c,L1D26
L1D30:	dec	 l
	ld	 h,d
	sbc	 a,h
	jr	 c,L1DAB
	ld	 h,$62
	sbc	 a,h
	jr	 c,L1D30
	ld	 e,$62
	sbc	 a,h
	jr	 c,L1DB5
L1D3F:	rla	
	ld	 h,d
	sbc	 a,h
	jr	 c,L1DBA
	ex	 af,af'
	ld	 h,d
	sbc	 a,h
	jr	 c,L1D3F
	nop	
L1D4A:	jp	 po,L389C
	ld	 c,h
	inc	 sp
	jp	 po,L389C
	ld	 b,(hl)
	inc	 sp
	jp	 po,L389C
	add	 a,$2B
	jp	 po,L389C
	ld	 b,(hl)
	inc	 h
	jp	 po,L389C
	add	 a,$1C
	jp	 po,L389C
	add	 a,$0D
	jp	 po,L389C
	ld	 b,(hl)
	ld	 b,$22
	or	 $38
	inc	 sp
	ld	 l,$22
	or	 $38
	add	 hl,sp
	ld	 l,$22
	or	 $38
L1D7A:	cp	 c
	ld	 h,$22
	or	 $38
	add	 hl,sp
	rra	
L1D81:	ld	 (L38F6),hl
	cp	 c
	rla	
	ld	 (L38F6),hl
	cp	 c
	ex	 af,af'
	ld	 (L38F6),hl
	add	 hl,sp
	ld	 bc,LF6A2
	jr	 c,L1D17
	inc	 sp
	and	 d
	or	 $38
	adc	 a,c
	inc	 sp
	and	 d
	or	 $38
	add	 hl,bc
	inc	 l
	and	 d
	or	 $38
	adc	 a,c
	inc	 h
	and	 d
	or	 $38
	add	 hl,bc
	dec	 e
	and	 d
	or	 $38
	add	 hl,bc
	ld	 c,$A2
	or	 $38
	adc	 a,c
	ld	 b,$22
	xor	 (hl)
L1DB5:	sbc	 a,(hl)
	inc	 h
	nop	
	ld	 (L9600),hl
	call	 po,L2208
	cpl	
	dec	 a
	and	 h
	ld	 de,L9C22
	jr	 c,L1E2A
	ld	 a,(de)
	ld	 (L38F6),hl
	inc	 h
	inc	 hl
	ld	 (LA39A),hl
	call	 po,L222B
	djnz	 L1D7A
L1DD4:	and	 h
	add	 hl,sp
L1DD6:	jp	 po,L9EAE
	sbc	 a,e
	ccf	
	jp	 po,L9600
	in	 a,($36)
	jp	 po,L3D2F
	dec	 de
	ld	 l,$E2
	sbc	 a,h
	jr	 c,L1E44
	dec	 h
	jp	 po,L38F6
	sbc	 a,e
	inc	 e
	jp	 po,LA39A
	in	 a,($13)
	jp	 po,LA610
	dec	 de
	ld	 b,$60
	sbc	 a,h
	jr	 c,L1DD4
	dec	 hl
L1DFE:	ret	 po
	sbc	 a,h
L1E00:	jr	 c,L1DF9
	djnz	 L1E26
	or	 $38
	ret	 m
	dec	 hl
L1E08:	and	 d
	or	 $38
L1E0B:	jr	 L1E1E
L1E0D:	ld	 a,(LD03C)
	dec	 a
	jr	 z,L1E2F
	ld	 hl,L1E7B
	ld	 de,L1E90
	call	 L1D20
	ld	 b,$04
L1E1E:	call	 L1E3F
	ld	 hl,L1E82
	ld	 de,L1E97
	call	 L1D20
L1E2A:	ld	 b,$04
	call	 L1E3F
L1E2F:	ld	 a,$78
	ld	 (LD04C),a
	ld	 hl,L1E74
	ld	 de,L1E89
	call	 L1D20
	ld	 b,$02
L1E3F:	ld	 a,(LD03C)
	cp	 b
	ld	 b,$02
	jr	 c,L1E49
	ld	 b,$05
L1E49:	ld	 a,(LD34F)
	and	 a
	ld	 a,b
	jr	 z,L1E52
	or	 $03
L1E52:	ld	 c,a
	ld	 b,$05
	ld	 de,LD1CB
L1E58:	ld	 a,(hl)
	inc	 hl
	ld	 (de),a
	inc	 de
	djnz	 L1E58
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	ld	 b,c
L1E62:	push	 de
	ld	 hl,LD1CB
	call	 L0B92
	pop	 de
	ld	 hl,(LD1CE)
	add	 hl,de
	ld	 (LD1CE),hl
	djnz	 L1E62
	ret	
L1E74:	ld	 (L38F6),hl
	ld	 c,d
	ld	 e,$FB
	rst	 38H
L1E7B:	ld	 h,d
	sbc	 a,h
	jr	 c,L1E85
	ld	 (L0005),a
L1E82:	ld	 (L38F6),hl
L1E85:	ld	 c,d
	ld	 (LFFFB),a
L1E89:	and	 d
	or	 $38
	jp	 pe,LFB1E
	rst	 38H
L1E90:	jp	 po,L389C
	and	 (hl)
	ld	 a,(bc)
	dec	 b
	nop	
L1E97:	and	 d
	or	 $38
	jp	 pe,LFB0A
	rst	 38H
	nop	
L1E9F:	ld	 a,(LD1D7)
	and	 a
	ret	 z
	ld	 hl,LD054
	bit	 7,(hl)
	jr	 nz,L1ED6
	ld	 a,(LD300)
	and	 a
	jr	 z,L1ED6
	ld	 b,$00
	ld	 de,L0344
	call	 L1F55
	dec	 c
	jr	 nz,L1EBE
	ld	 a,$03
L1EBE:	ld	 (LD1EF),a
	ld	 a,$02
	ld	 (LD043),a
	xor	 a
	ld	 (LD1ED),a
	ld	 (LD1DC),a
	ld	 a,$10
	jr	 z,L1ED3
	ld	 a,$20
L1ED3:	call	 L1EF3
L1ED6:	ld	 hl,LD074
	bit	 7,(hl)
	ret	 nz
	ld	 a,(LD301)
	and	 a
	ret	 z
	ld	 b,$F0
	ld	 de,L0248
	call	 L1F55
	ld	 (LD1F0),a
	ld	 a,$02
	ld	 (LD044),a
	ld	 a,$20
L1EF3:	ld	 c,$00
	push	 hl
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),a
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),b
	inc	 hl
	ld	 (hl),c
L1F00:	inc	 hl
	ld	 (hl),$90
	inc	 hl
	ld	 (hl),d
	inc	 hl
	ld	 (hl),e
	push	 bc
	push	 hl
	call	 L1D12
	ld	 a,b
L1F0D:	and	 a
	call	 nz,L1D1A
	ex	 de,hl
	pop	 hl
	ld	 b,$05
L1F15:	ld	 a,(de)
	inc	 de
	inc	 hl
	ld	 (hl),a
	djnz	 L1F15
	pop	 bc
	pop	 hl
	ld	 (hl),$94
L1F1F:	ld	 a,b
	cp	 $78
	jr	 c,L1F50
	ld	 hl,L2CA3
L1F27:	ld	 de,L004B
L1F2A:	ld	 b,$04
	di	
	ld	 a,$20
	out	 ($0C),a
	ld	 a,(LD1EB)
	and	 a
	ld	 c,$55
L1F37:	jr	 z,L1F3B
	ld	 c,$FF
L1F3B:	ld	 a,c
	and	 $0F
L1F3E:	ld	 (hl),a
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 a,c
	and	 $F0
	ld	 (hl),a
	add	 hl,de
	djnz	 L1F3B
	ret	
L1F50:	ld	 hl,L2C67
	jr	 L1F27
L1F55:	ld	 a,(Game_Mode)
	ld	 c,a
	and	 a
	ld	 a,$01
	ret	 z
	ld	 a,$0A
	ret	
	nop	
L1F61:	dec	 iy
	dec	 iy
	ei	
	call	 L2081
	call	 L2BFD
	call	 L203B
	ld	 hl,LD1C3
	bit	 0,(hl)
	jr	 z,L1F94
	res	 0,(hl)
	push	 hl
	ld	 ix,LD054
	call	 L21B8
	call	 L2259
	call	 L2132
	call	 L2633
	call	 L2D9A
	ld	 a,(LD1DF)
	and	 a
L1F90:	call	 z,L231F
	pop	 hl
L1F94:	bit	 1,(hl)
	jr	 z,L1FBC
	res	 1,(hl)
	push	 hl
	ld	 ix,LD074
	call	 L2274
	ld	 a,$01
	ld	 (LD1DA),a
	call	 L2132
	call	 L2633
	call	 L2D9A
	xor	 a
	ld	 (LD1DA),a
	ld	 a,(LD1DF)
	and	 a
	call	 z,L2327
	pop	 hl
L1FBC:	push	 hl
	call	 L0BF7
L1FC0:	ld	 hl,LD1E0
	dec	 (hl)
	pop	 hl
	jr	 z,L2014
	ld	 a,(hl)
	and	 $03
	ret	 nz
	bit	 2,(hl)
	jr	 z,L1FEE
	res	 2,(hl)
	push	 hl
	ld	 a,$05
L1FD4:	push	 af
	ld	 hl,LD054
	ld	 de,LD074
	call	 L0F1F
	call	 L256E
	call	 L2633
	call	 L2D9A
	pop	 af
	dec	 a
	dec	 a
	jp	 p,L1FD4
	pop	 hl
L1FEE:	bit	 3,(hl)
L1FF0:	jr	 z,L2010
	res	 3,(hl)
	push	 hl
	ld	 a,$06
L1FF7:	push	 af
	ld	 hl,LD074
	ld	 de,LD054
	call	 L0F1F
L2001:	call	 L256E
L2004:	call	 L2633
	call	 L2D9A
	pop	 af
	dec	 a
L200C:	dec	 a
	jr	 nz,L1FF7
	pop	 hl
L2010:	ld	 a,(hl)
	and	 $03
	ret	 nz
L2014:	ld	 a,$04
	ld	 (LD1E0),a
	call	 L0F39
	call	 L2B8B
	call	 L2A38
	call	 L22FD
	call	 L2740
	call	 L1E9F
	call	 L8003
	call	 L2D6B
	call	 L2C15
	call	 L1A0B
	jp	 L22A8
	nop	
L203B:	ld	 a,(LD1DB)
	and	 a
	ret	 z
	ld	 ix,LD054
	ld	 a,(LD1DC)
	call	 L2051
	ld	 ix,LD074
	ld	 a,(LD1DD)
L2051:	and	 $0F
	ld	 c,a
	ld	 b,(ix+$00)
	bit	 7,b
	ret	 z
	bit	 3,b
	ret	 nz
	bit	 4,b
	jr	 nz,L2070
	bit	 5,(ix+$08)
	jr	 nz,L2070
	ld	 a,(ix+$01)
	and	 $F0
	or	 c
	ld	 (ix+$01),a
L2070:	bit	 0,c
	ret	 z
	bit	 2,b
	ret	 z
	res	 2,(ix+$00)
	ld	 hl,LD241
	set	 6,(hl)
	ret	
L2080:	nop	
L2081:	ld	 hl,LD040
	ld	 a,(hl)
	and	 a
	jr	 z,L20AE
	ld	 (hl),$00
	rra	
	push	 af
	call	 c,L283D
	pop	 af
	rra	
	push	 af
	call	 c,L27C0
	pop	 af
	rra	
	push	 af
	call	 c,L284D
	pop	 af
	rra	
	push	 af
	call	 c,L27F3
	pop	 af
	rra	
	push	 af
	call	 c,L27DF
	pop	 af
	rra	
	push	 af
	call	 c,L20E8
	pop	 af
L20AE:	ld	 hl,LD041
	ld	 a,(hl)
	and	 a
	jr	 z,L20E7
	ld	 (hl),$00
	rra	
	push	 af
	call	 c,L2113
	pop	 af
	rra	
	push	 af
	call	 c,L28CD
	pop	 af
	rra	
	push	 af
	call	 c,L2100
	pop	 af
	rra	
	push	 af
	call	 c,L1E0D
	pop	 af
	rra	
	push	 af
	call	 c,L20E7
	pop	 af
	rra	
	push	 af
	call	 c,L20E7
	pop	 af
	rra	
	push	 af
	call	 c,L20E7
	pop	 af
	rra	
	push	 af
	call	 c,L20F6
	pop	 af
L20E7:	ret	
L20E8:	ld	 a,(LD053)
	and	 a
	ret	 nz
	inc	 a
	ld	 (LD050),a
	ld	 iy,(LD051)
	ret	
L20F6:	ld	 a,(LD050)
	and	 a
	ret	 nz
	inc	 iy
	inc	 iy
	ret
	

L2100:	xor	 a
	ld	 (LD1E8),a
	ld	 hl,LD096
	ld	 (hl),$40
	call	 L210C
L210C:	inc	 hl
	inc	 hl
	ld	 a,(hl)
	and	 $FC
	ld	 (hl),a
	ret	
L2113:	ld	 ix,LD094
	ld	 hl,LD04F
	bit	 7,(ix+$13)
	jr	 z,L2123
L2120:	ld	 (hl),$01
	ret	
L2123:	ld	 a,(ix+$00)
	and	 $88
	cp	 $80
	jr	 nz,L2120
	ld	 (hl),$14
	jp	 L2342
	nop	
L2132:	push	 iy
	ld	 a,$06
L2136:	push	 af
	call	 L0F2C
	call	 L2144
	pop	 af
	dec	 a
	jr	 nz,L2136
	pop	 iy
	ret	
L2144:	ld	 a,(ix+$00)
	and	 $98
	cp	 $80
	ret	 nz
	ld	 a,(iy+$00)
	and	 $9A
	cp	 $80
	ret	 nz
	ld	 a,(iy+$04)
	sub	 (ix+$04)
	ld	 h,a
	add	 a,$0E
	cp	 $1C
	ret	 nc
	ld	 a,(iy+$06)
	sub	 (ix+$06)
	ld	 d,a
	add	 a,$0E
	cp	 $1C
	ret	 nc
	ld	 b,(iy+$07)
	bit	 1,b
	jr	 z,L2174
	ex	 de,hl
L2174:	ld	 a,h
	and	 a
	jr	 nz,L2189
	ld	 a,d
	bit	 0,b
	ld	 h,$FF
	jr	 z,L2181
	inc	 h
	cpl	
L2181:	cp	 $0E
	jp	 c,L0D4C
	ld	 a,b
	cpl	
	ld	 b,a
L2189:	ld	 a,h
	and	 a
	ld	 d,$00
	ld	 h,d
	jp	 p,L2198
	ld	 d,$18
	bit	 1,b
	jr	 z,L2198
	ex	 de,hl
L2198:	ld	 a,(ix+$04)
	call	 L244D
	add	 a,d
	jr	 c,L21A4
	ld	 (ix+$04),a
L21A4:	ld	 a,(ix+$06)
	call	 L244D
	add	 a,h
	cp	 $79
	jr	 nc,L21B2
	ld	 (ix+$06),a
L21B2:	set	 5,(ix+$00)
	ret	
	nop	
L21B8:	ld	 a,(Game_Mode)
	dec	 a
	ret	 nz
	ld	 a,(LD074)
	ld	 b,a
	rla	
	jr	 c,L21D0
	ld	 a,(LD301)
	and	 a
	jr	 nz,L21D0
	ld	 (ix+$00),a
	ld	 (LD300),a
L21D0:	ld	 a,(ix+$00)
	and	 $9C
	cp	 $80
	ret	 nz
	ld	 a,(ix+$01)
	and	 $F0
	jr	 nz,L21EB
	ld	 a,r
	bit	 3,a
	ld	 a,$09
	jr	 z,L2237
	ld	 a,$06
	jr	 L2237
L21EB:	ld	 hl,LD049
	ld	 a,(hl)
	and	 a
	jr	 nz,L223A
	ld	 (hl),$90
	push	 iy
	ld	 de,L00FF
	ld	 a,$06
L21FB:	push	 af
	call	 L0F2C
	bit	 7,(iy+$00)
	jr	 z,L2230
	ld	 a,(ix+$04)
L2208:	sub	 (iy+$04)
	ld	 l,$04
	jr	 nc,L2213
L220F:	ld	 l,$08
	neg	
L2213:	rra	
	and	 $7F
	ld	 b,a
	ld	 a,(ix+$06)
	sub	 (iy+$06)
	ld	 h,$01
	jr	 nc,L2225
	ld	 h,$02
	neg	
L2225:	rra	
	and	 $7F
	add	 a,b
L2229:	cp	 e
	jr	 nc,L2230
	ld	 e,a
	ld	 a,h
	or	 l
	ld	 d,a
L2230:	pop	 af
	dec	 a
	jr	 nz,L21FB
	pop	 iy
	ld	 a,d
L2237:	ld	 (LD1ED),a
L223A:	bit	 7,(ix+$13)
	ret	 nz
	push	 iy
	ld	 a,$06
L2243:	push	 af
L2244:	call	 L0F2C
	ld	 a,(iy+$00)
	and	 $8A
	cp	 $80
	call	 z,L24B8
	pop	 af
	dec	 a
	jr	 nz,L2243
	pop	 iy
	ret	
	nop	
L2259:	ld	 a,(LD1ED)
	ld	 d,a
	ld	 a,(Game_Mode)
	cp	 $02
	jr	 nz,L226F
	in	 a,($11)
	ld	 hl,LD04A
	call	 L228E
	call	 c,L289D
L226F:	ld	 a,d
	ld	 (LD1DC),a
	ret	
L2274:	ld	 a,(LD1EE)
	ld	 d,a
	ld	 a,(Game_Mode)
	and	 a
	jr	 z,L2289
	in	 a,($12)
	ld	 hl,LD04B
	call	 L228E
	call	 c,L289D
L2289:	ld	 a,d
	ld	 (LD1DD),a
	ret	
L228E:	cpl	
	and	 $3F
	ld	 d,a
	bit	 3,(ix+$00)
	ret	 nz
	ld	 a,$20
	bit	 4,d
	jr	 nz,L22A3
	ld	 a,(hl)
	and	 a
	jr	 z,L22A5
	dec	 (hl)
	ret	
L22A3:	ld	 (hl),$02
L22A5:	scf	
	ret	
	nop	
L22A8:	ld	 hl,LD1E5
	bit	 1,(hl)
	res	 1,(hl)
	ld	 a,$0C
	push	 hl
	call	 nz,L22C0
	pop	 hl
	bit	 0,(hl)
	res	 0,(hl)
	ld	 a,$00
	call	 nz,L22C0
	ret	
L22C0:	ld	 hl,L22FA
	ld	 de,$1132
	push	 af
	call	 L22F4
	pop	 af
	ld	 de,L117C
	call	 L22F4
	ld	 c,$F0
	ld	 de,L0050
	di	
	ld	 a,$20
	out	 ($0C),a
	ld	 hl,L0F57
	call	 L22E4
	ld	 hl,L0F98
L22E4:	ld	 b,$08
L22E6:	ld	 (hl),c
	add	 hl,de
	ld	 (hl),c
	add	 hl,de
	add	 hl,de
	djnz	 L22E6
	ld	 a,c
	rrca	
	rrca	
	rrca	
	rrca	
	ld	 c,a
	ret	
L22F4:	ld	 bc,L01FF
	jp	 L0460
L22FA:	ld	 e,h
	ld	 h,c
	nop	
L22FD:	ld	 a,(LD1C6)
L2300:	and	 a
	ret	 nz
	ld	 a,$06
	push	 iy
L2306:	push	 af
L2307:	call	 L0F1F
	ld	 iy,LD054
	call	 L2498
	ld	 iy,LD074
	call	 L2498
L2318:	pop	 af
	dec	 a
	jr	 nz,L2306
	pop	 iy
	ret	
L231F:	ld	 a,(LD1DC)
	ld	 hl,LD1E6
	jr	 L232D
L2327:	ld	 a,(LD1DD)
	ld	 hl,LD1E7
L232D:	bit	 5,(hl)
	ld	 (hl),a
	ret	 nz
	bit	 5,a
	ret	 z
	ld	 a,(ix+$00)
	bit	 7,a
	ret	 z
	and	 $18
	ret	 nz
	bit	 7,(ix+$13)
	ret	 nz
L2342:	ld	 e,(ix+$04)
	ld	 d,(ix+$06)
	ld	 bc,$0000
	ld	 a,(ix+$07)
	bit	 1,a
	rra	
	jp	 z,L239B
	jr	 nc,L236D
	call	 L254C
L2359:	call	 L2463
L235C:	ret	 m
	cp	 b
	ret	 c
	call	 L2422
	ld	 a,(ix+$04)
	add	 a,$16
	jr	 nc,L2382
	ld	 a,$FF
	jr	 L2382
L236D:	call	 L2547
L2370:	call	 L246D
	ret	 m
	cp	 b
	ret	 c
	call	 L240E
	ld	 a,(ix+$04)
	bit	 2,c
	jr	 z,L2382
	sub	 $06
L2382:	and	 $F8
	jr	 nz,L2388
	ld	 a,$08
L2388:	ld	 b,a
	ld	 a,(ix+$06)
	add	 a,$0B
	bit	 2,(ix+$07)
	jr	 z,L2396
	add	 a,$03
L2396:	ld	 h,a
	ld	 l,$00
	jr	 L23D2
L239B:	jr	 nc,L23B1
	call	 L2542
L23A0:	call	 L2477
	ret	 m
	cp	 b
	ret	 c
	call	 L2422
	ld	 a,(ix+$06)
L23AC:	ld	 l,c
	add	 a,$16
	jr	 L23C7
L23B1:	call	 L253D
L23B4:	call	 L2481
	ret	 m
	cp	 b
	ret	 c
	call	 L240E
	ld	 a,(ix+$06)
	ld	 l,c
	bit	 2,c
	jr	 z,L23C7
	sub	 $06
L23C7:	and	 $F8
	ld	 h,a
	ld	 a,(ix+$04)
	add	 a,$08
	ld	 b,a
	ld	 c,$00
L23D2:	ld	 (ix+$14),c
	ld	 (ix+$16),l
	ld	 (ix+$15),b
	ld	 (ix+$17),h
	ld	 a,(ix+$07)
	and	 $07
	or	 d
	ld	 (ix+$13),a
	bit	 2,(ix+$07)
	ld	 hl,LD241
	jr	 nz,L23F3
	set	 1,(hl)
	ret	
L23F3:	inc	 hl
	ld	 a,(LD1EB)
	and	 a
	ld	 a,$08
	jr	 z,L240A
	ld	 b,a
L23FD:	ld	 a,(LD1C6)
	and	 a
	ld	 a,b
	jr	 z,L240A
	ld	 hl,LD243
	set	 2,(hl)
	ret	
L240A:	rrca	
	or	 (hl)
	ld	 (hl),a
	ret	
L240E:	ld	 c,$F8
	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L2418
	ld	 c,$FC
L2418:	bit	 2,(ix+$07)
	jr	 nz,L2434
	ld	 c,$F8
	jr	 L2434
L2422:	ld	 c,$08
	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L242C
	ld	 c,$04
L242C:	bit	 2,(ix+$07)
	jr	 nz,L2434
	ld	 c,$08
L2434:	set	 1,(ix+$08)
	ld	 (ix+$1a),$03
	ld	 (ix+$1b),$01
	set	 5,(ix+$00)
	ret	
L2445:	ld	 hl,L2458
L2448:	cp	 (hl)
	inc	 hl
	jr	 c,L2448
	ret	
L244D:	exx	
	ld	 hl,L2457
L2451:	inc	 hl
	cp	 (hl)
	jr	 c,L2451
	ld	 a,(hl)
	exx	
L2457:	ret	
L2458:	ret	 p
	ret	 c
	ret	 nz
	xor	 b
	sub	 b
	ld	 a,b
	ld	 h,b
	ld	 c,b
	jr	 nc,L247A
	nop	
L2463:	call	 L1ACC
	ld	 de,$0001
	ld	 a,$08
	jr	 L2489
L246D:	call	 L1ACC
	ld	 de,LFFFF
	ld	 a,$04
	jr	 L2489
L2477:	call	 L1ACC
L247A:	ld	 de,L000B
	ld	 a,$02
	jr	 L2489
L2481:	call	 L1ACC
	ld	 de,LFFF5
	ld	 a,$01
L2489:	and	 (hl)
	jr	 z,L2490
	add	 hl,de
	inc	 c
	jr	 L2489
L2490:	ld	 a,c
	and	 a
	ld	 d,$80
	ret	 nz
	ld	 d,$A0
	ret	
L2498:	ld	 a,(ix+$00)
	and	 $8A
	cp	 $80
	ret	 nz
	bit	 7,(ix+$13)
	ret	 nz
	ld	 a,(ix+$02)
	cp	 $40
	ret	 z
	ld	 a,(LD1E8)
	and	 a
	ret	 nz
	ld	 a,(iy+$00)
	and	 $98
	cp	 $80
	ret	 nz
L24B8:	ld	 a,(ix+$1c)
	and	 a
	ret	 nz
	ld	 d,(ix+$06)
	ld	 e,(ix+$04)
	ld	 h,(iy+$06)
	ld	 l,(iy+$04)
	ld	 a,(ix+$07)
	bit	 1,a
	rra	
	jr	 z,L24FD
	jr	 nc,L24E8
	ld	 a,h
	sub	 d
	add	 a,$0E
	cp	 $1C
	ret	 nc
	ld	 a,l
	sub	 e
	ret	 c
	call	 L254C
	ld	 c,e
	ld	 b,l
	call	 L2529
	jp	 L2359
L24E8:	ld	 a,d
	sub	 h
	add	 a,$0E
	cp	 $1C
	ret	 nc
	ld	 a,e
	sub	 l
	ret	 c
	ld	 c,l
	call	 L2547
	ld	 b,e
	call	 L2529
	jp	 L2370
L24FD:	jr	 nc,L2514
	ld	 a,l
	sub	 e
	add	 a,$0E
	cp	 $1C
	ret	 nc
	ld	 a,h
	sub	 d
	ret	 c
	call	 L2542
	ld	 c,d
	ld	 b,h
	call	 L2529
	jp	 L23A0
L2514:	ld	 a,e
	sub	 l
	add	 a,$0E
L2518:	cp	 $1C
	ret	 nc
	ld	 a,d
	sub	 h
	ret	 c
	call	 L253D
	ld	 c,h
	ld	 b,d
	call	 L2529
	jp	 L23B4
L2529:	ld	 a,c
	call	 L244D
	ld	 c,a
	ld	 a,b
	call	 L244D
	sub	 c
	ld	 b,$FF
L2535:	inc	 b
	sub	 $18
	jr	 nc,L2535
	ld	 c,$00
	ret	
L253D:	ld	 a,d
	add	 a,$02
	ld	 d,a
	ret	
L2542:	ld	 a,d
	add	 a,$14
	ld	 d,a
	ret	
L2547:	ld	 a,e
	add	 a,$02
	ld	 e,a
	ret	
L254C:	ld	 a,e
	add	 a,$14
	jr	 nc,L2553
	ld	 a,$FF
L2553:	ld	 e,a
	ret	
	nop	
L2556:	ld	 a,(ix+$06)
	cp	 $30
	ld	 b,$01
	jr	 nc,L2561
	ld	 b,$02
L2561:	ld	 a,(LD1EA)
	and	 a
	ld	 a,$04
	jr	 z,L256B
	ld	 a,$08
L256B:	jp	 L2609
L256E:	ld	 a,(ix+$00)
	ld	 b,a
	and	 $88
	cp	 $80
	ret	 nz
	ld	 a,(ix+$24)
	and	 a
	jr	 z,L2580
	dec	 (ix+$24)
L2580:	ld	 a,(ix+$23)
	and	 a
	jr	 z,L258A
	dec	 (ix+$23)
	ex	 af,af'
L258A:	bit	 6,b
	jr	 z,L2598
	res	 6,(ix+$00)
	ld	 a,(ix+$25)
	ld	 (ix+$02),a
L2598:	ld	 a,(ix+$01)
	and	 a
	jr	 z,L25A4
	and	 $F0
	jr	 z,L25C9
	ex	 af,af'
	ret	 nz
L25A4:	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L25B2
	ld	 a,(LD1EB)
	and	 a
	jr	 nz,L2556
	jr	 L25B9
L25B2:	ld	 a,(LD1EA)
	and	 a
	jr	 z,L25B9
	ex	 de,hl
L25B9:	ld	 a,(hl)
	and	 $98
	cp	 $80
	jr	 z,L25ED
	ex	 de,hl
	ld	 a,(hl)
	ex	 de,hl
	and	 $98
	cp	 $80
	jr	 z,L25E7
L25C9:	ld	 a,(ix+$06)
	cp	 $30
	ld	 c,$02
	jr	 c,L25D4
	ld	 c,$01
L25D4:	ld	 a,r
	bit	 4,a
	ld	 b,$01
	jr	 z,L25DD
	ld	 b,c
L25DD:	bit	 2,a
	ld	 a,$04
	jr	 z,L25E5
	ld	 a,$08
L25E5:	jr	 L2609
L25E7:	ex	 de,hl
	call	 L25ED
	ex	 de,hl
	ret	
L25ED:	push	 hl
	ld	 bc,$0004
	add	 hl,bc
	ld	 a,(hl)
	sub	 (ix+$04)
	ld	 b,$08
	jr	 nc,L25FC
	ld	 b,$04
L25FC:	inc	 hl
	inc	 hl
	ld	 a,(hl)
	sub	 (ix+$06)
	ld	 a,$02
	jr	 nc,L2608
	ld	 a,$01
L2608:	pop	 hl
L2609:	or	 b
	ld	 b,a
	ld	 a,(ix+$01)
	and	 $F0
	or	 b
	ld	 (ix+$01),a
L2614:	ld	 a,(ix+$02)
	ld	 b,$FF
	cp	 $10
	jr	 c,L262B
	ld	 b,$7F
	cp	 $20
	jr	 c,L262B
	ld	 b,$3F
	cp	 $40
	jr	 c,L262B
	ld	 b,$1F
L262B:	ld	 a,r
	and	 b
	ld	 (ix+$23),a
	ret	
	nop	
L2633:	bit	 3,(ix+$00)
	ret	 z
	bit	 7,(ix+$08)
	ret	 nz
	ld	 a,(ix+$1d)
	and	 a
	jp	 nz,L0D59
	dec	 (ix+$03)
	ret	 nz
	ld	 a,(ix+$05)
	and	 a
	jr	 z,L26AC
	ld	 (ix+$03),$04
	dec	 (ix+$05)
	jr	 z,L26A7
	ld	 a,(ix+$07)
	inc	 (ix+$07)
	bit	 7,a
	ld	 hl,L3348
	jr	 nz,L2667
	ld	 hl,L3352
L2667:	and	 $3F
	ld	 c,a
	ld	 b,$00
	add	 hl,bc
	add	 hl,bc
	ld	 c,(hl)
	inc	 hl
	ld	 b,(hl)
	call	 L30C7
	ld	 a,(ix+$01)
	or	 $23
	ld	 (hl),a
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),b
	inc	 d
	inc	 d
	inc	 d
	inc	 e
	inc	 e
	call	 L0938
	ld	 bc,$0000
	bit	 7,(ix+$01)
	jr	 z,L2692
	ld	 bc,$0550
L2692:	bit	 6,(ix+$01)
	jr	 z,L269C
	ld	 a,c
	add	 a,$05
	ld	 c,a
L269C:	ex	 de,hl
	add	 hl,bc
	ex	 de,hl
	inc	 hl
	ld	 (hl),e
	inc	 hl
	ld	 (hl),d
	set	 5,(ix+$08)
L26A7:	set	 7,(ix+$08)
	ret	
L26AC:	ld	 (ix+$00),$00
	bit	 7,(ix+$07)
	ret	 z
	ld	 a,(ix+$08)
	and	 $0C
	cp	 $0C
	ret	 z
	ld	 e,$0C
	cp	 $08
	jr	 z,L26E8
	ld	 e,$08
	ld	 a,(LD302)
	cp	 $07
	jr	 c,L26CE
	ld	 a,$06
L26CE:	sub	 $07
	ld	 hl,LD1D6
	inc	 (hl)
	add	 a,(hl)
	ret	 m
	ld	 hl,LD1E3
	ld	 a,(hl)
	and	 a
	jr	 nz,L26E8
	call	 L0F39
	and	 $07
	or	 $28
	ld	 (hl),a
	call	 L8009
L26E8:	ld	 d,(ix+$02)
L26EB:	call	 L0F39
	and	 $07
	inc	 a
	ld	 b,a
	xor	 a
L26F3:	add	 a,$18
	djnz	 L26F3
	ld	 c,a
	ld	 a,(LD058)
	sub	 c
	add	 a,$18
	cp	 $30
	jr	 c,L26EB
	ld	 a,(LD078)
	sub	 c
	add	 a,$18
	cp	 $30
	jr	 c,L26EB
L270C:	call	 L0F39
	and	 $03
	inc	 a
	ld	 b,a
L2713:	xor	 a
L2714:	add	 a,$18
	djnz	 L2714
	ld	 b,a
	ld	 a,(LD05A)
	sub	 b
	add	 a,$0C
	cp	 $18
	jr	 c,L270C
	ld	 a,(LD07A)
	sub	 b
	add	 a,$0C
	cp	 $18
	jr	 c,L270C
L272D:	ld	 a,c
	cp	 $78
	ld	 a,$01
	jr	 c,L2735
	xor	 a
L2735:	ld	 (LD1EA),a
	ld	 (ix+$24),$1E
	jp	 L2A0B
	nop	
L2740:	ld	 a,(LD1C9)
	and	 a
	ret	 nz
	ld	 a,$06
L2747:	push	 af
	call	 L0F1F
	call	 L2754
	pop	 af
	dec	 a
	jr	 nz,L2747
	ei	
	ret	
L2754:	bit	 7,(ix+$20)
	res	 7,(ix+$20)
	ld	 l,(ix+$21)
	ld	 h,(ix+$22)
	ld	 c,$00
	call	 nz,L27B5
	ld	 a,(ix+$00)
	and	 $88
	cp	 $80
	ret	 nz
	set	 7,(ix+$20)
	ld	 a,(ix+$06)
L2776:	add	 a,$0C
	ld	 c,$FF
L277A:	inc	 c
	sub	 $18
	jr	 nc,L277A
	ld	 b,$00
	ld	 hl,L18C4
	add	 hl,bc
	add	 hl,bc
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	ld	 a,(ix+$04)
	add	 a,$0C
	ld	 b,$00
	ld	 h,b
L2791:	inc	 b
	sub	 $18
	jr	 nc,L2791
	xor	 a
L2797:	add	 a,$02
	djnz	 L2797
	ld	 l,a
	add	 hl,de
	ld	 (ix+$21),l
	ld	 (ix+$22),h
	ld	 a,(ix+$08)
	and	 $0C
	ld	 c,$FF
	jp	 pe,L27B5
	ld	 c,$AA
	bit	 3,a
	jr	 nz,L27B5
	ld	 c,$55
L27B5:	ld	 de,L0050
	ld	 b,$04
L27BA:	ld	 (hl),c
	add	 hl,de
	djnz	 L27BA
	ret	
	nop	
L27C0:	ld	 a,$02
	ld	 (LD046),a
	call	 L27D0
	ld	 l,h
	call	 L27D0
	ld	 (LD1ED),hl
	ret	
L27D0:	ld	 a,r
	bit	 2,a
	ld	 h,$05
	jr	 nz,L27DA
	ld	 h,$09
L27DA:	bit	 3,a
	ret	 z
	inc	 h
	ret	
L27DF:	ld	 hl,LD043
	ld	 de,L2F90
	exx	
	ld	 hl,LD054
	ld	 de,LD1EF
	ld	 bc,LD1DC
	ld	 a,$04
	jr	 L2805
L27F3:	ld	 hl,LD044
	ld	 de,L2FBE
	exx	
	ld	 hl,LD074
	ld	 de,LD1F0
	ld	 bc,LD1DD
L2803:	ld	 a,$08
L2805:	ex	 af,af'
	ld	 a,(LD1DB)
	and	 a
	ret	 z
L280B:	bit	 7,(hl)
	ld	 a,$40
	jr	 z,L2825
	bit	 2,(hl)
	jr	 z,L2825
	ex	 de,hl
	dec	 (hl)
	jr	 nz,L281F
	ld	 h,b
	ld	 l,c
	ld	 (hl),$01
	jr	 L2825
L281F:	exx	
	inc	 (hl)
	exx	
	ld	 a,(hl)
	or	 $30
L2825:	exx	
	ld	 hl,LD1CB
	ld	 (hl),a
L282A:	in	 a,($10)
	bit	 7,a
	jr	 nz,L2837
	push	 hl
	ld	 hl,L02D1
	add	 hl,de
	ex	 de,hl
L2836:	pop	 hl
L2837:	ld	 b,$01
	ex	 af,af'
	jp	 L045C
L283D:	ld	 hl,LD18E
	set	 2,(hl)
	ld	 hl,LD198
	set	 3,(hl)
	ld	 hl,LD1E5
	set	 1,(hl)
	ret	
L284D:	ld	 a,$07
	ld	 (LD045),a		;
	ld	 a,(LD302)
	dec	 a
	ld	 d,$21
	jr	 nz,L285C
	ld	 d,$01
L285C:	cp	 $05
	ld	 e,$01
	jr	 nc,L286A
	ld	 c,a
	ld	 b,$00
	ld	 hl,L2882
	add	 hl,bc
	ld	 e,(hl)
L286A:	ld	 hl,LD1E9
	inc	 (hl)
	ld	 a,(hl)
	cp	 e
	jr	 c,L2874
	ld	 d,$41
L2874:	ld	 a,$06
L2876:	push	 af
	call	 L0F1F
	call	 L2887
	pop	 af
	dec	 a
	jr	 nz,L2876
	ret	
L2882:	dec	 b
	ld	 b,$04
	inc	 bc
	ld	 (bc),a
L2887:	ld	 a,(ix+$00)
	and	 $88
	cp	 $80
	ret	 nz
L288F:	ld	 a,(ix+$02)
	add	 a,a
	cp	 d
	ret	 nc
	res	 6,(ix+$00)
	cp	 $10
	jr	 c,L289F
L289D:	and	 $F0
L289F:	ld	 (ix+$02),a
	cp	 $20
	ret	 c
	ld	 b,$FE
	jr	 z,L28AB
	ld	 b,$FC
L28AB:	ld	 a,(ix+$04)
	and	 b
	ld	 (ix+$04),a
	ld	 a,(ix+$06)
	and	 b
	ld	 (ix+$06),a
	ret	
L28BA:	ld	 a,$2D
	ld	 (hl),a
	ld	 (LD04E),a
L28C0:	ld	 ix,LD094
	ld	 (ix+$00),$00
	set	 7,(ix+$08)
	ret	
L28CD:	ld	 hl,LD1C7
	ld	 a,(hl)
	and	 a
	jr	 z,L28BA
	ld	 (hl),$00
	ld	 a,(LD1C8)
	and	 a
	ret	 nz
	ld	 hl,LD243
	set	 1,(hl)
	ld	 a,(LD302)
	ld	 b,a
	ld	 a,$B0
L28E6:	sub	 $0A
	djnz	 L28E6
	cp	 $3C
	jr	 nc,L28F0
	ld	 a,$3C
L28F0:	ld	 (LD04E),a
	inc	 a
	ld	 (LD045),a		; ???
	ld	 hl,LD054
	ld	 de,LD074
	call	 L0F39
	bit	 3,a
	jr	 z,L2905
	ex	 de,hl
L2905:	ld	 a,(hl)
	and	 $98
	cp	 $80
	jr	 z,L2918
	ex	 de,hl
	ld	 a,(hl)
	and	 $98
	cp	 $80
	ld	 bc,L0078
	jp	 nz,L2998
L2918:	ld	 a,l
	ld	 de,LD054
	sub	 e
	ld	 (LD1EA),a
	push	 hl
	pop	 ix
	ld	 hl,LD1F3
	call	 L29AB
	ld	 c,a
	inc	 hl
	call	 L29AB
	ld	 h,a
	ld	 l,c
	ld	 a,(ix+$04)
	add	 a,$0C
	call	 L244D
	ld	 c,a
	ld	 a,(ix+$06)
	add	 a,$0C
	call	 L244D
	ld	 b,a
	ld	 de,LD1F3
L2945:	call	 L0F39
	bit	 2,a
	jr	 nz,L2971
	bit	 4,a
	ld	 a,(ix+$07)
	jr	 nz,L295E
	and	 $03
	cp	 $03
	call	 z,L29A2
	ld	 a,c
	add	 a,h
	jr	 L2967
L295E:	and	 $03
	cp	 $02
	call	 z,L29A2
	ld	 a,c
	sub	 h
L2967:	jr	 c,L2945
	cp	 $F1
	jr	 nc,L2945
	ld	 c,a
	inc	 de
	jr	 L2991
L2971:	bit	 4,a
	ld	 a,(ix+$07)
	jr	 nz,L2983
	and	 $03
	cp	 $01
	call	 z,L29A2
	ld	 a,b
	add	 a,l
	jr	 L298A
L2983:	and	 $03
	call	 z,L29A2
	ld	 a,b
	sub	 l
L298A:	jr	 c,L2945
	cp	 $79
	jr	 nc,L2945
	ld	 b,a
L2991:	ex	 de,hl
	ld	 a,(hl)
	cp	 $02
	jr	 c,L2998
	dec	 (hl)
L2998:	ld	 de,L200C
	ld	 ix,LD094
	jp	 L2A0B
L29A2:	call	 L0F39
	and	 $03
	ret	 z
	pop	 af
	jr	 L2945
L29AB:	ld	 b,(hl)
	xor	 a
L29AD:	add	 a,$18
	djnz	 L29AD
	ret	
	nop	
	ld	 a,(LD302)
	and	 a
	jr	 z,L29C5
	cp	 $04
	jr	 nc,L29D3
	add	 a,a
	add	 a,$03
	ld	 d,a
	ld	 a,$06
	jr	 L29C9
L29C5:	ld	 d,$06
	ld	 a,$02
L29C9:	push	 af
	call	 L29E6
	pop	 af
	inc	 d
	dec	 a
	jr	 nz,L29C9
	ret	
L29D3:	ld	 d,$20
	cp	 $07
	jr	 c,L29DB
	ld	 d,$40
L29DB:	ld	 a,$06
L29DD:	push	 af
	call	 L29E6
	pop	 af
	dec	 a
	jr	 nz,L29DD
	ret	
L29E6:	ld	 e,$04
	ld	 h,a
	call	 L0F1F
	ld	 c,h
	ld	 hl,L2A29
	add	 hl,bc
	add	 hl,bc
	call	 L0F39
	bit	 4,a
	ld	 a,$18
	jr	 z,L29FC
	xor	 a
L29FC:	add	 a,(hl)
	ld	 c,a
	call	 L0F39
	bit	 2,a
	ld	 a,$18
	jr	 z,L2A08
	xor	 a
L2A08:	inc	 hl
	add	 a,(hl)
	ld	 b,a
L2A0B:	push	 ix
	pop	 hl
	inc	 hl
	ld	 (hl),$00
	inc	 hl
	ld	 (hl),d
	inc	 hl
	ld	 (hl),$00
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),$00
	inc	 hl
	ld	 (hl),b
	inc	 hl
	ld	 (hl),$04
	inc	 hl
	ld	 (hl),e
	ld	 (ix+$1f),$E0
	ld	 (ix+$00),$80
	ret	
	jr	 L2A2D
L2A2D:	ld	 c,b
	nop	
	sub	 b
	nop	
	ret	 nz
	nop	
	ld	 c,b
	jr	 nc,L29C6
	jr	 nc,L2A38
L2A38:	push	 iy
	ld	 a,$06
L2A3C:	push	 af
	call	 L0F1F
	ld	 c,$FF
	ld	 iy,LD054
	call	 L2AF6
	ld	 a,c
	and	 a
	jp	 p,L2A90
	ld	 iy,LD074
	call	 L2AF6
	ld	 a,c
	and	 a
	jp	 p,L2A90
	ld	 a,(ix+$02)
	cp	 $10
	jr	 nc,L2A63
	ld	 a,$10
L2A63:	ld	 b,$1E
	sub	 $10
	jr	 z,L2A6B
	ld	 b,$0F
L2A6B:	ld	 a,(LD1C6)
	and	 a
	jr	 z,L2A73
	ld	 b,$00
L2A73:	ld	 (ix+$1c),b
	ld	 a,(ix+$24)
	and	 a
	jr	 nz,L2AE3
	bit	 5,(ix+$00)
	jr	 nz,L2AE3
	set	 1,(ix+$00)
	set	 7,(ix+$08)
	ld	 (ix+$1f),$E0
	jr	 L2AE3
L2A90:	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L2AE3
	res	 1,(ix+$00)
	ld	 a,(ix+$24)
	and	 a
	ld	 (ix+$24),$1E
	jr	 nz,L2AE3
	ld	 hl,LD241
	ld	 a,(ix+$08)
	and	 $0C
	jp	 pe,L2AEB
	bit	 2,a
	ld	 a,$10
	jr	 nz,L2AB7
L2AB5:	or	 (hl)
	ld	 (hl),a
L2AB7:	call	 L2614
	ld	 a,r
	ld	 b,a
	ld	 a,(LD1E9)
	cp	 $04
	jr	 nc,L2AD0
	bit	 3,b
	jr	 z,L2AD0
	ld	 a,(ix+$01)
	xor	 $0F
	ld	 (ix+$01),a
L2AD0:	bit	 4,b
	jr	 z,L2AE3
	ld	 a,(ix+$02)
	ld	 (ix+$25),a
	ld	 d,$20
	call	 L288F
	set	 6,(ix+$00)
L2AE3:	pop	 af
	dec	 a
	jp	 nz,L2A3C
	pop	 iy
	ret	
L2AEB:	ld	 a,(LD1EB)
	and	 a
	ld	 a,$08
	jr	 z,L2AB5
	rrca	
	jr	 L2AB5
L2AF6:	ld	 a,(ix+$00)
	and	 $88
	cp	 $80
	ret	 nz
	ld	 a,(iy+$00)
	and	 $98
	cp	 $80
	ret	 nz
	call	 L2B7D
	ld	 a,e
L2B0A:	sub	 l
	cp	 $F1
	jr	 nc,L2B14
	cp	 $0F
	jr	 nc,L2B48
	ex	 de,hl
L2B14:	ld	 a,h
L2B15:	call	 L244D
	ld	 h,a
	ld	 a,d
	call	 L244D
L2B1D:	sub	 h
	jr	 nc,L2B23
	ld	 h,d
	neg	
L2B23:	ld	 c,$02
L2B25:	dec	 c
	sub	 $18
	jr	 nc,L2B25
	ld	 d,h
	push	 hl
	ld	 a,c
	push	 af
	ld	 a,e
	add	 a,$11
	jr	 nc,L2B35
	ld	 a,$FF
L2B35:	ld	 e,a
	call	 L2477
	pop	 af
	pop	 de
	dec	 c
	ret	 p
	ld	 c,a
	call	 L2477
	dec	 c
	ret	 p
	ld	 c,$FF
	call	 L2B7D
L2B48:	ld	 a,d
	sub	 h
	cp	 $EF
	jr	 nc,L2B52
	cp	 $11
	ret	 nc
	ex	 de,hl
L2B52:	ld	 a,l
	call	 L244D
	ld	 l,a
	ld	 a,e
	call	 L244D
	sub	 l
	jr	 nc,L2B61
	ld	 l,e
	neg	
L2B61:	ld	 c,$02
L2B63:	dec	 c
	sub	 $18
	jr	 nc,L2B63
	ld	 e,l
	push	 hl
	ld	 a,c
	push	 af
	ld	 a,d
	add	 a,$11
	ld	 d,a
	call	 L2463
	pop	 af
	pop	 de
	dec	 c
	ret	 p
	ld	 c,a
	call	 L2463
	dec	 c
	ret	
L2B7D:	ld	 d,(ix+$06)
	ld	 e,(ix+$04)
	ld	 h,(iy+$06)
	ld	 l,(iy+$04)
	ret	
	nop	
L2B8B:	ld	 d,$00
	ld	 ix,LD054
	bit	 4,(ix+$00)
	jr	 z,L2B99
	set	 0,d
L2B99:	ld	 ix,LD074
	bit	 4,(ix+$00)
	jr	 z,L2BA5
	set	 1,d
L2BA5:	ld	 a,(LD1EB)
	and	 a
	jr	 z,L2BAD
	ld	 d,$03
L2BAD:	ld	 a,$06
L2BAF:	push	 af
	call	 L0F1F
	ld	 a,(ix+$00)
	and	 $88
	cp	 $80
	jr	 nz,L2BF7
	res	 5,(ix+$00)
	ld	 a,(ix+$04)
	cp	 $78
	ld	 a,$01
	jr	 c,L2BCB
	ld	 a,$02
L2BCB:	and	 d
	jr	 nz,L2BE5
	ld	 a,(ix+$08)
	and	 $0C
	cp	 $04
	jr	 z,L2BE5
	ld	 a,(ix+$02)
	cp	 $40
	jr	 nz,L2BF7
	ld	 a,(LD302)
	cp	 $05
	jr	 nc,L2BF7
L2BE5:	set	 5,(ix+$00)
	bit	 1,(ix+$00)
	jr	 z,L2BF3
	ld	 (ix+$1f),$E0
L2BF3:	res	 1,(ix+$00)
L2BF7:	pop	 af
	dec	 a
	jr	 nz,L2BAF
	ret	
	nop	
L2BFD:	ld	 hl,LD340
	ld	 a,(hl)
	and	 a
	ret	 z
	dec	 (hl)
	ld	 a,(LD349)
	and	 a
	ret	 nz
	ld	 iy,$1052
	inc	 a
L2C0E:	ld	 (LD050),a
	ld	 (LD053),a
	ret	
L2C15:	ld	 a,(Game_Mode)
	and	 a
	ret	 z
	ld	 a,(LD1DB)
	and	 a
	ret	 z
	in	 a,($10)
	and	 $28
	jp	 z,L2D12
	ld	 a,$06
L2C28:	push	 af
	call	 L0F1F
	pop	 af
	bit	 7,(ix+$00)
	ret	 nz
	dec	 a
	jr	 nz,L2C28
	ld	 a,(LD1BB)
	and	 a
	ret	 nz
	ld	 hl,LD1EB
	ld	 a,(hl)
	and	 a
	jr	 nz,L2C78
	ld	 a,(LD302)
	dec	 a
	jp	 z,L2CF9
	call	 L2CF0
	ld	 (hl),a
	call	 L3125
	ld	 a,(L6C68)
	ld	 (LD1F1),a
	ld	 a,(L6CA4)
	ld	 (LD1F2),a
	ld	 a,$03
	ld	 (LD047),a
	ld	 hl,LD242
	set	 7,(hl)
	ld	 iy,$121D
	ld	 a,$20
	ld	 (LD04D),a
	ld	 hl,LD1E8
	inc	 (hl)
	ld	 de,$010C
	jp	 L26EB
L2C78:	ld	 a,(LD054)
	ld	 hl,LD074
	or	 (hl)
	bit	 3,a
	ret	 nz
	ld	 a,(LD1DF)
	and	 a
	jr	 nz,L2CF9
	ld	 hl,LD1C7
	ld	 a,(hl)
	and	 a
	ret	 nz
	dec	 hl
	ld	 a,(hl)
	and	 a
	jr	 nz,L2CF9
	ld	 a,(LD302)
	cp	 $07
	jr	 c,L2CA3
	call	 L0F39
	and	 $03
	jr	 z,L2CF9
	jr	 L2CD6
L2CA3:	ld	 d,$FF
	ld	 a,(LD300)
	ld	 b,a
	ld	 a,(LD301)
	ld	 c,a
	ld	 a,(Game_Mode)
	dec	 a
	jr	 nz,L2CB7
	ld	 b,$00
	sla	 c
L2CB7:	ld	 a,c
	add	 a,b
	rrca	
	rrca	
	and	 $3F
	jr	 z,L2CC4
L2CBF:	srl	 d
	dec	 a
	jr	 nz,L2CBF
L2CC4:	ld	 a,(LD302)
	cp	 $03
	jr	 c,L2CF9
L2CCB:	srl	 d
	dec	 a
	jr	 nz,L2CCB
	call	 L0F39
	and	 d
	jr	 nz,L2CF9
L2CD6:	ld	 a,$03
	ld	 (LD04E),a
	ld	 (LD04F),a
	ld	 (hl),a
	inc	 hl
	ld	 (hl),a
	ld	 (LD1D8),a
	ld	 (LD1C9),a
	ld	 (LD048),a
	ld	 hl,$0403
	ld	 (LD1F3),hl
L2CF0:	ld	 a,$0A
	in	 a,($15)
	ld	 a,$52
	out	 ($07),a
	ret	
L2CF9:	ld	 a,$01
	ld	 (LD1DF),a
	ld	 hl,LD067
	ld	 a,(LD087)
	or	 (hl)
	and	 $80
	ret	 nz
	ld	 hl,LD054
L2D0B:	ld	 a,(LD074)
	or	 (hl)
	and	 $08
	ret	 nz
L2D12:	ld	 (LD1DB),a
	inc	 a
	ld	 (LD048),a
	ld	 a,(LD302)
	sub	 $07
	jr	 c,L2D42
L2D20:	sub	 $06
L2D22:	jr	 nc,L2D20
	add	 a,$06
	cp	 $05
	jr	 nz,L2D3D
	ld	 a,$02
	ld	 (LD350),a
	ld	 hl,LD354
	ld	 bc,L1700
L2D35:	ld	 (hl),c
	inc	 hl
	djnz	 L2D35
	ld	 a,$01
	jr	 L2D67
L2D3D:	ld	 a,$01
	ld	 (LD350),a

;
;*****************************************************************************
; Called this routine from L007E routine
; ???
;*****************************************************************************
;
L2D42:	ld	 a,(LD302)
	cp	 $07
	ld	 de,L000D
	jr	 c,L2D4F
	ld	 d,e
	ld	 e,$09
L2D4F:	call	 L0F39
	and	 $0F
	cp	 e
	jr	 nc,L2D4F
	add	 a,d
	ld	 hl,LD354
	ld	 c,a
	ld	 b,$00
	add	 hl,bc
	ld	 a,(hl)
	and	 a
	jr	 nz,L2D4F
	inc	 (hl)
	ld	 a,c
	inc	 a
	inc	 a
L2D67:	ld	 (LD318),a
	ret
;
L2D6B:	ld	 a,(Game_Mode)
L2D6E:	and	 a
	ret	 z
	ld	 a,(LD1D7)
	and	 a
	ret	 z
	ld	 a,(LD054)
	ld	 hl,LD074
	or	 (hl)
	ret	 m
	ld	 hl,LD300
	ld	 a,(hl)
	inc	 hl
	or	 (hl)
	ret	 nz
	call	 L1939
	xor	 a
	ld	 (Game_Mode),a
	ld	 (LD1D7),a
	ld	 (LD1DB),a
	inc	 a
	ld	 (LD048),a
	ld	 (LD1D9),a
	ret	
	nop	
L2D9A:	ld	 a,(LD1DB)
	and	 a
	ret	 z
	ld	 a,(ix+$00)
	ld	 b,a
	and	 $8C
	cp	 $80
	ret	 nz
	bit	 7,(ix+$08)
	ret	 nz
	bit	 4,b
	jp	 nz,L30DD
	ld	 a,(ix+$1c)
	and	 a
	jr	 z,L2DBB
	dec	 (ix+$1c)
L2DBB:	ld	 a,(ix+$01)
	and	 $0F
	ld	 (ix+$01),a
	call	 L2E4F
	ld	 a,(ix+$01)
	and	 $F0
	ret	 nz
	call	 L2E4F
	bit	 2,(ix+$07)
	ret	 nz
	ld	 a,(ix+$01)
	ld	 b,a
	and	 $F0
	ret	 nz
	ld	 a,b
	and	 $0F
	jp	 po,L2DF6
	bit	 5,(ix+$00)
	ret	 z
	res	 5,(ix+$00)
	ld	 a,(ix+$07)
	inc	 c
	exx	
	jp	 L2FE2
L2DF2:	ld	 bc,$0402
	ex	 af,af'
L2DF6:	call	 L30D6
	cp	 $04
	jr	 c,L2E14
	ld	 a,d
	add	 a,$18
L2E00:	sub	 $18
	jr	 z,L2E34
	jr	 nc,L2E00
	cp	 $F4
	ld	 c,$01
	jr	 c,L2E2D
	ld	 c,$02
	ld	 a,d
	add	 a,$0C
	ld	 d,a
	jr	 L2E2D
L2E14:	ld	 a,e
	add	 a,$18
	jr	 nc,L2E1B
	ld	 a,$FF
L2E1B:	sub	 $18
	jr	 z,L2E34
	jr	 nc,L2E1B
	cp	 $F4
	ld	 c,$04
	jr	 c,L2E2D
	ld	 c,$08
	ld	 a,e
	add	 a,$0C
	ld	 e,a
L2E2D:	call	 L1ACC
	ld	 a,b
	and	 (hl)
	jr	 nz,L2E41
L2E34:	ld	 a,(ix+$07)
	and	 $03
	ld	 c,a
	ld	 b,$00
	ld	 hl,L2DF2
	add	 hl,bc
	ld	 c,(hl)
L2E41:	call	 L2E4C
	ld	 a,(ix+$01)
	and	 $F0
	ret	 nz
	jr	 L2E4F
L2E4C:	ld	 (ix+$01),c
L2E4F:	ld	 c,$00
	exx	
	call	 L30D6
	ld	 a,(ix+$01)
	ld	 b,a
	and	 $0F
	ret	 z
	ld	 c,a
	ld	 (ix+$01),a
	ld	 a,b
	rrca	
	rrca	
	rrca	
	rrca	
	and	 $0F
	ld	 b,a
	and	 c
	jr	 z,L2E75
	ld	 a,b
	xor	 c
	jr	 z,L2E75
	cp	 $03
	jr	 c,L2E7B
	jr	 L2EA7
L2E75:	bit	 0,(ix+$00)
	jr	 z,L2EA7
L2E7B:	res	 0,(ix+$00)
	ld	 a,e
	call	 L2445
	ret	 nz
	bit	 1,c
	jr	 z,L2E94
	call	 L1ACC
	bit	 1,(hl)
	ret	 z
	ld	 b,$01
	set	 5,c
	jr	 L2ED5
L2E94:	bit	 0,c
	ret	 z
	ld	 a,$17
	add	 a,d
	ld	 d,a
	call	 L1ACC
	bit	 0,(hl)
	ret	 z
	ld	 b,$00
	set	 4,c
	jr	 L2ED5
L2EA7:	set	 0,(ix+$00)
	ld	 a,d
	call	 L2445
	ret	 nz
	bit	 3,c
	jr	 z,L2EC0
	call	 L1ACC
	bit	 3,(hl)
	ret	 z
	ld	 b,$03
	set	 7,c
	jr	 L2ED5
L2EC0:	bit	 2,c
	ret	 z
	ld	 a,$17
	add	 a,e
	jr	 nc,L2ECA
	ld	 a,$FF
L2ECA:	ld	 e,a
	call	 L1ACC
	bit	 2,(hl)
	ret	 z
	ld	 b,$02
	set	 6,c
L2ED5:	ld	 (ix+$01),c
	ld	 a,c
	and	 $50
	ld	 a,(ix+$02)
	jr	 z,L2EE2
	neg	
L2EE2:	and	 a
	jr	 nz,L2EE9
	exx	
	set	 0,c
	exx	
L2EE9:	ld	 l,a
	rla	
	ld	 h,$00
	jr	 nc,L2EF0
	dec	 h
L2EF0:	add	 hl,hl
	add	 hl,hl
	add	 hl,hl
	add	 hl,hl
	ld	 a,c
	cp	 $40
	jr	 c,L2F58
	ld	 e,(ix+$03)
	ld	 d,(ix+$04)
	add	 hl,de
	ld	 (ix+$03),l
	ld	 (ix+$04),h
	ld	 a,h
	cp	 $F1
	jr	 c,L2F65
	ld	 a,h
L2F0C:	cp	 $F8
	ld	 h,$00
	jr	 c,L2F14
	ld	 h,$F0
L2F14:	ld	 (ix+$04),h
	ld	 a,(LD1EB)
	and	 a
L2F1B:	jr	 z,L2F42
	ld	 a,(LD1C6)
	and	 a
	jr	 nz,L2F42
	bit	 2,(ix+$07)
	jr	 z,L2F65
	xor	 a
	ld	 (ix+$00),a
	ld	 a,$F3
	out	 ($07),a
	ld	 (LD1BA),a
	ld	 (LD1D8),a
	ld	 hl,LD242
	set	 5,(hl)
	call	 L06CC
	jp	 L30BA
L2F42:	ld	 a,$0A
	ld	 (LD047),a
	exx	
	ld	 hl,LD1E5
	set	 0,(hl)
	call	 L3125
	ld	 hl,LD242
	set	 4,(hl)
	exx	
	jr	 L2F65
L2F58:	ld	 e,(ix+$05)
	ld	 d,(ix+$06)
	add	 hl,de
	ld	 (ix+$05),l
	ld	 (ix+$06),h
L2F65:	ld	 a,h
	cp	 d
	ld	 c,$FF
	jr	 z,L2F70
	inc	 c
	exx	
	set	 0,c
	exx	
L2F70:	bit	 2,(ix+$07)
	jr	 z,L2F82
	ld	 hl,LD1E8
	ld	 a,(hl)
	dec	 a
	jr	 nz,L2F82
	inc	 (hl)
	exx	
	set	 0,c
	exx	
L2F82:	ld	 a,(ix+$02)
	and	 a
	jr	 z,L2F89
	inc	 c
L2F89:	ld	 d,(ix+$07)
	ld	 a,d
	and	 $03
	xor	 b
L2F90:	cp	 $01
	jr	 nz,L2FA2
	bit	 2,d
	jr	 z,L2F9E
	ld	 a,(LD1E8)
	and	 a
L2F9C:	jr	 nz,L2FA2
L2F9E:	exx	
	set	 0,c
	exx	
L2FA2:	ld	 a,(ix+$08)
	and	 $0C
	bit	 2,d
	jr	 z,L2FC8
	ld	 hl,L30BF
	sub	 $04
	jr	 z,L2FD7
	inc	 hl
	sub	 $04
	jr	 z,L2FD7
	inc	 hl
	ld	 a,(LD1EB)
	and	 a
	jr	 z,L2FD7
L2FBE:	inc	 hl
	ld	 a,(LD1C6)
	and	 a
	jr	 z,L2FD7
	inc	 hl
	jr	 L2FD7
L2FC8:	ld	 hl,L30C4
	sub	 $04
	jr	 nz,L2FD7
	inc	 hl
	ld	 a,(Game_Mode)
	dec	 a
	jr	 nz,L2FD7
	inc	 hl
L2FD7:	ld	 a,d
	dec	 c
	jr	 nz,L2FDC
	add	 a,(hl)
L2FDC:	and	 $FC
	or	 b
	ld	 (ix+$07),a
L2FE2:	ld	 de,L0008
	bit	 2,a
	ld	 hl,L385C
	jr	 z,L2FEF
	ld	 hl,L3F4C
L2FEF:	bit	 1,(ix+$08)
	jr	 z,L2FF6
	add	 hl,de
L2FF6:	bit	 1,a
	ld	 bc,$0550
	jr	 z,L3002
	add	 hl,de
	add	 hl,de
	ld	 bc,L0005
L3002:	bit	 0,a
	jr	 nz,L3009
	ld	 bc,$0000
L3009:	ld	 e,a
	bit	 1,a
L300C:	jr	 nz,L301A
	ld	 a,(LD1DA)
	and	 a
	jr	 z,L3026
	ld	 a,c
	add	 a,$05
	ld	 c,a
	jr	 L3026
L301A:	in	 a,($10)
	bit	 7,a
	jr	 nz,L3026
	ld	 b,$05
	ld	 a,c
	add	 a,$50
	ld	 c,a
L3026:	ld	 a,e
	push	 bc
	rlca	
	rlca	
	bit	 1,(ix+$08)
	jr	 z,L3033
L3030:	ld	 a,(ix+$1a)
L3033:	and	 $03
	ld	 e,a
	add	 hl,de
	add	 hl,de
	ld	 e,$20
	ld	 a,(ix+$08)
	and	 $0C
	sub	 $04
	jr	 z,L3057
	add	 hl,de
	sub	 $04
	jr	 z,L3057
	add	 hl,de
	ld	 a,(LD1EB)
	and	 a
	jr	 z,L3057
	add	 hl,de
	ld	 a,(LD1C6)
	and	 a
	jr	 z,L3057
	add	 hl,de
L3057:	ld	 c,(hl)
	inc	 hl
	ld	 b,(hl)
	call	 L30C7
	ex	 (sp),hl
	bit	 2,l
	jr	 nz,L3063
	inc	 e
L3063:	inc	 e
	inc	 d
	inc	 d
	inc	 d
	ld	 a,e
	and	 $03
	or	 $20
	bit	 4,l
	jr	 z,L3072
	or	 $80
L3072:	bit	 2,l
	jr	 z,L3078
	xor	 $43
L3078:	ex	 (sp),hl
	ld	 (hl),a
	inc	 hl
	ld	 (hl),c
	inc	 hl
	ld	 (hl),b
	call	 L0938
	ex	 de,hl
	pop	 bc
	add	 hl,bc
	ex	 de,hl
	inc	 hl
	ld	 (hl),e
	inc	 hl
	ld	 (hl),d
	bit	 1,(ix+$00)
	ret	 nz
	exx	
	bit	 0,c
	ret	 z
	bit	 1,(ix+$08)
	jr	 z,L30B6
	set	 5,(ix+$00)
	dec	 (ix+$1b)
	ret	 nz
	ld	 a,$01
	bit	 2,(ix+$07)
	jr	 z,L30AA
	ld	 a,$02
L30AA:	ld	 (ix+$1b),a
	dec	 (ix+$1a)
	jr	 nz,L30B6
	res	 1,(ix+$08)
L30B6:	set	 5,(ix+$08)
L30BA:	set	 7,(ix+$08)
	ret	
L30BF:	jr	 nz,L30E1
	jr	 nz,L30D3
	jr	 nz,L30E5
	jr	 nz,L30E7
L30C7:	push	 ix
	pop	 hl
	ld	 de,L0008
	add	 hl,de
	bit	 4,(hl)
	ld	 e,$05
	jr	 nz,L30D5
	add	 hl,de
L30D5:	inc	 hl
L30D6:	ld	 e,(ix+$04)
	ld	 d,(ix+$06)
	ret	
L30DD:	ld	 c,$01
	exx	
	ld	 a,(ix+$06)
	cp	 $79
L30E5:	jr	 c,L30F5
L30E7:	ld	 a,(ix+$06)
	sub	 $08
	ld	 (ix+$06),a
	ld	 a,(ix+$07)
	jp	 L2FE2
L30F5:	res	 4,(ix+$00)
	ld	 b,(ix+$04)
	call	 L1F1F
	ld	 bc,LD300
	call	 L1D12
	bit	 2,(ix+$08)
	jr	 nz,L310F
	call	 L1D1A
	inc	 bc
L310F:	ld	 a,(bc)
	dec	 a
	ret	 z
	cp	 $07
	jr	 nc,L3122
	push	 hl
	ld	 de,L0005
L311A:	add	 hl,de
	dec	 a
	jr	 nz,L311A
	call	 L0B92
	pop	 hl
L3122:	jp	 L0B92
L3125:	ld	 hl,LD18E
	res	 2,(hl)
	ld	 hl,LD198
	res	 3,(hl)
	ret	

;
; Strings here. Definatly starting at $3132 to $3335 or so...
;
	nop	
L3131:	dec	 bc
	ld	 c,c
	ld	 c,(hl)
	ld	 d,e
	ld	 b,l
	ld	 d,d
	ld	 d,h
L3138:	ld	 b,b
	ld	 b,e
	ld	 c,a
	ld	 c,c
	ld	 c,(hl)
	dec	 bc
	ld	 c,b
	ld	 c,c
	ld	 b,a
	ld	 c,b
	ld	 b,b
	ld	 d,e
	ld	 b,e
	ld	 c,a
	ld	 d,d
	ld	 b,l
	ld	 d,e
	rla	
	ld	 d,b
	ld	 d,d
	ld	 b,l
	ld	 d,e
	ld	 d,e
	ld	 b,b
	ld	 c,a
	ld	 c,(hl)
	ld	 b,l
	ld	 b,b
	ld	 d,b
	ld	 c,h
	ld	 b,c
	ld	 e,c
	ld	 b,l
	ld	 d,d
	ld	 b,b
	ld	 b,d
	ld	 d,l
	ld	 d,h
	ld	 d,h
	ld	 c,a
	ld	 c,(hl)
	rla	
	ld	 d,b
	ld	 d,d
	ld	 b,l
	ld	 d,e
	ld	 d,e
	ld	 b,b
	ld	 d,h
	ld	 d,a
	ld	 c,a
	ld	 b,b
	ld	 d,b
	ld	 c,h
	ld	 b,c
	ld	 e,c
	ld	 b,l
	ld	 d,d
	ld	 b,b
	ld	 b,d
	ld	 d,l
	ld	 d,h
	ld	 d,h
	ld	 c,a
	ld	 c,(hl)
	ld	 (bc),a
	ld	 c,a
	ld	 d,d
	rla	
	ld	 b,h
	ld	 b,l
	ld	 d,b
	ld	 c,a
	ld	 d,e
	ld	 c,c
	ld	 d,h
	ld	 b,b
	ld	 b,c
	ld	 b,h
	ld	 b,h
	ld	 c,c
	ld	 d,h
	ld	 c,c
	ld	 c,a
	ld	 c,(hl)
	ld	 b,c
	ld	 c,h
	ld	 b,b
	ld	 b,e
	ld	 c,a
	ld	 c,c
	ld	 c,(hl)
	inc	 de
	ld	 b,(hl)
	ld	 c,a
	ld	 d,d
	ld	 b,b
	ld	 d,h
	ld	 d,a
	ld	 c,a
	ld	 b,b
	ld	 d,b
	ld	 c,h
	ld	 b,c
	ld	 e,c
	ld	 b,l
	ld	 d,d
	ld	 b,b
	ld	 b,a
	ld	 b,c
	ld	 c,l
	ld	 b,l
	ld	 b,$50
	ld	 c,a
	ld	 c,c
	ld	 c,(hl)
	ld	 d,h
	ld	 d,e
	inc	 c
	ld	 b,d
	ld	 c,a
	ld	 c,(hl)
	ld	 d,l
	ld	 d,e
	ld	 b,b
	ld	 d,b
	ld	 c,h
	ld	 b,c
	ld	 e,c
	ld	 b,l
	ld	 d,d
	dec	 d
	ld	 d,a
	ld	 b,c
	ld	 c,c
	ld	 d,h
	ld	 b,b
	ld	 b,(hl)
	ld	 c,a
	ld	 d,d
	ld	 b,b
	ld	 c,c
	ld	 c,(hl)
	ld	 d,e
	ld	 d,h
	ld	 d,d
	ld	 d,l
	ld	 b,e
	ld	 d,h
	ld	 c,c
	ld	 c,a
	ld	 c,(hl)
	ld	 d,e
	ld	 e,$49
	ld	 c,(hl)
	ld	 d,(hl)
	ld	 c,c
	ld	 d,e
	ld	 c,c
	ld	 b,d
	ld	 c,h
	ld	 b,l
	ld	 b,b
	ld	 c,l
	ld	 c,a
	ld	 c,(hl)
	ld	 d,e
	ld	 d,h
	ld	 b,l
	ld	 d,d
	ld	 d,e
	ld	 b,b
	ld	 c,c
	ld	 c,(hl)
	ld	 b,b
	ld	 d,h
	ld	 c,b
	ld	 b,l
	ld	 b,b
	ld	 c,l
	ld	 b,c
	ld	 e,d
	ld	 b,l
	ld	 (L5241),hl
	ld	 b,l
	ld	 b,b
	ld	 c,h
	ld	 c,a
	ld	 b,e
	ld	 b,c
	ld	 d,h
	ld	 b,l
	ld	 b,h
	ld	 b,b
	ld	 d,l
	ld	 d,e
	ld	 c,c
	ld	 c,(hl)
	ld	 b,a
L3203:	ld	 b,b
	ld	 d,h
	ld	 c,b
	ld	 b,l
	ld	 b,b
	ld	 d,d
	ld	 b,c
	ld	 b,h
	ld	 b,c
	ld	 d,d
	ld	 b,b
	ld	 d,e
	ld	 b,e
	ld	 d,d
	ld	 b,l
	ld	 b,l
	ld	 c,(hl)
	dec	 h
	ld	 c,l
	ld	 c,a
	ld	 c,(hl)
	ld	 d,e
	ld	 d,h
	ld	 b,l
	ld	 d,d
	ld	 d,e
L321D:	ld	 b,b
	ld	 b,d
	ld	 b,l
	ld	 b,e
	ld	 c,a
	ld	 c,l
	ld	 b,l
	ld	 b,b
	ld	 d,(hl)
	ld	 c,c
	ld	 d,e
	ld	 c,c
	ld	 b,d
	ld	 c,h
	ld	 b,l
	ld	 b,b
	ld	 d,a
	ld	 c,b
	ld	 b,l
	ld	 c,(hl)
	ld	 b,b
	ld	 b,l
	ld	 c,(hl)
	ld	 d,h
	ld	 b,l
	ld	 d,d
	ld	 c,c
L3238:	ld	 c,(hl)
	ld	 b,a
	inc	 h
	ld	 d,h
	ld	 c,b
	ld	 b,l
	ld	 b,b
	ld	 d,e
	ld	 b,c
	ld	 c,l
	ld	 b,l
	ld	 b,b
	ld	 c,l
	ld	 b,c
	ld	 e,d
	ld	 b,l
	ld	 b,b
	ld	 b,e
	ld	 c,a
	ld	 d,d
	ld	 d,d
	ld	 c,c
	ld	 b,h
	ld	 c,a
	ld	 d,d
	ld	 b,b
	ld	 b,c
	ld	 d,e
	ld	 b,b
	ld	 d,h
	ld	 c,b
	ld	 b,l
	ld	 b,b
	ld	 d,b
	ld	 c,h
	ld	 b,c
	ld	 e,c
	ld	 b,l
	ld	 d,d
	dec	 bc
	ld	 b,b
	ld	 b,a
	ld	 b,l
	ld	 d,h
	ld	 b,b
	ld	 d,d
	ld	 b,l
	ld	 b,c
	ld	 b,h
	ld	 e,c
	ld	 b,b
	dec	 b
	ld	 d,d
	ld	 b,c
	ld	 b,h
	ld	 b,c
	ld	 d,d
	rlca	
	ld	 b,l
	ld	 d,e
	ld	 b,e
	ld	 b,c
	ld	 d,b
	ld	 b,l
	ld	 b,h
	rlca	
	ld	 b,e
	ld	 d,d
	ld	 b,l
	ld	 b,h
	ld	 c,c
	ld	 d,h
	ld	 d,e
	add	 hl,bc
	ld	 b,h
	ld	 d,l
	ld	 c,(hl)
	ld	 b,a
	ld	 b,l
	ld	 c,a
	ld	 c,(hl)
	ld	 b,b
	ld	 b,b
	rrca	
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 c,h
	ld	 c,a
	ld	 d,d
	ld	 b,h
	ld	 b,b
	ld	 b,h
	ld	 d,l
	ld	 c,(hl)
	ld	 b,a
	ld	 b,l
	ld	 c,a
	ld	 c,(hl)
	add	 hl,bc
	ld	 d,h
	ld	 c,b
	ld	 b,l
	ld	 b,b
	ld	 b,c
	ld	 d,d
	ld	 b,l
	ld	 c,(hl)
	ld	 b,c
	rlca	
	ld	 d,h
	ld	 c,b
	ld	 b,l
	ld	 b,b
	ld	 d,b
	ld	 c,c
	ld	 d,h
	dec	 d
	ld	 c,a
	ld	 d,d
	ld	 b,b
	ld	 b,(hl)
	ld	 c,a
	ld	 d,d
	ld	 b,b
	ld	 b,l
	ld	 e,b
	ld	 d,h
	ld	 d,d
	ld	 b,c
	ld	 b,b
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 d,d
	ld	 c,c
	ld	 c,a
	ld	 d,d
	ld	 d,e
	ld	 b,a
	ld	 c,a
	ld	 b,h
	ld	 c,a
	ld	 d,l
	ld	 b,d
	ld	 c,h
	ld	 b,l
	ld	 b,b
	ld	 d,e
	ld	 b,e
	ld	 c,a
	ld	 d,d
	ld	 b,l
L32D1:	ld	 b,a
	ld	 b,c
	ld	 c,l
	ld	 b,l
	ld	 b,b
	ld	 c,a
	ld	 d,(hl)
	ld	 b,l
	ld	 d,d
	ld	 e,e
	ld	 b,b
	ld	 sp,L3839
	jr	 nc,L3321
	ld	 c,l
	ld	 c,c
	ld	 b,h
	ld	 d,a
	ld	 b,c
	ld	 e,c
	ld	 b,b
	ld	 c,l
	ld	 b,(hl)
	ld	 b,a
	ld	 b,b
	ld	 b,e
	ld	 c,a
L32EE:	ld	 e,h
	ld	 e,l
	ld	 e,(hl)
	ld	 b,h
	ld	 d,l
	ld	 c,(hl)
	ld	 b,a
	ld	 b,l
	ld	 c,a
	ld	 c,(hl)
	ld	 b,d
	ld	 d,l
	ld	 d,d
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 b,a
	ld	 b,c
	ld	 d,d
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 d,h
	ld	 c,b
	ld	 c,a
	ld	 d,d
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 c,h
	ld	 d,l
	ld	 c,e
	ld	 d,a
	ld	 c,c
	ld	 e,d
	ld	 b,c
	ld	 d,d
	ld	 b,h
	ld	 b,b
	ld	 c,a
	ld	 b,(hl)
L331A:	ld	 b,b
	ld	 d,a
	ld	 c,a
	ld	 d,d
	ld	 d,d
	ld	 c,c
	ld	 c,a
L3321:	ld	 d,d
	ld	 d,e
	ld	 b,c
	ld	 c,h
	ld	 c,h
	ld	 b,b
	ld	 d,d
	ld	 c,c
	ld	 b,a
	ld	 c,b
	ld	 d,h
	ld	 d,e
	ld	 b,b
	ld	 d,d
	ld	 b,l
	ld	 d,e
	ld	 b,l
L3332:	ld	 d,d
	ld	 d,(hl)
	ld	 b,l
	ld	 b,h
	ld	 sp,L3030
	ld	 (L3030),a
	dec	 (hl)
	jr	 nc,L336F
	ld	 sp,L3030
	jr	 nc,L3376
	dec	 (hl)
	jr	 nc,L3377
L3347:	nop	
L3348:	adc	 a,$33
	jr	 z,L3380
	add	 a,d
	inc	 (hl)
	call	 c,L3634
	dec	 (hl)
L3352:	ld	 (de),a
	dec	 sp
	ld	 c,l
	scf	
	ld	 (de),a
	dec	 sp
	ld	 c,l
	scf	
	ld	 (de),a
	dec	 sp
	ld	 c,l
	scf	
	ld	 (de),a
	dec	 sp
	ld	 c,l
	scf	
	ld	 (de),a
	dec	 sp
	ld	 c,l
	scf	
	ld	 bc,L0138
	jr	 c,L3347
	inc	 (hl)
L336C:	ld	 (hl),$35
	nop	
L336F:	nop	
L3370:	nop	
	nop	
	sbc	 a,h
	jr	 c,L3368
	ld	 (hl),$9C
L3377:	jr	 c,L336C
	ld	 (hl),$9C
	jr	 c,L3370
	ld	 (hl),$9C
	jr	 c,L3374
	ld	 (hl),$9C
	jr	 c,L3378
	ld	 (hl),$A7
	scf	
	and	 a
	scf	
	call	 c,L3634
	dec	 (hl)
	nop	
	nop	
	nop	
	nop	
	cp	 b
	ld	 a,(L35EA)
	cp	 b
	ld	 a,(L35EA)
	cp	 b
	ld	 a,(L35EA)
	cp	 b
	ld	 a,(L35EA)
	cp	 b
	ld	 a,(L35EA)
	sbc	 a,c
	ld	 (hl),$99
	ld	 (hl),$DC
	inc	 (hl)
	ld	 (hl),$35
	nop	
	nop	
	nop	
	nop	
	or	 $38
	sub	 b
	dec	 (hl)
	or	 $38
	sub	 b
	dec	 (hl)
	or	 $38
	sub	 b
	dec	 (hl)
	or	 $38
	sub	 b
	dec	 (hl)
	or	 $38
	sub	 b
	dec	 (hl)
	ld	 b,h
	ld	 (hl),$44
	ld	 (hl),$DC
	inc	 (hl)
	ld	 (hl),$35
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ret	 nz
	inc	 b
	nop	
	nop	
	ret	 nz
	di	
	nop	
	nop	
	nop	
	ccf	
	rst	 38H
	inc	 c
	nop	
	nop	
	rrca	
	rst	 38H
	call	 m,$0000
	rrca	
	rst	 18H
	ld	 a,a
	nop	
	nop	
	rst	 38H
	ld	 d,l
	ld	 a,h
	nop	
	nop	
	.db	 $fd,$55
	ld	 a,h
	nop	
	nop	
	ccf	
L3401:	push	 af
	ld	 a,h
	nop	
	nop	
	rrca	
	ccf	
	ret	 p
	nop	
	nop	
	ld	 c,h
	rrca	
	pop	 bc
	nop	
	nop	
	nop	
	rrca	
	ret	 nz
	nop	
	nop	
	nop	
	inc	 bc
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ld	 bc,$0000
	nop	
	nop	
	nop	
	ld	 b,b
	nop	
	nop	
	nop	
	nop	
	ld	 d,h
L3434:	nop	
	nop	
	nop	
	nop	
	inc	 d
	inc	 b
	nop	
	nop	
	nop	
	dec	 d
	ld	 d,h
	nop	
	ret	 nz
	nop	
	dec	 e
	ld	 d,h
	inc	 d
	nop	
	nop	
	ld	 e,a
	push	 de
	ld	 d,l
	nop	
	nop	
	ld	 e,a
	rst	 38H
	push	 af
	nop	
	pop	 bc
	ld	 a,a
	rst	 38H
	call	 p,$1500
	ld	 a,a
	rst	 38H
	call	 nc,$1500
	rst	 38H
	rst	 38H
	call	 nc,$0500
	rst	 38H
	rst	 38H
	push	 af
	nop	
	dec	 b
	push	 af
	.db	  $fd,$f4
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	dec	 b
	inc	 d
	inc	 d
	ld	 b,l
	ld	 b,b
	inc	 b
	nop	
	nop	
	ld	 b,b
	ld	 b,b
	ld	 d,b
	nop	
L347A:	nop	
	ld	 b,b
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	jr	 nz,L3486
L3486:	nop	
	add	 a,b
	nop	
	add	 a,b
	nop	
	add	 a,b
	jr	 nz,L3490
	nop	
	ld	 (bc),a
L3490:	nop	
	ex	 af,af'
L3492:	ld	 (bc),a
	nop	
	ex	 af,af'
	nop	
	ld	 (bc),a
	adc	 a,d
	adc	 a,d
	ex	 af,af'
	nop	
	nop	
	xor	 a
	jp	 m,L0088
	ld	 (bc),a
	cp	 a
	cp	 $A0
	nop	
	ld	 (bc),a
	cp	 a
	rst	 38H
	and	 b
	jr	 nz,L34B5
	rst	 38H
	rst	 38H
	and	 b
	nop	
	ld	 a,(bc)
	rst	 38H
	rst	 38H
	and	 b
	nop	
	ld	 a,(bc)
L34B5:	rst	 38H
	rst	 38H
	and	 b
	nop	
	ld	 a,(bc)
	cp	 a
	rst	 38H
	ret	 pe
	nop	
	jr	 nz,L347A
	cp	 a
	ret	 po
	nop	
	add	 a,b
	and	 b
	xor	 a
	xor	 b
	nop	
	nop	
	nop	
	ld	 hl,(L808A)
	nop	
	nop	
	jr	 nz,L34D1
L34D1:	add	 a,b
	nop	
	nop	
	add	 a,b
	nop	
	nop	
	nop	
	ld	 (bc),a
	nop	
	nop	
	nop	
	nop	
	ex	 af,af'
	djnz	 L34E0
L34E0:	nop	
	inc	 de
	ld	 (bc),a
	ld	 (bc),a
	djnz	 L34E6
L34E6:	nop	
	add	 a,h
	ret	 nz
	jr	 nc,L34EB
L34EB:	nop	
	pop	 bc
	inc	 b
	nop	
	nop	
	ex	 af,af'
	nop	
	nop	
	inc	 c
	nop	
	nop	
	ld	 b,b
	ld	 b,$00
	nop	
	ret	 nz
	ld	 c,$03
	inc	 de
	nop	
	inc	 bc
	nop	
	ld	 c,b
L3502:	nop	
	nop	
	ld	 (bc),a
	ld	 bc,L2001
	nop	
	ld	 b,b
	inc	 b
	djnz	 L350E
	nop	
L350E:	nop	
	nop	
	jr	 nz,L3492
	nop	
	jr	 nc,L3538
	nop	
	inc	 c
	nop	
	ld	 bc,L2004
	call	 nz,L2300
	ld	 (bc),a
	inc	 bc
	ld	 bc,L0100
	jr	 nz,L3555
	ld	 sp,$0000
	jr	 nc,L34AA
	ld	 b,h
L352B:	nop	
	nop	
	inc	 b
	ex	 af,af'
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
L3538:	jr	 nc,L355A
	nop	
	nop	
	jr	 nz,L353E
L353E:	ld	 b,b
	nop	
	jr	 nz,L3502
	djnz	 L354C
	nop	
	nop	
	inc	 b
	nop	
	add	 a,b
	ld	 b,b
	nop	
	ld	 b,b
L354C:	nop	
	jr	 nz,L354F
L354F:	jr	 nc,L3551
L3551:	djnz	 L3583
	nop	
	nop	
L3555:	add	 a,b
	ld	 (bc),a
	nop	
	djnz	 L359A
L355A:	inc	 b
	jr	 nc,L3560
	nop	
	inc	 bc
	nop	
L3560:	nop	
	ld	 (bc),a
	nop	
	nop	
	nop	
	add	 a,b
	nop	
	nop	
	jr	 nc,L358A
	jr	 nc,L356D
	nop	
L356D:	ld	 bc,$0002
	inc	 c
	djnz	 L3574
	nop	
L3574:	add	 a,c
	nop	
	add	 a,b
	ld	 b,b
	inc	 c
	nop	
	inc	 c
	nop	
	ld	 (bc),a
	djnz	 L358F
	inc	 bc
	nop	
	nop	
	inc	 bc
L3583:	nop	
	ex	 af,af'
	nop	
	nop	
	ld	 b,b
	ld	 bc,$0000
	nop	
	inc	 bc
	nop	
	nop	
L358F:	nop	
	nop	
	nop	
	dec	 a
	ld	 e,a
	nop	
	nop	
	nop	
	dec	 sp
	ld	 d,a
	nop	
L359A:	nop	
	nop	
	rst	 38H
	ld	 d,a
	nop	
	nop	
	nop	
	push	 de
	ld	 e,a
	ret	 p
	nop	
	nop	
	.db	 $fd,$5d
	ld	 (hl),b
	nop	
	nop	
	dec	 c
	ld	 d,l
	ld	 d,b
	inc	 sp
	nop	
	dec	 (hl)
	and	 (hl)
	ld	 d,b
	.db	  $dd,$ff
	push	 de
	and	 (hl)
	ld	 (hl),b
	sbc	 a,c
	xor	 d
	xor	 d
	and	 l
	ld	 (hl),b
	.db	  $dd,$fe
	xor	 d
	sub	 l
	ld	 (hl),b
	inc	 sp
	rrca	
	push	 de
	ld	 e,l
	ld	 (hl),b
	nop	
	nop	
	dec	 (hl)
	ld	 e,a
	ret	 p
	nop	
	nop	
	dec	 (hl)
	ld	 d,a
	nop	
	nop	
	nop	
	push	 de
	ld	 d,a
	nop	
	nop	
	inc	 bc
	ld	 d,a
	rst	 10H
	nop	
	nop	
	dec	 c
	ld	 e,h
	push	 de
	ret	 nz
	nop	
	ld	 (iy-$0b),b
	ret	 nz
	nop	
	push	 de
	ld	 (hl),e
	ld	 d,l
	ret	 nz
L35EA:	nop	
	nop	
	call	 pe,$0000
	nop	
	inc	 bc
	ld	 d,a
	nop	
	nop	
	nop	
	nop	
	call	 pe,$0000
	nop	
	inc	 bc
	ld	 d,a
	nop	
	nop	
	ret	 p
	nop	
	call	 pe,$0000
	ld	 (hl),b
	nop	
	call	 pe,$0000
	ld	 a,h
L3609:	inc	 bc
	call	 pe,$0000
	ld	 d,a
	inc	 bc
	xor	 h
	nop	
	nop	
	ld	 d,l
	jp	 L3FAC
	nop	
	push	 af
	ld	 a,l
	and	 a
	scf	
	ret	 p
	dec	 c
	ld	 d,l
	and	 l
	rst	 30H
	or	 b
	jp	 LA555
	ld	 d,a
	ret	 nc
	ld	 a,a
	ld	 d,l
	xor	 d
L3629:	ld	 d,l
L362A:	ld	 d,b
	ld	 (hl),l
	ld	 d,l
	ld	 l,d
	ld	 d,l
	ld	 d,b
	ld	 d,l
	ld	 e,a
	ld	 d,l
	ld	 a,l
L3634:	ld	 (hl),b
	ld	 d,a
	.db	  $fd,$5a
	ld	 e,a
	ret	 p
	call	 m,L550D
	ld	 e,h
	nop	
	nop	
	rrca	
	.db	 $fd,$7c
	nop	
	nop	
	nop	
	nop	
	inc	 b
	nop	
	ld	 b,h
	nop	
	nop	
	inc	 d
	nop	
	ld	 h,(hl)
	xor	 d
	add	 a,b
	inc	 d
	add	 a,b
	ld	 b,h
	ld	 a,(bc)
	add	 a,b
	dec	 d
	ld	 d,b
	nop	
	nop	
	ld	 bc,L4001
	nop	
	nop	
	add	 a,b
	add	 a,e
	nop	
	nop	
	inc	 bc
	ld	 d,d
	ld	 bc,$0000
	nop	
	nop	
	ld	 ($0000),a
	nop	
	ld	 de,L4004
	nop	
	nop	
	ld	 c,(hl)
	ld	 (bc),a
	nop	
	nop	
	inc	 d
	jr	 nz,L369E
	ret	 nz
	nop	
	ld	 d,h
	ld	 bc,$0000
	ld	 bc,L0050
	ld	 bc,L0150
	ld	 b,b
	nop	
	dec	 b
	ld	 d,b
	dec	 d
	ld	 b,b
	nop	
	dec	 b
	djnz	 L3690
L3690:	nop	
	nop	
	nop	
	djnz	 L3695
L3695:	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	dec	 b
	ld	 b,b
L369E:	inc	 b
	nop	
	nop	
	ld	 (bc),a
	nop	
	inc	 b
	nop	
	nop	
	dec	 b
	ld	 b,b
	dec	 b
	ld	 b,b
	nop	
	ld	 (bc),a
	nop	
	dec	 b
	ld	 d,b
	nop	
	ld	 (bc),a
	nop	
	nop	
	ld	 d,h
	nop	
	ld	 (bc),a
	nop	
	nop	
	inc	 d
	nop	
	ld	 a,(bc)
	nop	
	nop	
	nop	
	inc	 c
	ld	 a,(bc)
	nop	
	nop	
	ld	 bc,L0A06
	nop	
	nop	
	ex	 af,af'
	ld	 b,h
	djnz	 L36CB
L36CB:	nop	
	inc	 bc
	nop	
	nop	
	nop	
	nop	
	ld	 (de),a
	ld	 c,b
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 (bc),a
	nop	
	nop	
	nop	
	ex	 af,af'
	jr	 nc,L36E3
	ld	 b,b
	dec	 b
	inc	 b
	ld	 b,b
	dec	 d
L36E3:	ld	 d,b
	dec	 b
	ld	 b,d
	daa	
	ld	 d,h
	nop	
	ld	 bc,L404C
	ld	 d,(hl)
	nop	
	dec	 d
	ld	 b,b
	nop	
	inc	 d
	nop	
	nop	
	nop	
	ld	 a,$AF
	nop	
	nop	
	nop	
	scf	
	xor	 e
	nop	
	nop	
	nop	
	rst	 38H
	xor	 e
	nop	
	nop	
	nop	
	jp	 pe,LF0AF
	nop	
	nop	
L3709:	cp	 $AE
	or	 b
	nop	
	nop	
	ld	 c,$AA
	and	 b
	inc	 sp
	nop	
	ld	 a,(LA059)
	xor	 $FF
	jp	 pe,LB059
	ld	 h,(hl)
	ld	 d,l
	ld	 d,l
	ld	 e,d
	or	 b
	xor	 $FD
	ld	 d,l
	ld	 l,d
	or	 b
	inc	 sp
	rrca	
	jp	 pe,LB0AE
	nop	
	nop	
	ld	 a,(LF0AF)
	nop	
	nop	
	ld	 a,(L00AB)
	nop	
	nop	
L3736:	jp	 pe,L00AB
	nop	
	inc	 bc
	xor	 e
	ex	 de,hl
	nop	
	nop	
	ld	 c,$AC
	jp	 pe,L00C0
	cp	 $B0
	jp	 m,L00C0
	jp	 pe,LAAB3
	ret	 nz
	nop	
	nop	
	call	 c,$0000
	nop	
	inc	 bc
	xor	 e
	nop	
	nop	
	nop	
	nop	
	call	 c,$0000
	nop	
	inc	 bc
	xor	 e
	nop	
	nop	
	ret	 p
	nop	
	call	 c,$0000
	or	 b
	nop	
	call	 c,$0000
	cp	 h
	inc	 bc
	call	 c,$0000
	xor	 e
	inc	 bc
	ld	 e,h
	nop	
	nop	
	xor	 d
	jp	 L3F5C
	nop	
	jp	 m,L5BBE
	dec	 sp
	ret	 p
	ld	 c,$AA
	ld	 e,d
	ei	
	ld	 (hl),b
	jp	 L5AAA
	xor	 e
	ret	 po
	cp	 a
	xor	 d
	ld	 d,l
	xor	 d
	and	 b
	cp	 d
	xor	 d
	sub	 l
	xor	 d
	and	 b
	xor	 d
	xor	 a
	xor	 d
	cp	 (hl)
	or	 b
	xor	 e
	cp	 $A5
	xor	 a
	ret	 p
	call	 m,LAA0E
	xor	 h
	nop	
L37A2:	nop	
L37A3:	rrca	
	cp	 $BC
	nop	
	nop	
	nop	
	nop	
	ex	 af,af'
	nop	
	adc	 a,b
	nop	
	nop	
	jr	 z,L37B1
L37B1:	sbc	 a,c
L37B2:	ld	 d,l
	ld	 b,b
	jr	 z,L37F6
	adc	 a,b
	dec	 b
	ld	 b,b
	ld	 hl,(L00A0)
	nop	
	jr	 nz,L37C9
	and	 b
	nop	
	nop	
	ld	 (bc),a
	ld	 (bc),a
	add	 a,b
	nop	
	nop	
	ld	 b,b
	ld	 b,e
L37C9:	nop	
	nop	
	inc	 bc
	and	 c
	ld	 (bc),a
	nop	
	nop	
	nop	
	nop	
	ld	 sp,$0000
	nop	
	ld	 (L8008),hl
	nop	
	nop	
	adc	 a,l
L37DC:	ld	 bc,$0000
	jr	 z,L37F1
	jr	 L37A3
	nop	
	xor	 b
	ld	 (bc),a
	nop	
	nop	
	ld	 (bc),a
	and	 b
	nop	
	ld	 (bc),a
L37EC:	and	 b
	ld	 (bc),a
	add	 a,b
	nop	
	ld	 a,(bc)
L37F1:	and	 b
	ld	 hl,(L0080)
	ld	 a,(bc)
L37F6:	jr	 nz,L37F8
L37F8:	nop	
	nop	
	nop	
	jr	 nz,L37FD
L37FD:	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ld	 a,(bc)
	add	 a,b
	ex	 af,af'
	nop	
	nop	
	ld	 bc,L0800
	nop	
	nop	
	ld	 a,(bc)
	add	 a,b
L3810:	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 bc,L0A00
	and	 b
	nop	
	ld	 bc,$0000
	xor	 b
	nop	
	ld	 bc,$0000
	jr	 z,L3822
L3822:	dec	 b
	nop	
	nop	
	nop	
	inc	 c
	dec	 b
	nop	
	nop	
	ld	 (bc),a
	add	 hl,bc
	dec	 b
	nop	
	nop	
	inc	 b
	adc	 a,b
	jr	 nz,L3833
L3833:	nop	
	inc	 bc
	nop	
	nop	
	nop	
	nop	
L3839:	ld	 hl,L8084
	nop	
	nop	
	nop	
	ld	 bc,$0000
	nop	
	inc	 b
	jr	 nc,L3850
	add	 a,b
	ld	 a,(bc)
	ex	 af,af'
	add	 a,b
	ld	 hl,(L0AA0)
	add	 a,c
	dec	 de
	xor	 b
L3850:	nop	
	ld	 (bc),a
	adc	 a,h
	add	 a,b
	xor	 c
	nop	
	ld	 hl,(L0080)
	jr	 z,L385B
L385B:	nop	
L385C:	ld	 (de),a
	dec	 sp
	add	 a,$3B
	ld	 a,d
	inc	 a
	add	 a,$3B
	nop	
	sbc	 a,b
	ld	 e,d
	sbc	 a,b
	or	 h
	sbc	 a,b
	ld	 c,$99
	sbc	 a,h
	jr	 c,L38BF
	add	 hl,sp
	inc	 b
	ld	 a,(L3950)
	ld	 l,b
	sbc	 a,c
	jp	 nz,L1C99
	sbc	 a,d
	halt	
	sbc	 a,d
	cp	 b
	ld	 a,(L3B6C)
	jr	 nz,L38BE
	ld	 l,h
	dec	 sp
	ret	 nc
	sbc	 a,d
	ld	 hl,(L849B)
	sbc	 a,e
	sbc	 a,$9B
	or	 $38
	xor	 d
	add	 hl,sp
	ld	 e,(hl)
	ld	 a,(L39AA)
	jr	 c,L3832
	sub	 d
	sbc	 a,h
	call	 pe,L469C
	sbc	 a,l
;
; At $389C starts pattern of Blue Worrior (demo screen) ???
;
L389C:	nop	
	nop	
	nop	
	ld	 d,h
	nop	
	nop	
	nop	
	inc	 bc
	dec	 d
	nop	
L38A6:	nop	
	nop	
	nop	
	dec	 d
	inc	 b
	nop	
	nop	
	dec	 b
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 bc,LD47D
L38BF:	inc	 b
	ld	 b,b
	dec	 b
	ld	 a,l
	ret	 nc
	scf	
	ld	 a,a
	rst	 38H
	.db	  $fd,$d0
	inc	 b
	ld	 b,b
	rst	 38H
	push	 af
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	nop	
	ld	 d,l
	nop	
	nop	
	nop	
	ld	 bc,L0055
	nop	
	nop	
	dec	 b
	ld	 d,l
	nop	
	nop	
	nop	
	dec	 d
L38E5:	dec	 b
	nop	
	nop	
	nop	
	ld	 d,h
	dec	 b
	ld	 b,b
	nop	
	nop	
	ld	 d,b
	ld	 bc,L0040
	dec	 b
	ld	 d,b
	dec	 d
	ld	 b,b
;
; At $38F6 starts pattern of Yellow Worrior (demo screen) ???
;
L38F6:	nop	
	nop	
	nop	
	xor	 b
	nop	
	nop	
	nop	
	inc	 bc
	ld	 hl,($0000)
	nop	
	nop	
L3903:	ld	 hl,(L0008)
	nop	
L3907:	ld	 a,(bc)
	xor	 b
	nop	
	nop	
	nop	
	nop	
	xor	 b
	and	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 (bc),a
	cp	 (hl)
	ret	 pe
	ex	 af,af'
	add	 a,b
	ld	 a,(bc)
	cp	 (hl)
	ret	 po
	dec	 sp
L391F:	cp	 a
	rst	 38H
	cp	 $E0
	ex	 af,af'
	add	 a,b
	rst	 38H
	jp	 m,L00A0
	nop	
	ld	 a,(bc)
	xor	 b
	and	 b
	nop	
	nop	
	nop	
	xor	 d
	nop	
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	nop	
	nop	
	nop	
	ld	 hl,(L000A)
	nop	
	nop	
	xor	 b
	ld	 a,(bc)
	add	 a,b
	nop	
	nop	
	and	 b
	ld	 (bc),a
	add	 a,b
	nop	
	ld	 a,(bc)
	and	 b
	ld	 hl,(L0080)
	nop	
	nop	
	ld	 d,h
	nop	
	nop	
	nop	
	inc	 bc
	dec	 d
	nop	
	nop	
	nop	
	nop	
	dec	 d
	inc	 b
	nop	
	nop	
	dec	 b
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 bc,LD47D
	inc	 b
	ld	 b,b
	dec	 b
	ld	 a,l
	ret	 nc
	scf	
	ld	 a,a
	rst	 38H
	.db	  $fd,$d0
	inc	 b
	ld	 b,b
	rst	 38H
	push	 af
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	ld	 bc,L0054
	nop	
	nop	
	nop	
	ld	 d,b
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	nop	
	nop	
	nop	
	dec	 b
	ld	 d,b
	nop	
L39AA:	nop	
	nop	
	nop	
	xor	 b
	nop	
	nop	
	nop	
	inc	 bc
	ld	 hl,($0000)
	nop	
	nop	
	ld	 hl,(L0008)
	nop	
	ld	 a,(bc)
	xor	 b
	nop	
	nop	
	nop	
	nop	
	xor	 b
	and	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 (bc),a
	cp	 (hl)
	ret	 pe
	ex	 af,af'
	add	 a,b
	ld	 a,(bc)
	cp	 (hl)
	ret	 po
	dec	 sp
	cp	 a
	rst	 38H
	cp	 $E0
	ex	 af,af'
	add	 a,b
	rst	 38H
	jp	 m,L00A0
	nop	
	ld	 a,(bc)
	xor	 b
	and	 b
	nop	
	nop	
	ld	 (bc),a
	xor	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	and	 b
	nop	
	nop	
	nop	
	nop	
	ld	 d,h
	nop	
	nop	
	nop	
	inc	 bc
	dec	 d
	nop	
	nop	
	nop	
L3A10:	nop	
	dec	 d
	inc	 b
	nop	
	nop	
	dec	 b
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 bc,LD47D
	inc	 b
	ld	 b,b
	dec	 b
	ld	 a,l
	ret	 nc
	scf	
	ld	 a,a
	rst	 38H
	.db	  $fd,$d0
	inc	 b
	ld	 b,b
	rst	 38H
	push	 af
	ld	 d,b
	nop	
	nop	
L3A38:	dec	 b
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	ld	 bc,L0054
	nop	
	nop	
	nop	
	ld	 d,l
	nop	
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	ld	 bc,L4045
	nop	
	nop	
	ld	 bc,L5041
	nop	
	nop	
	ld	 bc,$1040
	nop	
	nop	
	dec	 d
L3A5C:	ld	 b,c
	ld	 d,b
	nop	
	nop	
	nop	
	xor	 b
	nop	
	nop	
	nop	
	inc	 bc
	ld	 hl,($0000)
	nop	
	nop	
	ld	 hl,(L0008)
	nop	
	ld	 a,(bc)
	xor	 b
	nop	
	nop	
	nop	
	nop	
	xor	 b
	and	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 (bc),a
	cp	 (hl)
	ret	 pe
	ex	 af,af'
	add	 a,b
	ld	 a,(bc)
	cp	 (hl)
	ret	 po
	dec	 sp
	cp	 a
	rst	 38H
	cp	 $E0
	ex	 af,af'
	add	 a,b
	rst	 38H
	jp	 m,L00A0
	nop	
	ld	 a,(bc)
	xor	 b
	and	 b
	nop	
	nop	
	ld	 (bc),a
	xor	 b
	nop	
	nop	
	nop	
	nop	
	xor	 d
	nop	
	nop	
	nop	
	nop	
	xor	 d
	add	 a,b
	nop	
	nop	
	ld	 (bc),a
	adc	 a,d
	add	 a,b
	nop	
	nop	
	ld	 (bc),a
L3AAC:	add	 a,d
	and	 b
	nop	
	nop	
	ld	 (bc),a
	add	 a,b
	jr	 nz,L3AB4
L3AB4:	nop	
	ld	 hl,(LA082)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	ld	 hl,($0000)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	ld	 hl,($0000)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	jr	 nz,L3AD3
L3AD3:	inc	 c
	nop	
	nop	
	jr	 nz,L3AD8
L3AD8:	inc	 c
	nop	
	nop	
	ld	 hl,(L3C00)
	nop	
L3ADF:	nop	
	ld	 hl,(L3C80)
	nop	
	nop	
	ld	 (bc),a
	and	 b
	cp	 (hl)
	ld	 (bc),a
	nop	
	nop	
	xor	 d
	cp	 (hl)
	add	 a,d
	jr	 nc,L3AF0
L3AF0:	ld	 hl,(LAAAA)
	ex	 af,af'
	jr	 nz,L3B20
	cp	 a
	jp	 pe,L22A8
	xor	 d
	xor	 a
	jp	 pe,L2AA8
	xor	 b
	ld	 hl,(LA0A0)
	ld	 hl,(LAF00)
	ret	 pe
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
L3B0E:	nop	
	nop	
	and	 b
	add	 a,b
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	dec	 d
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
L3B20:	nop	
	nop	
	nop	
	dec	 d
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
L3B2B:	djnz	 L3B2D
L3B2D:	inc	 c
	nop	
	nop	
	djnz	 L3B32
L3B32:	inc	 c
	nop	
	nop	
	dec	 d
	nop	
	inc	 a
	nop	
	nop	
	dec	 d
	ld	 b,b
	inc	 a
	nop	
L3B3E:	nop	
	ld	 bc,L7D50
	ld	 bc,$0000
	ld	 d,l
	ld	 a,l
	ld	 b,c
	jr	 nc,L3B4A
L3B4A:	dec	 d
	ld	 a,l
	ld	 d,l
	inc	 b
	djnz	 L3B65
	ld	 a,a
	push	 de
	ld	 d,h
	ld	 de,L5F55
	push	 de
	ld	 d,h
	dec	 d
	ld	 d,h
	dec	 d
	ld	 d,b
	ld	 d,b
	dec	 d
	nop	
	ld	 e,a
	call	 nc,$0000
	nop	
	ld	 d,l
L3B65:	ld	 d,h
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	ld	 b,b
L3B6C:	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
L3B73:	ld	 hl,($0000)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	ld	 hl,($0000)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 a
	nop	
	nop	
	nop	
	nop	
	inc	 a
	nop	
	nop	
	jr	 nz,L3B9B
L3B9B:	cp	 (hl)
	ld	 (bc),a
	nop	
	jr	 nz,L3BA2
	cp	 (hl)
	add	 a,d
L3BA2:	jr	 nc,L3BCE
	xor	 d
	cp	 (hl)
	xor	 d
	ex	 af,af'
	ld	 hl,(LBFAA)
	jp	 pe,L00A8
	ld	 (bc),a
	xor	 a
	jp	 pe,L00A8
	nop	
	ld	 hl,(LA0A0)
	nop	
	nop	
	xor	 a
	ret	 pe
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	dec	 d
L3BCE:	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	dec	 d
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 a
	nop	
	nop	
	nop	
	nop	
	inc	 a
	nop	
	nop	
	djnz	 L3BF5
L3BF5:	ld	 a,l
	ld	 bc,$1000
	ld	 bc,L417D
	jr	 nc,L3C13
	ld	 d,l
	ld	 a,l
L3C00:	ld	 d,l
	inc	 b
	dec	 d
L3C03:	ld	 d,l
	ld	 a,a
	push	 de
	ld	 d,h
L3C07:	nop	
	ld	 bc,LD55F
	ld	 d,h
	nop	
	nop	
	dec	 d
L3C0F:	ld	 d,b
	ld	 d,b
	nop	
	nop	
L3C13:	ld	 e,a
	call	 nc,$0000
	nop	
L3C18:	ld	 d,l
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
L3C1E:	ld	 d,b
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	ld	 hl,($0000)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	ld	 hl,($0000)
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
L3C3C:	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 a
	nop	
	nop	
	jr	 nz,L3C4A
L3C4A:	inc	 a
	nop	
	nop	
	jr	 nz,L3C4F
L3C4F:	cp	 (hl)
	ld	 (bc),a
	nop	
	ld	 hl,(LBE82)
	add	 a,d
	jr	 nc,L3C82
	xor	 d
	cp	 (hl)
	xor	 d
	ex	 af,af'
	nop	
	ld	 hl,(LEABF)
	xor	 b
	nop	
	xor	 d
	xor	 a
	jp	 pe,L22A8
	xor	 b
	ld	 hl,(LA0A0)
	ld	 (LAFA0),hl
	ret	 pe
	nop	
	ld	 hl,(LAA00)
	xor	 b
	nop	
	nop	
	nop	
	nop	
	and	 b
	add	 a,b
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
L3C80:	nop	
	dec	 d
L3C82:	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	dec	 d
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 a
	nop	
	nop	
	djnz	 L3CA4
L3CA4:	inc	 a
	nop	
	nop	
	djnz	 L3CA9
L3CA9:	ld	 a,l
	ld	 bc,$1500
	ld	 b,c
	ld	 a,l
	ld	 b,c
	jr	 nc,L3CC7
	ld	 d,l
	ld	 a,l
	ld	 d,l
	inc	 b
	nop	
	dec	 d
	ld	 a,a
	push	 de
	ld	 d,h
	nop	
	ld	 d,l
	ld	 e,a
	push	 de
	ld	 d,h
	ld	 de,$1554
	ld	 d,b
	ld	 d,b
	ld	 de,L5F50
	call	 nc,$1500
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	ld	 b,b
	nop	
	nop	
	nop	
	inc	 bc
	nop	
	nop	
	nop	
	nop	
	nop	
	jp	 z,$0000
	nop	
	ld	 (bc),a
	jp	 z,$0000
	nop	
	dec	 bc
	jp	 pe,L0080
	nop	
	ld	 a,(bc)
	jp	 m,L0080
	nop	
	dec	 bc
	jp	 pe,L00A0
	ld	 (bc),a
	ld	 a,(bc)
	jp	 m,L02A8
	ld	 (bc),a
	xor	 e
	jp	 (hl)
L3CFC:	jr	 z,L3D08
	adc	 a,d
	xor	 d
L3D00:	ret	 m
	jr	 z,L3D2B
	xor	 d
	xor	 d
	xor	 d
	xor	 b
	nop	
L3D08:	xor	 d
	xor	 d
	xor	 d
	xor	 b
	ld	 (LAAAA),hl
	xor	 d
	add	 a,b
	ld	 hl,(LAAAA)
	xor	 d
	nop	
	nop	
	xor	 d
	xor	 d
	and	 b
	nop	
	nop	
	ld	 hl,(L00AA)
	ret	 p
	nop	
	ld	 hl,(L0AA0)
	call	 m,L0A00
	add	 a,b
	ex	 af,af'
	call	 m,L0200
	xor	 d
	xor	 b
	inc	 c
;
; At $3D2F starts pattern of Thorwor (demo screen) ???
;
L3D2F:	nop	
	nop	
	nop	
	nop	
	ret	 p
	nop	
L3D35:	ld	 a,(bc)
	nop	
	inc	 bc
	ret	 nz
	nop	
L3D3A:	jr	 nz,L3CFC
	rrca	
	ret	 nz
	nop	
	add	 a,e
	ret	 p
	inc	 c
L3D42:	nop	
	nop	
	rrca	
	call	 m,L000F
	nop	
	inc	 a
	rst	 38H
	inc	 bc
	nop	
	nop	
	ret	 p
	rst	 38H
	inc	 bc
	ret	 p
	nop	
	pop	 af
	rst	 38H
	ret	 nz
L3D56:	jr	 nc,L3D67
	rst	 38H
	rst	 38H
	ret	 p
	inc	 a
	ccf	
	rst	 38H
	rst	 38H
	rst	 38H
	call	 m,LFF00
	rst	 38H
	rst	 38H
	ret	 p
	inc	 a
L3D67:	inc	 bc
	rst	 38H
	rst	 38H
	ret	 p
	ccf	
	rst	 38H
	rst	 38H
	rst	 38H
	ret	 nz
	nop	
	rst	 38H
	inc	 c
	jr	 nc,L3D35
	nop	
	nop	
	inc	 c
	jr	 nc,L3D3A
	nop	
	nop	
L3D7C:	inc	 a
	di	
	ret	 nz
	nop	
	nop	
	jr	 nc,L3D46
	nop	
	nop	
	nop	
	ld	 d,c
	ld	 b,l
	nop	
	nop	
	nop	
	nop	
	nop	
	ret	 p
	nop	
	ld	 a,(bc)
	nop	
	nop	
	ret	 nz
	nop	
	jr	 nz,L3D56
	nop	
	ret	 nz
	nop	
L3D99:	add	 a,e
	ret	 p
	nop	
	ret	 p
	nop	
L3D9E:	rrca	
	call	 m,L3000
	nop	
	inc	 a
	rst	 38H
	nop	
	jr	 nc,L3DA8
L3DA8:	ret	 p
	rst	 38H
	nop	
	jr	 nc,L3DB0
	pop	 af
	rst	 38H
	ret	 nz
L3DB0:	inc	 a
	ccf	
	rst	 38H
	rst	 38H
	ret	 p
	inc	 a
	inc	 a
	ccf	
	rst	 38H
	rst	 38H
	call	 m,L0F01
	rst	 38H
	rst	 38H
	ret	 p
	nop	
	inc	 bc
	rst	 38H
	rst	 38H
	ret	 p
	ld	 sp,LFF3F
	rst	 38H
	ret	 nz
	ccf	
	rst	 38H
	call	 z,LC030
	inc	 bc
	call	 m,L300C
	ret	 nz
	nop	
	nop	
	inc	 c
	jr	 nc,L3D99
	nop	
	nop	
	inc	 c
	jr	 nc,L3D9E
	nop	
	nop	
	inc	 d
	ld	 d,c
	ld	 b,b
	nop	
	adc	 a,b
	inc	 bc
	call	 m,$0000
L3DE9:	ld	 (bc),a
	nop	
	rrca	
	ret	 nz
	nop	
L3DEE:	nop	
	ret	 nz
	nop	
	ret	 nz
	nop	
	inc	 bc
	ret	 p
	nop	
	ret	 nz
	nop	
	rst	 38H
	call	 m,LC000
	nop	
	inc	 a
	rst	 38H
	nop	
	ret	 p
	nop	
	ret	 p
	rst	 38H
	nop	
	jr	 nc,L3E16
	pop	 af
	rst	 38H
	ret	 nz
	inc	 a
	inc	 a
	ccf	
L3E0D:	rst	 38H
	ret	 p
	inc	 a
L3E10:	ld	 sp,LFF0F
	rst	 38H
	call	 m,L0301
	rst	 38H
	rst	 38H
	ret	 p
	nop	
	inc	 bc
	rst	 38H
	rst	 38H
L3E1E:	ret	 p
L3E1F:	nop	
	rrca	
	rst	 38H
	rst	 38H
	ret	 nz
	ld	 sp,LCC0F
	jr	 nc,L3DE9
	inc	 a
	ccf	
L3E2B:	inc	 c
	jr	 nc,L3DEE
	rrca	
	call	 m,L3C0F
	ret	 p
	nop	
	nop	
	inc	 bc
	inc	 c
	jr	 nc,L3E39
L3E39:	nop	
L3E3A:	dec	 b
	inc	 d
	ld	 d,b
	nop	
L3E3E:	rrca	
	jr	 nc,L3E41
L3E41:	nop	
	nop	
	rrca	
	inc	 a
	nop	
	nop	
L3E47:	nop	
	inc	 c
	inc	 a
	nop	
	nop	
	nop	
	inc	 a
	rst	 38H
	jp	 nz,$0000
	inc	 a
	rst	 38H
L3E54:	ret	 p
	add	 a,b
	nop	
	inc	 a
	call	 m,L203C
	nop	
	ccf	
	.db	  $fd,$0f
	jr	 nz,L3E71
	rrca	
	rst	 38H
	rst	 38H
	ret	 nz
	rra	
	rrca	
	rst	 38H
	rst	 38H
L3E69:	nop	
	inc	 bc
	rst	 38H
	rst	 38H
	call	 m,$1000
	rrca	
L3E71:	rst	 38H
	ret	 p
	nop	
	rra	
	rrca	
	rst	 38H
	nop	
	nop	
	inc	 bc
	rst	 38H
	call	 m,$0000
	djnz	 L3E8F
	ret	 p
	rrca	
	ret	 nz
	rra	
	rrca	
	ret	 p
	call	 m,L03F0
	rst	 38H
	ret	 p
	ret	 nz
	call	 m,L0300
L3E8F:	rst	 38H
	ret	 nz
	inc	 c
	nop	
	nop	
	inc	 a
	nop	
	nop	
	nop	
	inc	 a
	inc	 a
	nop	
	nop	
	nop	
	jr	 nc,L3EDB
	nop	
	nop	
	nop	
	call	 p,L004F
	nop	
	nop	
	ret	 p
	rrca	
	jp	 nz,$0000
	call	 m,LF03F
	add	 a,b
	nop	
	call	 m,L3CFC
	jr	 nz,L3EB6
L3EB6:	ccf	
	.db	  $fd,$0f
	jr	 nz,L3EBB
L3EBB:	ccf	
	rst	 38H
	rst	 38H
	ret	 nz
	djnz	 L3ED0
	rst	 38H
	rst	 38H
	nop	
	rra	
	rst	 38H
	rst	 38H
	call	 m,$0000
	rrca	
	rst	 38H
	ret	 p
	nop	
	nop	
	rrca	
L3ED0:	rst	 38H
	ret	 p
	nop	
	rra	
	rst	 38H
	call	 m,$0000
	nop	
	rrca	
	ret	 p
L3EDB:	nop	
	nop	
	djnz	 L3EEE
	ret	 p
	nop	
	nop	
	rra	
	rst	 38H
	ret	 p
	inc	 bc
	call	 m,L0300
	rst	 38H
	rst	 38H
	inc	 c
	nop	
	nop	
L3EEE:	ccf	
	nop	
	nop	
	nop	
	ret	 p
	inc	 a
	nop	
	nop	
	inc	 bc
	ret	 nz
	rrca	
	nop	
	nop	
	inc	 bc
	djnz	 L3F51
	nop	
	nop	
	inc	 bc
	nop	
	inc	 bc
	ret	 nz
	ex	 af,af'
	inc	 bc
	ret	 nz
L3F07:	rrca	
	ret	 p
	nop	
	inc	 bc
	call	 m,L3C3C
	ex	 af,af'
L3F0F:	nop	
	rst	 38H
	.db	  $fd,$0f
	jr	 nz,L3F15
L3F15:	ccf	
	rst	 38H
L3F17:	rst	 38H
	ret	 nz
	nop	
	rrca	
	rst	 38H
	rst	 38H
	nop	
	inc	 de
	rst	 38H
	rst	 38H
	call	 m,L1F00
	rrca	
	rst	 38H
	ret	 p
	inc	 c
	nop	
	rrca	
	rst	 38H
	nop	
	inc	 c
	inc	 de
	rst	 38H
	call	 m,L0C00
	rra	
	rrca	
	ret	 p
	nop	
	inc	 a
	nop	
	rrca	
	ret	 p
	nop	
	jr	 nc,L3F50
	rst	 38H
	ret	 p
	ccf	
	ret	 p
	rra	
	inc	 bc
	rst	 38H
	ret	 p
	nop	
	nop	
L3F47:	nop	
	ccf	
	nop	
	nop	
	nop	
L3F4C:	and	 b
	sbc	 a,l
	and	 b
	sbc	 a,l
L3F50:	jp	 m,L549D
	sbc	 a,(hl)
	cp	 h
	sbc	 a,a
	ld	 d,$A0
	ld	 (hl),b
	and	 b
	jp	 z,LAEA0
	sbc	 a,(hl)
	xor	 (hl)
	sbc	 a,(hl)
	ex	 af,af'
	sbc	 a,a
	ld	 h,d
	sbc	 a,a
	inc	 h
	and	 c
	ld	 a,(hl)
	and	 c
	ret	 c
	and	 c
	ld	 (L0EA2),a
	sub	 a
	ld	 l,b
	sub	 a
	ld	 l,b
	sub	 a
	push	 de
	inc	 a
	sub	 h
	xor	 c
	xor	 $A9
	ld	 c,b
	xor	 d
	and	 d
	xor	 d
	nop	
	sub	 (hl)
	ld	 e,d
	sub	 (hl)
	ld	 e,d
	sub	 (hl)
	or	 h
	sub	 (hl)
	call	 m,L56AA
	xor	 e
	or	 b
	xor	 e
	ld	 a,(bc)
	xor	 h
L3F8C:	dec	 a
L3F8D:	ld	 a,$97
	ld	 a,$F1
	ld	 a,$97
	ld	 a,$64
	xor	 h
	cp	 (hl)
	xor	 h
	jr	 L3F47
	ld	 (hl),d
	xor	 l
	cpl	
	dec	 a
	adc	 a,c
	dec	 a
	ex	 (sp),hl
	dec	 a
	adc	 a,c
	dec	 a
	call	 z,L26AD
	xor	 (hl)
	add	 a,b
	xor	 (hl)
	jp	 c,L8CAE
	and	 d
	adc	 a,h
	and	 d
	and	 $A2
	ld	 b,b
	and	 e
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	sbc	 a,d
	and	 e
	sbc	 a,d
	and	 e
	call	 p,L4EA3
	and	 h
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	xor	 b
	and	 h
	ld	 (bc),a
	and	 l
	ld	 e,h
L3FD1:	and	 l
	or	 (hl)
	and	 l
	ld	 a,b
	and	 a
	jp	 nc,L2CA7
	xor	 b
	jp	 nc,L10A7
	and	 (hl)
	ld	 l,d
	and	 (hl)
	call	 nz,L1EA6
	and	 a
	add	 a,(hl)
	xor	 b
	ret	 po
	xor	 b
	ld	 a,(LE0A9)
	xor	 b
	nop	
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
L3FFF:	rst	 38H
;
;
;
	.org	 $8000

L8000:	jp	 L84F2
L8003:	jp	 L86C1
L8006:	jp	 L8316		; Entry point from startup code
L8009:	jp	 L827D
	jp	 L8253
L800F:	bit	 7,a
	jp	 p,L8018
	neg	
	res	 7,a
L8018:	ret
;
;**********************************************************
; Purpose:	??? 
;		Set up 8 music ports
;
; Input:	DE
;
; Output:	(DE+17)=1
;		(DE+3) to (DE+16) = 0 (14 bytes)
;		out (DE) <- (DE+3) to (DE+11) (8 bytes)
;		(DE-1) to (DE-42) = 0 (42 bytes)
;		Switch main registers
;		
;**********************************************************
;
L8019:	ld	 hl,L0011	                           
	add	 hl,de		                            
	ld	 (hl),$01	; (DE+17) = 1	 
	
	push	 de		                    
	ld	 hl,$0000	                            
	add	 hl,de		                      
	ld	 c,(hl)		; c=(DE)
	
	ld	 b,$08		                             
	push	 bc		
	
	ld	 hl,L0003	                          
	add	 hl,de		
	
	ld	 bc,L000D	                                
	push	 hl		               
	push	 hl		                          
	pop	 de		
	
L8032:	ld	 (hl),$00	                    
	inc	 de		             
	ldir			; (DE+3) to (DE+16) = 0 (14 bytes)
				
	
	;Output 8 bytes to music ports
	pop	 hl		
	pop	 bc		
	otir			; out (DE) <- (DE+3) to (DE+11) (8 bytes)
				
	
	pop	 hl		
	dec	 hl		
	push	 hl		  
	pop	 de		
	dec	 de		
	ld	 bc,L0029	
	ld	 (hl),$00	
	lddr			; (DE-1) to (DE-42) = 0 (42 bytes)
				
	
	exx			; Switch registers
	
	ret			
				
;
;**********************************************************
;



L8049:	push	 iy		
	pop	 hl
	add	 hl,de
	dec	 (hl)
	jp	 nz,L80E1
	inc	 hl
	ld	 a,(hl)
	dec	 hl
	ld	 (hl),a
	inc	 hl
	inc	 hl
	bit	 0,(hl)
	ret	 z
	ld	 c,(hl)
	inc	 hl
	bit	 2,c
	jp	 nz,L80CF
	bit	 3,c
	jp	 z,L80A5
	inc	 hl
	inc	 hl
	inc	 hl
	xor	 a
	cp	 (hl)
	jp	 z,L8089
	dec	 (hl)
	jp	 nz,L8089
	res	 0,c
	ld	 de,L0006
	or	 a
	sbc	 hl,de
	ld	 (hl),$00
	bit	 5,c
	jp	 z,L8085
	ld	 (iy+$11),$01
L8085:	inc	 hl
	inc	 hl
	ld	 (hl),c
	ret	
L8089:	dec	 hl
L808A:	dec	 hl
	dec	 hl
	res	 3,c
	bit	 1,c
	jp	 nz,L8099
	xor	 a
	sub	 (hl)
	ld	 (hl),a
	jp	 L80A5
L8099:	dec	 hl
	ld	 (hl),c
	bit	 4,c
	inc	 hl
	inc	 hl
	jp	 z,L80A3
	inc	 hl
L80A3:	ld	 b,(hl)
	ret	
L80A5:	ld	 a,b
	add	 a,(hl)
	ld	 b,a
	inc	 hl
	cp	 (hl)
	jp	 nc,L80B4
	set	 4,c
	set	 3,c
	jp	 L80C8
L80B4:	jp	 nz,L80BE
	set	 4,c
	set	 3,c
	jp	 L80C8
L80BE:	inc	 hl
	cp	 (hl)
	jp	 c,L80C7
	res	 4,c
	set	 3,c
L80C7:	dec	 hl
L80C8:	dec	 hl
	dec	 hl
	ld	 (hl),c
	ret	
	jp	 L80DE
L80CF:	inc	 hl
	push	 hl
	ld	 a,(hl)
	inc	 hl
	sub	 (hl)
	neg	
	ld	 e,a
	ld	 d,$00
	ld	 a,r
	pop	 hl
	add	 a,(hl)
	ld	 b,a
L80DE:	jp	 L80E4
L80E1:	inc	 hl
	inc	 hl
	ld	 c,(hl)
L80E4:	ret	
	nop	
L80E6:	xor	 a
	cp	 (iy+$11)
	jp	 nz,L81C5
	cp	 (iy+$10)
	jp	 nz,L81C5
	inc	 (iy+$10)
	cp	 (iy+$0d)
	jr	 z,L8104
	dec	 (iy+$0d)
	jr	 nz,L8104
	ld	 (iy+$11),$01
L8104:	cp	 (iy-$2a)
	jr	 z,L8116
L8109:	ld	 b,(iy-$06)
	ld	 de,LFFD6
	call	 L8049
	ld	 (iy-$06),b
	xor	 a
L8116:	cp	 (iy-$1c)
	jr	 z,L8136
	ld	 a,(iy-$04)
	call	 L800F
	ld	 b,a
	ld	 de,LFFE4
	call	 L8049
	ld	 a,b
	ld	 b,(iy-$04)
	bit	 7,b
	jr	 z,L8132
	neg	
L8132:	ld	 (iy-$04),a
	xor	 a
L8136:	cp	 (iy-$23)
	jr	 z,L8148
	ld	 b,(iy+$04)
	ld	 de,LFFDD
	call	 L8049
	ld	 (iy+$04),b
	xor	 a
L8148:	cp	 (iy-$07)
	jr	 z,L818B
	inc	 a
	cp	 (iy-$07)
	jr	 nz,L817E
	ld	 a,(iy+$0c)
	or	 a
	jr	 z,L817E
	ld	 a,(iy-$06)
	rlca	
	rlca	
	rlca	
	rlca	
	and	 $0F
	inc	 a
	ld	 (iy-$0d),a
	ld	 a,(iy-$09)
	ld	 e,a
	rrca	
	rrca	
	rrca	
	rrca	
	or	 e
	ld	 (iy+$05),a
	ld	 (iy-$08),$01
	ld	 (iy-$0e),$01
	ld	 (iy-$0c),$03
L817E:	ld	 b,(iy+$0b)
	ld	 de,LFFF9
	call	 L8049
	ld	 (iy+$0b),b
	xor	 a
L818B:	cp	 (iy-$0e)
	jr	 z,L81AF
	ld	 a,(iy+$05)
	and	 $0F
	ld	 b,a
	ld	 de,LFFF2
	call	 L8049
	ld	 a,b
	rrca	
	rrca	
	rrca	
	rrca	
	or	 b
	ld	 (iy+$05),a
	ld	 a,(iy+$06)
	and	 $F0
	or	 b
	ld	 (iy+$06),a
	xor	 a
L81AF:	cp	 (iy-$15)
	jr	 z,L81C1
	ld	 b,(iy+$07)
	ld	 de,LFFEB
	call	 L8049
	ld	 (iy+$07),b
	xor	 a
L81C1:	xor	 a
	ld	 (iy+$10),a
L81C5:	ld	 c,(iy+$00)
	ld	 a,$17
	cp	 c
	jr	 nc,L81D8
	push	 iy
	pop	 hl
	ld	 de,$0004
	add	 hl,de
	ld	 b,$08
	otir	
L81D8:	ret
;
; Speech Routines ???
;
L81D9:	in	 a,($12)		; Check to see if phoneme is complete
	bit	 7,a			; Active HIGH (ready to accept phonemes)
	jr	 z,L81F7		; Not ready for new phoneme, so return
	ld	 hl,Num_Phonemes_Left		; Get length of phoneme left to go...
	dec	 (hl)			; ... and decrement the count
	inc	 hl			; $D2D1 holds inflection bit 7 ???	
	ld	 a,(hl)			; 
	ld	 hl,(LD2CE)		; $D2CE holds the address to the next phoneme
	xor	 (hl)			; ... moves phoneme into A ???
	inc	 hl			; Next phoneme address
	ld	 (LD2CE),hl		; ... and store it
	ld	 b,a			; B holds raw phoneme with inflection bits (iipp pppp)
	and	 $80			; A holds just bit 7 of phoneme (inflection bit?)
	ld	 (LD2D1),a		; Store it. $D2D1 holds inflection bit???
	ld	 c,$17			; C=Speech Port
	in	 a,(c)			; Speech output (a=phoneme, c=speech port)
L81F7:	ret				; ... and we are done with that phoneme, go back
;
;
;
L81F8:	ld	 a,(Num_Phonemes_Left)		; Load A with length of phoneme left to go		
	or	 a			; if no more phonemes...
	jr	 z,L8201		; ... then skip the phoneme output routine
	jp	 L81D9			; Otherwise, call speech phoneme output routine
L8201:	xor	 a			; Zero A
	ld	 hl,(LD2D2)		; HL = ???
	ld	 de,(LD2D4)		; DE = ???
	sbc	 hl,de			; HL = HL - DE
	jr	 z,L8234		; If HL = 0, then skip to STOP phoneme
	ex	 de,hl			
	ld	 e,(hl)
	inc	 hl
L8210:	ld	 d,(hl)
	ex	 de,hl
	ld	 a,(hl)
	ld	 (Num_Phonemes_Left),a
	inc	 hl
	ld	 (LD2CE),hl
	ld	 hl,(LD2D4)
	ld	 de,$0002
	add	 hl,de
	ex	 de,hl
	ld	 hl,LD2CC
	or	 a
	sbc	 hl,de
	jr	 nc,L822D
	ld	 de,LD2BE
L822D:	ld	 (LD2D4),de
	jp	 L81D9
L8234:	xor	 a
	ld	 (Is_Speech_Active),a	; Mark speech inactive
	ld	 bc,L3F17		; Write a STOP phoneme... (end of string?)
	in	 a,(c)			; 
	ret
;
;******************************************************************************
;
;******************************************************************************
;

L823E:	ld	 hl,LD2BD
	or	 a			; a = 0? or is it clear C flag?
					; It seems to serve no purpose ???
	sbc	 hl,de			
	jr	 c,L8248		; Jump if HL - DE is negative
	ld	 a,$01
L8248:	ld	 hl,LD2CC		
	or	 a			; a = 0? or is it clear C flag?
					; It seems to serve no purpose ???
	sbc	 hl,de
	jr	 nc,L8252		; Jump if HL - DE is positive
	ld	 a,$01
L8252:	ret	

;
;******************************************************************************
;
;******************************************************************************
;

L8253:	exx	
	ld	 de,(LD2D2)
	xor	 a
	call	 L823E
	ld	 de,(LD2D4)
	call	 L823E
	or	 a
	jr	 z,L827B		
	xor	 a
	ld	 (Is_Speech_Active),a		; Set speech to inactive
	ld	 hl,LD2BE
	ld	 (LD2D2),hl
	ld	 (LD2D4),hl
	ld	 (Num_Phonemes_Left),a		; Set number of phonemes left to zero
	ld	 bc,L3F17			; Write a STOP phoneme (stop speech)
	in	 a,(c)		
L827B:	exx	
	ret
	
;
;******************************************************************************
;
;******************************************************************************
;

L827D:	cp	 $50
	jr	 nc,L82F4
	ld	 hl,L9514
	inc	 a
	ld	 c,a
	ld	 a,(LD347)
	in	 a,($13)
	bit	 3,a
	jr	 nz,L8292
	ld	 hl,(LC002)
L8292:	ld	 a,$7F
L8294:	cp	 (hl)
	jr	 nc,L8298
	dec	 c
L8298:	inc	 hl
	jr	 nz,L8294
	dec	 hl
	ld	 a,(hl)
	and	 $7F
	jr	 z,L82F4
	ld	 c,a
	di	
L82A3:	inc	 hl
	ld	 a,(LD350)
	or	 a
	ld	 a,(hl)
	jr	 z,L82B7
	cp	 $09
	jr	 nz,L82B1
	ld	 a,$40
L82B1:	cp	 $37
	jr	 nz,L82B7
	ld	 a,$41
L82B7:	exx	
	rlca	
	ld	 hl,L9476
	ld	 e,a
	ld	 a,(LD347)
	in	 a,($13)
	bit	 3,a
	jr	 nz,L82C9
	ld	 hl,(LC000)
L82C9:	ld	 d,$00
	add	 hl,de
	ld	 e,(hl)
	ld	 a,e
	inc	 hl
	or	 (hl)
	jr	 z,L82EA
	ld	 d,(hl)
	ld	 hl,(LD2D2)
	ld	 (hl),e
	inc	 hl
	ld	 (hl),d
	inc	 hl
	ex	 de,hl
	ld	 hl,LD2CC
	and	 a
	sbc	 hl,de
	jr	 nc,L82E6
	ld	 de,LD2BE
L82E6:	ld	 (LD2D2),de
L82EA:	exx	
	dec	 c
	jr	 nz,L82A3
	ld	 a,$01
	ld	 (Is_Speech_Active),a	; Mark speech as active
	ei	
L82F4:	ret
;
;**********************************************************
; ???
;
; Input:	DE
;		A
;
; Output:	(DE)=A
;		(DE+1)=$40
;		(DE+2)=$87
; 
;**********************************************************
;
L82F5:	ld	 hl,$0000	                          
	add	 hl,de		
	
	ld	 (hl),a		                          
	ld	 hl,$0001	                          
	add	 hl,de		
	
	ld	 (hl),$40	; 
	inc	 hl		
	ld	 (hl),$87	;
	
L8303:	jp	 L8019		

;
;**********************************************************
; Temporary comments on right. Strange code...
; Called from: l8316 (maybe others)
; Essentially, a=$18, de'=$d270, jump to more code  ???
;**********************************************************
;

L8306:	ld	 a,$18		                                 
L8308:	exx			;Change other reg's to prime set        
	ld	 de,LD270	;de=$d270 (static RAM)                  
	jr	 L82F5		;Call continues through $82f5           
				;then through $8019 which does a return
;
;**********************************************************
; 
;**********************************************************
;
L830E:	ld	 a,$58			 
	exx	
	ld	 de,LD2AC	; ???
	jr	 L82F5		;Call continues through $82f5          
				;then through $8019 which does a return
;
;**********************************************************
; Jump here if you execute $8006...
; ???
;**********************************************************
;
L8316:	call	 L8306		;  
	call	 L830E		; ... and another ???           
	jp	 L8253		; ... Oh, and now jump away! ???
;
;**********************************************************
;

	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	ex	 de,hl
	xor	 a
	ret
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;		
;		
;**********************************************************
;
L8325:	push	 iy
	pop	 de
	push	 hl
	ld	 c,a
	ld	 hl,$0000
	add	 hl,de
	ld	 a,(hl)
	sub	 b
	ld	 b,c
	ld	 c,a
	out	 (c),b
	pop	 hl
	add	 hl,de
	ld	 (hl),b
	exx	
	xor	 a
	ret
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$08
	ld	 hl,L000B
	jr	 L8325
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$01
	ld	 hl,$0004
	jr	 L8325
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$02
	ld	 hl,L0005
	jp	 L8325
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$03
	ld	 hl,L0006
	jp	 L8325
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$04
	ld	 hl,L0007
	jp	 L8325
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$07
	ld	 hl,L000A
	jp	 L8325
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$06
	ld	 hl,L0009
	jp	 L8325
;
;**********************************************************
; Purpose: ??? 
;
; Input:	HL
;		
;
; Output:	
;		
;**********************************************************
;
	ld	 a,(hl)
	inc	 hl
	exx	
	ld	 b,$05
	ld	 hl,L0008
	jp	 L8325
	exx	
	push	 iy
	pop	 de
	jp	 L8019
	ld	 a,$01
	ret
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;
;**********************************************************
;	
	ld	 a,(hl)
	inc	 hl
	ld	 (iy+$0d),a
	ld	 a,$01
	ret	
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;		
;		
;		
;		
;		
;**********************************************************
;
	ld	 (iy+$03),$01
	xor	 a
	ret
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;		
;		
;**********************************************************
;
	ld	 (iy+$03),$00
	xor	 a
	ret
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;		
;		
;**********************************************************
;

	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	inc	 hl
	push	 hl
	push	 iy
	pop	 hl
	add	 hl,de
	ld	 (hl),$00
	inc	 hl
	inc	 hl
	res	 0,(hl)
	pop	 hl
	xor	 a
	ret
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;		
;		
;**********************************************************
;
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	inc	 hl
	ld	 a,(hl)
	inc	 hl
	push	 hl
	push	 iy
	pop	 hl
	add	 hl,de
	ld	 (hl),a
	inc	 hl
	inc	 hl
	set	 0,(hl)
	xor	 a
	pop	 hl
	ret	
;
;**********************************************************
; Purpose: ??? 
;
; Input:	
;
; Output:	
;		
;		
;**********************************************************
;
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	inc	 hl
	push	 hl
	push	 iy
	pop	 hl
	add	 hl,de
	ld	 de,L0005
	add	 hl,de
	ld	 a,$06
	pop	 de
L83E3:	ex	 de,hl
	ld	 b,(hl)
	inc	 hl
	ex	 de,hl
	ld	 (hl),b
	dec	 hl
	dec	 a
	jr	 nz,L83E3
	ex	 de,hl
	ret	
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	inc	 hl
	ld	 a,(hl)
	inc	 hl
	push	 hl
	push	 iy
	pop	 hl
	add	 hl,de
	ld	 de,L0006
	add	 hl,de
	ld	 (hl),a
	pop	 hl
	xor	 a
	ret	
	ld	 (iy+$0c),$01
	xor	 a
	ret	
L8407:	sub	 a
	add	 a,e
	sbc	 a,d
	add	 a,e
	rra	
	add	 a,e
	sub	 b
	add	 a,e
	out	 ($83),a
	xor	 $83
	ret	 nz
	add	 a,e
	xor	 (hl)
	add	 a,e
	ld	 bc,LA284
	add	 a,e
	xor	 b
	add	 a,e
	sub	 a
	add	 a,e
	sub	 a
	add	 a,e
	sub	 a
	add	 a,e
	sub	 a
	add	 a,e
	sub	 a
	add	 a,e
	ld	 a,(L6F83)
	add	 a,e
	ld	 a,d
	add	 a,e
	add	 a,l
	add	 a,e
	ld	 h,h
	add	 a,e
	ld	 e,c
	add	 a,e
	ld	 c,(hl)
	add	 a,e
	ld	 b,h
	add	 a,e
L8437:	ld	 a,(iy+$11)
	or	 a
	jr	 z,L846F
	ld	 l,(iy+$01)
	ld	 h,(iy+$02)
L8443:	ld	 a,(hl)
	inc	 hl
	cp	 $18
	jr	 nc,L845E
	exx	
	ld	 hl,L8462
	push	 hl
	ld	 hl,L8407
	rlca	
	ld	 e,a
	ld	 d,$00
	add	 hl,de
	ld	 e,(hl)
	inc	 hl
	ld	 d,(hl)
	push	 de
	exx	
	ret	
	jr	 L8462
L845E:	ld	 hl,L8740
	xor	 a
L8462:	or	 a
	jr	 z,L8443
	ld	 (iy+$01),l
	ld	 (iy+$02),h
	ld	 (iy+$11),$00
L846F:	ret	
L8470:	xor	 $FF
	ld	 b,a
	xor	 a
	ld	 de,L0008
L8477:	rl	 b
	adc	 a,d
	dec	 e
	jr	 nz,L8477
	rla	
	rla	
	rla	
	ret	
L8481:	ld	 bc,L3010
	out	 (c),b
	ld	 c,$50
	out	 (c),b
	in	 a,($10)	;
	set	 3,a		; Set service switch bit ???
	call	 L8470
	ld	 bc,L0C15
	or	 a
	jr	 nz,L849B
	ld	 b,$00
	jr	 L849F
L849B:	xor	 $7F
	add	 a,$32
L849F:	out	 (c),b
	out	 ($13),a
	in	 a,($11)
	and	 $3F
	or	 $C0
	call	 L8470
	ld	 bc,L0C56
	or	 a
	jr	 nz,L84B6
	ld	 b,$00
	jr	 L84B8
L84B6:	add	 a,$4E
L84B8:	out	 (c),b
	out	 ($51),a
	in	 a,($12)
	and	 $3F
	or	 $C0
	call	 L8470
	ld	 bc,L0C16
	or	 a
	jr	 nz,L84CF
	ld	 b,$00
	jr	 L84D1
L84CF:	add	 a,$27
L84D1:	out	 (c),b
	out	 ($11),a
	ld	 a,(LD347)
	in	 a,($13)
	call	 L8470
	ld	 bc,L0C55
	or	 a
	jr	 nz,L84E7
	ld	 b,$00
	jr	 L84EB
L84E7:	xor	 $7F
	sub	 $0F
L84EB:	out	 (c),b
	out	 ($53),a
	jp	 L81F8
L84F2:	ld	 a,(Game_Mode)
	or	 a
	jr	 nz,L8501
	in	 a,($10)	;
	bit	 3,a		; Is service switch on?
	jr	 nz,L8501	; no, skip down
	jp	 L8481		; Otherwise, go here ???
L8501:	ld	 a,(LD244)	
	or	 a
	jr	 z,L851C
	push	 iy
	ld	 iy,LD270
	call	 L80E6
	ld	 iy,LD2AC
	call	 L80E6
	call	 L81F8
	pop	 iy
L851C:	ret	
L851D:	ld	 a,d
	cp	 (iy+$03)
	jr	 c,L8537
	push	 iy
	exx	
	pop	 de
	call	 L8019
	ld	 (iy+$11),$01
	ld	 (iy+$03),d
	ld	 (iy+$01),l
	ld	 (iy+$02),h
L8537:	ret	
L8538:	ld	 hl,LD241
	ld	 a,(hl)
	or	 a
	jr	 z,L8582
	ld	 (hl),$00
	ld	 iy,LD2AC
	rra	
	ld	 d,$01
	ld	 hl,L8928
	jr	 c,L851D
	rra	
	ld	 d,$00
	ld	 hl,L887B
	jr	 c,L851D
	rra	
	ld	 d,$01
	ld	 hl,L87EA
	jr	 c,L851D
	rra	
	ld	 d,$00
	ld	 hl,L883B
	jp	 c,L851D
	rra	
	ld	 hl,L8825
	jp	 c,L851D
	rra	
	rra	
	ld	 hl,L8988
	jp	 c,L851D
	ld	 iy,LD270
	rra	
	ld	 d,$01
	ld	 hl,L8741
	jp	 c,L851D
L8582:	ret	
L8583:	ld	 hl,LD242
	ld	 a,(hl)
	ld	 (hl),$00
	ld	 iy,LD270
	rra	
	ld	 d,$01
	ld	 hl,L8AA1
	jr	 nc,L85A2
	call	 L851D
	ld	 iy,LD2AC
	ld	 hl,L8ADD
	jp	 L851D
L85A2:	ld	 iy,LD2AC
	rra	
	ld	 d,$00
	ld	 hl,L890E
	jp	 c,L851D
	rra	
	ld	 hl,L8851
	jp	 c,L851D
	rra	
	ld	 hl,L8851
	jp	 c,L851D
	rra	
	ld	 hl,L8A42
	jp	 c,L851D
	rra	
	jr	 nc,L85D9
	ld	 d,$01
	ld	 hl,L8A6C
	call	 L851D
	ld	 iy,LD270
	ld	 hl,L8A81
	jp	 L851D
L85D9:	rra	
	ld	 iy,LD270
	ld	 d,$01
	rra	
	ld	 hl,L877B
	jp	 c,L851D
	ret	
L85E8:	ld	 hl,LD243
	ld	 a,(hl)
	ld	 (hl),$00
	ld	 iy,LD270
	rra	
	ld	 d,$02
	ld	 hl,L88E2
	jr	 nc,L8607
	call	 L851D
	ld	 iy,LD2AC
	ld	 hl,L8905
	jp	 L851D
L8607:	rra	
	ld	 d,$01
	ld	 hl,L8AF6
	jr	 nc,L861C
	call	 L851D
	ld	 hl,L8B1F
	ld	 iy,LD2AC
	jp	 L851D
L861C:	ld	 iy,LD2AC
	rra	
	ld	 hl,L8AF3
	jp	 c,L851D
	rra	
	ld	 hl,L8B5D
	jr	 nc,L863A
	call	 L851D
	ld	 hl,L8B2E
	ld	 iy,LD270
	jp	 L851D
L863A:	ret	
	ld	 d,$02
	call	 L851D
	ld	 d,$00
	ret	
L8643:	call	 L8316
	ld	 iy,LD270
	ld	 d,$00
	ld	 hl,L89BE
	call	 L851D
	ld	 iy,LD2AC
	ld	 hl,L89E5
	jp	 L851D
L865C:	call	 L8316
	ld	 d,$00
	ld	 iy,LD270
	ld	 hl,L8A0C
	call	 L851D
	ld	 iy,LD2AC
	ld	 hl,L8A27
	jp	 L851D
L8675:	ld	 iy,LD270
	ld	 d,$00
	ld	 hl,L8971
	jp	 L851D
L8681:	call	 L8316
	ld	 iy,LD270
	ld	 hl,L89A0
	call	 L851D
	ld	 iy,LD2AC
	ld	 hl,L89AF
	jp	 L851D
L8698:	call	 L8316
	ld	 iy,LD270
	ld	 d,$00
	ld	 hl,L8981
	call	 L851D
	ret	
L86A8:	call	 L8316
	ld	 iy,LD2AC
	ld	 hl,L8772
	ld	 d,$00
	call	 L851D
	ld	 iy,LD270
	ld	 hl,L8741
	jp	 L851D
L86C1:	ld	 a,(LD244)
	or	 a
	jr	 z,L8717
	push	 iy
	ld	 hl,LD240
	ld	 a,(hl)
	or	 a
	jr	 z,L86FE
	ld	 d,$00
	ld	 (hl),d
	ld	 e,a
	bit	 0,e
	call	 nz,L8643
	bit	 1,e
	call	 nz,L8681
	bit	 2,e
	call	 nz,L86A8
	bit	 3,e
	call	 nz,L8698
	bit	 4,e
	call	 nz,L865C
	bit	 5,e
	call	 nz,L8675
	bit	 6,e
	call	 nz,L830E
	bit	 7,e
	call	 nz,L8306
	jr	 L8707
L86FE:	call	 L8538
	call	 L8583
	call	 L85E8
L8707:	ld	 iy,LD270
	call	 L8437
	ld	 iy,LD2AC
	call	 L8437
	pop	 iy
L8717:	ret	
	nop	
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
L8740:	inc	 bc
L8741:	inc	 de
	inc	 (hl)
	ld	 (de),a
	xor	 b
	ld	 de,L10FD
	and	 a
	inc	 b
	ld	 sp,hl
	rst	 38H
	or	 c
	and	 a
	dec	 b
	inc	 bc
	ld	 d,l
	ld	 d,l
	inc	 b
	sub	 $FF
	ld	 d,l
	ld	 b,$FF
	ld	 bc,L0117
	dec	 b
	sub	 $FF
	ld	 bc,LFF16
	dec	 d
	rrca	
	inc	 b
	jp	 p,L0FFF
	inc	 bc
	rst	 38H
	inc	 bc
	dec	 b
	ld	 bc,$0508
	jp	 p,L01FF
	nop	
L8772:	inc	 de
	ld	 l,d
	ld	 (de),a
	sub	 (hl)
	ld	 de,L02B2
	ld	 b,a
	add	 a,a
L877B:	djnz	 L878D
	inc	 b
	ld	 sp,hl
	rst	 38H
	and	 b
	djnz	 L8787
	ld	 hl,L0101
	inc	 de
L8787:	add	 hl,hl
	ld	 (de),a
	inc	 (hl)
	ld	 de,L143E
L878D:	add	 a,c
	dec	 b
	ld	 sp,hl
	rst	 38H
	dec	 b
	ld	 d,$DD
	dec	 d
	ld	 c,$00
	inc	 d
	nop	
	djnz	 L87F5
	inc	 de
	ld	 d,h
	ld	 (de),a
	ld	 l,d
	ld	 de,L017E
	ex	 af,af'
	inc	 de
	ld	 c,a
	ld	 (de),a
	ld	 e,(hl)
	ld	 de,L016A
	ex	 af,af'
	inc	 de
	ld	 b,(hl)
	ld	 (de),a
	ld	 d,h
	ld	 de,L015E
	ex	 af,af'
	djnz	 L880A
	inc	 de
	ld	 d,h
	ld	 (de),a
	ld	 l,d
	ld	 de,L017E
	ex	 af,af'
	inc	 de
	ld	 c,a
	ld	 (de),a
	ld	 e,(hl)
	ld	 de,L016A
	ex	 af,af'
	inc	 de
	ld	 b,(hl)
	ld	 (de),a
	ld	 d,h
	ld	 de,L015E
	ex	 af,af'
	djnz	 L881F
	inc	 de
	ld	 d,h
L87D1:	ld	 (de),a
	ld	 l,d
	ld	 de,L017E
	ex	 af,af'
	inc	 de
	ld	 c,a
	ld	 (de),a
	ld	 e,(hl)
	ld	 de,L016A
	ex	 af,af'
	inc	 de
	ld	 b,(hl)
	ld	 (de),a
	ld	 d,h
	ld	 de,L015E
	ex	 af,af'
	ld	 (bc),a
	sbc	 a,c
	add	 a,a
L87EA:	djnz	 L878C
	inc	 b
	ld	 sp,hl
L87EE:	rst	 38H
	and	 b
	djnz	 L87EE
	ld	 hl,L0101
L87F5:	inc	 de
	ld	 a,(de)
	ld	 (de),a
	jr	 nz,L880B
	dec	 h
	inc	 d
	add	 a,c
	dec	 b
	ld	 sp,hl
	rst	 38H
	dec	 b
	ld	 d,$CC
	dec	 d
	inc	 c
	nop	
	inc	 bc
	djnz	 L8829
	dec	 d
L880A:	dec	 de
L880B:	ld	 d,$AA
	nop	
	inc	 bc
	inc	 b
	ld	 sp,hl
	rst	 38H
	jr	 nz,L881A
	inc	 iy
L8816:	inc	 bc
	ld	 bc,LF905
L881A:	rst	 38H
	ld	 bc,L2713
	ld	 (de),a
L881F:	dec	 de
	ld	 de,L0217
	rlca	
	adc	 a,b
L8825:	inc	 b
	ld	 sp,hl
	rst	 38H
	jr	 nz,L882E
	cp	 $23
	ld	 (bc),a
	ld	 bc,LF905
	rst	 38H
	ld	 (bc),a
	inc	 de
	inc	 de
	ld	 (de),a
	dec	 c
	ld	 de,L020B
	rlca	
	adc	 a,b
L883B:	inc	 b
	ld	 sp,hl
	rst	 38H
	jr	 nz,L8842
	cp	 $23
L8842:	ld	 bc,$0501
	ld	 sp,hl
	rst	 38H
	inc	 bc
	inc	 de
	ld	 a,(bc)
	ld	 (de),a
	ex	 af,af'
	ld	 de,L0206
	rlca	
	adc	 a,b
L8851:	inc	 de
	inc	 l
	ld	 (de),a
	inc	 d
	ld	 de,$100F
	djnz	 L885F
	ld	 sp,hl
	rst	 38H
	ld	 (bc),a
	inc	 b
	ld	 sp,hl
L885F:	rst	 38H
	and	 b
	djnz	 L8867
	ld	 hl,L0101
	rla	
L8867:	djnz	 L886D
	.db	  $dd,$ff
	ld	 (hl),b
	djnz	 L8872
	inc	 bc
	ld	 bc,$0501
L8872:	.db	  $dd,$ff
	ld	 bc,L8816
	dec	 d
	jr	 L887A
L887A:	inc	 bc
L887B:	djnz	 L8895
	inc	 b
	ld	 sp,hl
	rst	 38H
	jr	 L8884
	cp	 $21
L8884:	ld	 bc,L1301
	daa	
	ld	 (de),a
	ld	 b,(hl)
	ld	 de,$057E
	ld	 sp,hl
	rst	 38H
	ld	 bc,L7716
	dec	 d
	rla	
	rla	
L8895:	ld	 a,(bc)
	inc	 b
	.db	  $dd,$ff
	ld	 a,(bc)
	ld	 (bc),a
	cp	 $03
	ld	 bc,$0501
	.db	  $dd,$ff
	ld	 bc,L0400
	ld	 sp,hl
	rst	 38H
	add	 a,b
	ld	 (bc),a
	ld	 (bc),a
L88AA:	inc	 hl
	ld	 bc,$0501
	ld	 sp,hl
	rst	 38H
	ld	 bc,L0300
	djnz	 L88BB
	inc	 b
	ld	 sp,hl
	rst	 38H
	ld	 (hl),d
	ld	 b,$05
L88BB:	ld	 hl,L0101
	rla	
	ld	 (LDD04),hl
	rst	 38H
	ld	 h,h
	dec	 b
	inc	 bc
	ld	 hl,L0101
	dec	 b
	.db	  $dd,$ff
	inc	 b
	inc	 de
	ld	 de,L1712
	ld	 de,L161F
	sbc	 a,d
	dec	 d
	ld	 a,(de)
	nop	
	dec	 b
	ld	 sp,hl
	rst	 38H
	ld	 bc,LF906
	rst	 38H
	ld	 bc,L0300
L88E2:	ld	 bc,$1302
	ld	 hl,(L1812)
	ld	 de,$1006
	jr	 nc,L88F1
	ld	 sp,hl
	rst	 38H
	jr	 nc,L8911
L88F1:	call	 m,L0101
	ld	 bc,L0017
	inc	 b
	.db	  $dd,$ff
	jr	 nz,L88FC
L88FC:	ld	 (bc),a
	ld	 bc,$0404
	ld	 d,$FF
	dec	 d
	rra	
	nop	
L8905:	inc	 de
	ld	 h,h
	ld	 (de),a
	ld	 d,b
	ld	 de,L023C
	jp	 pe,L1088
	jr	 nz,L8924
L8911:	inc	 de
	ld	 (de),a
	ld	 (de),a
	ld	 de,L1710
	djnz	 L892F
	ld	 h,a
	dec	 d
	rla	
	inc	 b
	ld	 sp,hl
	rst	 38H
	jr	 L8941
	ret	 m
	ld	 bc,L0202
	ld	 bc,L0328
L8928:	djnz	 L8952
	rla	
	add	 a,h
	inc	 b
	.db	  $dd,$ff
L892F:	add	 a,h
	ld	 a,(bc)
	rst	 38H
	inc	 bc
	ld	 bc,L1601
	call	 z,L0C15
	inc	 d
	add	 a,e
	inc	 de
	ld	 (L1D12),hl
	ld	 de,L0410
	ex	 de,hl
	rst	 38H
	adc	 a,h
	add	 a,e
	ld	 bc,L0823
	ex	 af,af'
	dec	 b
	ex	 de,hl
	rst	 38H
	ld	 bc,$1500
	inc	 e
	ld	 bc,L0320
	inc	 de
	ld	 d,h
	ld	 (de),a
	inc	 (hl)
	ld	 de,L10FD
	jr	 nz,L8961
	ld	 sp,hl
	rst	 38H
	jr	 c,L8969
L8961:	rst	 38H
	ld	 bc,L0101
	dec	 b
	jp	 p,L01FF
L8969:	ld	 bc,L0960
	ld	 b,$F9
	rst	 38H
	ld	 bc,$1600
	rst	 38H
	dec	 d
	rra	
	inc	 b
	jp	 p,L0FFF
	ld	 (bc),a
	rst	 38H
	inc	 bc
	add	 hl,de
	add	 hl,de
	ld	 (bc),a
	ld	 d,h
	adc	 a,c
L8981:	ld	 d,$22
	dec	 d
	ld	 (de),a
	ld	 (bc),a
	ld	 d,h
	adc	 a,c
L8988:	djnz	 L89AA
	inc	 b
	ld	 sp,hl
	rst	 38H
	jr	 nz,L899B
	rst	 38H
	inc	 bc
	ld	 (bc),a
	ld	 bc,L5516
	dec	 d
	ld	 b,$13
	ld	 d,h
	ld	 (de),a
	ld	 l,d
L899B:	ld	 de,L01FD
	jr	 z,L89A3
L89A0:	djnz	 L89D2
	inc	 d
L89A3:	add	 a,c
	ld	 d,$FC
	dec	 d
	ld	 c,$13
	ld	 b,(hl)
L89AA:	ld	 (de),a
	inc	 l
	ld	 de,L00FD
L89AF:	djnz	 L89E1
	inc	 d
	add	 a,c
	ld	 d,$FD
	dec	 d
	ld	 c,$13
	ld	 a,(hl)
	ld	 (de),a
	ld	 e,c
	ld	 de,L006A
L89BE:	djnz	 L89F0
	ld	 d,$FE
	dec	 d
	rrca	
	inc	 d
	add	 a,c
	inc	 de
	ld	 a,$12
	xor	 b
	ld	 de,L016A
	ld	 b,d
	inc	 de
	scf	
	ld	 (de),a
	ld	 (hl),b
L89D2:	ld	 de,L015E
	ld	 d,$13
	dec	 (hl)
	ld	 (de),a
	ld	 l,d
	ld	 de,L0154
	inc	 (hl)
	inc	 de
	ld	 a,$12
L89E1:	xor	 b
	ld	 de,L006A
L89E5:	djnz	 L8A17
	inc	 d
	add	 a,c
	ld	 d,$EE
	dec	 d
	rrca	
	inc	 de
	ld	 d,h
	ld	 (de),a
L89F0:	ld	 l,d
	ld	 de,L01FD
	ld	 b,d
	inc	 de
	ld	 e,(hl)
	ld	 (de),a
	ld	 d,h
	ld	 de,L01E1
	ld	 d,$13
	ld	 b,(hl)
	ld	 (de),a
	ld	 a,$11
	call	 nc,L3401
	inc	 de
	ld	 d,h
	ld	 (de),a
	ld	 a,(hl)
	ld	 de,L00FD
L8A0C:	djnz	 L8A3E
	ld	 d,$EF
	dec	 d
	rrca	
	inc	 d
	add	 a,c
	inc	 de
	ld	 (hl),b
	ld	 (de),a
L8A17:	ld	 a,$11
	xor	 b
	ld	 bc,L1360
	ld	 (hl),b
	ld	 (de),a
	ld	 b,d
	ld	 de,L01A8
	ld	 e,b
	ld	 (bc),a
	cp	 (hl)
	adc	 a,c
L8A27:	djnz	 L8A59
	inc	 d
	add	 a,c
	ld	 d,$EF
	dec	 d
	rrca	
	inc	 de
	ld	 c,a
	ld	 (de),a
	inc	 (hl)
	ld	 de,L015E
	ld	 h,b
	inc	 de
	ld	 d,h
	ld	 (de),a
	scf	
	ld	 de,L015E
L8A3E:	ld	 e,b
	ld	 (bc),a
	push	 hl
	adc	 a,c
L8A42:	djnz	 L8A58
	inc	 d
	ld	 c,b
	inc	 b
	ld	 sp,hl
	rst	 38H
	inc	 d
	ex	 af,af'
	rst	 38H
	inc	 hl
	ld	 (bc),a
	inc	 bc
	dec	 d
	jr	 z,L8A69
	jr	 nz,L8A58
	.db	  $dd,$ff
	ld	 d,h
	jr	 nz,L8A5D
L8A59:	inc	 bc
	ld	 (bc),a
	inc	 bc
	dec	 b
L8A5D:	ld	 sp,hl
	rst	 38H
	ld	 bc,L8816
	inc	 de
	.db	  $fd,$12
	cp	 $11
	rst	 38H
	nop	
L8A69:	ld	 bc,L0306
L8A6C:	inc	 de
	ret	 po
	ld	 (de),a
	ret	 z
	ld	 de,$10B6
	inc	 d
	inc	 d
	ld	 c,b
	ld	 d,$88
	dec	 d
	jr	 z,L8A92
	jr	 nz,L8A7E
	jr	 nz,L8A81
	ld	 b,d
	adc	 a,d
L8A81:	djnz	 L8A8C
	inc	 d
	ld	 c,b
	inc	 de
	ret	 po
	ld	 (de),a
	ret	 z
	ld	 de,L16B6
L8A8C:	cp	 e
	dec	 d
	dec	 c
	ld	 bc,L0420
L8A92:	ld	 sp,hl
	rst	 38H
	ld	 hl,L0209
	ld	 hl,L0302
	dec	 b
	ld	 sp,hl
	rst	 38H
	ld	 bc,L6202
	adc	 a,d
L8AA1:	inc	 b
	.db	  $dd,$ff
	add	 a,b
	nop	
	cp	 $21
	ld	 bc,L1701
	add	 a,b
	djnz	 L8AEE
	inc	 de
	ld	 l,b
	ld	 (de),a
	ld	 b,h
	ld	 de,$0521
	.db	  $dd,$ff
	ld	 bc,L1F15
	ld	 d,$EE
	nop	
	dec	 d
	cpl	
	dec	 b
	ld	 sp,hl
	rst	 38H
	ld	 (bc),a
	ld	 b,$DD
	rst	 38H
	ld	 bc,LF904
	rst	 38H
	add	 a,b
	ld	 (bc),a
	rst	 38H
	ld	 hl,L0101
	inc	 d
	add	 a,b
	inc	 b
	ex	 de,hl
	rst	 38H
	cp	 a
	add	 a,b
	ld	 bc,L0201
	ld	 (bc),a
	nop	
	inc	 bc
L8ADD:	inc	 de
	inc	 sp
	ld	 (de),a
	jr	 nc,L8AF3
	ld	 (bc),a
	ld	 bc,$0404
	.db	  $dd,$ff
	add	 a,b
	nop	
	ld	 (bc),a
	ld	 hl,L0101
L8AEE:	rla	
	nop	
	ld	 (bc),a
	xor	 h
	adc	 a,d
L8AF3:	ld	 (bc),a
	or	 e
	adc	 a,b
L8AF6:	djnz	 L8B0C
	inc	 d
	adc	 a,b
	inc	 de
	scf	
	ld	 (de),a
	add	 a,l
	ld	 de,L178D
	nop	
	ld	 d,$AA
	dec	 d
	ld	 hl,(L2801)
	inc	 d
	nop	
	inc	 b
	ld	 sp,hl
L8B0C:	rst	 38H
	ld	 e,h
	inc	 d
	ld	 b,$01
	inc	 b
	inc	 b
	rla	
	jr	 z,L8B1A
	sub	 $FF
	inc	 b
	ld	 bc,L01FF
	jr	 nc,L8B4E
	nop	
L8B1F:	djnz	 L8B31
	inc	 d
	add	 a,(hl)
	inc	 de
	add	 hl,hl
	ld	 (de),a
	dec	 (hl)
	ld	 de,L167E
	sbc	 a,c
	dec	 d
	add	 hl,hl
	nop	
L8B2E:	inc	 de
	xor	 b
	ld	 (de),a
L8B31:	ld	 a,(de)
	ld	 de,L167E
	.db	  $dd,$15
	dec	 l
	djnz	 L8B58
	rla	
	jr	 L8B51
	ld	 bc,LEB05
	rst	 38H
	dec	 b
	inc	 b
	ex	 de,hl
	rst	 38H
	inc	 e
	ld	 bc,L2303
	ld	 (bc),a
	ld	 (bc),a
	nop	
	dec	 b
	ex	 de,hl
L8B4E:	rst	 38H
	ld	 bc,L1C14
	inc	 b
	ex	 de,hl
	rst	 38H
	inc	 e
	ld	 bc,L23FD
	rlca	
	rlca	
	nop	
	inc	 bc
L8B5D:	inc	 de
	ld	 d,h
	ld	 (de),a
	inc	 (hl)
	ld	 de,L02FD
	inc	 (hl)
	adc	 a,e
	dec	 e
	add	 a,e
	add	 hl,de
	daa	
	add	 hl,bc
	jr	 L8B70
	dec	 l
	ld	 h,$35
L8B70:	dec	 hl
	jr	 L8BA6
	add	 hl,de
	inc	 bc
	dec	 e
	dec	 (hl)
	dec	 hl
	ld	 e,$33
	ld	 c,$23
	jr	 L8B9D
	add	 hl,de
	ld	 h,$35
	dec	 hl
	ld	 a,$83
	ld	 sp,L1D27
	ld	 (L3609),hl
	jr	 z,L8BA8
	dec	 sp
	nop	
	ld	 hl,(L282A)
	scf	
	dec	 h
	dec	 d
	dec	 l
	ld	 a,(L321D)
	jr	 L8BD8
	add	 a,e
	dec	 d
	nop	
L8B9D:	add	 hl,bc
	add	 hl,hl
	jr	 L8BCB
	jr	 nz,L8BBC
	add	 hl,de
	dec	 sp
	dec	 hl
L8BA6:	ld	 (L220F),a
	ld	 (hl),$28
	inc	 bc
	inc	 c
	dec	 d
	add	 hl,bc
	add	 hl,hl
	rra	
	dec	 sp
	jr	 L8BD1
	ld	 a,$83
	ld	 a,(bc)
	add	 a,e
	ld	 (L2836),hl
	inc	 sp
L8BBC:	dec	 hl
	daa	
	dec	 c
	ld	 a,$83
	inc	 d
	add	 a,e
	jr	 c,L8BF8
	inc	 bc
	ld	 e,$33
	dec	 c
	ld	 a,(de)
	ld	 (bc),a
L8BCB:	dec	 c
	ld	 (de),a
	inc	 sp
	rrca	
	dec	 l
	ld	 h,$35
	dec	 hl
	dec	 hl
	ld	 a,$BE
	ex	 af,af'
	dec	 d
L8BD8:	inc	 hl
	add	 hl,bc
	add	 hl,hl
	cpl	
	nop	
	inc	 c
	ld	 a,$11
	ld	 a,$38
	ld	 (hl),e
	dec	 l
	ld	 h,a
	ld	 (de),a
	ld	 a,(L731E)
	ld	 c,a
	inc	 bc
	ld	 l,l
	ld	 h,$35
	dec	 hl
	dec	 hl
	ld	 a,$2B
	dec	 l
	inc	 sp
	dec	 c
	ld	 c,$23
	ex	 af,af'
L8BF8:	add	 hl,hl
	ld	 hl,(L2B1D)
	inc	 sp
	inc	 c
	inc	 c
	dec	 d
	add	 hl,bc
	ld	 (L2B25),hl
	daa	
	ld	 hl,(L2229)
	rra	
	ld	 a,$3E
	cpl	
	nop	
	dec	 c
	ld	 e,$22
	ld	 (hl),$28
	jr	 L8C4F
	add	 hl,de
	inc	 bc
	rra	
	dec	 h
	jr	 L8C40
	scf	
	ld	 e,$3E
	inc	 e
	inc	 c
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	add	 hl,de
	dec	 hl
	inc	 a
	ld	 hl,(L3A10)
	rra	
	inc	 h
	inc	 hl
	dec	 hl
	dec	 hl
	ld	 b,$1E
	add	 hl,hl
	dec	 (hl)
	scf	
	cpl	
	nop	
	add	 hl,de
	ld	 hl,(L0F0B)
	ld	 a,$1E
	dec	 l
	ld	 h,$2B
	jr	 L8C73
L8C40:	add	 hl,de
	inc	 bc
	dec	 l
	daa	
	jr	 L8C49
	dec	 sp
	rra	
	inc	 bc
L8C49:	add	 hl,de
	ld	 b,$09
	ld	 (L0325),hl
L8C4F:	jr	 c,L8C7C
	jr	 z,L8C8B
	inc	 sp
	ld	 e,$26
	dec	 (hl)
	dec	 hl
	ld	 a,$1E
	ld	 (L2836),hl
	dec	 l
	dec	 (hl)
	dec	 c
	ld	 hl,(L6F1B)
	ld	 a,(bc)
	rrca	
	inc	 bc
	ld	 b,$2A
	djnz	 L8CD8
	dec	 c
	rra	
	inc	 bc
	dec	 e
	inc	 (hl)
	dec	 hl
	add	 hl,hl
	dec	 (hl)
	dec	 hl
L8C73:	ld	 e,$6E
	dec	 c
	rra	
	ld	 a,$1F
	add	 a,e
	dec	 hl
	inc	 a
L8C7C:	inc	 c
	ld	 a,e
	inc	 c
	ld	 c,$3A
	ld	 a,$55
	nop	
	add	 hl,bc
	add	 hl,hl
	inc	 c
	jr	 c,L8CBC
	dec	 l
	daa	
L8C8B:	ld	 (de),a
	ld	 a,(L3E1E)
	dec	 c
	dec	 d
	ld	 hl,(L3629)
	scf	
	scf	
	ld	 a,$83
	inc	 l
	ld	 c,e
	ld	 e,l
	add	 hl,hl
	add	 hl,bc
	scf	
	add	 hl,de
	cpl	
	nop	
	dec	 c
	ld	 hl,(L0E3E)
	ld	 a,h
	ld	 hl,(L3238)
	dec	 hl
	dec	 sp
	rra	
	ld	 hl,(LB83E)
	ld	 b,d
	dec	 c
	add	 hl,hl
	ld	 (hl),$37
	jr	 L8CC3
	ld	 (bc),a
	rrca	
	ld	 a,(L1C3E)
	ld	 b,d
L8CBC:	ld	 hl,(L3238)
	ld	 c,$3B
	rra	
	ld	 hl,(L8303)
	ld	 sp,L2783
	dec	 e
	add	 hl,hl
	add	 hl,bc
	jr	 z,L8CEB
	inc	 a
	rra	
	ld	 hl,(L352B)
	inc	 hl
	add	 hl,bc
	ld	 hl,L080C
	ex	 af,af'
L8CD8:	nop	
	add	 hl,bc
	add	 hl,hl
	ld	 c,$60
	ld	 c,$29
	ld	 (L3E1F),hl
	dec	 d
	add	 hl,bc
	ld	 (L2518),hl
	ld	 d,l
	dec	 h
	add	 hl,hl
	ld	 (hl),$28
	dec	 bc
	dec	 c
	jr	 c,L8D11
	ld	 hl,L0F33
	dec	 sp
	dec	 c
	ld	 a,$83
	dec	 d
	add	 a,e
	dec	 c
	ld	 d,l
	inc	 hl
	scf	
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	inc	 c
	inc	 e
	dec	 sp
	ld	 hl,($1427)
	inc	 c
	ld	 l,$00
	ld	 e,$3E
	add	 a,e
	dec	 de
	add	 a,e
	ld	 (L2836),hl
	dec	 l
	daa	
	jr	 L8D23
	ld	 a,e
	rrca	
	ld	 a,(L3C18)
	inc	 a
	rrca	
	dec	 l
	ld	 h,(hl)
	ld	 (hl),l
	dec	 hl
	ld	 (L5518),a
	nop	
	add	 hl,hl
	rrca	
	ld	 a,$83
	dec	 d
	add	 a,e
	inc	 e
	ld	 d,l
	dec	 hl
	dec	 l
	dec	 (hl)
	dec	 hl
	ld	 a,$1C
	dec	 (hl)
	dec	 (hl)
	ld	 l,a
	nop	
	dec	 e
	ld	 hl,(L383A)
	dec	 sp
	inc	 c
	ld	 a,$83
	ld	 c,$83
	dec	 l
	dec	 d
	ld	 hl,(L3810)
	inc	 sp
	dec	 hl
	jr	 nz,L8D68
	dec	 d
	dec	 hl
	inc	 bc
	add	 a,e
	ld	 b,$2D
	ld	 h,$2B
	add	 hl,hl
	ld	 a,(L1D3E)
	add	 a,e
	dec	 c
	dec	 d
	ld	 h,e
	ld	 (hl),a
	ld	 (L3736),hl
	scf	
	inc	 e
	dec	 sp
	ld	 hl,(L3903)
	ld	 (L5B03),a
	ld	 b,d
L8D68:	ld	 c,c
	rrca	
	inc	 a
	dec	 l
	ld	 b,l
	add	 hl,bc
	ld	 (L1F2A),hl
	ld	 a,$83
	ld	 d,$83
	add	 hl,hl
	inc	 (hl)
	inc	 (hl)
	dec	 hl
	ld	 l,a
	nop	
	ld	 e,a
	ld	 e,c
	daa	
	inc	 d
	dec	 e
	ld	 h,$2B
	ld	 hl,(L732B)
	ld	 c,(hl)
	inc	 hl
	jr	 L8DC7
	add	 a,e
	ld	 hl,(L1D27)
	add	 hl,hl
	ld	 (hl),$28
	ld	 hl,($152B)
	nop	
	add	 hl,bc
	add	 hl,hl
	ld	 l,a
	dec	 c
	add	 hl,hl
	dec	 de
	ld	 d,l
	ld	 l,e
	ld	 e,$3A
	ld	 a,$83
	ld	 (L2836),hl
	jr	 L8E1A
	dec	 c
	jr	 L8DD1
	inc	 c
	inc	 l
	inc	 a
	ld	 hl,(L272D)
	add	 hl,sp
	ld	 e,(hl)
	jr	 z,L8DDA
	inc	 c
	ld	 a,$83
	daa	
	add	 a,e
	ld	 c,$7A
	ld	 l,e
	dec	 l
	ld	 h,$2B
	ld	 a,$1C
	ld	 d,l
	ld	 l,e
	dec	 l
	ld	 h,$2B
	ld	 a,$2F
	nop	
L8DC7:	dec	 c
	ld	 e,$39
	ld	 h,(hl)
	ld	 l,e
	dec	 l
	ld	 h,$2B
	ld	 a,$2D
L8DD1:	daa	
	jr	 L8DF2
	ld	 (hl),$68
	ld	 (L2836),hl
	daa	
L8DDA:	dec	 c
	ld	 a,$83
	ld	 (L0C83),hl
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	dec	 l
	ld	 h,(hl)
	ld	 l,e
	jr	 L8E10
	inc	 d
	rra	
	dec	 d
	inc	 hl
	dec	 hl
	rrca	
	ld	 a,e
	ld	 b,b
	dec	 hl
L8DF2:	add	 hl,hl
	rrca	
	ld	 a,e
	ld	 b,b
	dec	 hl
	add	 hl,hl
	dec	 de
	ld	 (hl),e
	dec	 c
	inc	 e
	dec	 hl
	add	 hl,hl
	ld	 a,$83
	inc	 hl
	inc	 c
	dec	 d
	ld	 b,b
	ld	 c,c
	ld	 l,c
	inc	 c
	ld	 l,a
	ld	 e,$1A
	dec	 bc
	add	 hl,de
	daa	
	ld	 (de),a
	rra	
	ld	 hl,(L7D2B)
	dec	 c
	inc	 e
	dec	 hl
	jr	 c,L8E46
	nop	
	dec	 c
	add	 hl,hl
L8E1A:	inc	 (hl)
	inc	 (hl)
	dec	 hl
	dec	 l
	ld	 a,e
	ld	 h,l
	ld	 (L1F0D),a
	ld	 a,$26
	add	 a,e
	add	 hl,hl
	inc	 (hl)
	inc	 (hl)
	dec	 hl
	ld	 c,(hl)
	ld	 h,$34
	ld	 c,l
	rra	
	dec	 l
	daa	
	jr	 L8E36
	jr	 L8E4A
	nop	
L8E36:	add	 hl,hl
	daa	
	dec	 c
	jr	 c,L8E6E
	ld	 e,$73
	dec	 c
	ld	 a,(de)
	ld	 (bc),a
	dec	 c
	rra	
	inc	 sp
	rrca	
	dec	 l
	ld	 h,$35
	dec	 hl
	dec	 hl
	ld	 a,$83
	dec	 hl
	dec	 l
	dec	 d
	nop	
	add	 hl,bc
	jr	 L8E74
	halt	
	ld	 l,b
	ld	 e,$3C
	rrca	
	ld	 (bc),a
	jr	 L8E7D
	dec	 h
	ld	 hl,(L151F)
	add	 hl,bc
	ld	 hl,L0D3B
	rra	
	ld	 a,$2D
	inc	 a
	add	 hl,hl
	ld	 e,$3C
	rrca	
	ld	 (bc),a
	jr	 L8E90
	dec	 h
L8E6E:	ld	 hl,(L2F0C)
	nop	
	ld	 e,$1A
L8E74:	dec	 bc
	add	 hl,de
	ld	 a,$13

;Speech string "Hey, Insert coin!" for $13 ($19) bytes 

L8E78:	.db	$1B,$60,$4B,$62,$3E,$3E,$27,$0D,$1F,$7A,$6A,$3E,$59,$75,$34,$09,$22,$0D,$3E,$0A
;	.db	

	dec	 e
	ld	 d,l
	ld	 c,c
	ld	 l,c
L8E90:	dec	 c
	ld	 e,$0C
	inc	 l
	inc	 a
	ld	 a,$12
	dec	 d
	ld	 c,c
	ld	 l,c
	inc	 c
	inc	 bc
	ex	 af,af'
	dec	 (hl)
	scf	
	ld	 e,$15
	inc	 bc
	rra	
	dec	 h
	ex	 af,af'
	ld	 c,e
	ld	 l,c
	ld	 hl,(L083E)
	inc	 e
	dec	 sp
	ld	 hl,(L3B2B)
	ld	 e,$29
	ld	 a,$20
	ld	 (L2836),hl
	ld	 e,$03
	ld	 c,$42
	ld	 hl,(L033A)
	dec	 de
	ld	 h,$25
	ld	 (L6876),hl
	ld	 e,$26
	dec	 c
	ld	 hl,(L555D)
	add	 hl,bc
	ld	 (L1E0D),hl
	ld	 c,h
	inc	 l
	inc	 a
	ld	 a,$3E
	ld	 a,$1E
	dec	 d
	dec	 c
	inc	 sp
	add	 hl,sp
	ld	 a,(L1903)
	dec	 (hl)
	inc	 (hl)
	add	 hl,bc
	ld	 (L1D0D),hl
	ld	 h,$2B
	inc	 c
	ld	 d,l
	ld	 c,c
	ld	 h,d
	ld	 hl,(L022B)
	rlca	
	ld	 a,($102A)
	dec	 sp
	rra	
	ld	 hl,(L0A3E)
	ld	 a,$1B
	ld	 d,l
	dec	 de
	ld	 d,l
	dec	 de
	dec	 d
	dec	 de
	dec	 d
	ld	 a,$22
	ld	 h,h
	ex	 af,af'
	inc	 bc
	ld	 e,h
	halt	
	halt	
	ld	 (hl),$36
	ld	 e,$3E
	inc	 c
	dec	 d
	add	 hl,bc
	add	 hl,hl
	dec	 h
	ld	 a,e
	ld	 hl,(L2D1F)
	ld	 a,(L1C2B)
	dec	 sp
	ld	 hl,($1427)
	dec	 de
	ld	 (hl),e
	ld	 d,h
	inc	 e
	dec	 hl
	add	 hl,hl
	ld	 a,$3E
	inc	 d
	add	 a,e
	ld	 (L2836),hl
	jr	 L8F43
	dec	 sp
	ld	 hl,(L3E3E)
	jr	 c,L8F59
	inc	 bc
	ld	 c,b
	dec	 hl
	inc	 l
	dec	 c
	dec	 d
	ld	 a,$BE
	dec	 bc
	cp	 (hl)
	dec	 de
	dec	 d
	dec	 de
	dec	 d
	dec	 de
	dec	 d
	dec	 de
	dec	 d
	ld	 a,$83
	inc	 hl
	dec	 d
L8F43:	dec	 c
	ld	 (L3A38),a
	inc	 bc
	dec	 l
	ld	 h,$2B
	add	 hl,hl
	ld	 a,(L351D)
	dec	 hl
	inc	 c
	dec	 d
	add	 hl,bc
	ld	 (L200E),hl
	add	 hl,hl
	ld	 c,$22
L8F59:	add	 hl,hl
	rra	
	ld	 hl,(L1E28)
	inc	 l
	rrca	
	dec	 d
	inc	 (hl)
	scf	
	dec	 hl
	ld	 a,$1D
	add	 hl,de
	ld	 l,h
	dec	 h
	inc	 bc
	inc	 e
	ld	 h,$0B
	ld	 (L0314),hl
	ld	 l,$0D
	ld	 e,$29
	ld	 (hl),$28
	dec	 l
	daa	
	jr	 L8F97
	ld	 d,l
	dec	 bc
	ld	 (L1E0D),hl
	inc	 c
	inc	 l
	inc	 a
	ld	 a,$1D
	dec	 d
	dec	 e
	inc	 a
	jr	 z,L8FB1
	inc	 c
	ld	 h,$2B
	ld	 e,$33
	dec	 c
	ld	 a,(de)
	ld	 (bc),a
	dec	 c
	rra	
	ld	 a,$15
	dec	 c
	ld	 e,$29
	ld	 (hl),$68
	ld	 e,b
	ld	 c,$2C
	inc	 a
	inc	 bc
	jr	 nz,L8FA7
	ex	 af,af'
	dec	 l
	ld	 h,(hl)
	ld	 l,e
	jr	 L8FCD
L8FA7:	dec	 hl
	ld	 e,$3E
	ld	 a,(bc)
	add	 a,e
	dec	 l
	ld	 h,(hl)
	ld	 l,e
	jr	 L8FD7
L8FB1:	dec	 hl
	ld	 e,$3E
	add	 a,e
	ld	 (de),a
	add	 hl,de
	dec	 d
	inc	 c
	ld	 c,$2E
	add	 hl,de
	dec	 e
	ld	 h,$2B
	inc	 c
	ld	 h,$35
	dec	 hl
	ld	 a,$3E
	dec	 l
	daa	
	add	 hl,sp
	daa	
	add	 a,e
	jr	 c,L8FFF
	ld	 e,$73
	ld	 c,l
	ld	 a,(de)
	dec	 sp
	dec	 c
	rra	
	inc	 bc
	inc	 sp
	rrca	
	inc	 bc
L8FD7:	dec	 l
	ld	 h,$35
	dec	 hl
	dec	 hl
	dec	 d
	dec	 l
	ld	 b,(hl)
	ld	 h,c
	add	 hl,hl
	ld	 hl,(L2903)
	ld	 h,$35
	dec	 hl
	dec	 hl
	add	 hl,bc
	inc	 a
	ld	 hl,(L2B7A)
	dec	 c
	ld	 a,$83
	dec	 hl
	add	 a,e
	ld	 e,$6C
	inc	 a
	dec	 h
	daa	
	dec	 c
	jr	 c,L902D
	inc	 bc
	add	 hl,de
	ld	 l,$0F
	ld	 a,(L1F0D)
	inc	 bc
	inc	 sp
	rrca	
	dec	 l
	ld	 h,(hl)
	ld	 (hl),l
	dec	 hl
	dec	 hl
	ld	 a,$3E
	ld	 (L2836),hl
	scf	
	dec	 l
	daa	
	jr	 L901F
	ld	 l,h
	ld	 a,h
	ld	 hl,(L0C03)
	inc	 l
	inc	 a
	inc	 bc
	add	 a,e
	ld	 c,$3E
	add	 hl,sp
L901F:	add	 hl,sp
	cpl	
	nop	
	inc	 d
	add	 hl,de
	rra	
	inc	 bc
	add	 hl,hl
	ld	 (hl),$28
	scf	
	ld	 a,$18
	add	 a,e
L902D:	ld	 (L2836),hl
	dec	 c
	ld	 (hl),l
	ld	 (hl),l
	dec	 (hl)
	ld	 (L2836),hl
	add	 hl,de
	cpl	
	nop	
	dec	 c
	ld	 e,$36
	jr	 z,L904D
	ld	 a,e
	ld	 hl,(L3E3A)
	add	 a,e
	ld	 h,$1B
	ld	 a,d
	ld	 l,e
	add	 hl,hl
	ld	 c,$2F
	nop	
	add	 hl,de
L904D:	ld	 a,$3E
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	add	 hl,de
	cpl	
	nop	
	dec	 c
	ld	 hl,(L462D)
	ld	 h,c
	add	 hl,hl
	ld	 hl,(L362A)
	scf	
	ld	 e,$76
	jr	 z,L908B
	ld	 hl,(L1C32)
	ld	 b,l
	ld	 b,d
	dec	 c
	ld	 a,$27
	ld	 (L2836),hl
	add	 hl,de
	cpl	
	nop	
	dec	 c
	rra	
	ld	 hl,(L2B55)
	ld	 hl,(L0D15)
	halt	
	scf	
	dec	 l
	ld	 a,$BE
	ld	 c,$33
	ld	 hl,(L351D)
	dec	 hl
	dec	 c
	dec	 d
	ld	 h,e
	ld	 (hl),a
	add	 hl,hl
	inc	 (hl)
L908B:	inc	 (hl)
	dec	 hl
	add	 hl,sp
	dec	 hl
	ld	 (hl),a
	scf	
	ld	 a,$BE
	ld	 (L6C1B),hl
	dec	 de
	ld	 l,h
	dec	 de
	ld	 l,h
	dec	 de
	ld	 h,$1B
	ld	 h,$1B
	ld	 h,$1B
	dec	 d
	dec	 de
	dec	 d
L90A4:	dec	 de
	dec	 d
	dec	 de
	dec	 d
	ld	 a,$38
	ld	 l,(hl)
	nop	
	ld	 hl,(L0303)
	dec	 l
	inc	 sp
	ld	 (de),a
	dec	 e
	inc	 sp
	dec	 c
	ld	 a,$19
	dec	 l
	ld	 a,e
	jr	 L90D4
	inc	 sp
	inc	 c
	ld	 a,$2A
	scf	
	inc	 c
	dec	 d
	dec	 bc
	add	 hl,hl
	dec	 l
	dec	 (hl)
	ld	 a,d
	ld	 e,b
	ld	 e,$33
	rrca	
	dec	 l
	ld	 h,$35
	dec	 hl
	ld	 a,$21
	rra	
	ld	 h,(hl)
	add	 hl,hl
L90D4:	ld	 (hl),$37
	rrca	
	add	 hl,de
	ld	 (hl),e
	ld	 c,h
	ld	 hl,(L1F37)
	add	 hl,de
	ld	 h,$35
	dec	 hl
	ld	 a,$0B
	dec	 c
	jr	 c,L9118
	dec	 l
	dec	 (hl)
	ld	 a,d
	jr	 L9109
	inc	 sp
	rrca	
	dec	 l
	ld	 h,(hl)
	dec	 (hl)
	dec	 hl
	ld	 a,$2C
	add	 hl,hl
	inc	 (hl)
	inc	 (hl)
	dec	 hl
	dec	 a
	dec	 e
	ld	 hl,(L3736)
	sbc	 a,a
	inc	 a
	add	 hl,hl
	jr	 c,L9134
	xor	 l
	daa	
	ld	 (de),a
	ld	 a,(L3E1E)
	jr	 c,L913C
L9109:	inc	 c
	cpl	
	nop	
	ld	 e,$1A
	dec	 bc
	add	 hl,de
	ld	 (LAD18),a
	daa	
	ld	 (de),a
	ld	 a,(LB31E)
L9118:	rrca	
	dec	 l
	dec	 (hl)
	inc	 (hl)
	dec	 hl
	ld	 a,$3E
	jr	 nz,L90A4
	ld	 c,$3A
	dec	 hl
	dec	 l
	ld	 h,$2B
	dec	 de
	ld	 l,$1F
	dec	 c
	ld	 hl,(L2A2C)
	ld	 (bc),a
	dec	 c
	dec	 sp
	dec	 c
	add	 hl,hl
	dec	 l
L9134:	inc	 sp
	dec	 c
	inc	 bc
	dec	 bc
	dec	 c
	inc	 c
	inc	 sp
	dec	 c
L913C:	add	 hl,sp
	rra	
	ld	 a,$83
	ld	 d,$0C
	dec	 d
	add	 hl,bc
	add	 hl,hl
	ld	 c,$60
	ld	 c,$29
	ld	 (L031F),hl
	ld	 c,$2B
	inc	 a
	add	 hl,hl
	add	 hl,sp
	dec	 e
	ld	 d,l
	nop	
	ld	 hl,L3E2B
	ld	 h,$83
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	jr	 L917C
	dec	 hl
	dec	 d
	ld	 b,b
	ld	 b,b
	ld	 l,c
	ld	 (L3709),hl
	dec	 l
	dec	 bc
	add	 hl,sp
	inc	 c
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	jr	 L9194
	ld	 c,b
	ld	 l,c
	ld	 hl,(L270D)
	inc	 d
	ld	 c,$26
	jr	 L91A5
	rra	
L917C:	ld	 a,$83
	ld	 ($151C),hl
	dec	 hl
	dec	 l
	ld	 h,$2B
	cpl	
	nop	
	dec	 c
	ld	 e,$39
	ld	 h,$2B
	dec	 l
	ld	 h,$2B
	ld	 a,$0E
	add	 hl,hl
	add	 hl,de
	inc	 sp
L9194:	inc	 c
	ld	 a,$BE
	daa	
	dec	 c
	rrca	
	ld	 c,e
	ld	 d,d
	dec	 bc
	ld	 c,$18
	ld	 a,$83
	inc	 l
	add	 a,e
	add	 hl,sp
	ld	 h,$2B
	dec	 l
	ld	 h,$2B
	ld	 a,$27
	ld	 (de),a
	dec	 hl
	dec	 sp
	ld	 e,$3E
	inc	 c
	inc	 a
	ld	 hl,L3E0D
	cpl	
	nop	
	dec	 c
	ld	 e,$1B
	ld	 (hl),e
	ld	 d,h
	inc	 e
	dec	 hl
	add	 hl,hl
	dec	 e
	ld	 h,$2B
	rra	
	dec	 h
	ld	 b,$09
	add	 hl,hl
	rra	
	dec	 e
	scf	
	scf	
	ld	 e,$3E
	add	 a,e
	jr	 z,L91FD
	ld	 h,$2B
	add	 hl,hl
	ld	 a,(L211D)
	ld	 a,(bc)
	dec	 hl
	ld	 a,$15
	nop	
	add	 hl,bc
	add	 hl,hl
	ld	 e,$2B
	dec	 a
	dec	 c
	ld	 hl,L2B0A
	ld	 a,$3C
	ld	 hl,(L2A10)
	dec	 d
	add	 hl,bc
	ld	 ($150C),hl
	nop	
	add	 hl,bc
	add	 hl,hl
	ld	 (L6125),a
	ld	 c,c
	dec	 hl
	ld	 a,$07
	xor	 l
	ld	 h,$2B
	add	 hl,hl
	ld	 a,(L833E)
	rla	
	add	 hl,hl
	ld	 (hl),$37
	rrca	
	inc	 bc
	ld	 a,(de)
	inc	 sp
	rra	
	ld	 hl,(L3B0E)
	dec	 c
	dec	 e
	dec	 hl
	dec	 d
	dec	 bc
	ld	 (L3E1E),hl
	ld	 c,$15
	ld	 a,(bc)
	ld	 (L830F),hl
	ld	 c,$23
	dec	 d
	add	 hl,hl
	ld	 hl,(L3338)
	ld	 c,$35
	dec	 (hl)
	jr	 L924F
	ld	 a,$83
	dec	 e
	dec	 l
	inc	 sp
	rra	
	dec	 c
	ld	 hl,(L2F38)
	ld	 hl,(L2318)
	ld	 c,b
	ld	 l,c
	ld	 hl,(L270D)
	inc	 d
	ld	 c,$26
	jr	 L9266
	inc	 bc
	ld	 e,$2C
	jr	 L924C
	ld	 de,L1F32
	ld	 a,$2A
	add	 a,e
	ld	 l,$0D
	ld	 e,$03
	inc	 c
L924C:	dec	 d
	dec	 bc
	ld	 (L022A),hl
	jr	 L9255
	dec	 h
	ld	 h,$2B
	ld	 hl,($140B)
	rra	
	dec	 h
	dec	 sp
	jr	 L9261
	add	 hl,de
	cpl	
	dec	 c
L9261:	ld	 c,$2C
	inc	 bc
	inc	 bc
	inc	 a
L9266:	rrca	
	dec	 sp
	dec	 c
	dec	 e
	ld	 l,$1F
	ld	 hl,(L3E3A)
	add	 a,e
	inc	 hl
	add	 a,e
	dec	 c
	ld	 d,l
	inc	 hl
	scf	
	add	 hl,hl
	ld	 (hl),$37
	scf	
	dec	 c
	ld	 h,$26
	jr	 c,L92B2
	ld	 hl,($0520)
	rra	
	ld	 hl,(L3203)
	rrca	
	inc	 c
	dec	 d
	ld	 a,(bc)
	ld	 (L2F0C),hl
	nop	
	ld	 e,$1A
	dec	 bc
	add	 hl,de
	inc	 bc
	add	 a,e
	ld	 d,$0C
	jr	 nz,L92C1
	ld	 c,$2C
	add	 hl,hl
	ld	 (hl),$37
	jr	 L92BE
	inc	 l
	inc	 a
	inc	 c
	inc	 a
	inc	 l
	inc	 bc
	inc	 sp
	inc	 e
	ld	 b,$01
	dec	 c
	ld	 a,$23
	ld	 (L3434),hl
	dec	 hl
	ld	 (bc),a
	add	 hl,de
L92B2:	ld	 e,a
	dec	 h
	jr	 L931C
	rlca	
	inc	 sp
	dec	 c
	inc	 bc
	dec	 l
	ld	 (L0C1F),a
L92BE:	ld	 h,d
	ld	 l,b
	rra	
L92C1:	daa	
	add	 hl,de
	ld	 hl,(L0C28)
	dec	 d
	add	 hl,bc
	ld	 (L3C03),hl
	ld	 a,(L1F2B)
	ld	 a,$10
	dec	 d
	nop	
	add	 hl,bc
	add	 hl,hl
	jr	 L92F5
	jr	 nz,L92FA
	daa	
	ld	 hl,(L1C33)
	ld	 b,$01
	dec	 c
	ld	 a,$22
	add	 a,e
	ld	 c,$3C
	inc	 l
	dec	 e
	ld	 h,$2B
	dec	 l
	dec	 (hl)
	ld	 h,$2B
	dec	 c
	ld	 e,$3E
	add	 hl,hl
	ld	 (hl),$28
	scf	
	ld	 a,$32
L92F5:	dec	 h
	dec	 hl
	ld	 h,$35
	ld	 hl,(L3E10)
	jr	 c,L9331
	dec	 h
	daa	
	ld	 hl,(L833E)
	ld	 (L2983),hl
	inc	 (hl)
	inc	 (hl)
	dec	 hl
	dec	 h
	ld	 l,$39
	inc	 bc
	jr	 L933B
	ld	 e,$1F
	inc	 bc
	ld	 e,$3A
	ld	 (bc),a
	add	 hl,de
	ld	 hl,(L2218)
	inc	 bc
	ld	 hl,(L3728)
	ld	 a,$3E
	jr	 c,L9354
	dec	 h
	daa	
	ld	 hl,(L833E)
	ld	 d,$83
	ld	 e,$3C
	inc	 l
	dec	 h
	ld	 a,(L3B3E)
	rrca	
	ld	 a,(L3C1E)
	inc	 l
	dec	 h
	ld	 a,(L273E)
	dec	 c
	ld	 hl,(L3E28)
	add	 a,e
	ld	 hl,L0E83
	add	 hl,hl
	dec	 l
	dec	 sp
	dec	 hl
	ld	 a,$29
	ld	 (hl),$28
	dec	 d
	dec	 hl
	daa	
	dec	 c
	jr	 c,L9381
	dec	 l
	ld	 h,$2B
	jr	 L9379
	dec	 hl
L9354:	ld	 e,$03
	ld	 e,$33
	dec	 c
	ld	 a,(de)
	ld	 (bc),a
	dec	 c
	ld	 (de),a
	ld	 a,$83
	cpl	
	add	 a,e
	inc	 h
	dec	 d
	ld	 a,$29
	ld	 (hl),$28
	add	 hl,sp
	dec	 a
	ld	 hl,(L3629)
	jr	 z,L9387
	rla	
	ld	 e,$1B
	dec	 d
	ld	 a,(bc)
	ld	 (L3E1E),hl
	ld	 c,$33
	ld	 hl,(L0015)
	add	 hl,bc
	add	 hl,hl
	inc	 c
	ld	 a,$3E
	jr	 c,L93B5
	ld	 e,$33
	dec	 c
	ld	 a,(de)
	ld	 (bc),a
L9387:	dec	 c
	inc	 c
	ld	 l,$1F
	ld	 hl,(L3E3A)
	add	 a,e
	dec	 de
	add	 a,e
	add	 hl,sp
	ld	 h,$35
	dec	 hl
	inc	 bc
	ld	 c,$3A
	dec	 hl
	inc	 bc
	inc	 e
	inc	 h
	dec	 hl
	ld	 a,$1E
	ld	 h,a
	ld	 c,l
	ld	 a,(L031F)
	dec	 hl
	ld	 a,e
	add	 hl,bc
	ld	 e,$29
	ld	 a,$83
	rra	
	dec	 de
	ld	 h,b
	ld	 c,e
	ld	 h,d
	ld	 a,$3E
	add	 hl,hl
	inc	 (hl)
	inc	 (hl)
L93B5:	dec	 hl
	rra	
	dec	 h
	ld	 b,$21
	add	 hl,hl
	rra	
	inc	 bc
	ld	 c,$28
	scf	
	ld	 hl,(L031F)
	inc	 sp
	dec	 c
	ld	 hl,(L0A15)
	ld	 (L3E1E),hl
	inc	 l
	add	 a,e
	inc	 c
	dec	 d
	nop	
	add	 hl,bc
	ld	 (L2C0E),hl
	inc	 a
	rra	
	ld	 hl,(L031F)
	dec	 hl
	inc	 sp
	dec	 c
	dec	 l
	dec	 d
	ld	 a,(bc)
	ld	 (L1E18),hl
	ld	 a,$27
	dec	 c
	jr	 c,L941A
	dec	 l
	ld	 h,$2B
	jr	 L9412
	dec	 hl
	ld	 e,$3E
	ld	 e,$33
	dec	 c
	ld	 a,(de)
	ld	 (bc),a
	dec	 c
	ld	 (de),a
	ld	 a,$83
	inc	 e
	dec	 c
	dec	 d
	ld	 h,e
	ld	 (hl),a
	add	 hl,hl
	inc	 (hl)
	inc	 (hl)
	dec	 hl
	ld	 h,$0D
	jr	 L942E
	ld	 hl,(L2E10)
	dec	 c
	rra	
L940A:	dec	 bc
	rra	
	add	 hl,hl
	inc	 (hl)
	inc	 (hl)
	dec	 hl
	ld	 e,$2E
L9412:	dec	 c
	rra	
	ld	 a,$24
	add	 a,e
	inc	 h
	dec	 hl
	inc	 bc
L941A:	inc	 bc
	ld	 (L2836),hl
	inc	 bc
	add	 a,e
	dec	 e
	daa	
	ld	 hl,(L0303)
	ld	 hl,(L0328)
	add	 a,e
	rra	
	ld	 a,(L080F)
	ld	 a,(bc)
L942E:	ld	 (L030F),hl
	inc	 bc
	jr	 c,L9467
	inc	 bc
	add	 a,e
	dec	 h
	daa	
	ld	 hl,(L1F3E)
	jr	 z,L9462
	rra	
	ld	 a,$3E
	dec	 d
	inc	 hl
	add	 hl,bc
	add	 hl,hl
	inc	 c
	inc	 sp
	rra	
	ld	 hl,(L2F1B)
	rrca	
	dec	 e
	ld	 h,$2B
	inc	 e
	dec	 d
	ld	 hl,(L0D02)
	jr	 c,L9488
	dec	 l
	dec	 a
	jr	 L9478
	ld	 a,$1B
	add	 a,e
	dec	 l
	cpl	
	ld	 a,(L2B15)
	ld	 (L2836),hl
	inc	 e
	ld	 h,$0B
L9467:	inc	 d
	ld	 hl,(L1B28)
	dec	 d
	ld	 a,(bc)
	ld	 (L031E),hl
	dec	 c
	dec	 d
	inc	 hl
	jr	 z,L94B3
	add	 a,e
L9476:	ld	 h,(hl)
	adc	 a,e
L9478:	add	 a,h
	adc	 a,e
	pop	 bc
	adc	 a,e
	sub	 $8B
	rst	 18H
	adc	 a,e
	pop	 af
	adc	 a,e
	dec	 e
	adc	 a,h
	ld	 a,(L3F8C)
	adc	 a,l
L9488:	ld	 c,(hl)
	adc	 a,l
	ld	 (hl),a
	adc	 a,(hl)
	adc	 a,e
	adc	 a,(hl)
	sub	 (hl)
	adc	 a,(hl)
	xor	 c
	adc	 a,(hl)
	or	 d
	adc	 a,(hl)
	out	 ($8E),a
	jp	 p,LFD8E
	adc	 a,(hl)
	jr	 nz,L942B
	ld	 b,c
	adc	 a,a
	ld	 h,l
	adc	 a,a
L94A0:	add	 a,e
	adc	 a,a
	or	 l
	adc	 a,a
	ret	 z
	adc	 a,a
	ret	 p
	adc	 a,a
	inc	 e
	sub	 b
	ld	 d,l
L94AB:	adc	 a,l
	add	 hl,hl
	adc	 a,l
	adc	 a,d
	adc	 a,l
	or	 l
	adc	 a,l
	.db	 $dd,$8d
	nop	
	adc	 a,(hl)
	ld	 c,e
	adc	 a,(hl)
	inc	 h
	adc	 a,(hl)
	ld	 e,c
	adc	 a,h
	ld	 a,b
	adc	 a,h
	sbc	 a,b
	adc	 a,h
L94C0:	push	 bc
	adc	 a,h
	rst	 30H
	adc	 a,h
	dec	 c
	adc	 a,l
	ld	 a,(hl)
	sub	 c
	dec	 hl
	sub	 b
	ld	 b,h
L94CB:	sub	 b
	ld	 l,e
	sub	 b
	sub	 e
	sub	 b
	or	 (hl)
	sub	 b
	ret	 nc
	sub	 b
	jp	 p,L1F90
	sub	 c
	ld	 b,b
	sub	 c
	ld	 d,a
	sub	 c
	and	 c
	sub	 c
	adc	 a,$91
	ld	 (hl),e
	adc	 a,l
	dec	 (hl)
	adc	 a,a
	rst	 30H
	sub	 c
	rst	 38H
	sub	 c
	rla	
	sub	 d
	daa	
	sub	 d
	ld	 b,l
	sub	 d
	ld	 (hl),b
	sub	 d
	sub	 h
	sub	 d
	xor	 e
	sub	 d
	rst	 08H
	sub	 d
	and	 c
	adc	 a,a
	xor	 d
	adc	 a,a
	ret	 po
	sub	 d
L94FC:	inc	 bc
	sub	 e
	ld	 h,$93
	dec	 a
	sub	 e
	ld	 e,a
	sub	 e
	adc	 a,a
	sub	 e
	xor	 e
	sub	 e
	res	 2,e
	ret	 m
	sub	 e
	dec	 d
	sub	 h
	ld	 a,(L5A94)
	sub	 h
	or	 (hl)
L9513:	adc	 a,e
L9514:	add	 a,c
	ld	 a,(bc)
	add	 a,d
	dec	 bc
	inc	 b
	add	 a,c
	ld	 a,(bc)
	add	 a,d
	inc	 c
L951D:	djnz	 L94A0
	ld	 a,(bc)
	add	 a,d
	dec	 bc
	inc	 b
	add	 a,c
	ld	 a,(bc)
	add	 a,d
	inc	 c
	djnz	 L94AB
	dec	 c
	add	 hl,bc
	add	 a,d
	ld	 c,$04
	add	 a,c
	rrca	
	add	 a,d
	ld	 de,L8210
	ld	 e,$36
	add	 a,c
	dec	 l
	add	 a,d
	ld	 l,$10
	add	 a,d
	cpl	
	djnz	 L94C0
	nop	
	add	 a,d
	ld	 c,(hl)
	ld	 (bc),a
	add	 a,d
	inc	 bc
	inc	 b
	add	 a,d
	dec	 b
	djnz	 L94CB
	ld	 b,$81
L954C:	rlca	
	add	 a,d
	ex	 af,af'
	scf	
	add	 a,d
	inc	 sp
	ld	 (hl),$81
	inc	 hl
	add	 a,d
	inc	 h
	ld	 (hl),$82
	daa	
	ld	 (hl),$82
	dec	 h
	ld	 (hl),$82
	jr	 nc,L9597
	add	 a,d
L9562:	ld	 sp,L8109
	ld	 (L1D81),a
L9568:	add	 a,d
	ld	 (de),a
	ld	 (hl),$81
	inc	 de
	add	 a,c
L956E:	inc	 d
	add	 a,d
	dec	 d
	ld	 b,b
	add	 a,d
	scf	
L9574:	ld	 h,$82
	inc	 (hl)
	djnz	 L94FC
	add	 hl,bc
L957A:	ld	 (L8210),hl
	dec	 (hl)
	scf	
	add	 a,d
	ld	 a,(de)
	ld	 (hl),$81
	dec	 de
	add	 a,d
	inc	 e
	ld	 (hl),$82
	ld	 bc,L8236
	rra	
	add	 hl,bc
	add	 a,d
	add	 hl,bc
	jr	 nz,L9513
	ld	 hl,L8236
	jr	 z,L95CC
	add	 a,e
L9597:	ld	 d,$04
	djnz	 L951D
	rla	
	scf	
	add	 a,d
	jr	 L95D7
	add	 a,d
	inc	 b
	add	 hl,de
	add	 a,d
	add	 hl,hl
	scf	
	add	 a,c
	ld	 hl,(L2B82)
	ld	 (hl),$81
	inc	 l
	add	 a,e
	jr	 c,L95B4
	djnz	 L9535
	add	 hl,sp
	scf	
L95B4:	ld	 (hl),$82
	ld	 a,(L8210)
	dec	 sp
	ld	 (hl),$82
	inc	 a
	scf	
	add	 a,d
	add	 hl,bc
	dec	 a
	add	 a,d
	ld	 a,$10
	add	 a,e
	ccf	
	inc	 (hl)
	djnz	 L954C
	ld	 b,c
	ld	 b,d
	ld	 (hl),$83
	ld	 b,c
	ld	 b,e
	ld	 (hl),$83
	ld	 b,h
	ld	 (bc),a
	ld	 (hl),$81
	ld	 b,l
	add	 a,d
L95D7:	ld	 b,(hl)
	ld	 (hl),$82
	ld	 b,a
	ld	 (hl),$82
	ld	 c,b
	djnz	 L9562
	ld	 c,c
	ld	 (hl),$82
	ld	 c,d
	djnz	 L9568
	ld	 c,e
	djnz	 L956B
	ld	 c,h
	djnz	 L956E
	ld	 c,l
	ld	 (hl),$82
	ld	 c,d
	djnz	 L9574
	ld	 c,e
	ld	 (hl),$82
	ld	 c,h
	djnz	 L957A
	ld	 c,l
	ld	 (hl),$00

	.db	$ff,$ff,$ff,$ff,$ff

;******************************************************************************
; At $9600 starts pattern of Garwor (demo screen) ???
;******************************************************************************

L9600:	

	.DB	$00,$00,$00,$03,$C0	; . . . . . . . . . . . . . . . 3 3 . . .	
	.DB	$00,$02,$AA,$00,$FC  	; . . . . . . . 2 2 2 2 2 . . . . 3 3 3 .
	.DB	$00,$0A,$AA,$80,$3C     ; . . . . . . 2 2 2 2 . . . . . . 3 3 3 .   
	.DB	$00,$AA,$0A,$A0,$08	; . . . . 2 2 2 2 . . 2 2 2 2 . . . . 2 .
	.DB	$0A,$AA,$4A,$A8,$08	; . . 2 2 2 2 2 2 1 . 2 2 2 2 . . . . 2 .
	.DB	$0A,$AA,$AA,$AA,$08	; . . 2 2 2 2 2 2 2 2 2 2 2 2 2 2 . . 2 .
	.DB	$00,$BB,$AA,$AA,$08	; . . . . 2 3 2 3 2 2 2 2 2 2 2 2 . . 2 . 
	.DB	$0F,$FF,$AA,$AA,$88	; . . 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 . 2 .
	.DB	$32,$EE,$8A,$AA,$88     ; . 3 . 2 3 2 3 2 2 . 2 2 2 2 2 2 2 . 2 .      
	.DB	$00,$AA,$2A,$AA,$88    	; . . . . 2 2 2 2 . 2 2 2 2 2 2 2 2 . 2 .
	.DB	$00,$00,$2A,$AA,$A8    	; . . . . . . . . . 2 2 2 2 2 2 2 2 2 2 .
	.DB	$00,$00,$AA,$AA,$A8     ; . . . . . . . . 2 2 2 2 2 2 2 2 2 2 2 .      
	.DB	$00,$0A,$AA,$AA,$A0     ; . . . . . . 2 2 2 2 2 2 2 2 2 2 2 2 . .       
	.DB	$00,$20,$2A,$AA,$80     ; . . . . . 2 . . 2 . 2 2 2 2 2 2 2 . . .      
	.DB	$00,$00,$0A,$A8,$00     ; . . . . . . . . . . 2 2 2 2 2 . . . . .    
	.DB	$00,$00,$0A,$82,$00     ; . . . . . . . . . . 2 2 2 . . 2 . . . .       
	.DB	$00,$00,$0A,$02,$00     ; . . . . . . . . . . 2 2 . . . 2 . . . .       
	.DB	$00,$00,$A8,$0A,$00     ; . . . . . . . . 2 2 2 . . . 2 2 . . . .
	
; ------------------------------------------------------------------------------------

	nop	
	ld	 a,(bc)
	xor	 b
	nop	
	inc	 a
	nop	
	ld	 hl,(L03A8)
	ret	 p
	ld	 (bc),a
	xor	 b
	ld	 hl,(LC003)
	ld	 hl,(L2AA9)
	add	 a,d
	nop	
L966E:	ld	 hl,(LAAAA)
	add	 a,d
	nop	
	ld	 (bc),a
	xor	 $EA
	and	 d
	xor	 b
	inc	 bc
	rst	 38H
	jp	 pe,L08A0
	dec	 bc
	cp	 e
	xor	 d
	xor	 b
	ex	 af,af'
	ld	 (bc),a
	xor	 d
	xor	 d
	xor	 d
	jr	 z,L9688
L9688:	ld	 (bc),a
	xor	 d
	xor	 d
	jr	 nz,L968D
L968D:	ld	 (bc),a
	xor	 d
	xor	 d
	and	 b
	nop	
	ld	 (bc),a
	xor	 d
	xor	 d
	and	 b
	nop	
	ex	 af,af'
	xor	 d
	xor	 d
	add	 a,b
	nop	
	jr	 nz,$9648
	xor	 d
	nop	
	nop	
	nop	
	ld	 hl,(L00A0)
	nop	
	nop	
	ld	 a,(bc)
	ld	 a,(bc)
	add	 a,b
	nop	
	nop	
	ld	 (bc),a
	nop	
	add	 a,b
	nop	
	nop	
	ld	 hl,($0000)
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	call	 m,L0A00
	xor	 d
	inc	 bc
	ret	 p
	nop	
	xor	 d
	ld	 a,(bc)
	add	 a,e
	ret	 p
	ld	 a,(bc)
	xor	 d
	ld	 c,d
	and	 b
	add	 a,b
	ld	 a,(bc)
	xor	 d
	xor	 d
	and	 b
	xor	 b
	nop	
	cp	 e
	cp	 d
	xor	 b
	ex	 af,af'
	rrca	
	rst	 38H
	jp	 m,L08A8
	ld	 (LEAEE),a
	xor	 d
	ex	 af,af'
	nop	
	xor	 d
	xor	 d
	xor	 d
	ex	 af,af'
	nop	
	nop	
	xor	 d
	xor	 d
	adc	 a,b
	nop	
	nop	
	xor	 d
	xor	 d
	xor	 b
	nop	
	ld	 (bc),a
	xor	 d
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 hl,(LA0AA)
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
L96FB:	nop	
	ld	 hl,(L00A8)
	nop	
	nop	
	and	 b
	and	 b
	nop	
	nop	
	nop	
	jr	 z,L9728
	nop	
	nop	
	nop	
	ex	 af,af'
	and	 b
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 bc
	jr	 z,L9718
L9718:	nop	
	nop	
	dec	 bc
	jr	 z,L971D
L971D:	nop	
	nop	
	cpl	
	xor	 d
	nop	
	nop	
	jr	 nz,L9750
	jp	 pe,$0000
L9728:	ex	 af,af'
	cpl	
	xor	 d
	add	 a,b
	nop	
	ex	 af,af'
	dec	 hl
	jp	 pe,L20A0
	ld	 a,(bc)
	ld	 a,(bc)
	and	 h
	and	 b
	jr	 nz,L9762
	and	 d
	and	 b
	and	 b
	ld	 hl,(LAAAA)
	xor	 d
	and	 b
	ld	 a,(bc)
	xor	 d
	xor	 d
	xor	 d
	and	 b
	ld	 (bc),a
	xor	 d
	xor	 d
	xor	 d
	add	 a,b
	nop	
	xor	 d
	xor	 d
	xor	 d
	nop	
	jr	 nz,L96FB
	xor	 d
	xor	 b
	nop	
	ld	 hl,(LAA2A)
	and	 b
	inc	 c
	nop	
	ld	 hl,(L00AA)
	inc	 a
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
L9762:	ret	 p
	nop	
	ld	 (bc),a
	xor	 d
	xor	 d
	ret	 p
	nop	
	nop	
	nop	
	ld	 a,(bc)
	nop	
	nop	
	nop	
	ld	 (bc),a
	ld	 a,(bc)
	nop	
	nop	
	nop	
	dec	 bc
	jp	 pe,L0080
	nop	
	ld	 a,(bc)
	jp	 m,L0080
	jr	 nz,L978A
	jp	 pe,L00A0
	ex	 af,af'
	ld	 a,(bc)
	jp	 m,L00A8
	ld	 (bc),a
	xor	 e
	jp	 (hl)
L978A:	jr	 z,L978C
L978C:	ld	 hl,(LF8AA)
	jr	 z,L97B1
	xor	 d
	xor	 d
	xor	 d
	xor	 b
	ld	 (LAAAA),hl
	xor	 d
	xor	 b
	ld	 hl,(LAAAA)
	xor	 d
	add	 a,b
	nop	
	xor	 d
	xor	 d
	xor	 d
	nop	
	nop	
	xor	 d
	xor	 d
	and	 b
L97A8:	nop	
	ld	 (bc),a
	ld	 hl,(L00AA)
	nop	
	ld	 (bc),a
	ld	 hl,(L2AA8)
	ret	 p
	ld	 a,(bc)
	ld	 a,(bc)
	add	 a,b
	jr	 nz,L97A8
	nop	
	ld	 (bc),a
	xor	 b
	jr	 nz,$97F9
	nop	
	nop	
	ld	 a,(bc)
	and	 b
	inc	 c

; ----> Block of 62 $ff

	.db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	
	.db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	.db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	.db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	.db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	.db	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
	.db	$ff,$ff

; ----> Begin character data again

	nop	
	nop	
	ld	 c,$30
	nop	
	nop	
	nop	
	ld	 a,(bc)
	or	 b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	adc	 a,b
	nop	
	nop	
	nop	
	ld	 (bc),a
	nop	
	nop	
	nop	
	djnz	 L981A
	nop	
	nop	
	ld	 bc,$0540
	ld	 b,b
	nop	
	dec	 b
	nop	
	inc	 bc
	nop	
	nop	
	dec	 b
	ld	 b,b
	dec	 b
	ld	 b,b
	nop	
	dec	 b
	ld	 d,b
	inc	 bc
	nop	
	nop	
	nop	
	ld	 d,h
	inc	 bc
	nop	
	nop	
	nop	
	dec	 d
	rrca	
	ld	 bc,$0000
	dec	 b
L9839:	ld	 e,a
	ld	 b,c
	jr	 nc,L984D
	ld	 d,l
	ld	 e,a
	ld	 d,l
	inc	 b
	ld	 de,L5F55
	ld	 (hl),l
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 e,a
	push	 af
	ld	 d,h
	dec	 d
	dec	 b
L984D:	ld	 d,a
	ret	 nc
	ld	 d,b
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	jr	 nc,L985B
L985B:	nop	
	nop	
	inc	 c
	call	 z,$0000
	inc	 c
	ld	 (bc),a
	or	 b
	nop	
	nop	
	nop	
	ld	 (bc),a
	xor	 h
	nop	
	ld	 bc,L8200
	and	 b
	nop	
	inc	 d
	nop	
	ld	 (bc),a
	add	 a,b
	nop	
	ld	 d,b
	nop	
	inc	 e
	nop	
	nop	
	ld	 d,b
	nop	
	ld	 (hl),h
	nop	
	nop	
	inc	 d
	nop	
	ret	 nc
	nop	
	nop	
	dec	 d
	inc	 bc
	ret	 nz
	nop	
	nop	
	dec	 b
	rrca	
	nop	
	nop	
	djnz	 L9893
	rrca	
	nop	
	nop	
	djnz	 L9898
L9893:	ld	 a,h
	ld	 bc,$1000
	dec	 b
L9898:	ld	 a,l
	ld	 bc,$1530
	dec	 b
	ld	 a,l
	ld	 b,l
	inc	 b
	dec	 d
	ld	 d,l
	ld	 a,a
	ld	 d,l
	ld	 d,h
	dec	 b
	ld	 d,l
	ld	 e,a
	push	 de
	ld	 d,h
	nop	
	ld	 d,l
	ld	 d,a
	ld	 d,h
	ld	 d,b
	nop	
	nop	
	ld	 d,l
	ld	 d,b
	jr	 nc,L98B5
L98B5:	nop	
	nop	
	jr	 nc,L9839
	nop	
	nop	
	nop	
	ret	 z
	inc	 c
	nop	
	nop	
	nop	
	ld	 hl,(L00C0)
	nop	
	nop	
	ld	 hl,($0000)
	inc	 b
	nop	
	jr	 z,L98CD
L98CD:	nop	
	ld	 d,b
	ld	 bc,L08C0
	ld	 bc,L0340
	ld	 b,e
	nop	
	ld	 bc,L0F50
	ret	 nz
	nop	
	nop	
	ld	 d,h
	rrca	
	nop	
	nop	
	nop	
	dec	 d
	rrca	
	nop	
	nop	
	djnz	 L98ED
	ccf	
	nop	
	nop	
	djnz	 L98F2
L98ED:	ld	 a,h
	ld	 bc,$1000
	dec	 b
L98F2:	ld	 a,l
	ld	 b,c
	jr	 nc,L990B
	ld	 d,l
	ld	 a,l
	ld	 d,l
	inc	 b
	dec	 d
	ld	 d,l
	ld	 a,a
	ld	 d,l
	ld	 d,h
	nop	
	ld	 d,l
	ld	 e,a
	push	 de
	ld	 d,h
	nop	
	nop	
	ld	 d,a
	ld	 d,b
	ld	 d,b
	nop	
	nop	
L990B:	ld	 d,l
	ld	 d,h
	jr	 nc,L990F
L990F:	nop	
	ld	 c,$30
	nop	
	nop	
	nop	
	ld	 a,(bc)
	or	 b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	adc	 a,b
	jr	 nz,L991E
L991E:	nop	
	ld	 (bc),a
	nop	
	nop	
	nop	
	nop	
	inc	 bc
	nop	
	ret	 nz
	nop	
	ld	 b,b
	dec	 b
	ld	 b,b
	nop	
	ld	 bc,L0340
	nop	
	nop	
	dec	 b
	nop	
	dec	 b
	ld	 b,b
	nop	
	dec	 b
	ld	 d,b
	inc	 bc
	nop	
	nop	
	nop	
	ld	 d,l
	inc	 bc
	nop	
	nop	
	nop	
	dec	 d
	rrca	
	ld	 bc,$0000
	dec	 b
	ld	 e,a
	ld	 b,c
	jr	 nc,L995B
	dec	 b
	ld	 e,a
	ld	 d,l
	inc	 b
	djnz	 L99A6
	ld	 e,a
	ld	 (hl),l
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 e,a
	push	 af
	ld	 d,h
	dec	 d
	ld	 d,l
L995B:	ld	 d,a
	ret	 nc
	ld	 d,b
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	jr	 nc,L9969
L9969:	nop	
	nop	
	dec	 d
	nop	
	nop	
	nop	
	nop	
	push	 bc
	ld	 c,h
	nop	
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	ld	 bc,L0055
	ld	 (bc),a
	nop	
	nop	
	dec	 d
	inc	 d
	inc	 a
	nop	
	nop	
	rra	
	ld	 d,h
	ld	 a,(bc)
	inc	 b
	ld	 b,b
	ld	 d,a
	call	 nc,LB72A
	ld	 a,a
	rst	 38H
	call	 nc,L043A	; ??? This is in the middle of a string!
	ld	 b,e
	rst	 38H
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 bc,L4055
	nop	
	nop	
L99A6:	dec	 b
	ld	 d,l
	ld	 b,b
	nop	
	djnz	 L99C1
	dec	 d
	nop	
	nop	
	inc	 b
	ld	 d,h
	dec	 d
	nop	
	nop	
	dec	 b
	ld	 d,b
	dec	 b
	ld	 b,b
	nop	
	ld	 bc,L0140
	ld	 b,b
	nop	
	nop	
	nop	
	dec	 d
L99C1:	ld	 b,b
	inc	 sp
	nop	
	nop	
	dec	 b
	ld	 b,b
	ld	 c,$80
	nop	
	ld	 sp,L3A5C
	and	 b
	nop	
	ld	 bc,L0A50
	and	 b
	nop	
	ld	 d,l
	ld	 b,b
	jr	 nc,L99E5
	nop	
	dec	 b
	ld	 d,b
	nop	
	rlca	
	ld	 b,b
	ld	 bc,L0054
	add	 a,c
	ret	 p
	dec	 b
	call	 nc,$0000
	ccf	
	rla	
	call	 p,L000C
	rrca	
	rst	 38H
	call	 nc,$0000
	nop	
	rst	 38H
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	ld	 b,b
	dec	 d
	ld	 d,l
	ld	 d,b
	nop	
	djnz	 L9A56
	ld	 d,l
	ld	 d,b
	nop	
	dec	 d
	ld	 d,b
	ld	 bc,L0050
	dec	 b
	nop	
	ld	 bc,L0050
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	ld	 bc,L0055
	inc	 c
	ex	 af,af'
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 sp,L235C
	nop	
	nop	
	ld	 bc,L0250
	add	 a,e
	nop	
	ld	 d,l
	ld	 b,b
	ld	 a,(bc)
	and	 b
	nop	
	dec	 b
	ld	 b,h
	ld	 (L00A0),a
	dec	 b
	ld	 d,h
	inc	 c
	dec	 c
	ret	 nz
	dec	 d
	call	 nc,L0700
	rst	 38H
	rla	
	call	 p,$0000
	rst	 38H
	rst	 38H
	call	 nc,$0000
	inc	 bc
	rst	 38H
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	dec	 b
L9A56:	ld	 d,l
	ld	 b,b
	nop	
	djnz	 L9A70
	ld	 d,l
	ld	 b,b
	nop	
	inc	 b
	ld	 d,h
	dec	 b
	ld	 b,b
	nop	
	dec	 b
	ld	 d,b
	dec	 b
	ld	 b,b
	nop	
	ld	 bc,$0540
	nop	
	nop	
	nop	
	nop	
	dec	 b
L9A70:	nop	
	nop	
	nop	
	ld	 bc,L0055
	nop	
	nop	
	nop	
	dec	 d
	nop	
	ld	 (bc),a
	nop	
	nop	
	push	 bc
	ld	 c,h
	nop	
	jr	 nc,L9A83
L9A83:	dec	 b
	ld	 b,b
	nop	
	nop	
	ld	 bc,L0055
	ld	 (bc),a
	nop	
	nop	
	dec	 d
	inc	 d
	inc	 a
	nop	
L9A91:	nop	
	rra	
	ld	 d,h
	ld	 a,(bc)
	inc	 b
	ld	 b,b
	ld	 d,a
	call	 nc,LB72A
	ld	 a,a
	rst	 38H
	call	 nc,L043A	; ??? This is in the middle of a string!
	ld	 b,e
	rst	 38H
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	dec	 d
	dec	 b
	ld	 b,b
	nop	
	dec	 b
	inc	 d
	dec	 b
	ld	 b,b
	nop	
	ld	 bc,L0150
	ld	 b,b
	nop	
	nop	
	ld	 d,b
	ld	 bc,L0040
	nop	
	nop	
	dec	 d
	ld	 b,b
	nop	
	nop	
	ld	 c,$30
	nop	
	nop	
	nop	
	ld	 a,(bc)
	or	 b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	adc	 a,b
	nop	
	nop	
	nop	
	ld	 (bc),a
	nop	
	nop	
	nop	
	jr	 nz,L9AEA
	nop	
	nop	
	ld	 (bc),a
L9AEA:	add	 a,b
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 a,(bc)
	nop	
	inc	 bc
	nop	
	nop	
	ld	 a,(bc)
	add	 a,b
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 a,(bc)
	and	 b
	inc	 bc
	nop	
	nop	
	nop	
	xor	 b
	inc	 bc
	nop	
	nop	
	nop	
	ld	 hl,(L020F)
	nop	
	nop	
	ld	 a,(bc)
L9B09:	xor	 a
	add	 a,d
	jr	 nc,L9B2D
	xor	 d
	xor	 a
	xor	 d
	ex	 af,af'
	ld	 (LAFAA),hl
	cp	 d
	xor	 b
	ld	 hl,(LAFAA)
	jp	 m,L2AA8
	ld	 a,(bc)
	xor	 e
	ret	 po
	and	 b
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	jr	 nc,L9B2B
L9B2B:	nop	
	nop	
L9B2D:	inc	 c
	call	 z,$0000
	inc	 c
	ld	 (bc),a
	or	 b
	nop	
	nop	
	nop	
	ld	 (bc),a
	xor	 h
	nop	
	ld	 (bc),a
	nop	
	ld	 b,d
	and	 b
	nop	
	jr	 z,L9B41
L9B41:	ld	 (bc),a
	add	 a,b
	nop	
	and	 b
	nop	
	inc	 l
	nop	
	nop	
	and	 b
	nop	
	cp	 b
	inc	 b
	nop	
	jr	 z,L9B50
L9B50:	ret	 po
	nop	
	nop	
	ld	 hl,(LC003)
	nop	
	nop	
	ld	 a,(bc)
	rrca	
	nop	
	nop	
	jr	 nz,L9B68
	rrca	
	nop	
	nop	
	jr	 nz,L9B6D
	cp	 h
	ld	 (bc),a
	nop	
	jr	 nz,L9B72
L9B68:	cp	 (hl)
	ld	 (bc),a
	jr	 nc,L9B96
	ld	 a,(bc)
L9B6D:	cp	 (hl)
	adc	 a,d
	ex	 af,af'
	ld	 hl,(LBFAA)
	xor	 d
	xor	 b
	ld	 a,(bc)
	xor	 d
	xor	 a
	jp	 pe,L00A8
	xor	 d
	xor	 e
	xor	 b
	and	 b
	nop	
	nop	
	xor	 d
	and	 b
	jr	 nc,L9B85
L9B85:	nop	
	nop	
	jr	 nc,L9B09
	nop	
	nop	
	nop	
	ld	 c,b
	inc	 b
	nop	
	nop	
	nop	
	ld	 hl,(L00C0)
	nop	
	nop	
L9B96:	ld	 hl,($0000)
	ex	 af,af'
	nop	
	jr	 z,L9B9D
L9B9D:	nop	
	and	 b
	ld	 (bc),a
	ret	 nz
	ex	 af,af'
	ld	 (bc),a
	add	 a,b
	inc	 bc
	add	 a,e
	nop	
	ld	 (bc),a
	and	 b
	rrca	
	ret	 nz
	nop	
	nop	
	xor	 b
	rrca	
	nop	
	nop	
	nop	
	ld	 hl,(L000F)
	nop	
	jr	 nz,L9BC2
	ccf	
	nop	
	nop	
	jr	 nz,L9BC7
	cp	 h
	ld	 (bc),a
	nop	
	jr	 nz,L9BCC
L9BC2:	cp	 (hl)
	add	 a,d
	jr	 nc,L9BF0
	xor	 d
L9BC7:	cp	 (hl)
	xor	 d
	ex	 af,af'
	ld	 hl,(LBFAA)
	xor	 d
	xor	 b
	nop	
	xor	 d
	xor	 a
	jp	 pe,L00A8
	nop	
	xor	 e
	and	 b
	and	 b
	nop	
	nop	
	xor	 d
	xor	 b
	jr	 nc,L9BDF
L9BDF:	nop	
	ld	 c,$30
	nop	
	nop	
	nop	
	ld	 c,d
	or	 b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	adc	 a,b
	jr	 nz,L9BEE
L9BEE:	nop	
	ld	 (bc),a
L9BF0:	nop	
	nop	
	nop	
	nop	
	inc	 bc
	nop	
	ret	 nz
	nop	
	add	 a,b
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 (bc),a
	add	 a,b
	inc	 bc
	nop	
	nop	
	ld	 a,(bc)
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 a,(bc)
	and	 b
	inc	 bc
	nop	
	nop	
	nop	
	xor	 d
	inc	 bc
	nop	
	nop	
	nop	
	ld	 hl,(L020F)
	nop	
	nop	
	ld	 a,(bc)
	xor	 a
	add	 a,d
	jr	 nc,L9C3B
	ld	 a,(bc)
	xor	 a
	xor	 d
	ex	 af,af'
	jr	 nz,L9BCB
	xor	 a
L9C22:	cp	 d
	xor	 b
	ld	 hl,(LAFAA)
	jp	 m,L2AA8
	xor	 d
	xor	 e
	ret	 po
	and	 b
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
;
; At $9C38, it is the pattern for the blue player (demo screen)!
; Need to change to data and find start and end ???
;
	jr	 nc,L9C39
L9C39:	nop	
	nop	
L9C3B:	ld	 hl,($0000)
	nop	
	nop	
	jp	 z,L008C
	nop	
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	ld	 (bc),a
	nop	
	nop	
	ld	 hl,(L3C28)
	nop	
	nop	
	cpl	
	xor	 b
	ld	 a,(bc)
	ex	 af,af'
	add	 a,b
	xor	 e
	ret	 pe
	ld	 hl,(LBFBB)
	rst	 38H
	ret	 pe
	ld	 a,(L8308)
	rst	 38H
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	add	 a,b
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
	jr	 nz,L9CA6
	ld	 hl,($0000)
	ex	 af,af'
	xor	 b
	ld	 hl,($0000)
	ld	 a,(bc)
	and	 b
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 (bc),a
	add	 a,b
	ld	 (bc),a
	add	 a,b
	nop	
	nop	
	nop	
	ld	 hl,(L3380)
	ld	 bc,L0A00
	add	 a,b
	ld	 c,$80
	nop	
	ld	 (L3AAC),a
	and	 b
	nop	
	ld	 (bc),a
	and	 b
	ld	 a,(bc)
	and	 b
	nop	
	xor	 d
	add	 a,b
L9CA6:	jr	 nc,L9CB6
	nop	
	ld	 a,(bc)
	and	 b
	nop	
	dec	 bc
	add	 a,b
	ld	 (bc),a
	xor	 b
	nop	
	ld	 b,d
	ret	 p
	ld	 a,(bc)
	ret	 pe
	nop	
L9CB6:	nop	
	ccf	
	dec	 hl
	ret	 m
	inc	 c
	nop	
	rrca	
	rst	 38H
	ret	 pe
	nop	
	nop	
	nop	
	rst	 38H
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	add	 a,b
	ld	 hl,(LA0AA)
	nop	
	jr	 nz,L9C7B
	xor	 d
	and	 b
	nop	
	ld	 hl,(L02A0)
	and	 b
	nop	
	ld	 a,(bc)
	nop	
	ld	 (bc),a
	and	 b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
	nop	
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	inc	 b
	ex	 af,af'
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
	nop	
	nop	
	ld	 (L23AC),a
	nop	
	nop	
	ld	 (bc),a
	and	 b
	ld	 (bc),a
	add	 a,e
	nop	
	xor	 d
	add	 a,b
	ld	 a,(bc)
	and	 b
	nop	
	ld	 a,(bc)
	adc	 a,b
	ld	 (L00A0),a
	ld	 a,(bc)
	xor	 b
	inc	 b
	ld	 c,$C0
	ld	 hl,(L00E8)
	dec	 bc
	rst	 38H
	dec	 hl
	ret	 m
	nop	
	nop	
	rst	 38H
	rst	 38H
	ret	 pe
	nop	
	nop	
	inc	 bc
	rst	 38H
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
	jr	 nz,L9D55
	xor	 d
	add	 a,b
	nop	
	ex	 af,af'
	xor	 b
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 a,(bc)
	and	 b
	ld	 a,(bc)
	add	 a,b
	nop	
	ld	 (bc),a
	add	 a,b
	ld	 a,(bc)
	nop	
	nop	
	nop	
	nop	
	ld	 a,(bc)
	nop	
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	nop	
	nop	
	nop	
	ld	 hl,(L0200)
	nop	
	nop	
	jp	 z,L008C
	jr	 nc,L9D53
L9D53:	ld	 a,(bc)
	add	 a,b
L9D55:	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	ld	 (bc),a
	nop	
	nop	
	ld	 hl,(L3C28)
	nop	
	nop	
	cpl	
	xor	 b
	ld	 a,(bc)
	ex	 af,af'
	add	 a,b
	xor	 e
	ret	 pe
	ld	 hl,(LBFBB)
	rst	 38H
	ret	 pe
	ld	 a,(L8308)
	rst	 38H
	xor	 b
	nop	
	nop	
	nop	
	xor	 d
	xor	 b
	inc	 b
	nop	
	nop	
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
	nop	
	ld	 hl,(L800A)
	nop	
	ld	 a,(bc)
	jr	 z,L9D9A
	add	 a,b
	nop	
	ld	 (bc),a
	and	 b
	ld	 (bc),a
	add	 a,b
	nop	
	nop	
	and	 b
	ld	 (bc),a
L9D9A:	add	 a,b
	nop	
	nop	
	nop	
	ld	 hl,(L0080)
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	nop	
	dec	 d
	ld	 b,b
	nop	
	djnz	 L9DB1
	dec	 d
	ld	 b,b
	nop	
	djnz	 L9E06
L9DB1:	dec	 d
	ld	 d,b
	nop	
	ld	 de,$1555
	call	 nc,$1500
	ld	 d,h
	dec	 b
	call	 p,L1400
	djnz	 L9DC6
	call	 p,$1000
	ld	 d,b
	ld	 d,l
L9DC6:	ld	 d,h
	nop	
	ld	 bc,L5555
	ld	 d,h
	nop	
	ld	 bc,L5555
	ld	 d,h
	nop	
	ld	 bc,L0155
	ld	 b,c
	nop	
	ld	 bc,L0155
	djnz	 L9DDC
L9DDC:	ld	 bc,L4054
	nop	
	nop	
	djnz	 L9DF3
	djnz	 L9DE5
L9DE5:	nop	
	dec	 d
	ld	 d,h
	dec	 b
	ld	 d,l
	ld	 d,b
	ld	 de,L0154
	ld	 d,l
	ld	 d,h
	djnz	 L9E47
	nop	
L9DF3:	ld	 d,l
	ld	 d,b
	djnz	 L9DFC
	nop	
	dec	 b
	nop	
	nop	
	nop	
L9DFC:	nop	
	ld	 b,l
	nop	
	ld	 bc,L0101
	ld	 d,l
	nop	
	ld	 bc,L4105
	ld	 b,l
	nop	
	ld	 bc,L4115
	ld	 d,l
	ld	 b,b
	ld	 bc,L4155
	ld	 b,a
	ld	 d,b
	ld	 bc,L4041
	ld	 d,a
	ret	 nc
	ld	 bc,L4001
	ld	 d,a
	ret	 nc
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,b
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,b
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,b
	nop	
	dec	 b
	ld	 d,c
	dec	 d
	djnz	 L9E32
L9E32:	dec	 b
	ld	 b,b
	ld	 de,$0000
	dec	 d
	ld	 d,b
	nop	
	nop	
	inc	 b
	ld	 d,l
	ld	 b,l
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 bc,L4051
	inc	 b
	inc	 d
L9E47:	nop	
	ld	 d,l
	ld	 d,b
	djnz	 L9E4C
L9E4C:	nop	
	dec	 d
	ld	 d,h
	djnz	 L9E51
L9E51:	nop	
	dec	 b
	ld	 b,h
	inc	 b
	nop	
	nop	
	ld	 bc,L0140
	ld	 bc,L4500
	ld	 b,b
	nop	
	ld	 b,l
	ld	 b,b
	ld	 b,c
	ld	 b,b
	nop	
	ld	 d,l
	ld	 b,b
	ld	 b,l
	ld	 d,b
	nop	
	ld	 d,c
	ld	 d,b
	ld	 b,c
	call	 nc,L4000
	ld	 d,b
	dec	 d
	call	 p,$0000
	ld	 d,h
	dec	 d
	call	 p,$0000
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 d,h
	ld	 b,l
	ld	 b,h
	nop	
	nop	
	ld	 d,b
	inc	 b
	ld	 b,b
	nop	
	ld	 b,b
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 d,c
	ld	 d,c
	dec	 b
	ld	 d,b
	nop	
	ld	 d,l
	ld	 b,b
	ld	 d,l
	ld	 d,h
	nop	
	ld	 b,l
	ld	 b,b
	ld	 d,b
	ld	 d,h
	ld	 bc,$0001
	nop	
	inc	 d
	inc	 b
	nop	
	nop	
	nop
	.db	$10

;*******************************************************************************
;
; Character: Burwor Facing Left
;	     (may be rotated in code ???)
;
; Colors: 0 = Black   2 = ? (Note Used)
;         1 = Blue    3 = Red
;
; Decoded by Adam Trionfo - 01/08/2017
;
;*******************************************************************************

L9EAE:  .DB      $00,$00,$00,$00,$40 ; . . . . . . . . . . . . . . . . 1 . . .
        .DB      $00,$00,$00,$01,$50 ; . . . . . . . . . . . . . . . 1 1 1 . .
        .DB      $00,$00,$00,$01,$50 ; . . . . . . . . . . . . . . . 1 1 1 . .
        .DB      $00,$00,$01,$01,$54 ; . . . . . . . . . . . 1 . . . 1 1 1 1 .
        .DB      $00,$15,$54,$01,$54 ; . . . . . 1 1 1 1 1 1 . . . . 1 1 1 1 .
        .DB      $00,$5F,$54,$41,$50 ; . . . . 1 1 3 3 1 1 1 . 1 . . 1 1 1 . .
        .DB      $15,$7F,$55,$01,$50 ; . 1 1 1 1 3 3 3 1 1 1 1 . . . 1 1 1 . .
        .DB      $15,$55,$55,$41,$40 ; . 1 1 1 1 1 1 1 1 1 1 1 1 . . 1 1 . . .
        .DB      $15,$55,$54,$01,$00 ; . 1 1 1 1 1 1 1 1 1 1 . . . . 1 . . . .
        .DB      $05,$50,$54,$04,$00 ; . . 1 1 1 1 . . 1 1 1 . . . 1 . . . . .
        .DB      $00,$00,$54,$10,$00 ; . . . . . . . . 1 1 1 . . 1 . . . . . .
        .DB      $01,$50,$15,$40,$14 ; . . . 1 1 1 . . . 1 1 1 1 . . . . 1 1 .
        .DB      $01,$54,$15,$51,$54 ; . . . 1 1 1 1 . . 1 1 1 1 1 . 1 1 1 1 .
        .DB      $00,$55,$55,$55,$50 ; . . . . 1 1 1 1 1 1 1 1 1 1 1 1 1 1 . .
        .DB      $00,$54,$55,$51,$50 ; . . . . 1 1 1 . 1 1 1 1 1 1 . 1 1 1 . .
        .DB      $00,$14,$15,$51,$40 ; . . . . . 1 1 . . 1 1 1 1 1 . 1 1 . . .
        .DB      $00,$05,$00,$01,$00 ; . . . . . . 1 1 . . . . . . . 1 . . . .
        .DB      $01,$55,$40,$05,$54 ; . . . 1 1 1 1 1 1 . . . . . 1 1 1 1 1 .

;*******************************************************************************	


	nop	
	nop	
	nop	
	nop	
	inc	 d
	nop	
	dec	 d
	ld	 d,l
	nop	
	ld	 d,b
	nop	
	ld	 e,a
	ld	 d,h
	ld	 bc,$1554
	ld	 a,a
	ld	 d,l
	ld	 b,c
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,l
	nop	
	ld	 d,h
	inc	 b
	ld	 b,l
	ld	 d,l
	ld	 b,c
	ld	 d,b
	dec	 d
	ld	 d,l
	ld	 d,h
	ld	 bc,$0540
	ld	 d,b
	ld	 d,l
	dec	 b
	nop	
	nop	
	nop	
	ld	 d,h
	inc	 b
	nop	
	nop	
	nop	
	ld	 d,l
	djnz	 L9F3A
L9F3A:	ld	 bc,L5555
	ld	 d,h
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,l
	nop	
	ld	 bc,L5550
	ld	 d,l
	ld	 b,b
	nop	
	ld	 d,b
	nop	
	dec	 d
	ld	 b,b
	nop	
	inc	 d
	nop	
	dec	 b
	nop	
	dec	 b
	ld	 d,l
	nop	
	ld	 bc,$0000
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	nop	
	nop	
	inc	 d
	nop	
	dec	 d
	ld	 d,l
	ld	 bc,L0050
	ld	 e,a
	ld	 d,h
	dec	 b
	ld	 d,h
	dec	 d
	ld	 a,a
	ld	 d,l
	ld	 b,l
	ld	 b,b
	dec	 d
	ld	 d,l
	ld	 d,l
	dec	 b
	nop	
	inc	 b
	ld	 b,l
	ld	 d,l
	ld	 b,l
	nop	
	nop	
	dec	 b
	ld	 d,h
	ld	 bc,$0540
	ld	 d,b
	ld	 d,l
	ld	 bc,L0040
	nop	
	ld	 d,h
	inc	 b
	nop	
	nop	
	ld	 bc,$1055
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,h
	nop	
	ld	 bc,L5555
	ld	 d,l
	ld	 b,b
	dec	 b
	ld	 d,b
	nop	
	dec	 b
	ld	 d,b
	ld	 bc,L0040
	ld	 bc,L0040
	ld	 d,b
	nop	
	dec	 b
	nop	
	ld	 bc,L0054
	dec	 d
	ld	 b,b
	inc	 b
	nop	
	nop	
	nop	
	djnz	 L9FC3
	nop	
	nop	
	nop	
	inc	 b
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	call	 z,$0000
	nop	
	nop	
L9FC3:	rlca	
	ld	 d,b
	nop	
	djnz	 L9FCD
	dec	 d
	ld	 d,b
	nop	
	djnz	 LA022
L9FCD:	rla	
	ld	 d,b
	nop	
	ld	 de,$1555
	ld	 d,h
	nop	
	dec	 d
	ld	 d,h
	rla	
	ld	 (hl),l
	nop	
	inc	 d
	djnz	 L9FE2
	ld	 a,l
	nop	
	djnz	 LA031
	dec	 b
L9FE2:	ld	 a,l
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	ld	 bc,L4155
	ld	 d,b
	ld	 b,b
	ld	 bc,L0155
	inc	 b
	nop	
	djnz	 LA00F
	ld	 b,b
	nop	
	nop	
	dec	 d
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,b
	ld	 de,$0554
	ld	 d,l
	ld	 d,h
	djnz	 LA063
	ld	 bc,L5055
	djnz	 LA018
	nop	
	dec	 d
	nop	
	jr	 nc,LA018
LA018:	call	 z,$0000
	nop	
	ret	 nz
	rrca	
	ld	 d,b
	nop	
	djnz	 LA027
LA022:	dec	 e
	ld	 d,b
	nop	
	djnz	 LA07C
LA027:	rra	
	ld	 d,b
	nop	
	ld	 de,L1D55
	ld	 d,h
	nop	
	dec	 d
	ld	 d,h
LA031:	dec	 e
	ld	 (hl),l
	nop	
	inc	 d
	djnz	 LA03C
	ld	 a,l
	nop	
	djnz	 LA08B
	dec	 b
LA03C:	ld	 a,l
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	ld	 bc,L4155
	ld	 d,b
	ld	 b,b
	ld	 bc,L0155
	inc	 b
	nop	
	djnz	 LA069
LA059:	ld	 b,b
	nop	
	nop	
	dec	 d
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,h
	ld	 de,$0554
	ld	 d,l
	ld	 d,b
	djnz	 LA0BD
	ld	 bc,L4055
	djnz	 LA072
	nop	
	dec	 d
	nop	
	nop	
	jr	 nz,LA0A3
	call	 z,L0100
	ld	 bc,LF002
	nop	
	ld	 bc,L4005
	or	 c
	ld	 b,b
	ld	 bc,L4115
LA082:	push	 af
	ld	 b,b
	ld	 bc,L4155
	.db	  $fd,$40
	ld	 sp,L4141
	push	 af
	ld	 d,b
	ld	 bc,L5101
	.db	  $fd,$d4
	nop	
	dec	 b
	ld	 d,b
	ld	 (hl),l
	call	 p,$050C
	ld	 d,l
	ld	 d,l
	call	 p,$0500
	ld	 d,l
LA0A0:	ld	 d,l
	ld	 d,h
	nop	
LA0A3:	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	dec	 b
	ld	 d,h
LA0AA:	ld	 b,l
	ld	 b,h
	nop	
	dec	 d
	ld	 d,b
	inc	 b
	ld	 b,b
	inc	 b
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	dec	 b
	ld	 d,l
	dec	 b
	ld	 d,l
	ld	 b,b
	inc	 b
	inc	 d
LA0BD:	ld	 bc,L5055
	djnz	 LA0C2
LA0C2:	nop	
	ld	 d,l
	ld	 d,h
	djnz	 LA0C7
LA0C7:	nop	
	dec	 d
	ld	 d,b
	nop	
	nop	
	nop	
	ret	 z
	nop	
	inc	 b
	nop	
	nop	
	ret	 p
	nop	
	ld	 bc,L0201
	ret	 p
	nop	
	nop	
	ld	 b,l
	ld	 b,b
	or	 c
	ld	 b,b
	ex	 af,af'
	ld	 d,l
	ld	 b,c
	push	 af
	ld	 b,b
	nop	
	ld	 d,c
	ld	 d,c
	.db	  $fd,$40
	nop	
	ld	 b,c
	ld	 d,c
	push	 af
	ld	 d,b
	nop	
	nop	
	ld	 d,c
	.db	  $fd,$d4
	jr	 nc,LA0F4
LA0F4:	ld	 d,b
	ld	 (hl),l
	call	 p,$0000
	ld	 d,l
	ld	 d,l
	call	 p,L0003
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	ld	 b,b
	ld	 d,h
	ld	 b,l
	ld	 b,h
	nop	
	ld	 d,c
	ld	 d,h
	inc	 b
	ld	 b,b
	nop	
	ld	 d,l
	ld	 b,l
	nop	
	nop	
	nop	
	ld	 b,l
	ld	 b,c
	ld	 d,l
	ld	 b,b
	ld	 bc,$0001
	ld	 d,l
	ld	 d,b
	inc	 b
	nop	
	nop	
	dec	 d
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 bc,L0050
	nop	
	nop	
	ld	 b,c
	ld	 d,b
	nop	
	dec	 b
	ld	 d,l
	ld	 bc,L0054
	rla	
	push	 de
	ld	 de,$0554
	ld	 e,a
	push	 de
	ld	 b,c
	ld	 d,h
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 b,c
	ld	 d,b
	dec	 c
	.db	 $dd,$55
	ld	 d,c
	ld	 d,b
	dec	 (hl)
	ld	 d,l
	ld	 d,l
	ld	 bc,L0140
	ld	 d,h
	dec	 d
	ld	 bc,L3000
	nop	
	dec	 d
	ld	 b,h
	nop	
	ld	 bc,$1550
	ld	 d,b
	inc	 d
	ld	 bc,$1554
	ld	 d,c
	ld	 d,h
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,b
	nop	
	ld	 d,h
	ld	 d,l
	ld	 d,c
	ld	 d,b
	nop	
	inc	 d
	dec	 d
	ld	 d,c
	ld	 b,b
	nop	
	dec	 b
	nop	
	ld	 bc,L0100
	ld	 d,l
	ld	 b,b
	dec	 b
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 bc,$0000
	nop	
	nop	
	ld	 bc,L0040
	nop	
	nop	
	ld	 b,c
	ld	 d,b
	nop	
	dec	 b
	ld	 d,l
	ld	 bc,L0054
	rla	
	push	 de
	ld	 de,$0554
	ld	 e,a
	push	 de
	ld	 b,c
	ld	 d,h
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 b,c
	ld	 d,b
	dec	 c
	push	 de
	ld	 d,l
	ld	 d,c
	ld	 d,b
	ccf	
	.db	 $fd,$55
	ld	 bc,L0140
	ld	 d,h
	dec	 d
	ld	 bc,L3000
	nop	
	dec	 d
	ld	 b,h
	nop	
	ld	 bc,$1550
	ld	 d,b
	inc	 d
	ld	 bc,$1554
	ld	 d,c
	ld	 d,h
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,b
	inc	 c
	ld	 d,h
	ld	 d,l
	ld	 d,c
	ld	 d,b
	nop	
	inc	 d
	dec	 d
	ld	 d,c
	ld	 b,b
	nop	
	dec	 b
	nop	
	ld	 bc,L3100
	ld	 d,l
	ld	 b,b
	dec	 b
	ld	 d,h
	nop	
	ld	 bc,L4055
	djnz	 LA1DE
LA1DE:	dec	 b
	push	 af
	nop	
	ld	 d,h
	ld	 bc,LF557
	ld	 d,c
	ld	 d,h
	ld	 bc,L5555
	ld	 b,c
	ld	 d,h
	jr	 nc,LA265
	ld	 d,l
	ld	 d,c
	ld	 d,h
	rrca	
	rst	 38H
	push	 de
	ld	 bc,L3E54
	rst	 38H
	ld	 d,l
	ld	 b,c
	ld	 d,b
	ex	 af,af'
	ld	 d,l
	dec	 d
	ld	 bc,L0040
	nop	
	dec	 d
	ld	 b,l
	nop	
	jr	 nc,LA208
	ld	 d,l
LA208:	ld	 d,h
	nop	
	ld	 bc,L5555
	ld	 d,h
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,l
	nop	
	ld	 bc,L5550
	ld	 d,l
	ld	 b,b
	jr	 nz,LA26B
	nop	
	dec	 d
	ld	 b,b
	nop	
	inc	 d
	nop	
	dec	 b
	nop	
	dec	 b
	ld	 d,l
	nop	
	ld	 bc,$0000
	nop	
	jr	 nc,LA231
	ld	 b,b
	nop	
	inc	 c
	nop	
	nop	
LA231:	inc	 d
	nop	
	nop	
	ld	 d,l
	ld	 d,b
	inc	 b
	nop	
	ld	 bc,L407D
	inc	 d
	nop	
	ld	 d,l
	.db	 $fd,$54
	ld	 d,h
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,b
	ld	 d,h
	jr	 nz,LA265
	push	 de
	ld	 d,h
	ld	 d,h
	rrca	
	rst	 38H
	push	 af
	ld	 b,b
	ld	 d,h
	ccf	
	cp	 a
	push	 de
	ld	 d,b
	ld	 d,b
	ld	 (bc),a
	dec	 d
	ld	 b,l
	ld	 b,c
	ld	 b,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,h
	nop	
	nop	
LA265:	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 b,b
	ld	 bc,L0055
	dec	 b
	ld	 d,b
	nop	
	ld	 d,b
	nop	
	ld	 bc,L0040
	inc	 d
	nop	
	dec	 b
	nop	
	nop	
	ld	 d,l
	nop	
	dec	 d
	ld	 b,b
	ld	 bc,L0300
	nop	
	djnz	 LA287
	jr	 nz,LA285
LA285:	nop	
	inc	 b
LA287:	nop	
	nop	
	jr	 nc,LA28B
LA28B:	nop	
	nop	
	nop	
	nop	
	ld	 a,(de)
	and	 b
	nop	
	nop	
	dec	 b
	ld	 e,d
	and	 b
	djnz	 LA298
LA298:	ld	 (de),a
	ld	 l,d
	add	 a,b
	djnz	 LA29D
LA29D:	ld	 b,c
	xor	 d
	nop	
	dec	 d
	ld	 b,b
	add	 hl,bc
	xor	 b
	nop	
	nop	
	djnz	 LA2AE
	adc	 a,(hl)
	inc	 b
	nop	
	djnz	 LA2C7
	cpl	
LA2AE:	add	 a,h
	nop	
	ld	 d,l
	ld	 d,h
	ccf	
	ret	 nc
	ld	 bc,L5555
	xor	 a
	add	 a,b
	dec	 b
LA2BA:	ld	 d,l
	ld	 d,(hl)
	xor	 d
	add	 a,b
	ld	 bc,L5555
	xor	 a
	add	 a,b
	nop	
	ld	 d,l
	ld	 d,h
	ccf	
LA2C7:	ret	 nc
	nop	
	djnz	 LA2E5
	cpl	
	add	 a,h
	nop	
	djnz	 LA2F6
	xor	 (hl)
	inc	 b
	dec	 d
	ld	 b,b
	add	 hl,bc
	xor	 d
	nop	
	djnz	 LA2D9
LA2D9:	ld	 bc,LA0AA
	djnz	 LA2DE
LA2DE:	ld	 b,b
	ld	 l,d
	xor	 b
	nop	
	nop	
	dec	 d
	ld	 e,d
LA2E5:	xor	 b
	nop	
	nop	
	ld	 hl,(L00A4)
	nop	
	nop	
	and	 l
	ld	 d,(hl)
	nop	
	nop	
	ld	 (bc),a
	sbc	 a,d
	ld	 l,b
	nop	
	djnz	 LA349
	ld	 l,c
	and	 b
LA2F9:	nop	
	ld	 de,LA612
	adc	 a,(hl)
	nop	
	inc	 d
	djnz	 LA29C
	cpl	
	add	 a,b
	nop	
	ld	 d,l
	ld	 d,h
	ccf	
	call	 nc,L5501
	ld	 d,l
	xor	 a
	add	 a,b
	dec	 b
	ld	 d,l
	ld	 d,(hl)
	xor	 d
	add	 a,b
	ld	 bc,L5555
	xor	 a
LA317:	add	 a,b
	nop	
	ld	 d,l
	ld	 d,h
	ccf	
	ret	 nc
	nop	
	djnz	 LA2BA
	cpl	
	add	 a,h
	nop	
	ld	 b,d
	and	 (hl)
	adc	 a,(hl)
	nop	
	ld	 bc,LA902
	add	 a,b
	nop	
	inc	 d
	ld	 (bc),a
	ld	 l,c
	and	 b
	nop	
	djnz	 LA335
	sub	 l
	xor	 b
LA335:	nop	
	djnz	 LA338
LA338:	and	 l
	ld	 l,b
	nop	
	nop	
	nop	
	ld	 hl,(L0068)
	ld	 (bc),a
	xor	 b
	inc	 b
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	sub	 h
	nop	
LA349:	nop	
	nop	
	xor	 d
	ld	 h,h
	nop	
	nop	
	nop	
	add	 hl,hl
	and	 (hl)
	nop	
	nop	
	djnz	 LA3B0
	and	 (hl)
	nop	
	nop	
	ld	 de,LA612
	jr	 c,LA35E
LA35E:	inc	 d
	djnz	 LA2F9
	cp	 (hl)
	nop	
	djnz	 LA3BA
	ld	 d,b
	rst	 38H
	ld	 b,b
	ld	 bc,L5655
	cp	 (hl)
	djnz	 LA373
	ld	 d,l
	ld	 e,d
	xor	 d
	nop	
	ld	 bc,L5655
	cp	 (hl)
	nop	
	djnz	 LA3CE
	ld	 d,b
	rst	 38H
	ld	 b,b
	inc	 d
	djnz	 LA317
	cp	 (hl)
	djnz	 LA393
LA382:	ld	 de,L38A6
	nop	
	djnz	 LA3E2
	ld	 h,(hl)
	nop	
	nop	
	nop	
	xor	 d
	sub	 (hl)
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	sub	 h
LA393:	nop	
	nop	
	ld	 (bc),a
	xor	 d
	add	 a,h
	nop	
	nop	
;
; At $A39A starts pattern of Worluk (demo screen) ???
;
LA39A:	nop	
	dec	 b
	nop	
	inc	 d
	jr	 z,LA3C8
	nop	
	ld	 b,b
	ld	 b,b
	xor	 b
	ld	 hl,(LEA02)
	ret	 po
	xor	 b
	ld	 hl,(LFB8B)
	jp	 m,L2AA8
	xor	 a
LA3B0:	ei	
	cp	 $A8
	ld	 d,$A2
	jp	 pe,LA4EA
	dec	 b
	xor	 b
LA3BA:	ld	 hl,(L940A)
	ld	 b,$5A
	add	 hl,de
	add	 hl,hl
	ld	 b,h
	inc	 b
	ld	 h,$55
	ld	 h,(hl)
	inc	 b
	ld	 bc,L5501
	ld	 e,b
	inc	 b
	nop	
	ld	 b,b
LA3CE:	ld	 d,l
	ld	 b,b
	djnz	 LA3D2
LA3D2:	nop	
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	djnz	 LA438
	ld	 b,c
	nop	
	nop	
	djnz	 LA3FD
	ld	 bc,$0000
	djnz	 LA3F1
	ld	 bc,L0100
	ld	 d,b
LA3F1:	nop	
	ld	 bc,L0050
	ld	 bc,L4000
	nop	
	nop	
	ld	 bc,$0001
LA3FD:	nop	
	nop	
	dec	 bc
	xor	 e
	add	 a,b
	nop	
	ex	 af,af'
	cpl	
	rst	 28H
	ret	 po
	nop	
	ld	 d,$3F
	rst	 28H
	ret	 p
	xor	 b
	ld	 h,$8B
	xor	 e
	add	 a,d
	xor	 b
	dec	 h
	and	 b
	xor	 b
	ld	 hl,(L2694)
	ld	 l,b
	ld	 h,h
	and	 l
	ld	 e,b
	ld	 h,$99
	ld	 d,l
	sbc	 a,d
	ld	 e,b
	add	 hl,hl
	and	 l
	ld	 d,l
	ld	 l,d
	ld	 l,b
	ld	 a,(bc)
	ld	 l,c
	ld	 d,l
	xor	 c
	and	 b
	ld	 (bc),a
	and	 c
	ld	 d,l
	ld	 hl,(L0080)
	ld	 bc,L0055
	nop	
	nop	
	ld	 d,l
	ld	 d,l
LA438:	ld	 b,b
	nop	
	nop	
	ld	 b,c
	ld	 d,l
	djnz	 LA43F
LA43F:	nop	
	djnz	 LA496
	inc	 b
	nop	
	nop	
	inc	 b
	djnz	 LA449
	nop	
LA449:	nop	
	ld	 d,h
	nop	
	ld	 bc,L0050
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	djnz	 LA467
	nop	
	nop	
	nop	
	ld	 b,b
	ld	 b,b
	nop	
	nop	
	ld	 (bc),a
	jp	 pe,L00E0
	nop	
	dec	 bc
	ei	
	ret	 m
	nop	
LA467:	nop	
	rrca	
	ei	
	call	 m,$0000
	ld	 (bc),a
	jp	 pe,L00E0
	nop	
	xor	 b
	ld	 hl,(L800A)
	dec	 d
	ld	 d,(hl)
	add	 hl,de
	dec	 h
	ld	 d,h
	ld	 b,$A9
	ld	 d,l
	ld	 e,d
	ld	 d,b
	add	 hl,bc
	xor	 d
	ld	 d,l
	ld	 l,c
	xor	 b
	ld	 a,(bc)
	ld	 l,b
	ld	 d,l
	ld	 b,(hl)
	xor	 b
	ld	 hl,(L55A0)
	ld	 b,d
	xor	 b
	ld	 hl,(L5595)
	ld	 d,l
	xor	 b
	ld	 hl,(L5510)
	ld	 b,c
	xor	 b
	jr	 z,LA49F
	dec	 d
	inc	 b
	jr	 z,LA49F
LA49F:	ld	 bc,L1004
	nop	
	nop	
	dec	 d
	ld	 b,b
	ld	 d,l
	nop	
	nop	
	nop	
	jr	 nc,LA4AC
LA4AC:	nop	
	nop	
	nop	
	jr	 nc,LA4B1
LA4B1:	nop	
	nop	
	nop	
	inc	 b
	nop	
	nop	
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	inc	 d
	nop	
	ld	 d,b
	nop	
	ld	 de,L0054
	dec	 d
	ld	 b,b
	dec	 d
	ld	 d,l
	nop	
	ld	 d,$10
	dec	 d
	ld	 d,l
	ld	 b,b
	ld	 d,b
	djnz	 LA4E6
	ld	 d,l
	ld	 d,l
	ld	 b,d
	inc	 d
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,h
	inc	 d
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	inc	 b
	dec	 d
	ld	 d,l
LA4E6:	ld	 d,h
	dec	 d
	nop	
	dec	 d
LA4EA:	ld	 d,h
	nop	
	inc	 d
	nop	
	dec	 d
	ld	 b,b
	nop	
	djnz	 LA4F3
LA4F3:	dec	 d
	nop	
	nop	
	ld	 d,b
	nop	
	inc	 d
	nop	
	ld	 bc,L0040
	nop	
	nop	
	dec	 a
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	jr	 nc,LA50B
LA50B:	nop	
	nop	
	nop	
	jr	 nc,LA510
LA510:	nop	
	nop	
	nop	
	inc	 b
	nop	
	nop	
	nop	
	inc	 b
	dec	 b
	ld	 de,$1050
	dec	 d
	nop	
	ld	 d,l
	add	 a,h
	djnz	 LA577
	ld	 b,b
	inc	 d
	inc	 b
	dec	 d
	ld	 d,l
	ld	 d,b
	ld	 d,b
	sub	 h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,b
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,l
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	dec	 b
	nop	
	dec	 b
	ld	 d,h
	ld	 d,h
	inc	 d
	nop	
	dec	 b
	ld	 d,b
	nop	
	ld	 d,b
	nop	
	dec	 b
	ld	 b,b
	ld	 bc,L0040
	ld	 bc,L3D00
	nop	
	nop	
	nop	
	nop	
	nop	
LA555:	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	ret	 nz
	nop	
	nop	
	nop	
	nop	
	ret	 nz
	dec	 b
	ld	 b,b
	inc	 d
	inc	 d
LA577:	djnz	 LA5CF
	djnz	 LA590
	ld	 d,l
	inc	 d
	ld	 d,b
	ld	 d,b
	dec	 d
	ld	 d,l
	ld	 b,c
	ld	 b,c
	ld	 d,b
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 d,l
LA590:	ld	 d,l
	ld	 d,h
	inc	 b
	dec	 d
	ld	 d,l
	ld	 d,l
	inc	 d
	nop	
	inc	 d
	dec	 d
	call	 nc,L0050
	nop	
	ld	 bc,L40F5
	nop	
	nop	
	nop	
	dec	 b
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	nop	
	jr	 nc,LA5C4
LA5C4:	nop	
	nop	
	nop	
	jr	 nc,LA5C9
LA5C9:	nop	
	inc	 b
	dec	 b
	inc	 b
	nop	
	nop	
LA5CF:	inc	 b
	ld	 d,l
	ld	 b,l
	ld	 bc,$0550
	ld	 d,l
	ld	 b,c
	ld	 d,l
	add	 a,h
	dec	 b
	ld	 d,l
	ld	 d,b
	ld	 d,h
	inc	 b
	dec	 d
	ld	 d,l
	ld	 d,c
	ld	 d,b
	sub	 h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,b
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	nop	
	dec	 d
	ld	 bc,$1454
	nop	
	nop	
	nop	
	nop	
	inc	 d
	nop	
	nop	
	nop	
	nop	
	ld	 d,b
	nop	
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	nop	
	call	 p,$0000
LA60B:	nop	
	nop	
	nop	
	nop	
	nop	
;
; At $A610 starts pattern of Wizard of Wor (demo screen) ???
;
LA610:	nop	
	nop	
LA612:	dec	 d
	ld	 b,b
	nop	
	nop	
	ld	 bc,L0055
	nop	
	nop	
	inc	 b
	ld	 bc,$0000
	nop	
	ld	 b,$21
	ld	 d,b
	nop	
	nop	
	dec	 b
	dec	 b
	ld	 d,h
	nop	
	nop	
	dec	 d
	ld	 b,l
	ld	 d,l
	ld	 b,b
	nop	
	ld	 d,b
	ld	 d,l
	ld	 b,b
	ld	 d,b
	nop	
	ld	 b,b
	dec	 d
	ld	 b,b
	inc	 d
	ld	 bc,L1540
	ld	 d,b
	inc	 c
	inc	 a
	nop	
	dec	 d
	ld	 d,b
	inc	 c
	nop	
	nop	
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	ld	 bc,L5055
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 b,b
	nop	
	ld	 bc,L5555
	ld	 d,b
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,b
	nop	
	dec	 b
	ld	 d,b
	nop	
	nop	
	nop	
	djnz	 LA6C6
	nop	
	nop	
	nop	
	jr	 LA60B
	nop	
	nop	
	nop	
	inc	 d
	dec	 d
	ld	 b,b
	nop	
	nop	
	dec	 b
	dec	 d
	ld	 d,b
	nop	
	nop	
	dec	 d
	ld	 d,l
	inc	 d
	nop	
	nop	
	inc	 b
	ld	 d,l
	dec	 b
	nop	
	nop	
	djnz	 LA6A5
	ld	 b,c
	ld	 b,b
	nop	
	ld	 d,b
	dec	 d
	ld	 d,b
	ret	 nz
	rrca	
	nop	
	ld	 d,l
	ld	 d,b
	ret	 nz
	nop	
	ld	 bc,L5055
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 b,b
LA6A5:	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 bc,L5555
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	nop	
	nop	
	dec	 b
	ld	 d,h
	nop	
	nop	
	nop	
	nop	
LA6C6:	dec	 d
	nop	
	nop	
	nop	
	dec	 b
	ld	 d,h
	nop	
	nop	
	nop	
	ld	 de,L0054
	nop	
	nop	
	jr	 LA72A
	nop	
	nop	
	nop	
	inc	 d
	dec	 d
	ld	 b,b
	nop	
	nop	
	dec	 b
	dec	 d
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	inc	 d
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 b,l
	nop	
	nop	
	ld	 bc,L5515
	nop	
	nop	
	dec	 b
	dec	 d
	ld	 e,h
	nop	
	nop	
	ret	 p
	ld	 d,l
	ld	 a,h
	nop	
	nop	
	ld	 bc,L5455
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	ld	 bc,L4055
	nop	
	nop	
	ld	 bc,L4055
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	ld	 bc,L0054
	nop	
	nop	
	inc	 b
	dec	 d
	nop	
	nop	
	nop	
	ld	 b,$25
	nop	
	nop	
	nop	
	dec	 b
	dec	 b
	ld	 b,b
	nop	
	nop	
	ld	 bc,L5445
	nop	
	nop	
	ld	 bc,L5555
	nop	
	nop	
	ld	 bc,L4155
	ld	 b,b
	nop	
	dec	 b
	dec	 d
	ld	 b,b
	ld	 b,b
	nop	
	inc	 d
	dec	 b
	ld	 d,b
	ld	 d,b
	inc	 bc
	ret	 nz
	ld	 d,l
	ld	 d,b
	jr	 nc,LA751
LA751:	dec	 b
	ld	 d,l
	ld	 d,b
	jr	 nc,LA756
LA756:	dec	 d
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	ld	 bc,L5055
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	nop	
	dec	 d
	ld	 d,b
	nop	
	nop	
	nop	
	nop	
	inc	 bc
	nop	
	nop	
	nop	
	nop	
	inc	 bc
	nop	
	djnz	 LA784
LA784:	nop	
	inc	 bc
	ret	 nz
	inc	 d
	nop	
	nop	
	ld	 e,a
	nop	
	dec	 d
	ld	 d,b
	ld	 bc,L0054
	dec	 d
	ld	 d,h
	ld	 bc,L0040
	dec	 d
	ld	 d,l
	ld	 bc,L0050
	dec	 d
	ld	 d,l
	ld	 b,c
	ld	 d,c
	ld	 d,b
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	add	 a,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,b
	inc	 b
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 b,b
	add	 a,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 d,l
	nop	
	ld	 d,b
	ld	 d,b
	dec	 d
	ld	 b,b
	ld	 bc,L0050
	djnz	 LA7C0
LA7C0:	dec	 b
	ld	 c,h
	nop	
	nop	
	nop	
	dec	 b
	call	 m,$0000
	nop	
	ld	 bc,L007C
	nop	
	nop	
	nop	
	ld	 e,h
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	nop	
	nop	
	nop	
	inc	 c
	nop	
	djnz	 LA7E3
LA7E3:	nop	
	inc	 a
	nop	
	djnz	 LA7E8
LA7E8:	inc	 bc
	ret	 p
	nop	
	dec	 d
	nop	
	inc	 bc
	ret	 nz
	nop	
	dec	 d
	ld	 d,h
	nop	
	inc	 d
	nop	
	dec	 d
	ld	 d,l
	nop	
	inc	 d
	nop	
	dec	 d
	ld	 d,l
	ld	 b,b
	inc	 d
	nop	
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,b
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,h
	sub	 h
	dec	 d
	ld	 d,l
	ld	 d,l
	ret	 nz
	inc	 b
	dec	 d
	dec	 b
	ld	 b,l
	ret	 nc
	sub	 h
	inc	 d
	nop	
	ld	 bc,L50D5
	djnz	 LA81A
LA81A:	nop	
	pop	 de
	ld	 d,b
	nop	
	nop	
	inc	 bc
	ret	 nc
	ld	 b,b
	nop	
	nop	
	rrca	
	ld	 b,b
	nop	
	nop	
	nop	
	rrca	
	nop	
	nop	
	nop	
	nop	
	nop	
	ld	 ($0000),hl
	nop	
	nop	
	ld	 ($0000),hl
	nop	
	nop	
	jr	 nz,LA83B
LA83B:	nop	
	nop	
	nop	
	inc	 c
	ret	 nz
	nop	
	nop	
	nop	
	inc	 a
	ret	 nz
	djnz	 LA847
LA847:	nop	
	di	
	ret	 nz
	djnz	 LA84C
LA84C:	ld	 bc,L4051
	dec	 d
	nop	
	nop	
	ld	 d,c
	ld	 b,b
	dec	 d
	ld	 d,h
	nop	
	dec	 d
	nop	
	dec	 d
	ld	 d,l
	nop	
	dec	 d
	nop	
	dec	 d
	ld	 d,l
	ld	 b,b
	inc	 d
	nop	
	dec	 d
	ld	 d,l
	ld	 b,b
	inc	 d
	djnz	 LA87E
	ld	 d,l
	ld	 d,b
	ld	 d,h
	sub	 h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,h
	ld	 d,h
	dec	 d
	ld	 d,l
	ld	 d,l
	ld	 d,l
	ld	 d,h
	dec	 d
	ld	 bc,L5555
	ld	 d,h
	inc	 d
	nop	
LA87E:	dec	 b
	ld	 d,c
	ld	 d,b
	djnz	 LA883
LA883:	nop	
	nop	
	ld	 b,b
	nop	
	nop	
	dec	 d
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 b,b
	ld	 d,b
	nop	
	inc	 bc
	nop	
	ld	 h,d
	ld	 d,b
	nop	
	ccf	
	ret	 nz
	ld	 d,b
	ld	 b,b
	nop	
	nop	
	ret	 nc
	djnz	 LA8E1
	call	 m,L5100
	ld	 d,h
	ld	 d,h
	call	 p,L5500
	ld	 d,l
	ld	 d,l
	call	 nc,$1500
	ld	 d,l
	ld	 b,l
	ld	 d,b
	nop	
	nop	
	dec	 d
	ld	 b,c
	ld	 b,b
	nop	
	nop	
	dec	 d
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	ld	 bc,L5055
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	dec	 d
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	ld	 bc,L5555
	ld	 d,l
	nop	
	nop	
LA8E1:	nop	
	ld	 bc,L0050
	nop	
	nop	
	dec	 b
	dec	 d
	nop	
	nop	
	nop	
	ld	 b,$25
	ld	 b,b
	nop	
	nop	
	inc	 b
	dec	 b
	nop	
	ccf	
	pop	 bc
	ld	 d,l
	inc	 b
	nop	
	nop	
	pop	 af
	ld	 d,l
	dec	 d
	ld	 b,b
	nop	
	inc	 a
	dec	 b
	rst	 38H
LA902:	ret	 nc
	nop	
	inc	 a
	dec	 b
	ld	 d,h
	call	 m,$0000
	dec	 b
	ld	 d,b
	inc	 a
	nop	
	nop	
	dec	 b
	ld	 b,b
	nop	
	nop	
	nop	
	dec	 d
	ld	 d,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	ld	 bc,L5055
	nop	
	nop	
	ld	 bc,L4055
	nop	
	nop	
	ld	 bc,L4055
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,b
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,l
	nop	
	nop	
	nop	
	nop	
	dec	 d
	ld	 b,b
	nop	
	nop	
	nop	
	ld	 d,l
	ld	 d,b
	nop	
	.db	  $fd,$40
	dec	 h
	ld	 d,h
	jr	 z,LA958
	ld	 d,h
	ld	 bc,L0050
	ret	 p
	dec	 d
	ld	 d,l
	ld	 b,b
	ld	 hl,(L553D)
	ld	 d,l
	ld	 d,b
LA958:	nop	
	dec	 c
	ld	 b,b
	dec	 d
	ld	 d,b
	nop	
	ld	 bc,$0500
	ld	 d,b
	nop	
	nop	
	nop	
	dec	 b
	ld	 d,b
	nop	
	nop	
	nop	
	dec	 d
	ld	 b,b
	nop	
	nop	
	ld	 bc,L4055
	nop	
	nop	
	dec	 b
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	dec	 d
	ld	 d,l
	nop	
	nop	
	nop	
	dec	 d
	ld	 d,l
	nop	
	nop	
	nop	
	dec	 d
	ld	 d,l
	nop	
	nop	
LA986:	nop	
	ld	 d,l
	ld	 d,l
	ld	 b,b
	nop	
	nop	
	ld	 d,l
	ld	 d,l
	ld	 d,b
	nop	
	dec	 b
	ld	 d,l
	ld	 d,l
	ld	 d,h
	nop	
	nop	
	ex	 af,af'
	djnz	 LA999
LA999:	nop	
	nop	
	jp	 $0000
	nop	
	nop	
	inc	 bc
	jr	 z,LA9A3
LA9A3:	nop	
	nop	
	cpl	
	ret	 pe
	nop	
	nop	
	nop	
	cpl	
	xor	 d
	nop	
	nop	
	jr	 nz,LA9DB
	jp	 pe,$0000
	ex	 af,af'
	cpl	
	xor	 d
	add	 a,b
	nop	
	ex	 af,af'
	dec	 hl
	jp	 pe,L20A0
	ld	 a,(bc)
	ld	 a,(bc)
	and	 h
	and	 b
	jr	 nz,LA9ED
	and	 d
	and	 b
	and	 b
	ld	 hl,(LAAAA)
	xor	 d
	and	 b
	ld	 a,(bc)
	xor	 d
	xor	 d
	xor	 d
	and	 b
	ld	 (bc),a
	xor	 d
	xor	 d
	xor	 d
	add	 a,b
	nop	
	xor	 d
	xor	 d
	xor	 d
	nop	
	jr	 nz,LA986
	xor	 d
	xor	 b
	inc	 c
	ld	 hl,(LAA2A)
	and	 b
	inc	 a
	nop	
	ld	 hl,(L00AA)
	ret	 p
	nop	
	ld	 (bc),a
	and	 b
	xor	 d
LA9ED:	ret	 p
	nop	
	nop	
	ld	 l,$4C
	nop	
	nop	
	nop	
	ld	 l,d
	add	 a,b
	nop	
	nop	
	nop	
	rlca	
	ld	 a,(bc)
	nop	
	nop	
	nop	
	inc	 bc
LAA00:	ld	 a,(bc)
	nop	
	nop	
	nop	
	cpl	
	jp	 pe,L0080
	nop	
	cpl	
	jp	 m,L0080
	jr	 nz,LAA3E
	jp	 pe,L00A0
	ex	 af,af'
	dec	 hl
	jp	 m,L00A8
	ld	 (bc),a
	xor	 e
	jp	 (hl)
	jr	 z,LAA1C
LAA1C:	ld	 hl,(LF8AA)
	jr	 z,LAA41
	xor	 d
	xor	 d
	xor	 d
	xor	 b
	ld	 (LAAAA),hl
	xor	 d
	xor	 b
LAA2A:	ld	 hl,(LAAAA)
	xor	 d
	add	 a,b
	nop	
	xor	 d
	xor	 d
	xor	 d
	nop	
	nop	
	xor	 d
	xor	 d
	and	 b
	ret	 p
	ld	 (bc),a
	ld	 hl,(L0BAA)
	call	 m,L2A02
	and	 b
LAA41:	inc	 hl
	ret	 p
	ld	 a,(bc)
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
	nop	
	nop	
	jp	 po,LC058
	nop	
	inc	 bc
	dec	 bc
	ld	 b,b
	nop	
	nop	
	ld	 a,(bc)
	rlca	
	ld	 a,(bc)
	nop	
	nop	
	ld	 b,$03
	jp	 z,$0000
	nop	
	cpl	
	jp	 pe,L0080
	nop	
	cpl	
	jp	 m,L0080
	jr	 nz,LAA98
	jp	 pe,L00A0
	ex	 af,af'
	dec	 hl
	jp	 m,L00A8
	ld	 (bc),a
	xor	 e
	jp	 (hl)
	jr	 z,LAA76
LAA76:	ld	 hl,(LF8AA)
	jr	 z,LAA9B
	xor	 d
	xor	 d
	xor	 d
	xor	 b
	ld	 (LAAAA),hl
	xor	 d
	xor	 b
	ld	 hl,(LAAAA)
	xor	 d
	add	 a,b
	nop	
	xor	 d
	xor	 d
	xor	 d
	nop	
	nop	
	xor	 d
	xor	 d
	and	 e
	ret	 p
	ld	 (bc),a
	ld	 hl,(L0BAA)
	call	 m,L2A02
	and	 b
LAA9B:	jr	 nz,LAA99
	ld	 a,(bc)
	ld	 a,(bc)
	xor	 d
	add	 a,b
	inc	 c
	nop	
	nop	
	ret	 z
	inc	 b
	jr	 nc,LAAA8
LAAA8:	nop	
	nop	
LAAAA:	ret	 nz
	nop	
	nop	
	nop	
	inc	 de
	jp	 z,$0000
	inc	 b
LAAB3:	inc	 bc
	jp	 z,$0000
	nop	
	cpl	
	jp	 pe,L0080
	nop	
	cpl	
	jp	 m,L0080
	ld	 (bc),a
	cpl	
	jp	 pe,L02A0
	ld	 (bc),a
	dec	 hl
	jp	 m,L0AA8
	adc	 a,d
	xor	 e
	jp	 (hl)
	jr	 z,LAAF8
	xor	 d
LAAD1:	xor	 d
	ret	 m
	jr	 z,LAAD5
LAAD5:	xor	 d
	xor	 d
	xor	 d
	xor	 b
	ld	 (LAAAA),hl
	xor	 d
	xor	 b
	ld	 hl,(LAAAA)
	xor	 d
	add	 a,b
	nop	
	xor	 d
	xor	 d
	xor	 d
	nop	
	nop	
	ld	 hl,(LA0AA)
	inc	 c
	nop	
	ld	 hl,(L00AA)
	inc	 a
	nop	
	ld	 a,(bc)
	add	 a,b
	nop	
	ret	 p
	nop	
LAAF8:	ld	 (bc),a
	xor	 d
	xor	 d
	ret	 p
	nop	
	nop	
	nop	
	inc	 bc
	ret	 nz
	nop	
	nop	
	xor	 d
	add	 a,b
	call	 m,L0200
	xor	 d
	and	 b
	inc	 a
	nop	
	ld	 hl,(LA882)
	ex	 af,af'
	ld	 (bc),a
	xor	 d
	sub	 d
	xor	 d
	ex	 af,af'
	ld	 (de),a
	xor	 d
	xor	 d
	xor	 d
	adc	 a,b
	nop	
	xor	 $EA
	xor	 d
	adc	 a,b
	rrca	
	rst	 38H
LAB21:	jp	 pe,LA0AA
	jr	 nz,LAB21
	and	 d
	xor	 d
	and	 b
	nop	
	xor	 d
	adc	 a,d
	xor	 d
	xor	 b
	inc	 c
	nop	
	ld	 a,(bc)
	xor	 d
	xor	 b
	nop	
	nop	
	ld	 hl,(LA8AA)
	nop	
	ld	 (bc),a
	xor	 d
	xor	 d
	and	 b
	nop	
	ex	 af,af'
	ld	 a,(bc)
	xor	 d
	and	 b
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	nop	
	nop	
	ld	 (bc),a
	and	 b
	add	 a,b
	nop	
	nop	
	ld	 (bc),a
	add	 a,b
	add	 a,b
	nop	
	nop	
	ld	 hl,(L8002)
	nop	
	nop	
	xor	 d
	add	 a,b
	ret	 nz
	nop	
	ld	 (bc),a
	xor	 d
	add	 a,e
	ret	 p
	nop	
	ld	 hl,(LA382)
	ret	 p
	ld	 (bc),a
	xor	 d
	sub	 d
	xor	 b
	ret	 p
	ld	 (LAAAA),a
	xor	 b
	add	 a,b
	nop	
	ld	 l,$EE
	xor	 d
	jr	 nz,LAB8D
	ccf	
	cp	 $AA
	ex	 af,af'
	dec	 hl
	rst	 38H
	jp	 m,L88AA
	add	 hl,sp
	ccf	
	xor	 d
	xor	 d
	adc	 a,b
	jr	 z,LABAF
	xor	 d
	xor	 d
	xor	 b
	inc	 b
	nop	
	ld	 hl,(LA8AA)
LAB8D:	nop	
	nop	
	ld	 hl,(LA8AA)
	nop	
	nop	
	adc	 a,d
	xor	 d
	xor	 b
	nop	
	ld	 (bc),a
	ld	 a,(bc)
	xor	 d
	and	 b
	nop	
	nop	
	ld	 (bc),a
	xor	 d
	nop	
	nop	
	nop	
	nop	
	and	 b
	xor	 b
	nop	
	nop	
	nop	
	jr	 nz,LABB3
	nop	
	nop	
	ld	 (bc),a
	and	 b
LABAF:	nop	
	nop	
	nop	
	xor	 d
LABB3:	add	 a,b
	call	 m,L0200
	xor	 d
	add	 a,e
	ret	 p
	jr	 nc,LABE6
	add	 a,d
	and	 e
	ret	 p
	ld	 (bc),a
	xor	 d
	sub	 d
	xor	 e
	ret	 nz
	ld	 (LAAAA),hl
	xor	 b
	add	 a,b
	djnz	 LABF9
	xor	 $AA
	jr	 nz,LABE3
	rst	 38H
	cp	 $AA
	ex	 af,af'
	cpl	
	rst	 38H
	jp	 m,L88AA
	add	 hl,bc
	ccf	
	xor	 d
	xor	 d
	adc	 a,b
	inc	 l
	ld	 hl,(LAAAA)
	xor	 b
	inc	 a
LABE3:	nop	
	ld	 hl,(LA8AA)
	ld	 c,$80
	ld	 hl,(LA8AA)
	ld	 (bc),a
	ld	 b,b
	adc	 a,d
	xor	 d
	xor	 b
	nop	
	ld	 (bc),a
	ld	 a,(bc)
	xor	 d
	and	 b
	nop	
	nop	
	ld	 (bc),a
LABF9:	xor	 d
	nop	
	nop	
	nop	
	nop	
	and	 b
	xor	 b
	nop	
	nop	
	nop	
	jr	 nz,LAC0D
	nop	
	nop	
	ld	 (bc),a
	and	 b
	nop	
	nop	
	nop	
	xor	 d
LAC0D:	add	 a,e
	ret	 nz
	jr	 nc,LAC13
	xor	 d
	add	 a,b
LAC13:	call	 m,L2A00
	add	 a,d
	and	 b
	inc	 a
	ld	 (bc),a
	xor	 d
	sub	 d
	xor	 b
	ex	 af,af'
	ld	 (de),a
	xor	 d
	xor	 d
	xor	 b
	ex	 af,af'
	nop	
	ld	 l,$EE
	xor	 d
	ex	 af,af'
	rrca	
	rst	 38H
	cp	 $AA
	ex	 af,af'
	inc	 bc
	rst	 38H
	jp	 m,L88AA
	jr	 nz,LAC73
	xor	 d
	xor	 d
	adc	 a,b
	ld	 bc,LAA2A
	xor	 d
	adc	 a,b
	jr	 nc,LAC3E
LAC3E:	ld	 hl,(LA8AA)
	nop	
	ld	 (bc),a
	xor	 d
	xor	 d
	xor	 b
	nop	
	ld	 b,b
	ld	 hl,(LA0AA)
	nop	
	nop	
	ld	 a,(bc)
	xor	 d
	add	 a,b
	nop	
LAC51:	nop	
	ld	 hl,(L00A8)
	nop	
	nop	
	and	 b
	and	 b
	nop	
	nop	
	nop	
	jr	 z,LAC7E
	nop	
	nop	
	nop	
	ex	 af,af'
LAC62:	and	 b
	nop	
	nop	
	nop	
	ret	 nz
	ret	 nz
	nop	
	nop	
	rrca	
	nop	
	jr	 nc,LAC6E
LAC6E:	nop	
	inc	 c
	ld	 c,b
	ld	 c,h
	nop	
LAC73:	nop	
	jr	 nc,LAC7E
	inc	 c
	nop	
	nop	
	inc	 a
	jr	 z,LAC88
	nop	
	nop	
LAC7E:	ccf	
	jr	 z,LACBD
	nop	
	nop	
	rrca	
	ex	 de,hl
	rst	 08H
	nop	
	nop	
LAC88:	inc	 bc
	rst	 38H
	jp	 L1FC0
	rrca	
	rst	 38H
	ret	 nc
	ret	 p
	inc	 de
	rst	 38H
	rst	 38H
	rst	 38H
	ret	 m
	nop	
	rrca	
	rst	 38H
	rst	 38H
	ret	 z
	rra	
	rrca	
	rst	 38H
	rst	 38H
	ex	 af,af'
	inc	 de
	rst	 38H
	rst	 38H
	call	 m,L0020
	rrca	
	call	 m,L2000
	rra	
	rrca	
	ret	 p
	inc	 bc
	add	 a,b
	inc	 de
	rst	 38H
	ret	 p
	rrca	
	add	 a,b
	nop	
	inc	 bc
	call	 m,LC00C
	nop	
	nop	
	ccf	
	ret	 p
LACBD:	inc	 a
	nop	
	nop	
	jr	 nc,LAC62
	nop	
	nop	
	inc	 bc
	jp	 nz,L0083
	nop	
	inc	 bc
	ld	 (de),a
	inc	 bc
	nop	
	nop	
	ex	 af,af'
	rrca	
	inc	 d
	ret	 nz
	nop	
	rrca	
	ld	 a,(bc)
	inc	 bc
	ret	 nz
	nop	
	rrca	
	jp	 pe,LC00F
	nop	
LACDD:	inc	 bc
	jp	 m,LC03F
	nop	
	inc	 bc
	rst	 38H
	ret	 p
	ret	 p
	nop	
	rrca	
	rst	 38H
	call	 p,L00F0
	rst	 38H
	rst	 38H
	call	 m,$1330
	rst	 08H
	rst	 38H
	rst	 38H
	ret	 p
	rra	
	rrca	
	rst	 38H
	rst	 38H
	ret	 z
	nop	
	ccf	
	rst	 38H
	ret	 p
	ex	 af,af'
	inc	 de
	rst	 38H
	rst	 38H
	nop	
LAD03:	jr	 nz,LAD24
	rrca	
	ret	 p
	inc	 a
	add	 a,b
	nop	
	ccf	
	ret	 nz
	jp	 z,$13C0
	rst	 38H
	di	
	jr	 nz,LAD4F
	rra	
	inc	 bc
	call	 m,$0000
LAD18:	nop	
	nop	
	jr	 nc,LAD3C
	nop	
	nop	
	inc	 bc
	ret	 nz
	inc	 hl
	nop	
	nop	
	inc	 bc
LAD24:	djnz	 LACA9
	nop	
	nop	
	inc	 c
	ld	 (bc),a
	add	 a,h
	ret	 nz
	nop	
	rrca	
	ld	 a,(bc)
	inc	 bc
	ret	 nz
	nop	
	rrca	
	jp	 z,LC00F
	nop	
	inc	 bc
	jp	 m,LC03F
	nop	
LAD3C:	inc	 bc
	rst	 38H
	ret	 p
	ret	 p
	nop	
	rrca	
	rst	 38H
	call	 p,L1FF0
	rrca	
	rst	 38H
	call	 m,$1330
	rst	 38H
	rst	 38H
	rst	 38H
	ret	 p
LAD4F:	nop	
	rrca	
	rst	 38H
	rst	 38H
	ret	 z
	rra	
	rrca	
	rst	 38H
	ret	 p
	ex	 af,af'
	inc	 de
	rst	 38H
	rst	 38H
	ld	 a,(bc)
	and	 b
	nop	
	rrca	
	ret	 p
	jr	 nz,LAD63
LAD63:	rra	
	rrca	
	ret	 nz
	adc	 a,a
	ret	 p
	inc	 de
	rst	 38H
	rst	 38H
	ret	 p
	inc	 a
	nop	
	inc	 bc
	ret	 p
	nop	
	ret	 p
	nop	
	nop	
	jr	 nc,LAD9E
	nop	
	nop	
	inc	 bc
	ret	 nz
	and	 e
	nop	
	nop	
	inc	 bc
	djnz	 LAD03
	nop	
	nop	
	inc	 c
	ld	 (bc),a
	add	 a,h
	ret	 nz
	nop	
	rrca	
	ld	 a,(bc)
	add	 a,e
	ret	 nz
	nop	
	rrca	
	jp	 pe,LC00F
	nop	
	inc	 bc
	ret	 m
	ccf	
	ret	 nz
	nop	
	inc	 bc
	rst	 38H
	ret	 p
	ret	 p
	nop	
	rrca	
	rst	 38H
	call	 p,L00F0
	rst	 38H
	rst	 38H
	call	 m,$1330
	rst	 08H
	rst	 38H
	rst	 38H
	ret	 p
	rra	
	rrca	
	rst	 38H
	rst	 38H
	ret	 z
	nop	
	ccf	
	rst	 38H
	ret	 p
	jr	 nz,LADC7
	rst	 38H
	rst	 38H
	nop	
	add	 a,b
	rra	
	rrca	
	ret	 p
	ld	 hl,(L00F0)
	ccf	
	ret	 nz
	inc	 c
	inc	 a
	inc	 de
	rst	 38H
	ret	 p
	inc	 sp
	inc	 c
LADC7:	rra	
	inc	 bc
	rst	 38H
	jp	 L00FF
	nop	
	ld	 a,(bc)
	add	 a,b
	inc	 c
	nop	
	nop	
	inc	 a
	jr	 z,LADE2
	nop	
LADD7:	nop	
	rst	 38H
	ld	 (bc),a
	or	 b
	nop	
	inc	 bc
	rst	 08H
	jp	 L03C0
	rst	 38H
LADE2:	rrca	
LADE3:	ret	 p
	ret	 p
	inc	 c
	inc	 c
	rra	
	ret	 p
	inc	 c
	ld	 sp,LFF03
	ret	 p
	inc	 c
	nop	
	inc	 bc
	rst	 38H
	ret	 p
	inc	 c
	ld	 (bc),a
	xor	 d
	rst	 38H
	call	 m,L003C
	ld	 hl,(LFFFF)
	call	 m,L0331
	rst	 38H
	rst	 38H
	ret	 p
	inc	 c
	rrca	
	rst	 38H
	rst	 38H
	ret	 p
	rrca	
	ccf	
	ccf	
	rst	 38H
	ret	 nz
	nop	
	call	 m,L300C
	ret	 nz
	nop	
	nop	
	inc	 c
	jr	 nc,LADD7
	nop	
	nop	
	inc	 a
	di	
	ret	 nz
	nop	
	nop	
	jr	 nc,LADE3
	nop	
	nop	
	nop	
	inc	 d
	ld	 d,c
	ld	 b,b
	nop	
	nop	
	nop	
	and	 b
	jr	 nc,LAE2C
LAE2C:	nop	
	rst	 38H
	ex	 af,af'
	jr	 nc,LAE31
LAE31:	rst	 38H
	di	
	jp	 nz,L0FC0
	ccf	
	inc	 bc
	ret	 nz
	add	 a,b
	nop	
	ld	 c,a
	rra	
	jp	 L2080
	ld	 b,e
	rst	 38H
	di	
	jr	 nz,LAE6D
	nop	
	rst	 38H
	ret	 p
	ret	 nz
	ld	 a,(bc)
	xor	 d
	rst	 38H
	call	 m,L0030
	xor	 d
	rst	 38H
	call	 m,L310C
	dec	 bc
	rst	 38H
	rst	 38H
	inc	 a
	inc	 c
	rrca	
	rst	 38H
	rst	 38H
	call	 m,L3F0F
	rst	 38H
	rst	 38H
	call	 m,LFC00
	ccf	
	rst	 38H
	ret	 p
	nop	
	nop	
	inc	 c
	inc	 a
	ret	 p
	nop	
LAE6D:	nop	
	rrca	
	inc	 c
	jr	 nc,LAE72
LAE72:	nop	
	inc	 bc
	rst	 08H
	inc	 a
	nop	
	nop	
	nop	
	jp	 L000C
	nop	
	ld	 bc,$1445
	nop	
	nop	
	nop	
	and	 b
	jr	 nc,LAE86
LAE86:	nop	
	rst	 38H
	ex	 af,af'
	call	 m,LFF00
	di	
	ret	 z
	call	 z,L3F0F
	inc	 bc
	ret	 z
	ret	 nz
	nop	
LAE95:	ld	 c,a
	rra	
	ret	 z
	ret	 nz
	jr	 z,LAE9E
	rst	 38H
	jp	 p,L0230
	add	 a,b
LAEA0:	rst	 38H
	ret	 p
	or	 b
	nop	
	xor	 d
	rst	 38H
	call	 m,L0030
	ld	 hl,(LFCFF)
	jr	 nc,LAEDF
	inc	 bc
	rst	 38H
	rst	 38H
	inc	 a
	inc	 c
	rrca	
	rst	 38H
	rst	 38H
	call	 m,L3F0F
	rst	 38H
	rst	 38H
	call	 m,LFC00
	ccf	
	rst	 38H
	ret	 p
	nop	
	nop	
	inc	 bc
	inc	 c
	jr	 nc,LAEC7
LAEC7:	nop	
	inc	 bc
	inc	 c
	jr	 nc,LAECC
LAECC:	nop	
	rrca	
	inc	 a
	ret	 p
	nop	
LAED1:	nop	
	inc	 c
	jr	 nc,LAE95
	nop	
	nop	
	dec	 b
	inc	 d
	ld	 d,b
	nop	
	nop	
	nop	
LAEDD:	add	 a,b
	call	 m,$0000
	rst	 38H
	inc	 hl
	call	 z,LFF00
	di	
	rrc	 h
	rrca	
	ccf	
	inc	 bc
	jp	 nz,L203C
	ld	 c,a
	rra	
	jp	 nz,L28C0
	inc	 bc
	rst	 38H
	jp	 p,L0A30
	and	 b
	rst	 38H
	ret	 p
	inc	 c
	nop	
	xor	 b
	rst	 38H
LAF00:	call	 m,L000C
	ld	 hl,(LFCFF)
	inc	 c
	ld	 sp,LFF0B
	rst	 38H
	inc	 a
	inc	 c
	rrca	
	rst	 38H
	rst	 38H
	call	 m,L3F0F
	rst	 38H
	rst	 38H
	call	 m,LFC00
	ccf	
	rst	 38H
	ret	 p
	nop	
	nop	
	inc	 c
	inc	 a
	ret	 p
	nop	
	nop	
	rrca	
	inc	 c
	jr	 nc,LAF26
LAF26:	nop	
	inc	 bc
	rst	 08H
	inc	 a
	nop	
	nop	
	nop	
	jp	 L000C
	nop	
	ld	 bc,$1445
	nop	
	ld	 (L3538),a
	ld	 b,c
	ld	 d,(hl)
	ld	 b,l
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	nop	
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
LAF80:	ld	 a,$07
	out	 ($07),a
	xor	 a
	out	 ($04),a
	ld	 hl,L404F
	ld	 c,$CA
LAF8C:	ld	 b,$14
LAF8E:	inc	 hl
	inc	 hl
	inc	 hl
	inc	 hl
	ld	 (hl),$03
	djnz	 LAF8E
	dec	 c
	jr	 nz,LAF8C
	ld	 hl,L4050
	ld	 b,$CA
	ld	 de,L0050
LAFA1:	ld	 (hl),$C0
	add	 hl,de
	djnz	 LAFA1
	ld	 hl,L4000
	ld	 c,$0A
	ld	 de,$05F0
LAFAE:	call	 LAFC2
	add	 hl,de
	dec	 c
	jr	 nz,LAFAE
	ld	 hl,L7F70
	call	 LAFC2

; Wait until service switch is on, then restart everything...

LAFBB:	in	 a,($10)
	bit	 3,a		; Is service switch on?
	jr	 z,LAFBB	; No, then wait until hell freezes over for it to be on
	rst	 00H		; When it service switch goes on, restart everything
;
LAFC2:	ld	 b,$50
LAFC4:	ld	 (hl),$FF
	inc	 hl
	djnz	 LAFC4
	ret	

	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	rst	 38H
	ld	 d,h
	ld	 c,b
	ld	 b,l
	nop	
	ld	 d,a
	ld	 c,c
	ld	 e,d
	ld	 b,c
	ld	 d,d
	ld	 b,h
	nop	
	ld	 c,a
	ld	 b,(hl)
	nop	
	ld	 d,a
	ld	 c,a
	ld	 d,d
	nop	
	ld	 b,h
	ld	 c,(hl)
	ld	 b,c
	nop	
	inc	 b
	.db	$22, $81



;END OF ASSEMBLY - EQUATES ARE BELOW


;
;
;*****************************************************************************
;
; Begin decyphered (or almost decyphered) memory locations. 
; Their function will be expanded on as information comes available
;
;*****************************************************************************
;



LD038:			.EQU	$D038   ; This location seems to have a number which the
					; first nybble is the same as the second nybble. 
					; The next byte $D039 seems to always hold the complement
					; of that number. ???

LD03B:			.EQU	$D03B	; Indicates if a coin acceptor is tripped. 
					; Zero's itself back out when switch is open.
					;   0=None tripped
					;   1=coin switch 1 tripped
					;   2=coin switch 2 tripped

LD03C:			.EQU	$D03C	; Number of credits.

LD042:			.EQU	$D042	; Counter. Number of seconds before change screens 
					; especially when in attract mode.

LD048:			.EQU	$D048	; Another counter. Noticed that it times changes between
					; things like "GET READY" and "GO". ???

LD1D6:			.EQU	$D1D6	; $d1d6-LSN Direction indicator for player 2. 
					; The value of the least significant nybble changes 
					; according to the bit at IN Port 11.
					
LD1D7:			.EQU	$D1D7	; $d1d7-LSN Direction indicator for player 1. 
					; The value of the least significant nybble changes 
					; according to the bit at IN Port 12.
					; See that description for details.

LD244:			.EQU	$D244	; Dip switch - Bit 7 - "Sounds in Attract Mode"

Is_Speech_Active:	.EQU	$D245	; $00 = Speech is inactive
					; $01 = Speech is active

LD2BD:			.EQU	$D2BD	; ==

LD2CC:			.EQU	$D2CC	; ==

LD2CE:			.EQU	$D2CE	; Speech: Address of next phoneme to process (place in string)

Num_Phonemes_Left:	.EQU	$D2D0	; Seems to hold how many phonemes left to go in speech string

LD2D1:			.EQU	$D2D1	; Seems to hold a phoneme in speech routines minus inflection bits (00xx xxxx) ???

LD2D2:			.EQU	$D2D2	; ==
					;
LD2D3:			.EQU	$D2D3	;Not sure - always seems to be a $d2 in it. 
					;Code zero's it out early. ???
					
LD2D4:			.EQU	$D2D4	; ==

Game_Mode:		.EQU	$D303	; This byte holds if you are in demo or game mode
					;   0: Game over, demo mode
					;   1: One player game in progress
					;   2: Two player game in progress

LD347:			.EQU	$D347	; This memory location is read, but never written to... ??? verify ???

LD349:			.EQU	$D349	; ??? (saved to in beginning of demo sequence)



;
;
;*****************************************************************************
;
; Locations so far unknown 
; 
;*****************************************************************************
;


L0003:	.EQU	$0003
L0005:	.EQU	$0005
L0007:	.EQU	$0007
L0009:	.EQU	$0009
L000B:	.EQU	$000B
L000D:	.EQU	$000D
L000F:	.EQU	$000F
L0011:	.EQU	$0011
L001A:	.EQU	$001A
L0020:	.EQU	$0020
L0029:	.EQU	$0029
L004B:	.EQU	$004B
L0050:	.EQU	$0050
L0055:	.EQU	$0055
L006A:	.EQU	$006A
L007C:	.EQU	$007C
L0080:	.EQU	$0080
L0083:	.EQU	$0083
L008C:	.EQU	$008C
L00A8:	.EQU	$00A8
L00E0:	.EQU	$00E0
L00F0:	.EQU	$00F0
L00FD:	.EQU	$00FD
L0100:	.EQU	$0100
L0116:	.EQU	$0116
L0138:	.EQU	$0138
L0150:	.EQU	$0150
L015E:	.EQU	$015E
L017E:	.EQU	$017E
L01A8:	.EQU	$01A8
L01FF:	.EQU	$01FF
L0201:	.EQU	$0201
L0202:	.EQU	$0202
L0207:	.EQU	$0207
L0209:	.EQU	$0209
L020F:	.EQU	$020F
L0217:	.EQU	$0217
L022A:	.EQU	$022A
L022B:	.EQU	$022B
L023C:	.EQU	$023C
L0248:	.EQU	$0248
L0250:	.EQU	$0250
L02B2:	.EQU	$02B2
L02FD:	.EQU	$02FD
L0301:	.EQU	$0301
L0303:	.EQU	$0303
L0307:	.EQU	$0307
L031E:	.EQU	$031E
L0325:	.EQU	$0325
L03A8:	.EQU	$03A8
L03C0:	.EQU	$03C0
L040B:	.EQU	$040B
L040E:	.EQU	$040E
L042E:	.EQU	$042E
L0507:	.EQU	$0507
L0601:	.EQU	$0601
L061A:	.EQU	$061A
L061C:	.EQU	$061C
L068C:	.EQU	$068C
L06AD:	.EQU	$06AD
L0700:	.EQU	$0700
L0702:	.EQU	$0702
L0715:	.EQU	$0715
L07C9:	.EQU	$07C9
L08A8:	.EQU	$08A8
L0A07:	.EQU	$0A07
L0A30:	.EQU	$0A30
L0A3E:	.EQU	$0A3E
L0A50:	.EQU	$0A50
L0C00:	.EQU	$0C00
L0C07:	.EQU	$0C07
L0C15:	.EQU	$0C15
L0C1F:	.EQU	$0C1F
L0C28:	.EQU	$0C28
L0C56:	.EQU	$0C56
L0D15:	.EQU	$0D15
L0E07:	.EQU	$0E07
L0F24:	.EQU	$0F24
L0F49:	.EQU	$0F49
L0F98:	.EQU	$0F98
L0FB7:	.EQU	$0FB7
L0FC0:	.EQU	$0FC0
L0FD1:	.EQU	$0FD1
L0FED:	.EQU	$0FED
L0FFF:	.EQU	$0FFF
L1004:	.EQU	$1004
L1016:	.EQU	$1016
L106A:	.EQU	$106A
L107B:	.EQU	$107B
L1088:	.EQU	$1088
L109A:	.EQU	$109A
L10A7:	.EQU	$10A7
L10C1:	.EQU	$10C1
L10EA:	.EQU	$10EA
L10FD:	.EQU	$10FD
L1107:	.EQU	$1107
L1152:	.EQU	$1152
L1166:	.EQU	$1166
L117C:	.EQU	$117C
L12A1:	.EQU	$12A1
L1301:	.EQU	$1301
L134B:	.EQU	$134B
L1360:	.EQU	$1360
L13C4:	.EQU	$13C4
L1400:	.EQU	$1400
L1413:	.EQU	$1413
L143E:	.EQU	$143E
L151F:	.EQU	$151F
L1520:	.EQU	$1520
L1540:	.EQU	$1540
L15C8:	.EQU	$15C8
L1601:	.EQU	$1601
L1681:	.EQU	$1681
L16B5:	.EQU	$16B5
L16B6:	.EQU	$16B6
L1700:	.EQU	$1700
L1701:	.EQU	$1701
L1710:	.EQU	$1710
L178D:	.EQU	$178D
L1893:	.EQU	$1893
L18D0:	.EQU	$18D0
L18D2:	.EQU	$18D2
L190B:	.EQU	$190B
L1C33:	.EQU	$1C33
L1C99:	.EQU	$1C99
L1D0D:	.EQU	$1D0D
L1D17:	.EQU	$1D17
L1D3E:	.EQU	$1D3E
L1D55:	.EQU	$1D55
L1D6D:	.EQU	$1D6D
L1D90:	.EQU	$1D90
L1DAB:	.EQU	$1DAB
L1DB3:	.EQU	$1DB3
L1DBA:	.EQU	$1DBA
L1DF9:	.EQU	$1DF9
L1E03:	.EQU	$1E03
L1E18:	.EQU	$1E18
L1E26:	.EQU	$1E26
L1E28:	.EQU	$1E28
L1E44:	.EQU	$1E44
L1EA6:	.EQU	$1EA6
L1F2B:	.EQU	$1F2B
L1F32:	.EQU	$1F32
L2000:	.EQU	$2000
L200E:	.EQU	$200E
L203C:	.EQU	$203C
L20A0:	.EQU	$20A0
L211D:	.EQU	$211D
L2218:	.EQU	$2218
L222B:	.EQU	$222B
L2303:	.EQU	$2303
L230B:	.EQU	$230B
L2694:	.EQU	$2694
L26AD:	.EQU	$26AD
L2701:	.EQU	$2701
L270D:	.EQU	$270D
L273E:	.EQU	$273E
L2783:	.EQU	$2783
L2801:	.EQU	$2801
L2903:	.EQU	$2903
L29C6:	.EQU	$29C6
L2A00:	.EQU	$2A00
L2A02:	.EQU	$2A02
L2A10:	.EQU	$2A10
L2A29:	.EQU	$2A29
L2A2C:	.EQU	$2A2C
L2AA8:	.EQU	$2AA8
L2AA9:	.EQU	$2AA9
L2B55:	.EQU	$2B55
L2B7A:	.EQU	$2B7A
L2B82:	.EQU	$2B82
L2C0C:	.EQU	$2C0C
L2C67:	.EQU	$2C67
L2CA7:	.EQU	$2CA7
L2D03:	.EQU	$2D03
L2D1A:	.EQU	$2D1A
L2D1F:	.EQU	$2D1F
L2D30:	.EQU	$2D30
L2D38:	.EQU	$2D38
L2D43:	.EQU	$2D43
L2D96:	.EQU	$2D96
L2DD1:	.EQU	$2DD1
L2E10:	.EQU	$2E10
L2F38:	.EQU	$2F38
L3000:	.EQU	$3000
L3010:	.EQU	$3010
L30C4:	.EQU	$30C4
L30D3:	.EQU	$30D3
L30E1:	.EQU	$30E1
L30F4:	.EQU	$30F4
L3100:	.EQU	$3100
L310C:	.EQU	$310C
L3338:	.EQU	$3338
L3368:	.EQU	$3368
L3374:	.EQU	$3374
L3376:	.EQU	$3376
L3378:	.EQU	$3378
L3380:	.EQU	$3380
L34AA:	.EQU	$34AA
L351D:	.EQU	$351D
L3522:	.EQU	$3522
L355C:	.EQU	$355C
L358A:	.EQU	$358A
L3607:	.EQU	$3607
L3728:	.EQU	$3728
L3832:	.EQU	$3832
L383A:	.EQU	$383A
L38BE:	.EQU	$38BE
L3950:	.EQU	$3950
L3C28:	.EQU	$3C28
L3CC7:	.EQU	$3CC7
L3D2B:	.EQU	$3D2B
L3D46:	.EQU	$3D46
L3E16:	.EQU	$3E16
L3E28:	.EQU	$3E28
L3E51:	.EQU	$3E51
L3F51:	.EQU	$3F51
L3F5C:	.EQU	$3F5C
L3FAC:	.EQU	$3FAC
L4000:	.EQU	$4000
L4001:	.EQU	$4001
L4004:	.EQU	$4004
L4005:	.EQU	$4005
L4010:	.EQU	$4010
L4041:	.EQU	$4041
L4045:	.EQU	$4045
L404C:	.EQU	$404C
L404F:	.EQU	$404F
L4050:	.EQU	$4050
L4051:	.EQU	$4051
L4054:	.EQU	$4054
L4055:	.EQU	$4055
L407D:	.EQU	$407D
L40F5:	.EQU	$40F5
L4105:	.EQU	$4105
L4115:	.EQU	$4115
L4141:	.EQU	$4141
L4155:	.EQU	$4155
L417D:	.EQU	$417D
L4307:	.EQU	$4307
L4500:	.EQU	$4500
L462D:	.EQU	$462D
L469C:	.EQU	$469C
L4C50:	.EQU	$4C50
L4EA3:	.EQU	$4EA3
L4F14:	.EQU	$4F14
L4F43:	.EQU	$4F43
L5041:	.EQU	$5041
L5055:	.EQU	$5055
L50D5:	.EQU	$50D5
L5100:	.EQU	$5100
L5101:	.EQU	$5101
L5211:	.EQU	$5211
L5241:	.EQU	$5241
L52C4:	.EQU	$52C4
L5401:	.EQU	$5401
L5445:	.EQU	$5445
L5455:	.EQU	$5455
L549D:	.EQU	$549D
L5500:	.EQU	$5500
L5501:	.EQU	$5501
L550D:	.EQU	$550D
L5510:	.EQU	$5510
L5515:	.EQU	$5515
L5516:	.EQU	$5516
L5518:	.EQU	$5518
L553D:	.EQU	$553D
L5550:	.EQU	$5550
L5555:	.EQU	$5555
L555D:	.EQU	$555D
L5595:	.EQU	$5595
L55A0:	.EQU	$55A0
L5606:	.EQU	$5606
L5655:	.EQU	$5655
L56AA:	.EQU	$56AA
L5753:	.EQU	$5753
L592A:	.EQU	$592A
L5A19:	.EQU	$5A19
L5A3B:	.EQU	$5A3B
L5A94:	.EQU	$5A94
L5AAA:	.EQU	$5AAA
L5B03:	.EQU	$5B03
L5BBE:	.EQU	$5BBE
L5E0C:	.EQU	$5E0C
L5F50:	.EQU	$5F50
L5F55:	.EQU	$5F55
L6125:	.EQU	$6125
L6202:	.EQU	$6202
L622E:	.EQU	$622E
L66C3:	.EQU	$66C3
L6804:	.EQU	$6804
L6876:	.EQU	$6876
L6AAC:	.EQU	$6AAC
L6C1B:	.EQU	$6C1B
L6C68:	.EQU	$6C68
L6CA4:	.EQU	$6CA4
L6D0C:	.EQU	$6D0C
L6F1B:	.EQU	$6F1B
L6F83:	.EQU	$6F83
L731E:	.EQU	$731E
L732B:	.EQU	$732B
L735B:	.EQU	$735B
L73AD:	.EQU	$73AD
L73BC:	.EQU	$73BC
L73CD:	.EQU	$73CD
L7512:	.EQU	$7512
L7716:	.EQU	$7716
L7950:	.EQU	$7950
L7A3B:	.EQU	$7A3B
L7A51:	.EQU	$7A51
L7C73:	.EQU	$7C73
L7C78:	.EQU	$7C78
L7D2B:	.EQU	$7D2B
L7D50:	.EQU	$7D50
L7E7C:	.EQU	$7E7C
L7F70:	.EQU	$7F70
L8002:	.EQU	$8002
L8008:	.EQU	$8008
L800A:	.EQU	$800A
L807D:	.EQU	$807D
L8084:	.EQU	$8084
L8200:	.EQU	$8200
L8236:	.EQU	$8236
L830F:	.EQU	$830F
L833E:	.EQU	$833E
L878C:	.EQU	$878C
L8829:	.EQU	$8829
L882E:	.EQU	$882E
L886D:	.EQU	$886D
L890E:	.EQU	$890E
L8924:	.EQU	$8924
L8941:	.EQU	$8941
L8952:	.EQU	$8952
L8971:	.EQU	$8971
L8A58:	.EQU	$8A58
L8A7E:	.EQU	$8A7E
L8B1A:	.EQU	$8B1A
L8B51:	.EQU	$8B51
L8B58:	.EQU	$8B58
L8BA8:	.EQU	$8BA8
L8BD1:	.EQU	$8BD1
L8CAE:	.EQU	$8CAE
L8CC3:	.EQU	$8CC3
L8CEB:	.EQU	$8CEB
L8D11:	.EQU	$8D11
L8D23:	.EQU	$8D23
L8E10:	.EQU	$8E10
L8E46:	.EQU	$8E46
L8E4A:	.EQU	$8E4A
L8E7D:	.EQU	$8E7D
L8F97:	.EQU	$8F97
L8FCD:	.EQU	$8FCD
L8FFF:	.EQU	$8FFF
L91A5:	.EQU	$91A5
L91FD:	.EQU	$91FD
L924F:	.EQU	$924F
L9255:	.EQU	$9255
L92FA:	.EQU	$92FA
L931C:	.EQU	$931C
L9331:	.EQU	$9331
L933B:	.EQU	$933B
L9379:	.EQU	$9379
L9381:	.EQU	$9381
L942B:	.EQU	$942B
L9462:	.EQU	$9462
L94B3:	.EQU	$94B3
L9535:	.EQU	$9535
L956B:	.EQU	$956B
L9595:	.EQU	$9595
L95CC:	.EQU	$95CC
L9750:	.EQU	$9750
L97B1:	.EQU	$97B1
L981A:	.EQU	$981A
L99E5:	.EQU	$99E5
L9B72:	.EQU	$9B72
L9BCB:	.EQU	$9BCB
L9BCC:	.EQU	$9BCC
L9C7B:	.EQU	$9C7B
L9E06:	.EQU	$9E06
;L9EAE:	.EQU	$9EAE
LA00F:	.EQU	$A00F
LA063:	.EQU	$A063
LA069:	.EQU	$A069
LA072:	.EQU	$A072
LA07C:	.EQU	$A07C
LA08B:	.EQU	$A08B
LA26B:	.EQU	$A26B
LA284:	.EQU	$A284
LA29C:	.EQU	$A29C
LA2F6:	.EQU	$A2F6
LA373:	.EQU	$A373
LA3C8:	.EQU	$A3C8
LA3E2:	.EQU	$A3E2
LA496:	.EQU	$A496
LA72A:	.EQU	$A72A
LA882:	.EQU	$A882
LA8AA:	.EQU	$A8AA
LA9DB:	.EQU	$A9DB
LAA0E:	.EQU	$AA0E
LAA3E:	.EQU	$AA3E
LAA98:	.EQU	$AA98
LAA99:	.EQU	$AA99
LABE6:	.EQU	$ABE6
LACA9:	.EQU	$ACA9
LAD9E:	.EQU	$AD9E
LAE16:	.EQU	$AE16
LAE9E:	.EQU	$AE9E
LAEDF:	.EQU	$AEDF
LAFA0:	.EQU	$AFA0
LAFAA:	.EQU	$AFAA
LB059:	.EQU	$B059
LB0AE:	.EQU	$B0AE
LB31E:	.EQU	$B31E
LB697:	.EQU	$B697
LB72A:	.EQU	$B72A
LB83E:	.EQU	$B83E
LB9B0:	.EQU	$B9B0
LBCCE:	.EQU	$BCCE
LBE82:	.EQU	$BE82
LBEEF:	.EQU	$BEEF
LBFAA:	.EQU	$BFAA
LBFBB:	.EQU	$BFBB
LBFF6:	.EQU	$BFF6
LC000:	.EQU	$C000
LC002:	.EQU	$C002
LC003:	.EQU	$C003
LC004:	.EQU	$C004
LC00A:	.EQU	$C00A
LC00B:	.EQU	$C00B
LC00C:	.EQU	$C00C
LC00D:	.EQU	$C00D
LC00F:	.EQU	$C00F
LC030:	.EQU	$C030
LC03F:	.EQU	$C03F
LC058:	.EQU	$C058
LC3C3:	.EQU	$C3C3
LC507:	.EQU	$C507
LCB07:	.EQU	$CB07
LCC0F:	.EQU	$CC0F
LCD00:	.EQU	$CD00
LCEAC:	.EQU	$CEAC

;
; Begin Static RAM area
;

LD000:	.EQU	$D000
LD003:	.EQU	$D003



LD03A:	.EQU	$D03A

	
			;
LD03D:	.EQU	$D03D
LD03E:	.EQU	$D03E
LD040:	.EQU	$D040
LD041:	.EQU	$D041
			;

LD043:	.EQU	$D043
LD044:	.EQU	$D044
LD045:	.EQU	$D045
LD046:	.EQU	$D046
LD047:	.EQU	$D047



LD049:	.EQU	$D049
LD04A:	.EQU	$D04A
LD04B:	.EQU	$D04B
LD04C:	.EQU	$D04C
LD04D:	.EQU	$D04D
LD04E:	.EQU	$D04E
LD04F:	.EQU	$D04F
LD050:	.EQU	$D050
LD051:	.EQU	$D051
LD053:	.EQU	$D053
LD054:	.EQU	$D054
LD058:	.EQU	$D058
LD05A:	.EQU	$D05A
LD05C:	.EQU	$D05C
LD067:	.EQU	$D067
LD06E:	.EQU	$D06E
LD074:	.EQU	$D074
LD078:	.EQU	$D078
LD07A:	.EQU	$D07A
LD07C:	.EQU	$D07C
LD087:	.EQU	$D087
LD094:	.EQU	$D094
LD096:	.EQU	$D096
LD09C:	.EQU	$D09C
LD0A7:	.EQU	$D0A7
LD0C2:	.EQU	$D0C2
LD0CD:	.EQU	$D0CD
LD0E8:	.EQU	$D0E8
LD0F3:	.EQU	$D0F3
LD10E:	.EQU	$D10E
LD119:	.EQU	$D119
LD134:	.EQU	$D134
LD13F:	.EQU	$D13F
LD15A:	.EQU	$D15A
LD165:	.EQU	$D165
LD172:	.EQU	$D172
LD178:	.EQU	$D178
LD18E:	.EQU	$D18E
LD198:	.EQU	$D198
LD1BA:	.EQU	$D1BA
LD1BB:	.EQU	$D1BB
LD1BD:	.EQU	$D1BD
LD1BF:	.EQU	$D1BF
LD1C1:	.EQU	$D1C1
LD1C3:	.EQU	$D1C3
LD1C4:	.EQU	$D1C4
LD1C5:	.EQU	$D1C5
LD1C6:	.EQU	$D1C6
LD1C7:	.EQU	$D1C7
LD1C8:	.EQU	$D1C8
LD1C9:	.EQU	$D1C9
LD1CA:	.EQU	$D1CA
LD1CB:	.EQU	$D1CB
LD1CC:	.EQU	$D1CC
LD1CD:	.EQU	$D1CD
LD1CE:	.EQU	$D1CE
LD1CF:	.EQU	$D1CF
LD1D0:	.EQU	$D1D0
LD1D1:	.EQU	$D1D1
LD1D2:	.EQU	$D1D2
LD1D3:	.EQU	$D1D3
LD1D4:	.EQU	$D1D4
LD1D5:	.EQU	$D1D5
			;

			;
LD1D8:	.EQU	$D1D8
LD1D9:	.EQU	$D1D9
LD1DA:	.EQU	$D1DA
LD1DB:	.EQU	$D1DB
LD1DC:	.EQU	$D1DC
LD1DD:	.EQU	$D1DD
LD1DE:	.EQU	$D1DE
LD1DF:	.EQU	$D1DF
LD1E0:	.EQU	$D1E0
LD1E1:	.EQU	$D1E1
LD1E2:	.EQU	$D1E2
LD1E3:	.EQU	$D1E3
LD1E4:	.EQU	$D1E4
LD1E5:	.EQU	$D1E5
LD1E6:	.EQU	$D1E6
LD1E7:	.EQU	$D1E7
LD1E8:	.EQU	$D1E8
LD1E9:	.EQU	$D1E9
LD1EA:	.EQU	$D1EA
LD1EB:	.EQU	$D1EB
LD1EC:	.EQU	$D1EC
LD1ED:	.EQU	$D1ED
LD1EE:	.EQU	$D1EE
LD1EF:	.EQU	$D1EF
LD1F0:	.EQU	$D1F0
LD1F1:	.EQU	$D1F1
LD1F2:	.EQU	$D1F2
LD1F3:	.EQU	$D1F3
LD240:	.EQU	$D240
LD241:	.EQU	$D241
LD242:	.EQU	$D242
LD243:	.EQU	$D243


LD270:	.EQU	$D270
LD2AC:	.EQU	$D2AC

LD2BE:	.EQU	$D2BE




LD300:	.EQU	$D300
LD301:	.EQU	$D301
LD302:	.EQU	$D302
			;

			;
LD304:	.EQU	$D304
LD30E:	.EQU	$D30E
LD318:	.EQU	$D318
LD319:	.EQU	$D319
LD31A:	.EQU	$D31A
LD31B:	.EQU	$D31B
LD31D:	.EQU	$D31D
LD33A:	.EQU	$D33A
LD340:	.EQU	$D340
LD341:	.EQU	$D341
LD342:	.EQU	$D342
LD343:	.EQU	$D343
LD344:	.EQU	$D344
LD345:	.EQU	$D345
LD346:	.EQU	$D346

LD348:	.EQU	$D348

			;

			;
LD34A:	.EQU	$D34A
LD34C:	.EQU	$D34C
LD34E:	.EQU	$D34E
LD34F:	.EQU	$D34F
LD350:	.EQU	$D350
LD351:	.EQU	$D351
LD352:	.EQU	$D352
LD353:	.EQU	$D353
LD354:	.EQU	$D354
LD47D:	.EQU	$D47D
LD55F:	.EQU	$D55F
LD59C:	.EQU	$D59C
LDA07:	.EQU	$DA07
LDD04:	.EQU	$DD04
LDE15:	.EQU	$DE15
LE0A9:	.EQU	$E0A9
LE404:	.EQU	$E404
LE40C:	.EQU	$E40C
LE5BC:	.EQU	$E5BC
LE79F:	.EQU	$E79F
LE7E7:	.EQU	$E7E7
LEA02:	.EQU	$EA02
LEABF:	.EQU	$EABF
LEAEE:	.EQU	$EAEE
LEB05:	.EQU	$EB05
LEB12:	.EQU	$EB12
LEEAE:	.EQU	$EEAE
LF002:	.EQU	$F002
LF03F:	.EQU	$F03F
LF0AF:	.EQU	$F0AF
LF557:	.EQU	$F557
LF6A2:	.EQU	$F6A2
LF6A8:	.EQU	$F6A8
LF807:	.EQU	$F807
LF8AA:	.EQU	$F8AA
LF904:	.EQU	$F904
LF905:	.EQU	$F905
LF906:	.EQU	$F906
LFB00:	.EQU	$FB00
LFB0A:	.EQU	$FB0A
LFB1E:	.EQU	$FB1E
LFB8B:	.EQU	$FB8B
LFC00:	.EQU	$FC00
LFC3B:	.EQU	$FC3B
LFCD1:	.EQU	$FCD1
LFCFF:	.EQU	$FCFF
LFD8E:	.EQU	$FD8E
LFE07:	.EQU	$FE07
LFF00:	.EQU	$FF00
LFF03:	.EQU	$FF03
LFF0B:	.EQU	$FF0B
LFF0F:	.EQU	$FF0F
LFF16:	.EQU	$FF16
LFF18:	.EQU	$FF18
LFF3B:	.EQU	$FF3B
LFF3F:	.EQU	$FF3F
LFFD6:	.EQU	$FFD6
LFFDD:	.EQU	$FFDD
LFFE4:	.EQU	$FFE4
LFFEB:	.EQU	$FFEB
LFFF2:	.EQU	$FFF2
LFFF5:	.EQU	$FFF5
LFFF9:	.EQU	$FFF9
LFFFB:	.EQU	$FFFB
LFFFF:	.EQU	$FFFF

; END OF EQUATE AREA

	.END
