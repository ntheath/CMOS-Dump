;------------------------------------------------------------------------
;cdump - CMOS dump utility
;
;Author:  Nicholas Heath
;
;Copyright 2003, Dell Computer Corp.
;
;Version: 1.0
;------------------------------------------------------------------------

include equates.inc

STACK_SEG segment STACK

  DB 128 DUP('STACK    ')
  ;TOP_OF_STACK DW ?			;not needed if 'STACK' is used in stack
					;segment declaration

STACK_SEG ends

DATA_SEG segment 

hello_message 	db 	'cdump, CMOS dump utility, v1.0',CR,LINE_FEED,'$'
hello_message_2 db 	'Copyright Nick Heath, ', 60h, '03, Dell Computer Corp.',CR,LINE_FEED,'$'
new_line 	db 	CR, LINE_FEED,'$'
exit_message 	db 	'Thanks for stopping by!',CR, LINE_FEED,'$'
xtended_CMOS 	db 	'Extended CMOS detected!', CR, LINE_FEED,'$'
index_line 	db 	COLON,SPACE,SPACE,'$'

ASCIIAtoF 	db 	41h, 42h, 43h, 44h, 45h, 46h
ASCII0to9 	db 	30h, 31h, 32h, 33h, 34h, 35h, 36h, 37h, 38h, 39h

DATA_SEG ends
					
CODE_SEG segment 
  assume cs:CODE_SEG, ds:DATA_SEG, es:nothing, ss:STACK_SEG
					
main  proc
	mov	ax, DATA_SEG		;setup data segment
	mov	ds, ax
	mov	ax, STACK_SEG		;setup stack segment
      	mov 	ss, ax
      	;mov 	sp, offset TOP_OF_STACK ;setup stack pointer, not needed if 'STACK' is used in
					;the declaration of the stack segment

	call 	print_info		;print program info
      	
	xor 	cx, cx			;house cleaning, cx will be the index for the loop
      	xor 	ax,ax  			;...
      	xor 	bx, bx			;bl will contain current CMOS address
       
      	mov 	cl, 64			;get all 64 bytes by default (discludes xtended)
      	call 	check_xCMOS		;determine whether xtended CMOS is present
      	cmp 	ah, 0			;if so, we'll need to get 128 bytes
      
      	je 	xtend_not_present
      	mov 	cl, 128			;extended cmos is present, load all 128 bytes
      	mov 	bh, 01h			;use bh to tell other procedures that xtended
					;CMOS is present, 1 = true, present
      	mov 	ah, 09h
      	mov 	dx,offset xtended_CMOS	;printed xtended CMOS detection message
      	int 	21h 

xtend_not_present:
      	mov 	al, bl			;print out first index by default
      	call 	print_ASCIItoHEX	;print the first address index
      	mov 	ah, 09			
      	mov 	dx,offset index_line	;also include ' : '
      	int 	21h     
     
CMOS:
      	mov 	al, bl			;setup al for get_CMOS call (see procedure
					;comments)
      	call 	get_CMOS			
      	mov 	al, ah			;get_CMOS returns value in ah	
      
      	call 	print_ASCIItoHEX      	;print out value in hex
      	call 	check_index		;if we are at xFh, advance to next line
      	inc 	bl			;move to next address

;      	loop 	CMOS			;continue until all regs have been displayed
					;(contained in cl)
      

      	call 	print_exit      	;show exit message, mainly for debugging
      	mov 	ax,4C00h		;terminate process
      	int 	21h
main  endp


;------------------------------------------------------------------------
;Procedure:  get_CMOS
;
;Function:  retrieve requested CMOS value
;
;On entry:  al - contains address of CMOS byte to retrieve, 0Dh - 7Fh
;
;Returns:  ah - contains requested CMOS byte 
;
;Calls:  none
;------------------------------------------------------------------------
get_CMOS proc				
  	cli  				;disable interrupts and NMI per recommendation
  	or 	al, 80h			;in Undocumted PC, pg. 882
  				
  	out 	CMOS_OUT_PORT, al  	;send request with address of byte
  	in 	al, CMOS_IN_PORT	;get byte returned
  
  	mov 	ah, al				
  
  	mov 	al, 00h			;prepare to turn NMI back on (put 0 in disable bit)
  	;out 	CMOS_OUT_PORT, al
  	sti				;turn interrupts back on
  
  	ret			
get_CMOS endp


;------------------------------------------------------------------------
;Procedure:  check_xCMOS
;
;Function:  Determine whether extended CMOS is present, compares 0Eh - 3Fh with
;           40h - 7Fh.
;
;On entry:  none
;
;Returns:  ah = 1h if extended CMOS is present, 0h if not present
;
;Calls:  get_CMOS
;------------------------------------------------------------------------
check_xCMOS proc
  	pushf
  	push 	cx			;backup regs
  	push 	bx			;...
  	push 	dx			;...
  	xor 	ax, ax
  	mov 	ah, 00h			;default value = xCMOS is not present
  
  	mov 	bl, 0Eh			;start with the beginning addresses of 
  	mov 	cl, 4eh			;standard and extended CMOS

continue:   

  	mov 	al, bl			;prepare to call get_CMOS
  	call 	get_CMOS
  	mov 	dh, ah			;retrieved the standard CMOS value
  	mov 	al, cl			;lets check the extended	
  	call 	get_CMOS
  	mov 	dl, ah			;now have the extended value
  	cmp 	dh, dl			;are they equal?
  
  	jne 	xCMOS_present 		;if not, go ahead and return a 1h, indicating
					;xtended CMOS is present

  	inc 	cl			;so far they're the same, need to check 
  	inc 	bl			;the next two
  
  	cmp 	bl, 3Fh			;are we finished?
  	jne 	continue		;if not, go back and check again
  	jmp 	finished		;if all values are equal, then xCMOS
					;is not present

xCMOS_present:
  	mov 	ah, 01h			;if present, return 1 = true
   
finished:
  
  	pop 	dx			;restore all regs and flags
  	pop 	bx			;...
  	pop 	cx			;...
  	popf				;...
  	ret
check_xCMOS endp


;------------------------------------------------------------------------
;Procedure:  print_info
;
;Function:  Prints program info
;
;On entry:  none
;
;Returns: info to stdout
;
;Calls:  print_new_line
;------------------------------------------------------------------------
print_info proc
  	push 	ax			;backup regs
  	push 	dx			;...
  
  	mov 	ah,9			;prepare for string output
  	;mov 	dx,offset hello_message	;load add of string into dx
  	lea 	dx, hello_message	;load effective address of hello_message
  	int 	21h			;print string 
  	;mov 	dx,offset hello_message_2	;same as above
  	lea 	dx, hello_message_2
	int 	21h      			
 
	call 	print_new_line
	pop 	dx;			;restore regs used
	pop 	ax;			;...
  	ret
print_info endp


;------------------------------------------------------------------------
;Procedure:  print_ASCIItoHEX
;
;Function:  Print the byte in Al to std out, with a space after the byte
;           
;
;On entry:  al
;
;Returns:  byte with ASCII conversion before int21h to std out
;
;Calls:  none
;------------------------------------------------------------------------
print_ASCIItoHEX proc
  	pushf				;backup flags
  	push 	bx			;backup regs
  	push 	cx			;...
  	push 	dx			;...

  	xor 	bx, bx			;clean up regs, 0000h
  	xor 	cx, cx			;...
  	xor 	dx, dx			;...

  	mov 	cl, 04h 		;need to shift high nibble by 4 bits
 
  	mov 	ch, al			;backup our hex value, start work on high nibble
  	and 	ax, 00F0h		;chop off lower nibble
  	shr 	al, cl			;high nibble is now low nibble, cx is free now
  
  	mov 	bx, ax			;prepare for base indexing to ASCII0to9 using bx
 
  	cmp 	bx, 9h 			;need to check for 0 -9, or A - F
  	jg 	high_is_letter		;if this nibble is A - F, skip to is_letter
  
  	mov 	dl, ASCII0to9[bx] 	;check lookup table for our byte
  	jmp 	high_nibble_complete	;finished with high nibble, start work on low nibble

high_is_letter:
  	sub 	bx, 0Ah 		;need to subtract Ah to correctly align byte with 
  	mov 	dl, ASCIIAtoF[bx]	;lookup table

high_nibble_complete: 
  	mov 	ah, 2h			;done with the high nibble, print it
  	int 	21h
  
low_nibble:
  	mov 	al, ch			;grab lower nibble
  	and 	ax, 0Fh			;chop off high nibble
  
  	mov 	bx, ax			;prepare for indexing, using bx
  	cmp 	bx, 9h			;check 0 - 9, or A - F
  	jg 	low_is_letter  		;if this nibble is A - F, skip to is_letter

  	mov 	dl, ASCII0to9[bx]	;converts byte to hex ASCII code
  	jmp 	low_nibble_complete	;if low nibble is number, index into ASCII0to9

low_is_letter:  			;if letter, subtract away 0Ah to correctly index
  	sub 	bx, 0Ah  		;into ASCII0to9
  	mov 	dl, ASCIIAtoF[bx]

low_nibble_complete:  			;finished with low nibble, print
  	mov 	ah, 02
  	int 	21h

  	mov 	dl, SPACE		;add a space after the byte
  	int 	21h
    
  	pop 	dx			;restore regs,flags used
  	pop 	cx			;...
  	pop 	bx			;...
  	popf				;...
  	ret
print_ASCIItoHEX endp


;------------------------------------------------------------------------
;Procedure:  print_exit
;
;Function:  print exit message
;           
;
;On entry:  none
;
;Returns:  exit message to std out
;
;Calls:  print_new_line
;------------------------------------------------------------------------
print_exit proc
  	push 	ax			;back up regs used

  	call 	print_new_line		;make it look nice
  	mov 	ah, 9 ;string output	;show exit message
  	mov 	dx,offset exit_message
  	int 	21h   
  
  	pop 	ax			;restore regs used
  	ret
print_exit endp
					

;------------------------------------------------------------------------
;Procedure:  check_index
;
;Function:  Checks to see if our index is currently xFh, if so inserts a 
;           blank line on screen to advance to the next address, also prints
;	    our address labeling, i.e. 00:, 10:, etc., which appears
;           at the left of each line of bytes
;
;On entry:  current address, in bl
;
;Returns:  blank line, correct address for each line
;
;Calls:  print_new_line, print_ASCIItoHEX
;------------------------------------------------------------------------
					
check_index proc
  	pushf
  	push 	ax			;backup regs used
  	push 	bx			;...
  	xor 	ax, ax			;housecleaning
  	mov 	al, bl			;backup our address value
 
  	and 	bl, 0Fh			;just get the lower nibble, chop off high
  	cmp 	bl, 0Fh			;check to see if we're at the last address
					;of the line, i.e. xF
  	jne 	not_done		;bail out if new line isn't needed

  
  	call 	print_new_line		;at the end of line, insert a new line

  	cmp 	bh, 00h			;check if we need to display past 30 - 3F
  					;i.e. xtended CMOS is or isn't present
  	jne 	ok_continue		;if xtended exists, then proceed to > 3F
  	cmp 	al, 3fh			;if not, check to see if we're at 3Fh 
					;and then get out of here, no need to print 40:
  	je 	not_done

ok_continue:
  	cmp 	al, 7fh			;if this is true, we've already printed 70:
					;no need to do any more address printing  
  	je 	not_done

					;if we made it here, then we're at the end of a
					;line and need to print the next line's address
  	and 	al, 0F0h		;just get the upper nibble
  	add 	al, 10h			;increment because we display '00: '
					;before we call this procedure
  	call 	print_ASCIItoHEX
  	mov 	ah, 09
  	mov 	dx, offset index_line	;also put a colon and space in between
  	int 	21h			;address and data
  
not_done:  
  	pop 	bx			;restore regs, flags
  	pop 	ax			;...
  	popf				;...
  	ret
check_index endp


;------------------------------------------------------------------------
;Procedure:  print_new_line
;
;Function:  prints a new line to std out 
;
;On entry:  none
;
;Returns:  new line to std out
;
;Calls:  none
;------------------------------------------------------------------------
print_new_line proc
  	push 	ax			;backup regs
  	push 	dx			;...
  	mov 	ah, 09h
  	mov 	dx,offset new_line  	;insert a new line
  	int 	21h
  	pop 	dx			;restore regs
  	pop 	ax			;...
  	ret
print_new_line endp

CODE_SEG ends
					
end   main
