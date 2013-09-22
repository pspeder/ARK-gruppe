    .data
	.align 2
	array: .word -1,25,323,-455,599,9999999,-99999992 # indtast jeres array på denne måde når i skal teste, label bestemmes selv.
	.align 2
	text:  .asciiz "Print array: " #nul termineret, kan ændres til feks at sige sorteret array, ville give god mening for G1. 
	.align 2
	newLine: .byte		10,0
	.align 2
	negative: .asciiz "-"
	.align 2
	leftP: .asciiz "("
	.align 2
	rightP: .asciiz ")"
	.align 2
	comma: .asciiz ","
	.text

main:
#  addi $t0, $zero, 7		# build list on heap as you normally would
#  sw   $t0, 0($gp)
#  addi $t0, $zero, 12
#  sw   $t0, 4($gp)
#  addi $t0, $zero, -2
#  sw   $t0, 8($gp)
#  sw   $zero, 12($gp)
#  addi $t0, $zero, 4
#  sw   $t0, 16($gp)
#  addi $t0, $zero, 10
#  sw   $t0, 20($gp)
  la   $a0, array			# set list as an argument
  addi $a1, $zero, 0		# set l argument
  addi $a2, $zero, 7		# set r argument

  jal  quicksort			# run quicksort
  nop
  jal  callPrint 			# print sorted array
  j done					# terminate
  
#demonstrerer brugen af print
callPrint:
	la   $a0, array           # load sorted array into $a0
	addi $a1, $zero, 7      # let a1 = no of items in array
	jal  print              # jump to printing
	li   $v0, 10			# 'exit' system call
	syscall                 # execute

quicksort:
  slt  $t0, $a1, $a2		# is l < r
  beq  $t0, $zero, return	# if not l < r then return to caller

  addi $sp, $sp, -12		# make place on stack for $ra, $a1, $a2
  sw   $ra, 8($sp)			# save $ra on stack
  sw   $a1, 4($sp)			# save $a1 on stack
  sw   $a2, 0($sp)			# save $a2 on stack

  jal  partition		# call partition

  addi $a2, $v0, -1		# set j-1 as r argument
  jal  quicksort		# run quicksort on l..j-1

  lw   $a2, 0($sp)		# restore original r argument
  
  addi $a1, $v0, 1		# set j+1 as l argument
  jal  quicksort		# run quicksort on j+1..r

  lw   $a1, 4($sp)		# restore original l argument
  lw   $ra, 8($sp)		# restore $ra
  addi $sp, $sp, 12		# restore stack pointer

  jr   $ra				# return

partition:
  sll  $t0, $a1, 2		
  add  $t0, $a0, $t0		# $t0 is adress of a[l]
  lw   $t1, 0($t0)			# $t1 is the value of a[l] (pivot)
  addi $t2, $a1, 0			# $t2 is i
  addi $t3, $a2, 1			# $t3 is j

  loop:						# while(1)
    sll  $t4, $t2, 2
    add  $t4, $a0, $t4		# $t4 is a pointer to a[i]
    i_loop:
      addi $t2, $t2, 1		# i++
      addi $t4, $t4, 4		# increment the a[i] pointer

      lw   $t5, 0($t4)		# $t5 is the value of a[i]

      addi $t6, $t1, 1		# $t6 is pivot+1
      slt  $t6, $t5, $t6	# a[i] < povot+1 aka a[i] <= pivot
      addi $t7, $a2, 1		# $t7 is r+1
      slt  $t7, $t2, $t7	# i < r+1 aka i <= r
      and  $t7, $t6, $t7	# a[i] <= pivot && i <= r
      bne  $t7, $zero, i_loop	# if a[i] <= pivot && i <= r repeat "i_loop"

    addi $t6, $t4, 0		# save the pointer to a[i]
    addi $t7, $t5, 0		# save the value of a[i]


    sll  $t4, $t3, 2
    add  $t4, $a0, $t4		# $t4 is a pointer to a[j]
    j_loop:
      addi $t3, $t3, -1		# j--
      addi $t4, $t4, -4		# decrement the a[j] pointer
    
      lw   $t5, 0($t4)		# $t5 is the value of a[j]
      bgt  $t5, $t1, j_loop	# if a[j] > pivot repeat "j_loop"

    sw   $t5, 0($t6)		# save the value of a[j] at a[i]
    sw   $t7, 0($t4)		# save the value of a[i] at a[j]

    bgt  $t3, $t2, loop		# repeat "loop" if j > i

  sw   $t5, 0($t0)			# save the value of a[j] at a[l]
  sw   $t1, 0($t4)			# save the value of a[l] at a[j]

  addi $v0, $t3, 0			# set the value of j as the return value

  jr   $ra					# return

return:
  jr $ra

## Det er denne funktion der er tiltænkt i kan bruge til at printe jeres array ud for nemt at se om det er sorteret
print: 	#Printer string der gives i argument a0 med længden gemt i a1
	addi $sp, $sp, -12
	sw $s1, 0($sp)
	sw $s2, 4($sp)
	sw $ra, 8($sp)
	
	move $s1, $a0			# indeholder starten af array
	move $s2, $a1			# indeholder længen af array
	
	la $a0, text			# Print text
	li $v0, 4
	syscall
	
	la $a0, newLine			# Print \n
	li $v0, 4
	syscall
		
	la $a0, leftP			# Print (
	li $v0, 4
	syscall
	
startPrintArrayLoop:
	beqz $s2 EndPrintArrayLoop
	lw $a0, ($s1)		    # Hent tallet ud
	jal printSingleNumber
	beq $s2, 1, skipComma	# skip comma for last element
	la $a0, comma			# Print ,
	li $v0, 4
	syscall
skipComma:
	addi $s1, $s1, 4		# næste tal i array
	addi $s2, $s2, -1		# brug længden som counter
	j startPrintArrayLoop
EndPrintArrayLoop:
		
	la $a0, rightP			# Print )
	li $v0, 4
	syscall
	
	lw $s1, 0($sp)
	lw $s2, 4($sp)
	lw $ra, 8($sp)
	addi $sp, $sp, 12
	jr $ra

printSingleNumber:	#a0 er int der skal printes
	addi $sp, $sp, -12 # til at gemme tal i
	move $t1, $a0
	move $t5, $sp	#hvor vi er i vores array
	
	bgt $t1, -1, positive		#check for negative number
	neg $t1, $t1

	# add - til strengen
	addi $t2, $zero, 45
	sb $t2, ($t5) 
	addi $t5, $t5, 1 
	
positive: #when we come here the number is positive
	addi $t2, $zero, 1000000000
	move $t4, $zero				#bruges som flag til at teste om 
	# Loop and see if t2 is != 0, otherwise keep looping
startPositiveLoop:
	beqz $t2, exitPositiveLoop
	divu $t1, $t2
	mflo $t3
	mfhi $t1
	beq  $t3, $t4, skipZero
	addi $t4, $t4, -1 			# er nu under 0 og skip ville ikke blive taget
	addi $t3, $t3, 48 			# ascii 0
	sb 	 $t3, 0($t5)
	addi $t5, $t5, 1
skipZero:	
	divu $t2, $t2, 10
	j 	 startPositiveLoop
exitPositiveLoop:
	sb   $zero, ($t5)			# null terminer
	move $a0, $sp			# Print tal
	li   $v0, 4
	syscall
	
	addi $sp, $sp, 12
	jr   $ra
	
done: 					# 'exit' program
