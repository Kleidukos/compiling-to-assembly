
.global assert
assert:

  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}

  // conditional
  ldr r0, [fp, #-16]
  // is the conditional false?
  cmp r0, #0 
  // if yes, branch to alternative
  beq .L0

  // if no, we go to the consequence
  
  ldr r0, =46
  bl putchar

  // and branch to the next block of instructions
  b .L1

// alternative (the condition was false)
.L0:

  ldr r0, =70
  bl putchar

// end of conditional
.L1:


  mov sp, fp
  mov r0, #0
  pop {fp, pc}

.global main
main:

  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}

  // conditional
  ldr r0, =1
  // is the conditional false?
  cmp r0, #0 
  // if yes, branch to alternative
  beq .L2

  // if no, we go to the consequence
  
  ldr r0, =1
  bl assert

  // and branch to the next block of instructions
  b .L3

// alternative (the condition was false)
.L2:

  ldr r0, =0
  bl assert

// end of conditional
.L3:

  // conditional
  ldr r0, =0
  // is the conditional false?
  cmp r0, #0 
  // if yes, branch to alternative
  beq .L4

  // if no, we go to the consequence
  
  ldr r0, =0
  bl assert

  // and branch to the next block of instructions
  b .L5

// alternative (the condition was false)
.L4:

  ldr r0, =1
  bl assert

// end of conditional
.L5:


  mov sp, fp
  mov r0, #0
  pop {fp, pc}
