
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

  ldr r0, =1
  bl assert

  
  ldr r0, =0
  cmp r0, #0
  moveq r0, #1
  movne r0, #0
  bl assert


  mov sp, fp
  mov r0, #0
  pop {fp, pc}
