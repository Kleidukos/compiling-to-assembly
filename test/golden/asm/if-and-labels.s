
.global main
main:
  push {fp, lr}

  // conditional
  ldr r0, =1
  // is the conditional false?
  cmp r0, #0 
  // if yes, branch to alternative
  beq .L0

  // if no, we go to the consequence
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  // and branch to the next block of instructions
  b .L1

// alternative (the condition was false)
.L0:
ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

// end of conditional
.L1:

  // conditional
  ldr r0, =0
  // is the conditional false?
  cmp r0, #0 
  // if yes, branch to alternative
  beq .L2

  // if no, we go to the consequence
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  // and branch to the next block of instructions
  b .L3

// alternative (the condition was false)
.L2:
ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

// end of conditional
.L3:

  mov r0, #0
  pop {fp, pc}
