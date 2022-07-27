
  // conditional
  
  ldr r0, =1
  cmp r0, #0
  // branch to alternative
  beq .L0

  // consequence
   
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  b .L1

// alternative
.L0:
 
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar


.L1:

  // conditional
  
  ldr r0, =0
  cmp r0, #0
  // branch to alternative
  beq .L2

  // consequence
   
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  b .L3

// alternative
.L2:
 
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar


.L3:
