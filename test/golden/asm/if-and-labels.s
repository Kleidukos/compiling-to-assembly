
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

  b .L0

//alternative
.L0:
  
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar


.L0:

  // conditional
  
  ldr r0, =0
  cmp r0, #0
  // branch to alternative
  beq .L1

  // consequence
    
  ldr r0, =0
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar

  b .L1

//alternative
.L1:
  
  ldr r0, =1
  cmp r0, #1
  moveq r0, #'.'
  movne r0, #'F'
  bl putchar


.L1:
