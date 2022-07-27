
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

  ldr r0, =0
  push {r0, ip}
 
// loop start
.L2:

  ldr r0, [fp, #-24]
  push {r0, ip}
  ldr r0, =3
  pop {r1, ip}
  cmp r0, r1 
  moveq r0, #0
  movne r0, #1 

  // did we reach the loop-breaking condition?
  // if yes, branch to .L3
  cmp r0, #0
  beq .L3
 
  
  ldr r0, [fp, #-24]
  push {r0, ip}
  ldr r0, =1
  pop {r1, ip}
  add r0, r0, r1
  str r0, [fp, #-24]

  b .L2
// loop end
.L3:

  ldr r0, [fp, #-24]
  bl putchar

  
  // left expression
  ldr r0, [fp, #-24]
  push {r0, ip}
  // right expression
  ldr r0, =3
  pop {r1, ip}
  // left == right ?
  cmp r0, r1
  moveq r0, #1
  movne r0, #0
  bl assert


  mov sp, fp
  mov r0, #0
  pop {fp, pc}
