
.global main
main:

  push {fp, lr}
  mov fp, sp
  push {r0, r1, r2, r3}


  mov sp, fp
  mov r0, #0
  pop {fp, pc}
