proc A(k: integer; proc x1(): integer; proc x2(): integer; 
       proc x3(): integer; proc x4(): integer; proc x5(): integer): integer;
  proc B(): integer;
  begin
    k := k-1;
    return A(k, B, x1, x2, x3, x4)
  end;
begin
  if k <= 0 then return x4() + x5() else return B() end
end;

proc One(): integer; begin return 1 end;
proc MOne(): integer; begin return -1 end;
proc Zero(): integer; begin return 0 end;

begin
  print_num(A(10, One, MOne, MOne, One, Zero)); newline()
end.

(*<<
-67
>>*)

(*[[
@ picoPascal compiler output
        .global pmain

@ proc A(k: integer; proc x1(): integer; proc x2(): integer; 
        .text
_A:
        mov ip, sp
        stmfd sp!, {r0-r3}
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@   if k <= 0 then return x4() + x5() else return B() end
        ldr r0, [fp, #40]
        cmp r0, #0
        bgt .L3
        ldr r4, [fp, #72]
        ldr r0, [fp, #68]
        blx r0
        ldr r4, [fp, #80]
        mov r5, r0
        ldr r0, [fp, #76]
        blx r0
        add r0, r5, r0
        b .L1
.L3:
        mov r4, fp
        bl _B
.L1:
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@   proc B(): integer;
_B:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #32
@     k := k-1;
        ldr r0, [fp]
        add r5, r0, #40
        ldr r0, [r5]
        sub r0, r0, #1
        str r0, [r5]
@     return A(k, B, x1, x2, x3, x4)
        ldr r5, [fp]
        ldr r0, [r5, #72]
        str r0, [sp, #24]
        ldr r0, [r5, #68]
        str r0, [sp, #20]
        ldr r0, [r5, #64]
        str r0, [sp, #16]
        ldr r0, [r5, #60]
        str r0, [sp, #12]
        ldr r0, [r5, #56]
        str r0, [sp, #8]
        ldr r0, [r5, #52]
        str r0, [sp, #4]
        ldr r0, [r5, #48]
        str r0, [sp]
        ldr r3, [r5, #44]
        mov r2, r5
        ldr r1, =_B
        ldr r0, [r5, #40]
        bl _A
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc One(): integer; begin return 1 end;
_One:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@ proc One(): integer; begin return 1 end;
        mov r0, #1
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc MOne(): integer; begin return -1 end;
_MOne:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@ proc MOne(): integer; begin return -1 end;
        mov r0, #-1
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ proc Zero(): integer; begin return 0 end;
_Zero:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
@ proc Zero(): integer; begin return 0 end;
        mov r0, #0
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

pmain:
        mov ip, sp
        stmfd sp!, {r4-r10, fp, ip, lr}
        mov fp, sp
        sub sp, sp, #32
@   print_num(A(10, One, MOne, MOne, One, Zero)); newline()
        ldr r5, =_One
        ldr r6, =_MOne
        mov r0, #0
        str r0, [sp, #24]
        ldr r0, =_Zero
        str r0, [sp, #20]
        mov r0, #0
        str r0, [sp, #16]
        str r5, [sp, #12]
        mov r0, #0
        str r0, [sp, #8]
        str r6, [sp, #4]
        mov r0, #0
        str r0, [sp]
        mov r3, r6
        mov r2, #0
        mov r1, r5
        mov r0, #10
        bl _A
        bl print_num
        bl newline
        ldmfd fp, {r4-r10, fp, sp, pc}
        .ltorg

@ End
]]*)
