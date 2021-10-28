declare i32 @printf(i8*, ...) 
@dnl = internal constant [4 x i8] c"%d\0A\00" 
define void @printInt(i32 %x) { 
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0 
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
       ret void 
}
define i32 @main() {
%reg_0 = add i32 1, 1
%reg_1 = add i32 1, %reg_0
%reg_2 = add i32 1, %reg_1
%reg_3 = add i32 1, %reg_2
%reg_4 = add i32 1, %reg_3
%reg_5 = add i32 1, %reg_4
%reg_6 = add i32 1, %reg_5
%reg_7 = add i32 1, %reg_6
%reg_8 = add i32 1, %reg_7
%reg_9 = add i32 1, %reg_8
%reg_10 = add i32 1, %reg_9
%reg_11 = add i32 1, %reg_10
%reg_12 = add i32 1, %reg_11
%reg_13 = add i32 1, %reg_12
%reg_14 = add i32 1, %reg_13
%reg_15 = add i32 1, %reg_14
%reg_16 = add i32 1, %reg_15
%reg_17 = add i32 1, %reg_16
%reg_18 = add i32 1, %reg_17
%reg_19 = add i32 1, %reg_18
%reg_20 = add i32 1, %reg_19
%reg_21 = add i32 1, %reg_20
%reg_22 = add i32 1, %reg_21
%reg_23 = add i32 1, %reg_22
%reg_24 = add i32 1, %reg_23
%reg_25 = add i32 1, %reg_24
%reg_26 = add i32 1, %reg_25
%reg_27 = add i32 1, %reg_26
%reg_28 = add i32 1, %reg_27
%reg_29 = add i32 1, %reg_28
%reg_30 = add i32 1, %reg_29
%reg_31 = add i32 1, %reg_30
%reg_32 = add i32 1, %reg_31
%reg_33 = add i32 1, %reg_32
%reg_34 = add i32 1, %reg_33
%reg_35 = add i32 1, %reg_34
%reg_36 = add i32 1, %reg_35
%reg_37 = add i32 1, %reg_36
%reg_38 = add i32 1, %reg_37
%reg_39 = add i32 1, %reg_38
%reg_40 = add i32 1, %reg_39
call void @printInt(i32 %reg_40)
ret i32 0
}