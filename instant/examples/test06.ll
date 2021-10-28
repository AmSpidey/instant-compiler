declare i32 @printf(i8*, ...) 
@dnl = internal constant [4 x i8] c"%d\0A\00" 
define void @printInt(i32 %x) { 
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0 
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
       ret void 
}
define i32 @main() {
%ptr_g = alloca i32
%ptr_l = alloca i32
%ptr_m = alloca i32
%ptr_j = alloca i32
%ptr_k = alloca i32
%ptr_a = alloca i32
%ptr_n = alloca i32
%ptr_d = alloca i32
%ptr_e = alloca i32
%ptr_b = alloca i32
%ptr_c = alloca i32
%ptr_h = alloca i32
%ptr_i = alloca i32
%ptr_f = alloca i32
store i32 0, i32* %ptr_a
store i32 1, i32* %ptr_b
store i32 0, i32* %ptr_c
store i32 1, i32* %ptr_d
store i32 0, i32* %ptr_e
store i32 1, i32* %ptr_f
store i32 0, i32* %ptr_g
store i32 1, i32* %ptr_h
%reg_0 = load i32, i32* %ptr_a
%reg_1 = load i32, i32* %ptr_b
%reg_2 = mul i32 %reg_0, %reg_1
%reg_3 = load i32, i32* %ptr_c
%reg_4 = load i32, i32* %ptr_d
%reg_5 = mul i32 %reg_3, %reg_4
%reg_6 = load i32, i32* %ptr_e
%reg_7 = load i32, i32* %ptr_f
%reg_8 = load i32, i32* %ptr_g
%reg_9 = load i32, i32* %ptr_h
%reg_10 = add i32 %reg_8, %reg_9
%reg_11 = add i32 %reg_7, %reg_10
%reg_12 = add i32 %reg_6, %reg_11
%reg_13 = add i32 %reg_5, %reg_12
%reg_14 = add i32 %reg_2, %reg_13
call void @printInt(i32 %reg_14)
store i32 1, i32* %ptr_a
store i32 2, i32* %ptr_b
store i32 1, i32* %ptr_c
store i32 2, i32* %ptr_d
store i32 1, i32* %ptr_e
store i32 2, i32* %ptr_f
store i32 1, i32* %ptr_g
store i32 2, i32* %ptr_h
store i32 1, i32* %ptr_i
store i32 2, i32* %ptr_j
store i32 1, i32* %ptr_k
store i32 2, i32* %ptr_l
store i32 1, i32* %ptr_m
store i32 2, i32* %ptr_n
%reg_15 = load i32, i32* %ptr_a
%reg_16 = mul i32 2, %reg_15
%reg_17 = load i32, i32* %ptr_b
%reg_18 = udiv i32 %reg_17, 2
%reg_19 = load i32, i32* %ptr_c
%reg_20 = load i32, i32* %ptr_d
%reg_21 = load i32, i32* %ptr_e
%reg_22 = load i32, i32* %ptr_f
%reg_23 = load i32, i32* %ptr_g
%reg_24 = load i32, i32* %ptr_h
%reg_25 = load i32, i32* %ptr_i
%reg_26 = load i32, i32* %ptr_j
%reg_27 = udiv i32 %reg_26, 2
%reg_28 = load i32, i32* %ptr_k
%reg_29 = load i32, i32* %ptr_l
%reg_30 = load i32, i32* %ptr_m
%reg_31 = load i32, i32* %ptr_n
%reg_32 = add i32 %reg_30, %reg_31
%reg_33 = add i32 %reg_29, %reg_32
%reg_34 = add i32 %reg_28, %reg_33
%reg_35 = add i32 %reg_27, %reg_34
%reg_36 = add i32 %reg_25, %reg_35
%reg_37 = add i32 %reg_24, %reg_36
%reg_38 = add i32 %reg_23, %reg_37
%reg_39 = add i32 %reg_22, %reg_38
%reg_40 = add i32 %reg_21, %reg_39
%reg_41 = add i32 %reg_20, %reg_40
%reg_42 = add i32 %reg_19, %reg_41
%reg_43 = add i32 %reg_18, %reg_42
%reg_44 = add i32 %reg_16, %reg_43
%reg_45 = udiv i32 %reg_44, 10
call void @printInt(i32 %reg_45)
ret i32 0
}