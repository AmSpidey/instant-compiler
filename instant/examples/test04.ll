declare i32 @printf(i8*, ...) 
@dnl = internal constant [4 x i8] c"%d\0A\00" 
define void @printInt(i32 %x) { 
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0 
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
       ret void 
}
define i32 @main() {
%ptr_a = alloca i32
%ptr_b = alloca i32
store i32 1, i32* %ptr_a
store i32 2, i32* %ptr_b
%reg_0 = load i32, i32* %ptr_b
%reg_1 = load i32, i32* %ptr_a
%reg_2 = load i32, i32* %ptr_a
%reg_3 = load i32, i32* %ptr_a
%reg_4 = load i32, i32* %ptr_a
%reg_5 = load i32, i32* %ptr_a
%reg_6 = load i32, i32* %ptr_a
%reg_7 = load i32, i32* %ptr_a
%reg_8 = load i32, i32* %ptr_a
%reg_9 = load i32, i32* %ptr_a
%reg_10 = load i32, i32* %ptr_a
%reg_11 = load i32, i32* %ptr_b
%reg_12 = load i32, i32* %ptr_a
%reg_13 = load i32, i32* %ptr_a
%reg_14 = load i32, i32* %ptr_a
%reg_15 = load i32, i32* %ptr_a
%reg_16 = load i32, i32* %ptr_a
%reg_17 = load i32, i32* %ptr_a
%reg_18 = load i32, i32* %ptr_a
%reg_19 = load i32, i32* %ptr_a
%reg_20 = load i32, i32* %ptr_a
%reg_21 = load i32, i32* %ptr_a
%reg_22 = load i32, i32* %ptr_a
%reg_23 = load i32, i32* %ptr_a
%reg_24 = load i32, i32* %ptr_a
%reg_25 = load i32, i32* %ptr_a
%reg_26 = load i32, i32* %ptr_a
%reg_27 = load i32, i32* %ptr_a
%reg_28 = load i32, i32* %ptr_a
%reg_29 = load i32, i32* %ptr_a
%reg_30 = load i32, i32* %ptr_a
%reg_31 = load i32, i32* %ptr_b
%reg_32 = add i32 %reg_30, %reg_31
%reg_33 = add i32 1, %reg_32
%reg_34 = add i32 %reg_29, %reg_33
%reg_35 = add i32 %reg_28, %reg_34
%reg_36 = add i32 1, %reg_35
%reg_37 = add i32 %reg_27, %reg_36
%reg_38 = add i32 %reg_26, %reg_37
%reg_39 = add i32 1, %reg_38
%reg_40 = add i32 %reg_25, %reg_39
%reg_41 = add i32 %reg_24, %reg_40
%reg_42 = add i32 %reg_23, %reg_41
%reg_43 = add i32 %reg_22, %reg_42
%reg_44 = add i32 1, %reg_43
%reg_45 = add i32 %reg_21, %reg_44
%reg_46 = add i32 %reg_20, %reg_45
%reg_47 = add i32 %reg_19, %reg_46
%reg_48 = add i32 %reg_18, %reg_47
%reg_49 = add i32 %reg_17, %reg_48
%reg_50 = add i32 %reg_16, %reg_49
%reg_51 = add i32 %reg_15, %reg_50
%reg_52 = add i32 %reg_14, %reg_51
%reg_53 = add i32 %reg_13, %reg_52
%reg_54 = add i32 %reg_12, %reg_53
%reg_55 = add i32 1, %reg_54
%reg_56 = add i32 %reg_11, %reg_55
%reg_57 = add i32 %reg_10, %reg_56
%reg_58 = add i32 %reg_9, %reg_57
%reg_59 = add i32 %reg_8, %reg_58
%reg_60 = add i32 1, %reg_59
%reg_61 = add i32 %reg_7, %reg_60
%reg_62 = add i32 %reg_6, %reg_61
%reg_63 = add i32 %reg_5, %reg_62
%reg_64 = add i32 %reg_4, %reg_63
%reg_65 = add i32 %reg_3, %reg_64
%reg_66 = add i32 1, %reg_65
%reg_67 = add i32 %reg_2, %reg_66
%reg_68 = add i32 %reg_1, %reg_67
%reg_69 = add i32 %reg_0, %reg_68
call void @printInt(i32 %reg_69)
ret i32 0
}