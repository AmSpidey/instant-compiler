declare i32 @printf(i8*, ...) 
@dnl = internal constant [4 x i8] c"%d\0A\00" 
define void @printInt(i32 %x) { 
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0 
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
       ret void 
}
define i32 @main() {
call void @printInt(i32 65537)
ret i32 0
}