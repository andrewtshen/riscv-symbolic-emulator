; ModuleID = "/Users/andrewshen/Documents/PRIMES2019/lang/codegen.pyc"
target triple = "x86_64-apple-darwin18.6.0"
target datalayout = ""

define void @"main"() 
{
entry:
  %".2" = mul i8 8, 8
  %".3" = bitcast [5 x i8]* @"fstr" to i8*
  %".4" = call i32 (i8*, ...) @"printf"(i8* %".3", i8 %".2")
  ret void
}

declare i32 @"printf"(i8* %".1", ...) 

@"fstr" = internal constant [5 x i8] c"%i \0a\00"