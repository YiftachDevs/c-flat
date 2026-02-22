; ModuleID = 'main_module'
source_filename = "main_module"

define i32 @.main() {
entry:
  %result = alloca i32, align 4
  %fun_call_tmp = call i32 @math.foo(i32 4)
  store i32 %fun_call_tmp, ptr %result, align 4
  %side_load = load i32, ptr %result, align 4
  ret i32 %side_load
}

define i32 @math.foo(i32 %0) {
entry:
  %a = alloca i32, align 4
  store i32 %0, ptr %a, align 4
  %side_load = load i32, ptr %a, align 4
  ret i32 %side_load
}
