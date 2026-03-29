; ModuleID = 'main_module'
source_filename = "main_module"

%"Box:<T = i32>" = type { i32 }

define i32 @main() {
entry:
  %box = alloca %"Box:<T = i32>", align 8
  store i32 4, ptr %box, align 4
  %fun_call_tmp1 = call ptr @"Box:<T = i32>.as_ref"(ptr nonnull %box)
  %fun_call_tmp2 = call i32 @do_something(ptr %fun_call_tmp1)
  %fun_call_tmp3 = call {} @"Box:<T = i32>.drop"(ptr nonnull %box)
  ret i32 %fun_call_tmp2
}

define i32 @do_something(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  %tmp.i = shl i32 %tmp1, 1
  ret i32 %tmp.i
}

define ptr @"Box:<T = i32>.as_ref"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"Box:<T = i32>.deref"(ptr %0)
  ret ptr %fun_call_tmp
}

define ptr @"Box:<T = i32>.deref"(ptr %0) {
entry:
  ret ptr %0
}

define {} @"Box:<T = i32>.drop"(ptr %0) {
entry:
  ret {} zeroinitializer
}
