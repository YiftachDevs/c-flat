; ModuleID = 'main_module'
source_filename = "main_module"

define void @main() {
entry:
  %fun_call_tmp = call i32 @i32.get(i32 5)
  ret void
}

define i32 @i32.get(i32 %self) {
entry:
  %fun_call_tmp = call i32 @i32.get(i32 %self)
  ret i32 %fun_call_tmp
}
