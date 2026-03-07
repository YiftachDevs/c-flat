; ModuleID = 'main_module'
source_filename = "main_module"

define void @main() {
entry:
  %fun_call_tmp = call i1 @foo()
  br i1 %fun_call_tmp, label %then, label %merge

merge:                                            ; preds = %entry, %then
  ret void

then:                                             ; preds = %entry
  %fun_call_tmp1 = call i1 @foo()
  br label %merge
}

define i1 @foo() {
entry:
  ret i1 false
}
