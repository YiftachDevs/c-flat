; ModuleID = 'main_module'
source_filename = "main_module"

define void @.main() {
entry:
  %tmp_struct_constructor_rename_me = alloca { i32, i32 }, align 8
  %e = alloca i32, align 4
  store i32 3, ptr %e, align 4
  %side_load = load i32, ptr %e, align 4
  %struct_arg_rename_me = getelementptr inbounds nuw { i32, i32 }, ptr %tmp_struct_constructor_rename_me, i32 0, i32 0
  store i32 5, ptr %struct_arg_rename_me, align 4
  %struct_arg_rename_me1 = getelementptr inbounds nuw { i32, i32 }, ptr %tmp_struct_constructor_rename_me, i32 0, i32 1
  store i32 %side_load, ptr %struct_arg_rename_me1, align 4
}
