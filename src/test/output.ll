; ModuleID = 'main_module'
source_filename = "main_module"

define void @.main() {
entry:
  %var_1 = alloca float, align 4
  store float 8.000000e+00, ptr %var_1, align 4
}
