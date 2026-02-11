; ModuleID = 'main_module'
source_filename = "main_module"

define float @.main() {
entry:
  %var_1 = alloca i8, align 1
  store i8 5, ptr %var_1, align 1
  ret float 4.000000e+00
}
