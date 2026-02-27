; ModuleID = 'main_module'
source_filename = "main_module"

%"math.Vec_2<f32>" = type { float, float }

define void @main() {
entry:
  %fun_call_tmp = call float @"math.get_x<f32>"(%"math.Vec_2<f32>" { float 5.000000e+00, float 6.000000e+00 })
  ret void
}

define float @"math.get_x<f32>"(%"math.Vec_2<f32>" %vec) {
entry:
  %vec.x = extractvalue %"math.Vec_2<f32>" %vec, 0
  ret float %vec.x
}
