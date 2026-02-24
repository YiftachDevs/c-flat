; ModuleID = 'main_module'
source_filename = "main_module"

%math.Vec_2 = type { float, float }

define void @main() {
entry:
  %e = alloca float, align 4
  %tmp_struct = alloca %math.Vec_2, align 8
  %tmp_struct.x = getelementptr inbounds nuw %math.Vec_2, ptr %tmp_struct, i32 0, i32 0
  store float 1.000000e+01, ptr %tmp_struct.x, align 4
  %tmp_struct.y = getelementptr inbounds nuw %math.Vec_2, ptr %tmp_struct, i32 0, i32 1
  store float 1.200000e+01, ptr %tmp_struct.y, align 4
  %tmp_struct.x1 = getelementptr inbounds nuw %math.Vec_2, ptr %tmp_struct, i32 0, i32 0
  %tmp_load = load float, ptr %tmp_struct.x1, align 4
  %fun_call_tmp = call float @math.get_vec_2_x(float %tmp_load)
  store float %fun_call_tmp, ptr %e, align 4
}

define float @math.get_vec_2_x(float %0) {
entry:
  %vec = alloca float, align 4
  store float %0, ptr %vec, align 4
  %tmp_load = load float, ptr %vec, align 4
  ret float %tmp_load
}
