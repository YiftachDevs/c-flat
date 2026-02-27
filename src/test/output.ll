; ModuleID = 'main_module'
source_filename = "main_module"

%"Pair<f64, Pair<bool, i32>>" = type { double, %"Pair<bool, i32>" }
%"Pair<bool, i32>" = type { i1, i32 }

define %"Pair<f64, Pair<bool, i32>>" @main() {
entry:
  %pair = alloca %"Pair<f64, Pair<bool, i32>>", align 8
  %tmp_struct1 = alloca %"Pair<f64, Pair<bool, i32>>", align 8
  %tmp_struct = alloca %"Pair<bool, i32>", align 8
  %tmp_struct.item_1 = getelementptr inbounds nuw %"Pair<bool, i32>", ptr %tmp_struct, i32 0, i32 0
  store i1 true, ptr %tmp_struct.item_1, align 1
  %tmp_struct.item_2 = getelementptr inbounds nuw %"Pair<bool, i32>", ptr %tmp_struct, i32 0, i32 1
  store i32 4, ptr %tmp_struct.item_2, align 4
  %tmp_load = load %"Pair<bool, i32>", ptr %tmp_struct, align 4
  %tmp_struct1.item_1 = getelementptr inbounds nuw %"Pair<f64, Pair<bool, i32>>", ptr %tmp_struct1, i32 0, i32 0
  store double 4.000000e+00, ptr %tmp_struct1.item_1, align 8
  %tmp_struct1.item_2 = getelementptr inbounds nuw %"Pair<f64, Pair<bool, i32>>", ptr %tmp_struct1, i32 0, i32 1
  store %"Pair<bool, i32>" %tmp_load, ptr %tmp_struct1.item_2, align 4
  %tmp_load2 = load %"Pair<f64, Pair<bool, i32>>", ptr %tmp_struct1, align 8
  store %"Pair<f64, Pair<bool, i32>>" %tmp_load2, ptr %pair, align 8
  %tmp_load3 = load %"Pair<f64, Pair<bool, i32>>", ptr %pair, align 8
  ret %"Pair<f64, Pair<bool, i32>>" %tmp_load3
}
