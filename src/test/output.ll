; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:SliceMutIter<T = File>" = type { ptr, ptr }
%"[c-flat]:Range<T = i32>" = type { i32, i32 }
%"[c-flat]:Vec<T = File>" = type { ptr, i64, i64 }
%"[c-flat]:Player" = type { i32, %"[c-flat]:Vec_2<T = f64>", %"[c-flat]:Vec_2<T = f64>", %"[c-flat]:Vec_2<T = u64>" }
%"[c-flat]:Vec_2<T = f64>" = type { double, double }
%"[c-flat]:Vec_2<T = u64>" = type { i64, i64 }
%"[c-flat]:pixel:Game" = type { %"[c-flat]:Vec<T = char>", %"[c-flat]:Vec_2<T = u64>", i64, %"[c-flat]:Vec_2<T = u64>", %"[c-flat]:Vec<T = Vec<T = char>>" }
%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:Vec<T = Vec<T = char>>" = type { ptr, i64, i64 }
%"[c-flat]:Particle" = type { %"[c-flat]:Vec_2<T = f64>", double, double, double, double, i32 }
%"[c-flat]:Vec_2<T = i32>" = type { i32, i32 }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }
%"[c-flat]:SliceIter<T = Vec<T = char>>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:mem:Manually_Drop<T = File>" = type { %"[c-flat]:Particle" }
%"[c-flat]:Range<T = f64>" = type { double, double }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [14 x i8] c"My First Game\00", align 1
@global_string.29 = private unnamed_addr constant [14 x i8] c"res/slime.png\00", align 1
@"[c-flat]:seed" = internal global i64 91237
@global_string.86 = private unnamed_addr constant [10 x i8] c"game over\00", align 1
@global_string.87 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @"[c-flat]:game"()
  %fun_call_tmp1 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.86, i64 9 })
  %fun_call_tmp2 = call {} @"[c-flat]:debug_heap_count"()
  ret i64 0
}

define {} @"[c-flat]:game"() {
entry:
  %tmp_iter7 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %tmp_iter6 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %tmp_iter5 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %tmp_iter4 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %i = alloca i32, align 4
  %tmp_iter3 = alloca %"[c-flat]:Range<T = i32>", align 8
  %particles = alloca %"[c-flat]:Vec<T = File>", align 8
  %player = alloca %"[c-flat]:Player", align 8
  %game = alloca %"[c-flat]:pixel:Game", align 8
  %fun_call_tmp = call %"[c-flat]:pixel:Game" @"[c-flat]:pixel:Game:new"({ ptr, i64 } { ptr @global_string, i64 13 }, %"[c-flat]:Vec_2<T = u64>" { i64 480, i64 270 }, i64 4)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 0
  %fun_call_tmp.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 0
  store ptr %fun_call_tmp.elt.elt, ptr %game, align 8
  %game.repack188 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %fun_call_tmp.elt.elt189 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 1
  store i64 %fun_call_tmp.elt.elt189, ptr %game.repack188, align 8
  %game.repack190 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp.elt.elt191 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 2
  store i64 %fun_call_tmp.elt.elt191, ptr %game.repack190, align 8
  %game.repack180 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %fun_call_tmp.elt181 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 1
  %fun_call_tmp.elt181.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt181, 0
  store i64 %fun_call_tmp.elt181.elt, ptr %game.repack180, align 8
  %game.repack180.repack192 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %fun_call_tmp.elt181.elt193 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt181, 1
  store i64 %fun_call_tmp.elt181.elt193, ptr %game.repack180.repack192, align 8
  %game.repack182 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp.elt183 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt183, ptr %game.repack182, align 8
  %game.repack184 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %fun_call_tmp.elt185 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 3
  %fun_call_tmp.elt185.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt185, 0
  store i64 %fun_call_tmp.elt185.elt, ptr %game.repack184, align 8
  %game.repack184.repack194 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %fun_call_tmp.elt185.elt195 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt185, 1
  store i64 %fun_call_tmp.elt185.elt195, ptr %game.repack184.repack194, align 8
  %game.repack186 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %fun_call_tmp.elt187 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 4
  %fun_call_tmp.elt187.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt187, 0
  store ptr %fun_call_tmp.elt187.elt, ptr %game.repack186, align 8
  %game.repack186.repack196 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %fun_call_tmp.elt187.elt197 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt187, 1
  store i64 %fun_call_tmp.elt187.elt197, ptr %game.repack186.repack196, align 8
  %game.repack186.repack198 = getelementptr inbounds nuw i8, ptr %game, i64 80
  %fun_call_tmp.elt187.elt199 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt187, 2
  store i64 %fun_call_tmp.elt187.elt199, ptr %game.repack186.repack198, align 8
  %fun_call_tmp1 = call %"[c-flat]:Player" @"[c-flat]:File:new"(ptr nonnull %game)
  store %"[c-flat]:Player" %fun_call_tmp1, ptr %player, align 8
  %fun_call_tmp2 = call %"[c-flat]:Vec<T = File>" @"[c-flat]:Vec<T = File>:new"()
  %fun_call_tmp2.elt = extractvalue %"[c-flat]:Vec<T = File>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %particles, align 8
  %particles.repack200 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %fun_call_tmp2.elt201 = extractvalue %"[c-flat]:Vec<T = File>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt201, ptr %particles.repack200, align 8
  %particles.repack202 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %fun_call_tmp2.elt203 = extractvalue %"[c-flat]:Vec<T = File>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt203, ptr %particles.repack202, align 8
  store i32 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack204 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 4
  store i32 5, ptr %tmp_iter3.repack204, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp5 = call i32 @"[c-flat]:Range<T = i32>:next"(ptr nonnull %tmp_iter3)
  store i32 %fun_call_tmp5, ptr %i, align 4
  %game.pixel_frame_size = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp.unpack = load i64, ptr %game.pixel_frame_size, align 4
  %0 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp.unpack, 0
  %tmp.elt249 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp.unpack250 = load i64, ptr %tmp.elt249, align 4
  %tmp251 = insertvalue %"[c-flat]:Vec_2<T = u64>" %0, i64 %tmp.unpack250, 1
  %fun_call_tmp6 = call %"[c-flat]:Particle" @"[c-flat]:File:spawn"(%"[c-flat]:Vec_2<T = u64>" %tmp251)
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = File>:push"(ptr nonnull %particles, %"[c-flat]:Particle" %fun_call_tmp6)
  %tmp8 = load i32, ptr %i, align 4
  %fun_call_tmp9 = call {} @"[c-flat]:i32:drop"(i32 %tmp8)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp4 = call i1 @"[c-flat]:Range<T = i32>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp4, label %then, label %cond12

cond12:                                           ; preds = %cond, %else95
  %fun_call_tmp14 = call i1 @"[c-flat]:pixel:Game:should_close"(ptr nonnull %game)
  br i1 %fun_call_tmp14, label %else13, label %cond18

else13:                                           ; preds = %cond12
  %fun_call_tmp123 = call {} @"[c-flat]:pixel:Game:close"(ptr nonnull %game)
  %tmp124.unpack = load i32, ptr %tmp_iter3, align 4
  %tmp124.elt205 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 4
  %tmp124.unpack206 = load i32, ptr %tmp124.elt205, align 4
  %fun_call_tmp.i155 = call {} @"[c-flat]:i32:drop"(i32 %tmp124.unpack206)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp124.unpack)
  %tmp126.unpack = load ptr, ptr %particles, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = File>" poison, ptr %tmp126.unpack, 0
  %tmp126.elt208 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %tmp126.unpack209 = load i64, ptr %tmp126.elt208, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = File>" %1, i64 %tmp126.unpack209, 1
  %tmp126.elt210 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %tmp126.unpack211 = load i64, ptr %tmp126.elt210, align 8
  %tmp126212 = insertvalue %"[c-flat]:Vec<T = File>" %2, i64 %tmp126.unpack211, 2
  %fun_call_tmp127 = call {} @"[c-flat]:Vec<T = File>:drop.84"(%"[c-flat]:Vec<T = File>" %tmp126212)
  %tmp128 = load %"[c-flat]:Player", ptr %player, align 8
  %tmp.i145 = extractvalue %"[c-flat]:Player" %tmp128, 3
  %tmp.i.i146 = extractvalue %"[c-flat]:Vec_2<T = u64>" %tmp.i145, 1
  %fun_call_tmp.i.i147 = call {} @"[c-flat]:u64:drop"(i64 %tmp.i.i146)
  %tmp1.i.i148 = extractvalue %"[c-flat]:Vec_2<T = u64>" %tmp.i145, 0
  %fun_call_tmp2.i.i149 = call {} @"[c-flat]:u64:drop"(i64 %tmp1.i.i148)
  %tmp1.i150 = extractvalue %"[c-flat]:Player" %tmp128, 2
  %tmp.i13.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp1.i150, 1
  %fun_call_tmp.i14.i = call {} @"[c-flat]:f64:drop"(double %tmp.i13.i)
  %tmp1.i15.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp1.i150, 0
  %fun_call_tmp2.i16.i = call {} @"[c-flat]:f64:drop"(double %tmp1.i15.i)
  %tmp3.i151 = extractvalue %"[c-flat]:Player" %tmp128, 1
  %tmp.i8.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp3.i151, 1
  %fun_call_tmp.i9.i = call {} @"[c-flat]:f64:drop"(double %tmp.i8.i)
  %tmp1.i10.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp3.i151, 0
  %fun_call_tmp2.i11.i = call {} @"[c-flat]:f64:drop"(double %tmp1.i10.i)
  %tmp5.i152 = extractvalue %"[c-flat]:Player" %tmp128, 0
  %fun_call_tmp6.i = call {} @"[c-flat]:i32:drop"(i32 %tmp5.i152)
  %tmp130.unpack.unpack = load ptr, ptr %game, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp130.unpack.unpack, 0
  %tmp130.unpack.elt222 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %tmp130.unpack.unpack223 = load i64, ptr %tmp130.unpack.elt222, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp130.unpack.unpack223, 1
  %tmp130.unpack.elt224 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %tmp130.unpack.unpack225 = load i64, ptr %tmp130.unpack.elt224, align 8
  %tmp130.unpack226 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp130.unpack.unpack225, 2
  %tmp130.elt213 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp130.unpack214.unpack = load i64, ptr %tmp130.elt213, align 8
  %tmp130.unpack214.elt227 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp130.unpack214.unpack228 = load i64, ptr %tmp130.unpack214.elt227, align 8
  %tmp130.elt215 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %tmp130.unpack216 = load i64, ptr %tmp130.elt215, align 8
  %tmp130.elt217 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %tmp130.unpack218.unpack = load i64, ptr %tmp130.elt217, align 8
  %tmp130.unpack218.elt230 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %tmp130.unpack218.unpack231 = load i64, ptr %tmp130.unpack218.elt230, align 8
  %tmp130.elt219 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %tmp130.unpack220.unpack = load ptr, ptr %tmp130.elt219, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" poison, ptr %tmp130.unpack220.unpack, 0
  %tmp130.unpack220.elt233 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %tmp130.unpack220.unpack234 = load i64, ptr %tmp130.unpack220.elt233, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %5, i64 %tmp130.unpack220.unpack234, 1
  %tmp130.unpack220.elt235 = getelementptr inbounds nuw i8, ptr %game, i64 80
  %tmp130.unpack220.unpack236 = load i64, ptr %tmp130.unpack220.elt235, align 8
  %tmp130.unpack220237 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %6, i64 %tmp130.unpack220.unpack236, 2
  %fun_call_tmp.i = call {} @"[c-flat]:Vec<T = Vec<T = char>>:drop.25"(%"[c-flat]:Vec<T = Vec<T = char>>" %tmp130.unpack220237)
  %fun_call_tmp.i11.i = call {} @"[c-flat]:u64:drop"(i64 %tmp130.unpack218.unpack231)
  %fun_call_tmp2.i13.i = call {} @"[c-flat]:u64:drop"(i64 %tmp130.unpack218.unpack)
  %fun_call_tmp4.i = call {} @"[c-flat]:u64:drop"(i64 %tmp130.unpack216)
  %fun_call_tmp.i.i = call {} @"[c-flat]:u64:drop"(i64 %tmp130.unpack214.unpack228)
  %fun_call_tmp2.i.i = call {} @"[c-flat]:u64:drop"(i64 %tmp130.unpack214.unpack)
  %fun_call_tmp8.i = call {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %tmp130.unpack226)
  ret {} zeroinitializer

then17:                                           ; preds = %cond18
  %fun_call_tmp21 = call %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:pixel:Game:mouse_pos"(ptr nonnull %game)
  %fun_call_tmp22 = call %"[c-flat]:Vec_2<T = f64>" @"[c-flat]:Range<T = i32>:as<T2 = f64>"(%"[c-flat]:Vec_2<T = i32>" %fun_call_tmp21)
  %fun_call_tmp23 = call i32 @"[c-flat]:rand_color"()
  %fun_call_tmp24 = call %"[c-flat]:Particle" @"[c-flat]:File:spawn_at"(%"[c-flat]:Vec_2<T = f64>" %fun_call_tmp22, i32 %fun_call_tmp23)
  %fun_call_tmp25 = call {} @"[c-flat]:Vec<T = File>:push"(ptr nonnull %particles, %"[c-flat]:Particle" %fun_call_tmp24)
  br label %cond29

cond18:                                           ; preds = %cond12
  %fun_call_tmp20 = call i1 @"[c-flat]:pixel:Game:is_mouse_pressed"(ptr nonnull %game, i32 0)
  br i1 %fun_call_tmp20, label %then17, label %cond29

then28:                                           ; preds = %cond29
  %fun_call_tmp33 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp34 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp33)
  %fun_call_tmp34.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp34, 0
  store ptr %fun_call_tmp34.elt, ptr %tmp_iter4, align 8
  %tmp_iter4.repack238 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %fun_call_tmp34.elt239 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp34, 1
  store ptr %fun_call_tmp34.elt239, ptr %tmp_iter4.repack238, align 8
  br label %cond36

cond29:                                           ; preds = %cond18, %then17
  %fun_call_tmp31 = call i1 @"[c-flat]:pixel:Game:is_mouse_pressed"(ptr nonnull %game, i32 2)
  br i1 %fun_call_tmp31, label %then28, label %cond50

then35:                                           ; preds = %cond36
  %fun_call_tmp39 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter4)
  %tmp40.new_speed = getelementptr inbounds nuw i8, ptr %fun_call_tmp39, i64 32
  %tmp41.speed = getelementptr inbounds nuw i8, ptr %fun_call_tmp39, i64 24
  %tmp42 = load double, ptr %tmp41.speed, align 8
  %tmp.i142 = fneg double %tmp42
  store double %tmp.i142, ptr %tmp40.new_speed, align 8
  br label %cond36

cond36:                                           ; preds = %then35, %then28
  %fun_call_tmp38 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp38, label %then35, label %cond50

merge48:                                          ; preds = %cond57, %cond50
  %fun_call_tmp70 = call {} @"[c-flat]:File:update"(ptr nonnull %player, ptr nonnull %game, ptr nonnull %particles)
  %fun_call_tmp72 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp73 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp72)
  %fun_call_tmp73.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp73, 0
  store ptr %fun_call_tmp73.elt, ptr %tmp_iter6, align 8
  %tmp_iter6.repack242 = getelementptr inbounds nuw i8, ptr %tmp_iter6, i64 8
  %fun_call_tmp73.elt243 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp73, 1
  store ptr %fun_call_tmp73.elt243, ptr %tmp_iter6.repack242, align 8
  br label %cond75

then49:                                           ; preds = %cond50
  %fun_call_tmp54 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp55 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp54)
  %fun_call_tmp55.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp55, 0
  store ptr %fun_call_tmp55.elt, ptr %tmp_iter5, align 8
  %tmp_iter5.repack240 = getelementptr inbounds nuw i8, ptr %tmp_iter5, i64 8
  %fun_call_tmp55.elt241 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp55, 1
  store ptr %fun_call_tmp55.elt241, ptr %tmp_iter5.repack240, align 8
  br label %cond57

cond50:                                           ; preds = %cond36, %cond29
  %fun_call_tmp52 = call i1 @"[c-flat]:pixel:Game:is_key_down"(ptr nonnull %game, i8 70)
  br i1 %fun_call_tmp52, label %then49, label %merge48

then56:                                           ; preds = %cond57
  %fun_call_tmp60 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter5)
  %tmp62.new_speed = getelementptr inbounds nuw i8, ptr %fun_call_tmp60, i64 32
  %tmp63 = load double, ptr %tmp62.new_speed, align 8
  %tmp.i140 = fmul double %tmp63, 1.010000e+00
  %tmp65.new_speed = getelementptr inbounds nuw i8, ptr %fun_call_tmp60, i64 32
  store double %tmp.i140, ptr %tmp65.new_speed, align 8
  br label %cond57

cond57:                                           ; preds = %then56, %then49
  %fun_call_tmp59 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter5)
  br i1 %fun_call_tmp59, label %then56, label %merge48

then74:                                           ; preds = %cond75
  %fun_call_tmp78 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter6)
  %game.pixel_frame_size81 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp82.unpack = load i64, ptr %game.pixel_frame_size81, align 4
  %7 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp82.unpack, 0
  %tmp82.elt246 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp82.unpack247 = load i64, ptr %tmp82.elt246, align 4
  %tmp82248 = insertvalue %"[c-flat]:Vec_2<T = u64>" %7, i64 %tmp82.unpack247, 1
  %fun_call_tmp83 = call {} @"[c-flat]:File:update.82"(ptr %fun_call_tmp78, %"[c-flat]:Vec_2<T = u64>" %tmp82248)
  br label %cond75

cond75:                                           ; preds = %then74, %merge48
  %fun_call_tmp77 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter6)
  br i1 %fun_call_tmp77, label %then74, label %else76

else76:                                           ; preds = %cond75
  %fun_call_tmp87 = call {} @"[c-flat]:pixel:Game:begin_draw"(ptr nonnull %game)
  %fun_call_tmp88 = call {} @"[c-flat]:pixel:Game:clear_background"(ptr nonnull %game)
  %fun_call_tmp89 = call {} @"[c-flat]:File:render"(ptr nonnull %player, ptr nonnull %game)
  %fun_call_tmp91 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp92 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp91)
  %fun_call_tmp92.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp92, 0
  store ptr %fun_call_tmp92.elt, ptr %tmp_iter7, align 8
  %tmp_iter7.repack244 = getelementptr inbounds nuw i8, ptr %tmp_iter7, i64 8
  %fun_call_tmp92.elt245 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp92, 1
  store ptr %fun_call_tmp92.elt245, ptr %tmp_iter7.repack244, align 8
  br label %cond94

then93:                                           ; preds = %cond94
  %fun_call_tmp97 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter7)
  %fun_call_tmp100 = call {} @"[c-flat]:File:render.83"(ptr %fun_call_tmp97, ptr nonnull %game)
  br label %cond94

cond94:                                           ; preds = %then93, %else76
  %fun_call_tmp96 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter7)
  br i1 %fun_call_tmp96, label %then93, label %else95

else95:                                           ; preds = %cond94
  %game.pixel_frame_size104 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp105 = load i64, ptr %game.pixel_frame_size104, align 4
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = u64>" undef, i64 %tmp105, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = u64>" %tmp_agg, i64 11, 1
  %fun_call_tmp106 = call {} @"[c-flat]:pixel:Game:draw_rect"(ptr nonnull %game, %"[c-flat]:Vec_2<T = i32>" zeroinitializer, %"[c-flat]:Vec_2<T = u64>" %tmp_constructor, i32 -11184896)
  %fun_call_tmp107 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %game)
  %fun_call_tmp112 = call {} @"[c-flat]:pixel:Game:draw_text"(ptr nonnull %game, { ptr, i64 } %fun_call_tmp107, %"[c-flat]:Vec_2<T = i32>" { i32 32, i32 -1 }, i32 -1)
  %fun_call_tmp113 = call {} @"[c-flat]:pixel:Game:end_draw"(ptr nonnull %game)
  br label %cond12
}

define i1 @"[c-flat]:&char:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define i64 @"[c-flat]:u64:clone"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  ret i64 %tmp1
}

define {} @"[c-flat]:u64:drop"(i64 %0) {
entry:
  ret {} zeroinitializer
}

define i1 @"[c-flat]:&Vec<T = char>:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr %tmp1)
  %tmp3 = load ptr, ptr %1, align 8
  %fun_call_tmp4 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr %tmp3)
  %fun_call_tmp5 = call i1 @"[c-flat]:[char]:eq"({ ptr, i64 } %fun_call_tmp, { ptr, i64 } %fun_call_tmp4)
  ret i1 %fun_call_tmp5
}

define { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = char>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define i1 @"[c-flat]:[char]:eq"({ ptr, i64 } %0, { ptr, i64 } %1) {
entry:
  %idx = alloca i64, align 8
  %other = alloca { ptr, i64 }, align 8
  %self = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack46 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt47 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt47, ptr %self.repack46, align 8
  %.elt48 = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt48, ptr %other, align 8
  %other.repack49 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %.elt50 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt50, ptr %other.repack49, align 8
  br label %cond

common.ret:                                       ; preds = %then4, %else
  %common.ret.op = phi i1 [ true, %else ], [ false, %then4 ]
  ret i1 %common.ret.op

cond:                                             ; preds = %else6, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i32, %else6 ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1.elt51 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp1.unpack52 = load i64, ptr %tmp1.elt51, align 8
  %tmp.i35 = icmp ult i64 %storemerge, %tmp1.unpack52
  br i1 %tmp.i35, label %cond5, label %else

else:                                             ; preds = %cond
  %tmp24 = load i64, ptr %idx, align 4
  %fun_call_tmp25 = call {} @"[c-flat]:u64:drop"(i64 %tmp24)
  br label %common.ret

then4:                                            ; preds = %cond5
  %tmp15 = load i64, ptr %idx, align 4
  %fun_call_tmp16 = call {} @"[c-flat]:u64:drop"(i64 %tmp15)
  br label %common.ret

cond5:                                            ; preds = %cond
  %tmp7.unpack = load ptr, ptr %self, align 8
  %tmp8 = load i64, ptr %idx, align 4
  %tmp_index.i41 = getelementptr i8, ptr %tmp7.unpack, i64 %tmp8
  %tmp10.unpack = load ptr, ptr %other, align 8
  %tmp11 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp10.unpack, i64 %tmp11
  %tmp.i = load i8, ptr %tmp_index.i41, align 1
  %tmp1.i = load i8, ptr %tmp_index.i, align 1
  %tmp2.i.not = icmp eq i8 %tmp.i, %tmp1.i
  br i1 %tmp2.i.not, label %else6, label %then4

else6:                                            ; preds = %cond5
  %tmp21 = load i64, ptr %idx, align 4
  %tmp.i32 = add i64 %tmp21, 1
  br label %cond
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:clone"(ptr %0) {
entry:
  %tmp_iter2 = alloca %"[c-flat]:SliceIter<T = char>", align 8
  %clone = alloca %"[c-flat]:Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %clone, align 8
  %clone.repack22 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %fun_call_tmp.elt23 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt23, ptr %clone.repack22, align 8
  %clone.repack24 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %fun_call_tmp.elt25 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt25, ptr %clone.repack24, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp1 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr %tmp)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp1, 1
  %fun_call_tmp3 = call {} @"[c-flat]:Vec<T = char>:reserve"(ptr nonnull %clone, i64 %len.i)
  %fun_call_tmp5 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr %tmp)
  %fun_call_tmp6 = call %"[c-flat]:SliceIter<T = char>" @"[c-flat]:[char]:iter"({ ptr, i64 } %fun_call_tmp5)
  %fun_call_tmp6.elt = extractvalue %"[c-flat]:SliceIter<T = char>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack26 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp6.elt27 = extractvalue %"[c-flat]:SliceIter<T = char>" %fun_call_tmp6, 1
  store ptr %fun_call_tmp6.elt27, ptr %tmp_iter2.repack26, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp8 = call ptr @"[c-flat]:Vec<T = char>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call i8 @"[c-flat]:char:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %clone, i8 %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"[c-flat]:Vec<T = char>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp7, label %then, label %else

else:                                             ; preds = %cond
  %tmp14.unpack = load ptr, ptr %clone, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp14.unpack, 0
  %tmp14.elt28 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %tmp14.unpack29 = load i64, ptr %tmp14.elt28, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp14.unpack29, 1
  %tmp14.elt30 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %tmp14.unpack31 = load i64, ptr %tmp14.elt30, align 8
  %tmp1432 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp14.unpack31, 2
  ret %"[c-flat]:Vec<T = char>" %tmp1432
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:new"() {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:with_capacity"(i64 4)
  ret %"[c-flat]:Vec<T = char>" %fun_call_tmp
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:alloc<T = char>"(i64 %0)
  %tmp_agg = insertvalue %"[c-flat]:Vec<T = char>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"[c-flat]:Vec<T = char>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"[c-flat]:Vec<T = char>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"[c-flat]:u64:drop"(i64 %0)
  ret %"[c-flat]:Vec<T = char>" %tmp_constructor
}

define ptr @"[c-flat]:mem:alloc<T = char>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp6 = call ptr @c_malloc(i64 %0)
  %tmp7 = load i64, ptr %len, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:u64:drop"(i64 %tmp7)
  ret ptr %fun_call_tmp6
}

define ptr @"[c-flat]:mem:heap_count"() {
entry:
  %tmp = load i64, ptr @"[c-flat]:count", align 4
  %fun_call_tmp = call {} @"[c-flat]:u64:drop"(i64 %tmp)
  ret ptr @"[c-flat]:count"
}

declare ptr @c_malloc(i64)

define {} @"[c-flat]:Vec<T = char>:reserve"(ptr %0, i64 %1) {
entry:
  %new_ptr = alloca ptr, align 8
  %new_len = alloca i64, align 8
  %additional = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i64 %1, ptr %additional, align 4
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i = add i64 %tmp1, %1
  store i64 %tmp.i, ptr %new_len, align 4
  %tmp3 = load i64, ptr %new_len, align 4
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.capacity = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp5 = load i64, ptr %tmp4.capacity, align 4
  %tmp.i51 = icmp ugt i64 %tmp3, %tmp5
  br i1 %tmp.i51, label %cond9, label %merge

merge:                                            ; preds = %entry, %else10
  ret {} zeroinitializer

then8:                                            ; preds = %cond9
  %tmp15 = load ptr, ptr %self, align 8
  %tmp15.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  %tmp16 = load i64, ptr %tmp15.capacity, align 4
  %tmp.i45 = shl i64 %tmp16, 1
  %tmp18.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  store i64 %tmp.i45, ptr %tmp18.capacity, align 4
  br label %cond9

cond9:                                            ; preds = %entry, %then8
  %tmp11 = load i64, ptr %new_len, align 4
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.capacity = getelementptr inbounds nuw i8, ptr %tmp12, i64 16
  %tmp13 = load i64, ptr %tmp12.capacity, align 4
  %tmp.i48 = icmp ugt i64 %tmp11, %tmp13
  br i1 %tmp.i48, label %then8, label %else10

else10:                                           ; preds = %cond9
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.capacity = getelementptr inbounds nuw i8, ptr %tmp19, i64 16
  %tmp20 = load i64, ptr %tmp19.capacity, align 4
  %fun_call_tmp21 = call ptr @"[c-flat]:mem:alloc<T = char>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:mem:copy_range<T = char>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"[c-flat]:mem:free<T = char>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  %tmp35 = load i64, ptr %new_len, align 4
  %fun_call_tmp36 = call {} @"[c-flat]:u64:drop"(i64 %tmp35)
  %tmp37 = load i64, ptr %additional, align 4
  %fun_call_tmp38 = call {} @"[c-flat]:u64:drop"(i64 %tmp37)
  br label %merge
}

define {} @"[c-flat]:mem:copy_range<T = char>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %2)
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %2)
  ret {} zeroinitializer
}

declare {} @c_memcpy(ptr, ptr, i64)

define {} @"[c-flat]:mem:free<T = char>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

declare {} @c_free(ptr)

define %"[c-flat]:SliceIter<T = char>" @"[c-flat]:[char]:iter"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i = add i64 %.elt21, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"[c-flat]:SliceIter<T = char>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg, ptr %cast7, 1
  ret %"[c-flat]:SliceIter<T = char>" %tmp_constructor
}

define i1 @"[c-flat]:Vec<T = char>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"[c-flat]:Vec<T = char>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 1
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"[c-flat]:Vec<T = char>:push"(ptr %0, i8 %1) {
entry:
  %value = alloca i8, align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 %1, ptr %value, align 1
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = char>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i23 = add i64 %tmp4, %cast
  %cast8 = inttoptr i64 %tmp.i23 to ptr
  %fun_call_tmp9 = call {} @"[c-flat]:mem:copy<T = char>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load i8, ptr %value, align 1
  %tmp1528 = insertvalue %"[c-flat]:mem:Manually_Drop<T = char>" poison, i8 %tmp14, 0
  %fun_call_tmp16 = call {} @"[c-flat]:mem:Manually_Drop<T = char>:drop"(%"[c-flat]:mem:Manually_Drop<T = char>" %tmp1528)
  %tmp17 = load i8, ptr %value, align 1
  %fun_call_tmp18 = call {} @"[c-flat]:char:drop"(i8 %tmp17)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:copy<T = char>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define i8 @"[c-flat]:char:clone"(ptr %0) {
entry:
  %tmp1 = load i8, ptr %0, align 1
  ret i8 %tmp1
}

define {} @"[c-flat]:mem:Manually_Drop<T = char>:drop"(%"[c-flat]:mem:Manually_Drop<T = char>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"[c-flat]:char:drop"(i8 %0) {
entry:
  ret {} zeroinitializer
}

define %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:clone"(ptr %0) {
entry:
  %tmp_iter2 = alloca %"[c-flat]:SliceIter<T = Vec<T = char>>", align 8
  %clone = alloca %"[c-flat]:Vec<T = Vec<T = char>>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:new"()
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %clone, align 8
  %clone.repack22 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %fun_call_tmp.elt23 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt23, ptr %clone.repack22, align 8
  %clone.repack24 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %fun_call_tmp.elt25 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt25, ptr %clone.repack24, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp1 = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref"(ptr %tmp)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp1, 1
  %fun_call_tmp3 = call {} @"[c-flat]:Vec<T = Vec<T = char>>:reserve"(ptr nonnull %clone, i64 %len.i)
  %fun_call_tmp5 = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref"(ptr %tmp)
  %fun_call_tmp6 = call %"[c-flat]:SliceIter<T = Vec<T = char>>" @"[c-flat]:[Vec<T = char>]:iter"({ ptr, i64 } %fun_call_tmp5)
  %fun_call_tmp6.elt = extractvalue %"[c-flat]:SliceIter<T = Vec<T = char>>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack26 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp6.elt27 = extractvalue %"[c-flat]:SliceIter<T = Vec<T = char>>" %fun_call_tmp6, 1
  store ptr %fun_call_tmp6.elt27, ptr %tmp_iter2.repack26, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp8 = call ptr @"[c-flat]:Vec<T = Vec<T = char>>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"[c-flat]:Vec<T = Vec<T = char>>:push"(ptr nonnull %clone, %"[c-flat]:Vec<T = char>" %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"[c-flat]:Vec<T = Vec<T = char>>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp7, label %then, label %else

else:                                             ; preds = %cond
  %tmp14.unpack = load ptr, ptr %clone, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" poison, ptr %tmp14.unpack, 0
  %tmp14.elt28 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %tmp14.unpack29 = load i64, ptr %tmp14.elt28, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %1, i64 %tmp14.unpack29, 1
  %tmp14.elt30 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %tmp14.unpack31 = load i64, ptr %tmp14.elt30, align 8
  %tmp1432 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %2, i64 %tmp14.unpack31, 2
  ret %"[c-flat]:Vec<T = Vec<T = char>>" %tmp1432
}

define %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:new"() {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:with_capacity"(i64 4)
  ret %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp
}

define %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:alloc<T = Vec<T = char>>"(i64 %0)
  %tmp_agg = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"[c-flat]:u64:drop"(i64 %0)
  ret %"[c-flat]:Vec<T = Vec<T = char>>" %tmp_constructor
}

define ptr @"[c-flat]:mem:alloc<T = Vec<T = char>>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i11 = mul i64 %0, 24
  %fun_call_tmp6 = call ptr @c_malloc(i64 %tmp.i11)
  %tmp7 = load i64, ptr %len, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:u64:drop"(i64 %tmp7)
  ret ptr %fun_call_tmp6
}

define {} @"[c-flat]:Vec<T = Vec<T = char>>:reserve"(ptr %0, i64 %1) {
entry:
  %new_ptr = alloca ptr, align 8
  %new_len = alloca i64, align 8
  %additional = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i64 %1, ptr %additional, align 4
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i = add i64 %tmp1, %1
  store i64 %tmp.i, ptr %new_len, align 4
  %tmp3 = load i64, ptr %new_len, align 4
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.capacity = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp5 = load i64, ptr %tmp4.capacity, align 4
  %tmp.i51 = icmp ugt i64 %tmp3, %tmp5
  br i1 %tmp.i51, label %cond9, label %merge

merge:                                            ; preds = %entry, %else10
  ret {} zeroinitializer

then8:                                            ; preds = %cond9
  %tmp15 = load ptr, ptr %self, align 8
  %tmp15.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  %tmp16 = load i64, ptr %tmp15.capacity, align 4
  %tmp.i45 = shl i64 %tmp16, 1
  %tmp18.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  store i64 %tmp.i45, ptr %tmp18.capacity, align 4
  br label %cond9

cond9:                                            ; preds = %entry, %then8
  %tmp11 = load i64, ptr %new_len, align 4
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.capacity = getelementptr inbounds nuw i8, ptr %tmp12, i64 16
  %tmp13 = load i64, ptr %tmp12.capacity, align 4
  %tmp.i48 = icmp ugt i64 %tmp11, %tmp13
  br i1 %tmp.i48, label %then8, label %else10

else10:                                           ; preds = %cond9
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.capacity = getelementptr inbounds nuw i8, ptr %tmp19, i64 16
  %tmp20 = load i64, ptr %tmp19.capacity, align 4
  %fun_call_tmp21 = call ptr @"[c-flat]:mem:alloc<T = Vec<T = char>>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:mem:copy_range<T = Vec<T = char>>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"[c-flat]:mem:free<T = Vec<T = char>>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  %tmp35 = load i64, ptr %new_len, align 4
  %fun_call_tmp36 = call {} @"[c-flat]:u64:drop"(i64 %tmp35)
  %tmp37 = load i64, ptr %additional, align 4
  %fun_call_tmp38 = call {} @"[c-flat]:u64:drop"(i64 %tmp37)
  br label %merge
}

define {} @"[c-flat]:mem:copy_range<T = Vec<T = char>>"(ptr %0, ptr %1, i64 %2) {
entry:
  %tmp.i = mul i64 %2, 24
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %tmp.i)
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %2)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:free<T = Vec<T = char>>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define %"[c-flat]:SliceIter<T = Vec<T = char>>" @"[c-flat]:[Vec<T = char>]:iter"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i16 = mul i64 %.elt21, 24
  %tmp.i = add i64 %tmp.i16, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"[c-flat]:SliceIter<T = Vec<T = char>>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"[c-flat]:SliceIter<T = Vec<T = char>>" %tmp_agg, ptr %cast7, 1
  ret %"[c-flat]:SliceIter<T = Vec<T = char>>" %tmp_constructor
}

define i1 @"[c-flat]:Vec<T = Vec<T = char>>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"[c-flat]:Vec<T = Vec<T = char>>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 24
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"[c-flat]:Vec<T = Vec<T = char>>:push"(ptr %0, %"[c-flat]:Vec<T = char>" %1) {
entry:
  %value = alloca %"[c-flat]:Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %1, 0
  store ptr %.elt, ptr %value, align 8
  %value.repack26 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt27 = extractvalue %"[c-flat]:Vec<T = char>" %1, 1
  store i64 %.elt27, ptr %value.repack26, align 8
  %value.repack28 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt29 = extractvalue %"[c-flat]:Vec<T = char>" %1, 2
  store i64 %.elt29, ptr %value.repack28, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = Vec<T = char>>:reserve"(ptr %tmp, i64 1)
  %tmp2 = load ptr, ptr %tmp, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %tmp, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i24 = mul i64 %tmp4, 24
  %tmp.i21 = add i64 %tmp.i24, %cast
  %cast8 = inttoptr i64 %tmp.i21 to ptr
  %fun_call_tmp9 = call {} @"[c-flat]:mem:copy<T = Vec<T = char>>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14.unpack = load ptr, ptr %value, align 8
  %tmp14.elt30 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp14.unpack31 = load i64, ptr %tmp14.elt30, align 8
  %tmp14.elt32 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %tmp14.unpack33 = load i64, ptr %tmp14.elt32, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp14.unpack, 0
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp14.unpack31, 1
  %tmp15.unpack44 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp14.unpack33, 2
  %tmp1539 = insertvalue %"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>" poison, %"[c-flat]:Vec<T = char>" %tmp15.unpack44, 0
  %fun_call_tmp16 = call {} @"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>:drop"(%"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>" %tmp1539)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:copy<T = Vec<T = char>>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 24)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>:drop"(%"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"[c-flat]:Vec<T = Vec<T = char>>:drop.25"(%"[c-flat]:Vec<T = Vec<T = char>>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"[c-flat]:Vec<T = Vec<T = char>>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %0, 1
  store i64 %.elt20, ptr %self.repack19, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt22 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %0, 2
  store i64 %.elt22, ptr %self.repack21, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr %"[c-flat]:Vec<T = char>", ptr %ref.i, i64 %tmp4
  %tmp6.unpack = load ptr, ptr %tmp_index.i, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp6.unpack, 0
  %tmp6.elt23 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 8
  %tmp6.unpack24 = load i64, ptr %tmp6.elt23, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp6.unpack24, 1
  %tmp6.elt25 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 16
  %tmp6.unpack26 = load i64, ptr %tmp6.elt25, align 8
  %tmp627 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp6.unpack26, 2
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %tmp627)
  %tmp8 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp8, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"[c-flat]:mem:free<T = Vec<T = char>>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"[c-flat]:Vec<T = char>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"[c-flat]:Vec<T = char>" %0, 1
  store i64 %.elt20, ptr %self.repack19, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt22 = extractvalue %"[c-flat]:Vec<T = char>" %0, 2
  store i64 %.elt22, ptr %self.repack21, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp4
  %tmp6 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp7 = call {} @"[c-flat]:char:drop"(i8 %tmp6)
  %tmp.i = add i64 %tmp4, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"[c-flat]:mem:free<T = char>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define %"[c-flat]:pixel:Game" @"[c-flat]:pixel:Game:new"({ ptr, i64 } %0, %"[c-flat]:Vec_2<T = u64>" %1, i64 %2) {
entry:
  %tmp4 = alloca %"[c-flat]:Vec<T = char>", align 8
  %result = alloca %"[c-flat]:pixel:Game", align 8
  %pixel_size_factor = alloca i64, align 8
  %pixel_frame_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %title = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %title, align 8
  %title.repack36 = getelementptr inbounds nuw i8, ptr %title, i64 8
  %.elt37 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt37, ptr %title.repack36, align 8
  %.elt38 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 0
  store i64 %.elt38, ptr %pixel_frame_size, align 4
  %pixel_frame_size.repack39 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %.elt40 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 1
  store i64 %.elt40, ptr %pixel_frame_size.repack39, align 4
  store i64 %2, ptr %pixel_size_factor, align 4
  %tmp.unpack = load ptr, ptr %title, align 8
  %3 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt41 = getelementptr inbounds nuw i8, ptr %title, i64 8
  %tmp.unpack42 = load i64, ptr %tmp.elt41, align 8
  %tmp43 = insertvalue { ptr, i64 } %3, i64 %tmp.unpack42, 1
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %tmp43)
  %tmp1.unpack = load i64, ptr %pixel_frame_size, align 4
  %4 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp1.unpack, 0
  %tmp1.elt44 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp1.unpack45 = load i64, ptr %tmp1.elt44, align 4
  %tmp146 = insertvalue %"[c-flat]:Vec_2<T = u64>" %4, i64 %tmp1.unpack45, 1
  %tmp2 = load i64, ptr %pixel_size_factor, align 4
  %fun_call_tmp3 = call %"[c-flat]:Vec_2<T = u64>" @"[c-flat]:Vec_2<T = u64>:mul"(%"[c-flat]:Vec_2<T = u64>" %tmp146, i64 %tmp2)
  %fun_call_tmp4 = call %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:new"()
  %tmp5.unpack = load i64, ptr %pixel_frame_size, align 4
  %tmp5.elt47 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp5.unpack48 = load i64, ptr %tmp5.elt47, align 4
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack58 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt59 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt59, ptr %result.repack58, align 8
  %result.repack60 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt61 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt61, ptr %result.repack60, align 8
  %result.repack50 = getelementptr inbounds nuw i8, ptr %result, i64 24
  store i64 %tmp5.unpack, ptr %result.repack50, align 8
  %result.repack50.repack62 = getelementptr inbounds nuw i8, ptr %result, i64 32
  store i64 %tmp5.unpack48, ptr %result.repack50.repack62, align 8
  %result.repack52 = getelementptr inbounds nuw i8, ptr %result, i64 40
  store i64 %tmp2, ptr %result.repack52, align 8
  %result.repack54 = getelementptr inbounds nuw i8, ptr %result, i64 48
  %fun_call_tmp3.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp3, 0
  store i64 %fun_call_tmp3.elt, ptr %result.repack54, align 8
  %result.repack54.repack64 = getelementptr inbounds nuw i8, ptr %result, i64 56
  %fun_call_tmp3.elt65 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp3, 1
  store i64 %fun_call_tmp3.elt65, ptr %result.repack54.repack64, align 8
  %result.repack56 = getelementptr inbounds nuw i8, ptr %result, i64 64
  %fun_call_tmp4.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp4, 0
  store ptr %fun_call_tmp4.elt, ptr %result.repack56, align 8
  %result.repack56.repack66 = getelementptr inbounds nuw i8, ptr %result, i64 72
  %fun_call_tmp4.elt67 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp4, 1
  store i64 %fun_call_tmp4.elt67, ptr %result.repack56.repack66, align 8
  %result.repack56.repack68 = getelementptr inbounds nuw i8, ptr %result, i64 80
  %fun_call_tmp4.elt69 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp4, 2
  store i64 %fun_call_tmp4.elt69, ptr %result.repack56.repack68, align 8
  %fun_call_tmp10 = call {} @crl_set_config_flags(i32 64)
  %result.frame_size = getelementptr inbounds nuw i8, ptr %result, i64 48
  %tmp11 = load i64, ptr %result.frame_size, align 4
  %cast = trunc i64 %tmp11 to i32
  %result.frame_size12.y = getelementptr inbounds nuw i8, ptr %result, i64 56
  %tmp13 = load i64, ptr %result.frame_size12.y, align 4
  %cast14 = trunc i64 %tmp13 to i32
  %fun_call_tmp15 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:clone"(ptr nonnull %result)
  %fun_call_tmp16 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp15)
  %fun_call_tmp16.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp16, 0
  store ptr %fun_call_tmp16.elt, ptr %tmp4, align 8
  %tmp4.repack70 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %fun_call_tmp16.elt71 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp16, 1
  store i64 %fun_call_tmp16.elt71, ptr %tmp4.repack70, align 8
  %tmp4.repack72 = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %fun_call_tmp16.elt73 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp16, 2
  store i64 %fun_call_tmp16.elt73, ptr %tmp4.repack72, align 8
  %fun_call_tmp17 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp4)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp17, 0
  %fun_call_tmp19 = call {} @crl_init_window(i32 %cast, i32 %cast14, ptr %ref.i)
  %tmp20 = load i64, ptr %pixel_frame_size, align 4
  %cast21 = trunc i64 %tmp20 to i32
  %pixel_frame_size.y = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp22 = load i64, ptr %pixel_frame_size.y, align 4
  %cast23 = trunc i64 %tmp22 to i32
  %fun_call_tmp24 = call {} @crl_init_canvas(i32 %cast21, i32 %cast23)
  %tmp25.unpack.unpack = load ptr, ptr %result, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp25.unpack.unpack, 0
  %tmp25.unpack.elt83 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp25.unpack.unpack84 = load i64, ptr %tmp25.unpack.elt83, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" %5, i64 %tmp25.unpack.unpack84, 1
  %tmp25.unpack.elt85 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp25.unpack.unpack86 = load i64, ptr %tmp25.unpack.elt85, align 8
  %tmp25.unpack87 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp25.unpack.unpack86, 2
  %7 = insertvalue %"[c-flat]:pixel:Game" poison, %"[c-flat]:Vec<T = char>" %tmp25.unpack87, 0
  %tmp25.elt74 = getelementptr inbounds nuw i8, ptr %result, i64 24
  %tmp25.unpack75.unpack = load i64, ptr %tmp25.elt74, align 8
  %8 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp25.unpack75.unpack, 0
  %tmp25.unpack75.elt88 = getelementptr inbounds nuw i8, ptr %result, i64 32
  %tmp25.unpack75.unpack89 = load i64, ptr %tmp25.unpack75.elt88, align 8
  %tmp25.unpack7590 = insertvalue %"[c-flat]:Vec_2<T = u64>" %8, i64 %tmp25.unpack75.unpack89, 1
  %9 = insertvalue %"[c-flat]:pixel:Game" %7, %"[c-flat]:Vec_2<T = u64>" %tmp25.unpack7590, 1
  %tmp25.elt76 = getelementptr inbounds nuw i8, ptr %result, i64 40
  %tmp25.unpack77 = load i64, ptr %tmp25.elt76, align 8
  %10 = insertvalue %"[c-flat]:pixel:Game" %9, i64 %tmp25.unpack77, 2
  %tmp25.elt78 = getelementptr inbounds nuw i8, ptr %result, i64 48
  %tmp25.unpack79.unpack = load i64, ptr %tmp25.elt78, align 8
  %11 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp25.unpack79.unpack, 0
  %tmp25.unpack79.elt91 = getelementptr inbounds nuw i8, ptr %result, i64 56
  %tmp25.unpack79.unpack92 = load i64, ptr %tmp25.unpack79.elt91, align 8
  %tmp25.unpack7993 = insertvalue %"[c-flat]:Vec_2<T = u64>" %11, i64 %tmp25.unpack79.unpack92, 1
  %12 = insertvalue %"[c-flat]:pixel:Game" %10, %"[c-flat]:Vec_2<T = u64>" %tmp25.unpack7993, 3
  %tmp25.elt80 = getelementptr inbounds nuw i8, ptr %result, i64 64
  %tmp25.unpack81.unpack = load ptr, ptr %tmp25.elt80, align 8
  %13 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" poison, ptr %tmp25.unpack81.unpack, 0
  %tmp25.unpack81.elt94 = getelementptr inbounds nuw i8, ptr %result, i64 72
  %tmp25.unpack81.unpack95 = load i64, ptr %tmp25.unpack81.elt94, align 8
  %14 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %13, i64 %tmp25.unpack81.unpack95, 1
  %tmp25.unpack81.elt96 = getelementptr inbounds nuw i8, ptr %result, i64 80
  %tmp25.unpack81.unpack97 = load i64, ptr %tmp25.unpack81.elt96, align 8
  %tmp25.unpack8198 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %14, i64 %tmp25.unpack81.unpack97, 2
  %tmp2582 = insertvalue %"[c-flat]:pixel:Game" %12, %"[c-flat]:Vec<T = Vec<T = char>>" %tmp25.unpack8198, 4
  %tmp26.unpack = load ptr, ptr %tmp4, align 8
  %15 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp26.unpack, 0
  %tmp26.elt99 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %tmp26.unpack100 = load i64, ptr %tmp26.elt99, align 8
  %16 = insertvalue %"[c-flat]:Vec<T = char>" %15, i64 %tmp26.unpack100, 1
  %tmp26.elt101 = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp26.unpack102 = load i64, ptr %tmp26.elt101, align 8
  %tmp26103 = insertvalue %"[c-flat]:Vec<T = char>" %16, i64 %tmp26.unpack102, 2
  %fun_call_tmp27 = call {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %tmp26103)
  %tmp28 = load i64, ptr %pixel_size_factor, align 4
  %fun_call_tmp29 = call {} @"[c-flat]:u64:drop"(i64 %tmp28)
  %tmp30.unpack = load i64, ptr %pixel_frame_size, align 4
  %tmp30.elt104 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp30.unpack105 = load i64, ptr %tmp30.elt104, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp30.unpack105)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp30.unpack)
  ret %"[c-flat]:pixel:Game" %tmp2582
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0)
  ret %"[c-flat]:Vec<T = char>" %fun_call_tmp
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0) {
entry:
  %result = alloca %"[c-flat]:Vec<T = char>", align 8
  %slice = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack5 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt6 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt6, ptr %slice.repack5, align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack7 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt8 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt8, ptr %result.repack7, align 8
  %result.repack9 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt10 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt10, ptr %result.repack9, align 8
  %tmp.unpack = load ptr, ptr %slice, align 8
  %1 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt11 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp.unpack12 = load i64, ptr %tmp.elt11, align 8
  %tmp13 = insertvalue { ptr, i64 } %1, i64 %tmp.unpack12, 1
  %fun_call_tmp1 = call {} @"[c-flat]:Vec<T = char>:extend_from_slice"(ptr nonnull %result, { ptr, i64 } %tmp13)
  %tmp2.unpack = load ptr, ptr %result, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp2.unpack, 0
  %tmp2.elt14 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp2.unpack15 = load i64, ptr %tmp2.elt14, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp2.unpack15, 1
  %tmp2.elt16 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp2.unpack17 = load i64, ptr %tmp2.elt16, align 8
  %tmp218 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp2.unpack17, 2
  ret %"[c-flat]:Vec<T = char>" %tmp218
}

define {} @"[c-flat]:Vec<T = char>:extend_from_slice"(ptr %0, { ptr, i64 } %1) {
entry:
  %idx = alloca i64, align 8
  %slice = alloca { ptr, i64 }, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack24 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt25 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt25, ptr %slice.repack24, align 8
  store i64 0, ptr %idx, align 4
  br label %cond

then:                                             ; preds = %cond
  %tmp3 = load ptr, ptr %self, align 8
  %tmp4.unpack = load ptr, ptr %slice, align 8
  %tmp5 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp4.unpack, i64 %tmp5
  %tmp7 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp8 = call {} @"[c-flat]:Vec<T = char>:push"(ptr %tmp3, i8 %tmp7)
  %tmp.i = add i64 %tmp5, 1
  store i64 %tmp.i, ptr %idx, align 4
  %fun_call_tmp12 = call {} @"[c-flat]:u64:drop"(i64 %tmp.i)
  br label %cond

cond:                                             ; preds = %then, %entry
  %tmp = load i64, ptr %idx, align 4
  %tmp1.elt26 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp1.unpack27 = load i64, ptr %tmp1.elt26, align 8
  %tmp.i19 = icmp ult i64 %tmp, %tmp1.unpack27
  br i1 %tmp.i19, label %then, label %else

else:                                             ; preds = %cond
  ret {} zeroinitializer
}

define %"[c-flat]:Vec_2<T = u64>" @"[c-flat]:Vec_2<T = u64>:mul"(%"[c-flat]:Vec_2<T = u64>" %0, i64 %1) {
entry:
  %self = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %self, align 4
  %self.repack14 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt15 = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 1
  store i64 %.elt15, ptr %self.repack14, align 4
  %tmp.i11 = mul i64 %.elt, %1
  %tmp.i = mul i64 %.elt15, %1
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = u64>" undef, i64 %tmp.i11, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = u64>" %tmp_agg, i64 %tmp.i, 1
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %1)
  %tmp7.unpack = load i64, ptr %self, align 4
  %tmp7.elt16 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp7.unpack17 = load i64, ptr %tmp7.elt16, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp7.unpack17)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp7.unpack)
  ret %"[c-flat]:Vec_2<T = u64>" %tmp_constructor
}

declare {} @crl_set_config_flags(i32)

declare {} @crl_init_window(i32, i32, ptr)

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %0) {
entry:
  %self = alloca %"[c-flat]:Vec<T = char>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack1 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt2 = extractvalue %"[c-flat]:Vec<T = char>" %0, 1
  store i64 %.elt2, ptr %self.repack1, align 8
  %self.repack3 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt4 = extractvalue %"[c-flat]:Vec<T = char>" %0, 2
  store i64 %.elt4, ptr %self.repack3, align 8
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %self, i8 0)
  %tmp.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack, 0
  %tmp.elt5 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack6 = load i64, ptr %tmp.elt5, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack6, 1
  %tmp.elt7 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack8 = load i64, ptr %tmp.elt7, align 8
  %tmp9 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack8, 2
  ret %"[c-flat]:Vec<T = char>" %tmp9
}

declare {} @crl_init_canvas(i32, i32)

define double @"[c-flat]:f64:clone"(ptr %0) {
entry:
  %tmp1 = load double, ptr %0, align 8
  ret double %tmp1
}

define {} @"[c-flat]:f64:drop"(double %0) {
entry:
  ret {} zeroinitializer
}

define i32 @"[c-flat]:i32:clone"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  ret i32 %tmp1
}

define {} @"[c-flat]:i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}

define %"[c-flat]:Player" @"[c-flat]:File:new"(ptr %0) {
entry:
  %fun_call_tmp = call i32 @"[c-flat]:pixel:Game:load_texture"(ptr %0, { ptr, i64 } { ptr @global_string.29, i64 13 })
  %tmp_agg = insertvalue %"[c-flat]:Player" undef, i32 %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"[c-flat]:Player" %tmp_agg, %"[c-flat]:Vec_2<T = f64>" zeroinitializer, 1
  %tmp_agg3 = insertvalue %"[c-flat]:Player" %tmp_agg2, %"[c-flat]:Vec_2<T = f64>" zeroinitializer, 2
  %tmp_constructor = insertvalue %"[c-flat]:Player" %tmp_agg3, %"[c-flat]:Vec_2<T = u64>" zeroinitializer, 3
  %fun_call_tmp5 = call {} @"[c-flat]:i32:drop"(i32 %fun_call_tmp)
  ret %"[c-flat]:Player" %tmp_constructor
}

define i32 @"[c-flat]:pixel:Game:load_texture"(ptr %0, { ptr, i64 } %1) {
entry:
  %idx = alloca i64, align 8
  %path = alloca { ptr, i64 }, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %path, align 8
  %path.repack25 = getelementptr inbounds nuw i8, ptr %path, i64 8
  %.elt26 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt26, ptr %path.repack25, align 8
  %tmp.loaded_textures = getelementptr inbounds nuw i8, ptr %0, i64 64
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref_mut"(ptr nonnull %tmp.loaded_textures)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  store i64 %len.i, ptr %idx, align 4
  %tmp2 = load ptr, ptr %self, align 8
  %tmp2.loaded_textures = getelementptr inbounds nuw i8, ptr %tmp2, i64 64
  %tmp3.unpack = load ptr, ptr %path, align 8
  %2 = insertvalue { ptr, i64 } poison, ptr %tmp3.unpack, 0
  %tmp3.elt27 = getelementptr inbounds nuw i8, ptr %path, i64 8
  %tmp3.unpack28 = load i64, ptr %tmp3.elt27, align 8
  %tmp329 = insertvalue { ptr, i64 } %2, i64 %tmp3.unpack28, 1
  %fun_call_tmp4 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %tmp329)
  %fun_call_tmp5 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp4)
  %fun_call_tmp6 = call {} @"[c-flat]:Vec<T = Vec<T = char>>:push"(ptr nonnull %tmp2.loaded_textures, %"[c-flat]:Vec<T = char>" %fun_call_tmp5)
  %tmp7 = load ptr, ptr %self, align 8
  %tmp7.loaded_textures = getelementptr inbounds nuw i8, ptr %tmp7, i64 64
  %fun_call_tmp8 = call { ptr, i64 } @"[c-flat]:Vec<T = Vec<T = char>>:deref_mut"(ptr nonnull %tmp7.loaded_textures)
  %tmp9 = load i64, ptr %idx, align 4
  %ref.i23 = extractvalue { ptr, i64 } %fun_call_tmp8, 0
  %tmp_index.i = getelementptr %"[c-flat]:Vec<T = char>", ptr %ref.i23, i64 %tmp9
  %fun_call_tmp11 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr %tmp_index.i)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp11, 0
  %fun_call_tmp13 = call i32 @crl_load_texture(ptr %ref.i)
  %fun_call_tmp15 = call {} @"[c-flat]:u64:drop"(i64 %tmp9)
  ret i32 %fun_call_tmp13
}

declare i32 @crl_load_texture(ptr)

define i32 @"[c-flat]:u32:clone"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  ret i32 %tmp1
}

define {} @"[c-flat]:u32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}

define i1 @"[c-flat]:&File:eq"(ptr %0, ptr %1) {
entry:
  %other.i = alloca ptr, align 8
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %other.i)
  store ptr %tmp3, ptr %other.i, align 8
  %tmp.i9.i.i = load double, ptr %tmp1, align 8
  %tmp1.i10.i.i = load double, ptr %tmp3, align 8
  %tmp2.i11.i.i = fcmp oeq double %tmp.i9.i.i, %tmp1.i10.i.i
  %tmp3.i.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4.y.i.i = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp.i.i.i = load double, ptr %tmp3.i.i, align 8
  %tmp1.i.i.i = load double, ptr %tmp4.y.i.i, align 8
  %tmp2.i.i.i = fcmp oeq double %tmp.i.i.i, %tmp1.i.i.i
  %tmp6.i.i = and i1 %tmp2.i11.i.i, %tmp2.i.i.i
  %tmp3.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp4.i = load ptr, ptr %other.i, align 8
  %tmp4.size.i = getelementptr inbounds nuw i8, ptr %tmp4.i, i64 16
  %tmp.i35.i = load double, ptr %tmp3.i, align 8
  %tmp1.i36.i = load double, ptr %tmp4.size.i, align 8
  %tmp2.i37.i = fcmp oeq double %tmp.i35.i, %tmp1.i36.i
  %tmp6.i = and i1 %tmp6.i.i, %tmp2.i37.i
  %tmp7.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp8.i = load ptr, ptr %other.i, align 8
  %tmp8.speed.i = getelementptr inbounds nuw i8, ptr %tmp8.i, i64 24
  %tmp.i30.i = load double, ptr %tmp7.i, align 8
  %tmp1.i31.i = load double, ptr %tmp8.speed.i, align 8
  %tmp2.i32.i = fcmp oeq double %tmp.i30.i, %tmp1.i31.i
  %tmp10.i = and i1 %tmp6.i, %tmp2.i32.i
  %tmp11.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp12.i = load ptr, ptr %other.i, align 8
  %tmp12.new_speed.i = getelementptr inbounds nuw i8, ptr %tmp12.i, i64 32
  %tmp.i25.i = load double, ptr %tmp11.i, align 8
  %tmp1.i26.i = load double, ptr %tmp12.new_speed.i, align 8
  %tmp2.i27.i = fcmp oeq double %tmp.i25.i, %tmp1.i26.i
  %tmp14.i = and i1 %tmp10.i, %tmp2.i27.i
  %tmp15.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 40
  %tmp16.i = load ptr, ptr %other.i, align 8
  %tmp16.offset.i = getelementptr inbounds nuw i8, ptr %tmp16.i, i64 40
  %tmp.i.i = load double, ptr %tmp15.i, align 8
  %tmp1.i.i = load double, ptr %tmp16.offset.i, align 8
  %tmp2.i.i = fcmp oeq double %tmp.i.i, %tmp1.i.i
  %tmp18.i = and i1 %tmp14.i, %tmp2.i.i
  %tmp19.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 48
  %tmp20.i = load ptr, ptr %other.i, align 8
  %tmp20.color.i = getelementptr inbounds nuw i8, ptr %tmp20.i, i64 48
  %tmp.i45.i = load i32, ptr %tmp19.i, align 4
  %tmp1.i46.i = load i32, ptr %tmp20.color.i, align 4
  %tmp2.i47.i = icmp eq i32 %tmp.i45.i, %tmp1.i46.i
  %tmp22.i = and i1 %tmp18.i, %tmp2.i47.i
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %other.i)
  ret i1 %tmp22.i
}

define %"[c-flat]:Vec<T = File>" @"[c-flat]:Vec<T = File>:new"() {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = File>" @"[c-flat]:Vec<T = File>:with_capacity"(i64 4)
  ret %"[c-flat]:Vec<T = File>" %fun_call_tmp
}

define %"[c-flat]:Vec<T = File>" @"[c-flat]:Vec<T = File>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:alloc<T = File>"(i64 %0)
  %tmp_agg = insertvalue %"[c-flat]:Vec<T = File>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"[c-flat]:Vec<T = File>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"[c-flat]:Vec<T = File>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"[c-flat]:u64:drop"(i64 %0)
  ret %"[c-flat]:Vec<T = File>" %tmp_constructor
}

define ptr @"[c-flat]:mem:alloc<T = File>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i11 = mul i64 %0, 52
  %fun_call_tmp6 = call ptr @c_malloc(i64 %tmp.i11)
  %tmp7 = load i64, ptr %len, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:u64:drop"(i64 %tmp7)
  ret ptr %fun_call_tmp6
}

define i1 @"[c-flat]:Range<T = i32>:has_next"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 4
  %tmp3 = load i32, ptr %tmp2.end, align 4
  %tmp.i = icmp slt i32 %tmp1, %tmp3
  ret i1 %tmp.i
}

define i32 @"[c-flat]:Range<T = i32>:next"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  %tmp.i = add i32 %tmp1, 1
  store i32 %tmp.i, ptr %0, align 4
  %fun_call_tmp7 = call {} @"[c-flat]:i32:drop"(i32 %tmp1)
  ret i32 %tmp1
}

define {} @"[c-flat]:Vec<T = File>:push"(ptr %0, %"[c-flat]:Particle" %1) {
entry:
  %value = alloca %"[c-flat]:Particle", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store %"[c-flat]:Particle" %1, ptr %value, align 8
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = File>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i24 = mul i64 %tmp4, 52
  %tmp.i21 = add i64 %tmp.i24, %cast
  %cast8 = inttoptr i64 %tmp.i21 to ptr
  %fun_call_tmp9 = call {} @"[c-flat]:mem:copy<T = File>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load %"[c-flat]:Particle", ptr %value, align 8
  %tmp1526 = insertvalue %"[c-flat]:mem:Manually_Drop<T = File>" poison, %"[c-flat]:Particle" %tmp14, 0
  %fun_call_tmp16 = call {} @"[c-flat]:mem:Manually_Drop<T = File>:drop"(%"[c-flat]:mem:Manually_Drop<T = File>" %tmp1526)
  ret {} zeroinitializer
}

define {} @"[c-flat]:Vec<T = File>:reserve"(ptr %0, i64 %1) {
entry:
  %new_ptr = alloca ptr, align 8
  %new_len = alloca i64, align 8
  %additional = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i64 %1, ptr %additional, align 4
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i = add i64 %tmp1, %1
  store i64 %tmp.i, ptr %new_len, align 4
  %tmp3 = load i64, ptr %new_len, align 4
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.capacity = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp5 = load i64, ptr %tmp4.capacity, align 4
  %tmp.i50 = icmp ugt i64 %tmp3, %tmp5
  br i1 %tmp.i50, label %cond9, label %merge

merge:                                            ; preds = %entry, %else10
  ret {} zeroinitializer

then8:                                            ; preds = %cond9
  %tmp15 = load ptr, ptr %self, align 8
  %tmp15.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  %tmp16 = load i64, ptr %tmp15.capacity, align 4
  %tmp.i44 = shl i64 %tmp16, 1
  %tmp18.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  store i64 %tmp.i44, ptr %tmp18.capacity, align 4
  br label %cond9

cond9:                                            ; preds = %entry, %then8
  %tmp11 = load i64, ptr %new_len, align 4
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.capacity = getelementptr inbounds nuw i8, ptr %tmp12, i64 16
  %tmp13 = load i64, ptr %tmp12.capacity, align 4
  %tmp.i47 = icmp ugt i64 %tmp11, %tmp13
  br i1 %tmp.i47, label %then8, label %else10

else10:                                           ; preds = %cond9
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.capacity = getelementptr inbounds nuw i8, ptr %tmp19, i64 16
  %tmp20 = load i64, ptr %tmp19.capacity, align 4
  %fun_call_tmp21 = call ptr @"[c-flat]:mem:alloc<T = File>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:mem:copy_range<T = File>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"[c-flat]:mem:free<T = File>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  %tmp35 = load i64, ptr %new_len, align 4
  %fun_call_tmp36 = call {} @"[c-flat]:u64:drop"(i64 %tmp35)
  %tmp37 = load i64, ptr %additional, align 4
  %fun_call_tmp38 = call {} @"[c-flat]:u64:drop"(i64 %tmp37)
  br label %merge
}

define {} @"[c-flat]:mem:copy_range<T = File>"(ptr %0, ptr %1, i64 %2) {
entry:
  %tmp.i = mul i64 %2, 52
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %tmp.i)
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %2)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:free<T = File>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define {} @"[c-flat]:mem:copy<T = File>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 52)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:Manually_Drop<T = File>:drop"(%"[c-flat]:mem:Manually_Drop<T = File>" %0) {
entry:
  ret {} zeroinitializer
}

define %"[c-flat]:Particle" @"[c-flat]:File:spawn"(%"[c-flat]:Vec_2<T = u64>" %0) {
entry:
  %tmp6 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp510 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp4 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp3 = alloca %"[c-flat]:Range<T = f64>", align 8
  %speed = alloca double, align 8
  %tmp1 = alloca %"[c-flat]:Range<T = f64>", align 8
  %screen_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack75 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt76 = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 1
  store i64 %.elt76, ptr %screen_size.repack75, align 4
  store double 0.000000e+00, ptr %tmp1, align 8
  %tmp1.repack77 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  store double 5.000000e+00, ptr %tmp1.repack77, align 8
  %fun_call_tmp1 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp1)
  %tmp.i38 = fadd double %fun_call_tmp1, 2.500000e-01
  store double %tmp.i38, ptr %speed, align 8
  %tmp = load i64, ptr %screen_size, align 4
  %cast = uitofp i64 %tmp to double
  store double 0.000000e+00, ptr %tmp3, align 8
  %tmp3.repack78 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  store double %cast, ptr %tmp3.repack78, align 8
  %fun_call_tmp4 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp3)
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp5 = load i64, ptr %screen_size.y, align 4
  %cast6 = uitofp i64 %tmp5 to double
  store double 0.000000e+00, ptr %tmp4, align 8
  %tmp4.repack80 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  store double %cast6, ptr %tmp4.repack80, align 8
  %fun_call_tmp8 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp4)
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = f64>" undef, double %fun_call_tmp4, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = f64>" %tmp_agg, double %fun_call_tmp8, 1
  store double 1.000000e+00, ptr %tmp510, align 8
  %tmp510.repack82 = getelementptr inbounds nuw i8, ptr %tmp510, i64 8
  store double 2.400000e+00, ptr %tmp510.repack82, align 8
  %fun_call_tmp11 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp510)
  store double 0.000000e+00, ptr %tmp6, align 8
  %tmp6.repack83 = getelementptr inbounds nuw i8, ptr %tmp6, i64 8
  store double 1.000000e+00, ptr %tmp6.repack83, align 8
  %fun_call_tmp13 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp6)
  %fun_call_tmp14 = call i32 @"[c-flat]:rand_color"()
  %tmp15 = load double, ptr %speed, align 8
  %tmp_agg17 = insertvalue %"[c-flat]:Particle" undef, %"[c-flat]:Vec_2<T = f64>" %tmp_constructor, 0
  %tmp_agg18 = insertvalue %"[c-flat]:Particle" %tmp_agg17, double %fun_call_tmp11, 1
  %tmp_agg19 = insertvalue %"[c-flat]:Particle" %tmp_agg18, double %tmp15, 2
  %tmp_agg20 = insertvalue %"[c-flat]:Particle" %tmp_agg19, double %tmp15, 3
  %tmp_agg21 = insertvalue %"[c-flat]:Particle" %tmp_agg20, double %fun_call_tmp13, 4
  %tmp_constructor22 = insertvalue %"[c-flat]:Particle" %tmp_agg21, i32 %fun_call_tmp14, 5
  %tmp23.unpack = load double, ptr %tmp6, align 8
  %tmp23.elt84 = getelementptr inbounds nuw i8, ptr %tmp6, i64 8
  %tmp23.unpack85 = load double, ptr %tmp23.elt84, align 8
  %fun_call_tmp.i72 = call {} @"[c-flat]:f64:drop"(double %tmp23.unpack85)
  %fun_call_tmp2.i74 = call {} @"[c-flat]:f64:drop"(double %tmp23.unpack)
  %tmp25.unpack = load double, ptr %tmp510, align 8
  %tmp25.elt87 = getelementptr inbounds nuw i8, ptr %tmp510, i64 8
  %tmp25.unpack88 = load double, ptr %tmp25.elt87, align 8
  %fun_call_tmp.i67 = call {} @"[c-flat]:f64:drop"(double %tmp25.unpack88)
  %fun_call_tmp2.i69 = call {} @"[c-flat]:f64:drop"(double %tmp25.unpack)
  %tmp27.unpack = load double, ptr %tmp4, align 8
  %tmp27.elt90 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %tmp27.unpack91 = load double, ptr %tmp27.elt90, align 8
  %fun_call_tmp.i62 = call {} @"[c-flat]:f64:drop"(double %tmp27.unpack91)
  %fun_call_tmp2.i64 = call {} @"[c-flat]:f64:drop"(double %tmp27.unpack)
  %tmp29.unpack = load double, ptr %tmp3, align 8
  %tmp29.elt93 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp29.unpack94 = load double, ptr %tmp29.elt93, align 8
  %fun_call_tmp.i57 = call {} @"[c-flat]:f64:drop"(double %tmp29.unpack94)
  %fun_call_tmp2.i59 = call {} @"[c-flat]:f64:drop"(double %tmp29.unpack)
  %tmp31 = load double, ptr %speed, align 8
  %fun_call_tmp32 = call {} @"[c-flat]:f64:drop"(double %tmp31)
  %tmp33.unpack = load double, ptr %tmp1, align 8
  %tmp33.elt96 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp33.unpack97 = load double, ptr %tmp33.elt96, align 8
  %fun_call_tmp.i52 = call {} @"[c-flat]:f64:drop"(double %tmp33.unpack97)
  %fun_call_tmp2.i54 = call {} @"[c-flat]:f64:drop"(double %tmp33.unpack)
  %tmp35.unpack = load i64, ptr %screen_size, align 4
  %tmp35.elt99 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp35.unpack100 = load i64, ptr %tmp35.elt99, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp35.unpack100)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp35.unpack)
  ret %"[c-flat]:Particle" %tmp_constructor22
}

define double @"[c-flat]:Vec_2<T = f64>:rand"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call i64 @"[c-flat]:rng:next"()
  %cast = uitofp i64 %fun_call_tmp to double
  %tmp.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load double, ptr %tmp.end, align 8
  %tmp3 = load double, ptr %0, align 8
  %tmp.i13 = fsub double %tmp1, %tmp3
  %tmp.i16 = frem double %cast, %tmp.i13
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load double, ptr %tmp6, align 8
  %tmp.i = fadd double %tmp.i16, %tmp7
  ret double %tmp.i
}

define i64 @"[c-flat]:rng:next"() {
entry:
  %tmp = load i64, ptr @"[c-flat]:seed", align 4
  %tmp.i8 = mul i64 %tmp, 6364136223846793005
  %tmp.i = add i64 %tmp.i8, 1442695040888963407
  store i64 %tmp.i, ptr @"[c-flat]:seed", align 4
  %tmp.i10 = lshr i64 %tmp.i, 33
  %fun_call_tmp5 = call {} @"[c-flat]:u64:drop"(i64 %tmp.i)
  ret i64 %tmp.i10
}

define i32 @"[c-flat]:rand_color"() {
entry:
  %tmp2 = alloca %"[c-flat]:Range<T = i32>", align 8
  %tmp1 = alloca %"[c-flat]:Range<T = i32>", align 8
  %tmp0 = alloca %"[c-flat]:Range<T = i32>", align 8
  store i32 200, ptr %tmp0, align 4
  %tmp0.repack48 = getelementptr inbounds nuw i8, ptr %tmp0, i64 4
  store i32 256, ptr %tmp0.repack48, align 4
  %fun_call_tmp1 = call i32 @"[c-flat]:Range<T = i32>:rand"(ptr nonnull %tmp0)
  store i32 200, ptr %tmp1, align 4
  %tmp1.repack49 = getelementptr inbounds nuw i8, ptr %tmp1, i64 4
  store i32 256, ptr %tmp1.repack49, align 4
  %fun_call_tmp3 = call i32 @"[c-flat]:Range<T = i32>:rand"(ptr nonnull %tmp1)
  %tmp.i35 = shl i32 %fun_call_tmp3, 8
  %tmp.i28 = or i32 %fun_call_tmp1, %tmp.i35
  store i32 200, ptr %tmp2, align 4
  %tmp2.repack50 = getelementptr inbounds nuw i8, ptr %tmp2, i64 4
  store i32 256, ptr %tmp2.repack50, align 4
  %fun_call_tmp7 = call i32 @"[c-flat]:Range<T = i32>:rand"(ptr nonnull %tmp2)
  %tmp.i32 = shl i32 %fun_call_tmp7, 16
  %tmp.i25 = or i32 %tmp.i28, %tmp.i32
  %tmp.i = or i32 %tmp.i25, -16777216
  %tmp.unpack = load i32, ptr %tmp2, align 4
  %tmp.elt51 = getelementptr inbounds nuw i8, ptr %tmp2, i64 4
  %tmp.unpack52 = load i32, ptr %tmp.elt51, align 4
  %fun_call_tmp.i45 = call {} @"[c-flat]:i32:drop"(i32 %tmp.unpack52)
  %fun_call_tmp2.i47 = call {} @"[c-flat]:i32:drop"(i32 %tmp.unpack)
  %tmp13.unpack = load i32, ptr %tmp1, align 4
  %tmp13.elt54 = getelementptr inbounds nuw i8, ptr %tmp1, i64 4
  %tmp13.unpack55 = load i32, ptr %tmp13.elt54, align 4
  %fun_call_tmp.i40 = call {} @"[c-flat]:i32:drop"(i32 %tmp13.unpack55)
  %fun_call_tmp2.i42 = call {} @"[c-flat]:i32:drop"(i32 %tmp13.unpack)
  %tmp15.unpack = load i32, ptr %tmp0, align 4
  %tmp15.elt57 = getelementptr inbounds nuw i8, ptr %tmp0, i64 4
  %tmp15.unpack58 = load i32, ptr %tmp15.elt57, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp15.unpack58)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp15.unpack)
  ret i32 %tmp.i
}

define i32 @"[c-flat]:Range<T = i32>:rand"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call i64 @"[c-flat]:rng:next"()
  %cast = trunc i64 %fun_call_tmp to i32
  %tmp.end = getelementptr inbounds nuw i8, ptr %0, i64 4
  %tmp1 = load i32, ptr %tmp.end, align 4
  %tmp3 = load i32, ptr %0, align 4
  %tmp.i13 = sub i32 %tmp1, %tmp3
  %tmp.i16 = srem i32 %cast, %tmp.i13
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load i32, ptr %tmp6, align 4
  %tmp.i = add i32 %tmp.i16, %tmp7
  ret i32 %tmp.i
}

define i1 @"[c-flat]:pixel:Game:should_close"(ptr %0) {
entry:
  %fun_call_tmp = call i1 @crl_window_should_close()
  ret i1 %fun_call_tmp
}

declare i1 @crl_window_should_close()

define i1 @"[c-flat]:pixel:Game:is_mouse_pressed"(ptr %0, i32 %1) {
entry:
  %fun_call_tmp = call i1 @crl_is_mouse_button_pressed(i32 %1)
  %fun_call_tmp2 = call {} @"[c-flat]:i32:drop"(i32 %1)
  ret i1 %fun_call_tmp
}

declare i1 @crl_is_mouse_button_pressed(i32)

define %"[c-flat]:Particle" @"[c-flat]:File:spawn_at"(%"[c-flat]:Vec_2<T = f64>" %0, i32 %1) {
entry:
  %tmp5 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp4 = alloca %"[c-flat]:Range<T = f64>", align 8
  %speed = alloca double, align 8
  %tmp2 = alloca %"[c-flat]:Range<T = f64>", align 8
  %color = alloca i32, align 4
  %pos = alloca %"[c-flat]:Vec_2<T = f64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = f64>" %0, 0
  store double %.elt, ptr %pos, align 8
  %pos.repack49 = getelementptr inbounds nuw i8, ptr %pos, i64 8
  %.elt50 = extractvalue %"[c-flat]:Vec_2<T = f64>" %0, 1
  store double %.elt50, ptr %pos.repack49, align 8
  store i32 %1, ptr %color, align 4
  store double 0.000000e+00, ptr %tmp2, align 8
  %tmp2.repack51 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  store double 5.000000e+00, ptr %tmp2.repack51, align 8
  %fun_call_tmp1 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp2)
  %tmp.i = fadd double %fun_call_tmp1, 2.500000e-01
  store double %tmp.i, ptr %speed, align 8
  store double 3.000000e+00, ptr %tmp4, align 8
  %tmp4.repack52 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  store double 5.400000e+00, ptr %tmp4.repack52, align 8
  %fun_call_tmp4 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp4)
  store double 0.000000e+00, ptr %tmp5, align 8
  %tmp5.repack53 = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  store double 1.000000e+00, ptr %tmp5.repack53, align 8
  %fun_call_tmp6 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp5)
  %tmp.unpack = load double, ptr %pos, align 8
  %2 = insertvalue %"[c-flat]:Vec_2<T = f64>" poison, double %tmp.unpack, 0
  %tmp.elt54 = getelementptr inbounds nuw i8, ptr %pos, i64 8
  %tmp.unpack55 = load double, ptr %tmp.elt54, align 8
  %tmp56 = insertvalue %"[c-flat]:Vec_2<T = f64>" %2, double %tmp.unpack55, 1
  %tmp7 = load double, ptr %speed, align 8
  %tmp9 = load i32, ptr %color, align 4
  %tmp_agg = insertvalue %"[c-flat]:Particle" undef, %"[c-flat]:Vec_2<T = f64>" %tmp56, 0
  %tmp_agg10 = insertvalue %"[c-flat]:Particle" %tmp_agg, double %fun_call_tmp4, 1
  %tmp_agg11 = insertvalue %"[c-flat]:Particle" %tmp_agg10, double %tmp7, 2
  %tmp_agg12 = insertvalue %"[c-flat]:Particle" %tmp_agg11, double %tmp7, 3
  %tmp_agg13 = insertvalue %"[c-flat]:Particle" %tmp_agg12, double %fun_call_tmp6, 4
  %tmp_constructor = insertvalue %"[c-flat]:Particle" %tmp_agg13, i32 %tmp9, 5
  %tmp14.unpack = load double, ptr %tmp5, align 8
  %tmp14.elt57 = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  %tmp14.unpack58 = load double, ptr %tmp14.elt57, align 8
  %fun_call_tmp.i46 = call {} @"[c-flat]:f64:drop"(double %tmp14.unpack58)
  %fun_call_tmp2.i48 = call {} @"[c-flat]:f64:drop"(double %tmp14.unpack)
  %tmp16.unpack = load double, ptr %tmp4, align 8
  %tmp16.elt60 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %tmp16.unpack61 = load double, ptr %tmp16.elt60, align 8
  %fun_call_tmp.i41 = call {} @"[c-flat]:f64:drop"(double %tmp16.unpack61)
  %fun_call_tmp2.i43 = call {} @"[c-flat]:f64:drop"(double %tmp16.unpack)
  %tmp18 = load double, ptr %speed, align 8
  %fun_call_tmp19 = call {} @"[c-flat]:f64:drop"(double %tmp18)
  %tmp20.unpack = load double, ptr %tmp2, align 8
  %tmp20.elt63 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp20.unpack64 = load double, ptr %tmp20.elt63, align 8
  %fun_call_tmp.i36 = call {} @"[c-flat]:f64:drop"(double %tmp20.unpack64)
  %fun_call_tmp2.i38 = call {} @"[c-flat]:f64:drop"(double %tmp20.unpack)
  %tmp22 = load i32, ptr %color, align 4
  %fun_call_tmp23 = call {} @"[c-flat]:u32:drop"(i32 %tmp22)
  %tmp24.unpack = load double, ptr %pos, align 8
  %tmp24.elt66 = getelementptr inbounds nuw i8, ptr %pos, i64 8
  %tmp24.unpack67 = load double, ptr %tmp24.elt66, align 8
  %fun_call_tmp.i = call {} @"[c-flat]:f64:drop"(double %tmp24.unpack67)
  %fun_call_tmp2.i = call {} @"[c-flat]:f64:drop"(double %tmp24.unpack)
  ret %"[c-flat]:Particle" %tmp_constructor
}

define %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:pixel:Game:mouse_pos"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call i32 @crl_get_mouse_x()
  %tmp.pixel_size_factor = getelementptr inbounds nuw i8, ptr %0, i64 40
  %tmp1 = load i64, ptr %tmp.pixel_size_factor, align 4
  %cast = trunc i64 %tmp1 to i32
  %tmp.i13 = sdiv i32 %fun_call_tmp, %cast
  %fun_call_tmp3 = call i32 @crl_get_mouse_y()
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.pixel_size_factor = getelementptr inbounds nuw i8, ptr %tmp4, i64 40
  %tmp5 = load i64, ptr %tmp4.pixel_size_factor, align 4
  %cast6 = trunc i64 %tmp5 to i32
  %tmp.i = sdiv i32 %fun_call_tmp3, %cast6
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = i32>" undef, i32 %tmp.i13, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = i32>" %tmp_agg, i32 %tmp.i, 1
  ret %"[c-flat]:Vec_2<T = i32>" %tmp_constructor
}

declare i32 @crl_get_mouse_x()

declare i32 @crl_get_mouse_y()

define %"[c-flat]:Vec_2<T = f64>" @"[c-flat]:Range<T = i32>:as<T2 = f64>"(%"[c-flat]:Vec_2<T = i32>" %0) {
entry:
  %self = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = i32>" %0, 0
  store i32 %.elt, ptr %self, align 4
  %self.repack4 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %.elt5 = extractvalue %"[c-flat]:Vec_2<T = i32>" %0, 1
  store i32 %.elt5, ptr %self.repack4, align 4
  %cast = sitofp i32 %.elt to double
  %cast2 = sitofp i32 %.elt5 to double
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = f64>" undef, double %cast, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = f64>" %tmp_agg, double %cast2, 1
  %tmp3.unpack = load i32, ptr %self, align 4
  %tmp3.elt6 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %tmp3.unpack7 = load i32, ptr %tmp3.elt6, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp3.unpack7)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp3.unpack)
  ret %"[c-flat]:Vec_2<T = f64>" %tmp_constructor
}

define { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = File>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = File>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i15 = mul i64 %.elt21, 52
  %tmp.i = add i64 %tmp.i15, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"[c-flat]:SliceMutIter<T = File>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"[c-flat]:SliceMutIter<T = File>" %tmp_agg, ptr %cast7, 1
  ret %"[c-flat]:SliceMutIter<T = File>" %tmp_constructor
}

define i1 @"[c-flat]:&mut File:eq"(ptr %0, ptr %1) {
entry:
  %other.i = alloca ptr, align 8
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %other.i)
  store ptr %tmp3, ptr %other.i, align 8
  %tmp.i9.i.i = load double, ptr %tmp1, align 8
  %tmp1.i10.i.i = load double, ptr %tmp3, align 8
  %tmp2.i11.i.i = fcmp oeq double %tmp.i9.i.i, %tmp1.i10.i.i
  %tmp3.i.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4.y.i.i = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp.i.i.i = load double, ptr %tmp3.i.i, align 8
  %tmp1.i.i.i = load double, ptr %tmp4.y.i.i, align 8
  %tmp2.i.i.i = fcmp oeq double %tmp.i.i.i, %tmp1.i.i.i
  %tmp6.i.i = and i1 %tmp2.i11.i.i, %tmp2.i.i.i
  %tmp3.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp4.i = load ptr, ptr %other.i, align 8
  %tmp4.size.i = getelementptr inbounds nuw i8, ptr %tmp4.i, i64 16
  %tmp.i35.i = load double, ptr %tmp3.i, align 8
  %tmp1.i36.i = load double, ptr %tmp4.size.i, align 8
  %tmp2.i37.i = fcmp oeq double %tmp.i35.i, %tmp1.i36.i
  %tmp6.i = and i1 %tmp6.i.i, %tmp2.i37.i
  %tmp7.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp8.i = load ptr, ptr %other.i, align 8
  %tmp8.speed.i = getelementptr inbounds nuw i8, ptr %tmp8.i, i64 24
  %tmp.i30.i = load double, ptr %tmp7.i, align 8
  %tmp1.i31.i = load double, ptr %tmp8.speed.i, align 8
  %tmp2.i32.i = fcmp oeq double %tmp.i30.i, %tmp1.i31.i
  %tmp10.i = and i1 %tmp6.i, %tmp2.i32.i
  %tmp11.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp12.i = load ptr, ptr %other.i, align 8
  %tmp12.new_speed.i = getelementptr inbounds nuw i8, ptr %tmp12.i, i64 32
  %tmp.i25.i = load double, ptr %tmp11.i, align 8
  %tmp1.i26.i = load double, ptr %tmp12.new_speed.i, align 8
  %tmp2.i27.i = fcmp oeq double %tmp.i25.i, %tmp1.i26.i
  %tmp14.i = and i1 %tmp10.i, %tmp2.i27.i
  %tmp15.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 40
  %tmp16.i = load ptr, ptr %other.i, align 8
  %tmp16.offset.i = getelementptr inbounds nuw i8, ptr %tmp16.i, i64 40
  %tmp.i.i = load double, ptr %tmp15.i, align 8
  %tmp1.i.i = load double, ptr %tmp16.offset.i, align 8
  %tmp2.i.i = fcmp oeq double %tmp.i.i, %tmp1.i.i
  %tmp18.i = and i1 %tmp14.i, %tmp2.i.i
  %tmp19.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 48
  %tmp20.i = load ptr, ptr %other.i, align 8
  %tmp20.color.i = getelementptr inbounds nuw i8, ptr %tmp20.i, i64 48
  %tmp.i45.i = load i32, ptr %tmp19.i, align 4
  %tmp1.i46.i = load i32, ptr %tmp20.color.i, align 4
  %tmp2.i47.i = icmp eq i32 %tmp.i45.i, %tmp1.i46.i
  %tmp22.i = and i1 %tmp18.i, %tmp2.i47.i
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %other.i)
  ret i1 %tmp22.i
}

define i1 @"[c-flat]:Vec<T = File>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"[c-flat]:Vec<T = File>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 52
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define i1 @"[c-flat]:pixel:Game:is_key_down"(ptr %0, i8 %1) {
entry:
  %fun_call_tmp = call i1 @crl_is_key_down(i8 %1)
  %fun_call_tmp2 = call {} @"[c-flat]:char:drop"(i8 %1)
  ret i1 %fun_call_tmp
}

declare i1 @crl_is_key_down(i8)

define {} @"[c-flat]:File:update"(ptr %0, ptr %1, ptr %2) {
entry:
  %tmp5 = alloca double, align 8
  %floor = alloca double, align 8
  %prev_pos = alloca %"[c-flat]:Vec_2<T = f64>", align 8
  %particles = alloca ptr, align 8
  %game = alloca ptr, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store ptr %1, ptr %game, align 8
  store ptr %2, ptr %particles, align 8
  %tmp.pos = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1.unpack = load double, ptr %tmp.pos, align 8
  %tmp1.elt137 = getelementptr inbounds nuw i8, ptr %0, i64 16
  %tmp1.unpack138 = load double, ptr %tmp1.elt137, align 8
  store double %tmp1.unpack, ptr %prev_pos, align 8
  %prev_pos.repack140 = getelementptr inbounds nuw i8, ptr %prev_pos, i64 8
  store double %tmp1.unpack138, ptr %prev_pos.repack140, align 8
  %tmp2 = load ptr, ptr %self, align 8
  %tmp2.velocity = getelementptr inbounds nuw i8, ptr %tmp2, i64 24
  %tmp3 = load ptr, ptr %game, align 8
  %fun_call_tmp = call i1 @"[c-flat]:pixel:Game:is_key_down"(ptr %tmp3, i8 68)
  br i1 %fun_call_tmp, label %merge, label %cond5

merge:                                            ; preds = %cond5, %entry
  %result = phi double [ 1.000000e+00, %entry ], [ %spec.select, %cond5 ]
  store double %result, ptr %tmp2.velocity, align 8
  %tmp11 = load ptr, ptr %self, align 8
  %tmp11.velocity.y = getelementptr inbounds nuw i8, ptr %tmp11, i64 32
  %tmp16 = load ptr, ptr %game, align 8
  %fun_call_tmp17 = call i1 @"[c-flat]:pixel:Game:is_key_down"(ptr %tmp16, i8 83)
  br i1 %fun_call_tmp17, label %merge12, label %cond19

cond5:                                            ; preds = %entry
  %tmp7 = load ptr, ptr %game, align 8
  %fun_call_tmp8 = call i1 @"[c-flat]:pixel:Game:is_key_down"(ptr %tmp7, i8 65)
  %spec.select = select i1 %fun_call_tmp8, double -1.000000e+00, double 0.000000e+00
  br label %merge

merge12:                                          ; preds = %cond19, %merge, %then24
  %result27 = phi double [ %tmp26, %then24 ], [ 4.000000e+00, %merge ], [ -4.000000e+00, %cond19 ]
  store double %result27, ptr %tmp11.velocity.y, align 8
  %tmp28 = load ptr, ptr %self, align 8
  %tmp28.velocity.y = getelementptr inbounds nuw i8, ptr %tmp28, i64 32
  %tmp29 = load double, ptr %tmp28.velocity.y, align 8
  %tmp.i111 = fadd double %tmp29, 1.000000e-01
  %tmp31.velocity.y = getelementptr inbounds nuw i8, ptr %tmp28, i64 32
  store double %tmp.i111, ptr %tmp31.velocity.y, align 8
  %tmp32.pos = getelementptr inbounds nuw i8, ptr %tmp28, i64 8
  %tmp33 = load ptr, ptr %self, align 8
  %tmp33.pos = getelementptr inbounds nuw i8, ptr %tmp33, i64 8
  %tmp34.unpack = load double, ptr %tmp33.pos, align 8
  %3 = insertvalue %"[c-flat]:Vec_2<T = f64>" poison, double %tmp34.unpack, 0
  %tmp34.elt142 = getelementptr inbounds nuw i8, ptr %tmp33, i64 16
  %tmp34.unpack143 = load double, ptr %tmp34.elt142, align 8
  %tmp34144 = insertvalue %"[c-flat]:Vec_2<T = f64>" %3, double %tmp34.unpack143, 1
  %tmp35 = load ptr, ptr %self, align 8
  %tmp35.velocity = getelementptr inbounds nuw i8, ptr %tmp35, i64 24
  %tmp36.unpack = load double, ptr %tmp35.velocity, align 8
  %4 = insertvalue %"[c-flat]:Vec_2<T = f64>" poison, double %tmp36.unpack, 0
  %tmp36.elt145 = getelementptr inbounds nuw i8, ptr %tmp35, i64 32
  %tmp36.unpack146 = load double, ptr %tmp36.elt145, align 8
  %tmp36147 = insertvalue %"[c-flat]:Vec_2<T = f64>" %4, double %tmp36.unpack146, 1
  %fun_call_tmp37 = call %"[c-flat]:Vec_2<T = f64>" @"[c-flat]:Vec_2<T = f64>:add"(%"[c-flat]:Vec_2<T = f64>" %tmp34144, %"[c-flat]:Vec_2<T = f64>" %tmp36147)
  %fun_call_tmp37.elt = extractvalue %"[c-flat]:Vec_2<T = f64>" %fun_call_tmp37, 0
  store double %fun_call_tmp37.elt, ptr %tmp32.pos, align 8
  %tmp32.pos.repack148 = getelementptr inbounds nuw i8, ptr %tmp28, i64 16
  %fun_call_tmp37.elt149 = extractvalue %"[c-flat]:Vec_2<T = f64>" %fun_call_tmp37, 1
  store double %fun_call_tmp37.elt149, ptr %tmp32.pos.repack148, align 8
  %tmp38 = load ptr, ptr %game, align 8
  %tmp38.pixel_frame_size.y = getelementptr inbounds nuw i8, ptr %tmp38, i64 32
  %tmp39 = load i64, ptr %tmp38.pixel_frame_size.y, align 4
  %cast = uitofp i64 %tmp39 to double
  %tmp.i114 = fadd double %cast, -3.200000e+01
  store double %tmp.i114, ptr %floor, align 8
  %tmp45 = load ptr, ptr %self, align 8
  %tmp45.pos.y = getelementptr inbounds nuw i8, ptr %tmp45, i64 16
  %tmp46 = load double, ptr %tmp45.pos.y, align 8
  %tmp47 = load double, ptr %floor, align 8
  store double %tmp46, ptr %tmp5, align 8
  %tmp50 = fcmp ult double %tmp46, %tmp47
  br i1 %tmp50, label %merge41, label %then42

cond19:                                           ; preds = %merge
  %tmp21 = load ptr, ptr %game, align 8
  %fun_call_tmp22 = call i1 @"[c-flat]:pixel:Game:is_key_down"(ptr %tmp21, i8 87)
  br i1 %fun_call_tmp22, label %merge12, label %then24

then24:                                           ; preds = %cond19
  %tmp25 = load ptr, ptr %self, align 8
  %tmp25.velocity.y = getelementptr inbounds nuw i8, ptr %tmp25, i64 32
  %tmp26 = load double, ptr %tmp25.velocity.y, align 8
  br label %merge12

merge41:                                          ; preds = %merge12, %then42
  %tmp55 = load ptr, ptr %self, align 8
  %tmp55.anim_vec = getelementptr inbounds nuw i8, ptr %tmp55, i64 40
  %tmp56.anim_vec = getelementptr inbounds nuw i8, ptr %tmp55, i64 40
  %tmp57 = load i64, ptr %tmp56.anim_vec, align 4
  %tmp.i = add i64 %tmp57, 1
  %tmp.i108 = urem i64 %tmp.i, 30
  store i64 %tmp.i108, ptr %tmp55.anim_vec, align 4
  %tmp64 = load ptr, ptr %game, align 8
  %fun_call_tmp65 = call i1 @"[c-flat]:pixel:Game:is_key_down"(ptr %tmp64, i8 80)
  br i1 %fun_call_tmp65, label %then61, label %then86

then42:                                           ; preds = %merge12
  %tmp51 = load ptr, ptr %self, align 8
  %tmp51.pos.y = getelementptr inbounds nuw i8, ptr %tmp51, i64 16
  %tmp52 = load double, ptr %floor, align 8
  store double %tmp52, ptr %tmp51.pos.y, align 8
  %tmp53.velocity.y = getelementptr inbounds nuw i8, ptr %tmp51, i64 32
  store double 0.000000e+00, ptr %tmp53.velocity.y, align 8
  br label %merge41

merge60:                                          ; preds = %then86, %then61
  ret {} zeroinitializer

then61:                                           ; preds = %merge41
  %tmp66 = load ptr, ptr %particles, align 8
  %tmp67 = load ptr, ptr %self, align 8
  %tmp67.pos = getelementptr inbounds nuw i8, ptr %tmp67, i64 8
  %fun_call_tmp68 = call i32 @"[c-flat]:rand_color"()
  %tmp69.unpack = load double, ptr %tmp67.pos, align 8
  %5 = insertvalue %"[c-flat]:Vec_2<T = f64>" poison, double %tmp69.unpack, 0
  %tmp69.elt153 = getelementptr inbounds nuw i8, ptr %tmp67, i64 16
  %tmp69.unpack154 = load double, ptr %tmp69.elt153, align 8
  %tmp69155 = insertvalue %"[c-flat]:Vec_2<T = f64>" %5, double %tmp69.unpack154, 1
  %fun_call_tmp70 = call %"[c-flat]:Particle" @"[c-flat]:File:spawn_at"(%"[c-flat]:Vec_2<T = f64>" %tmp69155, i32 %fun_call_tmp68)
  %fun_call_tmp71 = call {} @"[c-flat]:Vec<T = File>:push"(ptr %tmp66, %"[c-flat]:Particle" %fun_call_tmp70)
  %tmp72 = load ptr, ptr %self, align 8
  %tmp72.anim_vec.y = getelementptr inbounds nuw i8, ptr %tmp72, i64 48
  store i64 9, ptr %tmp72.anim_vec.y, align 4
  %tmp74 = load double, ptr %tmp5, align 8
  %fun_call_tmp75 = call {} @"[c-flat]:f64:drop"(double %tmp74)
  %tmp76 = load double, ptr %floor, align 8
  %fun_call_tmp77 = call {} @"[c-flat]:f64:drop"(double %tmp76)
  %tmp78.unpack = load double, ptr %prev_pos, align 8
  %tmp78.elt156 = getelementptr inbounds nuw i8, ptr %prev_pos, i64 8
  %tmp78.unpack157 = load double, ptr %tmp78.elt156, align 8
  %fun_call_tmp.i128 = call {} @"[c-flat]:f64:drop"(double %tmp78.unpack157)
  %fun_call_tmp2.i130 = call {} @"[c-flat]:f64:drop"(double %tmp78.unpack)
  br label %merge60

then86:                                           ; preds = %merge41
  %tmp87 = load ptr, ptr %self, align 8
  %tmp87.anim_vec.y = getelementptr inbounds nuw i8, ptr %tmp87, i64 48
  store i64 6, ptr %tmp87.anim_vec.y, align 4
  %tmp89 = load double, ptr %tmp5, align 8
  %fun_call_tmp90 = call {} @"[c-flat]:f64:drop"(double %tmp89)
  %tmp91 = load double, ptr %floor, align 8
  %fun_call_tmp92 = call {} @"[c-flat]:f64:drop"(double %tmp91)
  %tmp93.unpack = load double, ptr %prev_pos, align 8
  %tmp93.elt150 = getelementptr inbounds nuw i8, ptr %prev_pos, i64 8
  %tmp93.unpack151 = load double, ptr %tmp93.elt150, align 8
  %fun_call_tmp.i = call {} @"[c-flat]:f64:drop"(double %tmp93.unpack151)
  %fun_call_tmp2.i = call {} @"[c-flat]:f64:drop"(double %tmp93.unpack)
  br label %merge60
}

define %"[c-flat]:Vec_2<T = f64>" @"[c-flat]:Vec_2<T = f64>:add"(%"[c-flat]:Vec_2<T = f64>" %0, %"[c-flat]:Vec_2<T = f64>" %1) {
entry:
  %other = alloca %"[c-flat]:Vec_2<T = f64>", align 8
  %self = alloca %"[c-flat]:Vec_2<T = f64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = f64>" %0, 0
  store double %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"[c-flat]:Vec_2<T = f64>" %0, 1
  store double %.elt20, ptr %self.repack19, align 8
  %.elt21 = extractvalue %"[c-flat]:Vec_2<T = f64>" %1, 0
  store double %.elt21, ptr %other, align 8
  %other.repack22 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %.elt23 = extractvalue %"[c-flat]:Vec_2<T = f64>" %1, 1
  store double %.elt23, ptr %other.repack22, align 8
  %tmp = load double, ptr %self, align 8
  %tmp.i11 = fadd double %tmp, %.elt21
  %self.y = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp2 = load double, ptr %self.y, align 8
  %tmp.i = fadd double %tmp2, %.elt23
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = f64>" undef, double %tmp.i11, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = f64>" %tmp_agg, double %tmp.i, 1
  %tmp5.unpack = load double, ptr %other, align 8
  %tmp5.elt24 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %tmp5.unpack25 = load double, ptr %tmp5.elt24, align 8
  %fun_call_tmp.i16 = call {} @"[c-flat]:f64:drop"(double %tmp5.unpack25)
  %fun_call_tmp2.i18 = call {} @"[c-flat]:f64:drop"(double %tmp5.unpack)
  %tmp7.unpack = load double, ptr %self, align 8
  %tmp7.elt27 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp7.unpack28 = load double, ptr %tmp7.elt27, align 8
  %fun_call_tmp.i = call {} @"[c-flat]:f64:drop"(double %tmp7.unpack28)
  %fun_call_tmp2.i = call {} @"[c-flat]:f64:drop"(double %tmp7.unpack)
  ret %"[c-flat]:Vec_2<T = f64>" %tmp_constructor
}

define {} @"[c-flat]:File:update.82"(ptr %0, %"[c-flat]:Vec_2<T = u64>" %1) {
entry:
  %tmp262 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp237 = alloca %"[c-flat]:Range<T = f64>", align 8
  %screen_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack117 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt118 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 1
  store i64 %.elt118, ptr %screen_size.repack117, align 4
  %tmp.speed = getelementptr inbounds nuw i8, ptr %0, i64 24
  %tmp1 = load ptr, ptr %self, align 8
  %tmp1.speed = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp2 = load double, ptr %tmp1.speed, align 8
  %tmp3.new_speed = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp4 = load double, ptr %tmp3.new_speed, align 8
  %fun_call_tmp = call double @"[c-flat]:f64:lerp"(double %tmp2, double %tmp4, double 5.000000e-02)
  store double %fun_call_tmp, ptr %tmp.speed, align 8
  %tmp5 = load ptr, ptr %self, align 8
  %tmp6 = load double, ptr %tmp5, align 8
  %tmp7.speed = getelementptr inbounds nuw i8, ptr %tmp5, i64 24
  %tmp8 = load double, ptr %tmp7.speed, align 8
  %tmp.i88 = fadd double %tmp6, %tmp8
  store double %tmp.i88, ptr %tmp5, align 8
  %tmp11.pos.y = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  %tmp12 = load double, ptr %tmp11.pos.y, align 8
  %tmp13 = load ptr, ptr %self, align 8
  %tmp13.offset = getelementptr inbounds nuw i8, ptr %tmp13, i64 40
  %tmp14 = load double, ptr %tmp13.offset, align 8
  %fun_call_tmp15 = call double @"[c-flat]:f64:sin"(double %tmp14)
  %tmp.i85 = fadd double %tmp12, %fun_call_tmp15
  %tmp17.pos.y = getelementptr inbounds nuw i8, ptr %tmp13, i64 8
  store double %tmp.i85, ptr %tmp17.pos.y, align 8
  %tmp18 = load ptr, ptr %self, align 8
  %tmp18.offset = getelementptr inbounds nuw i8, ptr %tmp18, i64 40
  %tmp19 = load double, ptr %tmp18.offset, align 8
  %tmp20.speed = getelementptr inbounds nuw i8, ptr %tmp18, i64 24
  %tmp21 = load double, ptr %tmp20.speed, align 8
  %tmp.i91 = fdiv double %tmp21, 4.600000e+01
  %fun_call_tmp23 = call double @"[c-flat]:f64:min"(double 5.000000e-02, double %tmp.i91)
  %tmp.i82 = fadd double %tmp19, %fun_call_tmp23
  %tmp25 = load ptr, ptr %self, align 8
  %tmp25.offset = getelementptr inbounds nuw i8, ptr %tmp25, i64 40
  store double %tmp.i82, ptr %tmp25.offset, align 8
  %tmp26 = load ptr, ptr %self, align 8
  %tmp27 = load double, ptr %tmp26, align 8
  %tmp28 = load i64, ptr %screen_size, align 4
  %cast = uitofp i64 %tmp28 to double
  %tmp.i79 = fadd double %cast, 4.000000e+00
  %tmp.i99 = fcmp ogt double %tmp27, %tmp.i79
  br i1 %tmp.i99, label %then, label %cond46

merge:                                            ; preds = %cond46, %then45, %then
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp31 = load ptr, ptr %self, align 8
  store double -4.000000e+00, ptr %tmp31, align 8
  %tmp33.pos.y = getelementptr inbounds nuw i8, ptr %tmp31, i64 8
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp34 = load i64, ptr %screen_size.y, align 4
  %cast35 = uitofp i64 %tmp34 to double
  store double 0.000000e+00, ptr %tmp237, align 8
  %tmp237.repack127 = getelementptr inbounds nuw i8, ptr %tmp237, i64 8
  store double %cast35, ptr %tmp237.repack127, align 8
  %fun_call_tmp38 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp237)
  store double %fun_call_tmp38, ptr %tmp33.pos.y, align 8
  %tmp39.unpack = load double, ptr %tmp237, align 8
  %tmp39.elt129 = getelementptr inbounds nuw i8, ptr %tmp237, i64 8
  %tmp39.unpack130 = load double, ptr %tmp39.elt129, align 8
  %fun_call_tmp.i112 = call {} @"[c-flat]:f64:drop"(double %tmp39.unpack130)
  %fun_call_tmp2.i114 = call {} @"[c-flat]:f64:drop"(double %tmp39.unpack)
  %tmp41.unpack = load i64, ptr %screen_size, align 4
  %tmp41.elt132 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp41.unpack133 = load i64, ptr %tmp41.elt132, align 4
  %fun_call_tmp.i72 = call {} @"[c-flat]:u64:drop"(i64 %tmp41.unpack133)
  %fun_call_tmp2.i74 = call {} @"[c-flat]:u64:drop"(i64 %tmp41.unpack)
  br label %merge

then45:                                           ; preds = %cond46
  %tmp52 = load ptr, ptr %self, align 8
  %tmp54 = load i64, ptr %screen_size, align 4
  %cast55 = uitofp i64 %tmp54 to double
  %tmp.i76 = fadd double %cast55, 4.000000e+00
  store double %tmp.i76, ptr %tmp52, align 8
  %tmp57.pos.y = getelementptr inbounds nuw i8, ptr %tmp52, i64 8
  %screen_size.y58 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp59 = load i64, ptr %screen_size.y58, align 4
  %cast60 = uitofp i64 %tmp59 to double
  store double 0.000000e+00, ptr %tmp262, align 8
  %tmp262.repack119 = getelementptr inbounds nuw i8, ptr %tmp262, i64 8
  store double %cast60, ptr %tmp262.repack119, align 8
  %fun_call_tmp63 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp262)
  store double %fun_call_tmp63, ptr %tmp57.pos.y, align 8
  %tmp64.unpack = load double, ptr %tmp262, align 8
  %tmp64.elt121 = getelementptr inbounds nuw i8, ptr %tmp262, i64 8
  %tmp64.unpack122 = load double, ptr %tmp64.elt121, align 8
  %fun_call_tmp.i107 = call {} @"[c-flat]:f64:drop"(double %tmp64.unpack122)
  %fun_call_tmp2.i109 = call {} @"[c-flat]:f64:drop"(double %tmp64.unpack)
  %tmp66.unpack = load i64, ptr %screen_size, align 4
  %tmp66.elt124 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp66.unpack125 = load i64, ptr %tmp66.elt124, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp66.unpack125)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp66.unpack)
  br label %merge

cond46:                                           ; preds = %entry
  %tmp48 = load ptr, ptr %self, align 8
  %tmp49 = load double, ptr %tmp48, align 8
  %tmp.i96 = fcmp olt double %tmp49, -4.000000e+00
  br i1 %tmp.i96, label %then45, label %merge
}

define double @"[c-flat]:f64:lerp"(double %0, double %1, double %2) {
entry:
  %self = alloca double, align 8
  store double %0, ptr %self, align 8
  %tmp.i21 = fmul double %1, %2
  %tmp.i15 = fsub double 1.000000e+00, %2
  %tmp.i18 = fmul double %0, %tmp.i15
  %tmp.i = fadd double %tmp.i21, %tmp.i18
  %fun_call_tmp8 = call {} @"[c-flat]:f64:drop"(double %2)
  %fun_call_tmp10 = call {} @"[c-flat]:f64:drop"(double %1)
  %tmp11 = load double, ptr %self, align 8
  %fun_call_tmp12 = call {} @"[c-flat]:f64:drop"(double %tmp11)
  ret double %tmp.i
}

define double @"[c-flat]:f64:sin"(double %0) {
entry:
  %fun_call_tmp = call double @c_sin(double %0)
  %fun_call_tmp2 = call {} @"[c-flat]:f64:drop"(double %0)
  ret double %fun_call_tmp
}

declare double @c_sin(double)

define double @"[c-flat]:f64:min"(double %0, double %1) {
entry:
  %other = alloca double, align 8
  %self = alloca double, align 8
  store double %0, ptr %self, align 8
  store double %1, ptr %other, align 8
  %tmp = load double, ptr %self, align 8
  %tmp1 = load double, ptr %other, align 8
  %tmp.i = fcmp olt double %tmp, %tmp1
  br i1 %tmp.i, label %then, label %then7

merge:                                            ; preds = %then7, %then
  %result = phi double [ %tmp2, %then ], [ %tmp8, %then7 ]
  ret double %result

then:                                             ; preds = %entry
  %tmp2 = load double, ptr %self, align 8
  %tmp3 = load double, ptr %other, align 8
  %fun_call_tmp4 = call {} @"[c-flat]:f64:drop"(double %tmp3)
  %fun_call_tmp6 = call {} @"[c-flat]:f64:drop"(double %tmp2)
  br label %merge

then7:                                            ; preds = %entry
  %tmp8 = load double, ptr %other, align 8
  %fun_call_tmp10 = call {} @"[c-flat]:f64:drop"(double %tmp8)
  %tmp11 = load double, ptr %self, align 8
  %fun_call_tmp12 = call {} @"[c-flat]:f64:drop"(double %tmp11)
  br label %merge
}

define {} @"[c-flat]:pixel:Game:begin_draw"(ptr %0) {
entry:
  %fun_call_tmp = call {} @crl_begin_canvas()
  ret {} zeroinitializer
}

declare {} @crl_begin_canvas()

define {} @"[c-flat]:pixel:Game:clear_background"(ptr %0) {
entry:
  %fun_call_tmp = call {} @crl_clear_background(i32 -16777216)
  ret {} zeroinitializer
}

declare {} @crl_clear_background(i32)

define {} @"[c-flat]:File:render"(ptr %0, ptr %1) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp2.pos = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3.unpack = load double, ptr %tmp2.pos, align 8
  %2 = insertvalue %"[c-flat]:Vec_2<T = f64>" poison, double %tmp3.unpack, 0
  %tmp3.elt14 = getelementptr inbounds nuw i8, ptr %0, i64 16
  %tmp3.unpack15 = load double, ptr %tmp3.elt14, align 8
  %tmp316 = insertvalue %"[c-flat]:Vec_2<T = f64>" %2, double %tmp3.unpack15, 1
  %fun_call_tmp = call %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:Vec_2<T = f64>:as<T2 = i32>"(%"[c-flat]:Vec_2<T = f64>" %tmp316)
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.anim_vec = getelementptr inbounds nuw i8, ptr %tmp4, i64 40
  %tmp5.unpack = load i64, ptr %tmp4.anim_vec, align 4
  %3 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp5.unpack, 0
  %tmp5.elt17 = getelementptr inbounds nuw i8, ptr %tmp4, i64 48
  %tmp5.unpack18 = load i64, ptr %tmp5.elt17, align 4
  %tmp519 = insertvalue %"[c-flat]:Vec_2<T = u64>" %3, i64 %tmp5.unpack18, 1
  %fun_call_tmp6 = call %"[c-flat]:Vec_2<T = u64>" @"[c-flat]:Vec_2<T = u64>:div"(%"[c-flat]:Vec_2<T = u64>" %tmp519, i64 3)
  %tmp7 = load i32, ptr %0, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:pixel:Game:draw_animation_texture"(ptr %1, i32 %tmp7, %"[c-flat]:Vec_2<T = i32>" %fun_call_tmp, %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp6, %"[c-flat]:Vec_2<T = u64>" { i64 32, i64 32 })
  ret {} zeroinitializer
}

define {} @"[c-flat]:pixel:Game:draw_animation_texture"(ptr %0, i32 %1, %"[c-flat]:Vec_2<T = i32>" %2, %"[c-flat]:Vec_2<T = u64>" %3, %"[c-flat]:Vec_2<T = u64>" %4) {
entry:
  %src_pos = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %index = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %pos = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %texture = alloca i32, align 4
  store i32 %1, ptr %texture, align 4
  %.elt = extractvalue %"[c-flat]:Vec_2<T = i32>" %2, 0
  store i32 %.elt, ptr %pos, align 4
  %pos.repack50 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %.elt51 = extractvalue %"[c-flat]:Vec_2<T = i32>" %2, 1
  store i32 %.elt51, ptr %pos.repack50, align 4
  %.elt52 = extractvalue %"[c-flat]:Vec_2<T = u64>" %3, 0
  store i64 %.elt52, ptr %index, align 4
  %index.repack53 = getelementptr inbounds nuw i8, ptr %index, i64 8
  %.elt54 = extractvalue %"[c-flat]:Vec_2<T = u64>" %3, 1
  store i64 %.elt54, ptr %index.repack53, align 4
  %.elt55 = extractvalue %"[c-flat]:Vec_2<T = u64>" %4, 0
  store i64 %.elt55, ptr %size, align 4
  %size.repack56 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %.elt57 = extractvalue %"[c-flat]:Vec_2<T = u64>" %4, 1
  store i64 %.elt57, ptr %size.repack56, align 4
  %tmp = load i64, ptr %index, align 4
  %tmp.i31 = mul i64 %tmp, %.elt55
  %index.y = getelementptr inbounds nuw i8, ptr %index, i64 8
  %tmp2 = load i64, ptr %index.y, align 4
  %tmp.i = mul i64 %tmp2, %.elt57
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = u64>" undef, i64 %tmp.i31, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = u64>" %tmp_agg, i64 %tmp.i, 1
  %fun_call_tmp5 = call %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:Vec_2<T = u64>:as<T2 = i32>"(%"[c-flat]:Vec_2<T = u64>" %tmp_constructor)
  %fun_call_tmp5.elt = extractvalue %"[c-flat]:Vec_2<T = i32>" %fun_call_tmp5, 0
  store i32 %fun_call_tmp5.elt, ptr %src_pos, align 4
  %src_pos.repack58 = getelementptr inbounds nuw i8, ptr %src_pos, i64 4
  %fun_call_tmp5.elt59 = extractvalue %"[c-flat]:Vec_2<T = i32>" %fun_call_tmp5, 1
  store i32 %fun_call_tmp5.elt59, ptr %src_pos.repack58, align 4
  %src_pos.y = getelementptr inbounds nuw i8, ptr %src_pos, i64 4
  %tmp7 = load i64, ptr %size, align 4
  %cast = trunc i64 %tmp7 to i32
  %size.y8 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %tmp9 = load i64, ptr %size.y8, align 4
  %cast10 = trunc i64 %tmp9 to i32
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp11 = load i32, ptr %texture, align 4
  %tmp12 = load i32, ptr %src_pos, align 4
  %tmp13 = load i32, ptr %src_pos.y, align 4
  %tmp14 = load i32, ptr %pos, align 4
  %tmp15 = load i32, ptr %pos.y, align 4
  %fun_call_tmp16 = call {} @crl_draw_texture_rec(i32 %tmp11, i32 %tmp12, i32 %tmp13, i32 %cast, i32 %cast10, i32 %tmp14, i32 %tmp15, i32 -1)
  %fun_call_tmp.i47 = call {} @"[c-flat]:i32:drop"(i32 %tmp13)
  %fun_call_tmp2.i49 = call {} @"[c-flat]:i32:drop"(i32 %tmp12)
  %tmp19.unpack = load i64, ptr %size, align 4
  %tmp19.elt63 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %tmp19.unpack64 = load i64, ptr %tmp19.elt63, align 4
  %fun_call_tmp.i36 = call {} @"[c-flat]:u64:drop"(i64 %tmp19.unpack64)
  %fun_call_tmp2.i38 = call {} @"[c-flat]:u64:drop"(i64 %tmp19.unpack)
  %tmp21.unpack = load i64, ptr %index, align 4
  %tmp21.elt66 = getelementptr inbounds nuw i8, ptr %index, i64 8
  %tmp21.unpack67 = load i64, ptr %tmp21.elt66, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp21.unpack67)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp21.unpack)
  %tmp23.unpack = load i32, ptr %pos, align 4
  %tmp23.elt69 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp23.unpack70 = load i32, ptr %tmp23.elt69, align 4
  %fun_call_tmp.i42 = call {} @"[c-flat]:i32:drop"(i32 %tmp23.unpack70)
  %fun_call_tmp2.i44 = call {} @"[c-flat]:i32:drop"(i32 %tmp23.unpack)
  %tmp25 = load i32, ptr %texture, align 4
  %fun_call_tmp26 = call {} @"[c-flat]:i32:drop"(i32 %tmp25)
  ret {} zeroinitializer
}

define %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:Vec_2<T = u64>:as<T2 = i32>"(%"[c-flat]:Vec_2<T = u64>" %0) {
entry:
  %self = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %self, align 4
  %self.repack4 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt5 = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 1
  store i64 %.elt5, ptr %self.repack4, align 4
  %cast = trunc i64 %.elt to i32
  %cast2 = trunc i64 %.elt5 to i32
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = i32>" undef, i32 %cast, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = i32>" %tmp_agg, i32 %cast2, 1
  %tmp3.unpack = load i64, ptr %self, align 4
  %tmp3.elt6 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp3.unpack7 = load i64, ptr %tmp3.elt6, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp3.unpack7)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp3.unpack)
  ret %"[c-flat]:Vec_2<T = i32>" %tmp_constructor
}

declare {} @crl_draw_texture_rec(i32, i32, i32, i32, i32, i32, i32, i32)

define %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:Vec_2<T = f64>:as<T2 = i32>"(%"[c-flat]:Vec_2<T = f64>" %0) {
entry:
  %self = alloca %"[c-flat]:Vec_2<T = f64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = f64>" %0, 0
  store double %.elt, ptr %self, align 8
  %self.repack4 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt5 = extractvalue %"[c-flat]:Vec_2<T = f64>" %0, 1
  store double %.elt5, ptr %self.repack4, align 8
  %cast = fptosi double %.elt to i32
  %cast2 = fptosi double %.elt5 to i32
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = i32>" undef, i32 %cast, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = i32>" %tmp_agg, i32 %cast2, 1
  %tmp3.unpack = load double, ptr %self, align 8
  %tmp3.elt6 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp3.unpack7 = load double, ptr %tmp3.elt6, align 8
  %fun_call_tmp.i = call {} @"[c-flat]:f64:drop"(double %tmp3.unpack7)
  %fun_call_tmp2.i = call {} @"[c-flat]:f64:drop"(double %tmp3.unpack)
  ret %"[c-flat]:Vec_2<T = i32>" %tmp_constructor
}

define %"[c-flat]:Vec_2<T = u64>" @"[c-flat]:Vec_2<T = u64>:div"(%"[c-flat]:Vec_2<T = u64>" %0, i64 %1) {
entry:
  %self = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %self, align 4
  %self.repack14 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt15 = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 1
  store i64 %.elt15, ptr %self.repack14, align 4
  %tmp.i11 = udiv i64 %.elt, %1
  %tmp.i = udiv i64 %.elt15, %1
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = u64>" undef, i64 %tmp.i11, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = u64>" %tmp_agg, i64 %tmp.i, 1
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %1)
  %tmp7.unpack = load i64, ptr %self, align 4
  %tmp7.elt16 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp7.unpack17 = load i64, ptr %tmp7.elt16, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp7.unpack17)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp7.unpack)
  ret %"[c-flat]:Vec_2<T = u64>" %tmp_constructor
}

define {} @"[c-flat]:File:render.83"(ptr %0, ptr %1) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp2 = load double, ptr %0, align 8
  %cast = fptosi double %tmp2 to i32
  %tmp3.pos.y = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load double, ptr %tmp3.pos.y, align 8
  %cast5 = fptosi double %tmp4 to i32
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = i32>" undef, i32 %cast, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = i32>" %tmp_agg, i32 %cast5, 1
  %tmp6 = load ptr, ptr %self, align 8
  %tmp6.size = getelementptr inbounds nuw i8, ptr %tmp6, i64 16
  %tmp7 = load double, ptr %tmp6.size, align 8
  %cast8 = fptoui double %tmp7 to i64
  %cast11 = fptoui double %tmp7 to i64
  %tmp_agg12 = insertvalue %"[c-flat]:Vec_2<T = u64>" undef, i64 %cast8, 0
  %tmp_constructor13 = insertvalue %"[c-flat]:Vec_2<T = u64>" %tmp_agg12, i64 %cast11, 1
  %tmp14 = load ptr, ptr %self, align 8
  %tmp14.color = getelementptr inbounds nuw i8, ptr %tmp14, i64 48
  %tmp15 = load i32, ptr %tmp14.color, align 4
  %fun_call_tmp = call {} @"[c-flat]:pixel:Game:draw_rect"(ptr %1, %"[c-flat]:Vec_2<T = i32>" %tmp_constructor, %"[c-flat]:Vec_2<T = u64>" %tmp_constructor13, i32 %tmp15)
  ret {} zeroinitializer
}

define {} @"[c-flat]:pixel:Game:draw_rect"(ptr %0, %"[c-flat]:Vec_2<T = i32>" %1, %"[c-flat]:Vec_2<T = u64>" %2, i32 %3) {
entry:
  %color = alloca i32, align 4
  %size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %pos = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = i32>" %1, 0
  store i32 %.elt, ptr %pos, align 4
  %pos.repack20 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %.elt21 = extractvalue %"[c-flat]:Vec_2<T = i32>" %1, 1
  store i32 %.elt21, ptr %pos.repack20, align 4
  %.elt22 = extractvalue %"[c-flat]:Vec_2<T = u64>" %2, 0
  store i64 %.elt22, ptr %size, align 4
  %size.repack23 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %.elt24 = extractvalue %"[c-flat]:Vec_2<T = u64>" %2, 1
  store i64 %.elt24, ptr %size.repack23, align 4
  store i32 %3, ptr %color, align 4
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %cast = trunc i64 %.elt22 to i32
  %cast2 = trunc i64 %.elt24 to i32
  %tmp3 = load i32, ptr %pos, align 4
  %tmp4 = load i32, ptr %pos.y, align 4
  %fun_call_tmp = call {} @crl_draw_rectangle(i32 %tmp3, i32 %tmp4, i32 %cast, i32 %cast2, i32 %3)
  %tmp6 = load i32, ptr %color, align 4
  %fun_call_tmp7 = call {} @"[c-flat]:u32:drop"(i32 %tmp6)
  %tmp8.unpack = load i64, ptr %size, align 4
  %tmp8.elt25 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %tmp8.unpack26 = load i64, ptr %tmp8.elt25, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp8.unpack26)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp8.unpack)
  %tmp10.unpack = load i32, ptr %pos, align 4
  %tmp10.elt28 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp10.unpack29 = load i32, ptr %tmp10.elt28, align 4
  %fun_call_tmp.i17 = call {} @"[c-flat]:i32:drop"(i32 %tmp10.unpack29)
  %fun_call_tmp2.i19 = call {} @"[c-flat]:i32:drop"(i32 %tmp10.unpack)
  ret {} zeroinitializer
}

declare {} @crl_draw_rectangle(i32, i32, i32, i32, i32)

define {} @"[c-flat]:pixel:Game:draw_text"(ptr %0, { ptr, i64 } %1, %"[c-flat]:Vec_2<T = i32>" %2, i32 %3) {
entry:
  %tmp4 = alloca %"[c-flat]:Vec<T = char>", align 8
  %color = alloca i32, align 4
  %pos = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %text = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %text, align 8
  %text.repack22 = getelementptr inbounds nuw i8, ptr %text, i64 8
  %.elt23 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt23, ptr %text.repack22, align 8
  %.elt24 = extractvalue %"[c-flat]:Vec_2<T = i32>" %2, 0
  store i32 %.elt24, ptr %pos, align 4
  %pos.repack25 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %.elt26 = extractvalue %"[c-flat]:Vec_2<T = i32>" %2, 1
  store i32 %.elt26, ptr %pos.repack25, align 4
  store i32 %3, ptr %color, align 4
  %tmp.unpack = load ptr, ptr %text, align 8
  %4 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt27 = getelementptr inbounds nuw i8, ptr %text, i64 8
  %tmp.unpack28 = load i64, ptr %tmp.elt27, align 8
  %tmp29 = insertvalue { ptr, i64 } %4, i64 %tmp.unpack28, 1
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %tmp29)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp4, align 8
  %tmp4.repack30 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %fun_call_tmp1.elt31 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt31, ptr %tmp4.repack30, align 8
  %tmp4.repack32 = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %fun_call_tmp1.elt33 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt33, ptr %tmp4.repack32, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp4)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp5 = load i32, ptr %pos, align 4
  %tmp6 = load i32, ptr %pos.y, align 4
  %tmp7 = load i32, ptr %color, align 4
  %fun_call_tmp8 = call {} @crl_draw_text(ptr %ref.i, i32 %tmp5, i32 %tmp6, i32 %tmp7)
  %tmp9.unpack = load ptr, ptr %tmp4, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp9.unpack, 0
  %tmp9.elt34 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %tmp9.unpack35 = load i64, ptr %tmp9.elt34, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" %5, i64 %tmp9.unpack35, 1
  %tmp9.elt36 = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp9.unpack37 = load i64, ptr %tmp9.elt36, align 8
  %tmp938 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp9.unpack37, 2
  %fun_call_tmp10 = call {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %tmp938)
  %tmp11 = load i32, ptr %color, align 4
  %fun_call_tmp12 = call {} @"[c-flat]:u32:drop"(i32 %tmp11)
  %tmp13.unpack = load i32, ptr %pos, align 4
  %tmp13.elt39 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp13.unpack40 = load i32, ptr %tmp13.elt39, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp13.unpack40)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp13.unpack)
  ret {} zeroinitializer
}

declare {} @crl_draw_text(ptr, i32, i32, i32)

define {} @"[c-flat]:pixel:Game:end_draw"(ptr %0) {
entry:
  %tmp.frame_size = getelementptr inbounds nuw i8, ptr %0, i64 48
  %tmp1 = load i64, ptr %tmp.frame_size, align 4
  %cast = trunc i64 %tmp1 to i32
  %tmp2.frame_size.y = getelementptr inbounds nuw i8, ptr %0, i64 56
  %tmp3 = load i64, ptr %tmp2.frame_size.y, align 4
  %cast4 = trunc i64 %tmp3 to i32
  %fun_call_tmp = call {} @crl_end_canvas(i32 %cast, i32 %cast4)
  ret {} zeroinitializer
}

declare {} @crl_end_canvas(i32, i32)

define {} @"[c-flat]:pixel:Game:close"(ptr %0) {
entry:
  %fun_call_tmp = call {} @crl_close_window()
  ret {} zeroinitializer
}

declare {} @crl_close_window()

define {} @"[c-flat]:Vec<T = File>:drop.84"(%"[c-flat]:Vec<T = File>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"[c-flat]:Vec<T = File>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = File>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt22 = extractvalue %"[c-flat]:Vec<T = File>" %0, 1
  store i64 %.elt22, ptr %self.repack21, align 8
  %self.repack23 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt24 = extractvalue %"[c-flat]:Vec<T = File>" %0, 2
  store i64 %.elt24, ptr %self.repack23, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr %"[c-flat]:Particle", ptr %ref.i, i64 %tmp4
  %tmp6 = load %"[c-flat]:Particle", ptr %tmp_index.i, align 8
  %tmp.i18 = extractvalue %"[c-flat]:Particle" %tmp6, 5
  %fun_call_tmp.i = call {} @"[c-flat]:u32:drop"(i32 %tmp.i18)
  %tmp1.i = extractvalue %"[c-flat]:Particle" %tmp6, 4
  %fun_call_tmp2.i = call {} @"[c-flat]:f64:drop"(double %tmp1.i)
  %tmp3.i = extractvalue %"[c-flat]:Particle" %tmp6, 3
  %fun_call_tmp4.i = call {} @"[c-flat]:f64:drop"(double %tmp3.i)
  %tmp5.i = extractvalue %"[c-flat]:Particle" %tmp6, 2
  %fun_call_tmp6.i = call {} @"[c-flat]:f64:drop"(double %tmp5.i)
  %tmp7.i = extractvalue %"[c-flat]:Particle" %tmp6, 1
  %fun_call_tmp8.i = call {} @"[c-flat]:f64:drop"(double %tmp7.i)
  %tmp9.i = extractvalue %"[c-flat]:Particle" %tmp6, 0
  %tmp.i.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp9.i, 1
  %fun_call_tmp.i.i = call {} @"[c-flat]:f64:drop"(double %tmp.i.i)
  %tmp1.i.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp9.i, 0
  %fun_call_tmp2.i.i = call {} @"[c-flat]:f64:drop"(double %tmp1.i.i)
  %tmp8 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp8, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"[c-flat]:mem:free<T = File>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"[c-flat]:Vec<T = File>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = File>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = File>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define {} @"[c-flat]:println"({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp1, align 8
  %tmp1.repack16 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp1.elt17 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt17, ptr %tmp1.repack16, align 8
  %tmp1.repack18 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp1.elt19 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt19, ptr %tmp1.repack18, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call {} @c_puts(ptr %ref.i)
  %fun_call_tmp5 = call {} @c_flush()
  %tmp6.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp6.unpack, 0
  %tmp6.elt20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp6.unpack21 = load i64, ptr %tmp6.elt20, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp6.unpack21, 1
  %tmp6.elt22 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp6.unpack23 = load i64, ptr %tmp6.elt22, align 8
  %tmp624 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp6.unpack23, 2
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %tmp624)
  ret {} zeroinitializer
}

declare {} @c_puts(ptr)

declare {} @c_flush()

define {} @"[c-flat]:debug_heap_count"() {
entry:
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %heap_count = alloca i64, align 8
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  store i64 %tmp, ptr %heap_count, align 4
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:u64:to_string"(ptr nonnull %heap_count)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp1, align 8
  %tmp1.repack8 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp1.elt9 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt9, ptr %tmp1.repack8, align 8
  %tmp1.repack10 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp1.elt11 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt11, ptr %tmp1.repack10, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call {} @"[c-flat]:println"({ ptr, i64 } %fun_call_tmp2)
  %tmp4.unpack = load ptr, ptr %tmp1, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp4.unpack, 0
  %tmp4.elt12 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4.unpack13 = load i64, ptr %tmp4.elt12, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp4.unpack13, 1
  %tmp4.elt14 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp4.unpack15 = load i64, ptr %tmp4.elt14, align 8
  %tmp416 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp4.unpack15, 2
  %fun_call_tmp5 = call {} @"[c-flat]:Vec<T = char>:drop.26"(%"[c-flat]:Vec<T = char>" %tmp416)
  %tmp6 = load i64, ptr %heap_count, align 4
  %fun_call_tmp7 = call {} @"[c-flat]:u64:drop"(i64 %tmp6)
  ret {} zeroinitializer
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:u64:to_string"(ptr %0) {
entry:
  %tmp426 = alloca i64, align 8
  %num = alloca i64, align 8
  %result7 = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp1 = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp = load ptr, ptr %self, align 8
  store i64 0, ptr %tmp1, align 4
  %tmp.i73 = load i64, ptr %tmp, align 4
  %tmp2.i75 = icmp eq i64 %tmp.i73, 0
  br i1 %tmp2.i75, label %then, label %else

common.ret:                                       ; preds = %else39, %then
  %common.ret.op = phi %"[c-flat]:Vec<T = char>" [ %fun_call_tmp1, %then ], [ %tmp4586, %else39 ]
  ret %"[c-flat]:Vec<T = char>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.87, i64 1 })
  %tmp2 = load i64, ptr %tmp1, align 4
  %fun_call_tmp3 = call {} @"[c-flat]:u64:drop"(i64 %tmp2)
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp6 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:new"()
  %fun_call_tmp6.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %result7, align 8
  %result7.repack78 = getelementptr inbounds nuw i8, ptr %result7, i64 8
  %fun_call_tmp6.elt79 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp6, 1
  store i64 %fun_call_tmp6.elt79, ptr %result7.repack78, align 8
  %result7.repack80 = getelementptr inbounds nuw i8, ptr %result7, i64 16
  %fun_call_tmp6.elt81 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp6, 2
  store i64 %fun_call_tmp6.elt81, ptr %result7.repack80, align 8
  %tmp19 = load ptr, ptr %self, align 8
  %tmp20 = load i64, ptr %tmp19, align 4
  br label %cond24

then23:                                           ; preds = %cond24
  %tmp29 = load i64, ptr %num, align 4
  %tmp.i61 = urem i64 %tmp29, 10
  %1 = trunc nuw nsw i64 %tmp.i61 to i8
  %cast = or disjoint i8 %1, 48
  %fun_call_tmp32 = call {} @"[c-flat]:Vec<T = char>:insert"(ptr nonnull %result7, i64 0, i8 %cast)
  %tmp.i56 = udiv i64 %tmp29, 10
  br label %cond24

cond24:                                           ; preds = %then23, %else
  %storemerge = phi i64 [ %tmp20, %else ], [ %tmp.i56, %then23 ]
  store i64 %storemerge, ptr %num, align 4
  store i64 0, ptr %tmp426, align 4
  %tmp2.i.not = icmp eq i64 %storemerge, 0
  br i1 %tmp2.i.not, label %else39, label %then23

else39:                                           ; preds = %cond24
  %tmp45.unpack = load ptr, ptr %result7, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp45.unpack, 0
  %tmp45.elt82 = getelementptr inbounds nuw i8, ptr %result7, i64 8
  %tmp45.unpack83 = load i64, ptr %tmp45.elt82, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp45.unpack83, 1
  %tmp45.elt84 = getelementptr inbounds nuw i8, ptr %result7, i64 16
  %tmp45.unpack85 = load i64, ptr %tmp45.elt84, align 8
  %tmp4586 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp45.unpack85, 2
  %tmp46 = load i64, ptr %tmp426, align 4
  %fun_call_tmp47 = call {} @"[c-flat]:u64:drop"(i64 %tmp46)
  %tmp48 = load i64, ptr %num, align 4
  %fun_call_tmp49 = call {} @"[c-flat]:u64:drop"(i64 %tmp48)
  %tmp50 = load i64, ptr %tmp1, align 4
  %fun_call_tmp51 = call {} @"[c-flat]:u64:drop"(i64 %tmp50)
  br label %common.ret
}

define {} @"[c-flat]:Vec<T = char>:insert"(ptr %0, i64 %1, i8 %2) {
entry:
  %insert_ptr = alloca ptr, align 8
  %type_mem_size = alloca i64, align 8
  %value = alloca i8, align 1
  %idx = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i64 %1, ptr %idx, align 4
  store i8 %2, ptr %value, align 1
  store i64 1, ptr %type_mem_size, align 4
  %fun_call_tmp1 = call {} @"[c-flat]:Vec<T = char>:reserve"(ptr %0, i64 1)
  %tmp3 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp3 to i64
  %tmp.i45 = add i64 %1, %cast
  %cast8 = inttoptr i64 %tmp.i45 to ptr
  store ptr %cast8, ptr %insert_ptr, align 8
  %tmp11 = load i64, ptr %type_mem_size, align 4
  %tmp.i42 = add i64 %tmp.i45, %tmp11
  %cast13 = inttoptr i64 %tmp.i42 to ptr
  %tmp14 = load ptr, ptr %self, align 8
  %tmp14.len = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  %tmp15 = load i64, ptr %tmp14.len, align 4
  %tmp16 = load i64, ptr %idx, align 4
  %tmp.i48 = sub i64 %tmp15, %tmp16
  %tmp18 = load ptr, ptr %insert_ptr, align 8
  %fun_call_tmp19 = call {} @"[c-flat]:mem:copy_range<T = char>"(ptr %cast13, ptr %tmp18, i64 %tmp.i48)
  %fun_call_tmp21 = call {} @"[c-flat]:mem:copy<T = char>"(ptr %tmp18, ptr nonnull %value)
  %tmp22 = load ptr, ptr %self, align 8
  %tmp22.len = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  %tmp23 = load i64, ptr %tmp22.len, align 4
  %tmp.i = add i64 %tmp23, 1
  %tmp25.len = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  store i64 %tmp.i, ptr %tmp25.len, align 4
  %tmp26 = load i8, ptr %value, align 1
  %tmp2753 = insertvalue %"[c-flat]:mem:Manually_Drop<T = char>" poison, i8 %tmp26, 0
  %fun_call_tmp28 = call {} @"[c-flat]:mem:Manually_Drop<T = char>:drop"(%"[c-flat]:mem:Manually_Drop<T = char>" %tmp2753)
  %tmp31 = load i64, ptr %type_mem_size, align 4
  %fun_call_tmp32 = call {} @"[c-flat]:u64:drop"(i64 %tmp31)
  %tmp33 = load i8, ptr %value, align 1
  %fun_call_tmp34 = call {} @"[c-flat]:char:drop"(i8 %tmp33)
  %tmp35 = load i64, ptr %idx, align 4
  %fun_call_tmp36 = call {} @"[c-flat]:u64:drop"(i64 %tmp35)
  ret {} zeroinitializer
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #0

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
