; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:SliceMutIter<T = File>" = type { ptr, ptr }
%"[c-flat]:Range<T = u64>" = type { i64, i64 }
%"[c-flat]:Vec<T = File>" = type { ptr, i64, i64 }
%"[c-flat]:pixel:Game" = type { %"[c-flat]:Vec<T = char>", %"[c-flat]:Vec_2<T = u64>", i64, %"[c-flat]:Vec_2<T = u64>", %"[c-flat]:Vec<T = Vec<T = char>>" }
%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:Vec_2<T = u64>" = type { i64, i64 }
%"[c-flat]:Vec<T = Vec<T = char>>" = type { ptr, i64, i64 }
%"[c-flat]:Particle" = type { %"[c-flat]:Vec_2<T = f64>", double, double, double, double, i32 }
%"[c-flat]:Vec_2<T = f64>" = type { double, double }
%"[c-flat]:Vec_2<T = i32>" = type { i32, i32 }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }
%"[c-flat]:SliceIter<T = Vec<T = char>>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:mem:Manually_Drop<T = File>" = type { %"[c-flat]:Particle" }
%"[c-flat]:Range<T = f64>" = type { double, double }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [14 x i8] c"My First Game\00", align 1
@global_string.33 = private unnamed_addr constant [14 x i8] c"res/image.png\00", align 1
@"[c-flat]:seed" = internal global i64 91237
@global_string.70 = private unnamed_addr constant [11 x i8] c"Mouse Down\00", align 1
@global_string.73 = private unnamed_addr constant [10 x i8] c"game over\00", align 1
@global_string.74 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @"[c-flat]:game"()
  %fun_call_tmp1 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.73, i64 9 })
  %fun_call_tmp2 = call {} @"[c-flat]:debug_heap_count"()
  ret i64 0
}

define {} @"[c-flat]:game"() {
entry:
  %tmp_iter5 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %tmp_iter453 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %tmp_iter4 = alloca %"[c-flat]:SliceMutIter<T = File>", align 8
  %i = alloca i64, align 8
  %tmp_iter3 = alloca %"[c-flat]:Range<T = u64>", align 8
  %particles = alloca %"[c-flat]:Vec<T = File>", align 8
  %texture = alloca i32, align 4
  %game = alloca %"[c-flat]:pixel:Game", align 8
  %fun_call_tmp = call %"[c-flat]:pixel:Game" @"[c-flat]:pixel:Game:new"({ ptr, i64 } { ptr @global_string, i64 13 }, %"[c-flat]:Vec_2<T = u64>" { i64 320, i64 180 }, i64 6)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 0
  %fun_call_tmp.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 0
  store ptr %fun_call_tmp.elt.elt, ptr %game, align 8
  %game.repack139 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %fun_call_tmp.elt.elt140 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 1
  store i64 %fun_call_tmp.elt.elt140, ptr %game.repack139, align 8
  %game.repack141 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp.elt.elt142 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 2
  store i64 %fun_call_tmp.elt.elt142, ptr %game.repack141, align 8
  %game.repack131 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %fun_call_tmp.elt132 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 1
  %fun_call_tmp.elt132.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt132, 0
  store i64 %fun_call_tmp.elt132.elt, ptr %game.repack131, align 8
  %game.repack131.repack143 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %fun_call_tmp.elt132.elt144 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt132, 1
  store i64 %fun_call_tmp.elt132.elt144, ptr %game.repack131.repack143, align 8
  %game.repack133 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp.elt134 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt134, ptr %game.repack133, align 8
  %game.repack135 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %fun_call_tmp.elt136 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 3
  %fun_call_tmp.elt136.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt136, 0
  store i64 %fun_call_tmp.elt136.elt, ptr %game.repack135, align 8
  %game.repack135.repack145 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %fun_call_tmp.elt136.elt146 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt136, 1
  store i64 %fun_call_tmp.elt136.elt146, ptr %game.repack135.repack145, align 8
  %game.repack137 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %fun_call_tmp.elt138 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 4
  %fun_call_tmp.elt138.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt138, 0
  store ptr %fun_call_tmp.elt138.elt, ptr %game.repack137, align 8
  %game.repack137.repack147 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %fun_call_tmp.elt138.elt148 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt138, 1
  store i64 %fun_call_tmp.elt138.elt148, ptr %game.repack137.repack147, align 8
  %game.repack137.repack149 = getelementptr inbounds nuw i8, ptr %game, i64 80
  %fun_call_tmp.elt138.elt150 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt138, 2
  store i64 %fun_call_tmp.elt138.elt150, ptr %game.repack137.repack149, align 8
  %fun_call_tmp1 = call i32 @"[c-flat]:pixel:Game:load_texture"(ptr nonnull %game, { ptr, i64 } { ptr @global_string.33, i64 13 })
  store i32 %fun_call_tmp1, ptr %texture, align 4
  %fun_call_tmp2 = call %"[c-flat]:Vec<T = File>" @"[c-flat]:Vec<T = File>:new"()
  %fun_call_tmp2.elt = extractvalue %"[c-flat]:Vec<T = File>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %particles, align 8
  %particles.repack151 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %fun_call_tmp2.elt152 = extractvalue %"[c-flat]:Vec<T = File>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt152, ptr %particles.repack151, align 8
  %particles.repack153 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %fun_call_tmp2.elt154 = extractvalue %"[c-flat]:Vec<T = File>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt154, ptr %particles.repack153, align 8
  store i64 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack155 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  store i64 50, ptr %tmp_iter3.repack155, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp5 = call i64 @"[c-flat]:Vec_2<T = u64>:next"(ptr nonnull %tmp_iter3)
  store i64 %fun_call_tmp5, ptr %i, align 4
  %game.pixel_frame_size = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp.unpack = load i64, ptr %game.pixel_frame_size, align 4
  %0 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp.unpack, 0
  %tmp.elt198 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp.unpack199 = load i64, ptr %tmp.elt198, align 4
  %tmp200 = insertvalue %"[c-flat]:Vec_2<T = u64>" %0, i64 %tmp.unpack199, 1
  %fun_call_tmp6 = call %"[c-flat]:Particle" @"[c-flat]:File:spawn"(%"[c-flat]:Vec_2<T = u64>" %tmp200)
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = File>:push"(ptr nonnull %particles, %"[c-flat]:Particle" %fun_call_tmp6)
  %tmp8 = load i64, ptr %i, align 4
  %fun_call_tmp9 = call {} @"[c-flat]:u64:drop"(i64 %tmp8)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp4 = call i1 @"[c-flat]:Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp4, label %then, label %cond12

cond12:                                           ; preds = %cond, %merge83
  %fun_call_tmp14 = call i1 @"[c-flat]:pixel:Game:should_close"(ptr nonnull %game)
  br i1 %fun_call_tmp14, label %else13, label %cond18

else13:                                           ; preds = %cond12
  %fun_call_tmp96 = call {} @"[c-flat]:pixel:Game:close"(ptr nonnull %game)
  %tmp97.unpack = load i64, ptr %tmp_iter3, align 4
  %tmp97.elt156 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %tmp97.unpack157 = load i64, ptr %tmp97.elt156, align 4
  %fun_call_tmp.i110 = call {} @"[c-flat]:u64:drop"(i64 %tmp97.unpack157)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp97.unpack)
  %tmp99.unpack = load ptr, ptr %particles, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = File>" poison, ptr %tmp99.unpack, 0
  %tmp99.elt159 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %tmp99.unpack160 = load i64, ptr %tmp99.elt159, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = File>" %1, i64 %tmp99.unpack160, 1
  %tmp99.elt161 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %tmp99.unpack162 = load i64, ptr %tmp99.elt161, align 8
  %tmp99163 = insertvalue %"[c-flat]:Vec<T = File>" %2, i64 %tmp99.unpack162, 2
  %fun_call_tmp100 = call {} @"[c-flat]:Vec<T = File>:drop.71"(%"[c-flat]:Vec<T = File>" %tmp99163)
  %tmp101 = load i32, ptr %texture, align 4
  %fun_call_tmp102 = call {} @"[c-flat]:i32:drop"(i32 %tmp101)
  %tmp103.unpack.unpack = load ptr, ptr %game, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp103.unpack.unpack, 0
  %tmp103.unpack.elt173 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %tmp103.unpack.unpack174 = load i64, ptr %tmp103.unpack.elt173, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp103.unpack.unpack174, 1
  %tmp103.unpack.elt175 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %tmp103.unpack.unpack176 = load i64, ptr %tmp103.unpack.elt175, align 8
  %tmp103.unpack177 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp103.unpack.unpack176, 2
  %tmp103.elt164 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp103.unpack165.unpack = load i64, ptr %tmp103.elt164, align 8
  %tmp103.unpack165.elt178 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp103.unpack165.unpack179 = load i64, ptr %tmp103.unpack165.elt178, align 8
  %tmp103.elt166 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %tmp103.unpack167 = load i64, ptr %tmp103.elt166, align 8
  %tmp103.elt168 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %tmp103.unpack169.unpack = load i64, ptr %tmp103.elt168, align 8
  %tmp103.unpack169.elt181 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %tmp103.unpack169.unpack182 = load i64, ptr %tmp103.unpack169.elt181, align 8
  %tmp103.elt170 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %tmp103.unpack171.unpack = load ptr, ptr %tmp103.elt170, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" poison, ptr %tmp103.unpack171.unpack, 0
  %tmp103.unpack171.elt184 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %tmp103.unpack171.unpack185 = load i64, ptr %tmp103.unpack171.elt184, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %5, i64 %tmp103.unpack171.unpack185, 1
  %tmp103.unpack171.elt186 = getelementptr inbounds nuw i8, ptr %game, i64 80
  %tmp103.unpack171.unpack187 = load i64, ptr %tmp103.unpack171.elt186, align 8
  %tmp103.unpack171188 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %6, i64 %tmp103.unpack171.unpack187, 2
  %fun_call_tmp.i = call {} @"[c-flat]:Vec<T = Vec<T = char>>:drop.29"(%"[c-flat]:Vec<T = Vec<T = char>>" %tmp103.unpack171188)
  %fun_call_tmp.i11.i = call {} @"[c-flat]:u64:drop"(i64 %tmp103.unpack169.unpack182)
  %fun_call_tmp2.i13.i = call {} @"[c-flat]:u64:drop"(i64 %tmp103.unpack169.unpack)
  %fun_call_tmp4.i = call {} @"[c-flat]:u64:drop"(i64 %tmp103.unpack167)
  %fun_call_tmp.i.i = call {} @"[c-flat]:u64:drop"(i64 %tmp103.unpack165.unpack179)
  %fun_call_tmp2.i.i = call {} @"[c-flat]:u64:drop"(i64 %tmp103.unpack165.unpack)
  %fun_call_tmp8.i = call {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %tmp103.unpack177)
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

merge27:                                          ; preds = %cond36, %cond29
  %fun_call_tmp51 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp52 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp51)
  %fun_call_tmp52.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp52, 0
  store ptr %fun_call_tmp52.elt, ptr %tmp_iter453, align 8
  %tmp_iter453.repack191 = getelementptr inbounds nuw i8, ptr %tmp_iter453, i64 8
  %fun_call_tmp52.elt192 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp52, 1
  store ptr %fun_call_tmp52.elt192, ptr %tmp_iter453.repack191, align 8
  br label %cond55

then28:                                           ; preds = %cond29
  %fun_call_tmp33 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp34 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp33)
  %fun_call_tmp34.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp34, 0
  store ptr %fun_call_tmp34.elt, ptr %tmp_iter4, align 8
  %tmp_iter4.repack189 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %fun_call_tmp34.elt190 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp34, 1
  store ptr %fun_call_tmp34.elt190, ptr %tmp_iter4.repack189, align 8
  br label %cond36

cond29:                                           ; preds = %cond18, %then17
  %fun_call_tmp31 = call i1 @"[c-flat]:pixel:Game:is_mouse_pressed"(ptr nonnull %game, i32 2)
  br i1 %fun_call_tmp31, label %then28, label %merge27

then35:                                           ; preds = %cond36
  %fun_call_tmp39 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter4)
  %tmp40.new_speed = getelementptr inbounds nuw i8, ptr %fun_call_tmp39, i64 32
  %tmp41.speed = getelementptr inbounds nuw i8, ptr %fun_call_tmp39, i64 24
  %tmp42 = load double, ptr %tmp41.speed, align 8
  %tmp.i107 = fneg double %tmp42
  store double %tmp.i107, ptr %tmp40.new_speed, align 8
  br label %cond36

cond36:                                           ; preds = %then35, %then28
  %fun_call_tmp38 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp38, label %then35, label %merge27

then54:                                           ; preds = %cond55
  %fun_call_tmp58 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter453)
  %game.pixel_frame_size61 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp62.unpack = load i64, ptr %game.pixel_frame_size61, align 4
  %7 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp62.unpack, 0
  %tmp62.elt195 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp62.unpack196 = load i64, ptr %tmp62.elt195, align 4
  %tmp62197 = insertvalue %"[c-flat]:Vec_2<T = u64>" %7, i64 %tmp62.unpack196, 1
  %fun_call_tmp63 = call {} @"[c-flat]:File:update"(ptr %fun_call_tmp58, %"[c-flat]:Vec_2<T = u64>" %tmp62197)
  br label %cond55

cond55:                                           ; preds = %then54, %merge27
  %fun_call_tmp57 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter453)
  br i1 %fun_call_tmp57, label %then54, label %else56

else56:                                           ; preds = %cond55
  %fun_call_tmp67 = call {} @"[c-flat]:pixel:Game:begin_draw"(ptr nonnull %game)
  %fun_call_tmp68 = call {} @"[c-flat]:pixel:Game:clear_background"(ptr nonnull %game)
  %fun_call_tmp70 = call { ptr, i64 } @"[c-flat]:Vec<T = File>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp71 = call %"[c-flat]:SliceMutIter<T = File>" @"[c-flat]:[File]:iter_mut"({ ptr, i64 } %fun_call_tmp70)
  %fun_call_tmp71.elt = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp71, 0
  store ptr %fun_call_tmp71.elt, ptr %tmp_iter5, align 8
  %tmp_iter5.repack193 = getelementptr inbounds nuw i8, ptr %tmp_iter5, i64 8
  %fun_call_tmp71.elt194 = extractvalue %"[c-flat]:SliceMutIter<T = File>" %fun_call_tmp71, 1
  store ptr %fun_call_tmp71.elt194, ptr %tmp_iter5.repack193, align 8
  br label %cond73

then72:                                           ; preds = %cond73
  %fun_call_tmp76 = call ptr @"[c-flat]:Vec<T = File>:next"(ptr nonnull %tmp_iter5)
  %fun_call_tmp79 = call {} @"[c-flat]:File:render"(ptr %fun_call_tmp76, ptr nonnull %game)
  br label %cond73

cond73:                                           ; preds = %then72, %else56
  %fun_call_tmp75 = call i1 @"[c-flat]:Vec<T = File>:has_next"(ptr nonnull %tmp_iter5)
  br i1 %fun_call_tmp75, label %then72, label %cond85

merge83:                                          ; preds = %cond85, %then84
  %fun_call_tmp90 = call {} @"[c-flat]:pixel:Game:end_draw"(ptr nonnull %game)
  br label %cond12

then84:                                           ; preds = %cond85
  %fun_call_tmp88 = call {} @"[c-flat]:pixel:Game:draw_text"(ptr nonnull %game, { ptr, i64 } { ptr @global_string.70, i64 10 }, %"[c-flat]:Vec_2<T = i32>" { i32 10, i32 10 }, i64 1, i32 -1)
  br label %merge83

cond85:                                           ; preds = %cond73
  %fun_call_tmp87 = call i1 @"[c-flat]:pixel:Game:is_mouse_down"(ptr nonnull %game, i32 0)
  br i1 %fun_call_tmp87, label %then84, label %merge83
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

define {} @"[c-flat]:u64:drop"(i64 %0) {
entry:
  ret {} zeroinitializer
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
  %tmp36 = load i64, ptr %new_len, align 4
  %fun_call_tmp37 = call {} @"[c-flat]:u64:drop"(i64 %tmp36)
  %tmp38 = load i64, ptr %additional, align 4
  %fun_call_tmp39 = call {} @"[c-flat]:u64:drop"(i64 %tmp38)
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

define ptr @"[c-flat]:&char:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
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

define i64 @"[c-flat]:u64:clone"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  ret i64 %tmp1
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
  %tmp36 = load i64, ptr %new_len, align 4
  %fun_call_tmp37 = call {} @"[c-flat]:u64:drop"(i64 %tmp36)
  %tmp38 = load i64, ptr %additional, align 4
  %fun_call_tmp39 = call {} @"[c-flat]:u64:drop"(i64 %tmp38)
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

define ptr @"[c-flat]:&Vec<T = char>:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
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

define {} @"[c-flat]:Vec<T = Vec<T = char>>:drop.29"(%"[c-flat]:Vec<T = Vec<T = char>>" %0) {
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
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %tmp627)
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

define {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %0) {
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
  %result = alloca %"[c-flat]:pixel:Game", align 8
  %pixel_size_factor = alloca i64, align 8
  %pixel_frame_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %title = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %title, align 8
  %title.repack33 = getelementptr inbounds nuw i8, ptr %title, i64 8
  %.elt34 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt34, ptr %title.repack33, align 8
  %.elt35 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 0
  store i64 %.elt35, ptr %pixel_frame_size, align 4
  %pixel_frame_size.repack36 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %.elt37 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 1
  store i64 %.elt37, ptr %pixel_frame_size.repack36, align 4
  store i64 %2, ptr %pixel_size_factor, align 4
  %tmp.unpack = load ptr, ptr %title, align 8
  %3 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt38 = getelementptr inbounds nuw i8, ptr %title, i64 8
  %tmp.unpack39 = load i64, ptr %tmp.elt38, align 8
  %tmp40 = insertvalue { ptr, i64 } %3, i64 %tmp.unpack39, 1
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %tmp40)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:null_terminate"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %tmp2.unpack = load i64, ptr %pixel_frame_size, align 4
  %4 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp2.unpack, 0
  %tmp2.elt41 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp2.unpack42 = load i64, ptr %tmp2.elt41, align 4
  %tmp243 = insertvalue %"[c-flat]:Vec_2<T = u64>" %4, i64 %tmp2.unpack42, 1
  %tmp3 = load i64, ptr %pixel_size_factor, align 4
  %fun_call_tmp4 = call %"[c-flat]:Vec_2<T = u64>" @"[c-flat]:Vec_2<T = u64>:mul"(%"[c-flat]:Vec_2<T = u64>" %tmp243, i64 %tmp3)
  %fun_call_tmp5 = call %"[c-flat]:Vec<T = Vec<T = char>>" @"[c-flat]:Vec<T = Vec<T = char>>:new"()
  %tmp6.unpack = load i64, ptr %pixel_frame_size, align 4
  %tmp6.elt44 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp6.unpack45 = load i64, ptr %tmp6.elt44, align 4
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %result, align 8
  %result.repack55 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp1.elt56 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt56, ptr %result.repack55, align 8
  %result.repack57 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp1.elt58 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt58, ptr %result.repack57, align 8
  %result.repack47 = getelementptr inbounds nuw i8, ptr %result, i64 24
  store i64 %tmp6.unpack, ptr %result.repack47, align 8
  %result.repack47.repack59 = getelementptr inbounds nuw i8, ptr %result, i64 32
  store i64 %tmp6.unpack45, ptr %result.repack47.repack59, align 8
  %result.repack49 = getelementptr inbounds nuw i8, ptr %result, i64 40
  store i64 %tmp3, ptr %result.repack49, align 8
  %result.repack51 = getelementptr inbounds nuw i8, ptr %result, i64 48
  %fun_call_tmp4.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp4, 0
  store i64 %fun_call_tmp4.elt, ptr %result.repack51, align 8
  %result.repack51.repack61 = getelementptr inbounds nuw i8, ptr %result, i64 56
  %fun_call_tmp4.elt62 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp4, 1
  store i64 %fun_call_tmp4.elt62, ptr %result.repack51.repack61, align 8
  %result.repack53 = getelementptr inbounds nuw i8, ptr %result, i64 64
  %fun_call_tmp5.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp5, 0
  store ptr %fun_call_tmp5.elt, ptr %result.repack53, align 8
  %result.repack53.repack63 = getelementptr inbounds nuw i8, ptr %result, i64 72
  %fun_call_tmp5.elt64 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp5, 1
  store i64 %fun_call_tmp5.elt64, ptr %result.repack53.repack63, align 8
  %result.repack53.repack65 = getelementptr inbounds nuw i8, ptr %result, i64 80
  %fun_call_tmp5.elt66 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp5, 2
  store i64 %fun_call_tmp5.elt66, ptr %result.repack53.repack65, align 8
  %fun_call_tmp11 = call {} @crl_set_config_flags(i32 64)
  %result.frame_size = getelementptr inbounds nuw i8, ptr %result, i64 48
  %tmp12 = load i64, ptr %result.frame_size, align 4
  %cast = trunc i64 %tmp12 to i32
  %result.frame_size13.y = getelementptr inbounds nuw i8, ptr %result, i64 56
  %tmp14 = load i64, ptr %result.frame_size13.y, align 4
  %cast15 = trunc i64 %tmp14 to i32
  %fun_call_tmp16 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %result)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp16, 0
  %fun_call_tmp18 = call {} @crl_init_window(i32 %cast, i32 %cast15, ptr %ref.i)
  %tmp19 = load i64, ptr %pixel_frame_size, align 4
  %cast20 = trunc i64 %tmp19 to i32
  %pixel_frame_size.y = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp21 = load i64, ptr %pixel_frame_size.y, align 4
  %cast22 = trunc i64 %tmp21 to i32
  %fun_call_tmp23 = call {} @crl_init_canvas(i32 %cast20, i32 %cast22)
  %tmp24.unpack.unpack = load ptr, ptr %result, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp24.unpack.unpack, 0
  %tmp24.unpack.elt76 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp24.unpack.unpack77 = load i64, ptr %tmp24.unpack.elt76, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" %5, i64 %tmp24.unpack.unpack77, 1
  %tmp24.unpack.elt78 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp24.unpack.unpack79 = load i64, ptr %tmp24.unpack.elt78, align 8
  %tmp24.unpack80 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp24.unpack.unpack79, 2
  %7 = insertvalue %"[c-flat]:pixel:Game" poison, %"[c-flat]:Vec<T = char>" %tmp24.unpack80, 0
  %tmp24.elt67 = getelementptr inbounds nuw i8, ptr %result, i64 24
  %tmp24.unpack68.unpack = load i64, ptr %tmp24.elt67, align 8
  %8 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp24.unpack68.unpack, 0
  %tmp24.unpack68.elt81 = getelementptr inbounds nuw i8, ptr %result, i64 32
  %tmp24.unpack68.unpack82 = load i64, ptr %tmp24.unpack68.elt81, align 8
  %tmp24.unpack6883 = insertvalue %"[c-flat]:Vec_2<T = u64>" %8, i64 %tmp24.unpack68.unpack82, 1
  %9 = insertvalue %"[c-flat]:pixel:Game" %7, %"[c-flat]:Vec_2<T = u64>" %tmp24.unpack6883, 1
  %tmp24.elt69 = getelementptr inbounds nuw i8, ptr %result, i64 40
  %tmp24.unpack70 = load i64, ptr %tmp24.elt69, align 8
  %10 = insertvalue %"[c-flat]:pixel:Game" %9, i64 %tmp24.unpack70, 2
  %tmp24.elt71 = getelementptr inbounds nuw i8, ptr %result, i64 48
  %tmp24.unpack72.unpack = load i64, ptr %tmp24.elt71, align 8
  %11 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp24.unpack72.unpack, 0
  %tmp24.unpack72.elt84 = getelementptr inbounds nuw i8, ptr %result, i64 56
  %tmp24.unpack72.unpack85 = load i64, ptr %tmp24.unpack72.elt84, align 8
  %tmp24.unpack7286 = insertvalue %"[c-flat]:Vec_2<T = u64>" %11, i64 %tmp24.unpack72.unpack85, 1
  %12 = insertvalue %"[c-flat]:pixel:Game" %10, %"[c-flat]:Vec_2<T = u64>" %tmp24.unpack7286, 3
  %tmp24.elt73 = getelementptr inbounds nuw i8, ptr %result, i64 64
  %tmp24.unpack74.unpack = load ptr, ptr %tmp24.elt73, align 8
  %13 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" poison, ptr %tmp24.unpack74.unpack, 0
  %tmp24.unpack74.elt87 = getelementptr inbounds nuw i8, ptr %result, i64 72
  %tmp24.unpack74.unpack88 = load i64, ptr %tmp24.unpack74.elt87, align 8
  %14 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %13, i64 %tmp24.unpack74.unpack88, 1
  %tmp24.unpack74.elt89 = getelementptr inbounds nuw i8, ptr %result, i64 80
  %tmp24.unpack74.unpack90 = load i64, ptr %tmp24.unpack74.elt89, align 8
  %tmp24.unpack7491 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %14, i64 %tmp24.unpack74.unpack90, 2
  %tmp2475 = insertvalue %"[c-flat]:pixel:Game" %12, %"[c-flat]:Vec<T = Vec<T = char>>" %tmp24.unpack7491, 4
  %tmp25 = load i64, ptr %pixel_size_factor, align 4
  %fun_call_tmp26 = call {} @"[c-flat]:u64:drop"(i64 %tmp25)
  %tmp27.unpack = load i64, ptr %pixel_frame_size, align 4
  %tmp27.elt92 = getelementptr inbounds nuw i8, ptr %pixel_frame_size, i64 8
  %tmp27.unpack93 = load i64, ptr %tmp27.elt92, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp27.unpack93)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp27.unpack)
  ret %"[c-flat]:pixel:Game" %tmp2475
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
  br label %cond

then:                                             ; preds = %cond
  %tmp3 = load ptr, ptr %self, align 8
  %tmp4.unpack = load ptr, ptr %slice, align 8
  %tmp5 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp4.unpack, i64 %tmp5
  %tmp7 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp8 = call {} @"[c-flat]:Vec<T = char>:push"(ptr %tmp3, i8 %tmp7)
  %tmp.i = add i64 %tmp5, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1.elt26 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp1.unpack27 = load i64, ptr %tmp1.elt26, align 8
  %tmp.i19 = icmp ult i64 %storemerge, %tmp1.unpack27
  br i1 %tmp.i19, label %then, label %else

else:                                             ; preds = %cond
  %tmp11 = load i64, ptr %idx, align 4
  %fun_call_tmp12 = call {} @"[c-flat]:u64:drop"(i64 %tmp11)
  ret {} zeroinitializer
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:null_terminate"(%"[c-flat]:Vec<T = char>" %0) {
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

declare {} @crl_init_canvas(i32, i32)

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
  %fun_call_tmp5 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:null_terminate"(%"[c-flat]:Vec<T = char>" %fun_call_tmp4)
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

define double @"[c-flat]:f64:clone"(ptr %0) {
entry:
  %tmp1 = load double, ptr %0, align 8
  ret double %tmp1
}

define {} @"[c-flat]:f64:drop"(double %0) {
entry:
  ret {} zeroinitializer
}

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

define i1 @"[c-flat]:Vec_2<T = u64>:has_next"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load i64, ptr %tmp2.end, align 4
  %tmp.i = icmp ult i64 %tmp1, %tmp3
  ret i1 %tmp.i
}

define i64 @"[c-flat]:Vec_2<T = u64>:next"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  %tmp.i = add i64 %tmp1, 1
  store i64 %tmp.i, ptr %0, align 4
  %fun_call_tmp7 = call {} @"[c-flat]:u64:drop"(i64 %tmp1)
  ret i64 %tmp1
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
  %tmp36 = load i64, ptr %new_len, align 4
  %fun_call_tmp37 = call {} @"[c-flat]:u64:drop"(i64 %tmp36)
  %tmp38 = load i64, ptr %additional, align 4
  %fun_call_tmp39 = call {} @"[c-flat]:u64:drop"(i64 %tmp38)
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
  %tmp2 = alloca %"[c-flat]:Range<T = u64>", align 8
  %tmp1 = alloca %"[c-flat]:Range<T = u64>", align 8
  %tmp0 = alloca %"[c-flat]:Range<T = u64>", align 8
  store i64 0, ptr %tmp0, align 4
  %tmp0.repack48 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  store i64 256, ptr %tmp0.repack48, align 4
  %fun_call_tmp1 = call i64 @"[c-flat]:Vec_2<T = u64>:rand"(ptr nonnull %tmp0)
  store i64 0, ptr %tmp1, align 4
  %tmp1.repack49 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  store i64 256, ptr %tmp1.repack49, align 4
  %fun_call_tmp3 = call i64 @"[c-flat]:Vec_2<T = u64>:rand"(ptr nonnull %tmp1)
  %tmp.i35 = shl i64 %fun_call_tmp3, 8
  %tmp.i28 = or i64 %fun_call_tmp1, %tmp.i35
  store i64 0, ptr %tmp2, align 4
  %tmp2.repack50 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  store i64 256, ptr %tmp2.repack50, align 4
  %fun_call_tmp7 = call i64 @"[c-flat]:Vec_2<T = u64>:rand"(ptr nonnull %tmp2)
  %tmp.i32 = shl i64 %fun_call_tmp7, 16
  %tmp.i25 = or i64 %tmp.i28, %tmp.i32
  %0 = trunc i64 %tmp.i25 to i32
  %cast = or i32 %0, -16777216
  %tmp.unpack = load i64, ptr %tmp2, align 4
  %tmp.elt51 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp.unpack52 = load i64, ptr %tmp.elt51, align 4
  %fun_call_tmp.i45 = call {} @"[c-flat]:u64:drop"(i64 %tmp.unpack52)
  %fun_call_tmp2.i47 = call {} @"[c-flat]:u64:drop"(i64 %tmp.unpack)
  %tmp13.unpack = load i64, ptr %tmp1, align 4
  %tmp13.elt54 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp13.unpack55 = load i64, ptr %tmp13.elt54, align 4
  %fun_call_tmp.i40 = call {} @"[c-flat]:u64:drop"(i64 %tmp13.unpack55)
  %fun_call_tmp2.i42 = call {} @"[c-flat]:u64:drop"(i64 %tmp13.unpack)
  %tmp15.unpack = load i64, ptr %tmp0, align 4
  %tmp15.elt57 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  %tmp15.unpack58 = load i64, ptr %tmp15.elt57, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp15.unpack58)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp15.unpack)
  ret i32 %cast
}

define i64 @"[c-flat]:Vec_2<T = u64>:rand"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call i64 @"[c-flat]:rng:next"()
  %tmp.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.end, align 4
  %tmp3 = load i64, ptr %0, align 4
  %tmp.i13 = sub i64 %tmp1, %tmp3
  %tmp.i16 = urem i64 %fun_call_tmp, %tmp.i13
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load i64, ptr %tmp6, align 4
  %tmp.i = add i64 %tmp.i16, %tmp7
  ret i64 %tmp.i
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

define {} @"[c-flat]:i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}

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

define i32 @"[c-flat]:i32:clone"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  ret i32 %tmp1
}

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

define ptr @"[c-flat]:&mut File:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
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

define {} @"[c-flat]:File:update"(ptr %0, %"[c-flat]:Vec_2<T = u64>" %1) {
entry:
  %tmp258 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp237 = alloca %"[c-flat]:Range<T = f64>", align 8
  %screen_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack107 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt108 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 1
  store i64 %.elt108, ptr %screen_size.repack107, align 4
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
  %tmp.i79 = fadd double %tmp6, %tmp8
  store double %tmp.i79, ptr %tmp5, align 8
  %tmp11.pos.y = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  %tmp12 = load double, ptr %tmp11.pos.y, align 8
  %tmp13 = load ptr, ptr %self, align 8
  %tmp13.offset = getelementptr inbounds nuw i8, ptr %tmp13, i64 40
  %tmp14 = load double, ptr %tmp13.offset, align 8
  %fun_call_tmp15 = call double @"[c-flat]:f64:sin"(double %tmp14)
  %tmp.i76 = fadd double %tmp12, %fun_call_tmp15
  %tmp17.pos.y = getelementptr inbounds nuw i8, ptr %tmp13, i64 8
  store double %tmp.i76, ptr %tmp17.pos.y, align 8
  %tmp18 = load ptr, ptr %self, align 8
  %tmp18.offset = getelementptr inbounds nuw i8, ptr %tmp18, i64 40
  %tmp19 = load double, ptr %tmp18.offset, align 8
  %tmp20.speed = getelementptr inbounds nuw i8, ptr %tmp18, i64 24
  %tmp21 = load double, ptr %tmp20.speed, align 8
  %tmp.i82 = fdiv double %tmp21, 4.600000e+01
  %fun_call_tmp23 = call double @"[c-flat]:f64:min"(double 5.000000e-02, double %tmp.i82)
  %tmp.i73 = fadd double %tmp19, %fun_call_tmp23
  %tmp25 = load ptr, ptr %self, align 8
  %tmp25.offset = getelementptr inbounds nuw i8, ptr %tmp25, i64 40
  store double %tmp.i73, ptr %tmp25.offset, align 8
  %tmp26 = load ptr, ptr %self, align 8
  %tmp27 = load double, ptr %tmp26, align 8
  %tmp28 = load i64, ptr %screen_size, align 4
  %cast = uitofp i64 %tmp28 to double
  %tmp.i70 = fadd double %cast, 4.000000e+00
  %tmp.i90 = fcmp ogt double %tmp27, %tmp.i70
  br i1 %tmp.i90, label %then, label %cond42

merge:                                            ; preds = %cond42, %then41, %then
  %tmp62.unpack = load i64, ptr %screen_size, align 4
  %tmp62.elt119 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp62.unpack120 = load i64, ptr %tmp62.elt119, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp62.unpack120)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp62.unpack)
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp31 = load ptr, ptr %self, align 8
  store double -4.000000e+00, ptr %tmp31, align 8
  %tmp33.pos.y = getelementptr inbounds nuw i8, ptr %tmp31, i64 8
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp34 = load i64, ptr %screen_size.y, align 4
  %cast35 = uitofp i64 %tmp34 to double
  store double 0.000000e+00, ptr %tmp237, align 8
  %tmp237.repack114 = getelementptr inbounds nuw i8, ptr %tmp237, i64 8
  store double %cast35, ptr %tmp237.repack114, align 8
  %fun_call_tmp38 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp237)
  store double %fun_call_tmp38, ptr %tmp33.pos.y, align 8
  %tmp39.unpack = load double, ptr %tmp237, align 8
  %tmp39.elt116 = getelementptr inbounds nuw i8, ptr %tmp237, i64 8
  %tmp39.unpack117 = load double, ptr %tmp39.elt116, align 8
  %fun_call_tmp.i103 = call {} @"[c-flat]:f64:drop"(double %tmp39.unpack117)
  %fun_call_tmp2.i105 = call {} @"[c-flat]:f64:drop"(double %tmp39.unpack)
  br label %merge

then41:                                           ; preds = %cond42
  %tmp48 = load ptr, ptr %self, align 8
  %tmp50 = load i64, ptr %screen_size, align 4
  %cast51 = uitofp i64 %tmp50 to double
  %tmp.i67 = fadd double %cast51, 4.000000e+00
  store double %tmp.i67, ptr %tmp48, align 8
  %tmp53.pos.y = getelementptr inbounds nuw i8, ptr %tmp48, i64 8
  %screen_size.y54 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp55 = load i64, ptr %screen_size.y54, align 4
  %cast56 = uitofp i64 %tmp55 to double
  store double 0.000000e+00, ptr %tmp258, align 8
  %tmp258.repack109 = getelementptr inbounds nuw i8, ptr %tmp258, i64 8
  store double %cast56, ptr %tmp258.repack109, align 8
  %fun_call_tmp59 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp258)
  store double %fun_call_tmp59, ptr %tmp53.pos.y, align 8
  %tmp60.unpack = load double, ptr %tmp258, align 8
  %tmp60.elt111 = getelementptr inbounds nuw i8, ptr %tmp258, i64 8
  %tmp60.unpack112 = load double, ptr %tmp60.elt111, align 8
  %fun_call_tmp.i98 = call {} @"[c-flat]:f64:drop"(double %tmp60.unpack112)
  %fun_call_tmp2.i100 = call {} @"[c-flat]:f64:drop"(double %tmp60.unpack)
  br label %merge

cond42:                                           ; preds = %entry
  %tmp44 = load ptr, ptr %self, align 8
  %tmp45 = load double, ptr %tmp44, align 8
  %tmp.i87 = fcmp olt double %tmp45, -4.000000e+00
  br i1 %tmp.i87, label %then41, label %merge
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
  %tmp2 = load double, ptr %self, align 8
  %tmp4 = load double, ptr %other, align 8
  %result = select i1 %tmp.i, double %tmp2, double %tmp4
  %tmp5 = load double, ptr %other, align 8
  %fun_call_tmp6 = call {} @"[c-flat]:f64:drop"(double %tmp5)
  %tmp7 = load double, ptr %self, align 8
  %fun_call_tmp8 = call {} @"[c-flat]:f64:drop"(double %tmp7)
  ret double %result
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

define i1 @"[c-flat]:pixel:Game:is_mouse_down"(ptr %0, i32 %1) {
entry:
  %fun_call_tmp = call i1 @crl_is_mouse_button_down(i32 %1)
  %fun_call_tmp2 = call {} @"[c-flat]:i32:drop"(i32 %1)
  ret i1 %fun_call_tmp
}

declare i1 @crl_is_mouse_button_down(i32)

define {} @"[c-flat]:pixel:Game:draw_text"(ptr %0, { ptr, i64 } %1, %"[c-flat]:Vec_2<T = i32>" %2, i64 %3, i32 %4) {
entry:
  %s = alloca %"[c-flat]:Vec<T = char>", align 8
  %color = alloca i32, align 4
  %font_size = alloca i64, align 8
  %pos = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %text = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %text, align 8
  %text.repack24 = getelementptr inbounds nuw i8, ptr %text, i64 8
  %.elt25 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt25, ptr %text.repack24, align 8
  %.elt26 = extractvalue %"[c-flat]:Vec_2<T = i32>" %2, 0
  store i32 %.elt26, ptr %pos, align 4
  %pos.repack27 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %.elt28 = extractvalue %"[c-flat]:Vec_2<T = i32>" %2, 1
  store i32 %.elt28, ptr %pos.repack27, align 4
  store i64 %3, ptr %font_size, align 4
  store i32 %4, ptr %color, align 4
  %tmp.unpack = load ptr, ptr %text, align 8
  %5 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt29 = getelementptr inbounds nuw i8, ptr %text, i64 8
  %tmp.unpack30 = load i64, ptr %tmp.elt29, align 8
  %tmp31 = insertvalue { ptr, i64 } %5, i64 %tmp.unpack30, 1
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %tmp31)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:null_terminate"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %s, align 8
  %s.repack32 = getelementptr inbounds nuw i8, ptr %s, i64 8
  %fun_call_tmp1.elt33 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt33, ptr %s.repack32, align 8
  %s.repack34 = getelementptr inbounds nuw i8, ptr %s, i64 16
  %fun_call_tmp1.elt35 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt35, ptr %s.repack34, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %s)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp4 = load i64, ptr %font_size, align 4
  %cast = trunc i64 %tmp4 to i32
  %tmp5 = load i32, ptr %pos, align 4
  %tmp6 = load i32, ptr %pos.y, align 4
  %tmp7 = load i32, ptr %color, align 4
  %fun_call_tmp8 = call {} @crl_draw_text(ptr %ref.i, i32 %tmp5, i32 %tmp6, i32 %cast, i32 %tmp7)
  %tmp9.unpack = load ptr, ptr %s, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp9.unpack, 0
  %tmp9.elt36 = getelementptr inbounds nuw i8, ptr %s, i64 8
  %tmp9.unpack37 = load i64, ptr %tmp9.elt36, align 8
  %7 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp9.unpack37, 1
  %tmp9.elt38 = getelementptr inbounds nuw i8, ptr %s, i64 16
  %tmp9.unpack39 = load i64, ptr %tmp9.elt38, align 8
  %tmp940 = insertvalue %"[c-flat]:Vec<T = char>" %7, i64 %tmp9.unpack39, 2
  %fun_call_tmp10 = call {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %tmp940)
  %tmp11 = load i32, ptr %color, align 4
  %fun_call_tmp12 = call {} @"[c-flat]:u32:drop"(i32 %tmp11)
  %tmp13 = load i64, ptr %font_size, align 4
  %fun_call_tmp14 = call {} @"[c-flat]:u64:drop"(i64 %tmp13)
  %tmp15.unpack = load i32, ptr %pos, align 4
  %tmp15.elt41 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp15.unpack42 = load i32, ptr %tmp15.elt41, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp15.unpack42)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp15.unpack)
  ret {} zeroinitializer
}

declare {} @crl_draw_text(ptr, i32, i32, i32, i32)

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

define {} @"[c-flat]:Vec<T = File>:drop.71"(%"[c-flat]:Vec<T = File>" %0) {
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
  %string = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:null_terminate"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %string, align 8
  %string.repack16 = getelementptr inbounds nuw i8, ptr %string, i64 8
  %fun_call_tmp1.elt17 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt17, ptr %string.repack16, align 8
  %string.repack18 = getelementptr inbounds nuw i8, ptr %string, i64 16
  %fun_call_tmp1.elt19 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt19, ptr %string.repack18, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %string)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call {} @c_puts(ptr %ref.i)
  %fun_call_tmp5 = call {} @c_flush()
  %tmp6.unpack = load ptr, ptr %string, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp6.unpack, 0
  %tmp6.elt20 = getelementptr inbounds nuw i8, ptr %string, i64 8
  %tmp6.unpack21 = load i64, ptr %tmp6.elt20, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp6.unpack21, 1
  %tmp6.elt22 = getelementptr inbounds nuw i8, ptr %string, i64 16
  %tmp6.unpack23 = load i64, ptr %tmp6.elt22, align 8
  %tmp624 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp6.unpack23, 2
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %tmp624)
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
  %fun_call_tmp5 = call {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %tmp416)
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
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.74, i64 1 })
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
