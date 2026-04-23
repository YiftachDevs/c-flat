; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:SliceMutIter<T = Particle>" = type { ptr, ptr }
%"[c-flat]:Range<T = u64>" = type { i64, i64 }
%"[c-flat]:Vec<T = Particle>" = type { ptr, i64, i64 }
%"[c-flat]:pixel:Game" = type { %"[c-flat]:Vec<T = char>", %"[c-flat]:Vec_2<T = u64>", i64, %"[c-flat]:Vec_2<T = u64>", %"[c-flat]:Vec<T = Vec<T = char>>" }
%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:Vec_2<T = u64>" = type { i64, i64 }
%"[c-flat]:Vec<T = Vec<T = char>>" = type { ptr, i64, i64 }
%"[c-flat]:Particle" = type { %"[c-flat]:Vec_2<T = f64>", double, double, double, i32 }
%"[c-flat]:Vec_2<T = f64>" = type { double, double }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }
%"[c-flat]:SliceIter<T = Vec<T = char>>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = Vec<T = char>>" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:mem:Manually_Drop<T = Particle>" = type { %"[c-flat]:Particle" }
%"[c-flat]:Range<T = f64>" = type { double, double }
%"[c-flat]:Vec_2<T = i32>" = type { i32, i32 }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [14 x i8] c"My First Game\00", align 1
@"[c-flat]:seed" = internal global i64 91237
@global_string.71 = private unnamed_addr constant [10 x i8] c"game over\00", align 1
@global_string.72 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @"[c-flat]:game"()
  %fun_call_tmp1 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.71, i64 9 })
  %fun_call_tmp2 = call {} @"[c-flat]:debug_heap_count"()
  ret i64 0
}

define {} @"[c-flat]:game"() {
entry:
  %tmp_iter4 = alloca %"[c-flat]:SliceMutIter<T = Particle>", align 8
  %tmp_iter3 = alloca %"[c-flat]:SliceMutIter<T = Particle>", align 8
  %i = alloca i64, align 8
  %tmp_iter2 = alloca %"[c-flat]:Range<T = u64>", align 8
  %particles = alloca %"[c-flat]:Vec<T = Particle>", align 8
  %game = alloca %"[c-flat]:pixel:Game", align 8
  %fun_call_tmp = call %"[c-flat]:pixel:Game" @"[c-flat]:pixel:Game:new"({ ptr, i64 } { ptr @global_string, i64 13 }, %"[c-flat]:Vec_2<T = u64>" { i64 320, i64 180 }, i64 6)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 0
  %fun_call_tmp.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 0
  store ptr %fun_call_tmp.elt.elt, ptr %game, align 8
  %game.repack85 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %fun_call_tmp.elt.elt86 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 1
  store i64 %fun_call_tmp.elt.elt86, ptr %game.repack85, align 8
  %game.repack87 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp.elt.elt88 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp.elt, 2
  store i64 %fun_call_tmp.elt.elt88, ptr %game.repack87, align 8
  %game.repack77 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %fun_call_tmp.elt78 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 1
  %fun_call_tmp.elt78.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt78, 0
  store i64 %fun_call_tmp.elt78.elt, ptr %game.repack77, align 8
  %game.repack77.repack89 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %fun_call_tmp.elt78.elt90 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt78, 1
  store i64 %fun_call_tmp.elt78.elt90, ptr %game.repack77.repack89, align 8
  %game.repack79 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp.elt80 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt80, ptr %game.repack79, align 8
  %game.repack81 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %fun_call_tmp.elt82 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 3
  %fun_call_tmp.elt82.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt82, 0
  store i64 %fun_call_tmp.elt82.elt, ptr %game.repack81, align 8
  %game.repack81.repack91 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %fun_call_tmp.elt82.elt92 = extractvalue %"[c-flat]:Vec_2<T = u64>" %fun_call_tmp.elt82, 1
  store i64 %fun_call_tmp.elt82.elt92, ptr %game.repack81.repack91, align 8
  %game.repack83 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %fun_call_tmp.elt84 = extractvalue %"[c-flat]:pixel:Game" %fun_call_tmp, 4
  %fun_call_tmp.elt84.elt = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt84, 0
  store ptr %fun_call_tmp.elt84.elt, ptr %game.repack83, align 8
  %game.repack83.repack93 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %fun_call_tmp.elt84.elt94 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt84, 1
  store i64 %fun_call_tmp.elt84.elt94, ptr %game.repack83.repack93, align 8
  %game.repack83.repack95 = getelementptr inbounds nuw i8, ptr %game, i64 80
  %fun_call_tmp.elt84.elt96 = extractvalue %"[c-flat]:Vec<T = Vec<T = char>>" %fun_call_tmp.elt84, 2
  store i64 %fun_call_tmp.elt84.elt96, ptr %game.repack83.repack95, align 8
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = Particle>" @"[c-flat]:Vec<T = Particle>:new"()
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = Particle>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %particles, align 8
  %particles.repack97 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %fun_call_tmp1.elt98 = extractvalue %"[c-flat]:Vec<T = Particle>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt98, ptr %particles.repack97, align 8
  %particles.repack99 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %fun_call_tmp1.elt100 = extractvalue %"[c-flat]:Vec<T = Particle>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt100, ptr %particles.repack99, align 8
  store i64 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack101 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  store i64 20, ptr %tmp_iter2.repack101, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp4 = call i64 @"[c-flat]:Vec_2<T = u64>:next"(ptr nonnull %tmp_iter2)
  store i64 %fun_call_tmp4, ptr %i, align 4
  %game.pixel_frame_size = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp.unpack = load i64, ptr %game.pixel_frame_size, align 4
  %0 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp.unpack, 0
  %tmp.elt142 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp.unpack143 = load i64, ptr %tmp.elt142, align 4
  %tmp144 = insertvalue %"[c-flat]:Vec_2<T = u64>" %0, i64 %tmp.unpack143, 1
  %fun_call_tmp5 = call %"[c-flat]:Particle" @"[c-flat]:Particle:spawn"(%"[c-flat]:Vec_2<T = u64>" %tmp144)
  %fun_call_tmp6 = call {} @"[c-flat]:Vec<T = Particle>:push"(ptr nonnull %particles, %"[c-flat]:Particle" %fun_call_tmp5)
  %tmp7 = load i64, ptr %i, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:u64:drop"(i64 %tmp7)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp3 = call i1 @"[c-flat]:Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp3, label %then, label %cond11

then10:                                           ; preds = %cond11
  %fun_call_tmp16 = call { ptr, i64 } @"[c-flat]:Vec<T = Particle>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp17 = call %"[c-flat]:SliceMutIter<T = Particle>" @"[c-flat]:[Particle]:iter_mut"({ ptr, i64 } %fun_call_tmp16)
  %fun_call_tmp17.elt = extractvalue %"[c-flat]:SliceMutIter<T = Particle>" %fun_call_tmp17, 0
  store ptr %fun_call_tmp17.elt, ptr %tmp_iter3, align 8
  %tmp_iter3.repack135 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %fun_call_tmp17.elt136 = extractvalue %"[c-flat]:SliceMutIter<T = Particle>" %fun_call_tmp17, 1
  store ptr %fun_call_tmp17.elt136, ptr %tmp_iter3.repack135, align 8
  br label %cond19

cond11:                                           ; preds = %cond, %else37
  %fun_call_tmp13 = call i1 @"[c-flat]:pixel:Game:should_close"(ptr nonnull %game)
  br i1 %fun_call_tmp13, label %else12, label %then10

else12:                                           ; preds = %cond11
  %fun_call_tmp52 = call {} @"[c-flat]:pixel:Game:close"(ptr nonnull %game)
  %tmp53.unpack = load i64, ptr %tmp_iter2, align 4
  %tmp53.elt102 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %tmp53.unpack103 = load i64, ptr %tmp53.elt102, align 4
  %fun_call_tmp.i62 = call {} @"[c-flat]:u64:drop"(i64 %tmp53.unpack103)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp53.unpack)
  %tmp55.unpack = load ptr, ptr %particles, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = Particle>" poison, ptr %tmp55.unpack, 0
  %tmp55.elt105 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %tmp55.unpack106 = load i64, ptr %tmp55.elt105, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = Particle>" %1, i64 %tmp55.unpack106, 1
  %tmp55.elt107 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %tmp55.unpack108 = load i64, ptr %tmp55.elt107, align 8
  %tmp55109 = insertvalue %"[c-flat]:Vec<T = Particle>" %2, i64 %tmp55.unpack108, 2
  %fun_call_tmp56 = call {} @"[c-flat]:Vec<T = Particle>:drop.69"(%"[c-flat]:Vec<T = Particle>" %tmp55109)
  %tmp57.unpack.unpack = load ptr, ptr %game, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp57.unpack.unpack, 0
  %tmp57.unpack.elt119 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %tmp57.unpack.unpack120 = load i64, ptr %tmp57.unpack.elt119, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp57.unpack.unpack120, 1
  %tmp57.unpack.elt121 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %tmp57.unpack.unpack122 = load i64, ptr %tmp57.unpack.elt121, align 8
  %tmp57.unpack123 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp57.unpack.unpack122, 2
  %tmp57.elt110 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp57.unpack111.unpack = load i64, ptr %tmp57.elt110, align 8
  %tmp57.unpack111.elt124 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp57.unpack111.unpack125 = load i64, ptr %tmp57.unpack111.elt124, align 8
  %tmp57.elt112 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %tmp57.unpack113 = load i64, ptr %tmp57.elt112, align 8
  %tmp57.elt114 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %tmp57.unpack115.unpack = load i64, ptr %tmp57.elt114, align 8
  %tmp57.unpack115.elt127 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %tmp57.unpack115.unpack128 = load i64, ptr %tmp57.unpack115.elt127, align 8
  %tmp57.elt116 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %tmp57.unpack117.unpack = load ptr, ptr %tmp57.elt116, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" poison, ptr %tmp57.unpack117.unpack, 0
  %tmp57.unpack117.elt130 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %tmp57.unpack117.unpack131 = load i64, ptr %tmp57.unpack117.elt130, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %5, i64 %tmp57.unpack117.unpack131, 1
  %tmp57.unpack117.elt132 = getelementptr inbounds nuw i8, ptr %game, i64 80
  %tmp57.unpack117.unpack133 = load i64, ptr %tmp57.unpack117.elt132, align 8
  %tmp57.unpack117134 = insertvalue %"[c-flat]:Vec<T = Vec<T = char>>" %6, i64 %tmp57.unpack117.unpack133, 2
  %fun_call_tmp.i = call {} @"[c-flat]:Vec<T = Vec<T = char>>:drop.29"(%"[c-flat]:Vec<T = Vec<T = char>>" %tmp57.unpack117134)
  %fun_call_tmp.i11.i = call {} @"[c-flat]:u64:drop"(i64 %tmp57.unpack115.unpack128)
  %fun_call_tmp2.i13.i = call {} @"[c-flat]:u64:drop"(i64 %tmp57.unpack115.unpack)
  %fun_call_tmp4.i = call {} @"[c-flat]:u64:drop"(i64 %tmp57.unpack113)
  %fun_call_tmp.i.i = call {} @"[c-flat]:u64:drop"(i64 %tmp57.unpack111.unpack125)
  %fun_call_tmp2.i.i = call {} @"[c-flat]:u64:drop"(i64 %tmp57.unpack111.unpack)
  %fun_call_tmp8.i = call {} @"[c-flat]:Vec<T = char>:drop.30"(%"[c-flat]:Vec<T = char>" %tmp57.unpack123)
  ret {} zeroinitializer

then18:                                           ; preds = %cond19
  %fun_call_tmp22 = call ptr @"[c-flat]:Vec<T = Particle>:next"(ptr nonnull %tmp_iter3)
  %game.pixel_frame_size24 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp25.unpack = load i64, ptr %game.pixel_frame_size24, align 4
  %7 = insertvalue %"[c-flat]:Vec_2<T = u64>" poison, i64 %tmp25.unpack, 0
  %tmp25.elt139 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp25.unpack140 = load i64, ptr %tmp25.elt139, align 4
  %tmp25141 = insertvalue %"[c-flat]:Vec_2<T = u64>" %7, i64 %tmp25.unpack140, 1
  %fun_call_tmp26 = call {} @"[c-flat]:Particle:update"(ptr %fun_call_tmp22, %"[c-flat]:Vec_2<T = u64>" %tmp25141)
  br label %cond19

cond19:                                           ; preds = %then18, %then10
  %fun_call_tmp21 = call i1 @"[c-flat]:Vec<T = Particle>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp21, label %then18, label %else20

else20:                                           ; preds = %cond19
  %fun_call_tmp30 = call {} @"[c-flat]:pixel:Game:begin_draw"(ptr nonnull %game)
  %fun_call_tmp31 = call {} @"[c-flat]:pixel:Game:clear_background"(ptr nonnull %game)
  %fun_call_tmp33 = call { ptr, i64 } @"[c-flat]:Vec<T = Particle>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp34 = call %"[c-flat]:SliceMutIter<T = Particle>" @"[c-flat]:[Particle]:iter_mut"({ ptr, i64 } %fun_call_tmp33)
  %fun_call_tmp34.elt = extractvalue %"[c-flat]:SliceMutIter<T = Particle>" %fun_call_tmp34, 0
  store ptr %fun_call_tmp34.elt, ptr %tmp_iter4, align 8
  %tmp_iter4.repack137 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %fun_call_tmp34.elt138 = extractvalue %"[c-flat]:SliceMutIter<T = Particle>" %fun_call_tmp34, 1
  store ptr %fun_call_tmp34.elt138, ptr %tmp_iter4.repack137, align 8
  br label %cond36

then35:                                           ; preds = %cond36
  %fun_call_tmp39 = call ptr @"[c-flat]:Vec<T = Particle>:next"(ptr nonnull %tmp_iter4)
  %fun_call_tmp42 = call {} @"[c-flat]:Particle:render"(ptr %fun_call_tmp39, ptr nonnull %game)
  br label %cond36

cond36:                                           ; preds = %then35, %else20
  %fun_call_tmp38 = call i1 @"[c-flat]:Vec<T = Particle>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp38, label %then35, label %else37

else37:                                           ; preds = %cond36
  %fun_call_tmp46 = call {} @"[c-flat]:pixel:Game:end_draw"(ptr nonnull %game)
  br label %cond11
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

define i1 @"[c-flat]:&Particle:eq"(ptr %0, ptr %1) {
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
  %tmp.i26.i = load double, ptr %tmp3.i, align 8
  %tmp1.i27.i = load double, ptr %tmp4.size.i, align 8
  %tmp2.i28.i = fcmp oeq double %tmp.i26.i, %tmp1.i27.i
  %tmp6.i = and i1 %tmp6.i.i, %tmp2.i28.i
  %tmp7.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp8.i = load ptr, ptr %other.i, align 8
  %tmp8.speed.i = getelementptr inbounds nuw i8, ptr %tmp8.i, i64 24
  %tmp.i21.i = load double, ptr %tmp7.i, align 8
  %tmp1.i22.i = load double, ptr %tmp8.speed.i, align 8
  %tmp2.i23.i = fcmp oeq double %tmp.i21.i, %tmp1.i22.i
  %tmp10.i = and i1 %tmp6.i, %tmp2.i23.i
  %tmp11.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp12.i = load ptr, ptr %other.i, align 8
  %tmp12.offset.i = getelementptr inbounds nuw i8, ptr %tmp12.i, i64 32
  %tmp.i.i = load double, ptr %tmp11.i, align 8
  %tmp1.i.i = load double, ptr %tmp12.offset.i, align 8
  %tmp2.i.i = fcmp oeq double %tmp.i.i, %tmp1.i.i
  %tmp14.i = and i1 %tmp10.i, %tmp2.i.i
  %tmp15.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 40
  %tmp16.i = load ptr, ptr %other.i, align 8
  %tmp16.color.i = getelementptr inbounds nuw i8, ptr %tmp16.i, i64 40
  %tmp.i36.i = load i32, ptr %tmp15.i, align 4
  %tmp1.i37.i = load i32, ptr %tmp16.color.i, align 4
  %tmp2.i38.i = icmp eq i32 %tmp.i36.i, %tmp1.i37.i
  %tmp18.i = and i1 %tmp14.i, %tmp2.i38.i
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %other.i)
  ret i1 %tmp18.i
}

define %"[c-flat]:Vec<T = Particle>" @"[c-flat]:Vec<T = Particle>:new"() {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = Particle>" @"[c-flat]:Vec<T = Particle>:with_capacity"(i64 4)
  ret %"[c-flat]:Vec<T = Particle>" %fun_call_tmp
}

define %"[c-flat]:Vec<T = Particle>" @"[c-flat]:Vec<T = Particle>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:alloc<T = Particle>"(i64 %0)
  %tmp_agg = insertvalue %"[c-flat]:Vec<T = Particle>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"[c-flat]:Vec<T = Particle>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"[c-flat]:Vec<T = Particle>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"[c-flat]:u64:drop"(i64 %0)
  ret %"[c-flat]:Vec<T = Particle>" %tmp_constructor
}

define ptr @"[c-flat]:mem:alloc<T = Particle>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i11 = mul i64 %0, 44
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

define {} @"[c-flat]:Vec<T = Particle>:push"(ptr %0, %"[c-flat]:Particle" %1) {
entry:
  %value = alloca %"[c-flat]:Particle", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store %"[c-flat]:Particle" %1, ptr %value, align 8
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = Particle>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i24 = mul i64 %tmp4, 44
  %tmp.i21 = add i64 %tmp.i24, %cast
  %cast8 = inttoptr i64 %tmp.i21 to ptr
  %fun_call_tmp9 = call {} @"[c-flat]:mem:copy<T = Particle>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load %"[c-flat]:Particle", ptr %value, align 8
  %tmp1526 = insertvalue %"[c-flat]:mem:Manually_Drop<T = Particle>" poison, %"[c-flat]:Particle" %tmp14, 0
  %fun_call_tmp16 = call {} @"[c-flat]:mem:Manually_Drop<T = Particle>:drop"(%"[c-flat]:mem:Manually_Drop<T = Particle>" %tmp1526)
  ret {} zeroinitializer
}

define {} @"[c-flat]:Vec<T = Particle>:reserve"(ptr %0, i64 %1) {
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
  %fun_call_tmp21 = call ptr @"[c-flat]:mem:alloc<T = Particle>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:mem:copy_range<T = Particle>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"[c-flat]:mem:free<T = Particle>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"[c-flat]:mem:copy_range<T = Particle>"(ptr %0, ptr %1, i64 %2) {
entry:
  %tmp.i = mul i64 %2, 44
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %tmp.i)
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %2)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:free<T = Particle>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define {} @"[c-flat]:mem:copy<T = Particle>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 44)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:Manually_Drop<T = Particle>:drop"(%"[c-flat]:mem:Manually_Drop<T = Particle>" %0) {
entry:
  ret {} zeroinitializer
}

define %"[c-flat]:Particle" @"[c-flat]:Particle:spawn"(%"[c-flat]:Vec_2<T = u64>" %0) {
entry:
  %tmp5 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp4 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp3 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp25 = alloca %"[c-flat]:Range<T = f64>", align 8
  %tmp1 = alloca %"[c-flat]:Range<T = f64>", align 8
  %screen_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack70 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt71 = extractvalue %"[c-flat]:Vec_2<T = u64>" %0, 1
  store i64 %.elt71, ptr %screen_size.repack70, align 4
  %cast = uitofp i64 %.elt to double
  store double 0.000000e+00, ptr %tmp1, align 8
  %tmp1.repack72 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  store double %cast, ptr %tmp1.repack72, align 8
  %fun_call_tmp1 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp1)
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp2 = load i64, ptr %screen_size.y, align 4
  %cast3 = uitofp i64 %tmp2 to double
  store double 0.000000e+00, ptr %tmp25, align 8
  %tmp25.repack74 = getelementptr inbounds nuw i8, ptr %tmp25, i64 8
  store double %cast3, ptr %tmp25.repack74, align 8
  %fun_call_tmp6 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp25)
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = f64>" undef, double %fun_call_tmp1, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = f64>" %tmp_agg, double %fun_call_tmp6, 1
  store double 1.000000e+00, ptr %tmp3, align 8
  %tmp3.repack76 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  store double 2.400000e+00, ptr %tmp3.repack76, align 8
  %fun_call_tmp8 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp3)
  store double 0.000000e+00, ptr %tmp4, align 8
  %tmp4.repack77 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  store double 5.000000e+00, ptr %tmp4.repack77, align 8
  %fun_call_tmp10 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp4)
  %tmp.i33 = fadd double %fun_call_tmp10, 2.500000e-01
  store double 0.000000e+00, ptr %tmp5, align 8
  %tmp5.repack78 = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  store double 1.000000e+00, ptr %tmp5.repack78, align 8
  %fun_call_tmp13 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp5)
  %tmp_agg14 = insertvalue %"[c-flat]:Particle" undef, %"[c-flat]:Vec_2<T = f64>" %tmp_constructor, 0
  %tmp_agg15 = insertvalue %"[c-flat]:Particle" %tmp_agg14, double %fun_call_tmp8, 1
  %tmp_agg16 = insertvalue %"[c-flat]:Particle" %tmp_agg15, double %tmp.i33, 2
  %tmp_agg17 = insertvalue %"[c-flat]:Particle" %tmp_agg16, double %fun_call_tmp13, 3
  %tmp_constructor18 = insertvalue %"[c-flat]:Particle" %tmp_agg17, i32 -1, 4
  %tmp19.unpack = load double, ptr %tmp5, align 8
  %tmp19.elt79 = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  %tmp19.unpack80 = load double, ptr %tmp19.elt79, align 8
  %fun_call_tmp.i67 = call {} @"[c-flat]:f64:drop"(double %tmp19.unpack80)
  %fun_call_tmp2.i69 = call {} @"[c-flat]:f64:drop"(double %tmp19.unpack)
  %tmp21.unpack = load double, ptr %tmp4, align 8
  %tmp21.elt82 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %tmp21.unpack83 = load double, ptr %tmp21.elt82, align 8
  %fun_call_tmp.i62 = call {} @"[c-flat]:f64:drop"(double %tmp21.unpack83)
  %fun_call_tmp2.i64 = call {} @"[c-flat]:f64:drop"(double %tmp21.unpack)
  %tmp23.unpack = load double, ptr %tmp3, align 8
  %tmp23.elt85 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp23.unpack86 = load double, ptr %tmp23.elt85, align 8
  %fun_call_tmp.i57 = call {} @"[c-flat]:f64:drop"(double %tmp23.unpack86)
  %fun_call_tmp2.i59 = call {} @"[c-flat]:f64:drop"(double %tmp23.unpack)
  %tmp26.unpack = load double, ptr %tmp25, align 8
  %tmp26.elt88 = getelementptr inbounds nuw i8, ptr %tmp25, i64 8
  %tmp26.unpack89 = load double, ptr %tmp26.elt88, align 8
  %fun_call_tmp.i52 = call {} @"[c-flat]:f64:drop"(double %tmp26.unpack89)
  %fun_call_tmp2.i54 = call {} @"[c-flat]:f64:drop"(double %tmp26.unpack)
  %tmp28.unpack = load double, ptr %tmp1, align 8
  %tmp28.elt91 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp28.unpack92 = load double, ptr %tmp28.elt91, align 8
  %fun_call_tmp.i47 = call {} @"[c-flat]:f64:drop"(double %tmp28.unpack92)
  %fun_call_tmp2.i49 = call {} @"[c-flat]:f64:drop"(double %tmp28.unpack)
  %tmp30.unpack = load i64, ptr %screen_size, align 4
  %tmp30.elt94 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp30.unpack95 = load i64, ptr %tmp30.elt94, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp30.unpack95)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp30.unpack)
  ret %"[c-flat]:Particle" %tmp_constructor18
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

define i1 @"[c-flat]:pixel:Game:should_close"(ptr %0) {
entry:
  %fun_call_tmp = call i1 @crl_window_should_close()
  ret i1 %fun_call_tmp
}

declare i1 @crl_window_should_close()

define { ptr, i64 } @"[c-flat]:Vec<T = Particle>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Particle>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = Particle>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define %"[c-flat]:SliceMutIter<T = Particle>" @"[c-flat]:[Particle]:iter_mut"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i15 = mul i64 %.elt21, 44
  %tmp.i = add i64 %tmp.i15, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"[c-flat]:SliceMutIter<T = Particle>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"[c-flat]:SliceMutIter<T = Particle>" %tmp_agg, ptr %cast7, 1
  ret %"[c-flat]:SliceMutIter<T = Particle>" %tmp_constructor
}

define ptr @"[c-flat]:&mut Particle:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
}

define i1 @"[c-flat]:&mut Particle:eq"(ptr %0, ptr %1) {
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
  %tmp.i26.i = load double, ptr %tmp3.i, align 8
  %tmp1.i27.i = load double, ptr %tmp4.size.i, align 8
  %tmp2.i28.i = fcmp oeq double %tmp.i26.i, %tmp1.i27.i
  %tmp6.i = and i1 %tmp6.i.i, %tmp2.i28.i
  %tmp7.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp8.i = load ptr, ptr %other.i, align 8
  %tmp8.speed.i = getelementptr inbounds nuw i8, ptr %tmp8.i, i64 24
  %tmp.i21.i = load double, ptr %tmp7.i, align 8
  %tmp1.i22.i = load double, ptr %tmp8.speed.i, align 8
  %tmp2.i23.i = fcmp oeq double %tmp.i21.i, %tmp1.i22.i
  %tmp10.i = and i1 %tmp6.i, %tmp2.i23.i
  %tmp11.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp12.i = load ptr, ptr %other.i, align 8
  %tmp12.offset.i = getelementptr inbounds nuw i8, ptr %tmp12.i, i64 32
  %tmp.i.i = load double, ptr %tmp11.i, align 8
  %tmp1.i.i = load double, ptr %tmp12.offset.i, align 8
  %tmp2.i.i = fcmp oeq double %tmp.i.i, %tmp1.i.i
  %tmp14.i = and i1 %tmp10.i, %tmp2.i.i
  %tmp15.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 40
  %tmp16.i = load ptr, ptr %other.i, align 8
  %tmp16.color.i = getelementptr inbounds nuw i8, ptr %tmp16.i, i64 40
  %tmp.i36.i = load i32, ptr %tmp15.i, align 4
  %tmp1.i37.i = load i32, ptr %tmp16.color.i, align 4
  %tmp2.i38.i = icmp eq i32 %tmp.i36.i, %tmp1.i37.i
  %tmp18.i = and i1 %tmp14.i, %tmp2.i38.i
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %other.i)
  ret i1 %tmp18.i
}

define i1 @"[c-flat]:Vec<T = Particle>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"[c-flat]:Vec<T = Particle>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 44
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"[c-flat]:Particle:update"(ptr %0, %"[c-flat]:Vec_2<T = u64>" %1) {
entry:
  %tmp231 = alloca %"[c-flat]:Range<T = f64>", align 8
  %screen_size = alloca %"[c-flat]:Vec_2<T = u64>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack65 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt66 = extractvalue %"[c-flat]:Vec_2<T = u64>" %1, 1
  store i64 %.elt66, ptr %screen_size.repack65, align 4
  %tmp1 = load double, ptr %0, align 8
  %tmp2 = load ptr, ptr %self, align 8
  %tmp2.speed = getelementptr inbounds nuw i8, ptr %tmp2, i64 24
  %tmp3 = load double, ptr %tmp2.speed, align 8
  %tmp.i49 = fadd double %tmp1, %tmp3
  store double %tmp.i49, ptr %tmp2, align 8
  %tmp5.pos.y = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp6 = load double, ptr %tmp5.pos.y, align 8
  %tmp7 = load ptr, ptr %self, align 8
  %tmp7.offset = getelementptr inbounds nuw i8, ptr %tmp7, i64 32
  %tmp8 = load double, ptr %tmp7.offset, align 8
  %fun_call_tmp9 = call double @"[c-flat]:f64:sin"(double %tmp8)
  %tmp.i46 = fadd double %tmp6, %fun_call_tmp9
  %tmp11.pos.y = getelementptr inbounds nuw i8, ptr %tmp7, i64 8
  store double %tmp.i46, ptr %tmp11.pos.y, align 8
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.offset = getelementptr inbounds nuw i8, ptr %tmp12, i64 32
  %tmp13 = load double, ptr %tmp12.offset, align 8
  %tmp14.speed = getelementptr inbounds nuw i8, ptr %tmp12, i64 24
  %tmp15 = load double, ptr %tmp14.speed, align 8
  %tmp.i52 = fdiv double %tmp15, 4.600000e+01
  %fun_call_tmp17 = call double @"[c-flat]:f64:min"(double 5.000000e-02, double %tmp.i52)
  %tmp.i43 = fadd double %tmp13, %fun_call_tmp17
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.offset = getelementptr inbounds nuw i8, ptr %tmp19, i64 32
  store double %tmp.i43, ptr %tmp19.offset, align 8
  %tmp20 = load ptr, ptr %self, align 8
  %tmp21 = load double, ptr %tmp20, align 8
  %tmp22 = load i64, ptr %screen_size, align 4
  %cast = uitofp i64 %tmp22 to double
  %tmp.i40 = fadd double %cast, 4.000000e+00
  %tmp.i56 = fcmp ogt double %tmp21, %tmp.i40
  br i1 %tmp.i56, label %then, label %merge

merge:                                            ; preds = %entry, %then
  %tmp35.unpack = load i64, ptr %screen_size, align 4
  %tmp35.elt72 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp35.unpack73 = load i64, ptr %tmp35.elt72, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:u64:drop"(i64 %tmp35.unpack73)
  %fun_call_tmp2.i = call {} @"[c-flat]:u64:drop"(i64 %tmp35.unpack)
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp25 = load ptr, ptr %self, align 8
  store double -4.000000e+00, ptr %tmp25, align 8
  %tmp27.pos.y = getelementptr inbounds nuw i8, ptr %tmp25, i64 8
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp28 = load i64, ptr %screen_size.y, align 4
  %cast29 = uitofp i64 %tmp28 to double
  store double 0.000000e+00, ptr %tmp231, align 8
  %tmp231.repack67 = getelementptr inbounds nuw i8, ptr %tmp231, i64 8
  store double %cast29, ptr %tmp231.repack67, align 8
  %fun_call_tmp32 = call double @"[c-flat]:Vec_2<T = f64>:rand"(ptr nonnull %tmp231)
  store double %fun_call_tmp32, ptr %tmp27.pos.y, align 8
  %tmp33.unpack = load double, ptr %tmp231, align 8
  %tmp33.elt69 = getelementptr inbounds nuw i8, ptr %tmp231, i64 8
  %tmp33.unpack70 = load double, ptr %tmp33.elt69, align 8
  %fun_call_tmp.i61 = call {} @"[c-flat]:f64:drop"(double %tmp33.unpack70)
  %fun_call_tmp2.i63 = call {} @"[c-flat]:f64:drop"(double %tmp33.unpack)
  br label %merge
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

define {} @"[c-flat]:Particle:render"(ptr %0, ptr %1) {
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
  %tmp14.color = getelementptr inbounds nuw i8, ptr %tmp14, i64 40
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

define i32 @"[c-flat]:i32:clone"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  ret i32 %tmp1
}

define {} @"[c-flat]:i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}

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

define {} @"[c-flat]:Vec<T = Particle>:drop.69"(%"[c-flat]:Vec<T = Particle>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"[c-flat]:Vec<T = Particle>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = Particle>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt22 = extractvalue %"[c-flat]:Vec<T = Particle>" %0, 1
  store i64 %.elt22, ptr %self.repack21, align 8
  %self.repack23 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt24 = extractvalue %"[c-flat]:Vec<T = Particle>" %0, 2
  store i64 %.elt24, ptr %self.repack23, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = Particle>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr %"[c-flat]:Particle", ptr %ref.i, i64 %tmp4
  %tmp6 = load %"[c-flat]:Particle", ptr %tmp_index.i, align 8
  %tmp.i18 = extractvalue %"[c-flat]:Particle" %tmp6, 4
  %fun_call_tmp.i = call {} @"[c-flat]:u32:drop"(i32 %tmp.i18)
  %tmp1.i = extractvalue %"[c-flat]:Particle" %tmp6, 3
  %fun_call_tmp2.i = call {} @"[c-flat]:f64:drop"(double %tmp1.i)
  %tmp3.i = extractvalue %"[c-flat]:Particle" %tmp6, 2
  %fun_call_tmp4.i = call {} @"[c-flat]:f64:drop"(double %tmp3.i)
  %tmp5.i = extractvalue %"[c-flat]:Particle" %tmp6, 1
  %fun_call_tmp6.i = call {} @"[c-flat]:f64:drop"(double %tmp5.i)
  %tmp7.i = extractvalue %"[c-flat]:Particle" %tmp6, 0
  %tmp.i.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp7.i, 1
  %fun_call_tmp.i.i = call {} @"[c-flat]:f64:drop"(double %tmp.i.i)
  %tmp1.i.i = extractvalue %"[c-flat]:Vec_2<T = f64>" %tmp7.i, 0
  %fun_call_tmp2.i.i = call {} @"[c-flat]:f64:drop"(double %tmp1.i.i)
  %tmp8 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp8, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Particle>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"[c-flat]:mem:free<T = Particle>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"[c-flat]:Vec<T = Particle>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = Particle>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = Particle>:as_slice"(ptr %0) {
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
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.72, i64 1 })
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
