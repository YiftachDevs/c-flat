; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:Range<T = u64>" = type { i64, i64 }
%"[c-flat]:Vec_2<T = i32>" = type { i32, i32 }
%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }

@global_string = private unnamed_addr constant [7 x i8] c"square\00", align 1
@global_string.1 = private unnamed_addr constant [10 x i8] c"image.png\00", align 1
@"[c-flat]:seed" = internal global i64 91237
@"[c-flat]:count" = internal global i64 0
@global_string.2 = private unnamed_addr constant [10 x i8] c"game over\00", align 1
@global_string.3 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @"[c-flat]:gaming"()
  %fun_call_tmp1 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.2, i64 9 })
  %fun_call_tmp2 = call {} @"[c-flat]:debug_heap_count"()
  ret i64 0
}

define {} @"[c-flat]:gaming"() {
entry:
  %tmp6 = alloca %"[c-flat]:Range<T = u64>", align 8
  %tmp519 = alloca %"[c-flat]:Range<T = u64>", align 8
  %dt = alloca float, align 4
  %texture = alloca i32, align 4
  %screen_viewport = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %pixel_factor = alloca i32, align 4
  %pixel_viewport = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  store i32 320, ptr %pixel_viewport, align 4
  %pixel_viewport.repack71 = getelementptr inbounds nuw i8, ptr %pixel_viewport, i64 4
  store i32 180, ptr %pixel_viewport.repack71, align 4
  store i32 6, ptr %pixel_factor, align 4
  %fun_call_tmp = call %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:Vec_2<T = i32>:mul"(%"[c-flat]:Vec_2<T = i32>" { i32 320, i32 180 }, i32 6)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec_2<T = i32>" %fun_call_tmp, 0
  store i32 %fun_call_tmp.elt, ptr %screen_viewport, align 4
  %screen_viewport.repack75 = getelementptr inbounds nuw i8, ptr %screen_viewport, i64 4
  %fun_call_tmp.elt76 = extractvalue %"[c-flat]:Vec_2<T = i32>" %fun_call_tmp, 1
  store i32 %fun_call_tmp.elt76, ptr %screen_viewport.repack75, align 4
  %fun_call_tmp2 = call {} @SetConfigFlags(i32 64)
  %fun_call_tmp6 = call {} @InitWindow(i32 %fun_call_tmp.elt, i32 %fun_call_tmp.elt76, ptr nonnull @global_string)
  %pixel_viewport.y = getelementptr inbounds nuw i8, ptr %pixel_viewport, i64 4
  %tmp7 = load i32, ptr %pixel_viewport, align 4
  %tmp8 = load i32, ptr %pixel_viewport.y, align 4
  %fun_call_tmp9 = call {} @init_canvas(i32 %tmp7, i32 %tmp8)
  %fun_call_tmp11 = call i32 @load_texture(ptr nonnull @global_string.1)
  store i32 %fun_call_tmp11, ptr %texture, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp14 = call float @GetFrameTime()
  store float %fun_call_tmp14, ptr %dt, align 4
  %fun_call_tmp15 = call {} @begin_canvas()
  %fun_call_tmp16 = call {} @ClearBackground(i32 -16777216)
  store i64 -10, ptr %tmp519, align 4
  %tmp519.repack83 = getelementptr inbounds nuw i8, ptr %tmp519, i64 8
  store i64 11, ptr %tmp519.repack83, align 4
  %fun_call_tmp20 = call i64 @"[c-flat]:Range<T = u64>:rand"(ptr nonnull %tmp519)
  %cast = trunc i64 %fun_call_tmp20 to i32
  store i64 -10, ptr %tmp6, align 4
  %tmp6.repack84 = getelementptr inbounds nuw i8, ptr %tmp6, i64 8
  store i64 11, ptr %tmp6.repack84, align 4
  %fun_call_tmp23 = call i64 @"[c-flat]:Range<T = u64>:rand"(ptr nonnull %tmp6)
  %cast24 = trunc i64 %fun_call_tmp23 to i32
  %tmp25 = load i32, ptr %texture, align 4
  %fun_call_tmp26 = call i32 @draw_texture(i32 %tmp25, i32 %cast, i32 %cast24, i32 -1)
  %screen_viewport.y28 = getelementptr inbounds nuw i8, ptr %screen_viewport, i64 4
  %tmp29 = load i32, ptr %screen_viewport, align 4
  %tmp30 = load i32, ptr %screen_viewport.y28, align 4
  %fun_call_tmp31 = call {} @end_canvas(i32 %tmp29, i32 %tmp30)
  %tmp32.unpack = load i64, ptr %tmp6, align 4
  %tmp32.elt85 = getelementptr inbounds nuw i8, ptr %tmp6, i64 8
  %tmp32.unpack86 = load i64, ptr %tmp32.elt85, align 4
  %fun_call_tmp.i68 = call {} @"[c-flat]:u64:drop"(i64 %tmp32.unpack86)
  %fun_call_tmp2.i70 = call {} @"[c-flat]:u64:drop"(i64 %tmp32.unpack)
  %tmp34.unpack = load i64, ptr %tmp519, align 4
  %tmp34.elt88 = getelementptr inbounds nuw i8, ptr %tmp519, i64 8
  %tmp34.unpack89 = load i64, ptr %tmp34.elt88, align 4
  %fun_call_tmp.i63 = call {} @"[c-flat]:u64:drop"(i64 %tmp34.unpack89)
  %fun_call_tmp2.i65 = call {} @"[c-flat]:u64:drop"(i64 %tmp34.unpack)
  %tmp36 = load float, ptr %dt, align 4
  %fun_call_tmp37 = call {} @"[c-flat]:f32:drop"(float %tmp36)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp12 = call i1 @WindowShouldClose()
  br i1 %fun_call_tmp12, label %else, label %then

else:                                             ; preds = %cond
  %fun_call_tmp38 = call {} @CloseWindow()
  %tmp39 = load i32, ptr %texture, align 4
  %fun_call_tmp40 = call {} @"[c-flat]:i32:drop"(i32 %tmp39)
  %tmp41.unpack = load i32, ptr %screen_viewport, align 4
  %tmp41.elt77 = getelementptr inbounds nuw i8, ptr %screen_viewport, i64 4
  %tmp41.unpack78 = load i32, ptr %tmp41.elt77, align 4
  %fun_call_tmp.i49 = call {} @"[c-flat]:i32:drop"(i32 %tmp41.unpack78)
  %fun_call_tmp2.i51 = call {} @"[c-flat]:i32:drop"(i32 %tmp41.unpack)
  %tmp43 = load i32, ptr %pixel_factor, align 4
  %fun_call_tmp44 = call {} @"[c-flat]:i32:drop"(i32 %tmp43)
  %tmp45.unpack = load i32, ptr %pixel_viewport, align 4
  %tmp45.elt80 = getelementptr inbounds nuw i8, ptr %pixel_viewport, i64 4
  %tmp45.unpack81 = load i32, ptr %tmp45.elt80, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp45.unpack81)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp45.unpack)
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

define %"[c-flat]:Vec_2<T = i32>" @"[c-flat]:Vec_2<T = i32>:mul"(%"[c-flat]:Vec_2<T = i32>" %0, i32 %1) {
entry:
  %self = alloca %"[c-flat]:Vec_2<T = i32>", align 8
  %.elt = extractvalue %"[c-flat]:Vec_2<T = i32>" %0, 0
  store i32 %.elt, ptr %self, align 4
  %self.repack14 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %.elt15 = extractvalue %"[c-flat]:Vec_2<T = i32>" %0, 1
  store i32 %.elt15, ptr %self.repack14, align 4
  %tmp.i11 = mul i32 %.elt, %1
  %tmp.i = mul i32 %.elt15, %1
  %tmp_agg = insertvalue %"[c-flat]:Vec_2<T = i32>" undef, i32 %tmp.i11, 0
  %tmp_constructor = insertvalue %"[c-flat]:Vec_2<T = i32>" %tmp_agg, i32 %tmp.i, 1
  %fun_call_tmp6 = call {} @"[c-flat]:i32:drop"(i32 %1)
  %tmp7.unpack = load i32, ptr %self, align 4
  %tmp7.elt16 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %tmp7.unpack17 = load i32, ptr %tmp7.elt16, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp7.unpack17)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp7.unpack)
  ret %"[c-flat]:Vec_2<T = i32>" %tmp_constructor
}

declare {} @SetConfigFlags(i32)

declare {} @InitWindow(i32, i32, ptr)

declare {} @init_canvas(i32, i32)

declare i32 @load_texture(ptr)

declare i1 @WindowShouldClose()

declare float @GetFrameTime()

declare {} @begin_canvas()

declare {} @ClearBackground(i32)

declare i32 @draw_texture(i32, i32, i32, i32)

define i64 @"[c-flat]:u64:clone"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  ret i64 %tmp1
}

define {} @"[c-flat]:u64:drop"(i64 %0) {
entry:
  ret {} zeroinitializer
}

define i64 @"[c-flat]:Range<T = u64>:rand"(ptr %0) {
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

declare {} @end_canvas(i32, i32)

define {} @"[c-flat]:f32:drop"(float %0) {
entry:
  ret {} zeroinitializer
}

declare {} @CloseWindow()

define {} @"[c-flat]:println"({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt21 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt21, ptr %tmp1.repack20, align 8
  %tmp1.repack22 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt23 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt23, ptr %tmp1.repack22, align 8
  %fun_call_tmp1 = call ptr @"[c-flat]:Vec<T = char>:null_terminate"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr %fun_call_tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %fun_call_tmp5 = call {} @c_puts(ptr %ref.i)
  %fun_call_tmp6 = call {} @c_flush()
  %tmp9.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp9.unpack, 0
  %tmp9.elt24 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp9.unpack25 = load i64, ptr %tmp9.elt24, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp9.unpack25, 1
  %tmp9.elt26 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp9.unpack27 = load i64, ptr %tmp9.elt26, align 8
  %tmp928 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp9.unpack27, 2
  %fun_call_tmp10 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp928)
  ret {} zeroinitializer
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
  %tmp.i22 = icmp ult i64 %storemerge, %tmp1.unpack27
  br i1 %tmp.i22, label %then, label %else

else:                                             ; preds = %cond
  %tmp11 = load i64, ptr %idx, align 4
  %fun_call_tmp12 = call {} @"[c-flat]:u64:drop"(i64 %tmp11)
  ret {} zeroinitializer
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

define ptr @"[c-flat]:Vec<T = char>:null_terminate"(ptr %0) {
entry:
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = char>:push"(ptr %0, i8 0)
  ret ptr %0
}

declare {} @c_puts(ptr)

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

declare {} @c_flush()

define {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %0) {
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
  %tmp.i18 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i18, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"[c-flat]:mem:free<T = char>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
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
  %fun_call_tmp5 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp416)
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
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.3, i64 1 })
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

define %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0)
  ret %"[c-flat]:Vec<T = char>" %fun_call_tmp
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
  %tmp.i44 = add i64 %1, %cast
  %cast8 = inttoptr i64 %tmp.i44 to ptr
  store ptr %cast8, ptr %insert_ptr, align 8
  %tmp11 = load i64, ptr %type_mem_size, align 4
  %tmp.i41 = add i64 %tmp.i44, %tmp11
  %cast13 = inttoptr i64 %tmp.i41 to ptr
  %tmp14 = load ptr, ptr %self, align 8
  %tmp14.len = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  %tmp15 = load i64, ptr %tmp14.len, align 4
  %tmp16 = load i64, ptr %idx, align 4
  %tmp.i47 = sub i64 %tmp15, %tmp16
  %tmp18 = load ptr, ptr %insert_ptr, align 8
  %fun_call_tmp19 = call {} @"[c-flat]:mem:copy_range<T = char>"(ptr %cast13, ptr %tmp18, i64 %tmp.i47)
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
