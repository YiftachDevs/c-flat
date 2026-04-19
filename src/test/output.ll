; ModuleID = 'main_module'
source_filename = "main_module"

%"Vec<T = char>" = type { ptr, i64, i64 }
%"Vec<T = i32>" = type { ptr, i64, i64 }

@count = internal global i64 0
@global_string = private unnamed_addr constant [3 x i8] c"[]\00", align 1
@global_string.1 = private unnamed_addr constant [2 x i8] c"[\00", align 1
@global_string.2 = private unnamed_addr constant [3 x i8] c", \00", align 1

define i64 @main() {
entry:
  %tmp2 = alloca %"Vec<T = char>", align 8
  %vec = alloca %"Vec<T = i32>", align 8
  %tmp0 = alloca [3 x i32], align 4
  store i32 1, ptr %tmp0, align 4
  %tmp0.repack8 = getelementptr inbounds nuw i8, ptr %tmp0, i64 4
  store i32 2, ptr %tmp0.repack8, align 4
  %tmp0.repack9 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  store i32 3, ptr %tmp0.repack9, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp0, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 3, 1
  %fun_call_tmp1 = call %"Vec<T = i32>" @"Vec<T = i32>:from"({ ptr, i64 } %len.i)
  %fun_call_tmp1.elt = extractvalue %"Vec<T = i32>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %vec, align 8
  %vec.repack10 = getelementptr inbounds nuw i8, ptr %vec, i64 8
  %fun_call_tmp1.elt11 = extractvalue %"Vec<T = i32>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt11, ptr %vec.repack10, align 8
  %vec.repack12 = getelementptr inbounds nuw i8, ptr %vec, i64 16
  %fun_call_tmp1.elt13 = extractvalue %"Vec<T = i32>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt13, ptr %vec.repack12, align 8
  %fun_call_tmp2 = call %"Vec<T = char>" @"Vec<T = i32>:to_string"(ptr nonnull %vec)
  %fun_call_tmp2.elt = extractvalue %"Vec<T = char>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %tmp2, align 8
  %tmp2.repack14 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %fun_call_tmp2.elt15 = extractvalue %"Vec<T = char>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt15, ptr %tmp2.repack14, align 8
  %tmp2.repack16 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %fun_call_tmp2.elt17 = extractvalue %"Vec<T = char>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt17, ptr %tmp2.repack16, align 8
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %tmp2)
  %fun_call_tmp4 = call {} @print({ ptr, i64 } %fun_call_tmp3)
  %fun_call_tmp5 = call {} @"Vec<T = char>:drop"(ptr nonnull %tmp2)
  %fun_call_tmp6 = call {} @"Vec<T = i32>:drop"(ptr nonnull %vec)
  %fun_call_tmp7 = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp7, align 4
  ret i64 %tmp
}

define %"Vec<T = i32>" @"Vec<T = i32>:from"({ ptr, i64 } %0) {
entry:
  %result = alloca %"Vec<T = i32>", align 8
  %slice = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack3 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt4 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt4, ptr %slice.repack3, align 8
  %fun_call_tmp = call %"Vec<T = i32>" @"Vec<T = i32>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = i32>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack5 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt6 = extractvalue %"Vec<T = i32>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt6, ptr %result.repack5, align 8
  %result.repack7 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt8 = extractvalue %"Vec<T = i32>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt8, ptr %result.repack7, align 8
  %tmp.unpack = load ptr, ptr %slice, align 8
  %1 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt9 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp.unpack10 = load i64, ptr %tmp.elt9, align 8
  %tmp11 = insertvalue { ptr, i64 } %1, i64 %tmp.unpack10, 1
  %fun_call_tmp1 = call {} @"Vec<T = i32>:extend_from_slice"(ptr nonnull %result, { ptr, i64 } %tmp11)
  %tmp2.unpack = load ptr, ptr %result, align 8
  %2 = insertvalue %"Vec<T = i32>" poison, ptr %tmp2.unpack, 0
  %tmp2.elt12 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp2.unpack13 = load i64, ptr %tmp2.elt12, align 8
  %3 = insertvalue %"Vec<T = i32>" %2, i64 %tmp2.unpack13, 1
  %tmp2.elt14 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp2.unpack15 = load i64, ptr %tmp2.elt14, align 8
  %tmp216 = insertvalue %"Vec<T = i32>" %3, i64 %tmp2.unpack15, 2
  ret %"Vec<T = i32>" %tmp216
}

define %"Vec<T = i32>" @"Vec<T = i32>:new"() {
entry:
  %fun_call_tmp = call %"Vec<T = i32>" @"Vec<T = i32>:with_capacity"(i64 4)
  ret %"Vec<T = i32>" %fun_call_tmp
}

define %"Vec<T = i32>" @"Vec<T = i32>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_alloc<T = i32>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = i32>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = i32>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = i32>" %tmp_agg2, i64 %0, 2
  ret %"Vec<T = i32>" %tmp_constructor
}

define ptr @"mem:heap_alloc<T = i32>"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i7 = shl i64 %0, 2
  %fun_call_tmp6 = call ptr @malloc(i64 %tmp.i7)
  ret ptr %fun_call_tmp6
}

define ptr @"mem:heap_count"() {
entry:
  ret ptr @count
}

declare ptr @malloc(i64)

define {} @"Vec<T = i32>:extend_from_slice"(ptr %0, { ptr, i64 } %1) {
entry:
  %idx = alloca i64, align 8
  %slice = alloca { ptr, i64 }, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack12 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt13 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt13, ptr %slice.repack12, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp3 = load ptr, ptr %self, align 8
  %tmp4.unpack = load ptr, ptr %slice, align 8
  %tmp5 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i32, ptr %tmp4.unpack, i64 %tmp5
  %tmp7 = load i32, ptr %tmp_index.i, align 4
  %fun_call_tmp8 = call {} @"Vec<T = i32>:push"(ptr %tmp3, i32 %tmp7)
  %tmp.i = add i64 %tmp5, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1.elt14 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp1.unpack15 = load i64, ptr %tmp1.elt14, align 8
  %tmp.i11 = icmp ult i64 %storemerge, %tmp1.unpack15
  br i1 %tmp.i11, label %then, label %else

else:                                             ; preds = %cond
  ret {} zeroinitializer
}

define {} @"Vec<T = i32>:push"(ptr %0, i32 %1) {
entry:
  %value = alloca i32, align 4
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i32 %1, ptr %value, align 4
  %fun_call_tmp = call {} @"Vec<T = i32>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i16 = shl i64 %tmp4, 2
  %tmp.i15 = add i64 %tmp.i16, %cast
  %cast8 = inttoptr i64 %tmp.i15 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = i32>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  ret {} zeroinitializer
}

define {} @"Vec<T = i32>:reserve"(ptr %0, i64 %1) {
entry:
  %new_ptr = alloca ptr, align 8
  %new_len = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i = add i64 %tmp1, %1
  store i64 %tmp.i, ptr %new_len, align 4
  %tmp3 = load i64, ptr %new_len, align 4
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.capacity = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp5 = load i64, ptr %tmp4.capacity, align 4
  %tmp.i36 = icmp ugt i64 %tmp3, %tmp5
  br i1 %tmp.i36, label %cond9, label %merge

merge:                                            ; preds = %entry, %else10
  ret {} zeroinitializer

then8:                                            ; preds = %cond9
  %tmp15 = load ptr, ptr %self, align 8
  %tmp15.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  %tmp16 = load i64, ptr %tmp15.capacity, align 4
  %tmp.i34 = shl i64 %tmp16, 1
  %tmp18.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  store i64 %tmp.i34, ptr %tmp18.capacity, align 4
  br label %cond9

cond9:                                            ; preds = %entry, %then8
  %tmp11 = load i64, ptr %new_len, align 4
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.capacity = getelementptr inbounds nuw i8, ptr %tmp12, i64 16
  %tmp13 = load i64, ptr %tmp12.capacity, align 4
  %tmp.i35 = icmp ugt i64 %tmp11, %tmp13
  br i1 %tmp.i35, label %then8, label %else10

else10:                                           ; preds = %cond9
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.capacity = getelementptr inbounds nuw i8, ptr %tmp19, i64 16
  %tmp20 = load i64, ptr %tmp19.capacity, align 4
  %fun_call_tmp21 = call ptr @"mem:heap_alloc<T = i32>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"mem:copy_range<T = i32>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"mem:heap_free<T = i32>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"mem:copy_range<T = i32>"(ptr %0, ptr %1, i64 %2) {
entry:
  %tmp.i = shl i64 %2, 2
  %fun_call_tmp4 = call {} @memcpy(ptr %0, ptr %1, i64 %tmp.i)
  ret {} zeroinitializer
}

declare {} @memcpy(ptr, ptr, i64)

define {} @"mem:heap_free<T = i32>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @free(ptr %0)
  ret {} %fun_call_tmp4
}

declare {} @free(ptr)

define {} @"mem:copy<T = i32>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @memcpy(ptr %0, ptr %1, i64 4)
  ret {} zeroinitializer
}

define {} @print({ ptr, i64 } %0) {
entry:
  %null_terminated = alloca %"Vec<T = char>", align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %null_terminated, align 8
  %null_terminated.repack11 = getelementptr inbounds nuw i8, ptr %null_terminated, i64 8
  %fun_call_tmp.elt12 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt12, ptr %null_terminated.repack11, align 8
  %null_terminated.repack13 = getelementptr inbounds nuw i8, ptr %null_terminated, i64 16
  %fun_call_tmp.elt14 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt14, ptr %null_terminated.repack13, align 8
  %fun_call_tmp1 = call {} @"Vec<T = char>:push"(ptr nonnull %null_terminated, i8 0)
  %fun_call_tmp2 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %null_terminated)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call {} @puts(ptr %ref.i)
  %fun_call_tmp5 = call {} @"Vec<T = char>:drop"(ptr nonnull %null_terminated)
  ret {} zeroinitializer
}

define %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0) {
entry:
  %result = alloca %"Vec<T = char>", align 8
  %slice = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack3 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt4 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt4, ptr %slice.repack3, align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack5 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt6 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt6, ptr %result.repack5, align 8
  %result.repack7 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt8 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt8, ptr %result.repack7, align 8
  %tmp.unpack = load ptr, ptr %slice, align 8
  %1 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt9 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp.unpack10 = load i64, ptr %tmp.elt9, align 8
  %tmp11 = insertvalue { ptr, i64 } %1, i64 %tmp.unpack10, 1
  %fun_call_tmp1 = call {} @"Vec<T = char>:extend_from_slice"(ptr nonnull %result, { ptr, i64 } %tmp11)
  %tmp2.unpack = load ptr, ptr %result, align 8
  %2 = insertvalue %"Vec<T = char>" poison, ptr %tmp2.unpack, 0
  %tmp2.elt12 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp2.unpack13 = load i64, ptr %tmp2.elt12, align 8
  %3 = insertvalue %"Vec<T = char>" %2, i64 %tmp2.unpack13, 1
  %tmp2.elt14 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp2.unpack15 = load i64, ptr %tmp2.elt14, align 8
  %tmp216 = insertvalue %"Vec<T = char>" %3, i64 %tmp2.unpack15, 2
  ret %"Vec<T = char>" %tmp216
}

define %"Vec<T = char>" @"Vec<T = char>:new"() {
entry:
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:with_capacity"(i64 4)
  ret %"Vec<T = char>" %fun_call_tmp
}

define %"Vec<T = char>" @"Vec<T = char>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_alloc<T = char>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = char>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = char>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = char>" %tmp_agg2, i64 %0, 2
  ret %"Vec<T = char>" %tmp_constructor
}

define ptr @"mem:heap_alloc<T = char>"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp6 = call ptr @malloc(i64 %0)
  ret ptr %fun_call_tmp6
}

define {} @"Vec<T = char>:extend_from_slice"(ptr %0, { ptr, i64 } %1) {
entry:
  %idx = alloca i64, align 8
  %slice = alloca { ptr, i64 }, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack12 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt13 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt13, ptr %slice.repack12, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp3 = load ptr, ptr %self, align 8
  %tmp4.unpack = load ptr, ptr %slice, align 8
  %tmp5 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp4.unpack, i64 %tmp5
  %tmp7 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp8 = call {} @"Vec<T = char>:push"(ptr %tmp3, i8 %tmp7)
  %tmp.i = add i64 %tmp5, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1.elt14 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp1.unpack15 = load i64, ptr %tmp1.elt14, align 8
  %tmp.i11 = icmp ult i64 %storemerge, %tmp1.unpack15
  br i1 %tmp.i11, label %then, label %else

else:                                             ; preds = %cond
  ret {} zeroinitializer
}

define {} @"Vec<T = char>:push"(ptr %0, i8 %1) {
entry:
  %value = alloca i8, align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 %1, ptr %value, align 1
  %fun_call_tmp = call {} @"Vec<T = char>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i15 = add i64 %tmp4, %cast
  %cast8 = inttoptr i64 %tmp.i15 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = char>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  ret {} zeroinitializer
}

define {} @"Vec<T = char>:reserve"(ptr %0, i64 %1) {
entry:
  %new_ptr = alloca ptr, align 8
  %new_len = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i = add i64 %tmp1, %1
  store i64 %tmp.i, ptr %new_len, align 4
  %tmp3 = load i64, ptr %new_len, align 4
  %tmp4 = load ptr, ptr %self, align 8
  %tmp4.capacity = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %tmp5 = load i64, ptr %tmp4.capacity, align 4
  %tmp.i36 = icmp ugt i64 %tmp3, %tmp5
  br i1 %tmp.i36, label %cond9, label %merge

merge:                                            ; preds = %entry, %else10
  ret {} zeroinitializer

then8:                                            ; preds = %cond9
  %tmp15 = load ptr, ptr %self, align 8
  %tmp15.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  %tmp16 = load i64, ptr %tmp15.capacity, align 4
  %tmp.i34 = shl i64 %tmp16, 1
  %tmp18.capacity = getelementptr inbounds nuw i8, ptr %tmp15, i64 16
  store i64 %tmp.i34, ptr %tmp18.capacity, align 4
  br label %cond9

cond9:                                            ; preds = %entry, %then8
  %tmp11 = load i64, ptr %new_len, align 4
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.capacity = getelementptr inbounds nuw i8, ptr %tmp12, i64 16
  %tmp13 = load i64, ptr %tmp12.capacity, align 4
  %tmp.i35 = icmp ugt i64 %tmp11, %tmp13
  br i1 %tmp.i35, label %then8, label %else10

else10:                                           ; preds = %cond9
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.capacity = getelementptr inbounds nuw i8, ptr %tmp19, i64 16
  %tmp20 = load i64, ptr %tmp19.capacity, align 4
  %fun_call_tmp21 = call ptr @"mem:heap_alloc<T = char>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"mem:copy_range<T = char>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"mem:heap_free<T = char>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"mem:copy_range<T = char>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp4 = call {} @memcpy(ptr %0, ptr %1, i64 %2)
  ret {} zeroinitializer
}

define {} @"mem:heap_free<T = char>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @free(ptr %0)
  ret {} %fun_call_tmp4
}

define {} @"mem:copy<T = char>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

declare {} @puts(ptr)

define { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = char>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = char>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define {} @"Vec<T = char>:drop"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp4 = load ptr, ptr %self, align 8
  %fun_call_tmp5 = call { ptr, i64 } @"Vec<T = char>:deref"(ptr %tmp4)
  %tmp6 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp5, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp6
  %fun_call_tmp8 = call {} @"char:drop"(ptr %tmp_index.i)
  %tmp.i = add i64 %tmp6, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1 = load ptr, ptr %self, align 8
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = char>:deref"(ptr %tmp1)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i14 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i14, label %then, label %else

else:                                             ; preds = %cond
  %tmp11 = load ptr, ptr %self, align 8
  %tmp12 = load ptr, ptr %tmp11, align 8
  %fun_call_tmp13 = call {} @"mem:heap_free<T = char>"(ptr %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"Vec<T = char>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = char>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = char>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define {} @"char:drop"(ptr %0) {
entry:
  ret {} zeroinitializer
}

define %"Vec<T = char>" @"Vec<T = i32>:to_string"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %result3 = alloca %"Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp = call i1 @"Vec<T = i32>:is_empty"(ptr %tmp)
  br i1 %fun_call_tmp, label %then, label %else

common.ret:                                       ; preds = %else7, %then
  %common.ret.op = phi %"Vec<T = char>" [ %fun_call_tmp1, %then ], [ %tmp2636, %else7 ]
  ret %"Vec<T = char>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp1 = call %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } { ptr @global_string, i64 2 })
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp2 = call %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } { ptr @global_string.1, i64 1 })
  %fun_call_tmp2.elt = extractvalue %"Vec<T = char>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %result3, align 8
  %result3.repack28 = getelementptr inbounds nuw i8, ptr %result3, i64 8
  %fun_call_tmp2.elt29 = extractvalue %"Vec<T = char>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt29, ptr %result3.repack28, align 8
  %result3.repack30 = getelementptr inbounds nuw i8, ptr %result3, i64 16
  %fun_call_tmp2.elt31 = extractvalue %"Vec<T = char>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt31, ptr %result3.repack30, align 8
  br label %cond6

then5:                                            ; preds = %cond6
  %tmp13 = load ptr, ptr %self, align 8
  %fun_call_tmp14 = call { ptr, i64 } @"Vec<T = i32>:deref"(ptr %tmp13)
  %tmp15 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp14, 0
  %tmp_index.i = getelementptr i32, ptr %ref.i, i64 %tmp15
  %fun_call_tmp17 = call %"Vec<T = char>" @"i32:to_string"(ptr %tmp_index.i)
  %fun_call_tmp18 = call {} @"Vec<T = char>:extend"(ptr nonnull %result3, %"Vec<T = char>" %fun_call_tmp17)
  %fun_call_tmp19 = call {} @"Vec<T = char>:extend_from_slice"(ptr nonnull %result3, { ptr, i64 } { ptr @global_string.2, i64 2 })
  %tmp.i = add i64 %tmp15, 1
  br label %cond6

cond6:                                            ; preds = %then5, %else
  %storemerge = phi i64 [ 0, %else ], [ %tmp.i, %then5 ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp9 = load ptr, ptr %self, align 8
  %fun_call_tmp10 = call { ptr, i64 } @"Vec<T = i32>:deref"(ptr %tmp9)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp10, 1
  %tmp.i27 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i27, label %then5, label %else7

else7:                                            ; preds = %cond6
  %fun_call_tmp23 = call {} @"Vec<T = char>:pop"(ptr nonnull %result3)
  %fun_call_tmp24 = call {} @"Vec<T = char>:pop"(ptr nonnull %result3)
  %fun_call_tmp25 = call {} @"Vec<T = char>:push"(ptr nonnull %result3, i8 93)
  %tmp26.unpack = load ptr, ptr %result3, align 8
  %1 = insertvalue %"Vec<T = char>" poison, ptr %tmp26.unpack, 0
  %tmp26.elt32 = getelementptr inbounds nuw i8, ptr %result3, i64 8
  %tmp26.unpack33 = load i64, ptr %tmp26.elt32, align 8
  %2 = insertvalue %"Vec<T = char>" %1, i64 %tmp26.unpack33, 1
  %tmp26.elt34 = getelementptr inbounds nuw i8, ptr %result3, i64 16
  %tmp26.unpack35 = load i64, ptr %tmp26.elt34, align 8
  %tmp2636 = insertvalue %"Vec<T = char>" %2, i64 %tmp26.unpack35, 2
  br label %common.ret
}

define i1 @"Vec<T = i32>:is_empty"(ptr %0) {
entry:
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i = icmp eq i64 %tmp1, 0
  ret i1 %tmp.i
}

define %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  ret %"Vec<T = char>" %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = i32>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = i32>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = i32>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define { ptr, i64 } @"Vec<T = i32>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = i32>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = i32>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define {} @"Vec<T = char>:extend"(ptr %0, %"Vec<T = char>" %1) {
entry:
  %other = alloca %"Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"Vec<T = char>" %1, 0
  store ptr %.elt, ptr %other, align 8
  %other.repack3 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %.elt4 = extractvalue %"Vec<T = char>" %1, 1
  store i64 %.elt4, ptr %other.repack3, align 8
  %other.repack5 = getelementptr inbounds nuw i8, ptr %other, i64 16
  %.elt6 = extractvalue %"Vec<T = char>" %1, 2
  store i64 %.elt6, ptr %other.repack5, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = char>:deref"(ptr nonnull %other)
  %fun_call_tmp1 = call {} @"Vec<T = char>:extend_from_slice"(ptr %tmp, { ptr, i64 } %fun_call_tmp)
  %fun_call_tmp2 = call {} @"Vec<T = char>:drop"(ptr nonnull %other)
  ret {} zeroinitializer
}

define %"Vec<T = char>" @"i32:to_string"(ptr %0) {
entry:
  %num = alloca i32, align 4
  %result = alloca %"Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack40 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt41 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt41, ptr %result.repack40, align 8
  %result.repack42 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt43 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt43, ptr %result.repack42, align 8
  %tmp = load ptr, ptr %self, align 8
  %tmp1 = load i32, ptr %tmp, align 4
  %tmp.i38 = icmp slt i32 %tmp1, 0
  br i1 %tmp.i38, label %then, label %then6

merge:                                            ; preds = %then6, %then
  %result9 = phi i32 [ %tmp.i35, %then ], [ %tmp8, %then6 ]
  br label %cond12

then:                                             ; preds = %entry
  %tmp3 = load ptr, ptr %self, align 8
  %tmp4 = load i32, ptr %tmp3, align 4
  %tmp.i35 = sub i32 0, %tmp4
  br label %merge

then6:                                            ; preds = %entry
  %tmp7 = load ptr, ptr %self, align 8
  %tmp8 = load i32, ptr %tmp7, align 4
  br label %merge

then11:                                           ; preds = %cond12
  %tmp17 = load i32, ptr %num, align 4
  %tmp.i36 = srem i32 %tmp17, 10
  %1 = trunc nsw i32 %tmp.i36 to i8
  %cast = add nsw i8 %1, 48
  %fun_call_tmp20 = call {} @"Vec<T = char>:insert"(ptr nonnull %result, i64 0, i8 %cast)
  %tmp.i34 = sdiv i32 %tmp17, 10
  br label %cond12

cond12:                                           ; preds = %then11, %merge
  %storemerge = phi i32 [ %result9, %merge ], [ %tmp.i34, %then11 ]
  store i32 %storemerge, ptr %num, align 4
  %tmp.i39.not = icmp eq i32 %storemerge, 0
  br i1 %tmp.i39.not, label %cond26, label %then11

merge24:                                          ; preds = %cond26, %then25
  %tmp33.unpack = load ptr, ptr %result, align 8
  %2 = insertvalue %"Vec<T = char>" poison, ptr %tmp33.unpack, 0
  %tmp33.elt44 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp33.unpack45 = load i64, ptr %tmp33.elt44, align 8
  %3 = insertvalue %"Vec<T = char>" %2, i64 %tmp33.unpack45, 1
  %tmp33.elt46 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp33.unpack47 = load i64, ptr %tmp33.elt46, align 8
  %tmp3348 = insertvalue %"Vec<T = char>" %3, i64 %tmp33.unpack47, 2
  ret %"Vec<T = char>" %tmp3348

then25:                                           ; preds = %cond26
  %fun_call_tmp31 = call {} @"Vec<T = char>:insert"(ptr nonnull %result, i64 0, i8 45)
  br label %merge24

cond26:                                           ; preds = %cond12
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load i32, ptr %tmp28, align 4
  %tmp.i37 = icmp slt i32 %tmp29, 0
  br i1 %tmp.i37, label %then25, label %merge24
}

define {} @"Vec<T = char>:insert"(ptr %0, i64 %1, i8 %2) {
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
  %fun_call_tmp1 = call {} @"Vec<T = char>:reserve"(ptr %0, i64 1)
  %tmp3 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp3 to i64
  %tmp.i28 = add i64 %1, %cast
  %cast8 = inttoptr i64 %tmp.i28 to ptr
  store ptr %cast8, ptr %insert_ptr, align 8
  %tmp11 = load i64, ptr %type_mem_size, align 4
  %tmp.i27 = add i64 %tmp.i28, %tmp11
  %cast13 = inttoptr i64 %tmp.i27 to ptr
  %tmp14 = load ptr, ptr %self, align 8
  %tmp14.len = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  %tmp15 = load i64, ptr %tmp14.len, align 4
  %tmp16 = load i64, ptr %idx, align 4
  %tmp.i29 = sub i64 %tmp15, %tmp16
  %tmp18 = load ptr, ptr %insert_ptr, align 8
  %fun_call_tmp19 = call {} @"mem:copy_range<T = char>"(ptr %cast13, ptr %tmp18, i64 %tmp.i29)
  %fun_call_tmp21 = call {} @"mem:copy<T = char>"(ptr %tmp18, ptr nonnull %value)
  %tmp22 = load ptr, ptr %self, align 8
  %tmp22.len = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  %tmp23 = load i64, ptr %tmp22.len, align 4
  %tmp.i = add i64 %tmp23, 1
  %tmp25.len = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  store i64 %tmp.i, ptr %tmp25.len, align 4
  ret {} zeroinitializer
}

define {} @"Vec<T = char>:pop"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp = load ptr, ptr %self, align 8
  %tmp.len = getelementptr inbounds nuw i8, ptr %tmp, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp.i12.not = icmp eq i64 %tmp1, 0
  br i1 %tmp.i12.not, label %merge, label %then

merge:                                            ; preds = %entry, %then
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp2 = load ptr, ptr %self, align 8
  %tmp2.len = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp3 = load i64, ptr %tmp2.len, align 4
  %tmp.i = add i64 %tmp3, -1
  %tmp5.len = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  store i64 %tmp.i, ptr %tmp5.len, align 4
  %fun_call_tmp7 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %tmp2)
  %tmp8 = load ptr, ptr %self, align 8
  %tmp8.len = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  %tmp9 = load i64, ptr %tmp8.len, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp7, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp9
  %fun_call_tmp11 = call {} @"char:drop"(ptr %tmp_index.i)
  br label %merge
}

define {} @"Vec<T = i32>:drop"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp4 = load ptr, ptr %self, align 8
  %fun_call_tmp5 = call { ptr, i64 } @"Vec<T = i32>:deref"(ptr %tmp4)
  %tmp6 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp5, 0
  %tmp_index.i = getelementptr i32, ptr %ref.i, i64 %tmp6
  %fun_call_tmp8 = call {} @"i32:drop"(ptr %tmp_index.i)
  %tmp.i = add i64 %tmp6, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1 = load ptr, ptr %self, align 8
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = i32>:deref"(ptr %tmp1)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i14 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i14, label %then, label %else

else:                                             ; preds = %cond
  %tmp11 = load ptr, ptr %self, align 8
  %tmp12 = load ptr, ptr %tmp11, align 8
  %fun_call_tmp13 = call {} @"mem:heap_free<T = i32>"(ptr %tmp12)
  ret {} zeroinitializer
}

define {} @"i32:drop"(ptr %0) {
entry:
  ret {} zeroinitializer
}
