; ModuleID = 'main_module'
source_filename = "main_module"

%"Box<T = i32>" = type { ptr }
%"Vec<T = Vec<T = i32>>" = type { ptr, i64, i64 }
%"Vec<T = i32>" = type { ptr, i64, i64 }

@count = internal global i64 0

define i64 @main() {
entry:
  %box = alloca %"Box<T = i32>", align 8
  %tmp18 = alloca [3 x i32], align 4
  %tmp14 = alloca [3 x i32], align 4
  %tmp1 = alloca [3 x i32], align 4
  %vec = alloca %"Vec<T = Vec<T = i32>>", align 8
  %fun_call_tmp = call %"Vec<T = Vec<T = i32>>" @"Vec<T = Vec<T = i32>>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = Vec<T = i32>>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %vec, align 8
  %vec.repack20 = getelementptr inbounds nuw i8, ptr %vec, i64 8
  %fun_call_tmp.elt21 = extractvalue %"Vec<T = Vec<T = i32>>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt21, ptr %vec.repack20, align 8
  %vec.repack22 = getelementptr inbounds nuw i8, ptr %vec, i64 16
  %fun_call_tmp.elt23 = extractvalue %"Vec<T = Vec<T = i32>>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt23, ptr %vec.repack22, align 8
  store i32 1, ptr %tmp1, align 4
  %tmp1.repack24 = getelementptr inbounds nuw i8, ptr %tmp1, i64 4
  store i32 2, ptr %tmp1.repack24, align 4
  %tmp1.repack25 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  store i32 3, ptr %tmp1.repack25, align 4
  %ptr.i18 = insertvalue { ptr, i64 } undef, ptr %tmp1, 0
  %len.i19 = insertvalue { ptr, i64 } %ptr.i18, i64 3, 1
  %fun_call_tmp2 = call %"Vec<T = i32>" @"Vec<T = i32>:from"({ ptr, i64 } %len.i19)
  %fun_call_tmp3 = call {} @"Vec<T = Vec<T = i32>>:push"(ptr nonnull %vec, %"Vec<T = i32>" %fun_call_tmp2)
  store i32 1, ptr %tmp14, align 4
  %tmp14.repack26 = getelementptr inbounds nuw i8, ptr %tmp14, i64 4
  store i32 2, ptr %tmp14.repack26, align 4
  %tmp14.repack27 = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  store i32 3, ptr %tmp14.repack27, align 4
  %ptr.i16 = insertvalue { ptr, i64 } undef, ptr %tmp14, 0
  %len.i17 = insertvalue { ptr, i64 } %ptr.i16, i64 3, 1
  %fun_call_tmp6 = call %"Vec<T = i32>" @"Vec<T = i32>:from"({ ptr, i64 } %len.i17)
  %fun_call_tmp7 = call {} @"Vec<T = Vec<T = i32>>:push"(ptr nonnull %vec, %"Vec<T = i32>" %fun_call_tmp6)
  store i32 1, ptr %tmp18, align 4
  %tmp18.repack28 = getelementptr inbounds nuw i8, ptr %tmp18, i64 4
  store i32 2, ptr %tmp18.repack28, align 4
  %tmp18.repack29 = getelementptr inbounds nuw i8, ptr %tmp18, i64 8
  store i32 3, ptr %tmp18.repack29, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp18, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 3, 1
  %fun_call_tmp10 = call %"Vec<T = i32>" @"Vec<T = i32>:from"({ ptr, i64 } %len.i)
  %fun_call_tmp11 = call {} @"Vec<T = Vec<T = i32>>:push"(ptr nonnull %vec, %"Vec<T = i32>" %fun_call_tmp10)
  %fun_call_tmp12 = call {} @"Vec<T = Vec<T = i32>>:drop"(ptr nonnull %vec)
  %fun_call_tmp13 = call %"Box<T = i32>" @"Box<T = i32>:new"(i32 4)
  %0 = extractvalue %"Box<T = i32>" %fun_call_tmp13, 0
  store ptr %0, ptr %box, align 8
  %fun_call_tmp14 = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp14, align 4
  %fun_call_tmp15 = call {} @"Box<T = i32>:drop"(ptr nonnull %box)
  ret i64 %tmp
}

define %"Vec<T = Vec<T = i32>>" @"Vec<T = Vec<T = i32>>:new"() {
entry:
  %fun_call_tmp2 = call ptr @"mem:heap_alloc<T = Vec<T = i32>>"(i64 96)
  %tmp_agg = insertvalue %"Vec<T = Vec<T = i32>>" undef, ptr %fun_call_tmp2, 0
  %tmp_agg3 = insertvalue %"Vec<T = Vec<T = i32>>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = Vec<T = i32>>" %tmp_agg3, i64 4, 2
  ret %"Vec<T = Vec<T = i32>>" %tmp_constructor
}

define ptr @"mem:heap_alloc<T = Vec<T = i32>>"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call ptr @malloc(i64 %0)
  ret ptr %fun_call_tmp4
}

define ptr @"mem:heap_count"() {
entry:
  ret ptr @count
}

declare ptr @malloc(i64)

define {} @"Vec<T = Vec<T = i32>>:push"(ptr %0, %"Vec<T = i32>" %1) {
entry:
  %ptr = alloca ptr, align 8
  %type_mem_size = alloca i64, align 8
  %value = alloca %"Vec<T = i32>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"Vec<T = i32>" %1, 0
  store ptr %.elt, ptr %value, align 8
  %value.repack48 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt49 = extractvalue %"Vec<T = i32>" %1, 1
  store i64 %.elt49, ptr %value.repack48, align 8
  %value.repack50 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt51 = extractvalue %"Vec<T = i32>" %1, 2
  store i64 %.elt51, ptr %value.repack50, align 8
  store i64 24, ptr %type_mem_size, align 4
  %tmp = load ptr, ptr %self, align 8
  %tmp.len = getelementptr inbounds nuw i8, ptr %tmp, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp2.capacity = getelementptr inbounds nuw i8, ptr %tmp, i64 16
  %tmp3 = load i64, ptr %tmp2.capacity, align 4
  %tmp.i47 = icmp eq i64 %tmp1, %tmp3
  br i1 %tmp.i47, label %then, label %merge

merge:                                            ; preds = %entry, %then
  %tmp27 = load ptr, ptr %self, align 8
  %tmp28 = load ptr, ptr %tmp27, align 8
  %cast = ptrtoint ptr %tmp28 to i64
  %tmp29.len = getelementptr inbounds nuw i8, ptr %tmp27, i64 8
  %tmp30 = load i64, ptr %tmp29.len, align 4
  %tmp31 = load i64, ptr %type_mem_size, align 4
  %tmp.i43 = mul i64 %tmp30, %tmp31
  %tmp.i42 = add i64 %tmp.i43, %cast
  %cast34 = inttoptr i64 %tmp.i42 to ptr
  %fun_call_tmp36 = call {} @memcpy(ptr %cast34, ptr nonnull %value, i64 %tmp31)
  %tmp37 = load ptr, ptr %self, align 8
  %tmp37.len = getelementptr inbounds nuw i8, ptr %tmp37, i64 8
  %tmp38 = load i64, ptr %tmp37.len, align 4
  %tmp.i = add i64 %tmp38, 1
  %tmp40.len = getelementptr inbounds nuw i8, ptr %tmp37, i64 8
  store i64 %tmp.i, ptr %tmp40.len, align 4
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp5 = load ptr, ptr %self, align 8
  %tmp5.capacity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  %tmp6 = load i64, ptr %tmp5.capacity, align 4
  %tmp.i46 = shl i64 %tmp6, 1
  %tmp8.capacity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  store i64 %tmp.i46, ptr %tmp8.capacity, align 4
  %tmp11 = load i64, ptr %type_mem_size, align 4
  %tmp.i45 = mul i64 %tmp.i46, %tmp11
  %fun_call_tmp13 = call ptr @"mem:heap_alloc<T = Vec<T = i32>>"(i64 %tmp.i45)
  store ptr %fun_call_tmp13, ptr %ptr, align 8
  %tmp14 = load ptr, ptr %self, align 8
  %tmp15.len = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  %tmp16 = load i64, ptr %tmp15.len, align 4
  %tmp17 = load i64, ptr %type_mem_size, align 4
  %tmp.i44 = mul i64 %tmp16, %tmp17
  %tmp20 = load ptr, ptr %tmp14, align 8
  %fun_call_tmp21 = call {} @"mem:copy<T = Vec<T = i32>>"(ptr %fun_call_tmp13, ptr %tmp20, i64 %tmp.i44)
  %tmp22 = load ptr, ptr %self, align 8
  %tmp23 = load ptr, ptr %tmp22, align 8
  %fun_call_tmp24 = call {} @"mem:heap_free<T = Vec<T = i32>>"(ptr %tmp23)
  %tmp26 = load ptr, ptr %ptr, align 8
  store ptr %tmp26, ptr %tmp22, align 8
  br label %merge
}

define {} @"mem:copy<T = Vec<T = i32>>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp = call {} @memcpy(ptr %0, ptr %1, i64 %2)
  ret {} zeroinitializer
}

declare {} @memcpy(ptr, ptr, i64)

define {} @"mem:heap_free<T = Vec<T = i32>>"(ptr %0) {
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

define %"Vec<T = i32>" @"Vec<T = i32>:from"({ ptr, i64 } %0) {
entry:
  %idx = alloca i64, align 8
  %vec = alloca %"Vec<T = i32>", align 8
  %slice = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack13 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt14 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt14, ptr %slice.repack13, align 8
  %fun_call_tmp = call %"Vec<T = i32>" @"Vec<T = i32>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = i32>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %vec, align 8
  %vec.repack15 = getelementptr inbounds nuw i8, ptr %vec, i64 8
  %fun_call_tmp.elt16 = extractvalue %"Vec<T = i32>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt16, ptr %vec.repack15, align 8
  %vec.repack17 = getelementptr inbounds nuw i8, ptr %vec, i64 16
  %fun_call_tmp.elt18 = extractvalue %"Vec<T = i32>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt18, ptr %vec.repack17, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp4.unpack = load ptr, ptr %slice, align 8
  %tmp5 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i32, ptr %tmp4.unpack, i64 %tmp5
  %tmp7 = load i32, ptr %tmp_index.i, align 4
  %fun_call_tmp8 = call {} @"Vec<T = i32>:push"(ptr nonnull %vec, i32 %tmp7)
  %tmp.i = add i64 %tmp5, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1.elt19 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp1.unpack20 = load i64, ptr %tmp1.elt19, align 8
  %tmp.i12 = icmp ult i64 %storemerge, %tmp1.unpack20
  br i1 %tmp.i12, label %then, label %else

else:                                             ; preds = %cond
  %tmp11.unpack = load ptr, ptr %vec, align 8
  %1 = insertvalue %"Vec<T = i32>" poison, ptr %tmp11.unpack, 0
  %tmp11.elt22 = getelementptr inbounds nuw i8, ptr %vec, i64 8
  %tmp11.unpack23 = load i64, ptr %tmp11.elt22, align 8
  %2 = insertvalue %"Vec<T = i32>" %1, i64 %tmp11.unpack23, 1
  %tmp11.elt24 = getelementptr inbounds nuw i8, ptr %vec, i64 16
  %tmp11.unpack25 = load i64, ptr %tmp11.elt24, align 8
  %tmp1126 = insertvalue %"Vec<T = i32>" %2, i64 %tmp11.unpack25, 2
  ret %"Vec<T = i32>" %tmp1126
}

define %"Vec<T = i32>" @"Vec<T = i32>:new"() {
entry:
  %fun_call_tmp2 = call ptr @"mem:heap_alloc<T = i32>"(i64 16)
  %tmp_agg = insertvalue %"Vec<T = i32>" undef, ptr %fun_call_tmp2, 0
  %tmp_agg3 = insertvalue %"Vec<T = i32>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = i32>" %tmp_agg3, i64 4, 2
  ret %"Vec<T = i32>" %tmp_constructor
}

define ptr @"mem:heap_alloc<T = i32>"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call ptr @malloc(i64 %0)
  ret ptr %fun_call_tmp4
}

define {} @"Vec<T = i32>:push"(ptr %0, i32 %1) {
entry:
  %ptr = alloca ptr, align 8
  %type_mem_size = alloca i64, align 8
  %value = alloca i32, align 4
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i32 %1, ptr %value, align 4
  store i64 4, ptr %type_mem_size, align 4
  %tmp = load ptr, ptr %self, align 8
  %tmp.len = getelementptr inbounds nuw i8, ptr %tmp, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp2.capacity = getelementptr inbounds nuw i8, ptr %tmp, i64 16
  %tmp3 = load i64, ptr %tmp2.capacity, align 4
  %tmp.i47 = icmp eq i64 %tmp1, %tmp3
  br i1 %tmp.i47, label %then, label %merge

merge:                                            ; preds = %entry, %then
  %tmp27 = load ptr, ptr %self, align 8
  %tmp28 = load ptr, ptr %tmp27, align 8
  %cast = ptrtoint ptr %tmp28 to i64
  %tmp29.len = getelementptr inbounds nuw i8, ptr %tmp27, i64 8
  %tmp30 = load i64, ptr %tmp29.len, align 4
  %tmp31 = load i64, ptr %type_mem_size, align 4
  %tmp.i43 = mul i64 %tmp30, %tmp31
  %tmp.i42 = add i64 %tmp.i43, %cast
  %cast34 = inttoptr i64 %tmp.i42 to ptr
  %fun_call_tmp36 = call {} @memcpy(ptr %cast34, ptr nonnull %value, i64 %tmp31)
  %tmp37 = load ptr, ptr %self, align 8
  %tmp37.len = getelementptr inbounds nuw i8, ptr %tmp37, i64 8
  %tmp38 = load i64, ptr %tmp37.len, align 4
  %tmp.i = add i64 %tmp38, 1
  %tmp40.len = getelementptr inbounds nuw i8, ptr %tmp37, i64 8
  store i64 %tmp.i, ptr %tmp40.len, align 4
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp5 = load ptr, ptr %self, align 8
  %tmp5.capacity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  %tmp6 = load i64, ptr %tmp5.capacity, align 4
  %tmp.i46 = shl i64 %tmp6, 1
  %tmp8.capacity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  store i64 %tmp.i46, ptr %tmp8.capacity, align 4
  %tmp11 = load i64, ptr %type_mem_size, align 4
  %tmp.i45 = mul i64 %tmp.i46, %tmp11
  %fun_call_tmp13 = call ptr @"mem:heap_alloc<T = i32>"(i64 %tmp.i45)
  store ptr %fun_call_tmp13, ptr %ptr, align 8
  %tmp14 = load ptr, ptr %self, align 8
  %tmp15.len = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  %tmp16 = load i64, ptr %tmp15.len, align 4
  %tmp17 = load i64, ptr %type_mem_size, align 4
  %tmp.i44 = mul i64 %tmp16, %tmp17
  %tmp20 = load ptr, ptr %tmp14, align 8
  %fun_call_tmp21 = call {} @"mem:copy<T = i32>"(ptr %fun_call_tmp13, ptr %tmp20, i64 %tmp.i44)
  %tmp22 = load ptr, ptr %self, align 8
  %tmp23 = load ptr, ptr %tmp22, align 8
  %fun_call_tmp24 = call {} @"mem:heap_free<T = i32>"(ptr %tmp23)
  %tmp26 = load ptr, ptr %ptr, align 8
  store ptr %tmp26, ptr %tmp22, align 8
  br label %merge
}

define {} @"mem:copy<T = i32>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp = call {} @memcpy(ptr %0, ptr %1, i64 %2)
  ret {} zeroinitializer
}

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

define {} @"Vec<T = Vec<T = i32>>:drop"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp4 = load ptr, ptr %self, align 8
  %fun_call_tmp5 = call { ptr, i64 } @"Vec<T = Vec<T = i32>>:deref"(ptr %tmp4)
  %tmp6 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp5, 0
  %tmp_index.i = getelementptr %"Vec<T = i32>", ptr %ref.i, i64 %tmp6
  %fun_call_tmp8 = call {} @"Vec<T = i32>:drop"(ptr %tmp_index.i)
  %tmp.i = add i64 %tmp6, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1 = load ptr, ptr %self, align 8
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Vec<T = i32>>:deref"(ptr %tmp1)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i14 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i14, label %then, label %else

else:                                             ; preds = %cond
  %tmp11 = load ptr, ptr %self, align 8
  %tmp12 = load ptr, ptr %tmp11, align 8
  %fun_call_tmp13 = call {} @"mem:heap_free<T = Vec<T = i32>>"(ptr %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"Vec<T = Vec<T = i32>>:deref_mut"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define { ptr, i64 } @"Vec<T = Vec<T = i32>>:deref"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
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

define { ptr, i64 } @"Vec<T = i32>:deref_mut"(ptr %0) {
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
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define {} @"i32:drop"(ptr %0) {
entry:
  ret {} zeroinitializer
}

define %"Box<T = i32>" @"Box<T = i32>:new"(i32 %0) {
entry:
  %value = alloca i32, align 4
  store i32 %0, ptr %value, align 4
  %fun_call_tmp1 = call ptr @"mem:heap_alloc<T = i32>"(i64 4)
  %fun_call_tmp4 = call {} @"mem:copy<T = i32>"(ptr %fun_call_tmp1, ptr nonnull %value, i64 4)
  %tmp_constructor = insertvalue %"Box<T = i32>" undef, ptr %fun_call_tmp1, 0
  ret %"Box<T = i32>" %tmp_constructor
}

define {} @"Box<T = i32>:drop"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %fun_call_tmp = call {} @"mem:heap_free<T = i32>"(ptr %tmp1)
  ret {} zeroinitializer
}
