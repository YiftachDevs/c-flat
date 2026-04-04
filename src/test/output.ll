; ModuleID = 'main_module'
source_filename = "main_module"

%String = type { %"Vec<T = u8>" }
%"Vec<T = u8>" = type { ptr, i64, i64 }

@global_string = private unnamed_addr constant [13 x i8] c"Hello world!\00", align 1

define i32 @main() {
entry:
  %name = alloca %String, align 8
  %fun_call_tmp = call %String @"[u8]:to_string"({ ptr, i64 } { ptr @global_string, i64 12 })
  %0 = extractvalue %String %fun_call_tmp, 0
  %.elt = extractvalue %"Vec<T = u8>" %0, 0
  store ptr %.elt, ptr %name, align 8
  %name.repack6 = getelementptr inbounds nuw i8, ptr %name, i64 8
  %.elt7 = extractvalue %"Vec<T = u8>" %0, 1
  store i64 %.elt7, ptr %name.repack6, align 8
  %name.repack8 = getelementptr inbounds nuw i8, ptr %name, i64 16
  %.elt9 = extractvalue %"Vec<T = u8>" %0, 2
  store i64 %.elt9, ptr %name.repack8, align 8
  %fun_call_tmp1 = call ptr @"String:deref_mut"(ptr nonnull %name)
  %fun_call_tmp2 = call {} @"Vec<T = u8>:push"(ptr %fun_call_tmp1, i8 65)
  %fun_call_tmp3 = call ptr @"String:deref_mut"(ptr nonnull %name)
  %fun_call_tmp4 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr %fun_call_tmp3)
  %fun_call_tmp5 = call {} @print({ ptr, i64 } %fun_call_tmp4)
  ret i32 0
}

define %String @"[u8]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %String @"String:from"({ ptr, i64 } %0)
  ret %String %fun_call_tmp
}

define %String @"String:from"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"Vec<T = u8>" @"Vec<T = u8>:from"({ ptr, i64 } %0)
  %tmp_constructor = insertvalue %String undef, %"Vec<T = u8>" %fun_call_tmp, 0
  ret %String %tmp_constructor
}

define %"Vec<T = u8>" @"Vec<T = u8>:from"({ ptr, i64 } %0) {
entry:
  %idx = alloca i64, align 8
  %vec = alloca %"Vec<T = u8>", align 8
  %slice = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack13 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt14 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt14, ptr %slice.repack13, align 8
  %fun_call_tmp = call %"Vec<T = u8>" @"Vec<T = u8>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %vec, align 8
  %vec.repack15 = getelementptr inbounds nuw i8, ptr %vec, i64 8
  %fun_call_tmp.elt16 = extractvalue %"Vec<T = u8>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt16, ptr %vec.repack15, align 8
  %vec.repack17 = getelementptr inbounds nuw i8, ptr %vec, i64 16
  %fun_call_tmp.elt18 = extractvalue %"Vec<T = u8>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt18, ptr %vec.repack17, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp4.unpack = load ptr, ptr %slice, align 8
  %tmp5 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp4.unpack, i64 %tmp5
  %tmp7 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp8 = call {} @"Vec<T = u8>:push"(ptr nonnull %vec, i8 %tmp7)
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
  %1 = insertvalue %"Vec<T = u8>" poison, ptr %tmp11.unpack, 0
  %tmp11.elt22 = getelementptr inbounds nuw i8, ptr %vec, i64 8
  %tmp11.unpack23 = load i64, ptr %tmp11.elt22, align 8
  %2 = insertvalue %"Vec<T = u8>" %1, i64 %tmp11.unpack23, 1
  %tmp11.elt24 = getelementptr inbounds nuw i8, ptr %vec, i64 16
  %tmp11.unpack25 = load i64, ptr %tmp11.elt24, align 8
  %tmp1126 = insertvalue %"Vec<T = u8>" %2, i64 %tmp11.unpack25, 2
  ret %"Vec<T = u8>" %tmp1126
}

define %"Vec<T = u8>" @"Vec<T = u8>:new"() {
entry:
  %fun_call_tmp2 = call ptr @malloc(i64 4)
  %tmp_agg = insertvalue %"Vec<T = u8>" undef, ptr %fun_call_tmp2, 0
  %tmp_agg3 = insertvalue %"Vec<T = u8>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = u8>" %tmp_agg3, i64 4, 2
  ret %"Vec<T = u8>" %tmp_constructor
}

declare ptr @malloc(i64)

define {} @"Vec<T = u8>:push"(ptr %0, i8 %1) {
entry:
  %ptr = alloca ptr, align 8
  %type_mem_size = alloca i64, align 8
  %value = alloca i8, align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 %1, ptr %value, align 1
  store i64 1, ptr %type_mem_size, align 4
  %tmp = load ptr, ptr %self, align 8
  %tmp.len = getelementptr inbounds nuw i8, ptr %tmp, i64 8
  %tmp1 = load i64, ptr %tmp.len, align 4
  %tmp2.capacity = getelementptr inbounds nuw i8, ptr %tmp, i64 16
  %tmp3 = load i64, ptr %tmp2.capacity, align 4
  %tmp.i46 = icmp eq i64 %tmp1, %tmp3
  br i1 %tmp.i46, label %then, label %merge

merge:                                            ; preds = %entry, %then
  %tmp27 = load ptr, ptr %self, align 8
  %tmp28 = load ptr, ptr %tmp27, align 8
  %cast = ptrtoint ptr %tmp28 to i64
  %tmp29.len = getelementptr inbounds nuw i8, ptr %tmp27, i64 8
  %tmp30 = load i64, ptr %tmp29.len, align 4
  %tmp31 = load i64, ptr %type_mem_size, align 4
  %tmp.i42 = mul i64 %tmp30, %tmp31
  %tmp.i41 = add i64 %tmp.i42, %cast
  %cast34 = inttoptr i64 %tmp.i41 to ptr
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
  %tmp.i45 = shl i64 %tmp6, 1
  %tmp8.capacity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  store i64 %tmp.i45, ptr %tmp8.capacity, align 4
  %tmp11 = load i64, ptr %type_mem_size, align 4
  %tmp.i44 = mul i64 %tmp.i45, %tmp11
  %fun_call_tmp13 = call ptr @malloc(i64 %tmp.i44)
  store ptr %fun_call_tmp13, ptr %ptr, align 8
  %tmp14 = load ptr, ptr %self, align 8
  %tmp15 = load ptr, ptr %tmp14, align 8
  %tmp16.len = getelementptr inbounds nuw i8, ptr %tmp14, i64 8
  %tmp17 = load i64, ptr %tmp16.len, align 4
  %tmp18 = load i64, ptr %type_mem_size, align 4
  %tmp.i43 = mul i64 %tmp17, %tmp18
  %tmp20 = load ptr, ptr %ptr, align 8
  %fun_call_tmp21 = call {} @memcpy(ptr %tmp20, ptr %tmp15, i64 %tmp.i43)
  %tmp22 = load ptr, ptr %self, align 8
  %tmp23 = load ptr, ptr %tmp22, align 8
  %fun_call_tmp24 = call {} @free(ptr %tmp23)
  store ptr %tmp20, ptr %tmp22, align 8
  br label %merge
}

declare {} @memcpy(ptr, ptr, i64)

declare {} @free(ptr)

define ptr @"String:deref_mut"(ptr %0) {
entry:
  ret ptr %0
}

define {} @print({ ptr, i64 } %0) {
entry:
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %fun_call_tmp1 = call {} @puts(ptr %ref.i)
  ret {} zeroinitializer
}

declare {} @puts(ptr)

define { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}
