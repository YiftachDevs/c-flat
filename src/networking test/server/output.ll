; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:Result<T = TCP_Listener>" = type { %"[c-flat]:TCP_Listener", %"[c-flat]:Error", i1 }
%"[c-flat]:TCP_Listener" = type { i32, i32, i32 }
%"[c-flat]:Error" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }

@"[c-flat]:count" = internal global i64 0

define i32 @main() {
entry:
  %fun_call_tmp = call %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:TCP_Listener:listen"(i32 8080, i32 1)
  %fun_call_tmp1 = call %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:TCP_Listener:listen"(i32 8080, i32 2)
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp2, align 4
  %cast = trunc i64 %tmp to i32
  %fun_call_tmp4 = call {} @"[c-flat]:Result<T = TCP_Listener>:drop"(%"[c-flat]:Result<T = TCP_Listener>" %fun_call_tmp1)
  %fun_call_tmp6 = call {} @"[c-flat]:Result<T = TCP_Listener>:drop"(%"[c-flat]:Result<T = TCP_Listener>" %fun_call_tmp)
  ret i32 %cast
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

define %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:TCP_Listener:listen"(i32 %0, i32 %1) {
entry:
  %tmp3 = alloca i32, align 4
  %socket = alloca i32, align 4
  %backlog = alloca i32, align 4
  %port = alloca i32, align 4
  store i32 %0, ptr %port, align 4
  store i32 %1, ptr %backlog, align 4
  %fun_call_tmp = call i32 @c_listen(i32 %0, i32 %1)
  store i32 %fun_call_tmp, ptr %socket, align 4
  store i32 -1, ptr %tmp3, align 4
  %tmp.i = load i32, ptr %socket, align 4
  %tmp2.i = icmp eq i32 %tmp.i, -1
  br i1 %tmp2.i, label %then, label %then6

merge:                                            ; preds = %then6, %then
  %result = phi %"[c-flat]:Result<T = TCP_Listener>" [ %fun_call_tmp5, %then ], [ %fun_call_tmp11, %then6 ]
  %tmp12 = load i32, ptr %tmp3, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:i32:drop"(i32 %tmp12)
  %tmp14 = load i32, ptr %socket, align 4
  %fun_call_tmp15 = call {} @"[c-flat]:i32:drop"(i32 %tmp14)
  %tmp16 = load i32, ptr %backlog, align 4
  %fun_call_tmp17 = call {} @"[c-flat]:i32:drop"(i32 %tmp16)
  %tmp18 = load i32, ptr %port, align 4
  %fun_call_tmp19 = call {} @"[c-flat]:i32:drop"(i32 %tmp18)
  ret %"[c-flat]:Result<T = TCP_Listener>" %result

then:                                             ; preds = %entry
  %fun_call_tmp4 = call %"[c-flat]:Error" @"[c-flat]:TCP_Listener:c_err"()
  %fun_call_tmp5 = call %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:Result<T = TCP_Listener>:err"(%"[c-flat]:Error" %fun_call_tmp4)
  br label %merge

then6:                                            ; preds = %entry
  %tmp7 = load i32, ptr %port, align 4
  %tmp8 = load i32, ptr %backlog, align 4
  %tmp9 = load i32, ptr %socket, align 4
  %tmp_agg = insertvalue %"[c-flat]:TCP_Listener" undef, i32 %tmp7, 0
  %tmp_agg10 = insertvalue %"[c-flat]:TCP_Listener" %tmp_agg, i32 %tmp8, 1
  %tmp_constructor = insertvalue %"[c-flat]:TCP_Listener" %tmp_agg10, i32 %tmp9, 2
  %fun_call_tmp11 = call %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:Result<T = TCP_Listener>:ok"(%"[c-flat]:TCP_Listener" %tmp_constructor)
  br label %merge
}

declare i32 @c_listen(i32, i32)

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

define {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %0) {
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

define i1 @"[c-flat]:bool:clone"(ptr %0) {
entry:
  %tmp1 = load i1, ptr %0, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:Result<T = TCP_Listener>:err"(%"[c-flat]:Error" %0) {
entry:
  %err = alloca %"[c-flat]:Error", align 8
  %1 = extractvalue %"[c-flat]:Error" %0, 0
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %1, 0
  store ptr %.elt, ptr %err, align 8
  %err.repack2 = getelementptr inbounds nuw i8, ptr %err, i64 8
  %.elt3 = extractvalue %"[c-flat]:Vec<T = char>" %1, 1
  store i64 %.elt3, ptr %err.repack2, align 8
  %err.repack4 = getelementptr inbounds nuw i8, ptr %err, i64 16
  %.elt5 = extractvalue %"[c-flat]:Vec<T = char>" %1, 2
  store i64 %.elt5, ptr %err.repack4, align 8
  %tmp.unpack.unpack = load ptr, ptr %err, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt7 = getelementptr inbounds nuw i8, ptr %err, i64 8
  %tmp.unpack.unpack8 = load i64, ptr %tmp.unpack.elt7, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack8, 1
  %tmp.unpack.elt9 = getelementptr inbounds nuw i8, ptr %err, i64 16
  %tmp.unpack.unpack10 = load i64, ptr %tmp.unpack.elt9, align 8
  %tmp.unpack11 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp.unpack.unpack10, 2
  %tmp6 = insertvalue %"[c-flat]:Error" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack11, 0
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = TCP_Listener>" { %"[c-flat]:TCP_Listener" zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = TCP_Listener>" %tmp_agg1, i1 false, 2
  ret %"[c-flat]:Result<T = TCP_Listener>" %tmp_constructor
}

define %"[c-flat]:Error" @"[c-flat]:TCP_Listener:c_err"() {
entry:
  %fun_call_tmp = call i32 @c_errno()
  %fun_call_tmp1 = call ptr @c_strerror(i32 %fun_call_tmp)
  %fun_call_tmp2 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from_c_str"(ptr %fun_call_tmp1)
  %tmp_constructor = insertvalue %"[c-flat]:Error" undef, %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 0
  ret %"[c-flat]:Error" %tmp_constructor
}

declare ptr @c_strerror(i32)

declare i32 @c_errno()

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from_c_str"(ptr %0) {
entry:
  %fun_call_tmp = call i64 @c_strlen(ptr %0)
  %1 = insertvalue { ptr, i64 } poison, ptr %0, 0
  %tmp417 = insertvalue { ptr, i64 } %1, i64 %fun_call_tmp, 1
  %fun_call_tmp5 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %tmp417)
  %fun_call_tmp9 = call {} @"[c-flat]:u64:drop"(i64 %fun_call_tmp)
  ret %"[c-flat]:Vec<T = char>" %fun_call_tmp5
}

declare i64 @c_strlen(ptr)

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

define %"[c-flat]:Result<T = TCP_Listener>" @"[c-flat]:Result<T = TCP_Listener>:ok"(%"[c-flat]:TCP_Listener" %0) {
entry:
  %value = alloca %"[c-flat]:TCP_Listener", align 8
  %.elt = extractvalue %"[c-flat]:TCP_Listener" %0, 0
  store i32 %.elt, ptr %value, align 4
  %value.repack2 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %.elt3 = extractvalue %"[c-flat]:TCP_Listener" %0, 1
  store i32 %.elt3, ptr %value.repack2, align 4
  %value.repack4 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt5 = extractvalue %"[c-flat]:TCP_Listener" %0, 2
  store i32 %.elt5, ptr %value.repack4, align 4
  %tmp.unpack = load i32, ptr %value, align 4
  %1 = insertvalue %"[c-flat]:TCP_Listener" poison, i32 %tmp.unpack, 0
  %tmp.elt6 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp.unpack7 = load i32, ptr %tmp.elt6, align 4
  %2 = insertvalue %"[c-flat]:TCP_Listener" %1, i32 %tmp.unpack7, 1
  %tmp.elt8 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp.unpack9 = load i32, ptr %tmp.elt8, align 4
  %tmp10 = insertvalue %"[c-flat]:TCP_Listener" %2, i32 %tmp.unpack9, 2
  %tmp_agg = insertvalue %"[c-flat]:Result<T = TCP_Listener>" undef, %"[c-flat]:TCP_Listener" %tmp10, 0
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = TCP_Listener>" %tmp_agg, %"[c-flat]:Error" zeroinitializer, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = TCP_Listener>" %tmp_agg1, i1 true, 2
  ret %"[c-flat]:Result<T = TCP_Listener>" %tmp_constructor
}

define {} @"[c-flat]:Result<T = TCP_Listener>:drop"(%"[c-flat]:Result<T = TCP_Listener>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = TCP_Listener>", align 8
  store %"[c-flat]:Result<T = TCP_Listener>" %0, ptr %self, align 8
  %self.is_ok = getelementptr inbounds nuw i8, ptr %self, i64 40
  %tmp = load i1, ptr %self.is_ok, align 1
  br i1 %tmp, label %then, label %then2

merge:                                            ; preds = %then2, %then
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp1.unpack = load i32, ptr %self, align 4
  %tmp1.elt14 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %tmp1.unpack15 = load i32, ptr %tmp1.elt14, align 4
  %tmp1.elt16 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp1.unpack17 = load i32, ptr %tmp1.elt16, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp1.unpack17)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp1.unpack15)
  %fun_call_tmp4.i = call {} @"[c-flat]:i32:drop"(i32 %tmp1.unpack)
  br label %merge

then2:                                            ; preds = %entry
  %self.err = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp3.unpack.unpack = load ptr, ptr %self.err, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp3.unpack.unpack, 0
  %tmp3.unpack.elt9 = getelementptr inbounds nuw i8, ptr %self, i64 24
  %tmp3.unpack.unpack10 = load i64, ptr %tmp3.unpack.elt9, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp3.unpack.unpack10, 1
  %tmp3.unpack.elt11 = getelementptr inbounds nuw i8, ptr %self, i64 32
  %tmp3.unpack.unpack12 = load i64, ptr %tmp3.unpack.elt11, align 8
  %tmp3.unpack13 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp3.unpack.unpack12, 2
  %fun_call_tmp.i7 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp3.unpack13)
  br label %merge
}
