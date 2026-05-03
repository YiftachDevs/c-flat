; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:Result<T = ()>" = type { {}, %"[c-flat]:Error", i1 }
%"[c-flat]:Error" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:TCP_Socket" = type { %"[c-flat]:Vec<T = char>", i32, i32 }
%"[c-flat]:Result<T = Error>" = type { %"[c-flat]:TCP_Socket", %"[c-flat]:Error", i1 }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [14 x i8] c"Connecting...\00", align 1
@global_string.27 = private unnamed_addr constant [10 x i8] c"127.0.0.1\00", align 1
@global_string.28 = private unnamed_addr constant [11 x i8] c"connected!\00", align 1
@global_string.29 = private unnamed_addr constant [7 x i8] c"hello!\00", align 1
@global_string.30 = private unnamed_addr constant [6 x i8] c"done.\00", align 1
@global_string.31 = private unnamed_addr constant [13 x i8] c"heap count: \00", align 1
@global_string.32 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i32 @main() {
entry:
  %tmp2 = alloca %"[c-flat]:Vec<T = char>", align 8
  %res = alloca i32, align 4
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %result = alloca %"[c-flat]:Result<T = ()>", align 8
  %fun_call_tmp = call %"[c-flat]:Result<T = ()>" @"[c-flat]:program"()
  store %"[c-flat]:Result<T = ()>" %fun_call_tmp, ptr %result, align 8
  %fun_call_tmp1 = call i1 @"[c-flat]:Result<T = ()>:is_ok"(ptr nonnull %result)
  br i1 %fun_call_tmp1, label %merge, label %then2

merge:                                            ; preds = %entry, %then2
  %result9 = phi i32 [ -1, %then2 ], [ 0, %entry ]
  store i32 %result9, ptr %res, align 4
  %fun_call_tmp10 = call {} @"[c-flat]:print"({ ptr, i64 } { ptr @global_string.31, i64 12 })
  %fun_call_tmp11 = call ptr @"[c-flat]:mem:heap_count"()
  %fun_call_tmp12 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:u64:to_string"(ptr %fun_call_tmp11)
  %fun_call_tmp12.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp12, 0
  store ptr %fun_call_tmp12.elt, ptr %tmp2, align 8
  %tmp2.repack29 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %fun_call_tmp12.elt30 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp12, 1
  store i64 %fun_call_tmp12.elt30, ptr %tmp2.repack29, align 8
  %tmp2.repack31 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %fun_call_tmp12.elt32 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp12, 2
  store i64 %fun_call_tmp12.elt32, ptr %tmp2.repack31, align 8
  %fun_call_tmp13 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp2)
  %fun_call_tmp14 = call {} @"[c-flat]:println"({ ptr, i64 } %fun_call_tmp13)
  %tmp15 = load i32, ptr %res, align 4
  %tmp16.unpack = load ptr, ptr %tmp2, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp16.unpack, 0
  %tmp16.elt33 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp16.unpack34 = load i64, ptr %tmp16.elt33, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp16.unpack34, 1
  %tmp16.elt35 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %tmp16.unpack36 = load i64, ptr %tmp16.elt35, align 8
  %tmp1637 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp16.unpack36, 2
  %fun_call_tmp17 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp1637)
  %tmp18 = load i32, ptr %res, align 4
  %fun_call_tmp19 = call {} @"[c-flat]:i32:drop"(i32 %tmp18)
  ret i32 %tmp15

then2:                                            ; preds = %entry
  %tmp = load %"[c-flat]:Result<T = ()>", ptr %result, align 8
  %fun_call_tmp3 = call %"[c-flat]:Error" @"[c-flat]:Result<T = ()>:unwrap_err"(%"[c-flat]:Result<T = ()>" %tmp)
  %fun_call_tmp3.msg = extractvalue %"[c-flat]:Error" %fun_call_tmp3, 0
  %fun_call_tmp3.msg.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp3.msg, 0
  store ptr %fun_call_tmp3.msg.elt, ptr %tmp1, align 8
  %tmp1.repack20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp3.msg.elt21 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp3.msg, 1
  store i64 %fun_call_tmp3.msg.elt21, ptr %tmp1.repack20, align 8
  %tmp1.repack22 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp3.msg.elt23 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp3.msg, 2
  store i64 %fun_call_tmp3.msg.elt23, ptr %tmp1.repack22, align 8
  %fun_call_tmp4 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %fun_call_tmp5 = call {} @"[c-flat]:println"({ ptr, i64 } %fun_call_tmp4)
  %tmp7.unpack = load ptr, ptr %tmp1, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp7.unpack, 0
  %tmp7.elt24 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp7.unpack25 = load i64, ptr %tmp7.elt24, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp7.unpack25, 1
  %tmp7.elt26 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp7.unpack27 = load i64, ptr %tmp7.elt26, align 8
  %tmp728 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp7.unpack27, 2
  %fun_call_tmp8 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp728)
  br label %merge
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:program"() {
entry:
  %socket = alloca %"[c-flat]:TCP_Socket", align 8
  %tmp0 = alloca %"[c-flat]:Result<T = Error>", align 8
  %fun_call_tmp = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string, i64 13 })
  %fun_call_tmp1 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Error:connect"({ ptr, i64 } { ptr @global_string.27, i64 9 }, i32 8080)
  store %"[c-flat]:Result<T = Error>" %fun_call_tmp1, ptr %tmp0, align 8
  %fun_call_tmp2 = call i1 @"[c-flat]:Result<T = Error>:is_unwrappable"(ptr nonnull %tmp0)
  br i1 %fun_call_tmp2, label %then, label %else

common.ret:                                       ; preds = %else, %then
  %common.ret.op = phi %"[c-flat]:Result<T = ()>" [ %fun_call_tmp8, %then ], [ %fun_call_tmp3, %else ]
  ret %"[c-flat]:Result<T = ()>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp4 = call %"[c-flat]:TCP_Socket" @"[c-flat]:Result<T = Error>:unwrap"(%"[c-flat]:Result<T = Error>" %fun_call_tmp1)
  %fun_call_tmp4.elt = extractvalue %"[c-flat]:TCP_Socket" %fun_call_tmp4, 0
  %fun_call_tmp4.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp4.elt, 0
  store ptr %fun_call_tmp4.elt.elt, ptr %socket, align 8
  %socket.repack14 = getelementptr inbounds nuw i8, ptr %socket, i64 8
  %fun_call_tmp4.elt.elt15 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp4.elt, 1
  store i64 %fun_call_tmp4.elt.elt15, ptr %socket.repack14, align 8
  %socket.repack16 = getelementptr inbounds nuw i8, ptr %socket, i64 16
  %fun_call_tmp4.elt.elt17 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp4.elt, 2
  store i64 %fun_call_tmp4.elt.elt17, ptr %socket.repack16, align 8
  %socket.repack10 = getelementptr inbounds nuw i8, ptr %socket, i64 24
  %fun_call_tmp4.elt11 = extractvalue %"[c-flat]:TCP_Socket" %fun_call_tmp4, 1
  store i32 %fun_call_tmp4.elt11, ptr %socket.repack10, align 8
  %socket.repack12 = getelementptr inbounds nuw i8, ptr %socket, i64 28
  %fun_call_tmp4.elt13 = extractvalue %"[c-flat]:TCP_Socket" %fun_call_tmp4, 2
  store i32 %fun_call_tmp4.elt13, ptr %socket.repack12, align 4
  %fun_call_tmp5 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.28, i64 10 })
  %fun_call_tmp6 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Error:send_text"(ptr nonnull %socket, { ptr, i64 } { ptr @global_string.29, i64 6 })
  %fun_call_tmp7 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.30, i64 5 })
  %fun_call_tmp8 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:ok"({} zeroinitializer)
  %tmp.unpack.unpack = load ptr, ptr %socket, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt23 = getelementptr inbounds nuw i8, ptr %socket, i64 8
  %tmp.unpack.unpack24 = load i64, ptr %tmp.unpack.elt23, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp.unpack.unpack24, 1
  %tmp.unpack.elt25 = getelementptr inbounds nuw i8, ptr %socket, i64 16
  %tmp.unpack.unpack26 = load i64, ptr %tmp.unpack.elt25, align 8
  %tmp.unpack27 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack26, 2
  %tmp.elt18 = getelementptr inbounds nuw i8, ptr %socket, i64 24
  %tmp.unpack19 = load i32, ptr %tmp.elt18, align 8
  %tmp.elt20 = getelementptr inbounds nuw i8, ptr %socket, i64 28
  %tmp.unpack21 = load i32, ptr %tmp.elt20, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp.unpack21)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp.unpack19)
  %fun_call_tmp4.i = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp.unpack27)
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp3 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Error>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Error>" %fun_call_tmp1)
  br label %common.ret
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
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp624)
  ret {} zeroinitializer
}

declare {} @c_puts(ptr)

define %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0)
  ret %"[c-flat]:Vec<T = char>" %fun_call_tmp
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

define {} @"[c-flat]:u64:drop"(i64 %0) {
entry:
  ret {} zeroinitializer
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
  %tmp.i22 = icmp ult i64 %tmp, %tmp1.unpack27
  br i1 %tmp.i22, label %then, label %else

else:                                             ; preds = %cond
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

define i32 @"[c-flat]:i32:clone"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  ret i32 %tmp1
}

define {} @"[c-flat]:i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Error:connect"({ ptr, i64 } %0, i32 %1) {
entry:
  %tmp5 = alloca i32, align 4
  %socket = alloca i32, align 4
  %tmp3 = alloca %"[c-flat]:Vec<T = char>", align 8
  %ip_string = alloca %"[c-flat]:Vec<T = char>", align 8
  %port = alloca i32, align 4
  store i32 %1, ptr %port, align 4
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %ip_string, align 8
  %ip_string.repack47 = getelementptr inbounds nuw i8, ptr %ip_string, i64 8
  %fun_call_tmp.elt48 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt48, ptr %ip_string.repack47, align 8
  %ip_string.repack49 = getelementptr inbounds nuw i8, ptr %ip_string, i64 16
  %fun_call_tmp.elt50 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt50, ptr %ip_string.repack49, align 8
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:clone"(ptr nonnull %ip_string)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp3, align 8
  %tmp3.repack51 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %fun_call_tmp1.elt52 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt52, ptr %tmp3.repack51, align 8
  %tmp3.repack53 = getelementptr inbounds nuw i8, ptr %tmp3, i64 16
  %fun_call_tmp1.elt54 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt54, ptr %tmp3.repack53, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp3)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %tmp4 = load i32, ptr %port, align 4
  %fun_call_tmp5 = call i32 @c_connect(ptr %ref.i, i32 %tmp4)
  store i32 %fun_call_tmp5, ptr %socket, align 4
  store i32 -1, ptr %tmp5, align 4
  %tmp.i = load i32, ptr %socket, align 4
  %tmp2.i = icmp eq i32 %tmp.i, -1
  br i1 %tmp2.i, label %then, label %then22

merge:                                            ; preds = %then22, %then
  %result = phi %"[c-flat]:Result<T = Error>" [ %fun_call_tmp9, %then ], [ %fun_call_tmp27, %then22 ]
  ret %"[c-flat]:Result<T = Error>" %result

then:                                             ; preds = %entry
  %fun_call_tmp8 = call %"[c-flat]:Error" @"[c-flat]:Error:c_err"()
  %fun_call_tmp9 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:err"(%"[c-flat]:Error" %fun_call_tmp8)
  %tmp10 = load i32, ptr %tmp5, align 4
  %fun_call_tmp11 = call {} @"[c-flat]:i32:drop"(i32 %tmp10)
  %tmp12 = load i32, ptr %socket, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:i32:drop"(i32 %tmp12)
  %tmp14.unpack = load ptr, ptr %tmp3, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp14.unpack, 0
  %tmp14.elt65 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp14.unpack66 = load i64, ptr %tmp14.elt65, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp14.unpack66, 1
  %tmp14.elt67 = getelementptr inbounds nuw i8, ptr %tmp3, i64 16
  %tmp14.unpack68 = load i64, ptr %tmp14.elt67, align 8
  %tmp1469 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp14.unpack68, 2
  %fun_call_tmp15 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp1469)
  %tmp16.unpack = load ptr, ptr %ip_string, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp16.unpack, 0
  %tmp16.elt70 = getelementptr inbounds nuw i8, ptr %ip_string, i64 8
  %tmp16.unpack71 = load i64, ptr %tmp16.elt70, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp16.unpack71, 1
  %tmp16.elt72 = getelementptr inbounds nuw i8, ptr %ip_string, i64 16
  %tmp16.unpack73 = load i64, ptr %tmp16.elt72, align 8
  %tmp1674 = insertvalue %"[c-flat]:Vec<T = char>" %5, i64 %tmp16.unpack73, 2
  %fun_call_tmp17 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp1674)
  %tmp18 = load i32, ptr %port, align 4
  %fun_call_tmp19 = call {} @"[c-flat]:i32:drop"(i32 %tmp18)
  br label %merge

then22:                                           ; preds = %entry
  %tmp23.unpack = load ptr, ptr %ip_string, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp23.unpack, 0
  %tmp23.elt55 = getelementptr inbounds nuw i8, ptr %ip_string, i64 8
  %tmp23.unpack56 = load i64, ptr %tmp23.elt55, align 8
  %7 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp23.unpack56, 1
  %tmp23.elt57 = getelementptr inbounds nuw i8, ptr %ip_string, i64 16
  %tmp23.unpack58 = load i64, ptr %tmp23.elt57, align 8
  %tmp2359 = insertvalue %"[c-flat]:Vec<T = char>" %7, i64 %tmp23.unpack58, 2
  %tmp24 = load i32, ptr %port, align 4
  %tmp25 = load i32, ptr %socket, align 4
  %tmp_agg = insertvalue %"[c-flat]:TCP_Socket" undef, %"[c-flat]:Vec<T = char>" %tmp2359, 0
  %tmp_agg26 = insertvalue %"[c-flat]:TCP_Socket" %tmp_agg, i32 %tmp24, 1
  %tmp_constructor = insertvalue %"[c-flat]:TCP_Socket" %tmp_agg26, i32 %tmp25, 2
  %fun_call_tmp27 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:ok"(%"[c-flat]:TCP_Socket" %tmp_constructor)
  %tmp28 = load i32, ptr %tmp5, align 4
  %fun_call_tmp29 = call {} @"[c-flat]:i32:drop"(i32 %tmp28)
  %tmp30 = load i32, ptr %socket, align 4
  %fun_call_tmp31 = call {} @"[c-flat]:i32:drop"(i32 %tmp30)
  %tmp32.unpack = load ptr, ptr %tmp3, align 8
  %8 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp32.unpack, 0
  %tmp32.elt60 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp32.unpack61 = load i64, ptr %tmp32.elt60, align 8
  %9 = insertvalue %"[c-flat]:Vec<T = char>" %8, i64 %tmp32.unpack61, 1
  %tmp32.elt62 = getelementptr inbounds nuw i8, ptr %tmp3, i64 16
  %tmp32.unpack63 = load i64, ptr %tmp32.elt62, align 8
  %tmp3264 = insertvalue %"[c-flat]:Vec<T = char>" %9, i64 %tmp32.unpack63, 2
  %fun_call_tmp33 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp3264)
  %tmp34 = load i32, ptr %port, align 4
  %fun_call_tmp35 = call {} @"[c-flat]:i32:drop"(i32 %tmp34)
  br label %merge
}

declare i32 @c_connect(ptr, i32)

define i1 @"[c-flat]:bool:clone"(ptr %0) {
entry:
  %tmp1 = load i1, ptr %0, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:err"(%"[c-flat]:Error" %0) {
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
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Error>" { %"[c-flat]:TCP_Socket" zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Error>" %tmp_agg1, i1 false, 2
  ret %"[c-flat]:Result<T = Error>" %tmp_constructor
}

define %"[c-flat]:Error" @"[c-flat]:Error:c_err"() {
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

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:ok"(%"[c-flat]:TCP_Socket" %0) {
entry:
  %value = alloca %"[c-flat]:TCP_Socket", align 8
  %.elt = extractvalue %"[c-flat]:TCP_Socket" %0, 0
  %.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %.elt, 0
  store ptr %.elt.elt, ptr %value, align 8
  %value.repack6 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt.elt7 = extractvalue %"[c-flat]:Vec<T = char>" %.elt, 1
  store i64 %.elt.elt7, ptr %value.repack6, align 8
  %value.repack8 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt.elt9 = extractvalue %"[c-flat]:Vec<T = char>" %.elt, 2
  store i64 %.elt.elt9, ptr %value.repack8, align 8
  %value.repack2 = getelementptr inbounds nuw i8, ptr %value, i64 24
  %.elt3 = extractvalue %"[c-flat]:TCP_Socket" %0, 1
  store i32 %.elt3, ptr %value.repack2, align 8
  %value.repack4 = getelementptr inbounds nuw i8, ptr %value, i64 28
  %.elt5 = extractvalue %"[c-flat]:TCP_Socket" %0, 2
  store i32 %.elt5, ptr %value.repack4, align 4
  %tmp.unpack.unpack = load ptr, ptr %value, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt15 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp.unpack.unpack16 = load i64, ptr %tmp.unpack.elt15, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack16, 1
  %tmp.unpack.elt17 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %tmp.unpack.unpack18 = load i64, ptr %tmp.unpack.elt17, align 8
  %tmp.unpack19 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack18, 2
  %3 = insertvalue %"[c-flat]:TCP_Socket" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack19, 0
  %tmp.elt10 = getelementptr inbounds nuw i8, ptr %value, i64 24
  %tmp.unpack11 = load i32, ptr %tmp.elt10, align 8
  %4 = insertvalue %"[c-flat]:TCP_Socket" %3, i32 %tmp.unpack11, 1
  %tmp.elt12 = getelementptr inbounds nuw i8, ptr %value, i64 28
  %tmp.unpack13 = load i32, ptr %tmp.elt12, align 4
  %tmp14 = insertvalue %"[c-flat]:TCP_Socket" %4, i32 %tmp.unpack13, 2
  %tmp_agg = insertvalue %"[c-flat]:Result<T = Error>" undef, %"[c-flat]:TCP_Socket" %tmp14, 0
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Error>" %tmp_agg, %"[c-flat]:Error" zeroinitializer, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Error>" %tmp_agg1, i1 true, 2
  ret %"[c-flat]:Result<T = Error>" %tmp_constructor
}

define i1 @"[c-flat]:Result<T = Error>:is_unwrappable"(ptr %0) {
entry:
  %fun_call_tmp = call i1 @"[c-flat]:Result<T = Error>:is_ok"(ptr %0)
  ret i1 %fun_call_tmp
}

define i1 @"[c-flat]:Result<T = Error>:is_ok"(ptr %0) {
entry:
  %tmp.is_ok = getelementptr inbounds nuw i8, ptr %0, i64 56
  %tmp1 = load i1, ptr %tmp.is_ok, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Error>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Error" @"[c-flat]:Result<T = Error>:unwrap_err"(%"[c-flat]:Result<T = Error>" %0)
  %fun_call_tmp1 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:err"(%"[c-flat]:Error" %fun_call_tmp)
  ret %"[c-flat]:Result<T = ()>" %fun_call_tmp1
}

define {} @"[c-flat]:():clone"(ptr %0) {
entry:
  ret {} poison
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:err"(%"[c-flat]:Error" %0) {
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
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = ()>" { {} zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = ()>" %tmp_agg1, i1 false, 2
  ret %"[c-flat]:Result<T = ()>" %tmp_constructor
}

define %"[c-flat]:Error" @"[c-flat]:Result<T = Error>:unwrap_err"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Error>", align 8
  store %"[c-flat]:Result<T = Error>" %0, ptr %self, align 8
  %self.err = getelementptr inbounds nuw i8, ptr %self, i64 32
  %tmp.unpack.unpack = load ptr, ptr %self.err, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 40
  %tmp.unpack.unpack3 = load i64, ptr %tmp.unpack.elt2, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack3, 1
  %tmp.unpack.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 48
  %tmp.unpack.unpack5 = load i64, ptr %tmp.unpack.elt4, align 8
  %tmp.unpack6 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack5, 2
  %tmp1 = insertvalue %"[c-flat]:Error" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack6, 0
  ret %"[c-flat]:Error" %tmp1
}

define %"[c-flat]:TCP_Socket" @"[c-flat]:Result<T = Error>:unwrap"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Error>", align 8
  store %"[c-flat]:Result<T = Error>" %0, ptr %self, align 8
  %tmp.unpack.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt6 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack.unpack7 = load i64, ptr %tmp.unpack.elt6, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack7, 1
  %tmp.unpack.elt8 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack.unpack9 = load i64, ptr %tmp.unpack.elt8, align 8
  %tmp.unpack10 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack9, 2
  %3 = insertvalue %"[c-flat]:TCP_Socket" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack10, 0
  %tmp.elt1 = getelementptr inbounds nuw i8, ptr %self, i64 24
  %tmp.unpack2 = load i32, ptr %tmp.elt1, align 8
  %4 = insertvalue %"[c-flat]:TCP_Socket" %3, i32 %tmp.unpack2, 1
  %tmp.elt3 = getelementptr inbounds nuw i8, ptr %self, i64 28
  %tmp.unpack4 = load i32, ptr %tmp.elt3, align 4
  %tmp5 = insertvalue %"[c-flat]:TCP_Socket" %4, i32 %tmp.unpack4, 2
  ret %"[c-flat]:TCP_Socket" %tmp5
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Error:send_text"(ptr %0, { ptr, i64 } %1) {
entry:
  %fun_call_tmp = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Error:send"(ptr %0, { ptr, i64 } %1)
  ret %"[c-flat]:Result<T = ()>" %fun_call_tmp
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Error:send"(ptr %0, { ptr, i64 } %1) {
entry:
  %tmp420 = alloca i32, align 4
  %sent_len = alloca i32, align 4
  %idx = alloca i64, align 8
  %data = alloca { ptr, i64 }, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt, ptr %data, align 8
  %data.repack71 = getelementptr inbounds nuw i8, ptr %data, i64 8
  %.elt72 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt72, ptr %data.repack71, align 8
  store i64 0, ptr %idx, align 4
  br label %cond

common.ret:                                       ; preds = %then16, %else
  %common.ret.op = phi %"[c-flat]:Result<T = ()>" [ %fun_call_tmp43, %else ], [ %fun_call_tmp23, %then16 ]
  ret %"[c-flat]:Result<T = ()>" %common.ret.op

then:                                             ; preds = %cond
  %tmp3 = load ptr, ptr %self, align 8
  %tmp3.socket = getelementptr inbounds nuw i8, ptr %tmp3, i64 28
  %tmp4.unpack = load ptr, ptr %data, align 8
  %cast = ptrtoint ptr %tmp4.unpack to i64
  %tmp6 = load i64, ptr %idx, align 4
  %tmp.i52 = add i64 %tmp6, %cast
  %cast8 = inttoptr i64 %tmp.i52 to ptr
  %tmp9.elt79 = getelementptr inbounds nuw i8, ptr %data, i64 8
  %tmp9.unpack80 = load i64, ptr %tmp9.elt79, align 8
  %tmp.i55 = sub i64 %tmp9.unpack80, %tmp6
  %tmp13 = load i32, ptr %tmp3.socket, align 4
  %fun_call_tmp14 = call i32 @c_send(i32 %tmp13, ptr %cast8, i64 %tmp.i55)
  store i32 %fun_call_tmp14, ptr %sent_len, align 4
  store i32 -1, ptr %tmp420, align 4
  %tmp.i62 = load i32, ptr %sent_len, align 4
  %tmp2.i = icmp eq i32 %tmp.i62, -1
  br i1 %tmp2.i, label %then16, label %else18

cond:                                             ; preds = %else18, %entry
  %tmp = load i64, ptr %idx, align 4
  %tmp1.elt73 = getelementptr inbounds nuw i8, ptr %data, i64 8
  %tmp1.unpack74 = load i64, ptr %tmp1.elt73, align 8
  %tmp.i58 = icmp ult i64 %tmp, %tmp1.unpack74
  br i1 %tmp.i58, label %then, label %else

else:                                             ; preds = %cond
  %fun_call_tmp43 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:ok"({} zeroinitializer)
  %tmp44 = load i64, ptr %idx, align 4
  %fun_call_tmp45 = call {} @"[c-flat]:u64:drop"(i64 %tmp44)
  br label %common.ret

then16:                                           ; preds = %then
  %fun_call_tmp22 = call %"[c-flat]:Error" @"[c-flat]:Error:c_err"()
  %fun_call_tmp23 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:err"(%"[c-flat]:Error" %fun_call_tmp22)
  %tmp24 = load i32, ptr %tmp420, align 4
  %fun_call_tmp25 = call {} @"[c-flat]:i32:drop"(i32 %tmp24)
  %tmp26 = load i32, ptr %sent_len, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:i32:drop"(i32 %tmp26)
  %tmp28 = load i64, ptr %idx, align 4
  %fun_call_tmp29 = call {} @"[c-flat]:u64:drop"(i64 %tmp28)
  br label %common.ret

else18:                                           ; preds = %then
  %tmp34 = load i64, ptr %idx, align 4
  %tmp35 = load i32, ptr %sent_len, align 4
  %cast36 = sext i32 %tmp35 to i64
  %tmp.i = add i64 %tmp34, %cast36
  store i64 %tmp.i, ptr %idx, align 4
  %tmp38 = load i32, ptr %tmp420, align 4
  %fun_call_tmp39 = call {} @"[c-flat]:i32:drop"(i32 %tmp38)
  %fun_call_tmp41 = call {} @"[c-flat]:i32:drop"(i32 %tmp35)
  br label %cond
}

declare i32 @c_send(i32, ptr, i64)

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:ok"({} %0) {
entry:
  %fun_call_tmp3 = call {} @"[c-flat]:():drop"({} undef)
  ret %"[c-flat]:Result<T = ()>" { {} undef, %"[c-flat]:Error" zeroinitializer, i1 true }
}

define {} @"[c-flat]:():drop"({} %0) {
entry:
  ret {} zeroinitializer
}

define i1 @"[c-flat]:Result<T = ()>:is_ok"(ptr %0) {
entry:
  %tmp.is_ok = getelementptr inbounds nuw i8, ptr %0, i64 24
  %tmp1 = load i1, ptr %tmp.is_ok, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Error" @"[c-flat]:Result<T = ()>:unwrap_err"(%"[c-flat]:Result<T = ()>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = ()>", align 8
  store %"[c-flat]:Result<T = ()>" %0, ptr %self, align 8
  %tmp.unpack.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack.unpack3 = load i64, ptr %tmp.unpack.elt2, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack3, 1
  %tmp.unpack.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack.unpack5 = load i64, ptr %tmp.unpack.elt4, align 8
  %tmp.unpack6 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack5, 2
  %tmp1 = insertvalue %"[c-flat]:Error" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack6, 0
  ret %"[c-flat]:Error" %tmp1
}

define {} @"[c-flat]:print"({ ptr, i64 } %0) {
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
  %fun_call_tmp4 = call {} @c_printf(ptr %ref.i)
  %fun_call_tmp5 = call {} @c_flush()
  %tmp6.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp6.unpack, 0
  %tmp6.elt20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp6.unpack21 = load i64, ptr %tmp6.elt20, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp6.unpack21, 1
  %tmp6.elt22 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp6.unpack23 = load i64, ptr %tmp6.elt22, align 8
  %tmp624 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp6.unpack23, 2
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp624)
  ret {} zeroinitializer
}

declare {} @c_printf(ptr)

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
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.32, i64 1 })
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
