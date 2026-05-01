; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [14 x i8] c"connecting...\00", align 1
@global_string.15 = private unnamed_addr constant [10 x i8] c"127.0.0.1\00", align 1
@global_string.16 = private unnamed_addr constant [11 x i8] c"connected!\00", align 1
@global_string.17 = private unnamed_addr constant [3 x i8] c"> \00", align 1
@global_string.18 = private unnamed_addr constant [3 x i8] c"< \00", align 1
@global_string.19 = private unnamed_addr constant [6 x i8] c"done.\00", align 1

define {} @main() {
entry:
  %buffer = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp5 = alloca i64, align 8
  %msg_len = alloca i64, align 8
  %msg = alloca %"[c-flat]:Vec<T = char>", align 8
  %sock = alloca i32, align 4
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp0 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string, i64 13 })
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.15, i64 9 })
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp0, align 8
  %tmp0.repack56 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  %fun_call_tmp1.elt57 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt57, ptr %tmp0.repack56, align 8
  %tmp0.repack58 = getelementptr inbounds nuw i8, ptr %tmp0, i64 16
  %fun_call_tmp1.elt59 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt59, ptr %tmp0.repack58, align 8
  %fun_call_tmp2 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(ptr nonnull %tmp0)
  %fun_call_tmp2.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %tmp1, align 8
  %tmp1.repack60 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp2.elt61 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt61, ptr %tmp1.repack60, align 8
  %tmp1.repack62 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp2.elt63 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt63, ptr %tmp1.repack62, align 8
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %ref.i53 = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %fun_call_tmp5 = call i32 @c_connect(ptr %ref.i53, i32 8080)
  store i32 %fun_call_tmp5, ptr %sock, align 4
  %fun_call_tmp6 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.16, i64 10 })
  br label %then

then:                                             ; preds = %else, %entry
  %fun_call_tmp7 = call {} @"[c-flat]:print"({ ptr, i64 } { ptr @global_string.17, i64 2 })
  %fun_call_tmp8 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:readln"()
  %fun_call_tmp8.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp8, 0
  store ptr %fun_call_tmp8.elt, ptr %msg, align 8
  %msg.repack64 = getelementptr inbounds nuw i8, ptr %msg, i64 8
  %fun_call_tmp8.elt65 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp8, 1
  store i64 %fun_call_tmp8.elt65, ptr %msg.repack64, align 8
  %msg.repack66 = getelementptr inbounds nuw i8, ptr %msg, i64 16
  %fun_call_tmp8.elt67 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp8, 2
  store i64 %fun_call_tmp8.elt67, ptr %msg.repack66, align 8
  %fun_call_tmp9 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %msg)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp9, 1
  store i64 %len.i, ptr %msg_len, align 4
  %fun_call_tmp11 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice"(ptr nonnull %msg)
  %ref.i51 = extractvalue { ptr, i64 } %fun_call_tmp11, 0
  %tmp = load i32, ptr %sock, align 4
  %fun_call_tmp14 = call i32 @c_send(i32 %tmp, ptr %ref.i51, i64 %len.i)
  store i64 3, ptr %tmp5, align 4
  %tmp.i = load i64, ptr %msg_len, align 4
  %tmp2.i = icmp eq i64 %tmp.i, 3
  br i1 %tmp2.i, label %then16, label %else

then16:                                           ; preds = %then
  %tmp18 = load i64, ptr %tmp5, align 4
  %fun_call_tmp19 = call {} @"[c-flat]:u64:drop"(i64 %tmp18)
  %tmp20 = load i64, ptr %msg_len, align 4
  %fun_call_tmp21 = call {} @"[c-flat]:u64:drop"(i64 %tmp20)
  %tmp22.unpack = load ptr, ptr %msg, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp22.unpack, 0
  %tmp22.elt82 = getelementptr inbounds nuw i8, ptr %msg, i64 8
  %tmp22.unpack83 = load i64, ptr %tmp22.elt82, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp22.unpack83, 1
  %tmp22.elt84 = getelementptr inbounds nuw i8, ptr %msg, i64 16
  %tmp22.unpack85 = load i64, ptr %tmp22.elt84, align 8
  %tmp2286 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp22.unpack85, 2
  %fun_call_tmp23 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp2286)
  %fun_call_tmp43 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.19, i64 5 })
  %tmp44 = load i32, ptr %sock, align 4
  %fun_call_tmp45 = call {} @"[c-flat]:i32:drop"(i32 %tmp44)
  %tmp46.unpack = load ptr, ptr %tmp1, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp46.unpack, 0
  %tmp46.elt87 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp46.unpack88 = load i64, ptr %tmp46.elt87, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp46.unpack88, 1
  %tmp46.elt89 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp46.unpack90 = load i64, ptr %tmp46.elt89, align 8
  %tmp4691 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp46.unpack90, 2
  %fun_call_tmp47 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp4691)
  %tmp48.unpack = load ptr, ptr %tmp0, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp48.unpack, 0
  %tmp48.elt92 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  %tmp48.unpack93 = load i64, ptr %tmp48.elt92, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp48.unpack93, 1
  %tmp48.elt94 = getelementptr inbounds nuw i8, ptr %tmp0, i64 16
  %tmp48.unpack95 = load i64, ptr %tmp48.elt94, align 8
  %tmp4896 = insertvalue %"[c-flat]:Vec<T = char>" %5, i64 %tmp48.unpack95, 2
  %fun_call_tmp49 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp4896)
  ret {} %fun_call_tmp43

else:                                             ; preds = %then
  %tmp24 = load i64, ptr %msg_len, align 4
  %fun_call_tmp25 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:with_capacity"(i64 %tmp24)
  %fun_call_tmp25.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp25, 0
  store ptr %fun_call_tmp25.elt, ptr %buffer, align 8
  %buffer.repack68 = getelementptr inbounds nuw i8, ptr %buffer, i64 8
  %fun_call_tmp25.elt69 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp25, 1
  store i64 %fun_call_tmp25.elt69, ptr %buffer.repack68, align 8
  %buffer.repack70 = getelementptr inbounds nuw i8, ptr %buffer, i64 16
  %fun_call_tmp25.elt71 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp25, 2
  store i64 %fun_call_tmp25.elt71, ptr %buffer.repack70, align 8
  %fun_call_tmp26 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice"(ptr nonnull %buffer)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp26, 0
  %tmp28 = load i32, ptr %sock, align 4
  %tmp29 = load i64, ptr %msg_len, align 4
  %fun_call_tmp30 = call i32 @c_recv(i32 %tmp28, ptr %ref.i, i64 %tmp29)
  %fun_call_tmp31 = call {} @"[c-flat]:print"({ ptr, i64 } { ptr @global_string.18, i64 2 })
  %fun_call_tmp32 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %buffer)
  %fun_call_tmp33 = call {} @"[c-flat]:println"({ ptr, i64 } %fun_call_tmp32)
  %tmp34.unpack = load ptr, ptr %buffer, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp34.unpack, 0
  %tmp34.elt72 = getelementptr inbounds nuw i8, ptr %buffer, i64 8
  %tmp34.unpack73 = load i64, ptr %tmp34.elt72, align 8
  %7 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp34.unpack73, 1
  %tmp34.elt74 = getelementptr inbounds nuw i8, ptr %buffer, i64 16
  %tmp34.unpack75 = load i64, ptr %tmp34.elt74, align 8
  %tmp3476 = insertvalue %"[c-flat]:Vec<T = char>" %7, i64 %tmp34.unpack75, 2
  %fun_call_tmp35 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp3476)
  %tmp36 = load i64, ptr %tmp5, align 4
  %fun_call_tmp37 = call {} @"[c-flat]:u64:drop"(i64 %tmp36)
  %tmp38 = load i64, ptr %msg_len, align 4
  %fun_call_tmp39 = call {} @"[c-flat]:u64:drop"(i64 %tmp38)
  %tmp40.unpack = load ptr, ptr %msg, align 8
  %8 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp40.unpack, 0
  %tmp40.elt77 = getelementptr inbounds nuw i8, ptr %msg, i64 8
  %tmp40.unpack78 = load i64, ptr %tmp40.elt77, align 8
  %9 = insertvalue %"[c-flat]:Vec<T = char>" %8, i64 %tmp40.unpack78, 1
  %tmp40.elt79 = getelementptr inbounds nuw i8, ptr %msg, i64 16
  %tmp40.unpack80 = load i64, ptr %tmp40.elt79, align 8
  %tmp4081 = insertvalue %"[c-flat]:Vec<T = char>" %9, i64 %tmp40.unpack80, 2
  %fun_call_tmp41 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp4081)
  br label %then
}

define {} @"[c-flat]:println"({ ptr, i64 } %0) {
entry:
  %tmp2 = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack18 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt19 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt19, ptr %tmp1.repack18, align 8
  %tmp1.repack20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt21 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt21, ptr %tmp1.repack20, align 8
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(ptr nonnull %tmp1)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp2, align 8
  %tmp2.repack22 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %fun_call_tmp1.elt23 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt23, ptr %tmp2.repack22, align 8
  %tmp2.repack24 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %fun_call_tmp1.elt25 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt25, ptr %tmp2.repack24, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp2)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call {} @c_puts(ptr %ref.i)
  %fun_call_tmp5 = call {} @c_flush()
  %tmp6.unpack = load ptr, ptr %tmp2, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp6.unpack, 0
  %tmp6.elt26 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp6.unpack27 = load i64, ptr %tmp6.elt26, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp6.unpack27, 1
  %tmp6.elt28 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %tmp6.unpack29 = load i64, ptr %tmp6.elt28, align 8
  %tmp630 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp6.unpack29, 2
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp630)
  %tmp8.unpack = load ptr, ptr %tmp1, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp8.unpack, 0
  %tmp8.elt31 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp8.unpack32 = load i64, ptr %tmp8.elt31, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp8.unpack32, 1
  %tmp8.elt33 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp8.unpack34 = load i64, ptr %tmp8.elt33, align 8
  %tmp835 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp8.unpack34, 2
  %fun_call_tmp9 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp835)
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

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(ptr %0) {
entry:
  %result = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:clone"(ptr %0)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack5 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt6 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt6, ptr %result.repack5, align 8
  %result.repack7 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt8 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt8, ptr %result.repack7, align 8
  %fun_call_tmp1 = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %result, i8 0)
  %tmp2.unpack = load ptr, ptr %result, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp2.unpack, 0
  %tmp2.elt9 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp2.unpack10 = load i64, ptr %tmp2.elt9, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp2.unpack10, 1
  %tmp2.elt11 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp2.unpack12 = load i64, ptr %tmp2.elt11, align 8
  %tmp213 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp2.unpack12, 2
  ret %"[c-flat]:Vec<T = char>" %tmp213
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

declare {} @c_flush()

define {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %0) {
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

declare i32 @c_connect(ptr, i32)

define {} @"[c-flat]:print"({ ptr, i64 } %0) {
entry:
  %tmp2 = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack18 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt19 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt19, ptr %tmp1.repack18, align 8
  %tmp1.repack20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt21 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt21, ptr %tmp1.repack20, align 8
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(ptr nonnull %tmp1)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp2, align 8
  %tmp2.repack22 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %fun_call_tmp1.elt23 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt23, ptr %tmp2.repack22, align 8
  %tmp2.repack24 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %fun_call_tmp1.elt25 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt25, ptr %tmp2.repack24, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp2)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call {} @c_printf(ptr %ref.i)
  %fun_call_tmp5 = call {} @c_flush()
  %tmp6.unpack = load ptr, ptr %tmp2, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp6.unpack, 0
  %tmp6.elt26 = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp6.unpack27 = load i64, ptr %tmp6.elt26, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp6.unpack27, 1
  %tmp6.elt28 = getelementptr inbounds nuw i8, ptr %tmp2, i64 16
  %tmp6.unpack29 = load i64, ptr %tmp6.elt28, align 8
  %tmp630 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp6.unpack29, 2
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp630)
  %tmp8.unpack = load ptr, ptr %tmp1, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp8.unpack, 0
  %tmp8.elt31 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp8.unpack32 = load i64, ptr %tmp8.elt31, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp8.unpack32, 1
  %tmp8.elt33 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp8.unpack34 = load i64, ptr %tmp8.elt33, align 8
  %tmp835 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp8.unpack34, 2
  %fun_call_tmp9 = call {} @"[c-flat]:Vec<T = char>:drop.13"(%"[c-flat]:Vec<T = char>" %tmp835)
  ret {} zeroinitializer
}

declare {} @c_printf(ptr)

define %"[c-flat]:Vec<T = char>" @"[c-flat]:readln"() {
entry:
  %tmp3 = alloca i8, align 1
  %tmp2 = alloca i8, align 1
  %ch = alloca i8, align 1
  %input = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %input, align 8
  %input.repack30 = getelementptr inbounds nuw i8, ptr %input, i64 8
  %fun_call_tmp.elt31 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt31, ptr %input.repack30, align 8
  %input.repack32 = getelementptr inbounds nuw i8, ptr %input, i64 16
  %fun_call_tmp.elt33 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt33, ptr %input.repack32, align 8
  br label %then

then:                                             ; preds = %else, %entry
  %fun_call_tmp1 = call i8 @c_getchar()
  store i8 %fun_call_tmp1, ptr %ch, align 1
  store i8 10, ptr %tmp2, align 1
  %tmp.i24 = load i8, ptr %ch, align 1
  store i8 0, ptr %tmp3, align 1
  switch i8 %tmp.i24, label %else [
    i8 10, label %then3
    i8 0, label %then3
  ]

then3:                                            ; preds = %then, %then
  %tmp = load i8, ptr %tmp3, align 1
  %fun_call_tmp7 = call {} @"[c-flat]:char:drop"(i8 %tmp)
  %tmp8 = load i8, ptr %tmp2, align 1
  %fun_call_tmp9 = call {} @"[c-flat]:char:drop"(i8 %tmp8)
  %tmp10 = load i8, ptr %ch, align 1
  %fun_call_tmp11 = call {} @"[c-flat]:char:drop"(i8 %tmp10)
  %tmp21.unpack = load ptr, ptr %input, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp21.unpack, 0
  %tmp21.elt34 = getelementptr inbounds nuw i8, ptr %input, i64 8
  %tmp21.unpack35 = load i64, ptr %tmp21.elt34, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp21.unpack35, 1
  %tmp21.elt36 = getelementptr inbounds nuw i8, ptr %input, i64 16
  %tmp21.unpack37 = load i64, ptr %tmp21.elt36, align 8
  %tmp2138 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp21.unpack37, 2
  ret %"[c-flat]:Vec<T = char>" %tmp2138

else:                                             ; preds = %then
  %tmp12 = load i8, ptr %ch, align 1
  %fun_call_tmp13 = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %input, i8 %tmp12)
  %tmp14 = load i8, ptr %tmp3, align 1
  %fun_call_tmp15 = call {} @"[c-flat]:char:drop"(i8 %tmp14)
  %tmp16 = load i8, ptr %tmp2, align 1
  %fun_call_tmp17 = call {} @"[c-flat]:char:drop"(i8 %tmp16)
  %fun_call_tmp19 = call {} @"[c-flat]:char:drop"(i8 %tmp12)
  br label %then
}

declare i8 @c_getchar()

declare i32 @c_send(i32, ptr, i64)

declare i32 @c_recv(i32, ptr, i64)

define {} @"[c-flat]:i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}
