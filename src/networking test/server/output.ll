; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:Result<T = ()>" = type { {}, %"[c-flat]:Error", i1 }
%"[c-flat]:Error" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:Result<T = Vec<T = char>>" = type { %"[c-flat]:Vec<T = char>", %"[c-flat]:Error", i1 }
%"[c-flat]:TCP_Socket" = type { %"[c-flat]:Vec<T = char>", i32, i32 }
%"[c-flat]:Option<T = Error>" = type { %"[c-flat]:TCP_Socket", i1 }
%"[c-flat]:TCP_Listener" = type { i32, i32, i32 }
%"[c-flat]:Result<T = Error>" = type { %"[c-flat]:TCP_Listener", %"[c-flat]:Error", i1 }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }
%"[c-flat]:Vec<T = u8>" = type { ptr, i64, i64 }
%"[c-flat]:Result<T = Vec<T = u8>>" = type { %"[c-flat]:Vec<T = u8>", %"[c-flat]:Error", i1 }
%"[c-flat]:SliceIter<T = u8>" = type { ptr, ptr }
%"[c-flat]:mem:Manually_Drop<T = u8>" = type { i8 }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [13 x i8] c"Listening...\00", align 1
@global_string.39 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@global_string.40 = private unnamed_addr constant [9 x i8] c"Accepted\00", align 1
@global_string.55 = private unnamed_addr constant [13 x i8] c"heap count: \00", align 1
@global_string.56 = private unnamed_addr constant [2 x i8] c"0\00", align 1

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
  %fun_call_tmp10 = call {} @"[c-flat]:print"({ ptr, i64 } { ptr @global_string.55, i64 12 })
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
  %fun_call_tmp17 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp1637)
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
  %fun_call_tmp8 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp728)
  br label %merge
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:program"() {
entry:
  %text = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp4 = alloca %"[c-flat]:Result<T = Vec<T = char>>", align 8
  %client = alloca %"[c-flat]:TCP_Socket", align 8
  %opt_client = alloca %"[c-flat]:Option<T = Error>", align 8
  %listener = alloca %"[c-flat]:TCP_Listener", align 8
  %tmp0 = alloca %"[c-flat]:Result<T = Error>", align 8
  %fun_call_tmp = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Error:listen"(i32 8080, i32 1)
  store %"[c-flat]:Result<T = Error>" %fun_call_tmp, ptr %tmp0, align 8
  %fun_call_tmp1 = call i1 @"[c-flat]:Result<T = Error>:is_unwrappable"(ptr nonnull %tmp0)
  br i1 %fun_call_tmp1, label %then, label %else

then:                                             ; preds = %entry
  %fun_call_tmp3 = call %"[c-flat]:TCP_Listener" @"[c-flat]:Result<T = Error>:unwrap"(%"[c-flat]:Result<T = Error>" %fun_call_tmp)
  %fun_call_tmp3.elt = extractvalue %"[c-flat]:TCP_Listener" %fun_call_tmp3, 0
  store i32 %fun_call_tmp3.elt, ptr %listener, align 4
  %listener.repack57 = getelementptr inbounds nuw i8, ptr %listener, i64 4
  %fun_call_tmp3.elt58 = extractvalue %"[c-flat]:TCP_Listener" %fun_call_tmp3, 1
  store i32 %fun_call_tmp3.elt58, ptr %listener.repack57, align 4
  %listener.repack59 = getelementptr inbounds nuw i8, ptr %listener, i64 8
  %fun_call_tmp3.elt60 = extractvalue %"[c-flat]:TCP_Listener" %fun_call_tmp3, 2
  store i32 %fun_call_tmp3.elt60, ptr %listener.repack59, align 4
  %fun_call_tmp4 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string, i64 12 })
  br label %then5

common.ret:                                       ; preds = %else19, %then18, %else
  %common.ret.op = phi %"[c-flat]:Result<T = ()>" [ %fun_call_tmp2, %else ], [ %fun_call_tmp33, %then18 ], [ %fun_call_tmp20, %else19 ]
  ret %"[c-flat]:Result<T = ()>" %common.ret.op

else:                                             ; preds = %entry
  %fun_call_tmp2 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Error>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Error>" %fun_call_tmp)
  br label %common.ret

then5:                                            ; preds = %then8, %then
  %fun_call_tmp6 = call %"[c-flat]:Option<T = Error>" @"[c-flat]:Error:accept"(ptr nonnull %listener)
  store %"[c-flat]:Option<T = Error>" %fun_call_tmp6, ptr %opt_client, align 8
  %fun_call_tmp10 = call i1 @"[c-flat]:Option<T = Error>:is_none"(ptr nonnull %opt_client)
  br i1 %fun_call_tmp10, label %then8, label %else9

then8:                                            ; preds = %then5
  %fun_call_tmp11 = call {} @"[c-flat]:sleep"(double 1.000000e-01)
  %tmp = load %"[c-flat]:Option<T = Error>", ptr %opt_client, align 8
  %fun_call_tmp12 = call {} @"[c-flat]:Option<T = Error>:drop"(%"[c-flat]:Option<T = Error>" %tmp)
  br label %then5

else9:                                            ; preds = %then5
  %fun_call_tmp13 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.40, i64 8 })
  %tmp14 = load %"[c-flat]:Option<T = Error>", ptr %opt_client, align 8
  %fun_call_tmp15 = call %"[c-flat]:TCP_Socket" @"[c-flat]:Option<T = Error>:unwrap"(%"[c-flat]:Option<T = Error>" %tmp14)
  %fun_call_tmp15.elt = extractvalue %"[c-flat]:TCP_Socket" %fun_call_tmp15, 0
  %fun_call_tmp15.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp15.elt, 0
  store ptr %fun_call_tmp15.elt.elt, ptr %client, align 8
  %client.repack65 = getelementptr inbounds nuw i8, ptr %client, i64 8
  %fun_call_tmp15.elt.elt66 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp15.elt, 1
  store i64 %fun_call_tmp15.elt.elt66, ptr %client.repack65, align 8
  %client.repack67 = getelementptr inbounds nuw i8, ptr %client, i64 16
  %fun_call_tmp15.elt.elt68 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp15.elt, 2
  store i64 %fun_call_tmp15.elt.elt68, ptr %client.repack67, align 8
  %client.repack61 = getelementptr inbounds nuw i8, ptr %client, i64 24
  %fun_call_tmp15.elt62 = extractvalue %"[c-flat]:TCP_Socket" %fun_call_tmp15, 1
  store i32 %fun_call_tmp15.elt62, ptr %client.repack61, align 8
  %client.repack63 = getelementptr inbounds nuw i8, ptr %client, i64 28
  %fun_call_tmp15.elt64 = extractvalue %"[c-flat]:TCP_Socket" %fun_call_tmp15, 2
  store i32 %fun_call_tmp15.elt64, ptr %client.repack63, align 4
  %fun_call_tmp16 = call %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Error:recv_text"(ptr nonnull %client, i64 6)
  store %"[c-flat]:Result<T = Vec<T = char>>" %fun_call_tmp16, ptr %tmp4, align 8
  %fun_call_tmp17 = call i1 @"[c-flat]:Result<T = Vec<T = char>>:is_unwrappable"(ptr nonnull %tmp4)
  br i1 %fun_call_tmp17, label %then18, label %else19

then18:                                           ; preds = %else9
  %fun_call_tmp25 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Result<T = Vec<T = char>>:unwrap"(%"[c-flat]:Result<T = Vec<T = char>>" %fun_call_tmp16)
  %fun_call_tmp25.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp25, 0
  store ptr %fun_call_tmp25.elt, ptr %text, align 8
  %text.repack84 = getelementptr inbounds nuw i8, ptr %text, i64 8
  %fun_call_tmp25.elt85 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp25, 1
  store i64 %fun_call_tmp25.elt85, ptr %text.repack84, align 8
  %text.repack86 = getelementptr inbounds nuw i8, ptr %text, i64 16
  %fun_call_tmp25.elt87 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp25, 2
  store i64 %fun_call_tmp25.elt87, ptr %text.repack86, align 8
  %fun_call_tmp26 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %text)
  %fun_call_tmp27 = call {} @"[c-flat]:println"({ ptr, i64 } %fun_call_tmp26)
  %tmp28.unpack = load ptr, ptr %text, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp28.unpack, 0
  %tmp28.elt88 = getelementptr inbounds nuw i8, ptr %text, i64 8
  %tmp28.unpack89 = load i64, ptr %tmp28.elt88, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp28.unpack89, 1
  %tmp28.elt90 = getelementptr inbounds nuw i8, ptr %text, i64 16
  %tmp28.unpack91 = load i64, ptr %tmp28.elt90, align 8
  %tmp2892 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp28.unpack91, 2
  %fun_call_tmp29 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp2892)
  %tmp30.unpack.unpack = load ptr, ptr %client, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp30.unpack.unpack, 0
  %tmp30.unpack.elt98 = getelementptr inbounds nuw i8, ptr %client, i64 8
  %tmp30.unpack.unpack99 = load i64, ptr %tmp30.unpack.elt98, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp30.unpack.unpack99, 1
  %tmp30.unpack.elt100 = getelementptr inbounds nuw i8, ptr %client, i64 16
  %tmp30.unpack.unpack101 = load i64, ptr %tmp30.unpack.elt100, align 8
  %tmp30.unpack102 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp30.unpack.unpack101, 2
  %tmp30.elt93 = getelementptr inbounds nuw i8, ptr %client, i64 24
  %tmp30.unpack94 = load i32, ptr %tmp30.elt93, align 8
  %tmp30.elt95 = getelementptr inbounds nuw i8, ptr %client, i64 28
  %tmp30.unpack96 = load i32, ptr %tmp30.elt95, align 4
  %fun_call_tmp.i45 = call {} @"[c-flat]:i32:drop"(i32 %tmp30.unpack96)
  %fun_call_tmp2.i47 = call {} @"[c-flat]:i32:drop"(i32 %tmp30.unpack94)
  %fun_call_tmp4.i49 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp30.unpack102)
  %fun_call_tmp33 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:ok"({} zeroinitializer)
  %tmp34.unpack = load i32, ptr %listener, align 4
  %tmp34.elt103 = getelementptr inbounds nuw i8, ptr %listener, i64 4
  %tmp34.unpack104 = load i32, ptr %tmp34.elt103, align 4
  %tmp34.elt105 = getelementptr inbounds nuw i8, ptr %listener, i64 8
  %tmp34.unpack106 = load i32, ptr %tmp34.elt105, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp34.unpack106)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp34.unpack104)
  %fun_call_tmp4.i = call {} @"[c-flat]:i32:drop"(i32 %tmp34.unpack)
  br label %common.ret

else19:                                           ; preds = %else9
  %fun_call_tmp20 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Vec<T = char>>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Vec<T = char>>" %fun_call_tmp16)
  %tmp21.unpack.unpack = load ptr, ptr %client, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp21.unpack.unpack, 0
  %tmp21.unpack.elt74 = getelementptr inbounds nuw i8, ptr %client, i64 8
  %tmp21.unpack.unpack75 = load i64, ptr %tmp21.unpack.elt74, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp21.unpack.unpack75, 1
  %tmp21.unpack.elt76 = getelementptr inbounds nuw i8, ptr %client, i64 16
  %tmp21.unpack.unpack77 = load i64, ptr %tmp21.unpack.elt76, align 8
  %tmp21.unpack78 = insertvalue %"[c-flat]:Vec<T = char>" %5, i64 %tmp21.unpack.unpack77, 2
  %tmp21.elt69 = getelementptr inbounds nuw i8, ptr %client, i64 24
  %tmp21.unpack70 = load i32, ptr %tmp21.elt69, align 8
  %tmp21.elt71 = getelementptr inbounds nuw i8, ptr %client, i64 28
  %tmp21.unpack72 = load i32, ptr %tmp21.elt71, align 4
  %fun_call_tmp.i52 = call {} @"[c-flat]:i32:drop"(i32 %tmp21.unpack72)
  %fun_call_tmp2.i54 = call {} @"[c-flat]:i32:drop"(i32 %tmp21.unpack70)
  %fun_call_tmp4.i56 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp21.unpack78)
  %tmp23.unpack = load i32, ptr %listener, align 4
  %tmp23.elt79 = getelementptr inbounds nuw i8, ptr %listener, i64 4
  %tmp23.unpack80 = load i32, ptr %tmp23.elt79, align 4
  %tmp23.elt81 = getelementptr inbounds nuw i8, ptr %listener, i64 8
  %tmp23.unpack82 = load i32, ptr %tmp23.elt81, align 4
  %fun_call_tmp.i38 = call {} @"[c-flat]:i32:drop"(i32 %tmp23.unpack82)
  %fun_call_tmp2.i40 = call {} @"[c-flat]:i32:drop"(i32 %tmp23.unpack80)
  %fun_call_tmp4.i42 = call {} @"[c-flat]:i32:drop"(i32 %tmp23.unpack)
  br label %common.ret
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

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Error:listen"(i32 %0, i32 %1) {
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
  br i1 %tmp2.i, label %then, label %then14

merge:                                            ; preds = %then14, %then
  %result = phi %"[c-flat]:Result<T = Error>" [ %fun_call_tmp5, %then ], [ %fun_call_tmp19, %then14 ]
  ret %"[c-flat]:Result<T = Error>" %result

then:                                             ; preds = %entry
  %fun_call_tmp4 = call %"[c-flat]:Error" @"[c-flat]:Error:c_err"()
  %fun_call_tmp5 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:err"(%"[c-flat]:Error" %fun_call_tmp4)
  %tmp6 = load i32, ptr %tmp3, align 4
  %fun_call_tmp7 = call {} @"[c-flat]:i32:drop"(i32 %tmp6)
  %tmp8 = load i32, ptr %socket, align 4
  %fun_call_tmp9 = call {} @"[c-flat]:i32:drop"(i32 %tmp8)
  %tmp10 = load i32, ptr %backlog, align 4
  %fun_call_tmp11 = call {} @"[c-flat]:i32:drop"(i32 %tmp10)
  %tmp12 = load i32, ptr %port, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:i32:drop"(i32 %tmp12)
  br label %merge

then14:                                           ; preds = %entry
  %tmp15 = load i32, ptr %port, align 4
  %tmp16 = load i32, ptr %backlog, align 4
  %tmp17 = load i32, ptr %socket, align 4
  %tmp_agg = insertvalue %"[c-flat]:TCP_Listener" undef, i32 %tmp15, 0
  %tmp_agg18 = insertvalue %"[c-flat]:TCP_Listener" %tmp_agg, i32 %tmp16, 1
  %tmp_constructor = insertvalue %"[c-flat]:TCP_Listener" %tmp_agg18, i32 %tmp17, 2
  %fun_call_tmp19 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:ok"(%"[c-flat]:TCP_Listener" %tmp_constructor)
  %tmp20 = load i32, ptr %tmp3, align 4
  %fun_call_tmp21 = call {} @"[c-flat]:i32:drop"(i32 %tmp20)
  %tmp22 = load i32, ptr %socket, align 4
  %fun_call_tmp23 = call {} @"[c-flat]:i32:drop"(i32 %tmp22)
  %tmp24 = load i32, ptr %backlog, align 4
  %fun_call_tmp25 = call {} @"[c-flat]:i32:drop"(i32 %tmp24)
  %tmp26 = load i32, ptr %port, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:i32:drop"(i32 %tmp26)
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
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Error>" { %"[c-flat]:TCP_Listener" zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
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

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:ok"(%"[c-flat]:TCP_Listener" %0) {
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
  %tmp_agg = insertvalue %"[c-flat]:Result<T = Error>" undef, %"[c-flat]:TCP_Listener" %tmp10, 0
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
  %tmp.is_ok = getelementptr inbounds nuw i8, ptr %0, i64 40
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
  %self.err = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack.unpack = load ptr, ptr %self.err, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 24
  %tmp.unpack.unpack3 = load i64, ptr %tmp.unpack.elt2, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack3, 1
  %tmp.unpack.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 32
  %tmp.unpack.unpack5 = load i64, ptr %tmp.unpack.elt4, align 8
  %tmp.unpack6 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack5, 2
  %tmp1 = insertvalue %"[c-flat]:Error" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack6, 0
  ret %"[c-flat]:Error" %tmp1
}

define %"[c-flat]:TCP_Listener" @"[c-flat]:Result<T = Error>:unwrap"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Error>", align 8
  store %"[c-flat]:Result<T = Error>" %0, ptr %self, align 8
  %tmp.unpack = load i32, ptr %self, align 4
  %1 = insertvalue %"[c-flat]:TCP_Listener" poison, i32 %tmp.unpack, 0
  %tmp.elt1 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %tmp.unpack2 = load i32, ptr %tmp.elt1, align 4
  %2 = insertvalue %"[c-flat]:TCP_Listener" %1, i32 %tmp.unpack2, 1
  %tmp.elt3 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack4 = load i32, ptr %tmp.elt3, align 4
  %tmp5 = insertvalue %"[c-flat]:TCP_Listener" %2, i32 %tmp.unpack4, 2
  ret %"[c-flat]:TCP_Listener" %tmp5
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
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp624)
  ret {} zeroinitializer
}

declare {} @c_puts(ptr)

define %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %0)
  ret %"[c-flat]:Vec<T = char>" %fun_call_tmp
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

declare {} @c_flush()

define %"[c-flat]:Option<T = Error>" @"[c-flat]:Error:accept"(ptr %0) {
entry:
  %tmp2 = alloca i32, align 4
  %result = alloca i32, align 4
  %tmp.socket = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i32, ptr %tmp.socket, align 4
  %fun_call_tmp = call i32 @c_accept(i32 %tmp1)
  store i32 %fun_call_tmp, ptr %result, align 4
  store i32 -1, ptr %tmp2, align 4
  %tmp.i = load i32, ptr %result, align 4
  %tmp2.i = icmp eq i32 %tmp.i, -1
  br i1 %tmp2.i, label %then, label %then11

merge:                                            ; preds = %then11, %then
  %result22 = phi %"[c-flat]:Option<T = Error>" [ %fun_call_tmp4, %then ], [ %fun_call_tmp15, %then11 ]
  ret %"[c-flat]:Option<T = Error>" %result22

then:                                             ; preds = %entry
  %fun_call_tmp4 = call %"[c-flat]:Option<T = Error>" @"[c-flat]:Option<T = Error>:none"()
  %tmp5 = load i32, ptr %tmp2, align 4
  %fun_call_tmp6 = call {} @"[c-flat]:i32:drop"(i32 %tmp5)
  %tmp7 = load i32, ptr %result, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:i32:drop"(i32 %tmp7)
  br label %merge

then11:                                           ; preds = %entry
  %fun_call_tmp12 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.39, i64 0 })
  %tmp13 = load i32, ptr %result, align 4
  %tmp_agg = insertvalue %"[c-flat]:TCP_Socket" undef, %"[c-flat]:Vec<T = char>" %fun_call_tmp12, 0
  %tmp_agg14 = insertvalue %"[c-flat]:TCP_Socket" %tmp_agg, i32 0, 1
  %tmp_constructor = insertvalue %"[c-flat]:TCP_Socket" %tmp_agg14, i32 %tmp13, 2
  %fun_call_tmp15 = call %"[c-flat]:Option<T = Error>" @"[c-flat]:Option<T = Error>:some"(%"[c-flat]:TCP_Socket" %tmp_constructor)
  %tmp16 = load i32, ptr %tmp2, align 4
  %fun_call_tmp17 = call {} @"[c-flat]:i32:drop"(i32 %tmp16)
  %tmp18 = load i32, ptr %result, align 4
  %fun_call_tmp19 = call {} @"[c-flat]:i32:drop"(i32 %tmp18)
  br label %merge
}

declare i32 @c_accept(i32)

define %"[c-flat]:Option<T = Error>" @"[c-flat]:Option<T = Error>:none"() {
entry:
  ret %"[c-flat]:Option<T = Error>" zeroinitializer
}

define %"[c-flat]:Option<T = Error>" @"[c-flat]:Option<T = Error>:some"(%"[c-flat]:TCP_Socket" %0) {
entry:
  %value = alloca %"[c-flat]:TCP_Socket", align 8
  %.elt = extractvalue %"[c-flat]:TCP_Socket" %0, 0
  %.elt.elt = extractvalue %"[c-flat]:Vec<T = char>" %.elt, 0
  store ptr %.elt.elt, ptr %value, align 8
  %value.repack5 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt.elt6 = extractvalue %"[c-flat]:Vec<T = char>" %.elt, 1
  store i64 %.elt.elt6, ptr %value.repack5, align 8
  %value.repack7 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt.elt8 = extractvalue %"[c-flat]:Vec<T = char>" %.elt, 2
  store i64 %.elt.elt8, ptr %value.repack7, align 8
  %value.repack1 = getelementptr inbounds nuw i8, ptr %value, i64 24
  %.elt2 = extractvalue %"[c-flat]:TCP_Socket" %0, 1
  store i32 %.elt2, ptr %value.repack1, align 8
  %value.repack3 = getelementptr inbounds nuw i8, ptr %value, i64 28
  %.elt4 = extractvalue %"[c-flat]:TCP_Socket" %0, 2
  store i32 %.elt4, ptr %value.repack3, align 4
  %tmp.unpack.unpack = load ptr, ptr %value, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt14 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp.unpack.unpack15 = load i64, ptr %tmp.unpack.elt14, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack15, 1
  %tmp.unpack.elt16 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %tmp.unpack.unpack17 = load i64, ptr %tmp.unpack.elt16, align 8
  %tmp.unpack18 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack17, 2
  %3 = insertvalue %"[c-flat]:TCP_Socket" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack18, 0
  %tmp.elt9 = getelementptr inbounds nuw i8, ptr %value, i64 24
  %tmp.unpack10 = load i32, ptr %tmp.elt9, align 8
  %4 = insertvalue %"[c-flat]:TCP_Socket" %3, i32 %tmp.unpack10, 1
  %tmp.elt11 = getelementptr inbounds nuw i8, ptr %value, i64 28
  %tmp.unpack12 = load i32, ptr %tmp.elt11, align 4
  %tmp13 = insertvalue %"[c-flat]:TCP_Socket" %4, i32 %tmp.unpack12, 2
  %tmp_agg = insertvalue %"[c-flat]:Option<T = Error>" undef, %"[c-flat]:TCP_Socket" %tmp13, 0
  %tmp_constructor = insertvalue %"[c-flat]:Option<T = Error>" %tmp_agg, i1 true, 1
  ret %"[c-flat]:Option<T = Error>" %tmp_constructor
}

define i1 @"[c-flat]:Option<T = Error>:is_none"(ptr %0) {
entry:
  %tmp.is_some = getelementptr inbounds nuw i8, ptr %0, i64 32
  %tmp1 = load i1, ptr %tmp.is_some, align 1
  %tmp.i = xor i1 %tmp1, true
  ret i1 %tmp.i
}

define {} @"[c-flat]:sleep"(double %0) {
entry:
  %tmp.i = fmul double %0, 1.000000e+06
  %cast = fptoui double %tmp.i to i32
  %fun_call_tmp1 = call i32 @c_usleep(i32 %cast)
  %fun_call_tmp3 = call {} @"[c-flat]:f64:drop"(double %0)
  ret {} zeroinitializer
}

declare i32 @c_usleep(i32)

define {} @"[c-flat]:f64:drop"(double %0) {
entry:
  ret {} zeroinitializer
}

define {} @"[c-flat]:Option<T = Error>:drop"(%"[c-flat]:Option<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Option<T = Error>", align 8
  store %"[c-flat]:Option<T = Error>" %0, ptr %self, align 8
  %self.is_some = getelementptr inbounds nuw i8, ptr %self, i64 32
  %tmp = load i1, ptr %self.is_some, align 1
  br i1 %tmp, label %then, label %merge

merge:                                            ; preds = %entry, %then
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp1.unpack.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp1.unpack.unpack, 0
  %tmp1.unpack.elt7 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp1.unpack.unpack8 = load i64, ptr %tmp1.unpack.elt7, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp1.unpack.unpack8, 1
  %tmp1.unpack.elt9 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp1.unpack.unpack10 = load i64, ptr %tmp1.unpack.elt9, align 8
  %tmp1.unpack11 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp1.unpack.unpack10, 2
  %tmp1.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 24
  %tmp1.unpack3 = load i32, ptr %tmp1.elt2, align 8
  %tmp1.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 28
  %tmp1.unpack5 = load i32, ptr %tmp1.elt4, align 4
  %fun_call_tmp.i = call {} @"[c-flat]:i32:drop"(i32 %tmp1.unpack5)
  %fun_call_tmp2.i = call {} @"[c-flat]:i32:drop"(i32 %tmp1.unpack3)
  %fun_call_tmp4.i = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp1.unpack11)
  br label %merge
}

define %"[c-flat]:TCP_Socket" @"[c-flat]:Option<T = Error>:unwrap"(%"[c-flat]:Option<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Option<T = Error>", align 8
  store %"[c-flat]:Option<T = Error>" %0, ptr %self, align 8
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

define %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Error:recv_text"(ptr %0, i64 %1) {
entry:
  %res = alloca %"[c-flat]:Vec<T = u8>", align 8
  %tmp2 = alloca %"[c-flat]:Result<T = Vec<T = u8>>", align 8
  %len = alloca i64, align 8
  store i64 %1, ptr %len, align 4
  %fun_call_tmp = call %"[c-flat]:Result<T = Vec<T = u8>>" @"[c-flat]:Error:recv"(ptr %0, i64 %1)
  store %"[c-flat]:Result<T = Vec<T = u8>>" %fun_call_tmp, ptr %tmp2, align 8
  %fun_call_tmp2 = call i1 @"[c-flat]:Result<T = Vec<T = u8>>:is_unwrappable"(ptr nonnull %tmp2)
  br i1 %fun_call_tmp2, label %then, label %else

common.ret:                                       ; preds = %else, %then
  %common.ret.op = phi %"[c-flat]:Result<T = Vec<T = char>>" [ %fun_call_tmp11, %then ], [ %fun_call_tmp3, %else ]
  ret %"[c-flat]:Result<T = Vec<T = char>>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp8 = call %"[c-flat]:Vec<T = u8>" @"[c-flat]:Result<T = Vec<T = u8>>:unwrap"(%"[c-flat]:Result<T = Vec<T = u8>>" %fun_call_tmp)
  %fun_call_tmp8.elt = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp8, 0
  store ptr %fun_call_tmp8.elt, ptr %res, align 8
  %res.repack19 = getelementptr inbounds nuw i8, ptr %res, i64 8
  %fun_call_tmp8.elt20 = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp8, 1
  store i64 %fun_call_tmp8.elt20, ptr %res.repack19, align 8
  %res.repack21 = getelementptr inbounds nuw i8, ptr %res, i64 16
  %fun_call_tmp8.elt22 = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp8, 2
  store i64 %fun_call_tmp8.elt22, ptr %res.repack21, align 8
  %fun_call_tmp9 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:as_slice"(ptr nonnull %res)
  %fun_call_tmp10 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:from"({ ptr, i64 } %fun_call_tmp9)
  %fun_call_tmp11 = call %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Result<T = Vec<T = char>>:ok"(%"[c-flat]:Vec<T = char>" %fun_call_tmp10)
  %tmp12.unpack = load ptr, ptr %res, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = u8>" poison, ptr %tmp12.unpack, 0
  %tmp12.elt23 = getelementptr inbounds nuw i8, ptr %res, i64 8
  %tmp12.unpack24 = load i64, ptr %tmp12.elt23, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = u8>" %2, i64 %tmp12.unpack24, 1
  %tmp12.elt25 = getelementptr inbounds nuw i8, ptr %res, i64 16
  %tmp12.unpack26 = load i64, ptr %tmp12.elt25, align 8
  %tmp1227 = insertvalue %"[c-flat]:Vec<T = u8>" %3, i64 %tmp12.unpack26, 2
  %fun_call_tmp13 = call {} @"[c-flat]:Vec<T = u8>:drop.53"(%"[c-flat]:Vec<T = u8>" %tmp1227)
  %tmp14 = load i64, ptr %len, align 4
  %fun_call_tmp15 = call {} @"[c-flat]:u64:drop"(i64 %tmp14)
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp3 = call %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Result<T = Vec<T = u8>>:wrap_as<T_2 = Result<T = Vec<T = char>>>"(%"[c-flat]:Result<T = Vec<T = u8>>" %fun_call_tmp)
  %tmp4 = load i64, ptr %len, align 4
  %fun_call_tmp5 = call {} @"[c-flat]:u64:drop"(i64 %tmp4)
  br label %common.ret
}

define %"[c-flat]:Result<T = Vec<T = u8>>" @"[c-flat]:Error:recv"(ptr %0, i64 %1) {
entry:
  %tmp5 = alloca i32, align 4
  %recv_len = alloca i32, align 4
  %idx = alloca i64, align 8
  %result = alloca %"[c-flat]:Vec<T = u8>", align 8
  %len = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i64 %1, ptr %len, align 4
  %fun_call_tmp = call %"[c-flat]:Vec<T = u8>" @"[c-flat]:Vec<T = u8>:with_capacity"(i64 %1)
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack76 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt77 = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt77, ptr %result.repack76, align 8
  %result.repack78 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt79 = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt79, ptr %result.repack78, align 8
  %tmp1 = load i64, ptr %len, align 4
  %fun_call_tmp2 = call {} @"[c-flat]:Vec<T = u8>:set_len"(ptr nonnull %result, i64 %tmp1)
  store i64 0, ptr %idx, align 4
  br label %cond

common.ret:                                       ; preds = %then20, %else
  %common.ret.op = phi %"[c-flat]:Result<T = Vec<T = u8>>" [ %fun_call_tmp50, %else ], [ %fun_call_tmp26, %then20 ]
  ret %"[c-flat]:Result<T = Vec<T = u8>>" %common.ret.op

then:                                             ; preds = %cond
  %tmp7 = load ptr, ptr %self, align 8
  %tmp7.socket = getelementptr inbounds nuw i8, ptr %tmp7, i64 28
  %fun_call_tmp8 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref_mut"(ptr nonnull %result)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp8, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp10 = load i64, ptr %idx, align 4
  %tmp.i63 = add i64 %tmp10, %cast
  %cast12 = inttoptr i64 %tmp.i63 to ptr
  %fun_call_tmp13 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref_mut"(ptr nonnull %result)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp13, 1
  %tmp.i66 = sub i64 %len.i, %tmp10
  %tmp17 = load i32, ptr %tmp7.socket, align 4
  %fun_call_tmp18 = call i32 @c_recv(i32 %tmp17, ptr %cast12, i64 %tmp.i66)
  store i32 %fun_call_tmp18, ptr %recv_len, align 4
  store i32 -1, ptr %tmp5, align 4
  %tmp.i = load i32, ptr %recv_len, align 4
  %tmp2.i = icmp eq i32 %tmp.i, -1
  br i1 %tmp2.i, label %then20, label %else22

cond:                                             ; preds = %else22, %entry
  %tmp3 = load i64, ptr %idx, align 4
  %fun_call_tmp4 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref_mut"(ptr nonnull %result)
  %len.i75 = extractvalue { ptr, i64 } %fun_call_tmp4, 1
  %tmp.i69 = icmp ult i64 %tmp3, %len.i75
  br i1 %tmp.i69, label %then, label %else

else:                                             ; preds = %cond
  %tmp49.unpack = load ptr, ptr %result, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = u8>" poison, ptr %tmp49.unpack, 0
  %tmp49.elt80 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp49.unpack81 = load i64, ptr %tmp49.elt80, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = u8>" %2, i64 %tmp49.unpack81, 1
  %tmp49.elt82 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp49.unpack83 = load i64, ptr %tmp49.elt82, align 8
  %tmp4984 = insertvalue %"[c-flat]:Vec<T = u8>" %3, i64 %tmp49.unpack83, 2
  %fun_call_tmp50 = call %"[c-flat]:Result<T = Vec<T = u8>>" @"[c-flat]:Result<T = Vec<T = u8>>:ok"(%"[c-flat]:Vec<T = u8>" %tmp4984)
  %tmp51 = load i64, ptr %idx, align 4
  %fun_call_tmp52 = call {} @"[c-flat]:u64:drop"(i64 %tmp51)
  %tmp53 = load i64, ptr %len, align 4
  %fun_call_tmp54 = call {} @"[c-flat]:u64:drop"(i64 %tmp53)
  br label %common.ret

then20:                                           ; preds = %then
  %fun_call_tmp25 = call %"[c-flat]:Error" @"[c-flat]:Error:c_err"()
  %fun_call_tmp26 = call %"[c-flat]:Result<T = Vec<T = u8>>" @"[c-flat]:Result<T = Vec<T = u8>>:err"(%"[c-flat]:Error" %fun_call_tmp25)
  %tmp27 = load i32, ptr %tmp5, align 4
  %fun_call_tmp28 = call {} @"[c-flat]:i32:drop"(i32 %tmp27)
  %tmp29 = load i32, ptr %recv_len, align 4
  %fun_call_tmp30 = call {} @"[c-flat]:i32:drop"(i32 %tmp29)
  %tmp31 = load i64, ptr %idx, align 4
  %fun_call_tmp32 = call {} @"[c-flat]:u64:drop"(i64 %tmp31)
  %tmp33.unpack = load ptr, ptr %result, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = u8>" poison, ptr %tmp33.unpack, 0
  %tmp33.elt85 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp33.unpack86 = load i64, ptr %tmp33.elt85, align 8
  %5 = insertvalue %"[c-flat]:Vec<T = u8>" %4, i64 %tmp33.unpack86, 1
  %tmp33.elt87 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp33.unpack88 = load i64, ptr %tmp33.elt87, align 8
  %tmp3389 = insertvalue %"[c-flat]:Vec<T = u8>" %5, i64 %tmp33.unpack88, 2
  %fun_call_tmp34 = call {} @"[c-flat]:Vec<T = u8>:drop.53"(%"[c-flat]:Vec<T = u8>" %tmp3389)
  %tmp35 = load i64, ptr %len, align 4
  %fun_call_tmp36 = call {} @"[c-flat]:u64:drop"(i64 %tmp35)
  br label %common.ret

else22:                                           ; preds = %then
  %tmp40 = load i64, ptr %idx, align 4
  %tmp41 = load i32, ptr %recv_len, align 4
  %cast42 = sext i32 %tmp41 to i64
  %tmp.i60 = add i64 %tmp40, %cast42
  store i64 %tmp.i60, ptr %idx, align 4
  %tmp44 = load i32, ptr %tmp5, align 4
  %fun_call_tmp45 = call {} @"[c-flat]:i32:drop"(i32 %tmp44)
  %fun_call_tmp47 = call {} @"[c-flat]:i32:drop"(i32 %tmp41)
  br label %cond
}

define i1 @"[c-flat]:&u8:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define %"[c-flat]:Vec<T = u8>" @"[c-flat]:Vec<T = u8>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:alloc<T = u8>"(i64 %0)
  %tmp_agg = insertvalue %"[c-flat]:Vec<T = u8>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"[c-flat]:Vec<T = u8>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"[c-flat]:Vec<T = u8>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"[c-flat]:u64:drop"(i64 %0)
  ret %"[c-flat]:Vec<T = u8>" %tmp_constructor
}

define ptr @"[c-flat]:mem:alloc<T = u8>"(i64 %0) {
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

define {} @"[c-flat]:Vec<T = u8>:set_len"(ptr %0, i64 %1) {
entry:
  %tmp.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  store i64 %1, ptr %tmp.len, align 4
  %fun_call_tmp = call {} @"[c-flat]:u64:drop"(i64 %1)
  ret {} zeroinitializer
}

define { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = u8>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

declare i32 @c_recv(i32, ptr, i64)

define %"[c-flat]:Vec<T = u8>" @"[c-flat]:Vec<T = u8>:clone"(ptr %0) {
entry:
  %tmp_iter2 = alloca %"[c-flat]:SliceIter<T = u8>", align 8
  %clone = alloca %"[c-flat]:Vec<T = u8>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = u8>" @"[c-flat]:Vec<T = u8>:new"()
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %clone, align 8
  %clone.repack22 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %fun_call_tmp.elt23 = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt23, ptr %clone.repack22, align 8
  %clone.repack24 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %fun_call_tmp.elt25 = extractvalue %"[c-flat]:Vec<T = u8>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt25, ptr %clone.repack24, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp1 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref"(ptr %tmp)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp1, 1
  %fun_call_tmp3 = call {} @"[c-flat]:Vec<T = u8>:reserve"(ptr nonnull %clone, i64 %len.i)
  %fun_call_tmp5 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref"(ptr %tmp)
  %fun_call_tmp6 = call %"[c-flat]:SliceIter<T = u8>" @"[c-flat]:[u8]:iter"({ ptr, i64 } %fun_call_tmp5)
  %fun_call_tmp6.elt = extractvalue %"[c-flat]:SliceIter<T = u8>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack26 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp6.elt27 = extractvalue %"[c-flat]:SliceIter<T = u8>" %fun_call_tmp6, 1
  store ptr %fun_call_tmp6.elt27, ptr %tmp_iter2.repack26, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp8 = call ptr @"[c-flat]:Vec<T = u8>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call i8 @"[c-flat]:u8:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"[c-flat]:Vec<T = u8>:push"(ptr nonnull %clone, i8 %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"[c-flat]:Vec<T = u8>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp7, label %then, label %else

else:                                             ; preds = %cond
  %tmp14.unpack = load ptr, ptr %clone, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = u8>" poison, ptr %tmp14.unpack, 0
  %tmp14.elt28 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %tmp14.unpack29 = load i64, ptr %tmp14.elt28, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = u8>" %1, i64 %tmp14.unpack29, 1
  %tmp14.elt30 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %tmp14.unpack31 = load i64, ptr %tmp14.elt30, align 8
  %tmp1432 = insertvalue %"[c-flat]:Vec<T = u8>" %2, i64 %tmp14.unpack31, 2
  ret %"[c-flat]:Vec<T = u8>" %tmp1432
}

define %"[c-flat]:Vec<T = u8>" @"[c-flat]:Vec<T = u8>:new"() {
entry:
  %fun_call_tmp = call %"[c-flat]:Vec<T = u8>" @"[c-flat]:Vec<T = u8>:with_capacity"(i64 4)
  ret %"[c-flat]:Vec<T = u8>" %fun_call_tmp
}

define {} @"[c-flat]:Vec<T = u8>:reserve"(ptr %0, i64 %1) {
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
  %fun_call_tmp21 = call ptr @"[c-flat]:mem:alloc<T = u8>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"[c-flat]:mem:copy_range<T = u8>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"[c-flat]:mem:free<T = u8>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  %tmp35 = load i64, ptr %new_len, align 4
  %fun_call_tmp36 = call {} @"[c-flat]:u64:drop"(i64 %tmp35)
  %tmp37 = load i64, ptr %additional, align 4
  %fun_call_tmp38 = call {} @"[c-flat]:u64:drop"(i64 %tmp37)
  br label %merge
}

define {} @"[c-flat]:mem:copy_range<T = u8>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %2)
  %fun_call_tmp6 = call {} @"[c-flat]:u64:drop"(i64 %2)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:free<T = u8>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"[c-flat]:mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"[c-flat]:mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"[c-flat]:Vec<T = u8>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define %"[c-flat]:SliceIter<T = u8>" @"[c-flat]:[u8]:iter"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i = add i64 %.elt21, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"[c-flat]:SliceIter<T = u8>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"[c-flat]:SliceIter<T = u8>" %tmp_agg, ptr %cast7, 1
  ret %"[c-flat]:SliceIter<T = u8>" %tmp_constructor
}

define i1 @"[c-flat]:Vec<T = u8>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"[c-flat]:Vec<T = u8>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 1
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"[c-flat]:Vec<T = u8>:push"(ptr %0, i8 %1) {
entry:
  %value = alloca i8, align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 %1, ptr %value, align 1
  %fun_call_tmp = call {} @"[c-flat]:Vec<T = u8>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i23 = add i64 %tmp4, %cast
  %cast8 = inttoptr i64 %tmp.i23 to ptr
  %fun_call_tmp9 = call {} @"[c-flat]:mem:copy<T = u8>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load i8, ptr %value, align 1
  %tmp1528 = insertvalue %"[c-flat]:mem:Manually_Drop<T = u8>" poison, i8 %tmp14, 0
  %fun_call_tmp16 = call {} @"[c-flat]:mem:Manually_Drop<T = u8>:drop"(%"[c-flat]:mem:Manually_Drop<T = u8>" %tmp1528)
  %tmp17 = load i8, ptr %value, align 1
  %fun_call_tmp18 = call {} @"[c-flat]:u8:drop"(i8 %tmp17)
  ret {} zeroinitializer
}

define {} @"[c-flat]:mem:copy<T = u8>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define i8 @"[c-flat]:u8:clone"(ptr %0) {
entry:
  %tmp1 = load i8, ptr %0, align 1
  ret i8 %tmp1
}

define {} @"[c-flat]:mem:Manually_Drop<T = u8>:drop"(%"[c-flat]:mem:Manually_Drop<T = u8>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"[c-flat]:u8:drop"(i8 %0) {
entry:
  ret {} zeroinitializer
}

define %"[c-flat]:Result<T = Vec<T = u8>>" @"[c-flat]:Result<T = Vec<T = u8>>:err"(%"[c-flat]:Error" %0) {
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
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Vec<T = u8>>" { %"[c-flat]:Vec<T = u8>" zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Vec<T = u8>>" %tmp_agg1, i1 false, 2
  ret %"[c-flat]:Result<T = Vec<T = u8>>" %tmp_constructor
}

define {} @"[c-flat]:Vec<T = u8>:drop.53"(%"[c-flat]:Vec<T = u8>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"[c-flat]:Vec<T = u8>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = u8>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"[c-flat]:Vec<T = u8>" %0, 1
  store i64 %.elt20, ptr %self.repack19, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt22 = extractvalue %"[c-flat]:Vec<T = u8>" %0, 2
  store i64 %.elt22, ptr %self.repack21, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp4
  %tmp6 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp7 = call {} @"[c-flat]:u8:drop"(i8 %tmp6)
  %tmp.i = add i64 %tmp4, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"[c-flat]:Vec<T = u8>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"[c-flat]:mem:free<T = u8>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"[c-flat]:u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define %"[c-flat]:Result<T = Vec<T = u8>>" @"[c-flat]:Result<T = Vec<T = u8>>:ok"(%"[c-flat]:Vec<T = u8>" %0) {
entry:
  %value = alloca %"[c-flat]:Vec<T = u8>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = u8>" %0, 0
  store ptr %.elt, ptr %value, align 8
  %value.repack2 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt3 = extractvalue %"[c-flat]:Vec<T = u8>" %0, 1
  store i64 %.elt3, ptr %value.repack2, align 8
  %value.repack4 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt5 = extractvalue %"[c-flat]:Vec<T = u8>" %0, 2
  store i64 %.elt5, ptr %value.repack4, align 8
  %tmp.unpack = load ptr, ptr %value, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = u8>" poison, ptr %tmp.unpack, 0
  %tmp.elt6 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp.unpack7 = load i64, ptr %tmp.elt6, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = u8>" %1, i64 %tmp.unpack7, 1
  %tmp.elt8 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %tmp.unpack9 = load i64, ptr %tmp.elt8, align 8
  %tmp10 = insertvalue %"[c-flat]:Vec<T = u8>" %2, i64 %tmp.unpack9, 2
  %tmp_agg = insertvalue %"[c-flat]:Result<T = Vec<T = u8>>" undef, %"[c-flat]:Vec<T = u8>" %tmp10, 0
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Vec<T = u8>>" %tmp_agg, %"[c-flat]:Error" zeroinitializer, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Vec<T = u8>>" %tmp_agg1, i1 true, 2
  ret %"[c-flat]:Result<T = Vec<T = u8>>" %tmp_constructor
}

define i1 @"[c-flat]:Result<T = Vec<T = u8>>:is_unwrappable"(ptr %0) {
entry:
  %fun_call_tmp = call i1 @"[c-flat]:Result<T = Vec<T = u8>>:is_ok"(ptr %0)
  ret i1 %fun_call_tmp
}

define i1 @"[c-flat]:Result<T = Vec<T = u8>>:is_ok"(ptr %0) {
entry:
  %tmp.is_ok = getelementptr inbounds nuw i8, ptr %0, i64 48
  %tmp1 = load i1, ptr %tmp.is_ok, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Result<T = Vec<T = u8>>:wrap_as<T_2 = Result<T = Vec<T = char>>>"(%"[c-flat]:Result<T = Vec<T = u8>>" %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Error" @"[c-flat]:Result<T = Vec<T = u8>>:unwrap_err"(%"[c-flat]:Result<T = Vec<T = u8>>" %0)
  %fun_call_tmp1 = call %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Result<T = Vec<T = char>>:err"(%"[c-flat]:Error" %fun_call_tmp)
  ret %"[c-flat]:Result<T = Vec<T = char>>" %fun_call_tmp1
}

define %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Result<T = Vec<T = char>>:err"(%"[c-flat]:Error" %0) {
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
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Vec<T = char>>" { %"[c-flat]:Vec<T = char>" zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Vec<T = char>>" %tmp_agg1, i1 false, 2
  ret %"[c-flat]:Result<T = Vec<T = char>>" %tmp_constructor
}

define %"[c-flat]:Error" @"[c-flat]:Result<T = Vec<T = u8>>:unwrap_err"(%"[c-flat]:Result<T = Vec<T = u8>>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Vec<T = u8>>", align 8
  store %"[c-flat]:Result<T = Vec<T = u8>>" %0, ptr %self, align 8
  %self.err = getelementptr inbounds nuw i8, ptr %self, i64 24
  %tmp.unpack.unpack = load ptr, ptr %self.err, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 32
  %tmp.unpack.unpack3 = load i64, ptr %tmp.unpack.elt2, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack3, 1
  %tmp.unpack.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 40
  %tmp.unpack.unpack5 = load i64, ptr %tmp.unpack.elt4, align 8
  %tmp.unpack6 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack5, 2
  %tmp1 = insertvalue %"[c-flat]:Error" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack6, 0
  ret %"[c-flat]:Error" %tmp1
}

define %"[c-flat]:Vec<T = u8>" @"[c-flat]:Result<T = Vec<T = u8>>:unwrap"(%"[c-flat]:Result<T = Vec<T = u8>>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Vec<T = u8>>", align 8
  store %"[c-flat]:Result<T = Vec<T = u8>>" %0, ptr %self, align 8
  %tmp.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = u8>" poison, ptr %tmp.unpack, 0
  %tmp.elt1 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack2 = load i64, ptr %tmp.elt1, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = u8>" %1, i64 %tmp.unpack2, 1
  %tmp.elt3 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack4 = load i64, ptr %tmp.elt3, align 8
  %tmp5 = insertvalue %"[c-flat]:Vec<T = u8>" %2, i64 %tmp.unpack4, 2
  ret %"[c-flat]:Vec<T = u8>" %tmp5
}

define %"[c-flat]:Result<T = Vec<T = char>>" @"[c-flat]:Result<T = Vec<T = char>>:ok"(%"[c-flat]:Vec<T = char>" %0) {
entry:
  %value = alloca %"[c-flat]:Vec<T = char>", align 8
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %0, 0
  store ptr %.elt, ptr %value, align 8
  %value.repack2 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt3 = extractvalue %"[c-flat]:Vec<T = char>" %0, 1
  store i64 %.elt3, ptr %value.repack2, align 8
  %value.repack4 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt5 = extractvalue %"[c-flat]:Vec<T = char>" %0, 2
  store i64 %.elt5, ptr %value.repack4, align 8
  %tmp.unpack = load ptr, ptr %value, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack, 0
  %tmp.elt6 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp.unpack7 = load i64, ptr %tmp.elt6, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack7, 1
  %tmp.elt8 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %tmp.unpack9 = load i64, ptr %tmp.elt8, align 8
  %tmp10 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack9, 2
  %tmp_agg = insertvalue %"[c-flat]:Result<T = Vec<T = char>>" undef, %"[c-flat]:Vec<T = char>" %tmp10, 0
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Vec<T = char>>" %tmp_agg, %"[c-flat]:Error" zeroinitializer, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Vec<T = char>>" %tmp_agg1, i1 true, 2
  ret %"[c-flat]:Result<T = Vec<T = char>>" %tmp_constructor
}

define i1 @"[c-flat]:Result<T = Vec<T = char>>:is_unwrappable"(ptr %0) {
entry:
  %fun_call_tmp = call i1 @"[c-flat]:Result<T = Vec<T = char>>:is_ok"(ptr %0)
  ret i1 %fun_call_tmp
}

define i1 @"[c-flat]:Result<T = Vec<T = char>>:is_ok"(ptr %0) {
entry:
  %tmp.is_ok = getelementptr inbounds nuw i8, ptr %0, i64 48
  %tmp1 = load i1, ptr %tmp.is_ok, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Vec<T = char>>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Vec<T = char>>" %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Error" @"[c-flat]:Result<T = Vec<T = char>>:unwrap_err"(%"[c-flat]:Result<T = Vec<T = char>>" %0)
  %fun_call_tmp1 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:err"(%"[c-flat]:Error" %fun_call_tmp)
  ret %"[c-flat]:Result<T = ()>" %fun_call_tmp1
}

define %"[c-flat]:Error" @"[c-flat]:Result<T = Vec<T = char>>:unwrap_err"(%"[c-flat]:Result<T = Vec<T = char>>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Vec<T = char>>", align 8
  store %"[c-flat]:Result<T = Vec<T = char>>" %0, ptr %self, align 8
  %self.err = getelementptr inbounds nuw i8, ptr %self, i64 24
  %tmp.unpack.unpack = load ptr, ptr %self.err, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 32
  %tmp.unpack.unpack3 = load i64, ptr %tmp.unpack.elt2, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack3, 1
  %tmp.unpack.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 40
  %tmp.unpack.unpack5 = load i64, ptr %tmp.unpack.elt4, align 8
  %tmp.unpack6 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack5, 2
  %tmp1 = insertvalue %"[c-flat]:Error" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack6, 0
  ret %"[c-flat]:Error" %tmp1
}

define %"[c-flat]:Vec<T = char>" @"[c-flat]:Result<T = Vec<T = char>>:unwrap"(%"[c-flat]:Result<T = Vec<T = char>>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Vec<T = char>>", align 8
  store %"[c-flat]:Result<T = Vec<T = char>>" %0, ptr %self, align 8
  %tmp.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack, 0
  %tmp.elt1 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack2 = load i64, ptr %tmp.elt1, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack2, 1
  %tmp.elt3 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack4 = load i64, ptr %tmp.elt3, align 8
  %tmp5 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack4, 2
  ret %"[c-flat]:Vec<T = char>" %tmp5
}

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
  %fun_call_tmp7 = call {} @"[c-flat]:Vec<T = char>:drop.17"(%"[c-flat]:Vec<T = char>" %tmp624)
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
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.56, i64 1 })
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
