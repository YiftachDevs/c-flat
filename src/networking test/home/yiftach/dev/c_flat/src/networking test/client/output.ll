; ModuleID = 'main_module'
source_filename = "main_module"

%"[c-flat]:Vec<T = char>" = type { ptr, i64, i64 }
%"[c-flat]:Result<T = ()>" = type { {}, %"[c-flat]:Error", i1 }
%"[c-flat]:Error" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:SliceIter<T = char>" = type { ptr, ptr, i1 }
%"[c-flat]:File" = type { %"[c-flat]:Vec<T = char>" }
%"[c-flat]:Result<T = Error>" = type { %"[c-flat]:File", %"[c-flat]:Error", i1 }
%"[c-flat]:mem:Manually_Drop<T = char>" = type { i8 }

@"[c-flat]:count" = internal global i64 0
@global_string = private unnamed_addr constant [36 x i8] c"- Welcome to the C-Flat Compiler -\0A\00", align 1
@global_string.1 = private unnamed_addr constant [27 x i8] c"Enter environment folder: \00", align 1
@global_string.2 = private unnamed_addr constant [11 x i8] c"{}/main.cf\00", align 1
@global_string.17 = private unnamed_addr constant [32 x i8] c"Error: File '{}' does not exist\00", align 1
@global_string.30 = private unnamed_addr constant [25 x i8] c"File '{}' does not exist\00", align 1
@global_string.32 = private unnamed_addr constant [32 x i8] c"zip -r c_flat_env_temp.zip \22{}\22\00", align 1
@global_string.33 = private unnamed_addr constant [6 x i8] c"Done.\00", align 1
@global_string.34 = private unnamed_addr constant [13 x i8] c"heap count: \00", align 1
@global_string.35 = private unnamed_addr constant [2 x i8] c"0\00", align 1

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
  %fun_call_tmp10 = call {} @"[c-flat]:print"({ ptr, i64 } { ptr @global_string.34, i64 12 })
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
  %tmp9 = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp8 = alloca [1 x { ptr, i64 }], align 8
  %tmp6 = alloca %"[c-flat]:SliceIter<T = char>", align 8
  %iter = alloca %"[c-flat]:SliceIter<T = char>", align 8
  %file = alloca %"[c-flat]:File", align 8
  %tmp323 = alloca %"[c-flat]:Result<T = Error>", align 8
  %err_msg = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp3 = alloca [1 x { ptr, i64 }], align 8
  %file_name = alloca %"[c-flat]:Vec<T = char>", align 8
  %tmp1 = alloca [1 x { ptr, i64 }], align 8
  %env_folder = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string, i64 35 })
  %fun_call_tmp1 = call {} @"[c-flat]:print"({ ptr, i64 } { ptr @global_string.1, i64 26 })
  %fun_call_tmp2 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:readln"()
  %fun_call_tmp2.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %env_folder, align 8
  %env_folder.repack88 = getelementptr inbounds nuw i8, ptr %env_folder, i64 8
  %fun_call_tmp2.elt89 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt89, ptr %env_folder.repack88, align 8
  %env_folder.repack90 = getelementptr inbounds nuw i8, ptr %env_folder, i64 16
  %fun_call_tmp2.elt91 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt91, ptr %env_folder.repack90, align 8
  %fun_call_tmp3 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %env_folder)
  %fun_call_tmp3.elt = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  store ptr %fun_call_tmp3.elt, ptr %tmp1, align 8
  %tmp1.repack92 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp3.elt93 = extractvalue { ptr, i64 } %fun_call_tmp3, 1
  store i64 %fun_call_tmp3.elt93, ptr %tmp1.repack92, align 8
  %ptr.i83 = insertvalue { ptr, i64 } undef, ptr %tmp1, 0
  %len.i84 = insertvalue { ptr, i64 } %ptr.i83, i64 1, 1
  %fun_call_tmp5 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:format"({ ptr, i64 } { ptr @global_string.2, i64 10 }, { ptr, i64 } %len.i84)
  %fun_call_tmp5.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp5, 0
  store ptr %fun_call_tmp5.elt, ptr %file_name, align 8
  %file_name.repack94 = getelementptr inbounds nuw i8, ptr %file_name, i64 8
  %fun_call_tmp5.elt95 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp5, 1
  store i64 %fun_call_tmp5.elt95, ptr %file_name.repack94, align 8
  %file_name.repack96 = getelementptr inbounds nuw i8, ptr %file_name, i64 16
  %fun_call_tmp5.elt97 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp5, 2
  store i64 %fun_call_tmp5.elt97, ptr %file_name.repack96, align 8
  %fun_call_tmp6 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %file_name)
  %fun_call_tmp7 = call i1 @"[c-flat]:Error:exists"({ ptr, i64 } %fun_call_tmp6)
  br i1 %fun_call_tmp7, label %else, label %then

common.ret:                                       ; preds = %else26, %then25, %then
  %common.ret.op = phi %"[c-flat]:Result<T = ()>" [ %fun_call_tmp12, %then ], [ %fun_call_tmp49, %then25 ], [ %fun_call_tmp27, %else26 ]
  ret %"[c-flat]:Result<T = ()>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp9 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %file_name)
  %fun_call_tmp9.elt = extractvalue { ptr, i64 } %fun_call_tmp9, 0
  store ptr %fun_call_tmp9.elt, ptr %tmp3, align 8
  %tmp3.repack151 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %fun_call_tmp9.elt152 = extractvalue { ptr, i64 } %fun_call_tmp9, 1
  store i64 %fun_call_tmp9.elt152, ptr %tmp3.repack151, align 8
  %ptr.i80 = insertvalue { ptr, i64 } undef, ptr %tmp3, 0
  %len.i81 = insertvalue { ptr, i64 } %ptr.i80, i64 1, 1
  %fun_call_tmp11 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:format"({ ptr, i64 } { ptr @global_string.17, i64 31 }, { ptr, i64 } %len.i81)
  %fun_call_tmp11.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp11, 0
  store ptr %fun_call_tmp11.elt, ptr %err_msg, align 8
  %err_msg.repack153 = getelementptr inbounds nuw i8, ptr %err_msg, i64 8
  %fun_call_tmp11.elt154 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp11, 1
  store i64 %fun_call_tmp11.elt154, ptr %err_msg.repack153, align 8
  %err_msg.repack155 = getelementptr inbounds nuw i8, ptr %err_msg, i64 16
  %fun_call_tmp11.elt156 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp11, 2
  store i64 %fun_call_tmp11.elt156, ptr %err_msg.repack155, align 8
  %tmp.unpack = load ptr, ptr %err_msg, align 8
  %0 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack, 0
  %tmp.elt157 = getelementptr inbounds nuw i8, ptr %err_msg, i64 8
  %tmp.unpack158 = load i64, ptr %tmp.elt157, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" %0, i64 %tmp.unpack158, 1
  %tmp.elt159 = getelementptr inbounds nuw i8, ptr %err_msg, i64 16
  %tmp.unpack160 = load i64, ptr %tmp.elt159, align 8
  %tmp161 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack160, 2
  %tmp_constructor = insertvalue %"[c-flat]:Error" undef, %"[c-flat]:Vec<T = char>" %tmp161, 0
  %fun_call_tmp12 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:err"(%"[c-flat]:Error" %tmp_constructor)
  %tmp13.unpack.unpack = load ptr, ptr %tmp3, align 8
  %2 = insertvalue { ptr, i64 } poison, ptr %tmp13.unpack.unpack, 0
  %tmp13.unpack.elt163 = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp13.unpack.unpack164 = load i64, ptr %tmp13.unpack.elt163, align 8
  %tmp13.unpack165 = insertvalue { ptr, i64 } %2, i64 %tmp13.unpack.unpack164, 1
  %tmp13162 = insertvalue [1 x { ptr, i64 }] poison, { ptr, i64 } %tmp13.unpack165, 0
  %fun_call_tmp14 = call {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %tmp13162)
  %tmp15.unpack = load ptr, ptr %file_name, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp15.unpack, 0
  %tmp15.elt166 = getelementptr inbounds nuw i8, ptr %file_name, i64 8
  %tmp15.unpack167 = load i64, ptr %tmp15.elt166, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp15.unpack167, 1
  %tmp15.elt168 = getelementptr inbounds nuw i8, ptr %file_name, i64 16
  %tmp15.unpack169 = load i64, ptr %tmp15.elt168, align 8
  %tmp15170 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp15.unpack169, 2
  %fun_call_tmp16 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp15170)
  %tmp17.unpack.unpack = load ptr, ptr %tmp1, align 8
  %5 = insertvalue { ptr, i64 } poison, ptr %tmp17.unpack.unpack, 0
  %tmp17.unpack.elt172 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp17.unpack.unpack173 = load i64, ptr %tmp17.unpack.elt172, align 8
  %tmp17.unpack174 = insertvalue { ptr, i64 } %5, i64 %tmp17.unpack.unpack173, 1
  %tmp17171 = insertvalue [1 x { ptr, i64 }] poison, { ptr, i64 } %tmp17.unpack174, 0
  %fun_call_tmp18 = call {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %tmp17171)
  %tmp19.unpack = load ptr, ptr %env_folder, align 8
  %6 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp19.unpack, 0
  %tmp19.elt175 = getelementptr inbounds nuw i8, ptr %env_folder, i64 8
  %tmp19.unpack176 = load i64, ptr %tmp19.elt175, align 8
  %7 = insertvalue %"[c-flat]:Vec<T = char>" %6, i64 %tmp19.unpack176, 1
  %tmp19.elt177 = getelementptr inbounds nuw i8, ptr %env_folder, i64 16
  %tmp19.unpack178 = load i64, ptr %tmp19.elt177, align 8
  %tmp19179 = insertvalue %"[c-flat]:Vec<T = char>" %7, i64 %tmp19.unpack178, 2
  %fun_call_tmp20 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp19179)
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp21 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %file_name)
  %fun_call_tmp22 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Error:open"({ ptr, i64 } %fun_call_tmp21)
  store %"[c-flat]:Result<T = Error>" %fun_call_tmp22, ptr %tmp323, align 8
  %fun_call_tmp24 = call i1 @"[c-flat]:Result<T = Error>:is_unwrappable"(ptr nonnull %tmp323)
  br i1 %fun_call_tmp24, label %then25, label %else26

then25:                                           ; preds = %else
  %fun_call_tmp34 = call %"[c-flat]:File" @"[c-flat]:Result<T = Error>:unwrap"(%"[c-flat]:Result<T = Error>" %fun_call_tmp22)
  %8 = extractvalue %"[c-flat]:File" %fun_call_tmp34, 0
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %8, 0
  store ptr %.elt, ptr %file, align 8
  %file.repack112 = getelementptr inbounds nuw i8, ptr %file, i64 8
  %.elt113 = extractvalue %"[c-flat]:Vec<T = char>" %8, 1
  store i64 %.elt113, ptr %file.repack112, align 8
  %file.repack114 = getelementptr inbounds nuw i8, ptr %file, i64 16
  %.elt115 = extractvalue %"[c-flat]:Vec<T = char>" %8, 2
  store i64 %.elt115, ptr %file.repack114, align 8
  %fun_call_tmp35 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref"(ptr nonnull %env_folder)
  %fun_call_tmp36 = call %"[c-flat]:SliceIter<T = char>" @"[c-flat]:[char]:iter"({ ptr, i64 } %fun_call_tmp35)
  %fun_call_tmp37 = call %"[c-flat]:SliceIter<T = char>" @"[c-flat]:Vec<T = char>:rev"(%"[c-flat]:SliceIter<T = char>" %fun_call_tmp36)
  store %"[c-flat]:SliceIter<T = char>" %fun_call_tmp37, ptr %iter, align 8
  %fun_call_tmp38 = call %"[c-flat]:SliceIter<T = char>" @"[c-flat]:Vec<T = char>:until"(ptr nonnull %iter, i8 47)
  %fun_call_tmp39 = call %"[c-flat]:SliceIter<T = char>" @"[c-flat]:Vec<T = char>:skip"(%"[c-flat]:SliceIter<T = char>" %fun_call_tmp38, i64 1)
  store %"[c-flat]:SliceIter<T = char>" %fun_call_tmp39, ptr %tmp6, align 8
  %fun_call_tmp40 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice.31"(ptr nonnull %tmp6)
  %fun_call_tmp41 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice.31"(ptr nonnull %iter)
  %fun_call_tmp42 = call i32 @"[c-flat]:set_working_dir"({ ptr, i64 } %fun_call_tmp41)
  %fun_call_tmp43 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice.31"(ptr nonnull %iter)
  %fun_call_tmp43.elt = extractvalue { ptr, i64 } %fun_call_tmp43, 0
  store ptr %fun_call_tmp43.elt, ptr %tmp8, align 8
  %tmp8.repack116 = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  %fun_call_tmp43.elt117 = extractvalue { ptr, i64 } %fun_call_tmp43, 1
  store i64 %fun_call_tmp43.elt117, ptr %tmp8.repack116, align 8
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp8, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 1, 1
  %fun_call_tmp45 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:format"({ ptr, i64 } { ptr @global_string.32, i64 31 }, { ptr, i64 } %len.i)
  %fun_call_tmp45.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp45, 0
  store ptr %fun_call_tmp45.elt, ptr %tmp9, align 8
  %tmp9.repack118 = getelementptr inbounds nuw i8, ptr %tmp9, i64 8
  %fun_call_tmp45.elt119 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp45, 1
  store i64 %fun_call_tmp45.elt119, ptr %tmp9.repack118, align 8
  %tmp9.repack120 = getelementptr inbounds nuw i8, ptr %tmp9, i64 16
  %fun_call_tmp45.elt121 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp45, 2
  store i64 %fun_call_tmp45.elt121, ptr %tmp9.repack120, align 8
  %fun_call_tmp46 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp9)
  %fun_call_tmp47 = call i32 @"[c-flat]:system"({ ptr, i64 } %fun_call_tmp46)
  %fun_call_tmp48 = call {} @"[c-flat]:println"({ ptr, i64 } { ptr @global_string.33, i64 5 })
  %fun_call_tmp49 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:ok"({} zeroinitializer)
  %tmp50.unpack = load ptr, ptr %tmp9, align 8
  %9 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp50.unpack, 0
  %tmp50.elt122 = getelementptr inbounds nuw i8, ptr %tmp9, i64 8
  %tmp50.unpack123 = load i64, ptr %tmp50.elt122, align 8
  %10 = insertvalue %"[c-flat]:Vec<T = char>" %9, i64 %tmp50.unpack123, 1
  %tmp50.elt124 = getelementptr inbounds nuw i8, ptr %tmp9, i64 16
  %tmp50.unpack125 = load i64, ptr %tmp50.elt124, align 8
  %tmp50126 = insertvalue %"[c-flat]:Vec<T = char>" %10, i64 %tmp50.unpack125, 2
  %fun_call_tmp51 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp50126)
  %tmp52.unpack.unpack = load ptr, ptr %tmp8, align 8
  %11 = insertvalue { ptr, i64 } poison, ptr %tmp52.unpack.unpack, 0
  %tmp52.unpack.elt128 = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  %tmp52.unpack.unpack129 = load i64, ptr %tmp52.unpack.elt128, align 8
  %tmp52.unpack130 = insertvalue { ptr, i64 } %11, i64 %tmp52.unpack.unpack129, 1
  %tmp52127 = insertvalue [1 x { ptr, i64 }] poison, { ptr, i64 } %tmp52.unpack130, 0
  %fun_call_tmp53 = call {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %tmp52127)
  %12 = getelementptr inbounds nuw i8, ptr %tmp6, i64 16
  %tmp.i74 = load i1, ptr %12, align 1
  %fun_call_tmp.i75 = call {} @"[c-flat]:bool:drop"(i1 %tmp.i74)
  %13 = getelementptr inbounds nuw i8, ptr %iter, i64 16
  %tmp.i70 = load i1, ptr %13, align 1
  %fun_call_tmp.i = call {} @"[c-flat]:bool:drop"(i1 %tmp.i70)
  %tmp60.unpack.unpack = load ptr, ptr %file, align 8
  %14 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp60.unpack.unpack, 0
  %tmp60.unpack.elt132 = getelementptr inbounds nuw i8, ptr %file, i64 8
  %tmp60.unpack.unpack133 = load i64, ptr %tmp60.unpack.elt132, align 8
  %15 = insertvalue %"[c-flat]:Vec<T = char>" %14, i64 %tmp60.unpack.unpack133, 1
  %tmp60.unpack.elt134 = getelementptr inbounds nuw i8, ptr %file, i64 16
  %tmp60.unpack.unpack135 = load i64, ptr %tmp60.unpack.elt134, align 8
  %tmp60.unpack136 = insertvalue %"[c-flat]:Vec<T = char>" %15, i64 %tmp60.unpack.unpack135, 2
  %fun_call_tmp.i87 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp60.unpack136)
  %tmp62.unpack = load ptr, ptr %file_name, align 8
  %16 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp62.unpack, 0
  %tmp62.elt137 = getelementptr inbounds nuw i8, ptr %file_name, i64 8
  %tmp62.unpack138 = load i64, ptr %tmp62.elt137, align 8
  %17 = insertvalue %"[c-flat]:Vec<T = char>" %16, i64 %tmp62.unpack138, 1
  %tmp62.elt139 = getelementptr inbounds nuw i8, ptr %file_name, i64 16
  %tmp62.unpack140 = load i64, ptr %tmp62.elt139, align 8
  %tmp62141 = insertvalue %"[c-flat]:Vec<T = char>" %17, i64 %tmp62.unpack140, 2
  %fun_call_tmp63 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp62141)
  %tmp64.unpack.unpack = load ptr, ptr %tmp1, align 8
  %18 = insertvalue { ptr, i64 } poison, ptr %tmp64.unpack.unpack, 0
  %tmp64.unpack.elt143 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp64.unpack.unpack144 = load i64, ptr %tmp64.unpack.elt143, align 8
  %tmp64.unpack145 = insertvalue { ptr, i64 } %18, i64 %tmp64.unpack.unpack144, 1
  %tmp64142 = insertvalue [1 x { ptr, i64 }] poison, { ptr, i64 } %tmp64.unpack145, 0
  %fun_call_tmp65 = call {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %tmp64142)
  %tmp66.unpack = load ptr, ptr %env_folder, align 8
  %19 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp66.unpack, 0
  %tmp66.elt146 = getelementptr inbounds nuw i8, ptr %env_folder, i64 8
  %tmp66.unpack147 = load i64, ptr %tmp66.elt146, align 8
  %20 = insertvalue %"[c-flat]:Vec<T = char>" %19, i64 %tmp66.unpack147, 1
  %tmp66.elt148 = getelementptr inbounds nuw i8, ptr %env_folder, i64 16
  %tmp66.unpack149 = load i64, ptr %tmp66.elt148, align 8
  %tmp66150 = insertvalue %"[c-flat]:Vec<T = char>" %20, i64 %tmp66.unpack149, 2
  %fun_call_tmp67 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp66150)
  br label %common.ret

else26:                                           ; preds = %else
  %fun_call_tmp27 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Error>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Error>" %fun_call_tmp22)
  %tmp28.unpack = load ptr, ptr %file_name, align 8
  %21 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp28.unpack, 0
  %tmp28.elt98 = getelementptr inbounds nuw i8, ptr %file_name, i64 8
  %tmp28.unpack99 = load i64, ptr %tmp28.elt98, align 8
  %22 = insertvalue %"[c-flat]:Vec<T = char>" %21, i64 %tmp28.unpack99, 1
  %tmp28.elt100 = getelementptr inbounds nuw i8, ptr %file_name, i64 16
  %tmp28.unpack101 = load i64, ptr %tmp28.elt100, align 8
  %tmp28102 = insertvalue %"[c-flat]:Vec<T = char>" %22, i64 %tmp28.unpack101, 2
  %fun_call_tmp29 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp28102)
  %tmp30.unpack.unpack = load ptr, ptr %tmp1, align 8
  %23 = insertvalue { ptr, i64 } poison, ptr %tmp30.unpack.unpack, 0
  %tmp30.unpack.elt104 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp30.unpack.unpack105 = load i64, ptr %tmp30.unpack.elt104, align 8
  %tmp30.unpack106 = insertvalue { ptr, i64 } %23, i64 %tmp30.unpack.unpack105, 1
  %tmp30103 = insertvalue [1 x { ptr, i64 }] poison, { ptr, i64 } %tmp30.unpack106, 0
  %fun_call_tmp31 = call {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %tmp30103)
  %tmp32.unpack = load ptr, ptr %env_folder, align 8
  %24 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp32.unpack, 0
  %tmp32.elt107 = getelementptr inbounds nuw i8, ptr %env_folder, i64 8
  %tmp32.unpack108 = load i64, ptr %tmp32.elt107, align 8
  %25 = insertvalue %"[c-flat]:Vec<T = char>" %24, i64 %tmp32.unpack108, 1
  %tmp32.elt109 = getelementptr inbounds nuw i8, ptr %env_folder, i64 16
  %tmp32.unpack110 = load i64, ptr %tmp32.elt109, align 8
  %tmp32111 = insertvalue %"[c-flat]:Vec<T = char>" %25, i64 %tmp32.unpack110, 2
  %fun_call_tmp33 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp32111)
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

define %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:format"({ ptr, i64 } %0, { ptr, i64 } %1) {
entry:
  %tmp9 = alloca i8, align 1
  %tmp8 = alloca i8, align 1
  %next_chr = alloca i8, align 1
  %tmp6 = alloca i8, align 1
  %chr = alloca i8, align 1
  %idx = alloca i64, align 8
  %iter = alloca %"[c-flat]:SliceIter<T = char>", align 8
  %result = alloca %"[c-flat]:Vec<T = char>", align 8
  %values = alloca { ptr, i64 }, align 8
  %self = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack111 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt112 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt112, ptr %self.repack111, align 8
  %.elt113 = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt113, ptr %values, align 8
  %values.repack114 = getelementptr inbounds nuw i8, ptr %values, i64 8
  %.elt115 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt115, ptr %values.repack114, align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack116 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt117 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt117, ptr %result.repack116, align 8
  %result.repack118 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt119 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt119, ptr %result.repack118, align 8
  %tmp.unpack = load ptr, ptr %self, align 8
  %2 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt120 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack121 = load i64, ptr %tmp.elt120, align 8
  %tmp122 = insertvalue { ptr, i64 } %2, i64 %tmp.unpack121, 1
  %fun_call_tmp1 = call %"[c-flat]:SliceIter<T = char>" @"[c-flat]:[char]:iter"({ ptr, i64 } %tmp122)
  store %"[c-flat]:SliceIter<T = char>" %fun_call_tmp1, ptr %iter, align 8
  store i64 0, ptr %idx, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call ptr @"[c-flat]:Vec<T = char>:next"(ptr nonnull %iter)
  %tmp4 = load i8, ptr %fun_call_tmp3, align 1
  store i8 %tmp4, ptr %chr, align 1
  store i8 123, ptr %tmp6, align 1
  %tmp.i90 = load i8, ptr %chr, align 1
  %tmp2.i92 = icmp eq i8 %tmp.i90, 123
  %fun_call_tmp10 = call i1 @"[c-flat]:Vec<T = char>:has_next"(ptr nonnull %iter)
  %tmp.i98 = and i1 %tmp2.i92, %fun_call_tmp10
  br i1 %tmp.i98, label %then6, label %merge5

cond:                                             ; preds = %merge5, %then39, %then15, %entry
  %fun_call_tmp2 = call i1 @"[c-flat]:Vec<T = char>:has_next"(ptr nonnull %iter)
  br i1 %fun_call_tmp2, label %then, label %else

else:                                             ; preds = %cond
  %tmp73.unpack = load ptr, ptr %result, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp73.unpack, 0
  %tmp73.elt123 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp73.unpack124 = load i64, ptr %tmp73.elt123, align 8
  %4 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp73.unpack124, 1
  %tmp73.elt125 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp73.unpack126 = load i64, ptr %tmp73.elt125, align 8
  %tmp73127 = insertvalue %"[c-flat]:Vec<T = char>" %4, i64 %tmp73.unpack126, 2
  %tmp74 = load i64, ptr %idx, align 4
  %fun_call_tmp75 = call {} @"[c-flat]:u64:drop"(i64 %tmp74)
  %5 = getelementptr inbounds nuw i8, ptr %iter, i64 16
  %tmp.i106 = load i1, ptr %5, align 1
  %fun_call_tmp.i = call {} @"[c-flat]:bool:drop"(i1 %tmp.i106)
  ret %"[c-flat]:Vec<T = char>" %tmp73127

merge5:                                           ; preds = %cond40, %then
  %tmp60 = load i8, ptr %chr, align 1
  %fun_call_tmp61 = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %result, i8 %tmp60)
  %tmp62 = load i8, ptr %tmp9, align 1
  %fun_call_tmp63 = call {} @"[c-flat]:char:drop"(i8 %tmp62)
  %tmp64 = load i8, ptr %tmp8, align 1
  %fun_call_tmp65 = call {} @"[c-flat]:char:drop"(i8 %tmp64)
  %tmp66 = load i8, ptr %next_chr, align 1
  %fun_call_tmp67 = call {} @"[c-flat]:char:drop"(i8 %tmp66)
  %tmp68 = load i8, ptr %tmp6, align 1
  %fun_call_tmp69 = call {} @"[c-flat]:char:drop"(i8 %tmp68)
  %tmp70 = load i8, ptr %chr, align 1
  %fun_call_tmp71 = call {} @"[c-flat]:char:drop"(i8 %tmp70)
  br label %cond

then6:                                            ; preds = %then
  %fun_call_tmp12 = call ptr @"[c-flat]:Vec<T = char>:next"(ptr nonnull %iter)
  %tmp13 = load i8, ptr %fun_call_tmp12, align 1
  store i8 %tmp13, ptr %next_chr, align 1
  store i8 125, ptr %tmp8, align 1
  %tmp.i85 = load i8, ptr %next_chr, align 1
  %tmp2.i87 = icmp eq i8 %tmp.i85, 125
  %tmp19 = load i64, ptr %idx, align 4
  %tmp20.elt128 = getelementptr inbounds nuw i8, ptr %values, i64 8
  %tmp20.unpack129 = load i64, ptr %tmp20.elt128, align 8
  %tmp.i104 = icmp ult i64 %tmp19, %tmp20.unpack129
  %tmp.i95 = and i1 %tmp2.i87, %tmp.i104
  br i1 %tmp.i95, label %then15, label %cond40

then15:                                           ; preds = %then6
  %tmp24.unpack = load ptr, ptr %values, align 8
  %tmp25 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr { ptr, i64 }, ptr %tmp24.unpack, i64 %tmp25
  %tmp27.unpack = load ptr, ptr %tmp_index.i, align 8
  %6 = insertvalue { ptr, i64 } poison, ptr %tmp27.unpack, 0
  %tmp27.elt134 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 8
  %tmp27.unpack135 = load i64, ptr %tmp27.elt134, align 8
  %tmp27136 = insertvalue { ptr, i64 } %6, i64 %tmp27.unpack135, 1
  %fun_call_tmp28 = call {} @"[c-flat]:Vec<T = char>:extend_from_slice"(ptr nonnull %result, { ptr, i64 } %tmp27136)
  %tmp29 = load i64, ptr %idx, align 4
  %tmp.i101 = add i64 %tmp29, 1
  store i64 %tmp.i101, ptr %idx, align 4
  %tmp31 = load i8, ptr %tmp8, align 1
  %fun_call_tmp32 = call {} @"[c-flat]:char:drop"(i8 %tmp31)
  %tmp33 = load i8, ptr %next_chr, align 1
  %fun_call_tmp34 = call {} @"[c-flat]:char:drop"(i8 %tmp33)
  %tmp35 = load i8, ptr %tmp6, align 1
  %fun_call_tmp36 = call {} @"[c-flat]:char:drop"(i8 %tmp35)
  %tmp37 = load i8, ptr %chr, align 1
  %fun_call_tmp38 = call {} @"[c-flat]:char:drop"(i8 %tmp37)
  br label %cond

then39:                                           ; preds = %cond40
  %tmp44 = load i8, ptr %chr, align 1
  %fun_call_tmp45 = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %result, i8 %tmp44)
  %tmp46 = load i8, ptr %next_chr, align 1
  %fun_call_tmp47 = call {} @"[c-flat]:Vec<T = char>:push"(ptr nonnull %result, i8 %tmp46)
  %tmp48 = load i8, ptr %tmp9, align 1
  %fun_call_tmp49 = call {} @"[c-flat]:char:drop"(i8 %tmp48)
  %tmp50 = load i8, ptr %tmp8, align 1
  %fun_call_tmp51 = call {} @"[c-flat]:char:drop"(i8 %tmp50)
  %fun_call_tmp53 = call {} @"[c-flat]:char:drop"(i8 %tmp46)
  %tmp54 = load i8, ptr %tmp6, align 1
  %fun_call_tmp55 = call {} @"[c-flat]:char:drop"(i8 %tmp54)
  %tmp56 = load i8, ptr %chr, align 1
  %fun_call_tmp57 = call {} @"[c-flat]:char:drop"(i8 %tmp56)
  br label %cond

cond40:                                           ; preds = %then6
  store i8 123, ptr %tmp9, align 1
  %tmp.i = load i8, ptr %next_chr, align 1
  %tmp2.i.not = icmp eq i8 %tmp.i, 123
  br i1 %tmp2.i.not, label %merge5, label %then39
}

define %"[c-flat]:SliceIter<T = char>" @"[c-flat]:[char]:iter"({ ptr, i64 } %0) {
entry:
  %.elt22 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i = add i64 %.elt22, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"[c-flat]:SliceIter<T = char>" undef, ptr %ref.i, 0
  %tmp_agg9 = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg, ptr %cast7, 1
  %tmp_constructor = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg9, i1 false, 2
  ret %"[c-flat]:SliceIter<T = char>" %tmp_constructor
}

define {} @"[c-flat]:bool:drop"(i1 %0) {
entry:
  ret {} zeroinitializer
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
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp = load ptr, ptr %self, align 8
  %tmp.rev = getelementptr inbounds nuw i8, ptr %tmp, i64 16
  %tmp1 = load i1, ptr %tmp.rev, align 1
  br i1 %tmp1, label %then, label %then11

merge:                                            ; preds = %then11, %then
  %result26 = phi ptr [ %tmp8, %then ], [ %tmp13, %then11 ]
  ret ptr %result26

then:                                             ; preds = %entry
  %tmp2 = load ptr, ptr %self, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp3.end = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp4 = load ptr, ptr %tmp3.end, align 8
  %cast = ptrtoint ptr %tmp4 to i64
  %tmp.i30 = add i64 %cast, -1
  %cast6 = inttoptr i64 %tmp.i30 to ptr
  store ptr %cast6, ptr %tmp2.end, align 8
  %tmp7 = load ptr, ptr %self, align 8
  %tmp7.end = getelementptr inbounds nuw i8, ptr %tmp7, i64 8
  %tmp8 = load ptr, ptr %tmp7.end, align 8
  br label %merge

then11:                                           ; preds = %entry
  %tmp12 = load ptr, ptr %self, align 8
  %tmp13 = load ptr, ptr %tmp12, align 8
  %cast17 = ptrtoint ptr %tmp13 to i64
  %tmp.i = add i64 %cast17, 1
  %cast20 = inttoptr i64 %tmp.i to ptr
  store ptr %cast20, ptr %tmp12, align 8
  br label %merge
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
  store %"[c-flat]:SliceIter<T = char>" %fun_call_tmp6, ptr %tmp_iter2, align 8
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
  %tmp14.elt26 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %tmp14.unpack27 = load i64, ptr %tmp14.elt26, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp14.unpack27, 1
  %tmp14.elt28 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %tmp14.unpack29 = load i64, ptr %tmp14.elt28, align 8
  %tmp1430 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp14.unpack29, 2
  %3 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 16
  %tmp.i = load i1, ptr %3, align 1
  %fun_call_tmp.i = call {} @"[c-flat]:bool:drop"(i1 %tmp.i)
  ret %"[c-flat]:Vec<T = char>" %tmp1430
}

define i1 @"[c-flat]:Error:exists"({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp1, align 8
  %tmp1.repack15 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp1.elt16 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt16, ptr %tmp1.repack15, align 8
  %tmp1.repack17 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp1.elt18 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt18, ptr %tmp1.repack17, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call i1 @c_file_exists(ptr %ref.i)
  %tmp5.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp5.unpack, 0
  %tmp5.elt19 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp5.unpack20 = load i64, ptr %tmp5.elt19, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp5.unpack20, 1
  %tmp5.elt21 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp5.unpack22 = load i64, ptr %tmp5.elt21, align 8
  %tmp523 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp5.unpack22, 2
  %fun_call_tmp6 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp523)
  ret i1 %fun_call_tmp4
}

declare i1 @c_file_exists(ptr)

define {} @"[c-flat]:():clone"(ptr %0) {
entry:
  ret {} poison
}

define i1 @"[c-flat]:bool:clone"(ptr %0) {
entry:
  %tmp1 = load i1, ptr %0, align 1
  ret i1 %tmp1
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

define {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %0) {
entry:
  %idx = alloca i64, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp5 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp5, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp.i10 = icmp eq i64 %storemerge, 0
  br i1 %tmp.i10, label %then, label %else

else:                                             ; preds = %cond
  ret {} zeroinitializer
}

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Error:open"({ ptr, i64 } %0) {
entry:
  %tmp18 = alloca [1 x { ptr, i64 }], align 8
  %path = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %path, align 8
  %path.repack19 = getelementptr inbounds nuw i8, ptr %path, i64 8
  %.elt20 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt20, ptr %path.repack19, align 8
  %tmp.unpack = load ptr, ptr %path, align 8
  %1 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt21 = getelementptr inbounds nuw i8, ptr %path, i64 8
  %tmp.unpack22 = load i64, ptr %tmp.elt21, align 8
  %tmp23 = insertvalue { ptr, i64 } %1, i64 %tmp.unpack22, 1
  %fun_call_tmp = call i1 @"[c-flat]:Error:exists"({ ptr, i64 } %tmp23)
  br i1 %fun_call_tmp, label %then, label %then6

merge:                                            ; preds = %then6, %then
  %result = phi %"[c-flat]:Result<T = Error>" [ %fun_call_tmp3, %then ], [ %fun_call_tmp12, %then6 ]
  ret %"[c-flat]:Result<T = Error>" %result

then:                                             ; preds = %entry
  %tmp1.unpack = load ptr, ptr %path, align 8
  %2 = insertvalue { ptr, i64 } poison, ptr %tmp1.unpack, 0
  %tmp1.elt33 = getelementptr inbounds nuw i8, ptr %path, i64 8
  %tmp1.unpack34 = load i64, ptr %tmp1.elt33, align 8
  %tmp135 = insertvalue { ptr, i64 } %2, i64 %tmp1.unpack34, 1
  %fun_call_tmp2 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %tmp135)
  %tmp_constructor = insertvalue %"[c-flat]:File" undef, %"[c-flat]:Vec<T = char>" %fun_call_tmp2, 0
  %fun_call_tmp3 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:ok"(%"[c-flat]:File" %tmp_constructor)
  br label %merge

then6:                                            ; preds = %entry
  %tmp7.unpack = load ptr, ptr %path, align 8
  %tmp7.elt24 = getelementptr inbounds nuw i8, ptr %path, i64 8
  %tmp7.unpack25 = load i64, ptr %tmp7.elt24, align 8
  store ptr %tmp7.unpack, ptr %tmp18, align 8
  %tmp18.repack27 = getelementptr inbounds nuw i8, ptr %tmp18, i64 8
  store i64 %tmp7.unpack25, ptr %tmp18.repack27, align 8
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp18, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 1, 1
  %fun_call_tmp10 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:format"({ ptr, i64 } { ptr @global_string.30, i64 24 }, { ptr, i64 } %len.i)
  %tmp_constructor11 = insertvalue %"[c-flat]:Error" undef, %"[c-flat]:Vec<T = char>" %fun_call_tmp10, 0
  %fun_call_tmp12 = call %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:err"(%"[c-flat]:Error" %tmp_constructor11)
  %tmp13.unpack.unpack = load ptr, ptr %tmp18, align 8
  %3 = insertvalue { ptr, i64 } poison, ptr %tmp13.unpack.unpack, 0
  %tmp13.unpack.elt30 = getelementptr inbounds nuw i8, ptr %tmp18, i64 8
  %tmp13.unpack.unpack31 = load i64, ptr %tmp13.unpack.elt30, align 8
  %tmp13.unpack32 = insertvalue { ptr, i64 } %3, i64 %tmp13.unpack.unpack31, 1
  %tmp1329 = insertvalue [1 x { ptr, i64 }] poison, { ptr, i64 } %tmp13.unpack32, 0
  %fun_call_tmp14 = call {} @"[c-flat]:[&[char]; 1]:drop"([1 x { ptr, i64 }] %tmp1329)
  br label %merge
}

define %"[c-flat]:Result<T = Error>" @"[c-flat]:Result<T = Error>:ok"(%"[c-flat]:File" %0) {
entry:
  %value = alloca %"[c-flat]:File", align 8
  %1 = extractvalue %"[c-flat]:File" %0, 0
  %.elt = extractvalue %"[c-flat]:Vec<T = char>" %1, 0
  store ptr %.elt, ptr %value, align 8
  %value.repack2 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt3 = extractvalue %"[c-flat]:Vec<T = char>" %1, 1
  store i64 %.elt3, ptr %value.repack2, align 8
  %value.repack4 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %.elt5 = extractvalue %"[c-flat]:Vec<T = char>" %1, 2
  store i64 %.elt5, ptr %value.repack4, align 8
  %tmp.unpack.unpack = load ptr, ptr %value, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt7 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %tmp.unpack.unpack8 = load i64, ptr %tmp.unpack.elt7, align 8
  %3 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack8, 1
  %tmp.unpack.elt9 = getelementptr inbounds nuw i8, ptr %value, i64 16
  %tmp.unpack.unpack10 = load i64, ptr %tmp.unpack.elt9, align 8
  %tmp.unpack11 = insertvalue %"[c-flat]:Vec<T = char>" %3, i64 %tmp.unpack.unpack10, 2
  %tmp6 = insertvalue %"[c-flat]:File" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack11, 0
  %tmp_agg = insertvalue %"[c-flat]:Result<T = Error>" undef, %"[c-flat]:File" %tmp6, 0
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Error>" %tmp_agg, %"[c-flat]:Error" zeroinitializer, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Error>" %tmp_agg1, i1 true, 2
  ret %"[c-flat]:Result<T = Error>" %tmp_constructor
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
  %tmp_agg1 = insertvalue %"[c-flat]:Result<T = Error>" { %"[c-flat]:File" zeroinitializer, %"[c-flat]:Error" undef, i1 undef }, %"[c-flat]:Error" %tmp6, 1
  %tmp_constructor = insertvalue %"[c-flat]:Result<T = Error>" %tmp_agg1, i1 false, 2
  ret %"[c-flat]:Result<T = Error>" %tmp_constructor
}

define i1 @"[c-flat]:Result<T = Error>:is_unwrappable"(ptr %0) {
entry:
  %fun_call_tmp = call i1 @"[c-flat]:Result<T = Error>:is_ok"(ptr %0)
  ret i1 %fun_call_tmp
}

define i1 @"[c-flat]:Result<T = Error>:is_ok"(ptr %0) {
entry:
  %tmp.is_ok = getelementptr inbounds nuw i8, ptr %0, i64 48
  %tmp1 = load i1, ptr %tmp.is_ok, align 1
  ret i1 %tmp1
}

define %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = Error>:wrap_as<T_2 = Result<T = ()>>"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %fun_call_tmp = call %"[c-flat]:Error" @"[c-flat]:Result<T = Error>:unwrap_err"(%"[c-flat]:Result<T = Error>" %0)
  %fun_call_tmp1 = call %"[c-flat]:Result<T = ()>" @"[c-flat]:Result<T = ()>:err"(%"[c-flat]:Error" %fun_call_tmp)
  ret %"[c-flat]:Result<T = ()>" %fun_call_tmp1
}

define %"[c-flat]:Error" @"[c-flat]:Result<T = Error>:unwrap_err"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Error>", align 8
  store %"[c-flat]:Result<T = Error>" %0, ptr %self, align 8
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

define %"[c-flat]:File" @"[c-flat]:Result<T = Error>:unwrap"(%"[c-flat]:Result<T = Error>" %0) {
entry:
  %self = alloca %"[c-flat]:Result<T = Error>", align 8
  store %"[c-flat]:Result<T = Error>" %0, ptr %self, align 8
  %tmp.unpack.unpack = load ptr, ptr %self, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp.unpack.unpack, 0
  %tmp.unpack.elt2 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack.unpack3 = load i64, ptr %tmp.unpack.elt2, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp.unpack.unpack3, 1
  %tmp.unpack.elt4 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp.unpack.unpack5 = load i64, ptr %tmp.unpack.elt4, align 8
  %tmp.unpack6 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp.unpack.unpack5, 2
  %tmp1 = insertvalue %"[c-flat]:File" poison, %"[c-flat]:Vec<T = char>" %tmp.unpack6, 0
  ret %"[c-flat]:File" %tmp1
}

define %"[c-flat]:SliceIter<T = char>" @"[c-flat]:Vec<T = char>:rev"(%"[c-flat]:SliceIter<T = char>" %0) {
entry:
  %self = alloca %"[c-flat]:SliceIter<T = char>", align 8
  store %"[c-flat]:SliceIter<T = char>" %0, ptr %self, align 8
  %self.rev = getelementptr inbounds nuw i8, ptr %self, i64 16
  %self.rev1 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %tmp = load i1, ptr %self.rev1, align 1
  %tmp.i = xor i1 %tmp, true
  store i1 %tmp.i, ptr %self.rev, align 1
  %tmp2 = load %"[c-flat]:SliceIter<T = char>", ptr %self, align 8
  ret %"[c-flat]:SliceIter<T = char>" %tmp2
}

define %"[c-flat]:SliceIter<T = char>" @"[c-flat]:Vec<T = char>:until"(ptr %0, i8 %1) {
entry:
  %prev_iter = alloca %"[c-flat]:SliceIter<T = char>", align 8
  %value = alloca i8, align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 %1, ptr %value, align 1
  %tmp1.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2.rev = getelementptr inbounds nuw i8, ptr %0, i64 16
  %tmp3 = load ptr, ptr %0, align 8
  %tmp4 = load ptr, ptr %tmp1.end, align 8
  %tmp5 = load i1, ptr %tmp2.rev, align 1
  %tmp_agg = insertvalue %"[c-flat]:SliceIter<T = char>" undef, ptr %tmp3, 0
  %tmp_agg6 = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg, ptr %tmp4, 1
  %tmp_constructor = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg6, i1 %tmp5, 2
  store %"[c-flat]:SliceIter<T = char>" %tmp_constructor, ptr %prev_iter, align 8
  br label %cond

cond:                                             ; preds = %cond10, %entry
  %tmp7 = load ptr, ptr %self, align 8
  %fun_call_tmp = call i1 @"[c-flat]:Vec<T = char>:has_next"(ptr %tmp7)
  br i1 %fun_call_tmp, label %cond10, label %cond18

cond10:                                           ; preds = %cond
  %tmp12 = load ptr, ptr %self, align 8
  %fun_call_tmp13 = call ptr @"[c-flat]:Vec<T = char>:next"(ptr %tmp12)
  %tmp.i = load i8, ptr %fun_call_tmp13, align 1
  %tmp1.i = load i8, ptr %value, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  br i1 %tmp2.i, label %cond18, label %cond

merge16:                                          ; preds = %then34, %then17
  %tmp23.pn = phi ptr [ %tmp23, %then17 ], [ %tmp36, %then34 ]
  %tmp24.pn = phi ptr [ %tmp24, %then17 ], [ %tmp37, %then34 ]
  %tmp_agg25.pn = insertvalue %"[c-flat]:SliceIter<T = char>" undef, ptr %tmp23.pn, 0
  %tmp_agg26.pn = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg25.pn, ptr %tmp24.pn, 1
  %result47 = insertvalue %"[c-flat]:SliceIter<T = char>" %tmp_agg26.pn, i1 false, 2
  ret %"[c-flat]:SliceIter<T = char>" %result47

then17:                                           ; preds = %cond18
  %tmp22 = load ptr, ptr %self, align 8
  %tmp22.end = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  %prev_iter.end = getelementptr inbounds nuw i8, ptr %prev_iter, i64 8
  %tmp23 = load ptr, ptr %tmp22.end, align 8
  %tmp24 = load ptr, ptr %prev_iter.end, align 8
  %2 = getelementptr inbounds nuw i8, ptr %prev_iter, i64 16
  %tmp.i54 = load i1, ptr %2, align 1
  %fun_call_tmp.i55 = call {} @"[c-flat]:bool:drop"(i1 %tmp.i54)
  %tmp30 = load i8, ptr %value, align 1
  %fun_call_tmp31 = call {} @"[c-flat]:char:drop"(i8 %tmp30)
  br label %merge16

cond18:                                           ; preds = %cond10, %cond
  %tmp20 = load ptr, ptr %self, align 8
  %tmp20.rev = getelementptr inbounds nuw i8, ptr %tmp20, i64 16
  %tmp21 = load i1, ptr %tmp20.rev, align 1
  br i1 %tmp21, label %then17, label %then34

then34:                                           ; preds = %cond18
  %tmp35 = load ptr, ptr %self, align 8
  %tmp36 = load ptr, ptr %prev_iter, align 8
  %tmp37 = load ptr, ptr %tmp35, align 8
  %3 = getelementptr inbounds nuw i8, ptr %prev_iter, i64 16
  %tmp.i49 = load i1, ptr %3, align 1
  %fun_call_tmp.i = call {} @"[c-flat]:bool:drop"(i1 %tmp.i49)
  %tmp43 = load i8, ptr %value, align 1
  %fun_call_tmp44 = call {} @"[c-flat]:char:drop"(i8 %tmp43)
  br label %merge16
}

define %"[c-flat]:SliceIter<T = char>" @"[c-flat]:Vec<T = char>:skip"(%"[c-flat]:SliceIter<T = char>" %0, i64 %1) {
entry:
  %count = alloca i64, align 8
  %self = alloca %"[c-flat]:SliceIter<T = char>", align 8
  store %"[c-flat]:SliceIter<T = char>" %0, ptr %self, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call ptr @"[c-flat]:Vec<T = char>:next"(ptr nonnull %self)
  %tmp4 = load i64, ptr %count, align 4
  %tmp.i11 = add i64 %tmp4, -1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ %1, %entry ], [ %tmp.i11, %then ]
  store i64 %storemerge, ptr %count, align 4
  %fun_call_tmp = call i1 @"[c-flat]:Vec<T = char>:has_next"(ptr nonnull %self)
  %tmp.i14 = icmp ne i64 %storemerge, 0
  %tmp.i = and i1 %fun_call_tmp, %tmp.i14
  br i1 %tmp.i, label %then, label %else

else:                                             ; preds = %cond
  %tmp6 = load %"[c-flat]:SliceIter<T = char>", ptr %self, align 8
  %tmp7 = load i64, ptr %count, align 4
  %fun_call_tmp8 = call {} @"[c-flat]:u64:drop"(i64 %tmp7)
  ret %"[c-flat]:SliceIter<T = char>" %tmp6
}

define { ptr, i64 } @"[c-flat]:Vec<T = char>:as_slice.31"(ptr %0) {
entry:
  %fun_call_tmp = call i64 @"[c-flat]:Vec<T = char>:len"(ptr %0)
  %tmp2 = load ptr, ptr %0, align 8
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %fun_call_tmp, 1
  ret { ptr, i64 } %len2.i
}

define i64 @"[c-flat]:Vec<T = char>:len"(ptr %0) {
entry:
  %tmp.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load ptr, ptr %tmp.end, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp3 = load ptr, ptr %0, align 8
  %cast4 = ptrtoint ptr %tmp3 to i64
  %tmp.i = sub i64 %cast, %cast4
  ret i64 %tmp.i
}

define i32 @"[c-flat]:set_working_dir"({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp1, align 8
  %tmp1.repack15 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp1.elt16 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt16, ptr %tmp1.repack15, align 8
  %tmp1.repack17 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp1.elt18 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt18, ptr %tmp1.repack17, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call i32 @c_chdir(ptr %ref.i)
  %tmp5.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp5.unpack, 0
  %tmp5.elt19 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp5.unpack20 = load i64, ptr %tmp5.elt19, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp5.unpack20, 1
  %tmp5.elt21 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp5.unpack22 = load i64, ptr %tmp5.elt21, align 8
  %tmp523 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp5.unpack22, 2
  %fun_call_tmp6 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp523)
  ret i32 %fun_call_tmp4
}

declare i32 @c_chdir(ptr)

define i32 @"[c-flat]:system"({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"[c-flat]:Vec<T = char>", align 8
  %fun_call_tmp = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } %0)
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:Vec<T = char>:c_string"(%"[c-flat]:Vec<T = char>" %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp1, align 8
  %tmp1.repack15 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp1.elt16 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt16, ptr %tmp1.repack15, align 8
  %tmp1.repack17 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp1.elt18 = extractvalue %"[c-flat]:Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt18, ptr %tmp1.repack17, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"[c-flat]:Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp2, 0
  %fun_call_tmp4 = call i32 @c_system(ptr %ref.i)
  %tmp5.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"[c-flat]:Vec<T = char>" poison, ptr %tmp5.unpack, 0
  %tmp5.elt19 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp5.unpack20 = load i64, ptr %tmp5.elt19, align 8
  %2 = insertvalue %"[c-flat]:Vec<T = char>" %1, i64 %tmp5.unpack20, 1
  %tmp5.elt21 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp5.unpack22 = load i64, ptr %tmp5.elt21, align 8
  %tmp523 = insertvalue %"[c-flat]:Vec<T = char>" %2, i64 %tmp5.unpack22, 2
  %fun_call_tmp6 = call {} @"[c-flat]:Vec<T = char>:drop"(%"[c-flat]:Vec<T = char>" %tmp523)
  ret i32 %fun_call_tmp4
}

declare i32 @c_system(ptr)

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
  %fun_call_tmp1 = call %"[c-flat]:Vec<T = char>" @"[c-flat]:[char]:to_string"({ ptr, i64 } { ptr @global_string.35, i64 1 })
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

define {} @"[c-flat]:i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}
