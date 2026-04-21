; ModuleID = 'main_module'
source_filename = "main_module"

%"Vec<T = char>" = type { ptr, i64, i64 }
%"Vec_2<T = i32>" = type { i32, i32 }
%"Range<T = i32>" = type { i32, i32 }
%"Vec<T = Vec_2<T = i32>>" = type { ptr, i64, i64 }
%"SliceIter<T = Vec_2<T = i32>>" = type { ptr, ptr }
%"mem:Manually_Drop<T = Vec_2<T = i32>>" = type { %"Vec_2<T = i32>" }
%"mem:Manually_Drop<T = char>" = type { i8 }

@count = internal global i64 0
@global_string = private unnamed_addr constant [8 x i8] c"\1B[2J\1B[H\00", align 1
@global_string.1 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %tmp1 = alloca %"Vec<T = char>", align 8
  %heap_count = alloca i64, align 8
  %fun_call_tmp = call {} @paint()
  %fun_call_tmp1 = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp1, align 4
  store i64 %tmp, ptr %heap_count, align 4
  %fun_call_tmp2 = call %"Vec<T = char>" @"u64:to_string"(ptr nonnull %heap_count)
  %fun_call_tmp2.elt = extractvalue %"Vec<T = char>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %tmp1, align 8
  %tmp1.repack9 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp2.elt10 = extractvalue %"Vec<T = char>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt10, ptr %tmp1.repack9, align 8
  %tmp1.repack11 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp2.elt12 = extractvalue %"Vec<T = char>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt12, ptr %tmp1.repack11, align 8
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %fun_call_tmp4 = call {} @print({ ptr, i64 } %fun_call_tmp3)
  %tmp5.unpack = load ptr, ptr %tmp1, align 8
  %0 = insertvalue %"Vec<T = char>" poison, ptr %tmp5.unpack, 0
  %tmp5.elt13 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp5.unpack14 = load i64, ptr %tmp5.elt13, align 8
  %1 = insertvalue %"Vec<T = char>" %0, i64 %tmp5.unpack14, 1
  %tmp5.elt15 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp5.unpack16 = load i64, ptr %tmp5.elt15, align 8
  %tmp517 = insertvalue %"Vec<T = char>" %1, i64 %tmp5.unpack16, 2
  %fun_call_tmp6 = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp517)
  %tmp7 = load i64, ptr %heap_count, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
  ret i64 0
}

define {} @paint() {
entry:
  %tmp4135 = alloca %"Vec<T = char>", align 8
  %tmp3133 = alloca i64, align 8
  %stamp = alloca i1, align 1
  %tmp10 = alloca i32, align 4
  %tmp9 = alloca i32, align 4
  %tmp886 = alloca i32, align 4
  %tmp784 = alloca i32, align 4
  %cur_pos = alloca %"Vec_2<T = i32>", align 8
  %x = alloca i32, align 4
  %tmp_iter4 = alloca %"Range<T = i32>", align 8
  %y = alloca i32, align 4
  %tmp_iter2 = alloca %"Range<T = i32>", align 8
  %tmp8 = alloca i8, align 1
  %tmp7 = alloca i8, align 1
  %tmp6 = alloca i8, align 1
  %tmp5 = alloca i8, align 1
  %tmp4 = alloca i8, align 1
  %tmp3 = alloca i8, align 1
  %input = alloca i8, align 1
  %pos = alloca %"Vec_2<T = i32>", align 8
  %dots = alloca %"Vec<T = Vec_2<T = i32>>", align 8
  %fun_call_tmp = call %"Vec<T = Vec_2<T = i32>>" @"Vec<T = Vec_2<T = i32>>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %dots, align 8
  %dots.repack254 = getelementptr inbounds nuw i8, ptr %dots, i64 8
  %fun_call_tmp.elt255 = extractvalue %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt255, ptr %dots.repack254, align 8
  %dots.repack256 = getelementptr inbounds nuw i8, ptr %dots, i64 16
  %fun_call_tmp.elt257 = extractvalue %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt257, ptr %dots.repack256, align 8
  store i32 12, ptr %pos, align 4
  %pos.repack258 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  store i32 10, ptr %pos.repack258, align 4
  br label %then

then:                                             ; preds = %else72, %entry
  br label %cond

then2:                                            ; preds = %cond
  %fun_call_tmp4 = call i8 @c_getch()
  store i8 %fun_call_tmp4, ptr %input, align 1
  store i8 113, ptr %tmp3, align 1
  %tmp.i218 = load i8, ptr %input, align 1
  %tmp2.i220 = icmp eq i8 %tmp.i218, 113
  br i1 %tmp2.i220, label %then6, label %cond12

cond:                                             ; preds = %merge38, %then
  %fun_call_tmp3 = call i1 @c_kbhit()
  br i1 %fun_call_tmp3, label %then2, label %else

else:                                             ; preds = %cond
  %fun_call_tmp67 = call {} @print({ ptr, i64 } { ptr @global_string, i64 7 })
  store i32 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack259 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 4
  store i32 32, ptr %tmp_iter2.repack259, align 4
  br label %cond71

then6:                                            ; preds = %then2
  %tmp147.unpack = load i32, ptr %pos, align 4
  %tmp147.elt290 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp147.unpack291 = load i32, ptr %tmp147.elt290, align 4
  %fun_call_tmp.i = call {} @"i32:drop"(i32 %tmp147.unpack291)
  %fun_call_tmp2.i = call {} @"i32:drop"(i32 %tmp147.unpack)
  %tmp149.unpack = load ptr, ptr %dots, align 8
  %0 = insertvalue %"Vec<T = Vec_2<T = i32>>" poison, ptr %tmp149.unpack, 0
  %tmp149.elt293 = getelementptr inbounds nuw i8, ptr %dots, i64 8
  %tmp149.unpack294 = load i64, ptr %tmp149.elt293, align 8
  %1 = insertvalue %"Vec<T = Vec_2<T = i32>>" %0, i64 %tmp149.unpack294, 1
  %tmp149.elt295 = getelementptr inbounds nuw i8, ptr %dots, i64 16
  %tmp149.unpack296 = load i64, ptr %tmp149.elt295, align 8
  %tmp149297 = insertvalue %"Vec<T = Vec_2<T = i32>>" %1, i64 %tmp149.unpack296, 2
  %fun_call_tmp150 = call {} @"Vec<T = Vec_2<T = i32>>:drop"(%"Vec<T = Vec_2<T = i32>>" %tmp149297)
  ret {} zeroinitializer

merge10:                                          ; preds = %cond12, %then11
  %tmp22 = load i32, ptr %pos, align 4
  store i8 100, ptr %tmp5, align 1
  %tmp.i208 = load i8, ptr %input, align 1
  %tmp2.i210 = icmp eq i8 %tmp.i208, 100
  br i1 %tmp2.i210, label %merge23, label %cond29

then11:                                           ; preds = %cond12
  %tmp19.unpack = load i32, ptr %pos, align 4
  %2 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp19.unpack, 0
  %tmp19.elt287 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp19.unpack288 = load i32, ptr %tmp19.elt287, align 4
  %tmp19289 = insertvalue %"Vec_2<T = i32>" %2, i32 %tmp19.unpack288, 1
  %fun_call_tmp20 = call {} @"Vec<T = Vec_2<T = i32>>:push"(ptr nonnull %dots, %"Vec_2<T = i32>" %tmp19289)
  br label %merge10

cond12:                                           ; preds = %then2
  store i8 112, ptr %tmp4, align 1
  %tmp.i213 = load i8, ptr %input, align 1
  %tmp2.i215 = icmp ne i8 %tmp.i213, 112
  %fun_call_tmp15 = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref_mut"(ptr nonnull %dots)
  %tmp.unpack = load i32, ptr %pos, align 4
  %3 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp.unpack, 0
  %tmp.elt284 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp.unpack285 = load i32, ptr %tmp.elt284, align 4
  %tmp286 = insertvalue %"Vec_2<T = i32>" %3, i32 %tmp.unpack285, 1
  %fun_call_tmp16 = call i1 @"[Vec_2<T = i32>]:has"({ ptr, i64 } %fun_call_tmp15, %"Vec_2<T = i32>" %tmp286)
  %tmp.i240.not = or i1 %tmp2.i215, %fun_call_tmp16
  br i1 %tmp.i240.not, label %merge10, label %then11

merge23:                                          ; preds = %cond29, %merge10
  %result34 = phi i32 [ 1, %merge10 ], [ %spec.select, %cond29 ]
  %tmp.i153 = add i32 %tmp22, %result34
  store i32 %tmp.i153, ptr %pos, align 4
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp37 = load i32, ptr %pos.y, align 4
  store i8 115, ptr %tmp7, align 1
  %tmp.i198 = load i8, ptr %input, align 1
  %tmp2.i200 = icmp eq i8 %tmp.i198, 115
  br i1 %tmp2.i200, label %merge38, label %cond44

cond29:                                           ; preds = %merge10
  store i8 97, ptr %tmp6, align 1
  %tmp.i203 = load i8, ptr %input, align 1
  %tmp2.i205 = icmp eq i8 %tmp.i203, 97
  %spec.select = select i1 %tmp2.i205, i32 -1, i32 0
  br label %merge23

merge38:                                          ; preds = %cond44, %merge23
  %result49 = phi i32 [ 1, %merge23 ], [ %spec.select298, %cond44 ]
  %tmp.i = add i32 %tmp37, %result49
  %pos.y51 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  store i32 %tmp.i, ptr %pos.y51, align 4
  %tmp52 = load i8, ptr %tmp8, align 1
  %fun_call_tmp53 = call {} @"char:drop"(i8 %tmp52)
  %tmp54 = load i8, ptr %tmp7, align 1
  %fun_call_tmp55 = call {} @"char:drop"(i8 %tmp54)
  %tmp56 = load i8, ptr %tmp6, align 1
  %fun_call_tmp57 = call {} @"char:drop"(i8 %tmp56)
  %tmp58 = load i8, ptr %tmp5, align 1
  %fun_call_tmp59 = call {} @"char:drop"(i8 %tmp58)
  %tmp60 = load i8, ptr %tmp4, align 1
  %fun_call_tmp61 = call {} @"char:drop"(i8 %tmp60)
  %tmp62 = load i8, ptr %tmp3, align 1
  %fun_call_tmp63 = call {} @"char:drop"(i8 %tmp62)
  %tmp64 = load i8, ptr %input, align 1
  %fun_call_tmp65 = call {} @"char:drop"(i8 %tmp64)
  br label %cond

cond44:                                           ; preds = %merge23
  store i8 119, ptr %tmp8, align 1
  %tmp.i193 = load i8, ptr %input, align 1
  %tmp2.i195 = icmp eq i8 %tmp.i193, 119
  %spec.select298 = select i1 %tmp2.i195, i32 -1, i32 0
  br label %merge38

then70:                                           ; preds = %cond71
  %fun_call_tmp74 = call i32 @"Range<T = i32>:next"(ptr nonnull %tmp_iter2)
  store i32 %fun_call_tmp74, ptr %y, align 4
  store i32 0, ptr %tmp_iter4, align 4
  %tmp_iter4.repack272 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 4
  store i32 32, ptr %tmp_iter4.repack272, align 4
  br label %cond78

cond71:                                           ; preds = %else79, %else
  %fun_call_tmp73 = call i1 @"Range<T = i32>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp73, label %then70, label %else72

else72:                                           ; preds = %cond71
  %fun_call_tmp131 = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref_mut"(ptr nonnull %dots)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp131, 1
  store i64 %len.i, ptr %tmp3133, align 4
  %fun_call_tmp134 = call %"Vec<T = char>" @"u64:to_string"(ptr nonnull %tmp3133)
  %fun_call_tmp134.elt = extractvalue %"Vec<T = char>" %fun_call_tmp134, 0
  store ptr %fun_call_tmp134.elt, ptr %tmp4135, align 8
  %tmp4135.repack260 = getelementptr inbounds nuw i8, ptr %tmp4135, i64 8
  %fun_call_tmp134.elt261 = extractvalue %"Vec<T = char>" %fun_call_tmp134, 1
  store i64 %fun_call_tmp134.elt261, ptr %tmp4135.repack260, align 8
  %tmp4135.repack262 = getelementptr inbounds nuw i8, ptr %tmp4135, i64 16
  %fun_call_tmp134.elt263 = extractvalue %"Vec<T = char>" %fun_call_tmp134, 2
  store i64 %fun_call_tmp134.elt263, ptr %tmp4135.repack262, align 8
  %fun_call_tmp136 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %tmp4135)
  %fun_call_tmp137 = call {} @println({ ptr, i64 } %fun_call_tmp136)
  %fun_call_tmp139 = call {} @sleep(double 0x3FA1111111111111)
  %tmp140.unpack = load ptr, ptr %tmp4135, align 8
  %4 = insertvalue %"Vec<T = char>" poison, ptr %tmp140.unpack, 0
  %tmp140.elt264 = getelementptr inbounds nuw i8, ptr %tmp4135, i64 8
  %tmp140.unpack265 = load i64, ptr %tmp140.elt264, align 8
  %5 = insertvalue %"Vec<T = char>" %4, i64 %tmp140.unpack265, 1
  %tmp140.elt266 = getelementptr inbounds nuw i8, ptr %tmp4135, i64 16
  %tmp140.unpack267 = load i64, ptr %tmp140.elt266, align 8
  %tmp140268 = insertvalue %"Vec<T = char>" %5, i64 %tmp140.unpack267, 2
  %fun_call_tmp141 = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp140268)
  %tmp142 = load i64, ptr %tmp3133, align 4
  %fun_call_tmp143 = call {} @"u64:drop"(i64 %tmp142)
  %tmp144.unpack = load i32, ptr %tmp_iter2, align 4
  %tmp144.elt269 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 4
  %tmp144.unpack270 = load i32, ptr %tmp144.elt269, align 4
  %fun_call_tmp.i244 = call {} @"i32:drop"(i32 %tmp144.unpack270)
  %fun_call_tmp2.i246 = call {} @"i32:drop"(i32 %tmp144.unpack)
  br label %then

then77:                                           ; preds = %cond78
  %fun_call_tmp81 = call i32 @"Range<T = i32>:next"(ptr nonnull %tmp_iter4)
  store i32 %fun_call_tmp81, ptr %x, align 4
  %tmp83 = load i32, ptr %y, align 4
  store i32 %fun_call_tmp81, ptr %cur_pos, align 4
  %cur_pos.repack276 = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  store i32 %tmp83, ptr %cur_pos.repack276, align 4
  store i32 0, ptr %tmp784, align 4
  %tmp2.i177 = icmp eq i32 %fun_call_tmp81, 0
  store i32 0, ptr %tmp886, align 4
  %tmp.i170 = load i32, ptr %y, align 4
  %tmp2.i172 = icmp eq i32 %tmp.i170, 0
  %tmp.i237 = or i1 %tmp2.i177, %tmp2.i172
  store i32 31, ptr %tmp9, align 4
  %tmp.i165 = load i32, ptr %x, align 4
  %tmp2.i167 = icmp eq i32 %tmp.i165, 31
  %tmp.i234 = or i1 %tmp.i237, %tmp2.i167
  store i32 31, ptr %tmp10, align 4
  %tmp.i162 = load i32, ptr %y, align 4
  %tmp2.i = icmp eq i32 %tmp.i162, 31
  %tmp.i231 = or i1 %tmp.i234, %tmp2.i
  %tmp.i188 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp1.y.i = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  %tmp.i9.i = load i32, ptr %tmp.i188, align 4
  %tmp1.i10.i = load i32, ptr %tmp1.y.i, align 4
  %tmp2.i11.i = icmp eq i32 %tmp.i9.i, %tmp1.i10.i
  %tmp.i.i = load i32, ptr %pos, align 4
  %tmp1.i.i = load i32, ptr %cur_pos, align 4
  %tmp2.i.i = icmp eq i32 %tmp.i.i, %tmp1.i.i
  %tmp6.i = and i1 %tmp2.i11.i, %tmp2.i.i
  %tmp.i228 = or i1 %tmp.i231, %tmp6.i
  %fun_call_tmp95 = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref_mut"(ptr nonnull %dots)
  %6 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp1.i.i, 0
  %tmp96.elt278 = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  %tmp96.unpack279 = load i32, ptr %tmp96.elt278, align 4
  %tmp96280 = insertvalue %"Vec_2<T = i32>" %6, i32 %tmp96.unpack279, 1
  %fun_call_tmp97 = call i1 @"[Vec_2<T = i32>]:has"({ ptr, i64 } %fun_call_tmp95, %"Vec_2<T = i32>" %tmp96280)
  %tmp.i225 = or i1 %tmp.i228, %fun_call_tmp97
  store i1 %tmp.i225, ptr %stamp, align 1
  %tmp103 = load i1, ptr %stamp, align 1
  %spec.select299 = select i1 %tmp103, i8 64, i8 32
  %fun_call_tmp107 = call i8 @c_putchar(i8 %spec.select299)
  %fun_call_tmp109 = call {} @"char:drop"(i8 %spec.select299)
  %tmp110 = load i1, ptr %stamp, align 1
  %fun_call_tmp111 = call {} @"bool:drop"(i1 %tmp110)
  %tmp112 = load i32, ptr %tmp10, align 4
  %fun_call_tmp113 = call {} @"i32:drop"(i32 %tmp112)
  %tmp114 = load i32, ptr %tmp9, align 4
  %fun_call_tmp115 = call {} @"i32:drop"(i32 %tmp114)
  %tmp116 = load i32, ptr %tmp886, align 4
  %fun_call_tmp117 = call {} @"i32:drop"(i32 %tmp116)
  %tmp118 = load i32, ptr %tmp784, align 4
  %fun_call_tmp119 = call {} @"i32:drop"(i32 %tmp118)
  %tmp120.unpack = load i32, ptr %cur_pos, align 4
  %tmp120.elt281 = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  %tmp120.unpack282 = load i32, ptr %tmp120.elt281, align 4
  %fun_call_tmp.i183 = call {} @"i32:drop"(i32 %tmp120.unpack282)
  %fun_call_tmp2.i185 = call {} @"i32:drop"(i32 %tmp120.unpack)
  %tmp122 = load i32, ptr %x, align 4
  %fun_call_tmp123 = call {} @"i32:drop"(i32 %tmp122)
  br label %cond78

cond78:                                           ; preds = %then77, %then70
  %fun_call_tmp80 = call i1 @"Range<T = i32>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp80, label %then77, label %else79

else79:                                           ; preds = %cond78
  %fun_call_tmp125 = call i8 @c_putchar(i8 10)
  %tmp126.unpack = load i32, ptr %tmp_iter4, align 4
  %tmp126.elt273 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 4
  %tmp126.unpack274 = load i32, ptr %tmp126.elt273, align 4
  %fun_call_tmp.i249 = call {} @"i32:drop"(i32 %tmp126.unpack274)
  %fun_call_tmp2.i251 = call {} @"i32:drop"(i32 %tmp126.unpack)
  %tmp128 = load i32, ptr %y, align 4
  %fun_call_tmp129 = call {} @"i32:drop"(i32 %tmp128)
  br label %cond71
}

define i1 @"&Vec_2<T = i32>:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 4
  %tmp1.y.i = getelementptr inbounds nuw i8, ptr %tmp3, i64 4
  %tmp.i9.i = load i32, ptr %tmp.i, align 4
  %tmp1.i10.i = load i32, ptr %tmp1.y.i, align 4
  %tmp2.i11.i = icmp eq i32 %tmp.i9.i, %tmp1.i10.i
  %tmp.i.i = load i32, ptr %tmp1, align 4
  %tmp1.i.i = load i32, ptr %tmp3, align 4
  %tmp2.i.i = icmp eq i32 %tmp.i.i, %tmp1.i.i
  %tmp6.i = and i1 %tmp2.i11.i, %tmp2.i.i
  ret i1 %tmp6.i
}

define {} @"i32:drop"(i32 %0) {
entry:
  ret {} zeroinitializer
}

define %"Vec<T = Vec_2<T = i32>>" @"Vec<T = Vec_2<T = i32>>:new"() {
entry:
  %fun_call_tmp = call %"Vec<T = Vec_2<T = i32>>" @"Vec<T = Vec_2<T = i32>>:with_capacity"(i64 4)
  ret %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp
}

define %"Vec<T = Vec_2<T = i32>>" @"Vec<T = Vec_2<T = i32>>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:alloc<T = Vec_2<T = i32>>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = Vec_2<T = i32>>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = Vec_2<T = i32>>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = Vec_2<T = i32>>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"u64:drop"(i64 %0)
  ret %"Vec<T = Vec_2<T = i32>>" %tmp_constructor
}

define ptr @"mem:alloc<T = Vec_2<T = i32>>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i11 = shl i64 %0, 3
  %fun_call_tmp6 = call ptr @c_malloc(i64 %tmp.i11)
  %tmp7 = load i64, ptr %len, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
  ret ptr %fun_call_tmp6
}

define ptr @"mem:heap_count"() {
entry:
  %tmp = load i64, ptr @count, align 4
  %fun_call_tmp = call {} @"u64:drop"(i64 %tmp)
  ret ptr @count
}

define {} @"u64:drop"(i64 %0) {
entry:
  ret {} zeroinitializer
}

declare ptr @c_malloc(i64)

declare i1 @c_kbhit()

declare i8 @c_getch()

define { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define i1 @"[Vec_2<T = i32>]:has"({ ptr, i64 } %0, %"Vec_2<T = i32>" %1) {
entry:
  %item = alloca ptr, align 8
  %tmp_iter2 = alloca %"SliceIter<T = Vec_2<T = i32>>", align 8
  %value = alloca %"Vec_2<T = i32>", align 8
  %self = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack47 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt48 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt48, ptr %self.repack47, align 8
  %.elt49 = extractvalue %"Vec_2<T = i32>" %1, 0
  store i32 %.elt49, ptr %value, align 4
  %value.repack50 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %.elt51 = extractvalue %"Vec_2<T = i32>" %1, 1
  store i32 %.elt51, ptr %value.repack50, align 4
  %tmp.unpack = load ptr, ptr %self, align 8
  %2 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt52 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp.unpack53 = load i64, ptr %tmp.elt52, align 8
  %tmp54 = insertvalue { ptr, i64 } %2, i64 %tmp.unpack53, 1
  %fun_call_tmp = call %"SliceIter<T = Vec_2<T = i32>>" @"[Vec_2<T = i32>]:iter"({ ptr, i64 } %tmp54)
  %fun_call_tmp.elt = extractvalue %"SliceIter<T = Vec_2<T = i32>>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack55 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp.elt56 = extractvalue %"SliceIter<T = Vec_2<T = i32>>" %fun_call_tmp, 1
  store ptr %fun_call_tmp.elt56, ptr %tmp_iter2.repack55, align 8
  br label %cond

common.ret:                                       ; preds = %then4, %else
  %common.ret.op = phi i1 [ false, %else ], [ true, %then4 ]
  ret i1 %common.ret.op

then:                                             ; preds = %cond
  %fun_call_tmp2 = call ptr @"SliceIter<T = Vec_2<T = i32>>:next"(ptr nonnull %tmp_iter2)
  store ptr %fun_call_tmp2, ptr %item, align 8
  %tmp7 = load ptr, ptr %item, align 8
  %tmp.i34 = getelementptr inbounds nuw i8, ptr %tmp7, i64 4
  %tmp1.y.i = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp.i9.i = load i32, ptr %tmp.i34, align 4
  %tmp1.i10.i = load i32, ptr %tmp1.y.i, align 4
  %tmp2.i11.i = icmp eq i32 %tmp.i9.i, %tmp1.i10.i
  %tmp.i.i = load i32, ptr %tmp7, align 4
  %tmp1.i.i = load i32, ptr %value, align 4
  %tmp2.i.i = icmp eq i32 %tmp.i.i, %tmp1.i.i
  %tmp6.i = and i1 %tmp2.i11.i, %tmp2.i.i
  br i1 %tmp6.i, label %then4, label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp1 = call i1 @"SliceIter<T = Vec_2<T = i32>>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp1, label %then, label %else

else:                                             ; preds = %cond
  %tmp22.unpack = load i32, ptr %value, align 4
  %tmp22.elt57 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp22.unpack58 = load i32, ptr %tmp22.elt57, align 4
  %fun_call_tmp.i = call {} @"i32:drop"(i32 %tmp22.unpack58)
  %fun_call_tmp2.i = call {} @"i32:drop"(i32 %tmp22.unpack)
  br label %common.ret

then4:                                            ; preds = %then
  %tmp13.unpack = load i32, ptr %value, align 4
  %tmp13.elt60 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp13.unpack61 = load i32, ptr %tmp13.elt60, align 4
  %fun_call_tmp.i30 = call {} @"i32:drop"(i32 %tmp13.unpack61)
  %fun_call_tmp2.i32 = call {} @"i32:drop"(i32 %tmp13.unpack)
  br label %common.ret
}

define %"SliceIter<T = Vec_2<T = i32>>" @"[Vec_2<T = i32>]:iter"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i15 = shl i64 %.elt21, 3
  %tmp.i = add i64 %tmp.i15, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"SliceIter<T = Vec_2<T = i32>>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"SliceIter<T = Vec_2<T = i32>>" %tmp_agg, ptr %cast7, 1
  ret %"SliceIter<T = Vec_2<T = i32>>" %tmp_constructor
}

define i1 @"SliceIter<T = Vec_2<T = i32>>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"SliceIter<T = Vec_2<T = i32>>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 8
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"Vec<T = Vec_2<T = i32>>:push"(ptr %0, %"Vec_2<T = i32>" %1) {
entry:
  %value = alloca %"Vec_2<T = i32>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"Vec_2<T = i32>" %1, 0
  store i32 %.elt, ptr %value, align 4
  %value.repack30 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %.elt31 = extractvalue %"Vec_2<T = i32>" %1, 1
  store i32 %.elt31, ptr %value.repack30, align 4
  %fun_call_tmp = call {} @"Vec<T = Vec_2<T = i32>>:reserve"(ptr %0, i64 1)
  %tmp1 = load ptr, ptr %self, align 8
  %tmp2 = load ptr, ptr %tmp1, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i26 = shl i64 %tmp4, 3
  %tmp.i23 = add i64 %tmp.i26, %cast
  %cast8 = inttoptr i64 %tmp.i23 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = Vec_2<T = i32>>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14.unpack = load i32, ptr %value, align 4
  %tmp14.elt32 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp14.unpack33 = load i32, ptr %tmp14.elt32, align 4
  %2 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp14.unpack, 0
  %tmp15.unpack40 = insertvalue %"Vec_2<T = i32>" %2, i32 %tmp14.unpack33, 1
  %tmp1537 = insertvalue %"mem:Manually_Drop<T = Vec_2<T = i32>>" poison, %"Vec_2<T = i32>" %tmp15.unpack40, 0
  %fun_call_tmp16 = call {} @"mem:Manually_Drop<T = Vec_2<T = i32>>:drop"(%"mem:Manually_Drop<T = Vec_2<T = i32>>" %tmp1537)
  %tmp17.unpack = load i32, ptr %value, align 4
  %tmp17.elt41 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp17.unpack42 = load i32, ptr %tmp17.elt41, align 4
  %fun_call_tmp.i = call {} @"i32:drop"(i32 %tmp17.unpack42)
  %fun_call_tmp2.i = call {} @"i32:drop"(i32 %tmp17.unpack)
  ret {} zeroinitializer
}

define {} @"Vec<T = Vec_2<T = i32>>:reserve"(ptr %0, i64 %1) {
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
  %fun_call_tmp37 = call {} @"u64:drop"(i64 %tmp36)
  %tmp38 = load i64, ptr %additional, align 4
  %fun_call_tmp39 = call {} @"u64:drop"(i64 %tmp38)
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
  %fun_call_tmp21 = call ptr @"mem:alloc<T = Vec_2<T = i32>>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"mem:copy_range<T = Vec_2<T = i32>>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"mem:free<T = Vec_2<T = i32>>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"mem:copy_range<T = Vec_2<T = i32>>"(ptr %0, ptr %1, i64 %2) {
entry:
  %tmp.i = shl i64 %2, 3
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %tmp.i)
  %fun_call_tmp6 = call {} @"u64:drop"(i64 %2)
  ret {} zeroinitializer
}

declare {} @c_memcpy(ptr, ptr, i64)

define {} @"mem:free<T = Vec_2<T = i32>>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

declare {} @c_free(ptr)

define {} @"mem:copy<T = Vec_2<T = i32>>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 8)
  ret {} zeroinitializer
}

define {} @"mem:Manually_Drop<T = Vec_2<T = i32>>:drop"(%"mem:Manually_Drop<T = Vec_2<T = i32>>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"char:drop"(i8 %0) {
entry:
  ret {} zeroinitializer
}

define {} @print({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"Vec<T = char>", align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt21 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt21, ptr %tmp1.repack20, align 8
  %tmp1.repack22 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt23 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt23, ptr %tmp1.repack22, align 8
  %fun_call_tmp1 = call ptr @"Vec<T = char>:null_terminate"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %fun_call_tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %fun_call_tmp5 = call {} @c_printf(ptr %ref.i)
  %fun_call_tmp6 = call {} @c_flush()
  %tmp9.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"Vec<T = char>" poison, ptr %tmp9.unpack, 0
  %tmp9.elt24 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp9.unpack25 = load i64, ptr %tmp9.elt24, align 8
  %2 = insertvalue %"Vec<T = char>" %1, i64 %tmp9.unpack25, 1
  %tmp9.elt26 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp9.unpack27 = load i64, ptr %tmp9.elt26, align 8
  %tmp928 = insertvalue %"Vec<T = char>" %2, i64 %tmp9.unpack27, 2
  %fun_call_tmp10 = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp928)
  ret {} zeroinitializer
}

define i1 @"&char:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0) {
entry:
  %result = alloca %"Vec<T = char>", align 8
  %slice = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %slice, align 8
  %slice.repack5 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %.elt6 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt6, ptr %slice.repack5, align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %result, align 8
  %result.repack7 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %fun_call_tmp.elt8 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt8, ptr %result.repack7, align 8
  %result.repack9 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %fun_call_tmp.elt10 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt10, ptr %result.repack9, align 8
  %tmp.unpack = load ptr, ptr %slice, align 8
  %1 = insertvalue { ptr, i64 } poison, ptr %tmp.unpack, 0
  %tmp.elt11 = getelementptr inbounds nuw i8, ptr %slice, i64 8
  %tmp.unpack12 = load i64, ptr %tmp.elt11, align 8
  %tmp13 = insertvalue { ptr, i64 } %1, i64 %tmp.unpack12, 1
  %fun_call_tmp1 = call {} @"Vec<T = char>:extend_from_slice"(ptr nonnull %result, { ptr, i64 } %tmp13)
  %tmp2.unpack = load ptr, ptr %result, align 8
  %2 = insertvalue %"Vec<T = char>" poison, ptr %tmp2.unpack, 0
  %tmp2.elt14 = getelementptr inbounds nuw i8, ptr %result, i64 8
  %tmp2.unpack15 = load i64, ptr %tmp2.elt14, align 8
  %3 = insertvalue %"Vec<T = char>" %2, i64 %tmp2.unpack15, 1
  %tmp2.elt16 = getelementptr inbounds nuw i8, ptr %result, i64 16
  %tmp2.unpack17 = load i64, ptr %tmp2.elt16, align 8
  %tmp218 = insertvalue %"Vec<T = char>" %3, i64 %tmp2.unpack17, 2
  ret %"Vec<T = char>" %tmp218
}

define %"Vec<T = char>" @"Vec<T = char>:new"() {
entry:
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:with_capacity"(i64 4)
  ret %"Vec<T = char>" %fun_call_tmp
}

define %"Vec<T = char>" @"Vec<T = char>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:alloc<T = char>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = char>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = char>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = char>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"u64:drop"(i64 %0)
  ret %"Vec<T = char>" %tmp_constructor
}

define ptr @"mem:alloc<T = char>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp6 = call ptr @c_malloc(i64 %0)
  %tmp7 = load i64, ptr %len, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
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
  %fun_call_tmp8 = call {} @"Vec<T = char>:push"(ptr %tmp3, i8 %tmp7)
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
  %fun_call_tmp12 = call {} @"u64:drop"(i64 %tmp11)
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
  %tmp.i23 = add i64 %tmp4, %cast
  %cast8 = inttoptr i64 %tmp.i23 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = char>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load i8, ptr %value, align 1
  %tmp1528 = insertvalue %"mem:Manually_Drop<T = char>" poison, i8 %tmp14, 0
  %fun_call_tmp16 = call {} @"mem:Manually_Drop<T = char>:drop"(%"mem:Manually_Drop<T = char>" %tmp1528)
  %tmp17 = load i8, ptr %value, align 1
  %fun_call_tmp18 = call {} @"char:drop"(i8 %tmp17)
  ret {} zeroinitializer
}

define {} @"Vec<T = char>:reserve"(ptr %0, i64 %1) {
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
  %fun_call_tmp37 = call {} @"u64:drop"(i64 %tmp36)
  %tmp38 = load i64, ptr %additional, align 4
  %fun_call_tmp39 = call {} @"u64:drop"(i64 %tmp38)
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
  %fun_call_tmp21 = call ptr @"mem:alloc<T = char>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"mem:copy_range<T = char>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"mem:free<T = char>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"mem:copy_range<T = char>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %2)
  %fun_call_tmp6 = call {} @"u64:drop"(i64 %2)
  ret {} zeroinitializer
}

define {} @"mem:free<T = char>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define {} @"mem:copy<T = char>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define {} @"mem:Manually_Drop<T = char>:drop"(%"mem:Manually_Drop<T = char>" %0) {
entry:
  ret {} zeroinitializer
}

define ptr @"Vec<T = char>:null_terminate"(ptr %0) {
entry:
  %fun_call_tmp = call {} @"Vec<T = char>:push"(ptr %0, i8 0)
  ret ptr %0
}

declare {} @c_printf(ptr)

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
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

declare {} @c_flush()

define {} @"Vec<T = char>:drop"(%"Vec<T = char>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"Vec<T = char>", align 8
  %.elt = extractvalue %"Vec<T = char>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"Vec<T = char>" %0, 1
  store i64 %.elt20, ptr %self.repack19, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt22 = extractvalue %"Vec<T = char>" %0, 2
  store i64 %.elt22, ptr %self.repack21, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp4
  %tmp6 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp7 = call {} @"char:drop"(i8 %tmp6)
  %tmp.i = add i64 %tmp4, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = char>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"mem:free<T = char>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"u64:drop"(i64 %tmp12)
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
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define i1 @"Range<T = i32>:has_next"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 4
  %tmp3 = load i32, ptr %tmp2.end, align 4
  %tmp.i = icmp slt i32 %tmp1, %tmp3
  ret i1 %tmp.i
}

define i32 @"Range<T = i32>:next"(ptr %0) {
entry:
  %tmp1 = load i32, ptr %0, align 4
  %tmp.i = add i32 %tmp1, 1
  store i32 %tmp.i, ptr %0, align 4
  %fun_call_tmp7 = call {} @"i32:drop"(i32 %tmp1)
  ret i32 %tmp1
}

declare i8 @c_putchar(i8)

define {} @"bool:drop"(i1 %0) {
entry:
  ret {} zeroinitializer
}

define {} @println({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"Vec<T = char>", align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack20 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt21 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt21, ptr %tmp1.repack20, align 8
  %tmp1.repack22 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt23 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt23, ptr %tmp1.repack22, align 8
  %fun_call_tmp1 = call ptr @"Vec<T = char>:null_terminate"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %fun_call_tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %fun_call_tmp5 = call {} @c_puts(ptr %ref.i)
  %fun_call_tmp6 = call {} @c_flush()
  %tmp9.unpack = load ptr, ptr %tmp1, align 8
  %1 = insertvalue %"Vec<T = char>" poison, ptr %tmp9.unpack, 0
  %tmp9.elt24 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp9.unpack25 = load i64, ptr %tmp9.elt24, align 8
  %2 = insertvalue %"Vec<T = char>" %1, i64 %tmp9.unpack25, 1
  %tmp9.elt26 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp9.unpack27 = load i64, ptr %tmp9.elt26, align 8
  %tmp928 = insertvalue %"Vec<T = char>" %2, i64 %tmp9.unpack27, 2
  %fun_call_tmp10 = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp928)
  ret {} zeroinitializer
}

declare {} @c_puts(ptr)

define %"Vec<T = char>" @"u64:to_string"(ptr %0) {
entry:
  %tmp426 = alloca i64, align 8
  %num = alloca i64, align 8
  %result7 = alloca %"Vec<T = char>", align 8
  %tmp1 = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp = load ptr, ptr %self, align 8
  store i64 0, ptr %tmp1, align 4
  %tmp.i73 = load i64, ptr %tmp, align 4
  %tmp2.i75 = icmp eq i64 %tmp.i73, 0
  br i1 %tmp2.i75, label %then, label %else

common.ret:                                       ; preds = %else39, %then
  %common.ret.op = phi %"Vec<T = char>" [ %fun_call_tmp1, %then ], [ %tmp4586, %else39 ]
  ret %"Vec<T = char>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp1 = call %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } { ptr @global_string.1, i64 1 })
  %tmp2 = load i64, ptr %tmp1, align 4
  %fun_call_tmp3 = call {} @"u64:drop"(i64 %tmp2)
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp6 = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %fun_call_tmp6.elt = extractvalue %"Vec<T = char>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %result7, align 8
  %result7.repack78 = getelementptr inbounds nuw i8, ptr %result7, i64 8
  %fun_call_tmp6.elt79 = extractvalue %"Vec<T = char>" %fun_call_tmp6, 1
  store i64 %fun_call_tmp6.elt79, ptr %result7.repack78, align 8
  %result7.repack80 = getelementptr inbounds nuw i8, ptr %result7, i64 16
  %fun_call_tmp6.elt81 = extractvalue %"Vec<T = char>" %fun_call_tmp6, 2
  store i64 %fun_call_tmp6.elt81, ptr %result7.repack80, align 8
  %tmp19 = load ptr, ptr %self, align 8
  %tmp20 = load i64, ptr %tmp19, align 4
  br label %cond24

then23:                                           ; preds = %cond24
  %tmp29 = load i64, ptr %num, align 4
  %tmp.i61 = urem i64 %tmp29, 10
  %1 = trunc nuw nsw i64 %tmp.i61 to i8
  %cast = or disjoint i8 %1, 48
  %fun_call_tmp32 = call {} @"Vec<T = char>:insert"(ptr nonnull %result7, i64 0, i8 %cast)
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
  %2 = insertvalue %"Vec<T = char>" poison, ptr %tmp45.unpack, 0
  %tmp45.elt82 = getelementptr inbounds nuw i8, ptr %result7, i64 8
  %tmp45.unpack83 = load i64, ptr %tmp45.elt82, align 8
  %3 = insertvalue %"Vec<T = char>" %2, i64 %tmp45.unpack83, 1
  %tmp45.elt84 = getelementptr inbounds nuw i8, ptr %result7, i64 16
  %tmp45.unpack85 = load i64, ptr %tmp45.elt84, align 8
  %tmp4586 = insertvalue %"Vec<T = char>" %3, i64 %tmp45.unpack85, 2
  %tmp46 = load i64, ptr %tmp426, align 4
  %fun_call_tmp47 = call {} @"u64:drop"(i64 %tmp46)
  %tmp48 = load i64, ptr %num, align 4
  %fun_call_tmp49 = call {} @"u64:drop"(i64 %tmp48)
  %tmp50 = load i64, ptr %tmp1, align 4
  %fun_call_tmp51 = call {} @"u64:drop"(i64 %tmp50)
  br label %common.ret
}

define %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } %0) {
entry:
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  ret %"Vec<T = char>" %fun_call_tmp
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
  %fun_call_tmp19 = call {} @"mem:copy_range<T = char>"(ptr %cast13, ptr %tmp18, i64 %tmp.i47)
  %fun_call_tmp21 = call {} @"mem:copy<T = char>"(ptr %tmp18, ptr nonnull %value)
  %tmp22 = load ptr, ptr %self, align 8
  %tmp22.len = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  %tmp23 = load i64, ptr %tmp22.len, align 4
  %tmp.i = add i64 %tmp23, 1
  %tmp25.len = getelementptr inbounds nuw i8, ptr %tmp22, i64 8
  store i64 %tmp.i, ptr %tmp25.len, align 4
  %tmp26 = load i8, ptr %value, align 1
  %tmp2753 = insertvalue %"mem:Manually_Drop<T = char>" poison, i8 %tmp26, 0
  %fun_call_tmp28 = call {} @"mem:Manually_Drop<T = char>:drop"(%"mem:Manually_Drop<T = char>" %tmp2753)
  %tmp31 = load i64, ptr %type_mem_size, align 4
  %fun_call_tmp32 = call {} @"u64:drop"(i64 %tmp31)
  %tmp33 = load i8, ptr %value, align 1
  %fun_call_tmp34 = call {} @"char:drop"(i8 %tmp33)
  %tmp35 = load i64, ptr %idx, align 4
  %fun_call_tmp36 = call {} @"u64:drop"(i64 %tmp35)
  ret {} zeroinitializer
}

define {} @sleep(double %0) {
entry:
  %tmp.i = fmul double %0, 1.000000e+06
  %cast = fptoui double %tmp.i to i32
  %fun_call_tmp1 = call i32 @c_usleep(i32 %cast)
  %fun_call_tmp3 = call {} @"f64:drop"(double %0)
  ret {} zeroinitializer
}

declare i32 @c_usleep(i32)

define {} @"f64:drop"(double %0) {
entry:
  ret {} zeroinitializer
}

define {} @"Vec<T = Vec_2<T = i32>>:drop"(%"Vec<T = Vec_2<T = i32>>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"Vec<T = Vec_2<T = i32>>", align 8
  %.elt = extractvalue %"Vec<T = Vec_2<T = i32>>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt22 = extractvalue %"Vec<T = Vec_2<T = i32>>" %0, 1
  store i64 %.elt22, ptr %self.repack21, align 8
  %self.repack23 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt24 = extractvalue %"Vec<T = Vec_2<T = i32>>" %0, 2
  store i64 %.elt24, ptr %self.repack23, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr %"Vec_2<T = i32>", ptr %ref.i, i64 %tmp4
  %tmp6.unpack = load i32, ptr %tmp_index.i, align 4
  %tmp6.elt25 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 4
  %tmp6.unpack26 = load i32, ptr %tmp6.elt25, align 4
  %fun_call_tmp.i = call {} @"i32:drop"(i32 %tmp6.unpack26)
  %fun_call_tmp2.i = call {} @"i32:drop"(i32 %tmp6.unpack)
  %tmp8 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp8, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"mem:free<T = Vec_2<T = i32>>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}
