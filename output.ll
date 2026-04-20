; ModuleID = 'main_module'
source_filename = "main_module"

%"Vec<T = char>" = type { ptr, i64, i64 }
%"Vec_2<T = i32>" = type { i32, i32 }
%"Range<T = i32>" = type { i32, i32 }
%"Vec<T = Vec_2<T = i32>>" = type { ptr, i64, i64 }
%"SliceIter<T = Vec_2<T = i32>>" = type { ptr, ptr }

@count = internal global i64 0
@global_string = private unnamed_addr constant [23 x i8] c"\1B[31mHello in red!\1B[0m\00", align 1
@global_string.1 = private unnamed_addr constant [8 x i8] c"\1B[2J\1B[H\00", align 1
@global_string.2 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @paint()
  %fun_call_tmp1 = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp1, align 4
  ret i64 %tmp
}

define {} @paint() {
entry:
  %tmp4 = alloca %"Vec<T = char>", align 8
  %tmp3 = alloca i64, align 8
  %stamp = alloca i1, align 1
  %cur_pos = alloca %"Vec_2<T = i32>", align 8
  %x = alloca i32, align 4
  %tmp_iter4 = alloca %"Range<T = i32>", align 8
  %y = alloca i32, align 4
  %tmp_iter2 = alloca %"Range<T = i32>", align 8
  %input = alloca i8, align 1
  %pos = alloca %"Vec_2<T = i32>", align 8
  %dots = alloca %"Vec<T = Vec_2<T = i32>>", align 8
  %fun_call_tmp = call %"Vec<T = Vec_2<T = i32>>" @"Vec<T = Vec_2<T = i32>>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %dots, align 8
  %dots.repack134 = getelementptr inbounds nuw i8, ptr %dots, i64 8
  %fun_call_tmp.elt135 = extractvalue %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt135, ptr %dots.repack134, align 8
  %dots.repack136 = getelementptr inbounds nuw i8, ptr %dots, i64 16
  %fun_call_tmp.elt137 = extractvalue %"Vec<T = Vec_2<T = i32>>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt137, ptr %dots.repack136, align 8
  store i32 12, ptr %pos, align 4
  %pos.repack138 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  store i32 10, ptr %pos.repack138, align 4
  br label %then

then:                                             ; preds = %else64, %entry
  %fun_call_tmp1 = call {} @print({ ptr, i64 } { ptr @global_string, i64 22 })
  br label %cond

then3:                                            ; preds = %cond
  %fun_call_tmp5 = call i8 @c_getch()
  store i8 %fun_call_tmp5, ptr %input, align 1
  %tmp = load i8, ptr %input, align 1
  %tmp.i120 = icmp eq i8 %tmp, 113
  br i1 %tmp.i120, label %then7, label %cond13

cond:                                             ; preds = %merge11, %then
  %fun_call_tmp4 = call i1 @c_kbhit()
  br i1 %fun_call_tmp4, label %then3, label %else

else:                                             ; preds = %cond
  %fun_call_tmp59 = call {} @write({ ptr, i64 } { ptr @global_string.1, i64 7 })
  store i32 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack139 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 4
  store i32 32, ptr %tmp_iter2.repack139, align 4
  br label %cond63

then7:                                            ; preds = %then3
  %fun_call_tmp115 = call {} @"Vec<T = Vec_2<T = i32>>:drop"(ptr nonnull %dots)
  ret {} zeroinitializer

merge11:                                          ; preds = %cond13, %then12
  %tmp24 = load i32, ptr %pos, align 4
  %tmp29 = load i8, ptr %input, align 1
  %tmp.i118 = icmp eq i8 %tmp29, 100
  %tmp34 = load i8, ptr %input, align 1
  %tmp.i117 = icmp eq i8 %tmp34, 97
  %spec.select = select i1 %tmp.i117, i32 -1, i32 0
  %result38 = select i1 %tmp.i118, i32 1, i32 %spec.select
  %tmp.i129 = add i32 %tmp24, %result38
  store i32 %tmp.i129, ptr %pos, align 4
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp41 = load i32, ptr %pos.y, align 4
  %tmp46 = load i8, ptr %input, align 1
  %tmp.i116 = icmp eq i8 %tmp46, 115
  %tmp51 = load i8, ptr %input, align 1
  %tmp.i = icmp eq i8 %tmp51, 119
  %spec.select162 = select i1 %tmp.i, i32 -1, i32 0
  %result55 = select i1 %tmp.i116, i32 1, i32 %spec.select162
  %tmp.i128 = add i32 %tmp41, %result55
  %pos.y57 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  store i32 %tmp.i128, ptr %pos.y57, align 4
  br label %cond

then12:                                           ; preds = %cond13
  %tmp21.unpack = load i32, ptr %pos, align 4
  %0 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp21.unpack, 0
  %tmp21.elt159 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp21.unpack160 = load i32, ptr %tmp21.elt159, align 4
  %tmp21161 = insertvalue %"Vec_2<T = i32>" %0, i32 %tmp21.unpack160, 1
  %fun_call_tmp22 = call {} @"Vec<T = Vec_2<T = i32>>:push"(ptr nonnull %dots, %"Vec_2<T = i32>" %tmp21161)
  br label %merge11

cond13:                                           ; preds = %then3
  %tmp15 = load i8, ptr %input, align 1
  %tmp.i119 = icmp ne i8 %tmp15, 112
  %tmp17.unpack = load i32, ptr %pos, align 4
  %1 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp17.unpack, 0
  %tmp17.elt156 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp17.unpack157 = load i32, ptr %tmp17.elt156, align 4
  %tmp17158 = insertvalue %"Vec_2<T = i32>" %1, i32 %tmp17.unpack157, 1
  %fun_call_tmp18 = call i1 @"Vec<T = Vec_2<T = i32>>:has"(ptr nonnull %dots, %"Vec_2<T = i32>" %tmp17158)
  %tmp.i127.not = or i1 %tmp.i119, %fun_call_tmp18
  br i1 %tmp.i127.not, label %merge11, label %then12

then62:                                           ; preds = %cond63
  %fun_call_tmp66 = call i32 @"Range<T = i32>:next"(ptr nonnull %tmp_iter2)
  store i32 %fun_call_tmp66, ptr %y, align 4
  store i32 0, ptr %tmp_iter4, align 4
  %tmp_iter4.repack144 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 4
  store i32 32, ptr %tmp_iter4.repack144, align 4
  br label %cond70

cond63:                                           ; preds = %else71, %else
  %fun_call_tmp65 = call i1 @"Range<T = i32>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp65, label %then62, label %else64

else64:                                           ; preds = %cond63
  %fun_call_tmp106 = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref_mut"(ptr nonnull %dots)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp106, 1
  store i64 %len.i, ptr %tmp3, align 4
  %fun_call_tmp108 = call %"Vec<T = char>" @"u64:to_string"(ptr nonnull %tmp3)
  %fun_call_tmp108.elt = extractvalue %"Vec<T = char>" %fun_call_tmp108, 0
  store ptr %fun_call_tmp108.elt, ptr %tmp4, align 8
  %tmp4.repack140 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %fun_call_tmp108.elt141 = extractvalue %"Vec<T = char>" %fun_call_tmp108, 1
  store i64 %fun_call_tmp108.elt141, ptr %tmp4.repack140, align 8
  %tmp4.repack142 = getelementptr inbounds nuw i8, ptr %tmp4, i64 16
  %fun_call_tmp108.elt143 = extractvalue %"Vec<T = char>" %fun_call_tmp108, 2
  store i64 %fun_call_tmp108.elt143, ptr %tmp4.repack142, align 8
  %fun_call_tmp109 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %tmp4)
  %fun_call_tmp110 = call {} @print({ ptr, i64 } %fun_call_tmp109)
  %fun_call_tmp112 = call {} @sleep(double 5.000000e-01)
  %fun_call_tmp113 = call {} @"Vec<T = char>:drop"(ptr nonnull %tmp4)
  br label %then

then69:                                           ; preds = %cond70
  %fun_call_tmp73 = call i32 @"Range<T = i32>:next"(ptr nonnull %tmp_iter4)
  store i32 %fun_call_tmp73, ptr %x, align 4
  %tmp75 = load i32, ptr %y, align 4
  store i32 %fun_call_tmp73, ptr %cur_pos, align 4
  %cur_pos.repack145 = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  store i32 %tmp75, ptr %cur_pos.repack145, align 4
  %tmp.i133 = icmp eq i32 %fun_call_tmp73, 0
  %tmp.i132 = icmp eq i32 %tmp75, 0
  %tmp.i126 = or i1 %tmp.i133, %tmp.i132
  %tmp81 = load i32, ptr %x, align 4
  %tmp.i131 = icmp eq i32 %tmp81, 31
  %tmp.i125 = or i1 %tmp.i126, %tmp.i131
  %tmp84 = load i32, ptr %y, align 4
  %tmp.i130 = icmp eq i32 %tmp84, 31
  %tmp.i124 = or i1 %tmp.i125, %tmp.i130
  %tmp87.unpack = load i32, ptr %pos, align 4
  %2 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp87.unpack, 0
  %tmp87.elt147 = getelementptr inbounds nuw i8, ptr %pos, i64 4
  %tmp87.unpack148 = load i32, ptr %tmp87.elt147, align 4
  %tmp87149 = insertvalue %"Vec_2<T = i32>" %2, i32 %tmp87.unpack148, 1
  %tmp88.unpack = load i32, ptr %cur_pos, align 4
  %3 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp88.unpack, 0
  %tmp88.elt150 = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  %tmp88.unpack151 = load i32, ptr %tmp88.elt150, align 4
  %tmp88152 = insertvalue %"Vec_2<T = i32>" %3, i32 %tmp88.unpack151, 1
  %fun_call_tmp89 = call i1 @"Vec_2<T = i32>:eq"(%"Vec_2<T = i32>" %tmp87149, %"Vec_2<T = i32>" %tmp88152)
  %tmp.i123 = or i1 %tmp.i124, %fun_call_tmp89
  %tmp91.unpack = load i32, ptr %cur_pos, align 4
  %4 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp91.unpack, 0
  %tmp91.elt153 = getelementptr inbounds nuw i8, ptr %cur_pos, i64 4
  %tmp91.unpack154 = load i32, ptr %tmp91.elt153, align 4
  %tmp91155 = insertvalue %"Vec_2<T = i32>" %4, i32 %tmp91.unpack154, 1
  %fun_call_tmp92 = call i1 @"Vec<T = Vec_2<T = i32>>:has"(ptr nonnull %dots, %"Vec_2<T = i32>" %tmp91155)
  %tmp.i122 = or i1 %tmp.i123, %fun_call_tmp92
  store i1 %tmp.i122, ptr %stamp, align 1
  %tmp98 = load i1, ptr %stamp, align 1
  %spec.select163 = select i1 %tmp98, i8 64, i8 32
  %fun_call_tmp102 = call i8 @c_putchar(i8 %spec.select163)
  br label %cond70

cond70:                                           ; preds = %then69, %then62
  %fun_call_tmp72 = call i1 @"Range<T = i32>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp72, label %then69, label %else71

else71:                                           ; preds = %cond70
  %fun_call_tmp104 = call i8 @c_putchar(i8 10)
  br label %cond63
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
  ret %"Vec<T = Vec_2<T = i32>>" %tmp_constructor
}

define ptr @"mem:alloc<T = Vec_2<T = i32>>"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i7 = shl i64 %0, 3
  %fun_call_tmp6 = call ptr @c_malloc(i64 %tmp.i7)
  ret ptr %fun_call_tmp6
}

define ptr @"mem:heap_count"() {
entry:
  ret ptr @count
}

declare ptr @c_malloc(i64)

define {} @print({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"Vec<T = char>", align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack13 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt14 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt14, ptr %tmp1.repack13, align 8
  %tmp1.repack15 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt16 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt16, ptr %tmp1.repack15, align 8
  %fun_call_tmp1 = call ptr @"Vec<T = char>:null_terminate"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %fun_call_tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %fun_call_tmp5 = call {} @c_puts(ptr %ref.i)
  %fun_call_tmp6 = call {} @c_flush()
  %fun_call_tmp7 = call {} @"Vec<T = char>:drop"(ptr nonnull %tmp1)
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
  %fun_call_tmp = call ptr @"mem:alloc<T = char>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = char>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = char>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = char>" %tmp_agg2, i64 %0, 2
  ret %"Vec<T = char>" %tmp_constructor
}

define ptr @"mem:alloc<T = char>"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp6 = call ptr @c_malloc(i64 %0)
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
  ret {} zeroinitializer
}

declare {} @c_memcpy(ptr, ptr, i64)

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

declare {} @c_free(ptr)

define {} @"mem:copy<T = char>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define ptr @"Vec<T = char>:null_terminate"(ptr %0) {
entry:
  %fun_call_tmp = call {} @"Vec<T = char>:push"(ptr %0, i8 0)
  ret ptr %0
}

declare {} @c_puts(ptr)

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

declare {} @c_flush()

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
  %fun_call_tmp13 = call {} @"mem:free<T = char>"(ptr %tmp12)
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

declare i1 @c_kbhit()

declare i8 @c_getch()

define i1 @"Vec<T = Vec_2<T = i32>>:has"(ptr %0, %"Vec_2<T = i32>" %1) {
entry:
  %item = alloca ptr, align 8
  %tmp_iter2 = alloca %"SliceIter<T = Vec_2<T = i32>>", align 8
  %value = alloca %"Vec_2<T = i32>", align 8
  %.elt = extractvalue %"Vec_2<T = i32>" %1, 0
  store i32 %.elt, ptr %value, align 4
  %value.repack13 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %.elt14 = extractvalue %"Vec_2<T = i32>" %1, 1
  store i32 %.elt14, ptr %value.repack13, align 4
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref"(ptr %0)
  %fun_call_tmp1 = call %"SliceIter<T = Vec_2<T = i32>>" @"[Vec_2<T = i32>]:iter"({ ptr, i64 } %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"SliceIter<T = Vec_2<T = i32>>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack15 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp1.elt16 = extractvalue %"SliceIter<T = Vec_2<T = i32>>" %fun_call_tmp1, 1
  store ptr %fun_call_tmp1.elt16, ptr %tmp_iter2.repack15, align 8
  br label %cond

common.ret:                                       ; preds = %then, %cond
  %common.ret.op = phi i1 [ false, %cond ], [ true, %then ]
  ret i1 %common.ret.op

then:                                             ; preds = %cond
  %fun_call_tmp3 = call ptr @"SliceIter<T = Vec_2<T = i32>>:next"(ptr nonnull %tmp_iter2)
  store ptr %fun_call_tmp3, ptr %item, align 8
  %tmp8 = load ptr, ptr %item, align 8
  %tmp9.unpack = load i32, ptr %tmp8, align 4
  %2 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp9.unpack, 0
  %tmp9.elt17 = getelementptr inbounds nuw i8, ptr %tmp8, i64 4
  %tmp9.unpack18 = load i32, ptr %tmp9.elt17, align 4
  %tmp919 = insertvalue %"Vec_2<T = i32>" %2, i32 %tmp9.unpack18, 1
  %tmp10.unpack = load i32, ptr %value, align 4
  %3 = insertvalue %"Vec_2<T = i32>" poison, i32 %tmp10.unpack, 0
  %tmp10.elt20 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %tmp10.unpack21 = load i32, ptr %tmp10.elt20, align 4
  %tmp1022 = insertvalue %"Vec_2<T = i32>" %3, i32 %tmp10.unpack21, 1
  %fun_call_tmp11 = call i1 @"Vec_2<T = i32>:eq"(%"Vec_2<T = i32>" %tmp919, %"Vec_2<T = i32>" %tmp1022)
  br i1 %fun_call_tmp11, label %common.ret, label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp2 = call i1 @"SliceIter<T = Vec_2<T = i32>>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp2, label %then, label %common.ret
}

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
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
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
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 %tmp3, 1
  ret { ptr, i64 } %len.i
}

define %"SliceIter<T = Vec_2<T = i32>>" @"[Vec_2<T = i32>]:iter"({ ptr, i64 } %0) {
entry:
  %.elt11 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i9 = shl i64 %.elt11, 3
  %tmp.i = add i64 %tmp.i9, %cast
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

define i1 @"Vec_2<T = i32>:eq"(%"Vec_2<T = i32>" %0, %"Vec_2<T = i32>" %1) {
entry:
  %self = alloca %"Vec_2<T = i32>", align 8
  %.elt = extractvalue %"Vec_2<T = i32>" %0, 0
  store i32 %.elt, ptr %self, align 4
  %self.repack8 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %.elt9 = extractvalue %"Vec_2<T = i32>" %0, 1
  store i32 %.elt9, ptr %self.repack8, align 4
  %.elt10 = extractvalue %"Vec_2<T = i32>" %1, 0
  %.elt12 = extractvalue %"Vec_2<T = i32>" %1, 1
  %tmp = load i32, ptr %self, align 4
  %tmp.i7 = icmp eq i32 %tmp, %.elt10
  %self.y = getelementptr inbounds nuw i8, ptr %self, i64 4
  %tmp2 = load i32, ptr %self.y, align 4
  %tmp.i6 = icmp eq i32 %tmp2, %.elt12
  %tmp.i = and i1 %tmp.i7, %tmp.i6
  ret i1 %tmp.i
}

define {} @"Vec<T = Vec_2<T = i32>>:push"(ptr %0, %"Vec_2<T = i32>" %1) {
entry:
  %value = alloca %"Vec_2<T = i32>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"Vec_2<T = i32>" %1, 0
  store i32 %.elt, ptr %value, align 4
  %value.repack17 = getelementptr inbounds nuw i8, ptr %value, i64 4
  %.elt18 = extractvalue %"Vec_2<T = i32>" %1, 1
  store i32 %.elt18, ptr %value.repack17, align 4
  %fun_call_tmp = call {} @"Vec<T = Vec_2<T = i32>>:reserve"(ptr %0, i64 1)
  %tmp1 = load ptr, ptr %self, align 8
  %tmp2 = load ptr, ptr %tmp1, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i16 = shl i64 %tmp4, 3
  %tmp.i15 = add i64 %tmp.i16, %cast
  %cast8 = inttoptr i64 %tmp.i15 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = Vec_2<T = i32>>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  ret {} zeroinitializer
}

define {} @"Vec<T = Vec_2<T = i32>>:reserve"(ptr %0, i64 %1) {
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
  ret {} zeroinitializer
}

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

define {} @"mem:copy<T = Vec_2<T = i32>>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 8)
  ret {} zeroinitializer
}

define {} @write({ ptr, i64 } %0) {
entry:
  %tmp1 = alloca %"Vec<T = char>", align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:from"({ ptr, i64 } %0)
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %tmp1, align 8
  %tmp1.repack13 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp.elt14 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt14, ptr %tmp1.repack13, align 8
  %tmp1.repack15 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp.elt16 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt16, ptr %tmp1.repack15, align 8
  %fun_call_tmp1 = call ptr @"Vec<T = char>:null_terminate"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %fun_call_tmp1)
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %fun_call_tmp5 = call {} @c_printf(ptr %ref.i)
  %fun_call_tmp6 = call {} @c_flush()
  %fun_call_tmp7 = call {} @"Vec<T = char>:drop"(ptr nonnull %tmp1)
  ret {} zeroinitializer
}

declare {} @c_printf(ptr)

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
  ret i32 %tmp1
}

declare i8 @c_putchar(i8)

define %"Vec<T = char>" @"u64:to_string"(ptr %0) {
entry:
  %num = alloca i64, align 8
  %result4 = alloca %"Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp = load ptr, ptr %self, align 8
  %tmp1 = load i64, ptr %tmp, align 4
  %tmp.i49 = icmp eq i64 %tmp1, 0
  br i1 %tmp.i49, label %then, label %else

common.ret:                                       ; preds = %else36, %then
  %common.ret.op = phi %"Vec<T = char>" [ %fun_call_tmp2, %then ], [ %tmp4258, %else36 ]
  ret %"Vec<T = char>" %common.ret.op

then:                                             ; preds = %entry
  %fun_call_tmp2 = call %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } { ptr @global_string.2, i64 1 })
  br label %common.ret

else:                                             ; preds = %entry
  %fun_call_tmp3 = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %fun_call_tmp3.elt = extractvalue %"Vec<T = char>" %fun_call_tmp3, 0
  store ptr %fun_call_tmp3.elt, ptr %result4, align 8
  %result4.repack50 = getelementptr inbounds nuw i8, ptr %result4, i64 8
  %fun_call_tmp3.elt51 = extractvalue %"Vec<T = char>" %fun_call_tmp3, 1
  store i64 %fun_call_tmp3.elt51, ptr %result4.repack50, align 8
  %result4.repack52 = getelementptr inbounds nuw i8, ptr %result4, i64 16
  %fun_call_tmp3.elt53 = extractvalue %"Vec<T = char>" %fun_call_tmp3, 2
  store i64 %fun_call_tmp3.elt53, ptr %result4.repack52, align 8
  %tmp16 = load ptr, ptr %self, align 8
  %tmp17 = load i64, ptr %tmp16, align 4
  br label %cond21

then20:                                           ; preds = %cond21
  %tmp26 = load i64, ptr %num, align 4
  %tmp.i45 = urem i64 %tmp26, 10
  %1 = trunc nuw nsw i64 %tmp.i45 to i8
  %cast = or disjoint i8 %1, 48
  %fun_call_tmp29 = call {} @"Vec<T = char>:insert"(ptr nonnull %result4, i64 0, i8 %cast)
  %tmp.i43 = udiv i64 %tmp26, 10
  br label %cond21

cond21:                                           ; preds = %then20, %else
  %storemerge = phi i64 [ %tmp17, %else ], [ %tmp.i43, %then20 ]
  store i64 %storemerge, ptr %num, align 4
  %tmp.i48.not = icmp eq i64 %storemerge, 0
  br i1 %tmp.i48.not, label %else36, label %then20

else36:                                           ; preds = %cond21
  %tmp42.unpack = load ptr, ptr %result4, align 8
  %2 = insertvalue %"Vec<T = char>" poison, ptr %tmp42.unpack, 0
  %tmp42.elt54 = getelementptr inbounds nuw i8, ptr %result4, i64 8
  %tmp42.unpack55 = load i64, ptr %tmp42.elt54, align 8
  %3 = insertvalue %"Vec<T = char>" %2, i64 %tmp42.unpack55, 1
  %tmp42.elt56 = getelementptr inbounds nuw i8, ptr %result4, i64 16
  %tmp42.unpack57 = load i64, ptr %tmp42.elt56, align 8
  %tmp4258 = insertvalue %"Vec<T = char>" %3, i64 %tmp42.unpack57, 2
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

define {} @sleep(double %0) {
entry:
  %tmp.i = fmul double %0, 1.000000e+06
  %cast = fptoui double %tmp.i to i32
  %fun_call_tmp1 = call i32 @c_usleep(i32 %cast)
  ret {} zeroinitializer
}

declare i32 @c_usleep(i32)

define {} @"Vec<T = Vec_2<T = i32>>:drop"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  br label %cond

then:                                             ; preds = %cond
  %tmp4 = load ptr, ptr %self, align 8
  %fun_call_tmp5 = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref"(ptr %tmp4)
  %tmp6 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp5, 0
  %tmp_index.i = getelementptr %"Vec_2<T = i32>", ptr %ref.i, i64 %tmp6
  %fun_call_tmp8 = call {} @"Vec_2<T = i32>:drop"(ptr %tmp_index.i)
  %tmp.i = add i64 %tmp6, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1 = load ptr, ptr %self, align 8
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Vec_2<T = i32>>:deref"(ptr %tmp1)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i14 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i14, label %then, label %else

else:                                             ; preds = %cond
  %tmp11 = load ptr, ptr %self, align 8
  %tmp12 = load ptr, ptr %tmp11, align 8
  %fun_call_tmp13 = call {} @"mem:free<T = Vec_2<T = i32>>"(ptr %tmp12)
  ret {} zeroinitializer
}

define {} @"Vec_2<T = i32>:drop"(ptr %0) {
entry:
  ret {} zeroinitializer
}
