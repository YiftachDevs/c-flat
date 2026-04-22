; ModuleID = 'main_module'
source_filename = "main_module"

%"SliceIter<T = char>" = type { ptr, ptr }
%"Range<T = u64>" = type { i64, i64 }
%"pixel:Game" = type { %"pixel:Frame", %"Vec<T = char>", i64 }
%"pixel:Frame" = type { %"Vec_2<T = u64>", %"Vec<T = u8>" }
%"Vec_2<T = u64>" = type { i64, i64 }
%"Vec<T = u8>" = type { ptr, i64, i64 }
%"Vec<T = char>" = type { ptr, i64, i64 }
%"mem:Manually_Drop<T = char>" = type { i8 }

@count = internal global i64 0
@global_string = private unnamed_addr constant [8 x i8] c"\1B[2J\1B[H\00", align 1
@global_string.1 = private unnamed_addr constant [6 x i8] c"\1B[40m\00", align 1
@global_string.2 = private unnamed_addr constant [6 x i8] c"\1B[41m\00", align 1
@global_string.3 = private unnamed_addr constant [6 x i8] c"\1B[42m\00", align 1
@global_string.4 = private unnamed_addr constant [6 x i8] c"\1B[43m\00", align 1
@global_string.5 = private unnamed_addr constant [6 x i8] c"\1B[44m\00", align 1
@global_string.6 = private unnamed_addr constant [6 x i8] c"\1B[45m\00", align 1
@global_string.7 = private unnamed_addr constant [6 x i8] c"\1B[46m\00", align 1
@global_string.8 = private unnamed_addr constant [6 x i8] c"\1B[47m\00", align 1
@ansi_codes = internal constant [8 x { ptr, i64 }] [{ ptr, i64 } { ptr @global_string.1, i64 5 }, { ptr, i64 } { ptr @global_string.2, i64 5 }, { ptr, i64 } { ptr @global_string.3, i64 5 }, { ptr, i64 } { ptr @global_string.4, i64 5 }, { ptr, i64 } { ptr @global_string.5, i64 5 }, { ptr, i64 } { ptr @global_string.6, i64 5 }, { ptr, i64 } { ptr @global_string.7, i64 5 }, { ptr, i64 } { ptr @global_string.8, i64 5 }]
@global_string.9 = private unnamed_addr constant [5 x i8] c"\1B[0m\00", align 1
@global_string.10 = private unnamed_addr constant [10 x i8] c"game over\00", align 1

define i64 @main() {
entry:
  %tmp5 = alloca i8, align 1
  %key_press = alloca ptr, align 8
  %tmp_iter355 = alloca %"SliceIter<T = char>", align 8
  %i = alloca i64, align 8
  %tmp_iter2 = alloca %"Range<T = u64>", align 8
  %x = alloca i64, align 8
  %tmp_iter3 = alloca %"Range<T = u64>", align 8
  %y = alloca i64, align 8
  %tmp_iter1 = alloca %"Range<T = u64>", align 8
  %game = alloca %"pixel:Game", align 8
  %fun_call_tmp = call %"pixel:Game" @"pixel:Game:new"(%"Vec_2<T = u64>" { i64 32, i64 32 }, i64 16)
  %fun_call_tmp.elt = extractvalue %"pixel:Game" %fun_call_tmp, 0
  %fun_call_tmp.elt.elt = extractvalue %"pixel:Frame" %fun_call_tmp.elt, 0
  %fun_call_tmp.elt.elt.elt = extractvalue %"Vec_2<T = u64>" %fun_call_tmp.elt.elt, 0
  store i64 %fun_call_tmp.elt.elt.elt, ptr %game, align 8
  %game.repack157 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %fun_call_tmp.elt.elt.elt158 = extractvalue %"Vec_2<T = u64>" %fun_call_tmp.elt.elt, 1
  store i64 %fun_call_tmp.elt.elt.elt158, ptr %game.repack157, align 8
  %game.repack155 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp.elt.elt156 = extractvalue %"pixel:Frame" %fun_call_tmp.elt, 1
  %fun_call_tmp.elt.elt156.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp.elt.elt156, 0
  store ptr %fun_call_tmp.elt.elt156.elt, ptr %game.repack155, align 8
  %game.repack155.repack159 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %fun_call_tmp.elt.elt156.elt160 = extractvalue %"Vec<T = u8>" %fun_call_tmp.elt.elt156, 1
  store i64 %fun_call_tmp.elt.elt156.elt160, ptr %game.repack155.repack159, align 8
  %game.repack155.repack161 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %fun_call_tmp.elt.elt156.elt162 = extractvalue %"Vec<T = u8>" %fun_call_tmp.elt.elt156, 2
  store i64 %fun_call_tmp.elt.elt156.elt162, ptr %game.repack155.repack161, align 8
  %game.repack151 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp.elt152 = extractvalue %"pixel:Game" %fun_call_tmp, 1
  %fun_call_tmp.elt152.elt = extractvalue %"Vec<T = char>" %fun_call_tmp.elt152, 0
  store ptr %fun_call_tmp.elt152.elt, ptr %game.repack151, align 8
  %game.repack151.repack163 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %fun_call_tmp.elt152.elt164 = extractvalue %"Vec<T = char>" %fun_call_tmp.elt152, 1
  store i64 %fun_call_tmp.elt152.elt164, ptr %game.repack151.repack163, align 8
  %game.repack151.repack165 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %fun_call_tmp.elt152.elt166 = extractvalue %"Vec<T = char>" %fun_call_tmp.elt152, 2
  store i64 %fun_call_tmp.elt152.elt166, ptr %game.repack151.repack165, align 8
  %game.repack153 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %fun_call_tmp.elt154 = extractvalue %"pixel:Game" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt154, ptr %game.repack153, align 8
  store i64 0, ptr %tmp_iter1, align 4
  %tmp_iter1.repack167 = getelementptr inbounds nuw i8, ptr %tmp_iter1, i64 8
  store i64 32, ptr %tmp_iter1.repack167, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter1)
  store i64 %fun_call_tmp3, ptr %y, align 4
  store i64 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack198 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  store i64 32, ptr %tmp_iter3.repack198, align 4
  br label %cond7

cond:                                             ; preds = %else8, %entry
  %fun_call_tmp2 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter1)
  br i1 %fun_call_tmp2, label %then, label %then28

then6:                                            ; preds = %cond7
  %fun_call_tmp10 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter3)
  store i64 %fun_call_tmp10, ptr %x, align 4
  %game.frame.buffer = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp11 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %game.frame.buffer)
  %tmp = load i64, ptr %y, align 4
  %tmp.i93 = shl i64 %tmp, 5
  %ref.i126 = extractvalue { ptr, i64 } %fun_call_tmp11, 0
  %0 = getelementptr i8, ptr %ref.i126, i64 %tmp.i93
  %tmp_index.i127 = getelementptr i8, ptr %0, i64 %fun_call_tmp10
  %tmp16 = load i64, ptr %x, align 4
  %tmp.i85 = add i64 %tmp16, %tmp
  %1 = trunc i64 %tmp.i85 to i8
  %cast = and i8 %1, 7
  store i8 %cast, ptr %tmp_index.i127, align 1
  %fun_call_tmp21 = call {} @"u64:drop"(i64 %tmp16)
  br label %cond7

cond7:                                            ; preds = %then6, %then
  %fun_call_tmp9 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp9, label %then6, label %else8

else8:                                            ; preds = %cond7
  %tmp22.unpack = load i64, ptr %tmp_iter3, align 4
  %tmp22.elt199 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %tmp22.unpack200 = load i64, ptr %tmp22.elt199, align 4
  %fun_call_tmp.i145 = call {} @"u64:drop"(i64 %tmp22.unpack200)
  %fun_call_tmp2.i147 = call {} @"u64:drop"(i64 %tmp22.unpack)
  %tmp24 = load i64, ptr %y, align 4
  %fun_call_tmp25 = call {} @"u64:drop"(i64 %tmp24)
  br label %cond

then28:                                           ; preds = %cond, %else58
  %fun_call_tmp29 = call {} @"pixel:Game:update"(ptr nonnull %game)
  store i64 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack168 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  store i64 1024, ptr %tmp_iter2.repack168, align 4
  br label %cond34

then33:                                           ; preds = %cond34
  %fun_call_tmp37 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter2)
  store i64 %fun_call_tmp37, ptr %i, align 4
  %game.frame38.buffer = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp39 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %game.frame38.buffer)
  %ref.i122 = extractvalue { ptr, i64 } %fun_call_tmp39, 0
  %tmp_index.i123 = getelementptr i8, ptr %ref.i122, i64 %fun_call_tmp37
  %game.frame42.buffer = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp43 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %game.frame42.buffer)
  %tmp44 = load i64, ptr %i, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp43, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp44
  %tmp46 = load i8, ptr %tmp_index.i, align 1
  %tmp.i115 = add i8 %tmp46, 7
  %tmp.i118 = and i8 %tmp.i115, 7
  store i8 %tmp.i118, ptr %tmp_index.i123, align 1
  %tmp49 = load i64, ptr %i, align 4
  %fun_call_tmp50 = call {} @"u64:drop"(i64 %tmp49)
  br label %cond34

cond34:                                           ; preds = %then33, %then28
  %fun_call_tmp36 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp36, label %then33, label %else35

else35:                                           ; preds = %cond34
  %game.key_presses = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp53 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %game.key_presses)
  %fun_call_tmp54 = call %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %fun_call_tmp53)
  %fun_call_tmp54.elt = extractvalue %"SliceIter<T = char>" %fun_call_tmp54, 0
  store ptr %fun_call_tmp54.elt, ptr %tmp_iter355, align 8
  %tmp_iter355.repack169 = getelementptr inbounds nuw i8, ptr %tmp_iter355, i64 8
  %fun_call_tmp54.elt170 = extractvalue %"SliceIter<T = char>" %fun_call_tmp54, 1
  store ptr %fun_call_tmp54.elt170, ptr %tmp_iter355.repack169, align 8
  br label %cond57

then56:                                           ; preds = %cond57
  %fun_call_tmp60 = call ptr @"SliceIter<T = char>:next"(ptr nonnull %tmp_iter355)
  store ptr %fun_call_tmp60, ptr %key_press, align 8
  %tmp65 = load ptr, ptr %key_press, align 8
  store i8 113, ptr %tmp5, align 1
  %tmp.i105 = load i8, ptr %tmp65, align 1
  %tmp2.i = icmp eq i8 %tmp.i105, 113
  br i1 %tmp2.i, label %then62, label %else64

cond57:                                           ; preds = %else64, %else35
  %fun_call_tmp59 = call i1 @"SliceIter<T = char>:has_next"(ptr nonnull %tmp_iter355)
  br i1 %fun_call_tmp59, label %then56, label %else58

else58:                                           ; preds = %cond57
  %fun_call_tmp73 = call {} @"pixel:Game:render"(ptr nonnull %game)
  %tmp76.unpack = load i64, ptr %tmp_iter2, align 4
  %tmp76.elt171 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %tmp76.unpack172 = load i64, ptr %tmp76.elt171, align 4
  %fun_call_tmp.i140 = call {} @"u64:drop"(i64 %tmp76.unpack172)
  %fun_call_tmp2.i142 = call {} @"u64:drop"(i64 %tmp76.unpack)
  br label %then28

then62:                                           ; preds = %then56
  %tmp79.unpack = load i64, ptr %tmp_iter1, align 4
  %tmp79.elt174 = getelementptr inbounds nuw i8, ptr %tmp_iter1, i64 8
  %tmp79.unpack175 = load i64, ptr %tmp79.elt174, align 4
  %fun_call_tmp.i135 = call {} @"u64:drop"(i64 %tmp79.unpack175)
  %fun_call_tmp2.i137 = call {} @"u64:drop"(i64 %tmp79.unpack)
  %tmp81.unpack.unpack.unpack = load i64, ptr %game, align 8
  %tmp81.unpack.unpack.elt185 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %tmp81.unpack.unpack.unpack186 = load i64, ptr %tmp81.unpack.unpack.elt185, align 8
  %tmp81.unpack.elt182 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %tmp81.unpack.unpack183.unpack = load ptr, ptr %tmp81.unpack.elt182, align 8
  %2 = insertvalue %"Vec<T = u8>" poison, ptr %tmp81.unpack.unpack183.unpack, 0
  %tmp81.unpack.unpack183.elt188 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp81.unpack.unpack183.unpack189 = load i64, ptr %tmp81.unpack.unpack183.elt188, align 8
  %3 = insertvalue %"Vec<T = u8>" %2, i64 %tmp81.unpack.unpack183.unpack189, 1
  %tmp81.unpack.unpack183.elt190 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp81.unpack.unpack183.unpack191 = load i64, ptr %tmp81.unpack.unpack183.elt190, align 8
  %tmp81.unpack.unpack183192 = insertvalue %"Vec<T = u8>" %3, i64 %tmp81.unpack.unpack183.unpack191, 2
  %tmp81.elt177 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %tmp81.unpack178.unpack = load ptr, ptr %tmp81.elt177, align 8
  %4 = insertvalue %"Vec<T = char>" poison, ptr %tmp81.unpack178.unpack, 0
  %tmp81.unpack178.elt193 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %tmp81.unpack178.unpack194 = load i64, ptr %tmp81.unpack178.elt193, align 8
  %5 = insertvalue %"Vec<T = char>" %4, i64 %tmp81.unpack178.unpack194, 1
  %tmp81.unpack178.elt195 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %tmp81.unpack178.unpack196 = load i64, ptr %tmp81.unpack178.elt195, align 8
  %tmp81.unpack178197 = insertvalue %"Vec<T = char>" %5, i64 %tmp81.unpack178.unpack196, 2
  %tmp81.elt179 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %tmp81.unpack180 = load i64, ptr %tmp81.elt179, align 8
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp81.unpack180)
  %fun_call_tmp2.i = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp81.unpack178197)
  %fun_call_tmp.i110 = call {} @"Vec<T = u8>:drop"(%"Vec<T = u8>" %tmp81.unpack.unpack183192)
  %fun_call_tmp.i130 = call {} @"u64:drop"(i64 %tmp81.unpack.unpack.unpack186)
  %fun_call_tmp2.i132 = call {} @"u64:drop"(i64 %tmp81.unpack.unpack.unpack)
  %fun_call_tmp83 = call {} @println({ ptr, i64 } { ptr @global_string.10, i64 9 })
  ret i64 0

else64:                                           ; preds = %then56
  %tmp68 = load i8, ptr %tmp5, align 1
  %fun_call_tmp69 = call {} @"char:drop"(i8 %tmp68)
  br label %cond57
}

define {} @"u64:drop"(i64 %0) {
entry:
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

define {} @"char:drop"(i8 %0) {
entry:
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

define ptr @"mem:heap_count"() {
entry:
  %tmp = load i64, ptr @count, align 4
  %fun_call_tmp = call {} @"u64:drop"(i64 %tmp)
  ret ptr @count
}

declare {} @c_free(ptr)

define i1 @"&u8:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define {} @"Vec<T = u8>:drop"(%"Vec<T = u8>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"Vec<T = u8>", align 8
  %.elt = extractvalue %"Vec<T = u8>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"Vec<T = u8>" %0, 1
  store i64 %.elt20, ptr %self.repack19, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt22 = extractvalue %"Vec<T = u8>" %0, 2
  store i64 %.elt22, ptr %self.repack21, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = u8>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp4
  %tmp6 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp7 = call {} @"u8:drop"(i8 %tmp6)
  %tmp.i = add i64 %tmp4, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = u8>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"mem:free<T = u8>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = u8>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = u8>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define { ptr, i64 } @"Vec<T = u8>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = u8>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = u8>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define {} @"u8:drop"(i8 %0) {
entry:
  ret {} zeroinitializer
}

define {} @"mem:free<T = u8>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define %"pixel:Game" @"pixel:Game:new"(%"Vec_2<T = u64>" %0, i64 %1) {
entry:
  %screen_size = alloca %"Vec_2<T = u64>", align 8
  %.elt = extractvalue %"Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack8 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt9 = extractvalue %"Vec_2<T = u64>" %0, 1
  store i64 %.elt9, ptr %screen_size.repack8, align 4
  %fun_call_tmp = call %"pixel:Frame" @"pixel:Frame:new"(%"Vec_2<T = u64>" %0)
  %fun_call_tmp1 = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %tmp_agg = insertvalue %"pixel:Game" undef, %"pixel:Frame" %fun_call_tmp, 0
  %tmp_agg3 = insertvalue %"pixel:Game" %tmp_agg, %"Vec<T = char>" %fun_call_tmp1, 1
  %tmp_constructor = insertvalue %"pixel:Game" %tmp_agg3, i64 %1, 2
  %fun_call_tmp5 = call {} @"u64:drop"(i64 %1)
  %tmp6.unpack = load i64, ptr %screen_size, align 4
  %tmp6.elt13 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp6.unpack14 = load i64, ptr %tmp6.elt13, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp6.unpack14)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp6.unpack)
  ret %"pixel:Game" %tmp_constructor
}

define %"pixel:Frame" @"pixel:Frame:new"(%"Vec_2<T = u64>" %0) {
entry:
  %tmp7 = alloca i64, align 8
  %tmp6 = alloca i64, align 8
  %x = alloca i64, align 8
  %tmp_iter4 = alloca %"Range<T = u64>", align 8
  %y = alloca i64, align 8
  %tmp_iter2 = alloca %"Range<T = u64>", align 8
  %buffer = alloca %"Vec<T = u8>", align 8
  %screen_size = alloca %"Vec_2<T = u64>", align 8
  %.elt = extractvalue %"Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack87 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt88 = extractvalue %"Vec_2<T = u64>" %0, 1
  store i64 %.elt88, ptr %screen_size.repack87, align 4
  %tmp.i61 = mul i64 %.elt, %.elt88
  %fun_call_tmp2 = call %"Vec<T = u8>" @"Vec<T = u8>:with_capacity"(i64 %tmp.i61)
  %fun_call_tmp2.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %buffer, align 8
  %buffer.repack89 = getelementptr inbounds nuw i8, ptr %buffer, i64 8
  %fun_call_tmp2.elt90 = extractvalue %"Vec<T = u8>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt90, ptr %buffer.repack89, align 8
  %buffer.repack91 = getelementptr inbounds nuw i8, ptr %buffer, i64 16
  %fun_call_tmp2.elt92 = extractvalue %"Vec<T = u8>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt92, ptr %buffer.repack91, align 8
  %screen_size.y3 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp4 = load i64, ptr %screen_size.y3, align 4
  store i64 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack93 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  store i64 %tmp4, ptr %tmp_iter2.repack93, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp7 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter2)
  store i64 %fun_call_tmp7, ptr %y, align 4
  %tmp10 = load i64, ptr %screen_size, align 4
  store i64 0, ptr %tmp_iter4, align 4
  %tmp_iter4.repack109 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  store i64 %tmp10, ptr %tmp_iter4.repack109, align 4
  br label %cond13

cond:                                             ; preds = %else14, %entry
  %fun_call_tmp6 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp6, label %then, label %else

else:                                             ; preds = %cond
  %tmp47.unpack = load i64, ptr %screen_size, align 4
  %1 = insertvalue %"Vec_2<T = u64>" poison, i64 %tmp47.unpack, 0
  %tmp47.elt95 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp47.unpack96 = load i64, ptr %tmp47.elt95, align 4
  %tmp4797 = insertvalue %"Vec_2<T = u64>" %1, i64 %tmp47.unpack96, 1
  %tmp48.unpack = load ptr, ptr %buffer, align 8
  %2 = insertvalue %"Vec<T = u8>" poison, ptr %tmp48.unpack, 0
  %tmp48.elt98 = getelementptr inbounds nuw i8, ptr %buffer, i64 8
  %tmp48.unpack99 = load i64, ptr %tmp48.elt98, align 8
  %3 = insertvalue %"Vec<T = u8>" %2, i64 %tmp48.unpack99, 1
  %tmp48.elt100 = getelementptr inbounds nuw i8, ptr %buffer, i64 16
  %tmp48.unpack101 = load i64, ptr %tmp48.elt100, align 8
  %tmp48102 = insertvalue %"Vec<T = u8>" %3, i64 %tmp48.unpack101, 2
  %tmp_agg = insertvalue %"pixel:Frame" undef, %"Vec_2<T = u64>" %tmp4797, 0
  %tmp_constructor = insertvalue %"pixel:Frame" %tmp_agg, %"Vec<T = u8>" %tmp48102, 1
  %tmp49.unpack = load i64, ptr %tmp_iter2, align 4
  %tmp49.elt103 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %tmp49.unpack104 = load i64, ptr %tmp49.elt103, align 4
  %fun_call_tmp.i79 = call {} @"u64:drop"(i64 %tmp49.unpack104)
  %fun_call_tmp2.i81 = call {} @"u64:drop"(i64 %tmp49.unpack)
  %tmp51.unpack = load i64, ptr %screen_size, align 4
  %tmp51.elt106 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp51.unpack107 = load i64, ptr %tmp51.elt106, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp51.unpack107)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp51.unpack)
  ret %"pixel:Frame" %tmp_constructor

then12:                                           ; preds = %cond13
  %fun_call_tmp16 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter4)
  store i64 %fun_call_tmp16, ptr %x, align 4
  %fun_call_tmp17 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %buffer)
  %tmp18 = load i64, ptr %y, align 4
  %tmp20 = load i64, ptr %screen_size, align 4
  %tmp.i58 = mul i64 %tmp18, %tmp20
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp17, 0
  %4 = getelementptr i8, ptr %ref.i, i64 %tmp.i58
  %tmp_index.i = getelementptr i8, ptr %4, i64 %fun_call_tmp16
  %tmp29 = load i64, ptr %x, align 4
  %tmp30 = load i64, ptr %y, align 4
  %tmp.i = add i64 %tmp29, %tmp30
  %tmp.i64 = and i64 %tmp.i, 1
  store i64 %tmp.i64, ptr %tmp6, align 4
  store i64 0, ptr %tmp7, align 4
  %tmp2.i = icmp eq i64 %tmp.i64, 0
  %spec.select = select i1 %tmp2.i, i8 0, i8 7
  store i8 %spec.select, ptr %tmp_index.i, align 1
  %tmp35 = load i64, ptr %tmp7, align 4
  %fun_call_tmp36 = call {} @"u64:drop"(i64 %tmp35)
  %tmp37 = load i64, ptr %tmp6, align 4
  %fun_call_tmp38 = call {} @"u64:drop"(i64 %tmp37)
  %tmp39 = load i64, ptr %x, align 4
  %fun_call_tmp40 = call {} @"u64:drop"(i64 %tmp39)
  br label %cond13

cond13:                                           ; preds = %then12, %then
  %fun_call_tmp15 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp15, label %then12, label %else14

else14:                                           ; preds = %cond13
  %tmp42.unpack = load i64, ptr %tmp_iter4, align 4
  %tmp42.elt111 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %tmp42.unpack112 = load i64, ptr %tmp42.elt111, align 4
  %fun_call_tmp.i84 = call {} @"u64:drop"(i64 %tmp42.unpack112)
  %fun_call_tmp2.i86 = call {} @"u64:drop"(i64 %tmp42.unpack)
  %tmp44 = load i64, ptr %y, align 4
  %fun_call_tmp45 = call {} @"u64:drop"(i64 %tmp44)
  br label %cond
}

define %"Vec<T = u8>" @"Vec<T = u8>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:alloc<T = u8>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = u8>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = u8>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = u8>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"u64:drop"(i64 %0)
  ret %"Vec<T = u8>" %tmp_constructor
}

define ptr @"mem:alloc<T = u8>"(i64 %0) {
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

declare ptr @c_malloc(i64)

define i1 @"Range<T = u64>:has_next"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load i64, ptr %tmp2.end, align 4
  %tmp.i = icmp ult i64 %tmp1, %tmp3
  ret i1 %tmp.i
}

define i64 @"Range<T = u64>:next"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  %tmp.i = add i64 %tmp1, 1
  store i64 %tmp.i, ptr %0, align 4
  %fun_call_tmp7 = call {} @"u64:drop"(i64 %tmp1)
  ret i64 %tmp1
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

define {} @"pixel:Game:update"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp.key_presses = getelementptr inbounds nuw i8, ptr %0, i64 40
  %fun_call_tmp = call {} @"Vec<T = char>:clear"(ptr nonnull %tmp.key_presses)
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp2 = call i8 @c_getch()
  %tmp3 = load ptr, ptr %self, align 8
  %tmp3.key_presses = getelementptr inbounds nuw i8, ptr %tmp3, i64 40
  %fun_call_tmp5 = call {} @"Vec<T = char>:push"(ptr nonnull %tmp3.key_presses, i8 %fun_call_tmp2)
  %fun_call_tmp7 = call {} @"char:drop"(i8 %fun_call_tmp2)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp1 = call i1 @c_kbhit()
  br i1 %fun_call_tmp1, label %then, label %else

else:                                             ; preds = %cond
  ret {} zeroinitializer
}

define {} @"Vec<T = char>:clear"(ptr %0) {
entry:
  %tmp_iter1 = alloca %"SliceIter<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr %0)
  %fun_call_tmp1 = call %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"SliceIter<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp_iter1, align 8
  %tmp_iter1.repack16 = getelementptr inbounds nuw i8, ptr %tmp_iter1, i64 8
  %fun_call_tmp1.elt17 = extractvalue %"SliceIter<T = char>" %fun_call_tmp1, 1
  store ptr %fun_call_tmp1.elt17, ptr %tmp_iter1.repack16, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call ptr @"SliceIter<T = char>:next"(ptr nonnull %tmp_iter1)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp2 = call i1 @"SliceIter<T = char>:has_next"(ptr nonnull %tmp_iter1)
  br i1 %fun_call_tmp2, label %then, label %else

else:                                             ; preds = %cond
  %tmp8 = load ptr, ptr %self, align 8
  %tmp8.len = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  store i64 0, ptr %tmp8.len, align 4
  ret {} zeroinitializer
}

define %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i = add i64 %.elt21, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"SliceIter<T = char>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"SliceIter<T = char>" %tmp_agg, ptr %cast7, 1
  ret %"SliceIter<T = char>" %tmp_constructor
}

define i1 @"SliceIter<T = char>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"SliceIter<T = char>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 1
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

declare i1 @c_kbhit()

declare i8 @c_getch()

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

declare {} @c_memcpy(ptr, ptr, i64)

define {} @"mem:copy<T = char>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define {} @"mem:Manually_Drop<T = char>:drop"(%"mem:Manually_Drop<T = char>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"pixel:Game:render"(ptr %0) {
entry:
  %fun_call_tmp = call {} @"pixel:Frame:render"(ptr %0)
  %tmp1.tick_speed = getelementptr inbounds nuw i8, ptr %0, i64 64
  %tmp2 = load i64, ptr %tmp1.tick_speed, align 4
  %cast = uitofp i64 %tmp2 to double
  %tmp.i = fdiv double 1.000000e+00, %cast
  %fun_call_tmp4 = call {} @sleep(double %tmp.i)
  ret {} zeroinitializer
}

define {} @"pixel:Frame:render"(ptr %0) {
entry:
  %x = alloca i64, align 8
  %tmp_iter3 = alloca %"Range<T = u64>", align 8
  %y = alloca i64, align 8
  %tmp_iter1 = alloca %"Range<T = u64>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call {} @print({ ptr, i64 } { ptr @global_string, i64 7 })
  %tmp.screen_size.y = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.screen_size.y, align 4
  store i64 0, ptr %tmp_iter1, align 4
  %tmp_iter1.repack58 = getelementptr inbounds nuw i8, ptr %tmp_iter1, i64 8
  store i64 %tmp1, ptr %tmp_iter1.repack58, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp4 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter1)
  store i64 %fun_call_tmp4, ptr %y, align 4
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load i64, ptr %tmp6, align 4
  store i64 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack63 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  store i64 %tmp7, ptr %tmp_iter3.repack63, align 4
  br label %cond10

cond:                                             ; preds = %else11, %entry
  %fun_call_tmp3 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter1)
  br i1 %fun_call_tmp3, label %then, label %else

else:                                             ; preds = %cond
  %fun_call_tmp36 = call {} @print({ ptr, i64 } { ptr @global_string.9, i64 4 })
  %tmp37.unpack = load i64, ptr %tmp_iter1, align 4
  %tmp37.elt60 = getelementptr inbounds nuw i8, ptr %tmp_iter1, i64 8
  %tmp37.unpack61 = load i64, ptr %tmp37.elt60, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp37.unpack61)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp37.unpack)
  ret {} zeroinitializer

then9:                                            ; preds = %cond10
  %fun_call_tmp13 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter3)
  store i64 %fun_call_tmp13, ptr %x, align 4
  %tmp14 = load ptr, ptr %self, align 8
  %tmp14.buffer = getelementptr inbounds nuw i8, ptr %tmp14, i64 16
  %fun_call_tmp15 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %tmp14.buffer)
  %tmp16 = load i64, ptr %y, align 4
  %tmp18 = load i64, ptr %tmp14, align 4
  %tmp.i43 = mul i64 %tmp16, %tmp18
  %tmp20 = load i64, ptr %x, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp15, 0
  %1 = getelementptr i8, ptr %ref.i, i64 %tmp.i43
  %tmp_index.i = getelementptr i8, ptr %1, i64 %tmp20
  %tmp23 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp24 = call { ptr, i64 } @"u8:ansi_code"(i8 %tmp23)
  %fun_call_tmp25 = call {} @print({ ptr, i64 } %fun_call_tmp24)
  %fun_call_tmp26 = call i8 @c_putchar(i8 32)
  %fun_call_tmp27 = call i8 @c_putchar(i8 32)
  %tmp28 = load i64, ptr %x, align 4
  %fun_call_tmp29 = call {} @"u64:drop"(i64 %tmp28)
  br label %cond10

cond10:                                           ; preds = %then9, %then
  %fun_call_tmp12 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp12, label %then9, label %else11

else11:                                           ; preds = %cond10
  %fun_call_tmp30 = call i8 @c_putchar(i8 10)
  %tmp31.unpack = load i64, ptr %tmp_iter3, align 4
  %tmp31.elt65 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %tmp31.unpack66 = load i64, ptr %tmp31.elt65, align 4
  %fun_call_tmp.i54 = call {} @"u64:drop"(i64 %tmp31.unpack66)
  %fun_call_tmp2.i56 = call {} @"u64:drop"(i64 %tmp31.unpack)
  %tmp33 = load i64, ptr %y, align 4
  %fun_call_tmp34 = call {} @"u64:drop"(i64 %tmp33)
  br label %cond
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

define ptr @"Vec<T = char>:null_terminate"(ptr %0) {
entry:
  %fun_call_tmp = call {} @"Vec<T = char>:push"(ptr %0, i8 0)
  ret ptr %0
}

declare {} @c_printf(ptr)

declare {} @c_flush()

define { ptr, i64 } @"u8:ansi_code"(i8 %0) {
entry:
  %self = alloca i8, align 1
  store i8 %0, ptr %self, align 1
  %cast = zext i8 %0 to i64
  %tmp_index.i = getelementptr { ptr, i64 }, ptr @ansi_codes, i64 %cast
  %tmp1.unpack = load ptr, ptr %tmp_index.i, align 8
  %1 = insertvalue { ptr, i64 } poison, ptr %tmp1.unpack, 0
  %tmp1.elt6 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 8
  %tmp1.unpack7 = load i64, ptr %tmp1.elt6, align 8
  %tmp18 = insertvalue { ptr, i64 } %1, i64 %tmp1.unpack7, 1
  %fun_call_tmp3 = call {} @"[&[char]; 8]:drop"([8 x { ptr, i64 }] [{ ptr, i64 } { ptr @global_string.1, i64 5 }, { ptr, i64 } { ptr @global_string.2, i64 5 }, { ptr, i64 } { ptr @global_string.3, i64 5 }, { ptr, i64 } { ptr @global_string.4, i64 5 }, { ptr, i64 } { ptr @global_string.5, i64 5 }, { ptr, i64 } { ptr @global_string.6, i64 5 }, { ptr, i64 } { ptr @global_string.7, i64 5 }, { ptr, i64 } { ptr @global_string.8, i64 5 }])
  %tmp4 = load i8, ptr %self, align 1
  %fun_call_tmp5 = call {} @"u8:drop"(i8 %tmp4)
  ret { ptr, i64 } %tmp18
}

define {} @"[&[char]; 8]:drop"([8 x { ptr, i64 }] %0) {
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
  %tmp.i11 = icmp ult i64 %storemerge, 8
  br i1 %tmp.i11, label %then, label %else

else:                                             ; preds = %cond
  %tmp7 = load i64, ptr %idx, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
  ret {} zeroinitializer
}

declare i8 @c_putchar(i8)

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
