; ModuleID = 'main_module'
source_filename = "main_module"

%"SliceIter<T = char>" = type { ptr, ptr }
%"Range<T = u64>" = type { i64, i64 }
%"pixel:Game" = type { %"pixel:Frame", %"Vec<T = char>", i64 }
%"pixel:Frame" = type { %"Vec_2<T = u64>", %"Vec<T = u8>" }
%"Vec_2<T = u64>" = type { i64, i64 }
%"Vec<T = u8>" = type { ptr, i64, i64 }
%"Vec<T = char>" = type { ptr, i64, i64 }
%"SliceIter<T = u8>" = type { ptr, ptr }
%"mem:Manually_Drop<T = u8>" = type { i8 }
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
@global_string.11 = private unnamed_addr constant [2 x i8] c"0\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @game()
  %fun_call_tmp1 = call {} @println({ ptr, i64 } { ptr @global_string.10, i64 9 })
  %fun_call_tmp2 = call {} @debug_heap_count()
  ret i64 0
}

define {} @game() {
entry:
  %tmp672 = alloca i8, align 1
  %key_press = alloca ptr, align 8
  %tmp_iter461 = alloca %"SliceIter<T = char>", align 8
  %i = alloca i64, align 8
  %tmp_iter3 = alloca %"Range<T = u64>", align 8
  %x = alloca i64, align 8
  %tmp_iter4 = alloca %"Range<T = u64>", align 8
  %y = alloca i64, align 8
  %tmp_iter2 = alloca %"Range<T = u64>", align 8
  %game = alloca %"pixel:Game", align 8
  %fun_call_tmp = call %"pixel:Game" @"pixel:Game:new"(%"Vec_2<T = u64>" { i64 96, i64 54 }, i64 12)
  %fun_call_tmp.elt = extractvalue %"pixel:Game" %fun_call_tmp, 0
  %fun_call_tmp.elt.elt = extractvalue %"pixel:Frame" %fun_call_tmp.elt, 0
  %fun_call_tmp.elt.elt.elt = extractvalue %"Vec_2<T = u64>" %fun_call_tmp.elt.elt, 0
  store i64 %fun_call_tmp.elt.elt.elt, ptr %game, align 8
  %game.repack164 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %fun_call_tmp.elt.elt.elt165 = extractvalue %"Vec_2<T = u64>" %fun_call_tmp.elt.elt, 1
  store i64 %fun_call_tmp.elt.elt.elt165, ptr %game.repack164, align 8
  %game.repack162 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp.elt.elt163 = extractvalue %"pixel:Frame" %fun_call_tmp.elt, 1
  %fun_call_tmp.elt.elt163.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp.elt.elt163, 0
  store ptr %fun_call_tmp.elt.elt163.elt, ptr %game.repack162, align 8
  %game.repack162.repack166 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %fun_call_tmp.elt.elt163.elt167 = extractvalue %"Vec<T = u8>" %fun_call_tmp.elt.elt163, 1
  store i64 %fun_call_tmp.elt.elt163.elt167, ptr %game.repack162.repack166, align 8
  %game.repack162.repack168 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %fun_call_tmp.elt.elt163.elt169 = extractvalue %"Vec<T = u8>" %fun_call_tmp.elt.elt163, 2
  store i64 %fun_call_tmp.elt.elt163.elt169, ptr %game.repack162.repack168, align 8
  %game.repack158 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp.elt159 = extractvalue %"pixel:Game" %fun_call_tmp, 1
  %fun_call_tmp.elt159.elt = extractvalue %"Vec<T = char>" %fun_call_tmp.elt159, 0
  store ptr %fun_call_tmp.elt159.elt, ptr %game.repack158, align 8
  %game.repack158.repack170 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %fun_call_tmp.elt159.elt171 = extractvalue %"Vec<T = char>" %fun_call_tmp.elt159, 1
  store i64 %fun_call_tmp.elt159.elt171, ptr %game.repack158.repack170, align 8
  %game.repack158.repack172 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %fun_call_tmp.elt159.elt173 = extractvalue %"Vec<T = char>" %fun_call_tmp.elt159, 2
  store i64 %fun_call_tmp.elt159.elt173, ptr %game.repack158.repack172, align 8
  %game.repack160 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %fun_call_tmp.elt161 = extractvalue %"pixel:Game" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt161, ptr %game.repack160, align 8
  store i64 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack174 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  store i64 54, ptr %tmp_iter2.repack174, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp4 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter2)
  store i64 %fun_call_tmp4, ptr %y, align 4
  store i64 0, ptr %tmp_iter4, align 4
  %tmp_iter4.repack205 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  store i64 96, ptr %tmp_iter4.repack205, align 4
  br label %cond9

cond:                                             ; preds = %else10, %entry
  %fun_call_tmp3 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp3, label %then, label %then32

then8:                                            ; preds = %cond9
  %fun_call_tmp12 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter4)
  store i64 %fun_call_tmp12, ptr %x, align 4
  %game.frame.buffer = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp13 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %game.frame.buffer)
  %tmp14 = load i64, ptr %y, align 4
  %tmp.i100 = mul i64 %tmp14, 96
  %ref.i127 = extractvalue { ptr, i64 } %fun_call_tmp13, 0
  %0 = getelementptr i8, ptr %ref.i127, i64 %tmp.i100
  %tmp_index.i128 = getelementptr i8, ptr %0, i64 %fun_call_tmp12
  %tmp20 = load i64, ptr %x, align 4
  %tmp.i = add i64 %tmp20, %tmp14
  %1 = trunc i64 %tmp.i to i8
  %cast = and i8 %1, 7
  store i8 %cast, ptr %tmp_index.i128, align 1
  %fun_call_tmp25 = call {} @"u64:drop"(i64 %tmp20)
  br label %cond9

cond9:                                            ; preds = %then8, %then
  %fun_call_tmp11 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp11, label %then8, label %else10

else10:                                           ; preds = %cond9
  %tmp26.unpack = load i64, ptr %tmp_iter4, align 4
  %tmp26.elt206 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %tmp26.unpack207 = load i64, ptr %tmp26.elt206, align 4
  %fun_call_tmp.i155 = call {} @"u64:drop"(i64 %tmp26.unpack207)
  %fun_call_tmp2.i157 = call {} @"u64:drop"(i64 %tmp26.unpack)
  %tmp28 = load i64, ptr %y, align 4
  %fun_call_tmp29 = call {} @"u64:drop"(i64 %tmp28)
  br label %cond

then32:                                           ; preds = %cond, %else64
  %fun_call_tmp33 = call {} @"pixel:Game:update"(ptr nonnull %game)
  store i64 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack175 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  store i64 5184, ptr %tmp_iter3.repack175, align 4
  br label %cond40

then39:                                           ; preds = %cond40
  %fun_call_tmp43 = call i64 @"Range<T = u64>:next"(ptr nonnull %tmp_iter3)
  store i64 %fun_call_tmp43, ptr %i, align 4
  %game.frame44.buffer = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp45 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %game.frame44.buffer)
  %ref.i123 = extractvalue { ptr, i64 } %fun_call_tmp45, 0
  %tmp_index.i124 = getelementptr i8, ptr %ref.i123, i64 %fun_call_tmp43
  %game.frame48.buffer = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp49 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %game.frame48.buffer)
  %tmp50 = load i64, ptr %i, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp49, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp50
  %tmp52 = load i8, ptr %tmp_index.i, align 1
  %tmp.i116 = add i8 %tmp52, 7
  %tmp.i119 = and i8 %tmp.i116, 7
  store i8 %tmp.i119, ptr %tmp_index.i124, align 1
  %tmp55 = load i64, ptr %i, align 4
  %fun_call_tmp56 = call {} @"u64:drop"(i64 %tmp55)
  br label %cond40

cond40:                                           ; preds = %then39, %then32
  %fun_call_tmp42 = call i1 @"Range<T = u64>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp42, label %then39, label %else41

else41:                                           ; preds = %cond40
  %game.key_presses = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp59 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %game.key_presses)
  %fun_call_tmp60 = call %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %fun_call_tmp59)
  %fun_call_tmp60.elt = extractvalue %"SliceIter<T = char>" %fun_call_tmp60, 0
  store ptr %fun_call_tmp60.elt, ptr %tmp_iter461, align 8
  %tmp_iter461.repack176 = getelementptr inbounds nuw i8, ptr %tmp_iter461, i64 8
  %fun_call_tmp60.elt177 = extractvalue %"SliceIter<T = char>" %fun_call_tmp60, 1
  store ptr %fun_call_tmp60.elt177, ptr %tmp_iter461.repack176, align 8
  br label %cond63

then62:                                           ; preds = %cond63
  %fun_call_tmp66 = call ptr @"SliceIter<T = char>:next"(ptr nonnull %tmp_iter461)
  store ptr %fun_call_tmp66, ptr %key_press, align 8
  %tmp71 = load ptr, ptr %key_press, align 8
  store i8 113, ptr %tmp672, align 1
  %tmp.i131 = load i8, ptr %tmp71, align 1
  %tmp2.i = icmp eq i8 %tmp.i131, 113
  br i1 %tmp2.i, label %then68, label %else70

cond63:                                           ; preds = %else70, %else41
  %fun_call_tmp65 = call i1 @"SliceIter<T = char>:has_next"(ptr nonnull %tmp_iter461)
  br i1 %fun_call_tmp65, label %then62, label %else64

else64:                                           ; preds = %cond63
  %fun_call_tmp80 = call {} @"pixel:Game:render"(ptr nonnull %game)
  %tmp83.unpack = load i64, ptr %tmp_iter3, align 4
  %tmp83.elt178 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %tmp83.unpack179 = load i64, ptr %tmp83.elt178, align 4
  %fun_call_tmp.i150 = call {} @"u64:drop"(i64 %tmp83.unpack179)
  %fun_call_tmp2.i152 = call {} @"u64:drop"(i64 %tmp83.unpack)
  br label %then32

then68:                                           ; preds = %then62
  %tmp86.unpack = load i64, ptr %tmp_iter2, align 4
  %tmp86.elt181 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %tmp86.unpack182 = load i64, ptr %tmp86.elt181, align 4
  %fun_call_tmp.i145 = call {} @"u64:drop"(i64 %tmp86.unpack182)
  %fun_call_tmp2.i147 = call {} @"u64:drop"(i64 %tmp86.unpack)
  %tmp88.unpack.unpack.unpack = load i64, ptr %game, align 8
  %tmp88.unpack.unpack.elt192 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %tmp88.unpack.unpack.unpack193 = load i64, ptr %tmp88.unpack.unpack.elt192, align 8
  %tmp88.unpack.elt189 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %tmp88.unpack.unpack190.unpack = load ptr, ptr %tmp88.unpack.elt189, align 8
  %2 = insertvalue %"Vec<T = u8>" poison, ptr %tmp88.unpack.unpack190.unpack, 0
  %tmp88.unpack.unpack190.elt195 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp88.unpack.unpack190.unpack196 = load i64, ptr %tmp88.unpack.unpack190.elt195, align 8
  %3 = insertvalue %"Vec<T = u8>" %2, i64 %tmp88.unpack.unpack190.unpack196, 1
  %tmp88.unpack.unpack190.elt197 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp88.unpack.unpack190.unpack198 = load i64, ptr %tmp88.unpack.unpack190.elt197, align 8
  %tmp88.unpack.unpack190199 = insertvalue %"Vec<T = u8>" %3, i64 %tmp88.unpack.unpack190.unpack198, 2
  %tmp88.elt184 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %tmp88.unpack185.unpack = load ptr, ptr %tmp88.elt184, align 8
  %4 = insertvalue %"Vec<T = char>" poison, ptr %tmp88.unpack185.unpack, 0
  %tmp88.unpack185.elt200 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %tmp88.unpack185.unpack201 = load i64, ptr %tmp88.unpack185.elt200, align 8
  %5 = insertvalue %"Vec<T = char>" %4, i64 %tmp88.unpack185.unpack201, 1
  %tmp88.unpack185.elt202 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %tmp88.unpack185.unpack203 = load i64, ptr %tmp88.unpack185.elt202, align 8
  %tmp88.unpack185204 = insertvalue %"Vec<T = char>" %5, i64 %tmp88.unpack185.unpack203, 2
  %tmp88.elt186 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %tmp88.unpack187 = load i64, ptr %tmp88.elt186, align 8
  %fun_call_tmp.i140 = call {} @"u64:drop"(i64 %tmp88.unpack187)
  %fun_call_tmp2.i142 = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp88.unpack185204)
  %fun_call_tmp.i.i = call {} @"Vec<T = u8>:drop"(%"Vec<T = u8>" %tmp88.unpack.unpack190199)
  %fun_call_tmp.i.i.i = call {} @"u64:drop"(i64 %tmp88.unpack.unpack.unpack193)
  %fun_call_tmp2.i.i.i = call {} @"u64:drop"(i64 %tmp88.unpack.unpack.unpack)
  %fun_call_tmp.i = call {} @"u64:drop"(i64 54)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 96)
  ret {} zeroinitializer

else70:                                           ; preds = %then62
  %tmp75 = load i8, ptr %tmp672, align 1
  %fun_call_tmp76 = call {} @"char:drop"(i8 %tmp75)
  br label %cond63
}

define i64 @"u64:clone"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  ret i64 %tmp1
}

define {} @"u64:drop"(i64 %0) {
entry:
  ret {} zeroinitializer
}

define i1 @"&u8:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define %"Vec<T = u8>" @"Vec<T = u8>:clone"(ptr %0) {
entry:
  %tmp_iter2 = alloca %"SliceIter<T = u8>", align 8
  %clone = alloca %"Vec<T = u8>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call %"Vec<T = u8>" @"Vec<T = u8>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %clone, align 8
  %clone.repack22 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %fun_call_tmp.elt23 = extractvalue %"Vec<T = u8>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt23, ptr %clone.repack22, align 8
  %clone.repack24 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %fun_call_tmp.elt25 = extractvalue %"Vec<T = u8>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt25, ptr %clone.repack24, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp1 = call { ptr, i64 } @"Vec<T = u8>:deref"(ptr %tmp)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp1, 1
  %fun_call_tmp3 = call {} @"Vec<T = u8>:reserve"(ptr nonnull %clone, i64 %len.i)
  %fun_call_tmp5 = call { ptr, i64 } @"Vec<T = u8>:deref"(ptr %tmp)
  %fun_call_tmp6 = call %"SliceIter<T = u8>" @"[u8]:iter"({ ptr, i64 } %fun_call_tmp5)
  %fun_call_tmp6.elt = extractvalue %"SliceIter<T = u8>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack26 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp6.elt27 = extractvalue %"SliceIter<T = u8>" %fun_call_tmp6, 1
  store ptr %fun_call_tmp6.elt27, ptr %tmp_iter2.repack26, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp8 = call ptr @"SliceIter<T = u8>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call i8 @"u8:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"Vec<T = u8>:push"(ptr nonnull %clone, i8 %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"SliceIter<T = u8>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp7, label %then, label %else

else:                                             ; preds = %cond
  %tmp14.unpack = load ptr, ptr %clone, align 8
  %1 = insertvalue %"Vec<T = u8>" poison, ptr %tmp14.unpack, 0
  %tmp14.elt28 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %tmp14.unpack29 = load i64, ptr %tmp14.elt28, align 8
  %2 = insertvalue %"Vec<T = u8>" %1, i64 %tmp14.unpack29, 1
  %tmp14.elt30 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %tmp14.unpack31 = load i64, ptr %tmp14.elt30, align 8
  %tmp1432 = insertvalue %"Vec<T = u8>" %2, i64 %tmp14.unpack31, 2
  ret %"Vec<T = u8>" %tmp1432
}

define %"Vec<T = u8>" @"Vec<T = u8>:new"() {
entry:
  %fun_call_tmp = call %"Vec<T = u8>" @"Vec<T = u8>:with_capacity"(i64 4)
  ret %"Vec<T = u8>" %fun_call_tmp
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

define ptr @"mem:heap_count"() {
entry:
  %tmp = load i64, ptr @count, align 4
  %fun_call_tmp = call {} @"u64:drop"(i64 %tmp)
  ret ptr @count
}

declare ptr @c_malloc(i64)

define {} @"Vec<T = u8>:reserve"(ptr %0, i64 %1) {
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
  %fun_call_tmp21 = call ptr @"mem:alloc<T = u8>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"mem:copy_range<T = u8>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"mem:free<T = u8>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"mem:copy_range<T = u8>"(ptr %0, ptr %1, i64 %2) {
entry:
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %2)
  %fun_call_tmp6 = call {} @"u64:drop"(i64 %2)
  ret {} zeroinitializer
}

declare {} @c_memcpy(ptr, ptr, i64)

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

declare {} @c_free(ptr)

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

define %"SliceIter<T = u8>" @"[u8]:iter"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i = add i64 %.elt21, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"SliceIter<T = u8>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"SliceIter<T = u8>" %tmp_agg, ptr %cast7, 1
  ret %"SliceIter<T = u8>" %tmp_constructor
}

define ptr @"&u8:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
}

define i1 @"SliceIter<T = u8>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"SliceIter<T = u8>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 1
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"Vec<T = u8>:push"(ptr %0, i8 %1) {
entry:
  %value = alloca i8, align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 %1, ptr %value, align 1
  %fun_call_tmp = call {} @"Vec<T = u8>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i23 = add i64 %tmp4, %cast
  %cast8 = inttoptr i64 %tmp.i23 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = u8>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load i8, ptr %value, align 1
  %tmp1528 = insertvalue %"mem:Manually_Drop<T = u8>" poison, i8 %tmp14, 0
  %fun_call_tmp16 = call {} @"mem:Manually_Drop<T = u8>:drop"(%"mem:Manually_Drop<T = u8>" %tmp1528)
  %tmp17 = load i8, ptr %value, align 1
  %fun_call_tmp18 = call {} @"u8:drop"(i8 %tmp17)
  ret {} zeroinitializer
}

define {} @"mem:copy<T = u8>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define i8 @"u8:clone"(ptr %0) {
entry:
  %tmp1 = load i8, ptr %0, align 1
  ret i8 %tmp1
}

define {} @"mem:Manually_Drop<T = u8>:drop"(%"mem:Manually_Drop<T = u8>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"u8:drop"(i8 %0) {
entry:
  ret {} zeroinitializer
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

define i1 @"&char:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define %"Vec<T = char>" @"Vec<T = char>:clone"(ptr %0) {
entry:
  %tmp_iter2 = alloca %"SliceIter<T = char>", align 8
  %clone = alloca %"Vec<T = char>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %fun_call_tmp.elt = extractvalue %"Vec<T = char>" %fun_call_tmp, 0
  store ptr %fun_call_tmp.elt, ptr %clone, align 8
  %clone.repack22 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %fun_call_tmp.elt23 = extractvalue %"Vec<T = char>" %fun_call_tmp, 1
  store i64 %fun_call_tmp.elt23, ptr %clone.repack22, align 8
  %clone.repack24 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %fun_call_tmp.elt25 = extractvalue %"Vec<T = char>" %fun_call_tmp, 2
  store i64 %fun_call_tmp.elt25, ptr %clone.repack24, align 8
  %tmp = load ptr, ptr %self, align 8
  %fun_call_tmp1 = call { ptr, i64 } @"Vec<T = char>:deref"(ptr %tmp)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp1, 1
  %fun_call_tmp3 = call {} @"Vec<T = char>:reserve"(ptr nonnull %clone, i64 %len.i)
  %fun_call_tmp5 = call { ptr, i64 } @"Vec<T = char>:deref"(ptr %tmp)
  %fun_call_tmp6 = call %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %fun_call_tmp5)
  %fun_call_tmp6.elt = extractvalue %"SliceIter<T = char>" %fun_call_tmp6, 0
  store ptr %fun_call_tmp6.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack26 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp6.elt27 = extractvalue %"SliceIter<T = char>" %fun_call_tmp6, 1
  store ptr %fun_call_tmp6.elt27, ptr %tmp_iter2.repack26, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp8 = call ptr @"SliceIter<T = char>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call i8 @"char:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"Vec<T = char>:push"(ptr nonnull %clone, i8 %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"SliceIter<T = char>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp7, label %then, label %else

else:                                             ; preds = %cond
  %tmp14.unpack = load ptr, ptr %clone, align 8
  %1 = insertvalue %"Vec<T = char>" poison, ptr %tmp14.unpack, 0
  %tmp14.elt28 = getelementptr inbounds nuw i8, ptr %clone, i64 8
  %tmp14.unpack29 = load i64, ptr %tmp14.elt28, align 8
  %2 = insertvalue %"Vec<T = char>" %1, i64 %tmp14.unpack29, 1
  %tmp14.elt30 = getelementptr inbounds nuw i8, ptr %clone, i64 16
  %tmp14.unpack31 = load i64, ptr %tmp14.elt30, align 8
  %tmp1432 = insertvalue %"Vec<T = char>" %2, i64 %tmp14.unpack31, 2
  ret %"Vec<T = char>" %tmp1432
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

define ptr @"&char:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
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

define {} @"mem:copy<T = char>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 1)
  ret {} zeroinitializer
}

define i8 @"char:clone"(ptr %0) {
entry:
  %tmp1 = load i8, ptr %0, align 1
  ret i8 %tmp1
}

define {} @"mem:Manually_Drop<T = char>:drop"(%"mem:Manually_Drop<T = char>" %0) {
entry:
  ret {} zeroinitializer
}

define {} @"char:drop"(i8 %0) {
entry:
  ret {} zeroinitializer
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

declare i1 @c_kbhit()

declare i8 @c_getch()

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

define {} @debug_heap_count() {
entry:
  %tmp1 = alloca %"Vec<T = char>", align 8
  %heap_count = alloca i64, align 8
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  store i64 %tmp, ptr %heap_count, align 4
  %fun_call_tmp1 = call %"Vec<T = char>" @"u64:to_string"(ptr nonnull %heap_count)
  %fun_call_tmp1.elt = extractvalue %"Vec<T = char>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp1, align 8
  %tmp1.repack8 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %fun_call_tmp1.elt9 = extractvalue %"Vec<T = char>" %fun_call_tmp1, 1
  store i64 %fun_call_tmp1.elt9, ptr %tmp1.repack8, align 8
  %tmp1.repack10 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %fun_call_tmp1.elt11 = extractvalue %"Vec<T = char>" %fun_call_tmp1, 2
  store i64 %fun_call_tmp1.elt11, ptr %tmp1.repack10, align 8
  %fun_call_tmp2 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %tmp1)
  %fun_call_tmp3 = call {} @println({ ptr, i64 } %fun_call_tmp2)
  %tmp4.unpack = load ptr, ptr %tmp1, align 8
  %0 = insertvalue %"Vec<T = char>" poison, ptr %tmp4.unpack, 0
  %tmp4.elt12 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4.unpack13 = load i64, ptr %tmp4.elt12, align 8
  %1 = insertvalue %"Vec<T = char>" %0, i64 %tmp4.unpack13, 1
  %tmp4.elt14 = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp4.unpack15 = load i64, ptr %tmp4.elt14, align 8
  %tmp416 = insertvalue %"Vec<T = char>" %1, i64 %tmp4.unpack15, 2
  %fun_call_tmp5 = call {} @"Vec<T = char>:drop"(%"Vec<T = char>" %tmp416)
  %tmp6 = load i64, ptr %heap_count, align 4
  %fun_call_tmp7 = call {} @"u64:drop"(i64 %tmp6)
  ret {} zeroinitializer
}

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
  %fun_call_tmp1 = call %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } { ptr @global_string.11, i64 1 })
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
