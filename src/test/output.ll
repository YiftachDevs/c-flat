; ModuleID = 'main_module'
source_filename = "main_module"

%"Box:<T = &[u8]>" = type { ptr }

@global_string = private unnamed_addr constant [5 x i8] c"John\00", align 1

define i64 @main() {
entry:
  %box = alloca %"Box:<T = &[u8]>", align 8
  %fun_call_tmp = call %"Box:<T = &[u8]>" @"Box:<T = &[u8]>.new"({ ptr, i64 } { ptr @global_string, i64 4 })
  %0 = extractvalue %"Box:<T = &[u8]>" %fun_call_tmp, 0
  store ptr %0, ptr %box, align 8
  %fun_call_tmp2 = call {} @"Box:<T = &[u8]>.drop"(ptr nonnull %box)
  ret i64 1
}

define %"Box:<T = &[u8]>" @"Box:<T = &[u8]>.new"({ ptr, i64 } %0) {
entry:
  %value = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %value, align 8
  %value.repack6 = getelementptr inbounds nuw i8, ptr %value, i64 8
  %.elt7 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt7, ptr %value.repack6, align 8
  %fun_call_tmp1 = call ptr @malloc(i64 16)
  %fun_call_tmp4 = call {} @memcpy(ptr %fun_call_tmp1, ptr nonnull %value, i64 16)
  %tmp_constructor = insertvalue %"Box:<T = &[u8]>" undef, ptr %fun_call_tmp1, 0
  ret %"Box:<T = &[u8]>" %tmp_constructor
}

declare ptr @malloc(i64)

declare {} @memcpy(ptr, ptr, i64)

define {} @"Box:<T = &[u8]>.drop"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %fun_call_tmp = call {} @free(ptr %tmp1)
  ret {} zeroinitializer
}

declare {} @free(ptr)
