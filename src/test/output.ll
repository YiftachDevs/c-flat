; ModuleID = 'main_module'
source_filename = "main_module"

define i32 @main() {
entry:
  %fun_call_tmp = call i32 @getchar()
  %tmp.i35 = icmp eq i32 %fun_call_tmp, 113
  br i1 %tmp.i35, label %common.ret, label %cond6

common.ret:                                       ; preds = %cond29, %entry, %then23
  %common.ret.op = phi i32 [ %tmp.i37, %then23 ], [ 0, %entry ], [ %spec.select, %cond29 ]
  ret i32 %common.ret.op

then5:                                            ; preds = %cond6
  %fun_call_tmp10 = call i32 @getchar()
  switch i32 %fun_call_tmp10, label %else19 [
    i32 42, label %cond24
    i32 33, label %cond6
  ]

cond6:                                            ; preds = %then5, %entry
  %fun_call_tmp8 = call i32 @getchar()
  %tmp.i36.not = icmp eq i32 %fun_call_tmp8, 10
  br i1 %tmp.i36.not, label %cond24, label %then5

else19:                                           ; preds = %then5
  br label %cond24

then23:                                           ; preds = %cond24
  %tmp.i37 = add nuw nsw i32 %result, 5
  br label %common.ret

cond24:                                           ; preds = %cond6, %then5, %else19
  %tmp.i38 = phi i1 [ true, %else19 ], [ false, %then5 ], [ false, %cond6 ]
  %result = phi i32 [ 4, %else19 ], [ 3, %then5 ], [ 54, %cond6 ]
  br i1 %tmp.i36.not, label %then23, label %cond29

cond29:                                           ; preds = %cond24
  %spec.select = select i1 %tmp.i38, i32 123, i32 %result
  br label %common.ret
}

declare i32 @getchar()
