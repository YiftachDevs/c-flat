; ModuleID = 'main_module'
source_filename = "main_module"

%"Vec<T = char>" = type { ptr, i64, i64 }
%"SliceMutIter<T = Particle>" = type { ptr, ptr }
%"Range<T = u64>" = type { i64, i64 }
%"SliceIter<T = char>" = type { ptr, ptr }
%Player = type { %"Vec_2<T = f64>", %"Vec_2<T = f64>" }
%"Vec_2<T = f64>" = type { double, double }
%"Vec<T = Particle>" = type { ptr, i64, i64 }
%"TileMap<W = \22i64 16\22, H = \22i64 9\22>" = type { [16 x [9 x i8]] }
%"pixel:Game" = type { %"pixel:Frame", %"Vec<T = char>", i64, i64 }
%"pixel:Frame" = type { %"Vec_2<T = u64>", %"Vec<T = u8>" }
%"Vec_2<T = u64>" = type { i64, i64 }
%"Vec<T = u8>" = type { ptr, i64, i64 }
%Particle = type { %"Vec_2<T = f64>", double, double, double, i8 }
%"SliceIter<T = u8>" = type { ptr, ptr }
%"mem:Manually_Drop<T = u8>" = type { i8 }
%"mem:Manually_Drop<T = char>" = type { i8 }
%"mem:Manually_Drop<T = Particle>" = type { %Particle }
%"Range<T = f64>" = type { double, double }
%"SliceMutIter<T = u8>" = type { ptr, ptr }

@count = internal global i64 0
@seed = internal global i64 91237
@global_string = private unnamed_addr constant [8 x i8] c"\1B[2J\1B[H\00", align 1
@global_string.123 = private unnamed_addr constant [6 x i8] c"\1B[40m\00", align 1
@global_string.124 = private unnamed_addr constant [6 x i8] c"\1B[41m\00", align 1
@global_string.125 = private unnamed_addr constant [6 x i8] c"\1B[42m\00", align 1
@global_string.126 = private unnamed_addr constant [6 x i8] c"\1B[43m\00", align 1
@global_string.127 = private unnamed_addr constant [6 x i8] c"\1B[44m\00", align 1
@global_string.128 = private unnamed_addr constant [6 x i8] c"\1B[45m\00", align 1
@global_string.129 = private unnamed_addr constant [6 x i8] c"\1B[46m\00", align 1
@global_string.130 = private unnamed_addr constant [6 x i8] c"\1B[47m\00", align 1
@ansi_codes = internal constant [8 x { ptr, i64 }] [{ ptr, i64 } { ptr @global_string.123, i64 5 }, { ptr, i64 } { ptr @global_string.124, i64 5 }, { ptr, i64 } { ptr @global_string.125, i64 5 }, { ptr, i64 } { ptr @global_string.126, i64 5 }, { ptr, i64 } { ptr @global_string.127, i64 5 }, { ptr, i64 } { ptr @global_string.128, i64 5 }, { ptr, i64 } { ptr @global_string.129, i64 5 }, { ptr, i64 } { ptr @global_string.130, i64 5 }]
@global_string.131 = private unnamed_addr constant [5 x i8] c"\1B[0m\00", align 1
@global_string.132 = private unnamed_addr constant [2 x i8] c"0\00", align 1
@global_string.135 = private unnamed_addr constant [10 x i8] c"game over\00", align 1

define i64 @main() {
entry:
  %fun_call_tmp = call {} @game()
  %fun_call_tmp1 = call {} @println({ ptr, i64 } { ptr @global_string.135, i64 9 })
  %fun_call_tmp2 = call {} @debug_heap_count()
  ret i64 0
}

define {} @game() {
entry:
  %tmp8 = alloca %"Vec<T = char>", align 8
  %tmp7159 = alloca i64, align 8
  %tmp_iter11127 = alloca %"SliceMutIter<T = Particle>", align 8
  %tmp14 = alloca i64, align 8
  %tmp13 = alloca i64, align 8
  %x93 = alloca i64, align 8
  %tmp_iter11 = alloca %"Range<T = u64>", align 8
  %tmp10 = alloca i64, align 8
  %tmp984 = alloca i64, align 8
  %tmp_iter8 = alloca %"SliceMutIter<T = Particle>", align 8
  %tmp9 = alloca i8, align 1
  %key_press = alloca ptr, align 8
  %tmp_iter7 = alloca %"SliceIter<T = char>", align 8
  %player = alloca %Player, align 8
  %i = alloca i64, align 8
  %tmp_iter5 = alloca %"Range<T = u64>", align 8
  %particles = alloca %"Vec<T = Particle>", align 8
  %tmp6 = alloca i64, align 8
  %tmp5 = alloca i64, align 8
  %x = alloca i64, align 8
  %tmp_iter3 = alloca %"Range<T = u64>", align 8
  %tile_map = alloca %"TileMap<W = \22i64 16\22, H = \22i64 9\22>", align 8
  %game = alloca %"pixel:Game", align 8
  %screen_size = alloca %"Vec_2<T = u64>", align 8
  store i64 64, ptr %screen_size, align 4
  %screen_size.repack284 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  store i64 36, ptr %screen_size.repack284, align 4
  %fun_call_tmp2 = call %"pixel:Game" @"pixel:Game:new"(%"Vec_2<T = u64>" { i64 64, i64 36 }, i64 20)
  %fun_call_tmp2.elt = extractvalue %"pixel:Game" %fun_call_tmp2, 0
  %fun_call_tmp2.elt.elt = extractvalue %"pixel:Frame" %fun_call_tmp2.elt, 0
  %fun_call_tmp2.elt.elt.elt = extractvalue %"Vec_2<T = u64>" %fun_call_tmp2.elt.elt, 0
  store i64 %fun_call_tmp2.elt.elt.elt, ptr %game, align 8
  %game.repack296 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %fun_call_tmp2.elt.elt.elt297 = extractvalue %"Vec_2<T = u64>" %fun_call_tmp2.elt.elt, 1
  store i64 %fun_call_tmp2.elt.elt.elt297, ptr %game.repack296, align 8
  %game.repack294 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %fun_call_tmp2.elt.elt295 = extractvalue %"pixel:Frame" %fun_call_tmp2.elt, 1
  %fun_call_tmp2.elt.elt295.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp2.elt.elt295, 0
  store ptr %fun_call_tmp2.elt.elt295.elt, ptr %game.repack294, align 8
  %game.repack294.repack298 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %fun_call_tmp2.elt.elt295.elt299 = extractvalue %"Vec<T = u8>" %fun_call_tmp2.elt.elt295, 1
  store i64 %fun_call_tmp2.elt.elt295.elt299, ptr %game.repack294.repack298, align 8
  %game.repack294.repack300 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %fun_call_tmp2.elt.elt295.elt301 = extractvalue %"Vec<T = u8>" %fun_call_tmp2.elt.elt295, 2
  store i64 %fun_call_tmp2.elt.elt295.elt301, ptr %game.repack294.repack300, align 8
  %game.repack288 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp2.elt289 = extractvalue %"pixel:Game" %fun_call_tmp2, 1
  %fun_call_tmp2.elt289.elt = extractvalue %"Vec<T = char>" %fun_call_tmp2.elt289, 0
  store ptr %fun_call_tmp2.elt289.elt, ptr %game.repack288, align 8
  %game.repack288.repack302 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %fun_call_tmp2.elt289.elt303 = extractvalue %"Vec<T = char>" %fun_call_tmp2.elt289, 1
  store i64 %fun_call_tmp2.elt289.elt303, ptr %game.repack288.repack302, align 8
  %game.repack288.repack304 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %fun_call_tmp2.elt289.elt305 = extractvalue %"Vec<T = char>" %fun_call_tmp2.elt289, 2
  store i64 %fun_call_tmp2.elt289.elt305, ptr %game.repack288.repack304, align 8
  %game.repack290 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %fun_call_tmp2.elt291 = extractvalue %"pixel:Game" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt291, ptr %game.repack290, align 8
  %game.repack292 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %fun_call_tmp2.elt293 = extractvalue %"pixel:Game" %fun_call_tmp2, 3
  store i64 %fun_call_tmp2.elt293, ptr %game.repack292, align 8
  %fun_call_tmp3 = call %"TileMap<W = \22i64 16\22, H = \22i64 9\22>" @"TileMap<W = \22i64 16\22, H = \22i64 9\22>:new"()
  %0 = extractvalue %"TileMap<W = \22i64 16\22, H = \22i64 9\22>" %fun_call_tmp3, 0
  %.elt = extractvalue [16 x [9 x i8]] %0, 0
  %.elt.elt = extractvalue [9 x i8] %.elt, 0
  store i8 %.elt.elt, ptr %tile_map, align 1
  %tile_map.repack336 = getelementptr inbounds nuw i8, ptr %tile_map, i64 1
  %.elt.elt337 = extractvalue [9 x i8] %.elt, 1
  store i8 %.elt.elt337, ptr %tile_map.repack336, align 1
  %tile_map.repack338 = getelementptr inbounds nuw i8, ptr %tile_map, i64 2
  %.elt.elt339 = extractvalue [9 x i8] %.elt, 2
  store i8 %.elt.elt339, ptr %tile_map.repack338, align 1
  %tile_map.repack340 = getelementptr inbounds nuw i8, ptr %tile_map, i64 3
  %.elt.elt341 = extractvalue [9 x i8] %.elt, 3
  store i8 %.elt.elt341, ptr %tile_map.repack340, align 1
  %tile_map.repack342 = getelementptr inbounds nuw i8, ptr %tile_map, i64 4
  %.elt.elt343 = extractvalue [9 x i8] %.elt, 4
  store i8 %.elt.elt343, ptr %tile_map.repack342, align 1
  %tile_map.repack344 = getelementptr inbounds nuw i8, ptr %tile_map, i64 5
  %.elt.elt345 = extractvalue [9 x i8] %.elt, 5
  store i8 %.elt.elt345, ptr %tile_map.repack344, align 1
  %tile_map.repack346 = getelementptr inbounds nuw i8, ptr %tile_map, i64 6
  %.elt.elt347 = extractvalue [9 x i8] %.elt, 6
  store i8 %.elt.elt347, ptr %tile_map.repack346, align 1
  %tile_map.repack348 = getelementptr inbounds nuw i8, ptr %tile_map, i64 7
  %.elt.elt349 = extractvalue [9 x i8] %.elt, 7
  store i8 %.elt.elt349, ptr %tile_map.repack348, align 1
  %tile_map.repack350 = getelementptr inbounds nuw i8, ptr %tile_map, i64 8
  %.elt.elt351 = extractvalue [9 x i8] %.elt, 8
  store i8 %.elt.elt351, ptr %tile_map.repack350, align 1
  %tile_map.repack306 = getelementptr inbounds nuw i8, ptr %tile_map, i64 9
  %.elt307 = extractvalue [16 x [9 x i8]] %0, 1
  %.elt307.elt = extractvalue [9 x i8] %.elt307, 0
  store i8 %.elt307.elt, ptr %tile_map.repack306, align 1
  %tile_map.repack306.repack352 = getelementptr inbounds nuw i8, ptr %tile_map, i64 10
  %.elt307.elt353 = extractvalue [9 x i8] %.elt307, 1
  store i8 %.elt307.elt353, ptr %tile_map.repack306.repack352, align 1
  %tile_map.repack306.repack354 = getelementptr inbounds nuw i8, ptr %tile_map, i64 11
  %.elt307.elt355 = extractvalue [9 x i8] %.elt307, 2
  store i8 %.elt307.elt355, ptr %tile_map.repack306.repack354, align 1
  %tile_map.repack306.repack356 = getelementptr inbounds nuw i8, ptr %tile_map, i64 12
  %.elt307.elt357 = extractvalue [9 x i8] %.elt307, 3
  store i8 %.elt307.elt357, ptr %tile_map.repack306.repack356, align 1
  %tile_map.repack306.repack358 = getelementptr inbounds nuw i8, ptr %tile_map, i64 13
  %.elt307.elt359 = extractvalue [9 x i8] %.elt307, 4
  store i8 %.elt307.elt359, ptr %tile_map.repack306.repack358, align 1
  %tile_map.repack306.repack360 = getelementptr inbounds nuw i8, ptr %tile_map, i64 14
  %.elt307.elt361 = extractvalue [9 x i8] %.elt307, 5
  store i8 %.elt307.elt361, ptr %tile_map.repack306.repack360, align 1
  %tile_map.repack306.repack362 = getelementptr inbounds nuw i8, ptr %tile_map, i64 15
  %.elt307.elt363 = extractvalue [9 x i8] %.elt307, 6
  store i8 %.elt307.elt363, ptr %tile_map.repack306.repack362, align 1
  %tile_map.repack306.repack364 = getelementptr inbounds nuw i8, ptr %tile_map, i64 16
  %.elt307.elt365 = extractvalue [9 x i8] %.elt307, 7
  store i8 %.elt307.elt365, ptr %tile_map.repack306.repack364, align 1
  %tile_map.repack306.repack366 = getelementptr inbounds nuw i8, ptr %tile_map, i64 17
  %.elt307.elt367 = extractvalue [9 x i8] %.elt307, 8
  store i8 %.elt307.elt367, ptr %tile_map.repack306.repack366, align 1
  %tile_map.repack308 = getelementptr inbounds nuw i8, ptr %tile_map, i64 18
  %.elt309 = extractvalue [16 x [9 x i8]] %0, 2
  %.elt309.elt = extractvalue [9 x i8] %.elt309, 0
  store i8 %.elt309.elt, ptr %tile_map.repack308, align 1
  %tile_map.repack308.repack368 = getelementptr inbounds nuw i8, ptr %tile_map, i64 19
  %.elt309.elt369 = extractvalue [9 x i8] %.elt309, 1
  store i8 %.elt309.elt369, ptr %tile_map.repack308.repack368, align 1
  %tile_map.repack308.repack370 = getelementptr inbounds nuw i8, ptr %tile_map, i64 20
  %.elt309.elt371 = extractvalue [9 x i8] %.elt309, 2
  store i8 %.elt309.elt371, ptr %tile_map.repack308.repack370, align 1
  %tile_map.repack308.repack372 = getelementptr inbounds nuw i8, ptr %tile_map, i64 21
  %.elt309.elt373 = extractvalue [9 x i8] %.elt309, 3
  store i8 %.elt309.elt373, ptr %tile_map.repack308.repack372, align 1
  %tile_map.repack308.repack374 = getelementptr inbounds nuw i8, ptr %tile_map, i64 22
  %.elt309.elt375 = extractvalue [9 x i8] %.elt309, 4
  store i8 %.elt309.elt375, ptr %tile_map.repack308.repack374, align 1
  %tile_map.repack308.repack376 = getelementptr inbounds nuw i8, ptr %tile_map, i64 23
  %.elt309.elt377 = extractvalue [9 x i8] %.elt309, 5
  store i8 %.elt309.elt377, ptr %tile_map.repack308.repack376, align 1
  %tile_map.repack308.repack378 = getelementptr inbounds nuw i8, ptr %tile_map, i64 24
  %.elt309.elt379 = extractvalue [9 x i8] %.elt309, 6
  store i8 %.elt309.elt379, ptr %tile_map.repack308.repack378, align 1
  %tile_map.repack308.repack380 = getelementptr inbounds nuw i8, ptr %tile_map, i64 25
  %.elt309.elt381 = extractvalue [9 x i8] %.elt309, 7
  store i8 %.elt309.elt381, ptr %tile_map.repack308.repack380, align 1
  %tile_map.repack308.repack382 = getelementptr inbounds nuw i8, ptr %tile_map, i64 26
  %.elt309.elt383 = extractvalue [9 x i8] %.elt309, 8
  store i8 %.elt309.elt383, ptr %tile_map.repack308.repack382, align 1
  %tile_map.repack310 = getelementptr inbounds nuw i8, ptr %tile_map, i64 27
  %.elt311 = extractvalue [16 x [9 x i8]] %0, 3
  %.elt311.elt = extractvalue [9 x i8] %.elt311, 0
  store i8 %.elt311.elt, ptr %tile_map.repack310, align 1
  %tile_map.repack310.repack384 = getelementptr inbounds nuw i8, ptr %tile_map, i64 28
  %.elt311.elt385 = extractvalue [9 x i8] %.elt311, 1
  store i8 %.elt311.elt385, ptr %tile_map.repack310.repack384, align 1
  %tile_map.repack310.repack386 = getelementptr inbounds nuw i8, ptr %tile_map, i64 29
  %.elt311.elt387 = extractvalue [9 x i8] %.elt311, 2
  store i8 %.elt311.elt387, ptr %tile_map.repack310.repack386, align 1
  %tile_map.repack310.repack388 = getelementptr inbounds nuw i8, ptr %tile_map, i64 30
  %.elt311.elt389 = extractvalue [9 x i8] %.elt311, 3
  store i8 %.elt311.elt389, ptr %tile_map.repack310.repack388, align 1
  %tile_map.repack310.repack390 = getelementptr inbounds nuw i8, ptr %tile_map, i64 31
  %.elt311.elt391 = extractvalue [9 x i8] %.elt311, 4
  store i8 %.elt311.elt391, ptr %tile_map.repack310.repack390, align 1
  %tile_map.repack310.repack392 = getelementptr inbounds nuw i8, ptr %tile_map, i64 32
  %.elt311.elt393 = extractvalue [9 x i8] %.elt311, 5
  store i8 %.elt311.elt393, ptr %tile_map.repack310.repack392, align 1
  %tile_map.repack310.repack394 = getelementptr inbounds nuw i8, ptr %tile_map, i64 33
  %.elt311.elt395 = extractvalue [9 x i8] %.elt311, 6
  store i8 %.elt311.elt395, ptr %tile_map.repack310.repack394, align 1
  %tile_map.repack310.repack396 = getelementptr inbounds nuw i8, ptr %tile_map, i64 34
  %.elt311.elt397 = extractvalue [9 x i8] %.elt311, 7
  store i8 %.elt311.elt397, ptr %tile_map.repack310.repack396, align 1
  %tile_map.repack310.repack398 = getelementptr inbounds nuw i8, ptr %tile_map, i64 35
  %.elt311.elt399 = extractvalue [9 x i8] %.elt311, 8
  store i8 %.elt311.elt399, ptr %tile_map.repack310.repack398, align 1
  %tile_map.repack312 = getelementptr inbounds nuw i8, ptr %tile_map, i64 36
  %.elt313 = extractvalue [16 x [9 x i8]] %0, 4
  %.elt313.elt = extractvalue [9 x i8] %.elt313, 0
  store i8 %.elt313.elt, ptr %tile_map.repack312, align 1
  %tile_map.repack312.repack400 = getelementptr inbounds nuw i8, ptr %tile_map, i64 37
  %.elt313.elt401 = extractvalue [9 x i8] %.elt313, 1
  store i8 %.elt313.elt401, ptr %tile_map.repack312.repack400, align 1
  %tile_map.repack312.repack402 = getelementptr inbounds nuw i8, ptr %tile_map, i64 38
  %.elt313.elt403 = extractvalue [9 x i8] %.elt313, 2
  store i8 %.elt313.elt403, ptr %tile_map.repack312.repack402, align 1
  %tile_map.repack312.repack404 = getelementptr inbounds nuw i8, ptr %tile_map, i64 39
  %.elt313.elt405 = extractvalue [9 x i8] %.elt313, 3
  store i8 %.elt313.elt405, ptr %tile_map.repack312.repack404, align 1
  %tile_map.repack312.repack406 = getelementptr inbounds nuw i8, ptr %tile_map, i64 40
  %.elt313.elt407 = extractvalue [9 x i8] %.elt313, 4
  store i8 %.elt313.elt407, ptr %tile_map.repack312.repack406, align 1
  %tile_map.repack312.repack408 = getelementptr inbounds nuw i8, ptr %tile_map, i64 41
  %.elt313.elt409 = extractvalue [9 x i8] %.elt313, 5
  store i8 %.elt313.elt409, ptr %tile_map.repack312.repack408, align 1
  %tile_map.repack312.repack410 = getelementptr inbounds nuw i8, ptr %tile_map, i64 42
  %.elt313.elt411 = extractvalue [9 x i8] %.elt313, 6
  store i8 %.elt313.elt411, ptr %tile_map.repack312.repack410, align 1
  %tile_map.repack312.repack412 = getelementptr inbounds nuw i8, ptr %tile_map, i64 43
  %.elt313.elt413 = extractvalue [9 x i8] %.elt313, 7
  store i8 %.elt313.elt413, ptr %tile_map.repack312.repack412, align 1
  %tile_map.repack312.repack414 = getelementptr inbounds nuw i8, ptr %tile_map, i64 44
  %.elt313.elt415 = extractvalue [9 x i8] %.elt313, 8
  store i8 %.elt313.elt415, ptr %tile_map.repack312.repack414, align 1
  %tile_map.repack314 = getelementptr inbounds nuw i8, ptr %tile_map, i64 45
  %.elt315 = extractvalue [16 x [9 x i8]] %0, 5
  %.elt315.elt = extractvalue [9 x i8] %.elt315, 0
  store i8 %.elt315.elt, ptr %tile_map.repack314, align 1
  %tile_map.repack314.repack416 = getelementptr inbounds nuw i8, ptr %tile_map, i64 46
  %.elt315.elt417 = extractvalue [9 x i8] %.elt315, 1
  store i8 %.elt315.elt417, ptr %tile_map.repack314.repack416, align 1
  %tile_map.repack314.repack418 = getelementptr inbounds nuw i8, ptr %tile_map, i64 47
  %.elt315.elt419 = extractvalue [9 x i8] %.elt315, 2
  store i8 %.elt315.elt419, ptr %tile_map.repack314.repack418, align 1
  %tile_map.repack314.repack420 = getelementptr inbounds nuw i8, ptr %tile_map, i64 48
  %.elt315.elt421 = extractvalue [9 x i8] %.elt315, 3
  store i8 %.elt315.elt421, ptr %tile_map.repack314.repack420, align 1
  %tile_map.repack314.repack422 = getelementptr inbounds nuw i8, ptr %tile_map, i64 49
  %.elt315.elt423 = extractvalue [9 x i8] %.elt315, 4
  store i8 %.elt315.elt423, ptr %tile_map.repack314.repack422, align 1
  %tile_map.repack314.repack424 = getelementptr inbounds nuw i8, ptr %tile_map, i64 50
  %.elt315.elt425 = extractvalue [9 x i8] %.elt315, 5
  store i8 %.elt315.elt425, ptr %tile_map.repack314.repack424, align 1
  %tile_map.repack314.repack426 = getelementptr inbounds nuw i8, ptr %tile_map, i64 51
  %.elt315.elt427 = extractvalue [9 x i8] %.elt315, 6
  store i8 %.elt315.elt427, ptr %tile_map.repack314.repack426, align 1
  %tile_map.repack314.repack428 = getelementptr inbounds nuw i8, ptr %tile_map, i64 52
  %.elt315.elt429 = extractvalue [9 x i8] %.elt315, 7
  store i8 %.elt315.elt429, ptr %tile_map.repack314.repack428, align 1
  %tile_map.repack314.repack430 = getelementptr inbounds nuw i8, ptr %tile_map, i64 53
  %.elt315.elt431 = extractvalue [9 x i8] %.elt315, 8
  store i8 %.elt315.elt431, ptr %tile_map.repack314.repack430, align 1
  %tile_map.repack316 = getelementptr inbounds nuw i8, ptr %tile_map, i64 54
  %.elt317 = extractvalue [16 x [9 x i8]] %0, 6
  %.elt317.elt = extractvalue [9 x i8] %.elt317, 0
  store i8 %.elt317.elt, ptr %tile_map.repack316, align 1
  %tile_map.repack316.repack432 = getelementptr inbounds nuw i8, ptr %tile_map, i64 55
  %.elt317.elt433 = extractvalue [9 x i8] %.elt317, 1
  store i8 %.elt317.elt433, ptr %tile_map.repack316.repack432, align 1
  %tile_map.repack316.repack434 = getelementptr inbounds nuw i8, ptr %tile_map, i64 56
  %.elt317.elt435 = extractvalue [9 x i8] %.elt317, 2
  store i8 %.elt317.elt435, ptr %tile_map.repack316.repack434, align 1
  %tile_map.repack316.repack436 = getelementptr inbounds nuw i8, ptr %tile_map, i64 57
  %.elt317.elt437 = extractvalue [9 x i8] %.elt317, 3
  store i8 %.elt317.elt437, ptr %tile_map.repack316.repack436, align 1
  %tile_map.repack316.repack438 = getelementptr inbounds nuw i8, ptr %tile_map, i64 58
  %.elt317.elt439 = extractvalue [9 x i8] %.elt317, 4
  store i8 %.elt317.elt439, ptr %tile_map.repack316.repack438, align 1
  %tile_map.repack316.repack440 = getelementptr inbounds nuw i8, ptr %tile_map, i64 59
  %.elt317.elt441 = extractvalue [9 x i8] %.elt317, 5
  store i8 %.elt317.elt441, ptr %tile_map.repack316.repack440, align 1
  %tile_map.repack316.repack442 = getelementptr inbounds nuw i8, ptr %tile_map, i64 60
  %.elt317.elt443 = extractvalue [9 x i8] %.elt317, 6
  store i8 %.elt317.elt443, ptr %tile_map.repack316.repack442, align 1
  %tile_map.repack316.repack444 = getelementptr inbounds nuw i8, ptr %tile_map, i64 61
  %.elt317.elt445 = extractvalue [9 x i8] %.elt317, 7
  store i8 %.elt317.elt445, ptr %tile_map.repack316.repack444, align 1
  %tile_map.repack316.repack446 = getelementptr inbounds nuw i8, ptr %tile_map, i64 62
  %.elt317.elt447 = extractvalue [9 x i8] %.elt317, 8
  store i8 %.elt317.elt447, ptr %tile_map.repack316.repack446, align 1
  %tile_map.repack318 = getelementptr inbounds nuw i8, ptr %tile_map, i64 63
  %.elt319 = extractvalue [16 x [9 x i8]] %0, 7
  %.elt319.elt = extractvalue [9 x i8] %.elt319, 0
  store i8 %.elt319.elt, ptr %tile_map.repack318, align 1
  %tile_map.repack318.repack448 = getelementptr inbounds nuw i8, ptr %tile_map, i64 64
  %.elt319.elt449 = extractvalue [9 x i8] %.elt319, 1
  store i8 %.elt319.elt449, ptr %tile_map.repack318.repack448, align 1
  %tile_map.repack318.repack450 = getelementptr inbounds nuw i8, ptr %tile_map, i64 65
  %.elt319.elt451 = extractvalue [9 x i8] %.elt319, 2
  store i8 %.elt319.elt451, ptr %tile_map.repack318.repack450, align 1
  %tile_map.repack318.repack452 = getelementptr inbounds nuw i8, ptr %tile_map, i64 66
  %.elt319.elt453 = extractvalue [9 x i8] %.elt319, 3
  store i8 %.elt319.elt453, ptr %tile_map.repack318.repack452, align 1
  %tile_map.repack318.repack454 = getelementptr inbounds nuw i8, ptr %tile_map, i64 67
  %.elt319.elt455 = extractvalue [9 x i8] %.elt319, 4
  store i8 %.elt319.elt455, ptr %tile_map.repack318.repack454, align 1
  %tile_map.repack318.repack456 = getelementptr inbounds nuw i8, ptr %tile_map, i64 68
  %.elt319.elt457 = extractvalue [9 x i8] %.elt319, 5
  store i8 %.elt319.elt457, ptr %tile_map.repack318.repack456, align 1
  %tile_map.repack318.repack458 = getelementptr inbounds nuw i8, ptr %tile_map, i64 69
  %.elt319.elt459 = extractvalue [9 x i8] %.elt319, 6
  store i8 %.elt319.elt459, ptr %tile_map.repack318.repack458, align 1
  %tile_map.repack318.repack460 = getelementptr inbounds nuw i8, ptr %tile_map, i64 70
  %.elt319.elt461 = extractvalue [9 x i8] %.elt319, 7
  store i8 %.elt319.elt461, ptr %tile_map.repack318.repack460, align 1
  %tile_map.repack318.repack462 = getelementptr inbounds nuw i8, ptr %tile_map, i64 71
  %.elt319.elt463 = extractvalue [9 x i8] %.elt319, 8
  store i8 %.elt319.elt463, ptr %tile_map.repack318.repack462, align 1
  %tile_map.repack320 = getelementptr inbounds nuw i8, ptr %tile_map, i64 72
  %.elt321 = extractvalue [16 x [9 x i8]] %0, 8
  %.elt321.elt = extractvalue [9 x i8] %.elt321, 0
  store i8 %.elt321.elt, ptr %tile_map.repack320, align 1
  %tile_map.repack320.repack464 = getelementptr inbounds nuw i8, ptr %tile_map, i64 73
  %.elt321.elt465 = extractvalue [9 x i8] %.elt321, 1
  store i8 %.elt321.elt465, ptr %tile_map.repack320.repack464, align 1
  %tile_map.repack320.repack466 = getelementptr inbounds nuw i8, ptr %tile_map, i64 74
  %.elt321.elt467 = extractvalue [9 x i8] %.elt321, 2
  store i8 %.elt321.elt467, ptr %tile_map.repack320.repack466, align 1
  %tile_map.repack320.repack468 = getelementptr inbounds nuw i8, ptr %tile_map, i64 75
  %.elt321.elt469 = extractvalue [9 x i8] %.elt321, 3
  store i8 %.elt321.elt469, ptr %tile_map.repack320.repack468, align 1
  %tile_map.repack320.repack470 = getelementptr inbounds nuw i8, ptr %tile_map, i64 76
  %.elt321.elt471 = extractvalue [9 x i8] %.elt321, 4
  store i8 %.elt321.elt471, ptr %tile_map.repack320.repack470, align 1
  %tile_map.repack320.repack472 = getelementptr inbounds nuw i8, ptr %tile_map, i64 77
  %.elt321.elt473 = extractvalue [9 x i8] %.elt321, 5
  store i8 %.elt321.elt473, ptr %tile_map.repack320.repack472, align 1
  %tile_map.repack320.repack474 = getelementptr inbounds nuw i8, ptr %tile_map, i64 78
  %.elt321.elt475 = extractvalue [9 x i8] %.elt321, 6
  store i8 %.elt321.elt475, ptr %tile_map.repack320.repack474, align 1
  %tile_map.repack320.repack476 = getelementptr inbounds nuw i8, ptr %tile_map, i64 79
  %.elt321.elt477 = extractvalue [9 x i8] %.elt321, 7
  store i8 %.elt321.elt477, ptr %tile_map.repack320.repack476, align 1
  %tile_map.repack320.repack478 = getelementptr inbounds nuw i8, ptr %tile_map, i64 80
  %.elt321.elt479 = extractvalue [9 x i8] %.elt321, 8
  store i8 %.elt321.elt479, ptr %tile_map.repack320.repack478, align 1
  %tile_map.repack322 = getelementptr inbounds nuw i8, ptr %tile_map, i64 81
  %.elt323 = extractvalue [16 x [9 x i8]] %0, 9
  %.elt323.elt = extractvalue [9 x i8] %.elt323, 0
  store i8 %.elt323.elt, ptr %tile_map.repack322, align 1
  %tile_map.repack322.repack480 = getelementptr inbounds nuw i8, ptr %tile_map, i64 82
  %.elt323.elt481 = extractvalue [9 x i8] %.elt323, 1
  store i8 %.elt323.elt481, ptr %tile_map.repack322.repack480, align 1
  %tile_map.repack322.repack482 = getelementptr inbounds nuw i8, ptr %tile_map, i64 83
  %.elt323.elt483 = extractvalue [9 x i8] %.elt323, 2
  store i8 %.elt323.elt483, ptr %tile_map.repack322.repack482, align 1
  %tile_map.repack322.repack484 = getelementptr inbounds nuw i8, ptr %tile_map, i64 84
  %.elt323.elt485 = extractvalue [9 x i8] %.elt323, 3
  store i8 %.elt323.elt485, ptr %tile_map.repack322.repack484, align 1
  %tile_map.repack322.repack486 = getelementptr inbounds nuw i8, ptr %tile_map, i64 85
  %.elt323.elt487 = extractvalue [9 x i8] %.elt323, 4
  store i8 %.elt323.elt487, ptr %tile_map.repack322.repack486, align 1
  %tile_map.repack322.repack488 = getelementptr inbounds nuw i8, ptr %tile_map, i64 86
  %.elt323.elt489 = extractvalue [9 x i8] %.elt323, 5
  store i8 %.elt323.elt489, ptr %tile_map.repack322.repack488, align 1
  %tile_map.repack322.repack490 = getelementptr inbounds nuw i8, ptr %tile_map, i64 87
  %.elt323.elt491 = extractvalue [9 x i8] %.elt323, 6
  store i8 %.elt323.elt491, ptr %tile_map.repack322.repack490, align 1
  %tile_map.repack322.repack492 = getelementptr inbounds nuw i8, ptr %tile_map, i64 88
  %.elt323.elt493 = extractvalue [9 x i8] %.elt323, 7
  store i8 %.elt323.elt493, ptr %tile_map.repack322.repack492, align 1
  %tile_map.repack322.repack494 = getelementptr inbounds nuw i8, ptr %tile_map, i64 89
  %.elt323.elt495 = extractvalue [9 x i8] %.elt323, 8
  store i8 %.elt323.elt495, ptr %tile_map.repack322.repack494, align 1
  %tile_map.repack324 = getelementptr inbounds nuw i8, ptr %tile_map, i64 90
  %.elt325 = extractvalue [16 x [9 x i8]] %0, 10
  %.elt325.elt = extractvalue [9 x i8] %.elt325, 0
  store i8 %.elt325.elt, ptr %tile_map.repack324, align 1
  %tile_map.repack324.repack496 = getelementptr inbounds nuw i8, ptr %tile_map, i64 91
  %.elt325.elt497 = extractvalue [9 x i8] %.elt325, 1
  store i8 %.elt325.elt497, ptr %tile_map.repack324.repack496, align 1
  %tile_map.repack324.repack498 = getelementptr inbounds nuw i8, ptr %tile_map, i64 92
  %.elt325.elt499 = extractvalue [9 x i8] %.elt325, 2
  store i8 %.elt325.elt499, ptr %tile_map.repack324.repack498, align 1
  %tile_map.repack324.repack500 = getelementptr inbounds nuw i8, ptr %tile_map, i64 93
  %.elt325.elt501 = extractvalue [9 x i8] %.elt325, 3
  store i8 %.elt325.elt501, ptr %tile_map.repack324.repack500, align 1
  %tile_map.repack324.repack502 = getelementptr inbounds nuw i8, ptr %tile_map, i64 94
  %.elt325.elt503 = extractvalue [9 x i8] %.elt325, 4
  store i8 %.elt325.elt503, ptr %tile_map.repack324.repack502, align 1
  %tile_map.repack324.repack504 = getelementptr inbounds nuw i8, ptr %tile_map, i64 95
  %.elt325.elt505 = extractvalue [9 x i8] %.elt325, 5
  store i8 %.elt325.elt505, ptr %tile_map.repack324.repack504, align 1
  %tile_map.repack324.repack506 = getelementptr inbounds nuw i8, ptr %tile_map, i64 96
  %.elt325.elt507 = extractvalue [9 x i8] %.elt325, 6
  store i8 %.elt325.elt507, ptr %tile_map.repack324.repack506, align 1
  %tile_map.repack324.repack508 = getelementptr inbounds nuw i8, ptr %tile_map, i64 97
  %.elt325.elt509 = extractvalue [9 x i8] %.elt325, 7
  store i8 %.elt325.elt509, ptr %tile_map.repack324.repack508, align 1
  %tile_map.repack324.repack510 = getelementptr inbounds nuw i8, ptr %tile_map, i64 98
  %.elt325.elt511 = extractvalue [9 x i8] %.elt325, 8
  store i8 %.elt325.elt511, ptr %tile_map.repack324.repack510, align 1
  %tile_map.repack326 = getelementptr inbounds nuw i8, ptr %tile_map, i64 99
  %.elt327 = extractvalue [16 x [9 x i8]] %0, 11
  %.elt327.elt = extractvalue [9 x i8] %.elt327, 0
  store i8 %.elt327.elt, ptr %tile_map.repack326, align 1
  %tile_map.repack326.repack512 = getelementptr inbounds nuw i8, ptr %tile_map, i64 100
  %.elt327.elt513 = extractvalue [9 x i8] %.elt327, 1
  store i8 %.elt327.elt513, ptr %tile_map.repack326.repack512, align 1
  %tile_map.repack326.repack514 = getelementptr inbounds nuw i8, ptr %tile_map, i64 101
  %.elt327.elt515 = extractvalue [9 x i8] %.elt327, 2
  store i8 %.elt327.elt515, ptr %tile_map.repack326.repack514, align 1
  %tile_map.repack326.repack516 = getelementptr inbounds nuw i8, ptr %tile_map, i64 102
  %.elt327.elt517 = extractvalue [9 x i8] %.elt327, 3
  store i8 %.elt327.elt517, ptr %tile_map.repack326.repack516, align 1
  %tile_map.repack326.repack518 = getelementptr inbounds nuw i8, ptr %tile_map, i64 103
  %.elt327.elt519 = extractvalue [9 x i8] %.elt327, 4
  store i8 %.elt327.elt519, ptr %tile_map.repack326.repack518, align 1
  %tile_map.repack326.repack520 = getelementptr inbounds nuw i8, ptr %tile_map, i64 104
  %.elt327.elt521 = extractvalue [9 x i8] %.elt327, 5
  store i8 %.elt327.elt521, ptr %tile_map.repack326.repack520, align 1
  %tile_map.repack326.repack522 = getelementptr inbounds nuw i8, ptr %tile_map, i64 105
  %.elt327.elt523 = extractvalue [9 x i8] %.elt327, 6
  store i8 %.elt327.elt523, ptr %tile_map.repack326.repack522, align 1
  %tile_map.repack326.repack524 = getelementptr inbounds nuw i8, ptr %tile_map, i64 106
  %.elt327.elt525 = extractvalue [9 x i8] %.elt327, 7
  store i8 %.elt327.elt525, ptr %tile_map.repack326.repack524, align 1
  %tile_map.repack326.repack526 = getelementptr inbounds nuw i8, ptr %tile_map, i64 107
  %.elt327.elt527 = extractvalue [9 x i8] %.elt327, 8
  store i8 %.elt327.elt527, ptr %tile_map.repack326.repack526, align 1
  %tile_map.repack328 = getelementptr inbounds nuw i8, ptr %tile_map, i64 108
  %.elt329 = extractvalue [16 x [9 x i8]] %0, 12
  %.elt329.elt = extractvalue [9 x i8] %.elt329, 0
  store i8 %.elt329.elt, ptr %tile_map.repack328, align 1
  %tile_map.repack328.repack528 = getelementptr inbounds nuw i8, ptr %tile_map, i64 109
  %.elt329.elt529 = extractvalue [9 x i8] %.elt329, 1
  store i8 %.elt329.elt529, ptr %tile_map.repack328.repack528, align 1
  %tile_map.repack328.repack530 = getelementptr inbounds nuw i8, ptr %tile_map, i64 110
  %.elt329.elt531 = extractvalue [9 x i8] %.elt329, 2
  store i8 %.elt329.elt531, ptr %tile_map.repack328.repack530, align 1
  %tile_map.repack328.repack532 = getelementptr inbounds nuw i8, ptr %tile_map, i64 111
  %.elt329.elt533 = extractvalue [9 x i8] %.elt329, 3
  store i8 %.elt329.elt533, ptr %tile_map.repack328.repack532, align 1
  %tile_map.repack328.repack534 = getelementptr inbounds nuw i8, ptr %tile_map, i64 112
  %.elt329.elt535 = extractvalue [9 x i8] %.elt329, 4
  store i8 %.elt329.elt535, ptr %tile_map.repack328.repack534, align 1
  %tile_map.repack328.repack536 = getelementptr inbounds nuw i8, ptr %tile_map, i64 113
  %.elt329.elt537 = extractvalue [9 x i8] %.elt329, 5
  store i8 %.elt329.elt537, ptr %tile_map.repack328.repack536, align 1
  %tile_map.repack328.repack538 = getelementptr inbounds nuw i8, ptr %tile_map, i64 114
  %.elt329.elt539 = extractvalue [9 x i8] %.elt329, 6
  store i8 %.elt329.elt539, ptr %tile_map.repack328.repack538, align 1
  %tile_map.repack328.repack540 = getelementptr inbounds nuw i8, ptr %tile_map, i64 115
  %.elt329.elt541 = extractvalue [9 x i8] %.elt329, 7
  store i8 %.elt329.elt541, ptr %tile_map.repack328.repack540, align 1
  %tile_map.repack328.repack542 = getelementptr inbounds nuw i8, ptr %tile_map, i64 116
  %.elt329.elt543 = extractvalue [9 x i8] %.elt329, 8
  store i8 %.elt329.elt543, ptr %tile_map.repack328.repack542, align 1
  %tile_map.repack330 = getelementptr inbounds nuw i8, ptr %tile_map, i64 117
  %.elt331 = extractvalue [16 x [9 x i8]] %0, 13
  %.elt331.elt = extractvalue [9 x i8] %.elt331, 0
  store i8 %.elt331.elt, ptr %tile_map.repack330, align 1
  %tile_map.repack330.repack544 = getelementptr inbounds nuw i8, ptr %tile_map, i64 118
  %.elt331.elt545 = extractvalue [9 x i8] %.elt331, 1
  store i8 %.elt331.elt545, ptr %tile_map.repack330.repack544, align 1
  %tile_map.repack330.repack546 = getelementptr inbounds nuw i8, ptr %tile_map, i64 119
  %.elt331.elt547 = extractvalue [9 x i8] %.elt331, 2
  store i8 %.elt331.elt547, ptr %tile_map.repack330.repack546, align 1
  %tile_map.repack330.repack548 = getelementptr inbounds nuw i8, ptr %tile_map, i64 120
  %.elt331.elt549 = extractvalue [9 x i8] %.elt331, 3
  store i8 %.elt331.elt549, ptr %tile_map.repack330.repack548, align 1
  %tile_map.repack330.repack550 = getelementptr inbounds nuw i8, ptr %tile_map, i64 121
  %.elt331.elt551 = extractvalue [9 x i8] %.elt331, 4
  store i8 %.elt331.elt551, ptr %tile_map.repack330.repack550, align 1
  %tile_map.repack330.repack552 = getelementptr inbounds nuw i8, ptr %tile_map, i64 122
  %.elt331.elt553 = extractvalue [9 x i8] %.elt331, 5
  store i8 %.elt331.elt553, ptr %tile_map.repack330.repack552, align 1
  %tile_map.repack330.repack554 = getelementptr inbounds nuw i8, ptr %tile_map, i64 123
  %.elt331.elt555 = extractvalue [9 x i8] %.elt331, 6
  store i8 %.elt331.elt555, ptr %tile_map.repack330.repack554, align 1
  %tile_map.repack330.repack556 = getelementptr inbounds nuw i8, ptr %tile_map, i64 124
  %.elt331.elt557 = extractvalue [9 x i8] %.elt331, 7
  store i8 %.elt331.elt557, ptr %tile_map.repack330.repack556, align 1
  %tile_map.repack330.repack558 = getelementptr inbounds nuw i8, ptr %tile_map, i64 125
  %.elt331.elt559 = extractvalue [9 x i8] %.elt331, 8
  store i8 %.elt331.elt559, ptr %tile_map.repack330.repack558, align 1
  %tile_map.repack332 = getelementptr inbounds nuw i8, ptr %tile_map, i64 126
  %.elt333 = extractvalue [16 x [9 x i8]] %0, 14
  %.elt333.elt = extractvalue [9 x i8] %.elt333, 0
  store i8 %.elt333.elt, ptr %tile_map.repack332, align 1
  %tile_map.repack332.repack560 = getelementptr inbounds nuw i8, ptr %tile_map, i64 127
  %.elt333.elt561 = extractvalue [9 x i8] %.elt333, 1
  store i8 %.elt333.elt561, ptr %tile_map.repack332.repack560, align 1
  %tile_map.repack332.repack562 = getelementptr inbounds nuw i8, ptr %tile_map, i64 128
  %.elt333.elt563 = extractvalue [9 x i8] %.elt333, 2
  store i8 %.elt333.elt563, ptr %tile_map.repack332.repack562, align 1
  %tile_map.repack332.repack564 = getelementptr inbounds nuw i8, ptr %tile_map, i64 129
  %.elt333.elt565 = extractvalue [9 x i8] %.elt333, 3
  store i8 %.elt333.elt565, ptr %tile_map.repack332.repack564, align 1
  %tile_map.repack332.repack566 = getelementptr inbounds nuw i8, ptr %tile_map, i64 130
  %.elt333.elt567 = extractvalue [9 x i8] %.elt333, 4
  store i8 %.elt333.elt567, ptr %tile_map.repack332.repack566, align 1
  %tile_map.repack332.repack568 = getelementptr inbounds nuw i8, ptr %tile_map, i64 131
  %.elt333.elt569 = extractvalue [9 x i8] %.elt333, 5
  store i8 %.elt333.elt569, ptr %tile_map.repack332.repack568, align 1
  %tile_map.repack332.repack570 = getelementptr inbounds nuw i8, ptr %tile_map, i64 132
  %.elt333.elt571 = extractvalue [9 x i8] %.elt333, 6
  store i8 %.elt333.elt571, ptr %tile_map.repack332.repack570, align 1
  %tile_map.repack332.repack572 = getelementptr inbounds nuw i8, ptr %tile_map, i64 133
  %.elt333.elt573 = extractvalue [9 x i8] %.elt333, 7
  store i8 %.elt333.elt573, ptr %tile_map.repack332.repack572, align 1
  %tile_map.repack332.repack574 = getelementptr inbounds nuw i8, ptr %tile_map, i64 134
  %.elt333.elt575 = extractvalue [9 x i8] %.elt333, 8
  store i8 %.elt333.elt575, ptr %tile_map.repack332.repack574, align 1
  %tile_map.repack334 = getelementptr inbounds nuw i8, ptr %tile_map, i64 135
  %.elt335 = extractvalue [16 x [9 x i8]] %0, 15
  %.elt335.elt = extractvalue [9 x i8] %.elt335, 0
  store i8 %.elt335.elt, ptr %tile_map.repack334, align 1
  %tile_map.repack334.repack576 = getelementptr inbounds nuw i8, ptr %tile_map, i64 136
  %.elt335.elt577 = extractvalue [9 x i8] %.elt335, 1
  store i8 %.elt335.elt577, ptr %tile_map.repack334.repack576, align 1
  %tile_map.repack334.repack578 = getelementptr inbounds nuw i8, ptr %tile_map, i64 137
  %.elt335.elt579 = extractvalue [9 x i8] %.elt335, 2
  store i8 %.elt335.elt579, ptr %tile_map.repack334.repack578, align 1
  %tile_map.repack334.repack580 = getelementptr inbounds nuw i8, ptr %tile_map, i64 138
  %.elt335.elt581 = extractvalue [9 x i8] %.elt335, 3
  store i8 %.elt335.elt581, ptr %tile_map.repack334.repack580, align 1
  %tile_map.repack334.repack582 = getelementptr inbounds nuw i8, ptr %tile_map, i64 139
  %.elt335.elt583 = extractvalue [9 x i8] %.elt335, 4
  store i8 %.elt335.elt583, ptr %tile_map.repack334.repack582, align 1
  %tile_map.repack334.repack584 = getelementptr inbounds nuw i8, ptr %tile_map, i64 140
  %.elt335.elt585 = extractvalue [9 x i8] %.elt335, 5
  store i8 %.elt335.elt585, ptr %tile_map.repack334.repack584, align 1
  %tile_map.repack334.repack586 = getelementptr inbounds nuw i8, ptr %tile_map, i64 141
  %.elt335.elt587 = extractvalue [9 x i8] %.elt335, 6
  store i8 %.elt335.elt587, ptr %tile_map.repack334.repack586, align 1
  %tile_map.repack334.repack588 = getelementptr inbounds nuw i8, ptr %tile_map, i64 142
  %.elt335.elt589 = extractvalue [9 x i8] %.elt335, 7
  store i8 %.elt335.elt589, ptr %tile_map.repack334.repack588, align 1
  %tile_map.repack334.repack590 = getelementptr inbounds nuw i8, ptr %tile_map, i64 143
  %.elt335.elt591 = extractvalue [9 x i8] %.elt335, 8
  store i8 %.elt335.elt591, ptr %tile_map.repack334.repack590, align 1
  store i64 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack592 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  store i64 16, ptr %tmp_iter3.repack592, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp6 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter3)
  store i64 %fun_call_tmp6, ptr %x, align 4
  %tmp_index.i254 = getelementptr [9 x i8], ptr %tile_map, i64 %fun_call_tmp6, i64 8
  %tmp15 = load i64, ptr %x, align 4
  %tmp.i197 = and i64 %tmp15, 1
  store i64 %tmp.i197, ptr %tmp5, align 4
  store i64 0, ptr %tmp6, align 4
  %tmp2.i216 = icmp eq i64 %tmp.i197, 0
  %spec.select = select i1 %tmp2.i216, i8 4, i8 7
  store i8 %spec.select, ptr %tmp_index.i254, align 1
  %tmp19 = load i64, ptr %tmp6, align 4
  %fun_call_tmp20 = call {} @"u64:drop"(i64 %tmp19)
  %tmp21 = load i64, ptr %tmp5, align 4
  %fun_call_tmp22 = call {} @"u64:drop"(i64 %tmp21)
  %tmp23 = load i64, ptr %x, align 4
  %fun_call_tmp24 = call {} @"u64:drop"(i64 %tmp23)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp5 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp5, label %then, label %else

else:                                             ; preds = %cond
  %fun_call_tmp26 = call %"Vec<T = Particle>" @"Vec<T = Particle>:new"()
  %fun_call_tmp26.elt = extractvalue %"Vec<T = Particle>" %fun_call_tmp26, 0
  store ptr %fun_call_tmp26.elt, ptr %particles, align 8
  %particles.repack593 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %fun_call_tmp26.elt594 = extractvalue %"Vec<T = Particle>" %fun_call_tmp26, 1
  store i64 %fun_call_tmp26.elt594, ptr %particles.repack593, align 8
  %particles.repack595 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %fun_call_tmp26.elt596 = extractvalue %"Vec<T = Particle>" %fun_call_tmp26, 2
  store i64 %fun_call_tmp26.elt596, ptr %particles.repack595, align 8
  store i64 0, ptr %tmp_iter5, align 4
  %tmp_iter5.repack597 = getelementptr inbounds nuw i8, ptr %tmp_iter5, i64 8
  store i64 40, ptr %tmp_iter5.repack597, align 4
  br label %cond30

then29:                                           ; preds = %cond30
  %fun_call_tmp33 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter5)
  store i64 %fun_call_tmp33, ptr %i, align 4
  %tmp34.unpack = load i64, ptr %screen_size, align 4
  %1 = insertvalue %"Vec_2<T = u64>" poison, i64 %tmp34.unpack, 0
  %tmp34.elt976 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp34.unpack977 = load i64, ptr %tmp34.elt976, align 4
  %tmp34978 = insertvalue %"Vec_2<T = u64>" %1, i64 %tmp34.unpack977, 1
  %fun_call_tmp35 = call %Particle @"Particle:spawn"(%"Vec_2<T = u64>" %tmp34978)
  %fun_call_tmp36 = call {} @"Vec<T = Particle>:push"(ptr nonnull %particles, %Particle %fun_call_tmp35)
  %tmp37 = load i64, ptr %i, align 4
  %fun_call_tmp38 = call {} @"u64:drop"(i64 %tmp37)
  br label %cond30

cond30:                                           ; preds = %then29, %else
  %fun_call_tmp32 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter5)
  br i1 %fun_call_tmp32, label %then29, label %else31

else31:                                           ; preds = %cond30
  %fun_call_tmp40 = call %Player @"Particle:new"()
  %fun_call_tmp40.elt = extractvalue %Player %fun_call_tmp40, 0
  %fun_call_tmp40.elt.elt = extractvalue %"Vec_2<T = f64>" %fun_call_tmp40.elt, 0
  store double %fun_call_tmp40.elt.elt, ptr %player, align 8
  %player.repack600 = getelementptr inbounds nuw i8, ptr %player, i64 8
  %fun_call_tmp40.elt.elt601 = extractvalue %"Vec_2<T = f64>" %fun_call_tmp40.elt, 1
  store double %fun_call_tmp40.elt.elt601, ptr %player.repack600, align 8
  %player.repack598 = getelementptr inbounds nuw i8, ptr %player, i64 16
  %fun_call_tmp40.elt599 = extractvalue %Player %fun_call_tmp40, 1
  %fun_call_tmp40.elt599.elt = extractvalue %"Vec_2<T = f64>" %fun_call_tmp40.elt599, 0
  store double %fun_call_tmp40.elt599.elt, ptr %player.repack598, align 8
  %player.repack598.repack602 = getelementptr inbounds nuw i8, ptr %player, i64 24
  %fun_call_tmp40.elt599.elt603 = extractvalue %"Vec_2<T = f64>" %fun_call_tmp40.elt599, 1
  store double %fun_call_tmp40.elt599.elt603, ptr %player.repack598.repack602, align 8
  br label %then42

then42:                                           ; preds = %else130, %else31
  %fun_call_tmp43 = call {} @"pixel:Game:update"(ptr nonnull %game)
  %game.key_presses = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp45 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %game.key_presses)
  %fun_call_tmp46 = call %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %fun_call_tmp45)
  %fun_call_tmp46.elt = extractvalue %"SliceIter<T = char>" %fun_call_tmp46, 0
  store ptr %fun_call_tmp46.elt, ptr %tmp_iter7, align 8
  %tmp_iter7.repack604 = getelementptr inbounds nuw i8, ptr %tmp_iter7, i64 8
  %fun_call_tmp46.elt605 = extractvalue %"SliceIter<T = char>" %fun_call_tmp46, 1
  store ptr %fun_call_tmp46.elt605, ptr %tmp_iter7.repack604, align 8
  br label %cond48

then47:                                           ; preds = %cond48
  %fun_call_tmp51 = call ptr @"Vec<T = char>:next"(ptr nonnull %tmp_iter7)
  store ptr %fun_call_tmp51, ptr %key_press, align 8
  %tmp56 = load ptr, ptr %key_press, align 8
  store i8 113, ptr %tmp9, align 1
  %tmp.i222 = load i8, ptr %tmp56, align 1
  %tmp2.i224 = icmp eq i8 %tmp.i222, 113
  br i1 %tmp2.i224, label %then53, label %else55

cond48:                                           ; preds = %else55, %then42
  %fun_call_tmp50 = call i1 @"Vec<T = char>:has_next"(ptr nonnull %tmp_iter7)
  br i1 %fun_call_tmp50, label %then47, label %else49

else49:                                           ; preds = %cond48
  %fun_call_tmp65 = call { ptr, i64 } @"Vec<T = Particle>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp66 = call %"SliceMutIter<T = Particle>" @"[Particle]:iter_mut"({ ptr, i64 } %fun_call_tmp65)
  %fun_call_tmp66.elt = extractvalue %"SliceMutIter<T = Particle>" %fun_call_tmp66, 0
  store ptr %fun_call_tmp66.elt, ptr %tmp_iter8, align 8
  %tmp_iter8.repack606 = getelementptr inbounds nuw i8, ptr %tmp_iter8, i64 8
  %fun_call_tmp66.elt607 = extractvalue %"SliceMutIter<T = Particle>" %fun_call_tmp66, 1
  store ptr %fun_call_tmp66.elt607, ptr %tmp_iter8.repack606, align 8
  br label %cond68

then53:                                           ; preds = %then47
  %game.key_presses156 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp157 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %game.key_presses156)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp157, 1
  store i64 %len.i, ptr %tmp7159, align 4
  %fun_call_tmp160 = call %"Vec<T = char>" @"u64:to_string"(ptr nonnull %tmp7159)
  %fun_call_tmp160.elt = extractvalue %"Vec<T = char>" %fun_call_tmp160, 0
  store ptr %fun_call_tmp160.elt, ptr %tmp8, align 8
  %tmp8.repack617 = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  %fun_call_tmp160.elt618 = extractvalue %"Vec<T = char>" %fun_call_tmp160, 1
  store i64 %fun_call_tmp160.elt618, ptr %tmp8.repack617, align 8
  %tmp8.repack619 = getelementptr inbounds nuw i8, ptr %tmp8, i64 16
  %fun_call_tmp160.elt620 = extractvalue %"Vec<T = char>" %fun_call_tmp160, 2
  store i64 %fun_call_tmp160.elt620, ptr %tmp8.repack619, align 8
  %fun_call_tmp161 = call { ptr, i64 } @"Vec<T = char>:deref_mut"(ptr nonnull %tmp8)
  %fun_call_tmp162 = call {} @println({ ptr, i64 } %fun_call_tmp161)
  %tmp163.unpack = load ptr, ptr %tmp8, align 8
  %2 = insertvalue %"Vec<T = char>" poison, ptr %tmp163.unpack, 0
  %tmp163.elt621 = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  %tmp163.unpack622 = load i64, ptr %tmp163.elt621, align 8
  %3 = insertvalue %"Vec<T = char>" %2, i64 %tmp163.unpack622, 1
  %tmp163.elt623 = getelementptr inbounds nuw i8, ptr %tmp8, i64 16
  %tmp163.unpack624 = load i64, ptr %tmp163.elt623, align 8
  %tmp163625 = insertvalue %"Vec<T = char>" %3, i64 %tmp163.unpack624, 2
  %fun_call_tmp164 = call {} @"Vec<T = char>:drop.35"(%"Vec<T = char>" %tmp163625)
  %tmp165 = load i64, ptr %tmp7159, align 4
  %fun_call_tmp166 = call {} @"u64:drop"(i64 %tmp165)
  %tmp167.unpack.unpack = load double, ptr %player, align 8
  %tmp167.unpack.elt629 = getelementptr inbounds nuw i8, ptr %player, i64 8
  %tmp167.unpack.unpack630 = load double, ptr %tmp167.unpack.elt629, align 8
  %tmp167.elt626 = getelementptr inbounds nuw i8, ptr %player, i64 16
  %tmp167.unpack627.unpack = load double, ptr %tmp167.elt626, align 8
  %tmp167.unpack627.elt632 = getelementptr inbounds nuw i8, ptr %player, i64 24
  %tmp167.unpack627.unpack633 = load double, ptr %tmp167.unpack627.elt632, align 8
  %fun_call_tmp.i5.i = call {} @"f64:drop"(double %tmp167.unpack627.unpack633)
  %fun_call_tmp2.i7.i = call {} @"f64:drop"(double %tmp167.unpack627.unpack)
  %fun_call_tmp.i.i270 = call {} @"f64:drop"(double %tmp167.unpack.unpack630)
  %fun_call_tmp2.i.i = call {} @"f64:drop"(double %tmp167.unpack.unpack)
  %tmp169.unpack = load i64, ptr %tmp_iter5, align 4
  %tmp169.elt635 = getelementptr inbounds nuw i8, ptr %tmp_iter5, i64 8
  %tmp169.unpack636 = load i64, ptr %tmp169.elt635, align 4
  %fun_call_tmp.i243 = call {} @"u64:drop"(i64 %tmp169.unpack636)
  %fun_call_tmp2.i245 = call {} @"u64:drop"(i64 %tmp169.unpack)
  %tmp171.unpack = load ptr, ptr %particles, align 8
  %4 = insertvalue %"Vec<T = Particle>" poison, ptr %tmp171.unpack, 0
  %tmp171.elt638 = getelementptr inbounds nuw i8, ptr %particles, i64 8
  %tmp171.unpack639 = load i64, ptr %tmp171.elt638, align 8
  %5 = insertvalue %"Vec<T = Particle>" %4, i64 %tmp171.unpack639, 1
  %tmp171.elt640 = getelementptr inbounds nuw i8, ptr %particles, i64 16
  %tmp171.unpack641 = load i64, ptr %tmp171.elt640, align 8
  %tmp171642 = insertvalue %"Vec<T = Particle>" %5, i64 %tmp171.unpack641, 2
  %fun_call_tmp172 = call {} @"Vec<T = Particle>:drop.133"(%"Vec<T = Particle>" %tmp171642)
  %tmp173.unpack = load i64, ptr %tmp_iter3, align 4
  %tmp173.elt643 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %tmp173.unpack644 = load i64, ptr %tmp173.elt643, align 4
  %fun_call_tmp.i238 = call {} @"u64:drop"(i64 %tmp173.unpack644)
  %fun_call_tmp2.i240 = call {} @"u64:drop"(i64 %tmp173.unpack)
  %tmp175.unpack.unpack.unpack = load i8, ptr %tile_map, align 1
  %6 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack.unpack, 0
  %tmp175.unpack.unpack.elt678 = getelementptr inbounds nuw i8, ptr %tile_map, i64 1
  %tmp175.unpack.unpack.unpack679 = load i8, ptr %tmp175.unpack.unpack.elt678, align 1
  %7 = insertvalue [9 x i8] %6, i8 %tmp175.unpack.unpack.unpack679, 1
  %tmp175.unpack.unpack.elt680 = getelementptr inbounds nuw i8, ptr %tile_map, i64 2
  %tmp175.unpack.unpack.unpack681 = load i8, ptr %tmp175.unpack.unpack.elt680, align 1
  %8 = insertvalue [9 x i8] %7, i8 %tmp175.unpack.unpack.unpack681, 2
  %tmp175.unpack.unpack.elt682 = getelementptr inbounds nuw i8, ptr %tile_map, i64 3
  %tmp175.unpack.unpack.unpack683 = load i8, ptr %tmp175.unpack.unpack.elt682, align 1
  %9 = insertvalue [9 x i8] %8, i8 %tmp175.unpack.unpack.unpack683, 3
  %tmp175.unpack.unpack.elt684 = getelementptr inbounds nuw i8, ptr %tile_map, i64 4
  %tmp175.unpack.unpack.unpack685 = load i8, ptr %tmp175.unpack.unpack.elt684, align 1
  %10 = insertvalue [9 x i8] %9, i8 %tmp175.unpack.unpack.unpack685, 4
  %tmp175.unpack.unpack.elt686 = getelementptr inbounds nuw i8, ptr %tile_map, i64 5
  %tmp175.unpack.unpack.unpack687 = load i8, ptr %tmp175.unpack.unpack.elt686, align 1
  %11 = insertvalue [9 x i8] %10, i8 %tmp175.unpack.unpack.unpack687, 5
  %tmp175.unpack.unpack.elt688 = getelementptr inbounds nuw i8, ptr %tile_map, i64 6
  %tmp175.unpack.unpack.unpack689 = load i8, ptr %tmp175.unpack.unpack.elt688, align 1
  %12 = insertvalue [9 x i8] %11, i8 %tmp175.unpack.unpack.unpack689, 6
  %tmp175.unpack.unpack.elt690 = getelementptr inbounds nuw i8, ptr %tile_map, i64 7
  %tmp175.unpack.unpack.unpack691 = load i8, ptr %tmp175.unpack.unpack.elt690, align 1
  %13 = insertvalue [9 x i8] %12, i8 %tmp175.unpack.unpack.unpack691, 7
  %tmp175.unpack.unpack.elt692 = getelementptr inbounds nuw i8, ptr %tile_map, i64 8
  %tmp175.unpack.unpack.unpack693 = load i8, ptr %tmp175.unpack.unpack.elt692, align 1
  %tmp175.unpack.unpack694 = insertvalue [9 x i8] %13, i8 %tmp175.unpack.unpack.unpack693, 8
  %14 = insertvalue [16 x [9 x i8]] poison, [9 x i8] %tmp175.unpack.unpack694, 0
  %tmp175.unpack.elt647 = getelementptr inbounds nuw i8, ptr %tile_map, i64 9
  %tmp175.unpack.unpack648.unpack = load i8, ptr %tmp175.unpack.elt647, align 1
  %15 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack648.unpack, 0
  %tmp175.unpack.unpack648.elt695 = getelementptr inbounds nuw i8, ptr %tile_map, i64 10
  %tmp175.unpack.unpack648.unpack696 = load i8, ptr %tmp175.unpack.unpack648.elt695, align 1
  %16 = insertvalue [9 x i8] %15, i8 %tmp175.unpack.unpack648.unpack696, 1
  %tmp175.unpack.unpack648.elt697 = getelementptr inbounds nuw i8, ptr %tile_map, i64 11
  %tmp175.unpack.unpack648.unpack698 = load i8, ptr %tmp175.unpack.unpack648.elt697, align 1
  %17 = insertvalue [9 x i8] %16, i8 %tmp175.unpack.unpack648.unpack698, 2
  %tmp175.unpack.unpack648.elt699 = getelementptr inbounds nuw i8, ptr %tile_map, i64 12
  %tmp175.unpack.unpack648.unpack700 = load i8, ptr %tmp175.unpack.unpack648.elt699, align 1
  %18 = insertvalue [9 x i8] %17, i8 %tmp175.unpack.unpack648.unpack700, 3
  %tmp175.unpack.unpack648.elt701 = getelementptr inbounds nuw i8, ptr %tile_map, i64 13
  %tmp175.unpack.unpack648.unpack702 = load i8, ptr %tmp175.unpack.unpack648.elt701, align 1
  %19 = insertvalue [9 x i8] %18, i8 %tmp175.unpack.unpack648.unpack702, 4
  %tmp175.unpack.unpack648.elt703 = getelementptr inbounds nuw i8, ptr %tile_map, i64 14
  %tmp175.unpack.unpack648.unpack704 = load i8, ptr %tmp175.unpack.unpack648.elt703, align 1
  %20 = insertvalue [9 x i8] %19, i8 %tmp175.unpack.unpack648.unpack704, 5
  %tmp175.unpack.unpack648.elt705 = getelementptr inbounds nuw i8, ptr %tile_map, i64 15
  %tmp175.unpack.unpack648.unpack706 = load i8, ptr %tmp175.unpack.unpack648.elt705, align 1
  %21 = insertvalue [9 x i8] %20, i8 %tmp175.unpack.unpack648.unpack706, 6
  %tmp175.unpack.unpack648.elt707 = getelementptr inbounds nuw i8, ptr %tile_map, i64 16
  %tmp175.unpack.unpack648.unpack708 = load i8, ptr %tmp175.unpack.unpack648.elt707, align 1
  %22 = insertvalue [9 x i8] %21, i8 %tmp175.unpack.unpack648.unpack708, 7
  %tmp175.unpack.unpack648.elt709 = getelementptr inbounds nuw i8, ptr %tile_map, i64 17
  %tmp175.unpack.unpack648.unpack710 = load i8, ptr %tmp175.unpack.unpack648.elt709, align 1
  %tmp175.unpack.unpack648711 = insertvalue [9 x i8] %22, i8 %tmp175.unpack.unpack648.unpack710, 8
  %23 = insertvalue [16 x [9 x i8]] %14, [9 x i8] %tmp175.unpack.unpack648711, 1
  %tmp175.unpack.elt649 = getelementptr inbounds nuw i8, ptr %tile_map, i64 18
  %tmp175.unpack.unpack650.unpack = load i8, ptr %tmp175.unpack.elt649, align 1
  %24 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack650.unpack, 0
  %tmp175.unpack.unpack650.elt712 = getelementptr inbounds nuw i8, ptr %tile_map, i64 19
  %tmp175.unpack.unpack650.unpack713 = load i8, ptr %tmp175.unpack.unpack650.elt712, align 1
  %25 = insertvalue [9 x i8] %24, i8 %tmp175.unpack.unpack650.unpack713, 1
  %tmp175.unpack.unpack650.elt714 = getelementptr inbounds nuw i8, ptr %tile_map, i64 20
  %tmp175.unpack.unpack650.unpack715 = load i8, ptr %tmp175.unpack.unpack650.elt714, align 1
  %26 = insertvalue [9 x i8] %25, i8 %tmp175.unpack.unpack650.unpack715, 2
  %tmp175.unpack.unpack650.elt716 = getelementptr inbounds nuw i8, ptr %tile_map, i64 21
  %tmp175.unpack.unpack650.unpack717 = load i8, ptr %tmp175.unpack.unpack650.elt716, align 1
  %27 = insertvalue [9 x i8] %26, i8 %tmp175.unpack.unpack650.unpack717, 3
  %tmp175.unpack.unpack650.elt718 = getelementptr inbounds nuw i8, ptr %tile_map, i64 22
  %tmp175.unpack.unpack650.unpack719 = load i8, ptr %tmp175.unpack.unpack650.elt718, align 1
  %28 = insertvalue [9 x i8] %27, i8 %tmp175.unpack.unpack650.unpack719, 4
  %tmp175.unpack.unpack650.elt720 = getelementptr inbounds nuw i8, ptr %tile_map, i64 23
  %tmp175.unpack.unpack650.unpack721 = load i8, ptr %tmp175.unpack.unpack650.elt720, align 1
  %29 = insertvalue [9 x i8] %28, i8 %tmp175.unpack.unpack650.unpack721, 5
  %tmp175.unpack.unpack650.elt722 = getelementptr inbounds nuw i8, ptr %tile_map, i64 24
  %tmp175.unpack.unpack650.unpack723 = load i8, ptr %tmp175.unpack.unpack650.elt722, align 1
  %30 = insertvalue [9 x i8] %29, i8 %tmp175.unpack.unpack650.unpack723, 6
  %tmp175.unpack.unpack650.elt724 = getelementptr inbounds nuw i8, ptr %tile_map, i64 25
  %tmp175.unpack.unpack650.unpack725 = load i8, ptr %tmp175.unpack.unpack650.elt724, align 1
  %31 = insertvalue [9 x i8] %30, i8 %tmp175.unpack.unpack650.unpack725, 7
  %tmp175.unpack.unpack650.elt726 = getelementptr inbounds nuw i8, ptr %tile_map, i64 26
  %tmp175.unpack.unpack650.unpack727 = load i8, ptr %tmp175.unpack.unpack650.elt726, align 1
  %tmp175.unpack.unpack650728 = insertvalue [9 x i8] %31, i8 %tmp175.unpack.unpack650.unpack727, 8
  %32 = insertvalue [16 x [9 x i8]] %23, [9 x i8] %tmp175.unpack.unpack650728, 2
  %tmp175.unpack.elt651 = getelementptr inbounds nuw i8, ptr %tile_map, i64 27
  %tmp175.unpack.unpack652.unpack = load i8, ptr %tmp175.unpack.elt651, align 1
  %33 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack652.unpack, 0
  %tmp175.unpack.unpack652.elt729 = getelementptr inbounds nuw i8, ptr %tile_map, i64 28
  %tmp175.unpack.unpack652.unpack730 = load i8, ptr %tmp175.unpack.unpack652.elt729, align 1
  %34 = insertvalue [9 x i8] %33, i8 %tmp175.unpack.unpack652.unpack730, 1
  %tmp175.unpack.unpack652.elt731 = getelementptr inbounds nuw i8, ptr %tile_map, i64 29
  %tmp175.unpack.unpack652.unpack732 = load i8, ptr %tmp175.unpack.unpack652.elt731, align 1
  %35 = insertvalue [9 x i8] %34, i8 %tmp175.unpack.unpack652.unpack732, 2
  %tmp175.unpack.unpack652.elt733 = getelementptr inbounds nuw i8, ptr %tile_map, i64 30
  %tmp175.unpack.unpack652.unpack734 = load i8, ptr %tmp175.unpack.unpack652.elt733, align 1
  %36 = insertvalue [9 x i8] %35, i8 %tmp175.unpack.unpack652.unpack734, 3
  %tmp175.unpack.unpack652.elt735 = getelementptr inbounds nuw i8, ptr %tile_map, i64 31
  %tmp175.unpack.unpack652.unpack736 = load i8, ptr %tmp175.unpack.unpack652.elt735, align 1
  %37 = insertvalue [9 x i8] %36, i8 %tmp175.unpack.unpack652.unpack736, 4
  %tmp175.unpack.unpack652.elt737 = getelementptr inbounds nuw i8, ptr %tile_map, i64 32
  %tmp175.unpack.unpack652.unpack738 = load i8, ptr %tmp175.unpack.unpack652.elt737, align 1
  %38 = insertvalue [9 x i8] %37, i8 %tmp175.unpack.unpack652.unpack738, 5
  %tmp175.unpack.unpack652.elt739 = getelementptr inbounds nuw i8, ptr %tile_map, i64 33
  %tmp175.unpack.unpack652.unpack740 = load i8, ptr %tmp175.unpack.unpack652.elt739, align 1
  %39 = insertvalue [9 x i8] %38, i8 %tmp175.unpack.unpack652.unpack740, 6
  %tmp175.unpack.unpack652.elt741 = getelementptr inbounds nuw i8, ptr %tile_map, i64 34
  %tmp175.unpack.unpack652.unpack742 = load i8, ptr %tmp175.unpack.unpack652.elt741, align 1
  %40 = insertvalue [9 x i8] %39, i8 %tmp175.unpack.unpack652.unpack742, 7
  %tmp175.unpack.unpack652.elt743 = getelementptr inbounds nuw i8, ptr %tile_map, i64 35
  %tmp175.unpack.unpack652.unpack744 = load i8, ptr %tmp175.unpack.unpack652.elt743, align 1
  %tmp175.unpack.unpack652745 = insertvalue [9 x i8] %40, i8 %tmp175.unpack.unpack652.unpack744, 8
  %41 = insertvalue [16 x [9 x i8]] %32, [9 x i8] %tmp175.unpack.unpack652745, 3
  %tmp175.unpack.elt653 = getelementptr inbounds nuw i8, ptr %tile_map, i64 36
  %tmp175.unpack.unpack654.unpack = load i8, ptr %tmp175.unpack.elt653, align 1
  %42 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack654.unpack, 0
  %tmp175.unpack.unpack654.elt746 = getelementptr inbounds nuw i8, ptr %tile_map, i64 37
  %tmp175.unpack.unpack654.unpack747 = load i8, ptr %tmp175.unpack.unpack654.elt746, align 1
  %43 = insertvalue [9 x i8] %42, i8 %tmp175.unpack.unpack654.unpack747, 1
  %tmp175.unpack.unpack654.elt748 = getelementptr inbounds nuw i8, ptr %tile_map, i64 38
  %tmp175.unpack.unpack654.unpack749 = load i8, ptr %tmp175.unpack.unpack654.elt748, align 1
  %44 = insertvalue [9 x i8] %43, i8 %tmp175.unpack.unpack654.unpack749, 2
  %tmp175.unpack.unpack654.elt750 = getelementptr inbounds nuw i8, ptr %tile_map, i64 39
  %tmp175.unpack.unpack654.unpack751 = load i8, ptr %tmp175.unpack.unpack654.elt750, align 1
  %45 = insertvalue [9 x i8] %44, i8 %tmp175.unpack.unpack654.unpack751, 3
  %tmp175.unpack.unpack654.elt752 = getelementptr inbounds nuw i8, ptr %tile_map, i64 40
  %tmp175.unpack.unpack654.unpack753 = load i8, ptr %tmp175.unpack.unpack654.elt752, align 1
  %46 = insertvalue [9 x i8] %45, i8 %tmp175.unpack.unpack654.unpack753, 4
  %tmp175.unpack.unpack654.elt754 = getelementptr inbounds nuw i8, ptr %tile_map, i64 41
  %tmp175.unpack.unpack654.unpack755 = load i8, ptr %tmp175.unpack.unpack654.elt754, align 1
  %47 = insertvalue [9 x i8] %46, i8 %tmp175.unpack.unpack654.unpack755, 5
  %tmp175.unpack.unpack654.elt756 = getelementptr inbounds nuw i8, ptr %tile_map, i64 42
  %tmp175.unpack.unpack654.unpack757 = load i8, ptr %tmp175.unpack.unpack654.elt756, align 1
  %48 = insertvalue [9 x i8] %47, i8 %tmp175.unpack.unpack654.unpack757, 6
  %tmp175.unpack.unpack654.elt758 = getelementptr inbounds nuw i8, ptr %tile_map, i64 43
  %tmp175.unpack.unpack654.unpack759 = load i8, ptr %tmp175.unpack.unpack654.elt758, align 1
  %49 = insertvalue [9 x i8] %48, i8 %tmp175.unpack.unpack654.unpack759, 7
  %tmp175.unpack.unpack654.elt760 = getelementptr inbounds nuw i8, ptr %tile_map, i64 44
  %tmp175.unpack.unpack654.unpack761 = load i8, ptr %tmp175.unpack.unpack654.elt760, align 1
  %tmp175.unpack.unpack654762 = insertvalue [9 x i8] %49, i8 %tmp175.unpack.unpack654.unpack761, 8
  %50 = insertvalue [16 x [9 x i8]] %41, [9 x i8] %tmp175.unpack.unpack654762, 4
  %tmp175.unpack.elt655 = getelementptr inbounds nuw i8, ptr %tile_map, i64 45
  %tmp175.unpack.unpack656.unpack = load i8, ptr %tmp175.unpack.elt655, align 1
  %51 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack656.unpack, 0
  %tmp175.unpack.unpack656.elt763 = getelementptr inbounds nuw i8, ptr %tile_map, i64 46
  %tmp175.unpack.unpack656.unpack764 = load i8, ptr %tmp175.unpack.unpack656.elt763, align 1
  %52 = insertvalue [9 x i8] %51, i8 %tmp175.unpack.unpack656.unpack764, 1
  %tmp175.unpack.unpack656.elt765 = getelementptr inbounds nuw i8, ptr %tile_map, i64 47
  %tmp175.unpack.unpack656.unpack766 = load i8, ptr %tmp175.unpack.unpack656.elt765, align 1
  %53 = insertvalue [9 x i8] %52, i8 %tmp175.unpack.unpack656.unpack766, 2
  %tmp175.unpack.unpack656.elt767 = getelementptr inbounds nuw i8, ptr %tile_map, i64 48
  %tmp175.unpack.unpack656.unpack768 = load i8, ptr %tmp175.unpack.unpack656.elt767, align 1
  %54 = insertvalue [9 x i8] %53, i8 %tmp175.unpack.unpack656.unpack768, 3
  %tmp175.unpack.unpack656.elt769 = getelementptr inbounds nuw i8, ptr %tile_map, i64 49
  %tmp175.unpack.unpack656.unpack770 = load i8, ptr %tmp175.unpack.unpack656.elt769, align 1
  %55 = insertvalue [9 x i8] %54, i8 %tmp175.unpack.unpack656.unpack770, 4
  %tmp175.unpack.unpack656.elt771 = getelementptr inbounds nuw i8, ptr %tile_map, i64 50
  %tmp175.unpack.unpack656.unpack772 = load i8, ptr %tmp175.unpack.unpack656.elt771, align 1
  %56 = insertvalue [9 x i8] %55, i8 %tmp175.unpack.unpack656.unpack772, 5
  %tmp175.unpack.unpack656.elt773 = getelementptr inbounds nuw i8, ptr %tile_map, i64 51
  %tmp175.unpack.unpack656.unpack774 = load i8, ptr %tmp175.unpack.unpack656.elt773, align 1
  %57 = insertvalue [9 x i8] %56, i8 %tmp175.unpack.unpack656.unpack774, 6
  %tmp175.unpack.unpack656.elt775 = getelementptr inbounds nuw i8, ptr %tile_map, i64 52
  %tmp175.unpack.unpack656.unpack776 = load i8, ptr %tmp175.unpack.unpack656.elt775, align 1
  %58 = insertvalue [9 x i8] %57, i8 %tmp175.unpack.unpack656.unpack776, 7
  %tmp175.unpack.unpack656.elt777 = getelementptr inbounds nuw i8, ptr %tile_map, i64 53
  %tmp175.unpack.unpack656.unpack778 = load i8, ptr %tmp175.unpack.unpack656.elt777, align 1
  %tmp175.unpack.unpack656779 = insertvalue [9 x i8] %58, i8 %tmp175.unpack.unpack656.unpack778, 8
  %59 = insertvalue [16 x [9 x i8]] %50, [9 x i8] %tmp175.unpack.unpack656779, 5
  %tmp175.unpack.elt657 = getelementptr inbounds nuw i8, ptr %tile_map, i64 54
  %tmp175.unpack.unpack658.unpack = load i8, ptr %tmp175.unpack.elt657, align 1
  %60 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack658.unpack, 0
  %tmp175.unpack.unpack658.elt780 = getelementptr inbounds nuw i8, ptr %tile_map, i64 55
  %tmp175.unpack.unpack658.unpack781 = load i8, ptr %tmp175.unpack.unpack658.elt780, align 1
  %61 = insertvalue [9 x i8] %60, i8 %tmp175.unpack.unpack658.unpack781, 1
  %tmp175.unpack.unpack658.elt782 = getelementptr inbounds nuw i8, ptr %tile_map, i64 56
  %tmp175.unpack.unpack658.unpack783 = load i8, ptr %tmp175.unpack.unpack658.elt782, align 1
  %62 = insertvalue [9 x i8] %61, i8 %tmp175.unpack.unpack658.unpack783, 2
  %tmp175.unpack.unpack658.elt784 = getelementptr inbounds nuw i8, ptr %tile_map, i64 57
  %tmp175.unpack.unpack658.unpack785 = load i8, ptr %tmp175.unpack.unpack658.elt784, align 1
  %63 = insertvalue [9 x i8] %62, i8 %tmp175.unpack.unpack658.unpack785, 3
  %tmp175.unpack.unpack658.elt786 = getelementptr inbounds nuw i8, ptr %tile_map, i64 58
  %tmp175.unpack.unpack658.unpack787 = load i8, ptr %tmp175.unpack.unpack658.elt786, align 1
  %64 = insertvalue [9 x i8] %63, i8 %tmp175.unpack.unpack658.unpack787, 4
  %tmp175.unpack.unpack658.elt788 = getelementptr inbounds nuw i8, ptr %tile_map, i64 59
  %tmp175.unpack.unpack658.unpack789 = load i8, ptr %tmp175.unpack.unpack658.elt788, align 1
  %65 = insertvalue [9 x i8] %64, i8 %tmp175.unpack.unpack658.unpack789, 5
  %tmp175.unpack.unpack658.elt790 = getelementptr inbounds nuw i8, ptr %tile_map, i64 60
  %tmp175.unpack.unpack658.unpack791 = load i8, ptr %tmp175.unpack.unpack658.elt790, align 1
  %66 = insertvalue [9 x i8] %65, i8 %tmp175.unpack.unpack658.unpack791, 6
  %tmp175.unpack.unpack658.elt792 = getelementptr inbounds nuw i8, ptr %tile_map, i64 61
  %tmp175.unpack.unpack658.unpack793 = load i8, ptr %tmp175.unpack.unpack658.elt792, align 1
  %67 = insertvalue [9 x i8] %66, i8 %tmp175.unpack.unpack658.unpack793, 7
  %tmp175.unpack.unpack658.elt794 = getelementptr inbounds nuw i8, ptr %tile_map, i64 62
  %tmp175.unpack.unpack658.unpack795 = load i8, ptr %tmp175.unpack.unpack658.elt794, align 1
  %tmp175.unpack.unpack658796 = insertvalue [9 x i8] %67, i8 %tmp175.unpack.unpack658.unpack795, 8
  %68 = insertvalue [16 x [9 x i8]] %59, [9 x i8] %tmp175.unpack.unpack658796, 6
  %tmp175.unpack.elt659 = getelementptr inbounds nuw i8, ptr %tile_map, i64 63
  %tmp175.unpack.unpack660.unpack = load i8, ptr %tmp175.unpack.elt659, align 1
  %69 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack660.unpack, 0
  %tmp175.unpack.unpack660.elt797 = getelementptr inbounds nuw i8, ptr %tile_map, i64 64
  %tmp175.unpack.unpack660.unpack798 = load i8, ptr %tmp175.unpack.unpack660.elt797, align 1
  %70 = insertvalue [9 x i8] %69, i8 %tmp175.unpack.unpack660.unpack798, 1
  %tmp175.unpack.unpack660.elt799 = getelementptr inbounds nuw i8, ptr %tile_map, i64 65
  %tmp175.unpack.unpack660.unpack800 = load i8, ptr %tmp175.unpack.unpack660.elt799, align 1
  %71 = insertvalue [9 x i8] %70, i8 %tmp175.unpack.unpack660.unpack800, 2
  %tmp175.unpack.unpack660.elt801 = getelementptr inbounds nuw i8, ptr %tile_map, i64 66
  %tmp175.unpack.unpack660.unpack802 = load i8, ptr %tmp175.unpack.unpack660.elt801, align 1
  %72 = insertvalue [9 x i8] %71, i8 %tmp175.unpack.unpack660.unpack802, 3
  %tmp175.unpack.unpack660.elt803 = getelementptr inbounds nuw i8, ptr %tile_map, i64 67
  %tmp175.unpack.unpack660.unpack804 = load i8, ptr %tmp175.unpack.unpack660.elt803, align 1
  %73 = insertvalue [9 x i8] %72, i8 %tmp175.unpack.unpack660.unpack804, 4
  %tmp175.unpack.unpack660.elt805 = getelementptr inbounds nuw i8, ptr %tile_map, i64 68
  %tmp175.unpack.unpack660.unpack806 = load i8, ptr %tmp175.unpack.unpack660.elt805, align 1
  %74 = insertvalue [9 x i8] %73, i8 %tmp175.unpack.unpack660.unpack806, 5
  %tmp175.unpack.unpack660.elt807 = getelementptr inbounds nuw i8, ptr %tile_map, i64 69
  %tmp175.unpack.unpack660.unpack808 = load i8, ptr %tmp175.unpack.unpack660.elt807, align 1
  %75 = insertvalue [9 x i8] %74, i8 %tmp175.unpack.unpack660.unpack808, 6
  %tmp175.unpack.unpack660.elt809 = getelementptr inbounds nuw i8, ptr %tile_map, i64 70
  %tmp175.unpack.unpack660.unpack810 = load i8, ptr %tmp175.unpack.unpack660.elt809, align 1
  %76 = insertvalue [9 x i8] %75, i8 %tmp175.unpack.unpack660.unpack810, 7
  %tmp175.unpack.unpack660.elt811 = getelementptr inbounds nuw i8, ptr %tile_map, i64 71
  %tmp175.unpack.unpack660.unpack812 = load i8, ptr %tmp175.unpack.unpack660.elt811, align 1
  %tmp175.unpack.unpack660813 = insertvalue [9 x i8] %76, i8 %tmp175.unpack.unpack660.unpack812, 8
  %77 = insertvalue [16 x [9 x i8]] %68, [9 x i8] %tmp175.unpack.unpack660813, 7
  %tmp175.unpack.elt661 = getelementptr inbounds nuw i8, ptr %tile_map, i64 72
  %tmp175.unpack.unpack662.unpack = load i8, ptr %tmp175.unpack.elt661, align 1
  %78 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack662.unpack, 0
  %tmp175.unpack.unpack662.elt814 = getelementptr inbounds nuw i8, ptr %tile_map, i64 73
  %tmp175.unpack.unpack662.unpack815 = load i8, ptr %tmp175.unpack.unpack662.elt814, align 1
  %79 = insertvalue [9 x i8] %78, i8 %tmp175.unpack.unpack662.unpack815, 1
  %tmp175.unpack.unpack662.elt816 = getelementptr inbounds nuw i8, ptr %tile_map, i64 74
  %tmp175.unpack.unpack662.unpack817 = load i8, ptr %tmp175.unpack.unpack662.elt816, align 1
  %80 = insertvalue [9 x i8] %79, i8 %tmp175.unpack.unpack662.unpack817, 2
  %tmp175.unpack.unpack662.elt818 = getelementptr inbounds nuw i8, ptr %tile_map, i64 75
  %tmp175.unpack.unpack662.unpack819 = load i8, ptr %tmp175.unpack.unpack662.elt818, align 1
  %81 = insertvalue [9 x i8] %80, i8 %tmp175.unpack.unpack662.unpack819, 3
  %tmp175.unpack.unpack662.elt820 = getelementptr inbounds nuw i8, ptr %tile_map, i64 76
  %tmp175.unpack.unpack662.unpack821 = load i8, ptr %tmp175.unpack.unpack662.elt820, align 1
  %82 = insertvalue [9 x i8] %81, i8 %tmp175.unpack.unpack662.unpack821, 4
  %tmp175.unpack.unpack662.elt822 = getelementptr inbounds nuw i8, ptr %tile_map, i64 77
  %tmp175.unpack.unpack662.unpack823 = load i8, ptr %tmp175.unpack.unpack662.elt822, align 1
  %83 = insertvalue [9 x i8] %82, i8 %tmp175.unpack.unpack662.unpack823, 5
  %tmp175.unpack.unpack662.elt824 = getelementptr inbounds nuw i8, ptr %tile_map, i64 78
  %tmp175.unpack.unpack662.unpack825 = load i8, ptr %tmp175.unpack.unpack662.elt824, align 1
  %84 = insertvalue [9 x i8] %83, i8 %tmp175.unpack.unpack662.unpack825, 6
  %tmp175.unpack.unpack662.elt826 = getelementptr inbounds nuw i8, ptr %tile_map, i64 79
  %tmp175.unpack.unpack662.unpack827 = load i8, ptr %tmp175.unpack.unpack662.elt826, align 1
  %85 = insertvalue [9 x i8] %84, i8 %tmp175.unpack.unpack662.unpack827, 7
  %tmp175.unpack.unpack662.elt828 = getelementptr inbounds nuw i8, ptr %tile_map, i64 80
  %tmp175.unpack.unpack662.unpack829 = load i8, ptr %tmp175.unpack.unpack662.elt828, align 1
  %tmp175.unpack.unpack662830 = insertvalue [9 x i8] %85, i8 %tmp175.unpack.unpack662.unpack829, 8
  %86 = insertvalue [16 x [9 x i8]] %77, [9 x i8] %tmp175.unpack.unpack662830, 8
  %tmp175.unpack.elt663 = getelementptr inbounds nuw i8, ptr %tile_map, i64 81
  %tmp175.unpack.unpack664.unpack = load i8, ptr %tmp175.unpack.elt663, align 1
  %87 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack664.unpack, 0
  %tmp175.unpack.unpack664.elt831 = getelementptr inbounds nuw i8, ptr %tile_map, i64 82
  %tmp175.unpack.unpack664.unpack832 = load i8, ptr %tmp175.unpack.unpack664.elt831, align 1
  %88 = insertvalue [9 x i8] %87, i8 %tmp175.unpack.unpack664.unpack832, 1
  %tmp175.unpack.unpack664.elt833 = getelementptr inbounds nuw i8, ptr %tile_map, i64 83
  %tmp175.unpack.unpack664.unpack834 = load i8, ptr %tmp175.unpack.unpack664.elt833, align 1
  %89 = insertvalue [9 x i8] %88, i8 %tmp175.unpack.unpack664.unpack834, 2
  %tmp175.unpack.unpack664.elt835 = getelementptr inbounds nuw i8, ptr %tile_map, i64 84
  %tmp175.unpack.unpack664.unpack836 = load i8, ptr %tmp175.unpack.unpack664.elt835, align 1
  %90 = insertvalue [9 x i8] %89, i8 %tmp175.unpack.unpack664.unpack836, 3
  %tmp175.unpack.unpack664.elt837 = getelementptr inbounds nuw i8, ptr %tile_map, i64 85
  %tmp175.unpack.unpack664.unpack838 = load i8, ptr %tmp175.unpack.unpack664.elt837, align 1
  %91 = insertvalue [9 x i8] %90, i8 %tmp175.unpack.unpack664.unpack838, 4
  %tmp175.unpack.unpack664.elt839 = getelementptr inbounds nuw i8, ptr %tile_map, i64 86
  %tmp175.unpack.unpack664.unpack840 = load i8, ptr %tmp175.unpack.unpack664.elt839, align 1
  %92 = insertvalue [9 x i8] %91, i8 %tmp175.unpack.unpack664.unpack840, 5
  %tmp175.unpack.unpack664.elt841 = getelementptr inbounds nuw i8, ptr %tile_map, i64 87
  %tmp175.unpack.unpack664.unpack842 = load i8, ptr %tmp175.unpack.unpack664.elt841, align 1
  %93 = insertvalue [9 x i8] %92, i8 %tmp175.unpack.unpack664.unpack842, 6
  %tmp175.unpack.unpack664.elt843 = getelementptr inbounds nuw i8, ptr %tile_map, i64 88
  %tmp175.unpack.unpack664.unpack844 = load i8, ptr %tmp175.unpack.unpack664.elt843, align 1
  %94 = insertvalue [9 x i8] %93, i8 %tmp175.unpack.unpack664.unpack844, 7
  %tmp175.unpack.unpack664.elt845 = getelementptr inbounds nuw i8, ptr %tile_map, i64 89
  %tmp175.unpack.unpack664.unpack846 = load i8, ptr %tmp175.unpack.unpack664.elt845, align 1
  %tmp175.unpack.unpack664847 = insertvalue [9 x i8] %94, i8 %tmp175.unpack.unpack664.unpack846, 8
  %95 = insertvalue [16 x [9 x i8]] %86, [9 x i8] %tmp175.unpack.unpack664847, 9
  %tmp175.unpack.elt665 = getelementptr inbounds nuw i8, ptr %tile_map, i64 90
  %tmp175.unpack.unpack666.unpack = load i8, ptr %tmp175.unpack.elt665, align 1
  %96 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack666.unpack, 0
  %tmp175.unpack.unpack666.elt848 = getelementptr inbounds nuw i8, ptr %tile_map, i64 91
  %tmp175.unpack.unpack666.unpack849 = load i8, ptr %tmp175.unpack.unpack666.elt848, align 1
  %97 = insertvalue [9 x i8] %96, i8 %tmp175.unpack.unpack666.unpack849, 1
  %tmp175.unpack.unpack666.elt850 = getelementptr inbounds nuw i8, ptr %tile_map, i64 92
  %tmp175.unpack.unpack666.unpack851 = load i8, ptr %tmp175.unpack.unpack666.elt850, align 1
  %98 = insertvalue [9 x i8] %97, i8 %tmp175.unpack.unpack666.unpack851, 2
  %tmp175.unpack.unpack666.elt852 = getelementptr inbounds nuw i8, ptr %tile_map, i64 93
  %tmp175.unpack.unpack666.unpack853 = load i8, ptr %tmp175.unpack.unpack666.elt852, align 1
  %99 = insertvalue [9 x i8] %98, i8 %tmp175.unpack.unpack666.unpack853, 3
  %tmp175.unpack.unpack666.elt854 = getelementptr inbounds nuw i8, ptr %tile_map, i64 94
  %tmp175.unpack.unpack666.unpack855 = load i8, ptr %tmp175.unpack.unpack666.elt854, align 1
  %100 = insertvalue [9 x i8] %99, i8 %tmp175.unpack.unpack666.unpack855, 4
  %tmp175.unpack.unpack666.elt856 = getelementptr inbounds nuw i8, ptr %tile_map, i64 95
  %tmp175.unpack.unpack666.unpack857 = load i8, ptr %tmp175.unpack.unpack666.elt856, align 1
  %101 = insertvalue [9 x i8] %100, i8 %tmp175.unpack.unpack666.unpack857, 5
  %tmp175.unpack.unpack666.elt858 = getelementptr inbounds nuw i8, ptr %tile_map, i64 96
  %tmp175.unpack.unpack666.unpack859 = load i8, ptr %tmp175.unpack.unpack666.elt858, align 1
  %102 = insertvalue [9 x i8] %101, i8 %tmp175.unpack.unpack666.unpack859, 6
  %tmp175.unpack.unpack666.elt860 = getelementptr inbounds nuw i8, ptr %tile_map, i64 97
  %tmp175.unpack.unpack666.unpack861 = load i8, ptr %tmp175.unpack.unpack666.elt860, align 1
  %103 = insertvalue [9 x i8] %102, i8 %tmp175.unpack.unpack666.unpack861, 7
  %tmp175.unpack.unpack666.elt862 = getelementptr inbounds nuw i8, ptr %tile_map, i64 98
  %tmp175.unpack.unpack666.unpack863 = load i8, ptr %tmp175.unpack.unpack666.elt862, align 1
  %tmp175.unpack.unpack666864 = insertvalue [9 x i8] %103, i8 %tmp175.unpack.unpack666.unpack863, 8
  %104 = insertvalue [16 x [9 x i8]] %95, [9 x i8] %tmp175.unpack.unpack666864, 10
  %tmp175.unpack.elt667 = getelementptr inbounds nuw i8, ptr %tile_map, i64 99
  %tmp175.unpack.unpack668.unpack = load i8, ptr %tmp175.unpack.elt667, align 1
  %105 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack668.unpack, 0
  %tmp175.unpack.unpack668.elt865 = getelementptr inbounds nuw i8, ptr %tile_map, i64 100
  %tmp175.unpack.unpack668.unpack866 = load i8, ptr %tmp175.unpack.unpack668.elt865, align 1
  %106 = insertvalue [9 x i8] %105, i8 %tmp175.unpack.unpack668.unpack866, 1
  %tmp175.unpack.unpack668.elt867 = getelementptr inbounds nuw i8, ptr %tile_map, i64 101
  %tmp175.unpack.unpack668.unpack868 = load i8, ptr %tmp175.unpack.unpack668.elt867, align 1
  %107 = insertvalue [9 x i8] %106, i8 %tmp175.unpack.unpack668.unpack868, 2
  %tmp175.unpack.unpack668.elt869 = getelementptr inbounds nuw i8, ptr %tile_map, i64 102
  %tmp175.unpack.unpack668.unpack870 = load i8, ptr %tmp175.unpack.unpack668.elt869, align 1
  %108 = insertvalue [9 x i8] %107, i8 %tmp175.unpack.unpack668.unpack870, 3
  %tmp175.unpack.unpack668.elt871 = getelementptr inbounds nuw i8, ptr %tile_map, i64 103
  %tmp175.unpack.unpack668.unpack872 = load i8, ptr %tmp175.unpack.unpack668.elt871, align 1
  %109 = insertvalue [9 x i8] %108, i8 %tmp175.unpack.unpack668.unpack872, 4
  %tmp175.unpack.unpack668.elt873 = getelementptr inbounds nuw i8, ptr %tile_map, i64 104
  %tmp175.unpack.unpack668.unpack874 = load i8, ptr %tmp175.unpack.unpack668.elt873, align 1
  %110 = insertvalue [9 x i8] %109, i8 %tmp175.unpack.unpack668.unpack874, 5
  %tmp175.unpack.unpack668.elt875 = getelementptr inbounds nuw i8, ptr %tile_map, i64 105
  %tmp175.unpack.unpack668.unpack876 = load i8, ptr %tmp175.unpack.unpack668.elt875, align 1
  %111 = insertvalue [9 x i8] %110, i8 %tmp175.unpack.unpack668.unpack876, 6
  %tmp175.unpack.unpack668.elt877 = getelementptr inbounds nuw i8, ptr %tile_map, i64 106
  %tmp175.unpack.unpack668.unpack878 = load i8, ptr %tmp175.unpack.unpack668.elt877, align 1
  %112 = insertvalue [9 x i8] %111, i8 %tmp175.unpack.unpack668.unpack878, 7
  %tmp175.unpack.unpack668.elt879 = getelementptr inbounds nuw i8, ptr %tile_map, i64 107
  %tmp175.unpack.unpack668.unpack880 = load i8, ptr %tmp175.unpack.unpack668.elt879, align 1
  %tmp175.unpack.unpack668881 = insertvalue [9 x i8] %112, i8 %tmp175.unpack.unpack668.unpack880, 8
  %113 = insertvalue [16 x [9 x i8]] %104, [9 x i8] %tmp175.unpack.unpack668881, 11
  %tmp175.unpack.elt669 = getelementptr inbounds nuw i8, ptr %tile_map, i64 108
  %tmp175.unpack.unpack670.unpack = load i8, ptr %tmp175.unpack.elt669, align 1
  %114 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack670.unpack, 0
  %tmp175.unpack.unpack670.elt882 = getelementptr inbounds nuw i8, ptr %tile_map, i64 109
  %tmp175.unpack.unpack670.unpack883 = load i8, ptr %tmp175.unpack.unpack670.elt882, align 1
  %115 = insertvalue [9 x i8] %114, i8 %tmp175.unpack.unpack670.unpack883, 1
  %tmp175.unpack.unpack670.elt884 = getelementptr inbounds nuw i8, ptr %tile_map, i64 110
  %tmp175.unpack.unpack670.unpack885 = load i8, ptr %tmp175.unpack.unpack670.elt884, align 1
  %116 = insertvalue [9 x i8] %115, i8 %tmp175.unpack.unpack670.unpack885, 2
  %tmp175.unpack.unpack670.elt886 = getelementptr inbounds nuw i8, ptr %tile_map, i64 111
  %tmp175.unpack.unpack670.unpack887 = load i8, ptr %tmp175.unpack.unpack670.elt886, align 1
  %117 = insertvalue [9 x i8] %116, i8 %tmp175.unpack.unpack670.unpack887, 3
  %tmp175.unpack.unpack670.elt888 = getelementptr inbounds nuw i8, ptr %tile_map, i64 112
  %tmp175.unpack.unpack670.unpack889 = load i8, ptr %tmp175.unpack.unpack670.elt888, align 1
  %118 = insertvalue [9 x i8] %117, i8 %tmp175.unpack.unpack670.unpack889, 4
  %tmp175.unpack.unpack670.elt890 = getelementptr inbounds nuw i8, ptr %tile_map, i64 113
  %tmp175.unpack.unpack670.unpack891 = load i8, ptr %tmp175.unpack.unpack670.elt890, align 1
  %119 = insertvalue [9 x i8] %118, i8 %tmp175.unpack.unpack670.unpack891, 5
  %tmp175.unpack.unpack670.elt892 = getelementptr inbounds nuw i8, ptr %tile_map, i64 114
  %tmp175.unpack.unpack670.unpack893 = load i8, ptr %tmp175.unpack.unpack670.elt892, align 1
  %120 = insertvalue [9 x i8] %119, i8 %tmp175.unpack.unpack670.unpack893, 6
  %tmp175.unpack.unpack670.elt894 = getelementptr inbounds nuw i8, ptr %tile_map, i64 115
  %tmp175.unpack.unpack670.unpack895 = load i8, ptr %tmp175.unpack.unpack670.elt894, align 1
  %121 = insertvalue [9 x i8] %120, i8 %tmp175.unpack.unpack670.unpack895, 7
  %tmp175.unpack.unpack670.elt896 = getelementptr inbounds nuw i8, ptr %tile_map, i64 116
  %tmp175.unpack.unpack670.unpack897 = load i8, ptr %tmp175.unpack.unpack670.elt896, align 1
  %tmp175.unpack.unpack670898 = insertvalue [9 x i8] %121, i8 %tmp175.unpack.unpack670.unpack897, 8
  %122 = insertvalue [16 x [9 x i8]] %113, [9 x i8] %tmp175.unpack.unpack670898, 12
  %tmp175.unpack.elt671 = getelementptr inbounds nuw i8, ptr %tile_map, i64 117
  %tmp175.unpack.unpack672.unpack = load i8, ptr %tmp175.unpack.elt671, align 1
  %123 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack672.unpack, 0
  %tmp175.unpack.unpack672.elt899 = getelementptr inbounds nuw i8, ptr %tile_map, i64 118
  %tmp175.unpack.unpack672.unpack900 = load i8, ptr %tmp175.unpack.unpack672.elt899, align 1
  %124 = insertvalue [9 x i8] %123, i8 %tmp175.unpack.unpack672.unpack900, 1
  %tmp175.unpack.unpack672.elt901 = getelementptr inbounds nuw i8, ptr %tile_map, i64 119
  %tmp175.unpack.unpack672.unpack902 = load i8, ptr %tmp175.unpack.unpack672.elt901, align 1
  %125 = insertvalue [9 x i8] %124, i8 %tmp175.unpack.unpack672.unpack902, 2
  %tmp175.unpack.unpack672.elt903 = getelementptr inbounds nuw i8, ptr %tile_map, i64 120
  %tmp175.unpack.unpack672.unpack904 = load i8, ptr %tmp175.unpack.unpack672.elt903, align 1
  %126 = insertvalue [9 x i8] %125, i8 %tmp175.unpack.unpack672.unpack904, 3
  %tmp175.unpack.unpack672.elt905 = getelementptr inbounds nuw i8, ptr %tile_map, i64 121
  %tmp175.unpack.unpack672.unpack906 = load i8, ptr %tmp175.unpack.unpack672.elt905, align 1
  %127 = insertvalue [9 x i8] %126, i8 %tmp175.unpack.unpack672.unpack906, 4
  %tmp175.unpack.unpack672.elt907 = getelementptr inbounds nuw i8, ptr %tile_map, i64 122
  %tmp175.unpack.unpack672.unpack908 = load i8, ptr %tmp175.unpack.unpack672.elt907, align 1
  %128 = insertvalue [9 x i8] %127, i8 %tmp175.unpack.unpack672.unpack908, 5
  %tmp175.unpack.unpack672.elt909 = getelementptr inbounds nuw i8, ptr %tile_map, i64 123
  %tmp175.unpack.unpack672.unpack910 = load i8, ptr %tmp175.unpack.unpack672.elt909, align 1
  %129 = insertvalue [9 x i8] %128, i8 %tmp175.unpack.unpack672.unpack910, 6
  %tmp175.unpack.unpack672.elt911 = getelementptr inbounds nuw i8, ptr %tile_map, i64 124
  %tmp175.unpack.unpack672.unpack912 = load i8, ptr %tmp175.unpack.unpack672.elt911, align 1
  %130 = insertvalue [9 x i8] %129, i8 %tmp175.unpack.unpack672.unpack912, 7
  %tmp175.unpack.unpack672.elt913 = getelementptr inbounds nuw i8, ptr %tile_map, i64 125
  %tmp175.unpack.unpack672.unpack914 = load i8, ptr %tmp175.unpack.unpack672.elt913, align 1
  %tmp175.unpack.unpack672915 = insertvalue [9 x i8] %130, i8 %tmp175.unpack.unpack672.unpack914, 8
  %131 = insertvalue [16 x [9 x i8]] %122, [9 x i8] %tmp175.unpack.unpack672915, 13
  %tmp175.unpack.elt673 = getelementptr inbounds nuw i8, ptr %tile_map, i64 126
  %tmp175.unpack.unpack674.unpack = load i8, ptr %tmp175.unpack.elt673, align 1
  %132 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack674.unpack, 0
  %tmp175.unpack.unpack674.elt916 = getelementptr inbounds nuw i8, ptr %tile_map, i64 127
  %tmp175.unpack.unpack674.unpack917 = load i8, ptr %tmp175.unpack.unpack674.elt916, align 1
  %133 = insertvalue [9 x i8] %132, i8 %tmp175.unpack.unpack674.unpack917, 1
  %tmp175.unpack.unpack674.elt918 = getelementptr inbounds nuw i8, ptr %tile_map, i64 128
  %tmp175.unpack.unpack674.unpack919 = load i8, ptr %tmp175.unpack.unpack674.elt918, align 1
  %134 = insertvalue [9 x i8] %133, i8 %tmp175.unpack.unpack674.unpack919, 2
  %tmp175.unpack.unpack674.elt920 = getelementptr inbounds nuw i8, ptr %tile_map, i64 129
  %tmp175.unpack.unpack674.unpack921 = load i8, ptr %tmp175.unpack.unpack674.elt920, align 1
  %135 = insertvalue [9 x i8] %134, i8 %tmp175.unpack.unpack674.unpack921, 3
  %tmp175.unpack.unpack674.elt922 = getelementptr inbounds nuw i8, ptr %tile_map, i64 130
  %tmp175.unpack.unpack674.unpack923 = load i8, ptr %tmp175.unpack.unpack674.elt922, align 1
  %136 = insertvalue [9 x i8] %135, i8 %tmp175.unpack.unpack674.unpack923, 4
  %tmp175.unpack.unpack674.elt924 = getelementptr inbounds nuw i8, ptr %tile_map, i64 131
  %tmp175.unpack.unpack674.unpack925 = load i8, ptr %tmp175.unpack.unpack674.elt924, align 1
  %137 = insertvalue [9 x i8] %136, i8 %tmp175.unpack.unpack674.unpack925, 5
  %tmp175.unpack.unpack674.elt926 = getelementptr inbounds nuw i8, ptr %tile_map, i64 132
  %tmp175.unpack.unpack674.unpack927 = load i8, ptr %tmp175.unpack.unpack674.elt926, align 1
  %138 = insertvalue [9 x i8] %137, i8 %tmp175.unpack.unpack674.unpack927, 6
  %tmp175.unpack.unpack674.elt928 = getelementptr inbounds nuw i8, ptr %tile_map, i64 133
  %tmp175.unpack.unpack674.unpack929 = load i8, ptr %tmp175.unpack.unpack674.elt928, align 1
  %139 = insertvalue [9 x i8] %138, i8 %tmp175.unpack.unpack674.unpack929, 7
  %tmp175.unpack.unpack674.elt930 = getelementptr inbounds nuw i8, ptr %tile_map, i64 134
  %tmp175.unpack.unpack674.unpack931 = load i8, ptr %tmp175.unpack.unpack674.elt930, align 1
  %tmp175.unpack.unpack674932 = insertvalue [9 x i8] %139, i8 %tmp175.unpack.unpack674.unpack931, 8
  %140 = insertvalue [16 x [9 x i8]] %131, [9 x i8] %tmp175.unpack.unpack674932, 14
  %tmp175.unpack.elt675 = getelementptr inbounds nuw i8, ptr %tile_map, i64 135
  %tmp175.unpack.unpack676.unpack = load i8, ptr %tmp175.unpack.elt675, align 1
  %141 = insertvalue [9 x i8] poison, i8 %tmp175.unpack.unpack676.unpack, 0
  %tmp175.unpack.unpack676.elt933 = getelementptr inbounds nuw i8, ptr %tile_map, i64 136
  %tmp175.unpack.unpack676.unpack934 = load i8, ptr %tmp175.unpack.unpack676.elt933, align 1
  %142 = insertvalue [9 x i8] %141, i8 %tmp175.unpack.unpack676.unpack934, 1
  %tmp175.unpack.unpack676.elt935 = getelementptr inbounds nuw i8, ptr %tile_map, i64 137
  %tmp175.unpack.unpack676.unpack936 = load i8, ptr %tmp175.unpack.unpack676.elt935, align 1
  %143 = insertvalue [9 x i8] %142, i8 %tmp175.unpack.unpack676.unpack936, 2
  %tmp175.unpack.unpack676.elt937 = getelementptr inbounds nuw i8, ptr %tile_map, i64 138
  %tmp175.unpack.unpack676.unpack938 = load i8, ptr %tmp175.unpack.unpack676.elt937, align 1
  %144 = insertvalue [9 x i8] %143, i8 %tmp175.unpack.unpack676.unpack938, 3
  %tmp175.unpack.unpack676.elt939 = getelementptr inbounds nuw i8, ptr %tile_map, i64 139
  %tmp175.unpack.unpack676.unpack940 = load i8, ptr %tmp175.unpack.unpack676.elt939, align 1
  %145 = insertvalue [9 x i8] %144, i8 %tmp175.unpack.unpack676.unpack940, 4
  %tmp175.unpack.unpack676.elt941 = getelementptr inbounds nuw i8, ptr %tile_map, i64 140
  %tmp175.unpack.unpack676.unpack942 = load i8, ptr %tmp175.unpack.unpack676.elt941, align 1
  %146 = insertvalue [9 x i8] %145, i8 %tmp175.unpack.unpack676.unpack942, 5
  %tmp175.unpack.unpack676.elt943 = getelementptr inbounds nuw i8, ptr %tile_map, i64 141
  %tmp175.unpack.unpack676.unpack944 = load i8, ptr %tmp175.unpack.unpack676.elt943, align 1
  %147 = insertvalue [9 x i8] %146, i8 %tmp175.unpack.unpack676.unpack944, 6
  %tmp175.unpack.unpack676.elt945 = getelementptr inbounds nuw i8, ptr %tile_map, i64 142
  %tmp175.unpack.unpack676.unpack946 = load i8, ptr %tmp175.unpack.unpack676.elt945, align 1
  %148 = insertvalue [9 x i8] %147, i8 %tmp175.unpack.unpack676.unpack946, 7
  %tmp175.unpack.unpack676.elt947 = getelementptr inbounds nuw i8, ptr %tile_map, i64 143
  %tmp175.unpack.unpack676.unpack948 = load i8, ptr %tmp175.unpack.unpack676.elt947, align 1
  %tmp175.unpack.unpack676949 = insertvalue [9 x i8] %148, i8 %tmp175.unpack.unpack676.unpack948, 8
  %tmp175.unpack677 = insertvalue [16 x [9 x i8]] %140, [9 x i8] %tmp175.unpack.unpack676949, 15
  %fun_call_tmp.i263 = call {} @"[[u8; 9]; 16]:drop"([16 x [9 x i8]] %tmp175.unpack677)
  %tmp177.unpack.unpack.unpack = load i64, ptr %game, align 8
  %tmp177.unpack.unpack.elt960 = getelementptr inbounds nuw i8, ptr %game, i64 8
  %tmp177.unpack.unpack.unpack961 = load i64, ptr %tmp177.unpack.unpack.elt960, align 8
  %tmp177.unpack.elt957 = getelementptr inbounds nuw i8, ptr %game, i64 16
  %tmp177.unpack.unpack958.unpack = load ptr, ptr %tmp177.unpack.elt957, align 8
  %149 = insertvalue %"Vec<T = u8>" poison, ptr %tmp177.unpack.unpack958.unpack, 0
  %tmp177.unpack.unpack958.elt963 = getelementptr inbounds nuw i8, ptr %game, i64 24
  %tmp177.unpack.unpack958.unpack964 = load i64, ptr %tmp177.unpack.unpack958.elt963, align 8
  %150 = insertvalue %"Vec<T = u8>" %149, i64 %tmp177.unpack.unpack958.unpack964, 1
  %tmp177.unpack.unpack958.elt965 = getelementptr inbounds nuw i8, ptr %game, i64 32
  %tmp177.unpack.unpack958.unpack966 = load i64, ptr %tmp177.unpack.unpack958.elt965, align 8
  %tmp177.unpack.unpack958967 = insertvalue %"Vec<T = u8>" %150, i64 %tmp177.unpack.unpack958.unpack966, 2
  %tmp177.elt950 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %tmp177.unpack951.unpack = load ptr, ptr %tmp177.elt950, align 8
  %151 = insertvalue %"Vec<T = char>" poison, ptr %tmp177.unpack951.unpack, 0
  %tmp177.unpack951.elt968 = getelementptr inbounds nuw i8, ptr %game, i64 48
  %tmp177.unpack951.unpack969 = load i64, ptr %tmp177.unpack951.elt968, align 8
  %152 = insertvalue %"Vec<T = char>" %151, i64 %tmp177.unpack951.unpack969, 1
  %tmp177.unpack951.elt970 = getelementptr inbounds nuw i8, ptr %game, i64 56
  %tmp177.unpack951.unpack971 = load i64, ptr %tmp177.unpack951.elt970, align 8
  %tmp177.unpack951972 = insertvalue %"Vec<T = char>" %152, i64 %tmp177.unpack951.unpack971, 2
  %tmp177.elt952 = getelementptr inbounds nuw i8, ptr %game, i64 64
  %tmp177.unpack953 = load i64, ptr %tmp177.elt952, align 8
  %tmp177.elt954 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %tmp177.unpack955 = load i64, ptr %tmp177.elt954, align 8
  %fun_call_tmp.i233 = call {} @"u64:drop"(i64 %tmp177.unpack955)
  %fun_call_tmp2.i235 = call {} @"u64:drop"(i64 %tmp177.unpack953)
  %fun_call_tmp4.i = call {} @"Vec<T = char>:drop.35"(%"Vec<T = char>" %tmp177.unpack951972)
  %fun_call_tmp.i.i = call {} @"Vec<T = u8>:drop.17"(%"Vec<T = u8>" %tmp177.unpack.unpack958967)
  %fun_call_tmp.i.i.i = call {} @"u64:drop"(i64 %tmp177.unpack.unpack.unpack961)
  %fun_call_tmp2.i.i.i = call {} @"u64:drop"(i64 %tmp177.unpack.unpack.unpack)
  %tmp179.unpack = load i64, ptr %screen_size, align 4
  %tmp179.elt973 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp179.unpack974 = load i64, ptr %tmp179.elt973, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp179.unpack974)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp179.unpack)
  ret {} zeroinitializer

else55:                                           ; preds = %then47
  %tmp59 = load i8, ptr %tmp9, align 1
  %fun_call_tmp60 = call {} @"char:drop"(i8 %tmp59)
  br label %cond48

then67:                                           ; preds = %cond68
  %fun_call_tmp71 = call ptr @"Vec<T = Particle>:next"(ptr nonnull %tmp_iter8)
  %tmp73.unpack = load i64, ptr %screen_size, align 4
  %153 = insertvalue %"Vec_2<T = u64>" poison, i64 %tmp73.unpack, 0
  %tmp73.elt614 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp73.unpack615 = load i64, ptr %tmp73.elt614, align 4
  %tmp73616 = insertvalue %"Vec_2<T = u64>" %153, i64 %tmp73.unpack615, 1
  %fun_call_tmp74 = call {} @"Particle:update"(ptr %fun_call_tmp71, %"Vec_2<T = u64>" %tmp73616)
  br label %cond68

cond68:                                           ; preds = %then67, %else49
  %fun_call_tmp70 = call i1 @"Vec<T = Particle>:has_next"(ptr nonnull %tmp_iter8)
  br i1 %fun_call_tmp70, label %then67, label %cond80

merge78:                                          ; preds = %cond80, %else90
  %game.key_presses121 = getelementptr inbounds nuw i8, ptr %game, i64 40
  %fun_call_tmp122 = call {} @"Particle:update.94"(ptr nonnull %player, ptr nonnull %game.key_presses121)
  %fun_call_tmp123 = call {} @"pixel:Game:fill"(ptr nonnull %game, i8 0)
  %fun_call_tmp125 = call { ptr, i64 } @"Vec<T = Particle>:deref_mut"(ptr nonnull %particles)
  %fun_call_tmp126 = call %"SliceMutIter<T = Particle>" @"[Particle]:iter_mut"({ ptr, i64 } %fun_call_tmp125)
  %fun_call_tmp126.elt = extractvalue %"SliceMutIter<T = Particle>" %fun_call_tmp126, 0
  store ptr %fun_call_tmp126.elt, ptr %tmp_iter11127, align 8
  %tmp_iter11127.repack612 = getelementptr inbounds nuw i8, ptr %tmp_iter11127, i64 8
  %fun_call_tmp126.elt613 = extractvalue %"SliceMutIter<T = Particle>" %fun_call_tmp126, 1
  store ptr %fun_call_tmp126.elt613, ptr %tmp_iter11127.repack612, align 8
  br label %cond129

then79:                                           ; preds = %cond80
  store i64 0, ptr %tmp_iter11, align 4
  %tmp_iter11.repack608 = getelementptr inbounds nuw i8, ptr %tmp_iter11, i64 8
  store i64 16, ptr %tmp_iter11.repack608, align 4
  br label %cond89

cond80:                                           ; preds = %cond68
  %game.tick = getelementptr inbounds nuw i8, ptr %game, i64 72
  %tmp82 = load i64, ptr %game.tick, align 4
  %tmp.i194 = urem i64 %tmp82, 3
  store i64 %tmp.i194, ptr %tmp984, align 4
  store i64 0, ptr %tmp10, align 4
  %tmp2.i211 = icmp eq i64 %tmp.i194, 0
  br i1 %tmp2.i211, label %then79, label %merge78

then88:                                           ; preds = %cond89
  %fun_call_tmp92 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter11)
  store i64 %fun_call_tmp92, ptr %x93, align 4
  %tmp_index.i = getelementptr [9 x i8], ptr %tile_map, i64 %fun_call_tmp92, i64 8
  %tmp103 = load i64, ptr %x93, align 4
  %game.tick104 = getelementptr inbounds nuw i8, ptr %game, i64 72
  %tmp105 = load i64, ptr %game.tick104, align 4
  %tmp.i = add i64 %tmp103, %tmp105
  %tmp.i191 = and i64 %tmp.i, 1
  store i64 %tmp.i191, ptr %tmp13, align 4
  store i64 0, ptr %tmp14, align 4
  %tmp2.i = icmp eq i64 %tmp.i191, 0
  %spec.select979 = select i1 %tmp2.i, i8 4, i8 7
  store i8 %spec.select979, ptr %tmp_index.i, align 1
  %tmp111 = load i64, ptr %tmp14, align 4
  %fun_call_tmp112 = call {} @"u64:drop"(i64 %tmp111)
  %tmp113 = load i64, ptr %tmp13, align 4
  %fun_call_tmp114 = call {} @"u64:drop"(i64 %tmp113)
  %tmp115 = load i64, ptr %x93, align 4
  %fun_call_tmp116 = call {} @"u64:drop"(i64 %tmp115)
  br label %cond89

cond89:                                           ; preds = %then88, %then79
  %fun_call_tmp91 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter11)
  br i1 %fun_call_tmp91, label %then88, label %else90

else90:                                           ; preds = %cond89
  %tmp118.unpack = load i64, ptr %tmp_iter11, align 4
  %tmp118.elt609 = getelementptr inbounds nuw i8, ptr %tmp_iter11, i64 8
  %tmp118.unpack610 = load i64, ptr %tmp118.elt609, align 4
  %fun_call_tmp.i248 = call {} @"u64:drop"(i64 %tmp118.unpack610)
  %fun_call_tmp2.i250 = call {} @"u64:drop"(i64 %tmp118.unpack)
  br label %merge78

then128:                                          ; preds = %cond129
  %fun_call_tmp132 = call ptr @"Vec<T = Particle>:next"(ptr nonnull %tmp_iter11127)
  %fun_call_tmp136 = call {} @"Particle:render"(ptr %fun_call_tmp132, ptr nonnull %game)
  br label %cond129

cond129:                                          ; preds = %then128, %merge78
  %fun_call_tmp131 = call i1 @"Vec<T = Particle>:has_next"(ptr nonnull %tmp_iter11127)
  br i1 %fun_call_tmp131, label %then128, label %else130

else130:                                          ; preds = %cond129
  %fun_call_tmp141 = call {} @"TileMap<W = \22i64 16\22, H = \22i64 9\22>:render"(ptr nonnull %tile_map, ptr nonnull %game)
  %fun_call_tmp143 = call {} @"Particle:render.121"(ptr nonnull %player, ptr nonnull %game)
  %fun_call_tmp144 = call {} @"pixel:Game:render"(ptr nonnull %game)
  %tmp147 = load i64, ptr %tmp10, align 4
  %fun_call_tmp148 = call {} @"u64:drop"(i64 %tmp147)
  %tmp149 = load i64, ptr %tmp984, align 4
  %fun_call_tmp150 = call {} @"u64:drop"(i64 %tmp149)
  br label %then42
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
  %fun_call_tmp8 = call ptr @"Vec<T = u8>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call i8 @"u8:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"Vec<T = u8>:push"(ptr nonnull %clone, i8 %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"Vec<T = u8>:has_next"(ptr nonnull %tmp_iter2)
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

define i1 @"Vec<T = u8>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"Vec<T = u8>:next"(ptr %0) {
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

define {} @"Vec<T = u8>:drop.17"(%"Vec<T = u8>" %0) {
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
  %fun_call_tmp8 = call ptr @"Vec<T = char>:next"(ptr nonnull %tmp_iter2)
  %fun_call_tmp10 = call i8 @"char:clone"(ptr %fun_call_tmp8)
  %fun_call_tmp11 = call {} @"Vec<T = char>:push"(ptr nonnull %clone, i8 %fun_call_tmp10)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp7 = call i1 @"Vec<T = char>:has_next"(ptr nonnull %tmp_iter2)
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

define i1 @"Vec<T = char>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"Vec<T = char>:next"(ptr %0) {
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

define {} @"Vec<T = char>:drop.35"(%"Vec<T = char>" %0) {
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
  %tick_speed = alloca i64, align 8
  %screen_size = alloca %"Vec_2<T = u64>", align 8
  %.elt = extractvalue %"Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack9 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt10 = extractvalue %"Vec_2<T = u64>" %0, 1
  store i64 %.elt10, ptr %screen_size.repack9, align 4
  store i64 %1, ptr %tick_speed, align 4
  %fun_call_tmp = call %"pixel:Frame" @"pixel:Game:new.45"(%"Vec_2<T = u64>" %0)
  %fun_call_tmp1 = call %"Vec<T = char>" @"Vec<T = char>:new"()
  %tmp_agg = insertvalue %"pixel:Game" undef, %"pixel:Frame" %fun_call_tmp, 0
  %tmp_agg3 = insertvalue %"pixel:Game" %tmp_agg, %"Vec<T = char>" %fun_call_tmp1, 1
  %tmp_agg4 = insertvalue %"pixel:Game" %tmp_agg3, i64 %1, 2
  %tmp_constructor = insertvalue %"pixel:Game" %tmp_agg4, i64 0, 3
  %tmp5 = load i64, ptr %tick_speed, align 4
  %fun_call_tmp6 = call {} @"u64:drop"(i64 %tmp5)
  %tmp7.unpack = load i64, ptr %screen_size, align 4
  %tmp7.elt14 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp7.unpack15 = load i64, ptr %tmp7.elt14, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp7.unpack15)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp7.unpack)
  ret %"pixel:Game" %tmp_constructor
}

define %"pixel:Frame" @"pixel:Game:new.45"(%"Vec_2<T = u64>" %0) {
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
  %screen_size.repack73 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt74 = extractvalue %"Vec_2<T = u64>" %0, 1
  store i64 %.elt74, ptr %screen_size.repack73, align 4
  %tmp.i48 = mul i64 %.elt, %.elt74
  %fun_call_tmp2 = call %"Vec<T = u8>" @"Vec<T = u8>:with_capacity"(i64 %tmp.i48)
  %fun_call_tmp2.elt = extractvalue %"Vec<T = u8>" %fun_call_tmp2, 0
  store ptr %fun_call_tmp2.elt, ptr %buffer, align 8
  %buffer.repack75 = getelementptr inbounds nuw i8, ptr %buffer, i64 8
  %fun_call_tmp2.elt76 = extractvalue %"Vec<T = u8>" %fun_call_tmp2, 1
  store i64 %fun_call_tmp2.elt76, ptr %buffer.repack75, align 8
  %buffer.repack77 = getelementptr inbounds nuw i8, ptr %buffer, i64 16
  %fun_call_tmp2.elt78 = extractvalue %"Vec<T = u8>" %fun_call_tmp2, 2
  store i64 %fun_call_tmp2.elt78, ptr %buffer.repack77, align 8
  %screen_size.y3 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp4 = load i64, ptr %screen_size.y3, align 4
  store i64 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack79 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  store i64 %tmp4, ptr %tmp_iter2.repack79, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp7 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter2)
  store i64 %fun_call_tmp7, ptr %y, align 4
  %tmp10 = load i64, ptr %screen_size, align 4
  store i64 0, ptr %tmp_iter4, align 4
  %tmp_iter4.repack95 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  store i64 %tmp10, ptr %tmp_iter4.repack95, align 4
  br label %cond13

cond:                                             ; preds = %else14, %entry
  %fun_call_tmp6 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp6, label %then, label %else

else:                                             ; preds = %cond
  %tmp40.unpack = load i64, ptr %screen_size, align 4
  %1 = insertvalue %"Vec_2<T = u64>" poison, i64 %tmp40.unpack, 0
  %tmp40.elt81 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp40.unpack82 = load i64, ptr %tmp40.elt81, align 4
  %tmp4083 = insertvalue %"Vec_2<T = u64>" %1, i64 %tmp40.unpack82, 1
  %tmp41.unpack = load ptr, ptr %buffer, align 8
  %2 = insertvalue %"Vec<T = u8>" poison, ptr %tmp41.unpack, 0
  %tmp41.elt84 = getelementptr inbounds nuw i8, ptr %buffer, i64 8
  %tmp41.unpack85 = load i64, ptr %tmp41.elt84, align 8
  %3 = insertvalue %"Vec<T = u8>" %2, i64 %tmp41.unpack85, 1
  %tmp41.elt86 = getelementptr inbounds nuw i8, ptr %buffer, i64 16
  %tmp41.unpack87 = load i64, ptr %tmp41.elt86, align 8
  %tmp4188 = insertvalue %"Vec<T = u8>" %3, i64 %tmp41.unpack87, 2
  %tmp_agg = insertvalue %"pixel:Frame" undef, %"Vec_2<T = u64>" %tmp4083, 0
  %tmp_constructor = insertvalue %"pixel:Frame" %tmp_agg, %"Vec<T = u8>" %tmp4188, 1
  %tmp42.unpack = load i64, ptr %tmp_iter2, align 4
  %tmp42.elt89 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %tmp42.unpack90 = load i64, ptr %tmp42.elt89, align 4
  %fun_call_tmp.i65 = call {} @"u64:drop"(i64 %tmp42.unpack90)
  %fun_call_tmp2.i67 = call {} @"u64:drop"(i64 %tmp42.unpack)
  %tmp44.unpack = load i64, ptr %screen_size, align 4
  %tmp44.elt92 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp44.unpack93 = load i64, ptr %tmp44.elt92, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp44.unpack93)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp44.unpack)
  ret %"pixel:Frame" %tmp_constructor

then12:                                           ; preds = %cond13
  %fun_call_tmp16 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter4)
  store i64 %fun_call_tmp16, ptr %x, align 4
  %tmp21 = load i64, ptr %x, align 4
  %tmp22 = load i64, ptr %y, align 4
  %tmp.i = add i64 %tmp21, %tmp22
  %tmp.i51 = and i64 %tmp.i, 1
  store i64 %tmp.i51, ptr %tmp6, align 4
  store i64 0, ptr %tmp7, align 4
  %tmp2.i = icmp eq i64 %tmp.i51, 0
  %spec.select = select i1 %tmp2.i, i8 0, i8 7
  %fun_call_tmp27 = call {} @"Vec<T = u8>:push"(ptr nonnull %buffer, i8 %spec.select)
  %tmp28 = load i64, ptr %tmp7, align 4
  %fun_call_tmp29 = call {} @"u64:drop"(i64 %tmp28)
  %tmp30 = load i64, ptr %tmp6, align 4
  %fun_call_tmp31 = call {} @"u64:drop"(i64 %tmp30)
  %tmp32 = load i64, ptr %x, align 4
  %fun_call_tmp33 = call {} @"u64:drop"(i64 %tmp32)
  br label %cond13

cond13:                                           ; preds = %then12, %then
  %fun_call_tmp15 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp15, label %then12, label %else14

else14:                                           ; preds = %cond13
  %tmp35.unpack = load i64, ptr %tmp_iter4, align 4
  %tmp35.elt97 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %tmp35.unpack98 = load i64, ptr %tmp35.elt97, align 4
  %fun_call_tmp.i70 = call {} @"u64:drop"(i64 %tmp35.unpack98)
  %fun_call_tmp2.i72 = call {} @"u64:drop"(i64 %tmp35.unpack)
  %tmp37 = load i64, ptr %y, align 4
  %fun_call_tmp38 = call {} @"u64:drop"(i64 %tmp37)
  br label %cond
}

define i1 @"Vec_2<T = u64>:has_next"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load i64, ptr %tmp2.end, align 4
  %tmp.i = icmp ult i64 %tmp1, %tmp3
  ret i1 %tmp.i
}

define i64 @"Vec_2<T = u64>:next"(ptr %0) {
entry:
  %tmp1 = load i64, ptr %0, align 4
  %tmp.i = add i64 %tmp1, 1
  store i64 %tmp.i, ptr %0, align 4
  %fun_call_tmp7 = call {} @"u64:drop"(i64 %tmp1)
  ret i64 %tmp1
}

define [16 x [9 x i8]] @"[[u8; 9]; 16]:clone"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %res = alloca [16 x [9 x i8]], align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 0, ptr %res, align 1
  %res.repack40 = getelementptr inbounds nuw i8, ptr %res, i64 1
  store i8 0, ptr %res.repack40, align 1
  %res.repack41 = getelementptr inbounds nuw i8, ptr %res, i64 2
  store i8 0, ptr %res.repack41, align 1
  %res.repack42 = getelementptr inbounds nuw i8, ptr %res, i64 3
  store i8 0, ptr %res.repack42, align 1
  %res.repack43 = getelementptr inbounds nuw i8, ptr %res, i64 4
  store i8 0, ptr %res.repack43, align 1
  %res.repack44 = getelementptr inbounds nuw i8, ptr %res, i64 5
  store i8 0, ptr %res.repack44, align 1
  %res.repack45 = getelementptr inbounds nuw i8, ptr %res, i64 6
  store i8 0, ptr %res.repack45, align 1
  %res.repack46 = getelementptr inbounds nuw i8, ptr %res, i64 7
  store i8 0, ptr %res.repack46, align 1
  %res.repack47 = getelementptr inbounds nuw i8, ptr %res, i64 8
  store i8 0, ptr %res.repack47, align 1
  %res.repack25 = getelementptr inbounds nuw i8, ptr %res, i64 9
  store i8 0, ptr %res.repack25, align 1
  %res.repack25.repack48 = getelementptr inbounds nuw i8, ptr %res, i64 10
  store i8 0, ptr %res.repack25.repack48, align 1
  %res.repack25.repack49 = getelementptr inbounds nuw i8, ptr %res, i64 11
  store i8 0, ptr %res.repack25.repack49, align 1
  %res.repack25.repack50 = getelementptr inbounds nuw i8, ptr %res, i64 12
  store i8 0, ptr %res.repack25.repack50, align 1
  %res.repack25.repack51 = getelementptr inbounds nuw i8, ptr %res, i64 13
  store i8 0, ptr %res.repack25.repack51, align 1
  %res.repack25.repack52 = getelementptr inbounds nuw i8, ptr %res, i64 14
  store i8 0, ptr %res.repack25.repack52, align 1
  %res.repack25.repack53 = getelementptr inbounds nuw i8, ptr %res, i64 15
  store i8 0, ptr %res.repack25.repack53, align 1
  %res.repack25.repack54 = getelementptr inbounds nuw i8, ptr %res, i64 16
  store i8 0, ptr %res.repack25.repack54, align 1
  %res.repack25.repack55 = getelementptr inbounds nuw i8, ptr %res, i64 17
  store i8 0, ptr %res.repack25.repack55, align 1
  %res.repack26 = getelementptr inbounds nuw i8, ptr %res, i64 18
  store i8 0, ptr %res.repack26, align 1
  %res.repack26.repack56 = getelementptr inbounds nuw i8, ptr %res, i64 19
  store i8 0, ptr %res.repack26.repack56, align 1
  %res.repack26.repack57 = getelementptr inbounds nuw i8, ptr %res, i64 20
  store i8 0, ptr %res.repack26.repack57, align 1
  %res.repack26.repack58 = getelementptr inbounds nuw i8, ptr %res, i64 21
  store i8 0, ptr %res.repack26.repack58, align 1
  %res.repack26.repack59 = getelementptr inbounds nuw i8, ptr %res, i64 22
  store i8 0, ptr %res.repack26.repack59, align 1
  %res.repack26.repack60 = getelementptr inbounds nuw i8, ptr %res, i64 23
  store i8 0, ptr %res.repack26.repack60, align 1
  %res.repack26.repack61 = getelementptr inbounds nuw i8, ptr %res, i64 24
  store i8 0, ptr %res.repack26.repack61, align 1
  %res.repack26.repack62 = getelementptr inbounds nuw i8, ptr %res, i64 25
  store i8 0, ptr %res.repack26.repack62, align 1
  %res.repack26.repack63 = getelementptr inbounds nuw i8, ptr %res, i64 26
  store i8 0, ptr %res.repack26.repack63, align 1
  %res.repack27 = getelementptr inbounds nuw i8, ptr %res, i64 27
  store i8 0, ptr %res.repack27, align 1
  %res.repack27.repack64 = getelementptr inbounds nuw i8, ptr %res, i64 28
  store i8 0, ptr %res.repack27.repack64, align 1
  %res.repack27.repack65 = getelementptr inbounds nuw i8, ptr %res, i64 29
  store i8 0, ptr %res.repack27.repack65, align 1
  %res.repack27.repack66 = getelementptr inbounds nuw i8, ptr %res, i64 30
  store i8 0, ptr %res.repack27.repack66, align 1
  %res.repack27.repack67 = getelementptr inbounds nuw i8, ptr %res, i64 31
  store i8 0, ptr %res.repack27.repack67, align 1
  %res.repack27.repack68 = getelementptr inbounds nuw i8, ptr %res, i64 32
  store i8 0, ptr %res.repack27.repack68, align 1
  %res.repack27.repack69 = getelementptr inbounds nuw i8, ptr %res, i64 33
  store i8 0, ptr %res.repack27.repack69, align 1
  %res.repack27.repack70 = getelementptr inbounds nuw i8, ptr %res, i64 34
  store i8 0, ptr %res.repack27.repack70, align 1
  %res.repack27.repack71 = getelementptr inbounds nuw i8, ptr %res, i64 35
  store i8 0, ptr %res.repack27.repack71, align 1
  %res.repack28 = getelementptr inbounds nuw i8, ptr %res, i64 36
  store i8 0, ptr %res.repack28, align 1
  %res.repack28.repack72 = getelementptr inbounds nuw i8, ptr %res, i64 37
  store i8 0, ptr %res.repack28.repack72, align 1
  %res.repack28.repack73 = getelementptr inbounds nuw i8, ptr %res, i64 38
  store i8 0, ptr %res.repack28.repack73, align 1
  %res.repack28.repack74 = getelementptr inbounds nuw i8, ptr %res, i64 39
  store i8 0, ptr %res.repack28.repack74, align 1
  %res.repack28.repack75 = getelementptr inbounds nuw i8, ptr %res, i64 40
  store i8 0, ptr %res.repack28.repack75, align 1
  %res.repack28.repack76 = getelementptr inbounds nuw i8, ptr %res, i64 41
  store i8 0, ptr %res.repack28.repack76, align 1
  %res.repack28.repack77 = getelementptr inbounds nuw i8, ptr %res, i64 42
  store i8 0, ptr %res.repack28.repack77, align 1
  %res.repack28.repack78 = getelementptr inbounds nuw i8, ptr %res, i64 43
  store i8 0, ptr %res.repack28.repack78, align 1
  %res.repack28.repack79 = getelementptr inbounds nuw i8, ptr %res, i64 44
  store i8 0, ptr %res.repack28.repack79, align 1
  %res.repack29 = getelementptr inbounds nuw i8, ptr %res, i64 45
  store i8 0, ptr %res.repack29, align 1
  %res.repack29.repack80 = getelementptr inbounds nuw i8, ptr %res, i64 46
  store i8 0, ptr %res.repack29.repack80, align 1
  %res.repack29.repack81 = getelementptr inbounds nuw i8, ptr %res, i64 47
  store i8 0, ptr %res.repack29.repack81, align 1
  %res.repack29.repack82 = getelementptr inbounds nuw i8, ptr %res, i64 48
  store i8 0, ptr %res.repack29.repack82, align 1
  %res.repack29.repack83 = getelementptr inbounds nuw i8, ptr %res, i64 49
  store i8 0, ptr %res.repack29.repack83, align 1
  %res.repack29.repack84 = getelementptr inbounds nuw i8, ptr %res, i64 50
  store i8 0, ptr %res.repack29.repack84, align 1
  %res.repack29.repack85 = getelementptr inbounds nuw i8, ptr %res, i64 51
  store i8 0, ptr %res.repack29.repack85, align 1
  %res.repack29.repack86 = getelementptr inbounds nuw i8, ptr %res, i64 52
  store i8 0, ptr %res.repack29.repack86, align 1
  %res.repack29.repack87 = getelementptr inbounds nuw i8, ptr %res, i64 53
  store i8 0, ptr %res.repack29.repack87, align 1
  %res.repack30 = getelementptr inbounds nuw i8, ptr %res, i64 54
  store i8 0, ptr %res.repack30, align 1
  %res.repack30.repack88 = getelementptr inbounds nuw i8, ptr %res, i64 55
  store i8 0, ptr %res.repack30.repack88, align 1
  %res.repack30.repack89 = getelementptr inbounds nuw i8, ptr %res, i64 56
  store i8 0, ptr %res.repack30.repack89, align 1
  %res.repack30.repack90 = getelementptr inbounds nuw i8, ptr %res, i64 57
  store i8 0, ptr %res.repack30.repack90, align 1
  %res.repack30.repack91 = getelementptr inbounds nuw i8, ptr %res, i64 58
  store i8 0, ptr %res.repack30.repack91, align 1
  %res.repack30.repack92 = getelementptr inbounds nuw i8, ptr %res, i64 59
  store i8 0, ptr %res.repack30.repack92, align 1
  %res.repack30.repack93 = getelementptr inbounds nuw i8, ptr %res, i64 60
  store i8 0, ptr %res.repack30.repack93, align 1
  %res.repack30.repack94 = getelementptr inbounds nuw i8, ptr %res, i64 61
  store i8 0, ptr %res.repack30.repack94, align 1
  %res.repack30.repack95 = getelementptr inbounds nuw i8, ptr %res, i64 62
  store i8 0, ptr %res.repack30.repack95, align 1
  %res.repack31 = getelementptr inbounds nuw i8, ptr %res, i64 63
  store i8 0, ptr %res.repack31, align 1
  %res.repack31.repack96 = getelementptr inbounds nuw i8, ptr %res, i64 64
  store i8 0, ptr %res.repack31.repack96, align 1
  %res.repack31.repack97 = getelementptr inbounds nuw i8, ptr %res, i64 65
  store i8 0, ptr %res.repack31.repack97, align 1
  %res.repack31.repack98 = getelementptr inbounds nuw i8, ptr %res, i64 66
  store i8 0, ptr %res.repack31.repack98, align 1
  %res.repack31.repack99 = getelementptr inbounds nuw i8, ptr %res, i64 67
  store i8 0, ptr %res.repack31.repack99, align 1
  %res.repack31.repack100 = getelementptr inbounds nuw i8, ptr %res, i64 68
  store i8 0, ptr %res.repack31.repack100, align 1
  %res.repack31.repack101 = getelementptr inbounds nuw i8, ptr %res, i64 69
  store i8 0, ptr %res.repack31.repack101, align 1
  %res.repack31.repack102 = getelementptr inbounds nuw i8, ptr %res, i64 70
  store i8 0, ptr %res.repack31.repack102, align 1
  %res.repack31.repack103 = getelementptr inbounds nuw i8, ptr %res, i64 71
  store i8 0, ptr %res.repack31.repack103, align 1
  %res.repack32 = getelementptr inbounds nuw i8, ptr %res, i64 72
  store i8 0, ptr %res.repack32, align 1
  %res.repack32.repack104 = getelementptr inbounds nuw i8, ptr %res, i64 73
  store i8 0, ptr %res.repack32.repack104, align 1
  %res.repack32.repack105 = getelementptr inbounds nuw i8, ptr %res, i64 74
  store i8 0, ptr %res.repack32.repack105, align 1
  %res.repack32.repack106 = getelementptr inbounds nuw i8, ptr %res, i64 75
  store i8 0, ptr %res.repack32.repack106, align 1
  %res.repack32.repack107 = getelementptr inbounds nuw i8, ptr %res, i64 76
  store i8 0, ptr %res.repack32.repack107, align 1
  %res.repack32.repack108 = getelementptr inbounds nuw i8, ptr %res, i64 77
  store i8 0, ptr %res.repack32.repack108, align 1
  %res.repack32.repack109 = getelementptr inbounds nuw i8, ptr %res, i64 78
  store i8 0, ptr %res.repack32.repack109, align 1
  %res.repack32.repack110 = getelementptr inbounds nuw i8, ptr %res, i64 79
  store i8 0, ptr %res.repack32.repack110, align 1
  %res.repack32.repack111 = getelementptr inbounds nuw i8, ptr %res, i64 80
  store i8 0, ptr %res.repack32.repack111, align 1
  %res.repack33 = getelementptr inbounds nuw i8, ptr %res, i64 81
  store i8 0, ptr %res.repack33, align 1
  %res.repack33.repack112 = getelementptr inbounds nuw i8, ptr %res, i64 82
  store i8 0, ptr %res.repack33.repack112, align 1
  %res.repack33.repack113 = getelementptr inbounds nuw i8, ptr %res, i64 83
  store i8 0, ptr %res.repack33.repack113, align 1
  %res.repack33.repack114 = getelementptr inbounds nuw i8, ptr %res, i64 84
  store i8 0, ptr %res.repack33.repack114, align 1
  %res.repack33.repack115 = getelementptr inbounds nuw i8, ptr %res, i64 85
  store i8 0, ptr %res.repack33.repack115, align 1
  %res.repack33.repack116 = getelementptr inbounds nuw i8, ptr %res, i64 86
  store i8 0, ptr %res.repack33.repack116, align 1
  %res.repack33.repack117 = getelementptr inbounds nuw i8, ptr %res, i64 87
  store i8 0, ptr %res.repack33.repack117, align 1
  %res.repack33.repack118 = getelementptr inbounds nuw i8, ptr %res, i64 88
  store i8 0, ptr %res.repack33.repack118, align 1
  %res.repack33.repack119 = getelementptr inbounds nuw i8, ptr %res, i64 89
  store i8 0, ptr %res.repack33.repack119, align 1
  %res.repack34 = getelementptr inbounds nuw i8, ptr %res, i64 90
  store i8 0, ptr %res.repack34, align 1
  %res.repack34.repack120 = getelementptr inbounds nuw i8, ptr %res, i64 91
  store i8 0, ptr %res.repack34.repack120, align 1
  %res.repack34.repack121 = getelementptr inbounds nuw i8, ptr %res, i64 92
  store i8 0, ptr %res.repack34.repack121, align 1
  %res.repack34.repack122 = getelementptr inbounds nuw i8, ptr %res, i64 93
  store i8 0, ptr %res.repack34.repack122, align 1
  %res.repack34.repack123 = getelementptr inbounds nuw i8, ptr %res, i64 94
  store i8 0, ptr %res.repack34.repack123, align 1
  %res.repack34.repack124 = getelementptr inbounds nuw i8, ptr %res, i64 95
  store i8 0, ptr %res.repack34.repack124, align 1
  %res.repack34.repack125 = getelementptr inbounds nuw i8, ptr %res, i64 96
  store i8 0, ptr %res.repack34.repack125, align 1
  %res.repack34.repack126 = getelementptr inbounds nuw i8, ptr %res, i64 97
  store i8 0, ptr %res.repack34.repack126, align 1
  %res.repack34.repack127 = getelementptr inbounds nuw i8, ptr %res, i64 98
  store i8 0, ptr %res.repack34.repack127, align 1
  %res.repack35 = getelementptr inbounds nuw i8, ptr %res, i64 99
  store i8 0, ptr %res.repack35, align 1
  %res.repack35.repack128 = getelementptr inbounds nuw i8, ptr %res, i64 100
  store i8 0, ptr %res.repack35.repack128, align 1
  %res.repack35.repack129 = getelementptr inbounds nuw i8, ptr %res, i64 101
  store i8 0, ptr %res.repack35.repack129, align 1
  %res.repack35.repack130 = getelementptr inbounds nuw i8, ptr %res, i64 102
  store i8 0, ptr %res.repack35.repack130, align 1
  %res.repack35.repack131 = getelementptr inbounds nuw i8, ptr %res, i64 103
  store i8 0, ptr %res.repack35.repack131, align 1
  %res.repack35.repack132 = getelementptr inbounds nuw i8, ptr %res, i64 104
  store i8 0, ptr %res.repack35.repack132, align 1
  %res.repack35.repack133 = getelementptr inbounds nuw i8, ptr %res, i64 105
  store i8 0, ptr %res.repack35.repack133, align 1
  %res.repack35.repack134 = getelementptr inbounds nuw i8, ptr %res, i64 106
  store i8 0, ptr %res.repack35.repack134, align 1
  %res.repack35.repack135 = getelementptr inbounds nuw i8, ptr %res, i64 107
  store i8 0, ptr %res.repack35.repack135, align 1
  %res.repack36 = getelementptr inbounds nuw i8, ptr %res, i64 108
  store i8 0, ptr %res.repack36, align 1
  %res.repack36.repack136 = getelementptr inbounds nuw i8, ptr %res, i64 109
  store i8 0, ptr %res.repack36.repack136, align 1
  %res.repack36.repack137 = getelementptr inbounds nuw i8, ptr %res, i64 110
  store i8 0, ptr %res.repack36.repack137, align 1
  %res.repack36.repack138 = getelementptr inbounds nuw i8, ptr %res, i64 111
  store i8 0, ptr %res.repack36.repack138, align 1
  %res.repack36.repack139 = getelementptr inbounds nuw i8, ptr %res, i64 112
  store i8 0, ptr %res.repack36.repack139, align 1
  %res.repack36.repack140 = getelementptr inbounds nuw i8, ptr %res, i64 113
  store i8 0, ptr %res.repack36.repack140, align 1
  %res.repack36.repack141 = getelementptr inbounds nuw i8, ptr %res, i64 114
  store i8 0, ptr %res.repack36.repack141, align 1
  %res.repack36.repack142 = getelementptr inbounds nuw i8, ptr %res, i64 115
  store i8 0, ptr %res.repack36.repack142, align 1
  %res.repack36.repack143 = getelementptr inbounds nuw i8, ptr %res, i64 116
  store i8 0, ptr %res.repack36.repack143, align 1
  %res.repack37 = getelementptr inbounds nuw i8, ptr %res, i64 117
  store i8 0, ptr %res.repack37, align 1
  %res.repack37.repack144 = getelementptr inbounds nuw i8, ptr %res, i64 118
  store i8 0, ptr %res.repack37.repack144, align 1
  %res.repack37.repack145 = getelementptr inbounds nuw i8, ptr %res, i64 119
  store i8 0, ptr %res.repack37.repack145, align 1
  %res.repack37.repack146 = getelementptr inbounds nuw i8, ptr %res, i64 120
  store i8 0, ptr %res.repack37.repack146, align 1
  %res.repack37.repack147 = getelementptr inbounds nuw i8, ptr %res, i64 121
  store i8 0, ptr %res.repack37.repack147, align 1
  %res.repack37.repack148 = getelementptr inbounds nuw i8, ptr %res, i64 122
  store i8 0, ptr %res.repack37.repack148, align 1
  %res.repack37.repack149 = getelementptr inbounds nuw i8, ptr %res, i64 123
  store i8 0, ptr %res.repack37.repack149, align 1
  %res.repack37.repack150 = getelementptr inbounds nuw i8, ptr %res, i64 124
  store i8 0, ptr %res.repack37.repack150, align 1
  %res.repack37.repack151 = getelementptr inbounds nuw i8, ptr %res, i64 125
  store i8 0, ptr %res.repack37.repack151, align 1
  %res.repack38 = getelementptr inbounds nuw i8, ptr %res, i64 126
  store i8 0, ptr %res.repack38, align 1
  %res.repack38.repack152 = getelementptr inbounds nuw i8, ptr %res, i64 127
  store i8 0, ptr %res.repack38.repack152, align 1
  %res.repack38.repack153 = getelementptr inbounds nuw i8, ptr %res, i64 128
  store i8 0, ptr %res.repack38.repack153, align 1
  %res.repack38.repack154 = getelementptr inbounds nuw i8, ptr %res, i64 129
  store i8 0, ptr %res.repack38.repack154, align 1
  %res.repack38.repack155 = getelementptr inbounds nuw i8, ptr %res, i64 130
  store i8 0, ptr %res.repack38.repack155, align 1
  %res.repack38.repack156 = getelementptr inbounds nuw i8, ptr %res, i64 131
  store i8 0, ptr %res.repack38.repack156, align 1
  %res.repack38.repack157 = getelementptr inbounds nuw i8, ptr %res, i64 132
  store i8 0, ptr %res.repack38.repack157, align 1
  %res.repack38.repack158 = getelementptr inbounds nuw i8, ptr %res, i64 133
  store i8 0, ptr %res.repack38.repack158, align 1
  %res.repack38.repack159 = getelementptr inbounds nuw i8, ptr %res, i64 134
  store i8 0, ptr %res.repack38.repack159, align 1
  %res.repack39 = getelementptr inbounds nuw i8, ptr %res, i64 135
  store i8 0, ptr %res.repack39, align 1
  %res.repack39.repack160 = getelementptr inbounds nuw i8, ptr %res, i64 136
  store i8 0, ptr %res.repack39.repack160, align 1
  %res.repack39.repack161 = getelementptr inbounds nuw i8, ptr %res, i64 137
  store i8 0, ptr %res.repack39.repack161, align 1
  %res.repack39.repack162 = getelementptr inbounds nuw i8, ptr %res, i64 138
  store i8 0, ptr %res.repack39.repack162, align 1
  %res.repack39.repack163 = getelementptr inbounds nuw i8, ptr %res, i64 139
  store i8 0, ptr %res.repack39.repack163, align 1
  %res.repack39.repack164 = getelementptr inbounds nuw i8, ptr %res, i64 140
  store i8 0, ptr %res.repack39.repack164, align 1
  %res.repack39.repack165 = getelementptr inbounds nuw i8, ptr %res, i64 141
  store i8 0, ptr %res.repack39.repack165, align 1
  %res.repack39.repack166 = getelementptr inbounds nuw i8, ptr %res, i64 142
  store i8 0, ptr %res.repack39.repack166, align 1
  %res.repack39.repack167 = getelementptr inbounds nuw i8, ptr %res, i64 143
  store i8 0, ptr %res.repack39.repack167, align 1
  br label %cond

then:                                             ; preds = %cond
  %tmp2 = load i64, ptr %idx, align 4
  %tmp_index.i23 = getelementptr [9 x i8], ptr %res, i64 %tmp2
  %tmp4 = load ptr, ptr %self, align 8
  %tmp_index.i = getelementptr [9 x i8], ptr %tmp4, i64 %tmp2
  %fun_call_tmp7 = call [9 x i8] @"[u8; 9]:clone"(ptr %tmp_index.i)
  %fun_call_tmp7.elt = extractvalue [9 x i8] %fun_call_tmp7, 0
  store i8 %fun_call_tmp7.elt, ptr %tmp_index.i23, align 1
  %tmp_index.i23.repack774 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 1
  %fun_call_tmp7.elt775 = extractvalue [9 x i8] %fun_call_tmp7, 1
  store i8 %fun_call_tmp7.elt775, ptr %tmp_index.i23.repack774, align 1
  %tmp_index.i23.repack776 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 2
  %fun_call_tmp7.elt777 = extractvalue [9 x i8] %fun_call_tmp7, 2
  store i8 %fun_call_tmp7.elt777, ptr %tmp_index.i23.repack776, align 1
  %tmp_index.i23.repack778 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 3
  %fun_call_tmp7.elt779 = extractvalue [9 x i8] %fun_call_tmp7, 3
  store i8 %fun_call_tmp7.elt779, ptr %tmp_index.i23.repack778, align 1
  %tmp_index.i23.repack780 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 4
  %fun_call_tmp7.elt781 = extractvalue [9 x i8] %fun_call_tmp7, 4
  store i8 %fun_call_tmp7.elt781, ptr %tmp_index.i23.repack780, align 1
  %tmp_index.i23.repack782 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 5
  %fun_call_tmp7.elt783 = extractvalue [9 x i8] %fun_call_tmp7, 5
  store i8 %fun_call_tmp7.elt783, ptr %tmp_index.i23.repack782, align 1
  %tmp_index.i23.repack784 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 6
  %fun_call_tmp7.elt785 = extractvalue [9 x i8] %fun_call_tmp7, 6
  store i8 %fun_call_tmp7.elt785, ptr %tmp_index.i23.repack784, align 1
  %tmp_index.i23.repack786 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 7
  %fun_call_tmp7.elt787 = extractvalue [9 x i8] %fun_call_tmp7, 7
  store i8 %fun_call_tmp7.elt787, ptr %tmp_index.i23.repack786, align 1
  %tmp_index.i23.repack788 = getelementptr inbounds nuw i8, ptr %tmp_index.i23, i64 8
  %fun_call_tmp7.elt789 = extractvalue [9 x i8] %fun_call_tmp7, 8
  store i8 %fun_call_tmp7.elt789, ptr %tmp_index.i23.repack788, align 1
  %tmp8 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp8, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp.i19 = icmp ult i64 %storemerge, 16
  br i1 %tmp.i19, label %then, label %else

else:                                             ; preds = %cond
  %tmp10.unpack.unpack = load i8, ptr %res, align 1
  %1 = insertvalue [9 x i8] poison, i8 %tmp10.unpack.unpack, 0
  %tmp10.unpack.elt199 = getelementptr inbounds nuw i8, ptr %res, i64 1
  %tmp10.unpack.unpack200 = load i8, ptr %tmp10.unpack.elt199, align 1
  %2 = insertvalue [9 x i8] %1, i8 %tmp10.unpack.unpack200, 1
  %tmp10.unpack.elt201 = getelementptr inbounds nuw i8, ptr %res, i64 2
  %tmp10.unpack.unpack202 = load i8, ptr %tmp10.unpack.elt201, align 1
  %3 = insertvalue [9 x i8] %2, i8 %tmp10.unpack.unpack202, 2
  %tmp10.unpack.elt203 = getelementptr inbounds nuw i8, ptr %res, i64 3
  %tmp10.unpack.unpack204 = load i8, ptr %tmp10.unpack.elt203, align 1
  %4 = insertvalue [9 x i8] %3, i8 %tmp10.unpack.unpack204, 3
  %tmp10.unpack.elt205 = getelementptr inbounds nuw i8, ptr %res, i64 4
  %tmp10.unpack.unpack206 = load i8, ptr %tmp10.unpack.elt205, align 1
  %5 = insertvalue [9 x i8] %4, i8 %tmp10.unpack.unpack206, 4
  %tmp10.unpack.elt207 = getelementptr inbounds nuw i8, ptr %res, i64 5
  %tmp10.unpack.unpack208 = load i8, ptr %tmp10.unpack.elt207, align 1
  %6 = insertvalue [9 x i8] %5, i8 %tmp10.unpack.unpack208, 5
  %tmp10.unpack.elt209 = getelementptr inbounds nuw i8, ptr %res, i64 6
  %tmp10.unpack.unpack210 = load i8, ptr %tmp10.unpack.elt209, align 1
  %7 = insertvalue [9 x i8] %6, i8 %tmp10.unpack.unpack210, 6
  %tmp10.unpack.elt211 = getelementptr inbounds nuw i8, ptr %res, i64 7
  %tmp10.unpack.unpack212 = load i8, ptr %tmp10.unpack.elt211, align 1
  %8 = insertvalue [9 x i8] %7, i8 %tmp10.unpack.unpack212, 7
  %tmp10.unpack.elt213 = getelementptr inbounds nuw i8, ptr %res, i64 8
  %tmp10.unpack.unpack214 = load i8, ptr %tmp10.unpack.elt213, align 1
  %tmp10.unpack215 = insertvalue [9 x i8] %8, i8 %tmp10.unpack.unpack214, 8
  %9 = insertvalue [16 x [9 x i8]] poison, [9 x i8] %tmp10.unpack215, 0
  %tmp10.elt168 = getelementptr inbounds nuw i8, ptr %res, i64 9
  %tmp10.unpack169.unpack = load i8, ptr %tmp10.elt168, align 1
  %10 = insertvalue [9 x i8] poison, i8 %tmp10.unpack169.unpack, 0
  %tmp10.unpack169.elt216 = getelementptr inbounds nuw i8, ptr %res, i64 10
  %tmp10.unpack169.unpack217 = load i8, ptr %tmp10.unpack169.elt216, align 1
  %11 = insertvalue [9 x i8] %10, i8 %tmp10.unpack169.unpack217, 1
  %tmp10.unpack169.elt218 = getelementptr inbounds nuw i8, ptr %res, i64 11
  %tmp10.unpack169.unpack219 = load i8, ptr %tmp10.unpack169.elt218, align 1
  %12 = insertvalue [9 x i8] %11, i8 %tmp10.unpack169.unpack219, 2
  %tmp10.unpack169.elt220 = getelementptr inbounds nuw i8, ptr %res, i64 12
  %tmp10.unpack169.unpack221 = load i8, ptr %tmp10.unpack169.elt220, align 1
  %13 = insertvalue [9 x i8] %12, i8 %tmp10.unpack169.unpack221, 3
  %tmp10.unpack169.elt222 = getelementptr inbounds nuw i8, ptr %res, i64 13
  %tmp10.unpack169.unpack223 = load i8, ptr %tmp10.unpack169.elt222, align 1
  %14 = insertvalue [9 x i8] %13, i8 %tmp10.unpack169.unpack223, 4
  %tmp10.unpack169.elt224 = getelementptr inbounds nuw i8, ptr %res, i64 14
  %tmp10.unpack169.unpack225 = load i8, ptr %tmp10.unpack169.elt224, align 1
  %15 = insertvalue [9 x i8] %14, i8 %tmp10.unpack169.unpack225, 5
  %tmp10.unpack169.elt226 = getelementptr inbounds nuw i8, ptr %res, i64 15
  %tmp10.unpack169.unpack227 = load i8, ptr %tmp10.unpack169.elt226, align 1
  %16 = insertvalue [9 x i8] %15, i8 %tmp10.unpack169.unpack227, 6
  %tmp10.unpack169.elt228 = getelementptr inbounds nuw i8, ptr %res, i64 16
  %tmp10.unpack169.unpack229 = load i8, ptr %tmp10.unpack169.elt228, align 1
  %17 = insertvalue [9 x i8] %16, i8 %tmp10.unpack169.unpack229, 7
  %tmp10.unpack169.elt230 = getelementptr inbounds nuw i8, ptr %res, i64 17
  %tmp10.unpack169.unpack231 = load i8, ptr %tmp10.unpack169.elt230, align 1
  %tmp10.unpack169232 = insertvalue [9 x i8] %17, i8 %tmp10.unpack169.unpack231, 8
  %18 = insertvalue [16 x [9 x i8]] %9, [9 x i8] %tmp10.unpack169232, 1
  %tmp10.elt170 = getelementptr inbounds nuw i8, ptr %res, i64 18
  %tmp10.unpack171.unpack = load i8, ptr %tmp10.elt170, align 1
  %19 = insertvalue [9 x i8] poison, i8 %tmp10.unpack171.unpack, 0
  %tmp10.unpack171.elt233 = getelementptr inbounds nuw i8, ptr %res, i64 19
  %tmp10.unpack171.unpack234 = load i8, ptr %tmp10.unpack171.elt233, align 1
  %20 = insertvalue [9 x i8] %19, i8 %tmp10.unpack171.unpack234, 1
  %tmp10.unpack171.elt235 = getelementptr inbounds nuw i8, ptr %res, i64 20
  %tmp10.unpack171.unpack236 = load i8, ptr %tmp10.unpack171.elt235, align 1
  %21 = insertvalue [9 x i8] %20, i8 %tmp10.unpack171.unpack236, 2
  %tmp10.unpack171.elt237 = getelementptr inbounds nuw i8, ptr %res, i64 21
  %tmp10.unpack171.unpack238 = load i8, ptr %tmp10.unpack171.elt237, align 1
  %22 = insertvalue [9 x i8] %21, i8 %tmp10.unpack171.unpack238, 3
  %tmp10.unpack171.elt239 = getelementptr inbounds nuw i8, ptr %res, i64 22
  %tmp10.unpack171.unpack240 = load i8, ptr %tmp10.unpack171.elt239, align 1
  %23 = insertvalue [9 x i8] %22, i8 %tmp10.unpack171.unpack240, 4
  %tmp10.unpack171.elt241 = getelementptr inbounds nuw i8, ptr %res, i64 23
  %tmp10.unpack171.unpack242 = load i8, ptr %tmp10.unpack171.elt241, align 1
  %24 = insertvalue [9 x i8] %23, i8 %tmp10.unpack171.unpack242, 5
  %tmp10.unpack171.elt243 = getelementptr inbounds nuw i8, ptr %res, i64 24
  %tmp10.unpack171.unpack244 = load i8, ptr %tmp10.unpack171.elt243, align 1
  %25 = insertvalue [9 x i8] %24, i8 %tmp10.unpack171.unpack244, 6
  %tmp10.unpack171.elt245 = getelementptr inbounds nuw i8, ptr %res, i64 25
  %tmp10.unpack171.unpack246 = load i8, ptr %tmp10.unpack171.elt245, align 1
  %26 = insertvalue [9 x i8] %25, i8 %tmp10.unpack171.unpack246, 7
  %tmp10.unpack171.elt247 = getelementptr inbounds nuw i8, ptr %res, i64 26
  %tmp10.unpack171.unpack248 = load i8, ptr %tmp10.unpack171.elt247, align 1
  %tmp10.unpack171249 = insertvalue [9 x i8] %26, i8 %tmp10.unpack171.unpack248, 8
  %27 = insertvalue [16 x [9 x i8]] %18, [9 x i8] %tmp10.unpack171249, 2
  %tmp10.elt172 = getelementptr inbounds nuw i8, ptr %res, i64 27
  %tmp10.unpack173.unpack = load i8, ptr %tmp10.elt172, align 1
  %28 = insertvalue [9 x i8] poison, i8 %tmp10.unpack173.unpack, 0
  %tmp10.unpack173.elt250 = getelementptr inbounds nuw i8, ptr %res, i64 28
  %tmp10.unpack173.unpack251 = load i8, ptr %tmp10.unpack173.elt250, align 1
  %29 = insertvalue [9 x i8] %28, i8 %tmp10.unpack173.unpack251, 1
  %tmp10.unpack173.elt252 = getelementptr inbounds nuw i8, ptr %res, i64 29
  %tmp10.unpack173.unpack253 = load i8, ptr %tmp10.unpack173.elt252, align 1
  %30 = insertvalue [9 x i8] %29, i8 %tmp10.unpack173.unpack253, 2
  %tmp10.unpack173.elt254 = getelementptr inbounds nuw i8, ptr %res, i64 30
  %tmp10.unpack173.unpack255 = load i8, ptr %tmp10.unpack173.elt254, align 1
  %31 = insertvalue [9 x i8] %30, i8 %tmp10.unpack173.unpack255, 3
  %tmp10.unpack173.elt256 = getelementptr inbounds nuw i8, ptr %res, i64 31
  %tmp10.unpack173.unpack257 = load i8, ptr %tmp10.unpack173.elt256, align 1
  %32 = insertvalue [9 x i8] %31, i8 %tmp10.unpack173.unpack257, 4
  %tmp10.unpack173.elt258 = getelementptr inbounds nuw i8, ptr %res, i64 32
  %tmp10.unpack173.unpack259 = load i8, ptr %tmp10.unpack173.elt258, align 1
  %33 = insertvalue [9 x i8] %32, i8 %tmp10.unpack173.unpack259, 5
  %tmp10.unpack173.elt260 = getelementptr inbounds nuw i8, ptr %res, i64 33
  %tmp10.unpack173.unpack261 = load i8, ptr %tmp10.unpack173.elt260, align 1
  %34 = insertvalue [9 x i8] %33, i8 %tmp10.unpack173.unpack261, 6
  %tmp10.unpack173.elt262 = getelementptr inbounds nuw i8, ptr %res, i64 34
  %tmp10.unpack173.unpack263 = load i8, ptr %tmp10.unpack173.elt262, align 1
  %35 = insertvalue [9 x i8] %34, i8 %tmp10.unpack173.unpack263, 7
  %tmp10.unpack173.elt264 = getelementptr inbounds nuw i8, ptr %res, i64 35
  %tmp10.unpack173.unpack265 = load i8, ptr %tmp10.unpack173.elt264, align 1
  %tmp10.unpack173266 = insertvalue [9 x i8] %35, i8 %tmp10.unpack173.unpack265, 8
  %36 = insertvalue [16 x [9 x i8]] %27, [9 x i8] %tmp10.unpack173266, 3
  %tmp10.elt174 = getelementptr inbounds nuw i8, ptr %res, i64 36
  %tmp10.unpack175.unpack = load i8, ptr %tmp10.elt174, align 1
  %37 = insertvalue [9 x i8] poison, i8 %tmp10.unpack175.unpack, 0
  %tmp10.unpack175.elt267 = getelementptr inbounds nuw i8, ptr %res, i64 37
  %tmp10.unpack175.unpack268 = load i8, ptr %tmp10.unpack175.elt267, align 1
  %38 = insertvalue [9 x i8] %37, i8 %tmp10.unpack175.unpack268, 1
  %tmp10.unpack175.elt269 = getelementptr inbounds nuw i8, ptr %res, i64 38
  %tmp10.unpack175.unpack270 = load i8, ptr %tmp10.unpack175.elt269, align 1
  %39 = insertvalue [9 x i8] %38, i8 %tmp10.unpack175.unpack270, 2
  %tmp10.unpack175.elt271 = getelementptr inbounds nuw i8, ptr %res, i64 39
  %tmp10.unpack175.unpack272 = load i8, ptr %tmp10.unpack175.elt271, align 1
  %40 = insertvalue [9 x i8] %39, i8 %tmp10.unpack175.unpack272, 3
  %tmp10.unpack175.elt273 = getelementptr inbounds nuw i8, ptr %res, i64 40
  %tmp10.unpack175.unpack274 = load i8, ptr %tmp10.unpack175.elt273, align 1
  %41 = insertvalue [9 x i8] %40, i8 %tmp10.unpack175.unpack274, 4
  %tmp10.unpack175.elt275 = getelementptr inbounds nuw i8, ptr %res, i64 41
  %tmp10.unpack175.unpack276 = load i8, ptr %tmp10.unpack175.elt275, align 1
  %42 = insertvalue [9 x i8] %41, i8 %tmp10.unpack175.unpack276, 5
  %tmp10.unpack175.elt277 = getelementptr inbounds nuw i8, ptr %res, i64 42
  %tmp10.unpack175.unpack278 = load i8, ptr %tmp10.unpack175.elt277, align 1
  %43 = insertvalue [9 x i8] %42, i8 %tmp10.unpack175.unpack278, 6
  %tmp10.unpack175.elt279 = getelementptr inbounds nuw i8, ptr %res, i64 43
  %tmp10.unpack175.unpack280 = load i8, ptr %tmp10.unpack175.elt279, align 1
  %44 = insertvalue [9 x i8] %43, i8 %tmp10.unpack175.unpack280, 7
  %tmp10.unpack175.elt281 = getelementptr inbounds nuw i8, ptr %res, i64 44
  %tmp10.unpack175.unpack282 = load i8, ptr %tmp10.unpack175.elt281, align 1
  %tmp10.unpack175283 = insertvalue [9 x i8] %44, i8 %tmp10.unpack175.unpack282, 8
  %45 = insertvalue [16 x [9 x i8]] %36, [9 x i8] %tmp10.unpack175283, 4
  %tmp10.elt176 = getelementptr inbounds nuw i8, ptr %res, i64 45
  %tmp10.unpack177.unpack = load i8, ptr %tmp10.elt176, align 1
  %46 = insertvalue [9 x i8] poison, i8 %tmp10.unpack177.unpack, 0
  %tmp10.unpack177.elt284 = getelementptr inbounds nuw i8, ptr %res, i64 46
  %tmp10.unpack177.unpack285 = load i8, ptr %tmp10.unpack177.elt284, align 1
  %47 = insertvalue [9 x i8] %46, i8 %tmp10.unpack177.unpack285, 1
  %tmp10.unpack177.elt286 = getelementptr inbounds nuw i8, ptr %res, i64 47
  %tmp10.unpack177.unpack287 = load i8, ptr %tmp10.unpack177.elt286, align 1
  %48 = insertvalue [9 x i8] %47, i8 %tmp10.unpack177.unpack287, 2
  %tmp10.unpack177.elt288 = getelementptr inbounds nuw i8, ptr %res, i64 48
  %tmp10.unpack177.unpack289 = load i8, ptr %tmp10.unpack177.elt288, align 1
  %49 = insertvalue [9 x i8] %48, i8 %tmp10.unpack177.unpack289, 3
  %tmp10.unpack177.elt290 = getelementptr inbounds nuw i8, ptr %res, i64 49
  %tmp10.unpack177.unpack291 = load i8, ptr %tmp10.unpack177.elt290, align 1
  %50 = insertvalue [9 x i8] %49, i8 %tmp10.unpack177.unpack291, 4
  %tmp10.unpack177.elt292 = getelementptr inbounds nuw i8, ptr %res, i64 50
  %tmp10.unpack177.unpack293 = load i8, ptr %tmp10.unpack177.elt292, align 1
  %51 = insertvalue [9 x i8] %50, i8 %tmp10.unpack177.unpack293, 5
  %tmp10.unpack177.elt294 = getelementptr inbounds nuw i8, ptr %res, i64 51
  %tmp10.unpack177.unpack295 = load i8, ptr %tmp10.unpack177.elt294, align 1
  %52 = insertvalue [9 x i8] %51, i8 %tmp10.unpack177.unpack295, 6
  %tmp10.unpack177.elt296 = getelementptr inbounds nuw i8, ptr %res, i64 52
  %tmp10.unpack177.unpack297 = load i8, ptr %tmp10.unpack177.elt296, align 1
  %53 = insertvalue [9 x i8] %52, i8 %tmp10.unpack177.unpack297, 7
  %tmp10.unpack177.elt298 = getelementptr inbounds nuw i8, ptr %res, i64 53
  %tmp10.unpack177.unpack299 = load i8, ptr %tmp10.unpack177.elt298, align 1
  %tmp10.unpack177300 = insertvalue [9 x i8] %53, i8 %tmp10.unpack177.unpack299, 8
  %54 = insertvalue [16 x [9 x i8]] %45, [9 x i8] %tmp10.unpack177300, 5
  %tmp10.elt178 = getelementptr inbounds nuw i8, ptr %res, i64 54
  %tmp10.unpack179.unpack = load i8, ptr %tmp10.elt178, align 1
  %55 = insertvalue [9 x i8] poison, i8 %tmp10.unpack179.unpack, 0
  %tmp10.unpack179.elt301 = getelementptr inbounds nuw i8, ptr %res, i64 55
  %tmp10.unpack179.unpack302 = load i8, ptr %tmp10.unpack179.elt301, align 1
  %56 = insertvalue [9 x i8] %55, i8 %tmp10.unpack179.unpack302, 1
  %tmp10.unpack179.elt303 = getelementptr inbounds nuw i8, ptr %res, i64 56
  %tmp10.unpack179.unpack304 = load i8, ptr %tmp10.unpack179.elt303, align 1
  %57 = insertvalue [9 x i8] %56, i8 %tmp10.unpack179.unpack304, 2
  %tmp10.unpack179.elt305 = getelementptr inbounds nuw i8, ptr %res, i64 57
  %tmp10.unpack179.unpack306 = load i8, ptr %tmp10.unpack179.elt305, align 1
  %58 = insertvalue [9 x i8] %57, i8 %tmp10.unpack179.unpack306, 3
  %tmp10.unpack179.elt307 = getelementptr inbounds nuw i8, ptr %res, i64 58
  %tmp10.unpack179.unpack308 = load i8, ptr %tmp10.unpack179.elt307, align 1
  %59 = insertvalue [9 x i8] %58, i8 %tmp10.unpack179.unpack308, 4
  %tmp10.unpack179.elt309 = getelementptr inbounds nuw i8, ptr %res, i64 59
  %tmp10.unpack179.unpack310 = load i8, ptr %tmp10.unpack179.elt309, align 1
  %60 = insertvalue [9 x i8] %59, i8 %tmp10.unpack179.unpack310, 5
  %tmp10.unpack179.elt311 = getelementptr inbounds nuw i8, ptr %res, i64 60
  %tmp10.unpack179.unpack312 = load i8, ptr %tmp10.unpack179.elt311, align 1
  %61 = insertvalue [9 x i8] %60, i8 %tmp10.unpack179.unpack312, 6
  %tmp10.unpack179.elt313 = getelementptr inbounds nuw i8, ptr %res, i64 61
  %tmp10.unpack179.unpack314 = load i8, ptr %tmp10.unpack179.elt313, align 1
  %62 = insertvalue [9 x i8] %61, i8 %tmp10.unpack179.unpack314, 7
  %tmp10.unpack179.elt315 = getelementptr inbounds nuw i8, ptr %res, i64 62
  %tmp10.unpack179.unpack316 = load i8, ptr %tmp10.unpack179.elt315, align 1
  %tmp10.unpack179317 = insertvalue [9 x i8] %62, i8 %tmp10.unpack179.unpack316, 8
  %63 = insertvalue [16 x [9 x i8]] %54, [9 x i8] %tmp10.unpack179317, 6
  %tmp10.elt180 = getelementptr inbounds nuw i8, ptr %res, i64 63
  %tmp10.unpack181.unpack = load i8, ptr %tmp10.elt180, align 1
  %64 = insertvalue [9 x i8] poison, i8 %tmp10.unpack181.unpack, 0
  %tmp10.unpack181.elt318 = getelementptr inbounds nuw i8, ptr %res, i64 64
  %tmp10.unpack181.unpack319 = load i8, ptr %tmp10.unpack181.elt318, align 1
  %65 = insertvalue [9 x i8] %64, i8 %tmp10.unpack181.unpack319, 1
  %tmp10.unpack181.elt320 = getelementptr inbounds nuw i8, ptr %res, i64 65
  %tmp10.unpack181.unpack321 = load i8, ptr %tmp10.unpack181.elt320, align 1
  %66 = insertvalue [9 x i8] %65, i8 %tmp10.unpack181.unpack321, 2
  %tmp10.unpack181.elt322 = getelementptr inbounds nuw i8, ptr %res, i64 66
  %tmp10.unpack181.unpack323 = load i8, ptr %tmp10.unpack181.elt322, align 1
  %67 = insertvalue [9 x i8] %66, i8 %tmp10.unpack181.unpack323, 3
  %tmp10.unpack181.elt324 = getelementptr inbounds nuw i8, ptr %res, i64 67
  %tmp10.unpack181.unpack325 = load i8, ptr %tmp10.unpack181.elt324, align 1
  %68 = insertvalue [9 x i8] %67, i8 %tmp10.unpack181.unpack325, 4
  %tmp10.unpack181.elt326 = getelementptr inbounds nuw i8, ptr %res, i64 68
  %tmp10.unpack181.unpack327 = load i8, ptr %tmp10.unpack181.elt326, align 1
  %69 = insertvalue [9 x i8] %68, i8 %tmp10.unpack181.unpack327, 5
  %tmp10.unpack181.elt328 = getelementptr inbounds nuw i8, ptr %res, i64 69
  %tmp10.unpack181.unpack329 = load i8, ptr %tmp10.unpack181.elt328, align 1
  %70 = insertvalue [9 x i8] %69, i8 %tmp10.unpack181.unpack329, 6
  %tmp10.unpack181.elt330 = getelementptr inbounds nuw i8, ptr %res, i64 70
  %tmp10.unpack181.unpack331 = load i8, ptr %tmp10.unpack181.elt330, align 1
  %71 = insertvalue [9 x i8] %70, i8 %tmp10.unpack181.unpack331, 7
  %tmp10.unpack181.elt332 = getelementptr inbounds nuw i8, ptr %res, i64 71
  %tmp10.unpack181.unpack333 = load i8, ptr %tmp10.unpack181.elt332, align 1
  %tmp10.unpack181334 = insertvalue [9 x i8] %71, i8 %tmp10.unpack181.unpack333, 8
  %72 = insertvalue [16 x [9 x i8]] %63, [9 x i8] %tmp10.unpack181334, 7
  %tmp10.elt182 = getelementptr inbounds nuw i8, ptr %res, i64 72
  %tmp10.unpack183.unpack = load i8, ptr %tmp10.elt182, align 1
  %73 = insertvalue [9 x i8] poison, i8 %tmp10.unpack183.unpack, 0
  %tmp10.unpack183.elt335 = getelementptr inbounds nuw i8, ptr %res, i64 73
  %tmp10.unpack183.unpack336 = load i8, ptr %tmp10.unpack183.elt335, align 1
  %74 = insertvalue [9 x i8] %73, i8 %tmp10.unpack183.unpack336, 1
  %tmp10.unpack183.elt337 = getelementptr inbounds nuw i8, ptr %res, i64 74
  %tmp10.unpack183.unpack338 = load i8, ptr %tmp10.unpack183.elt337, align 1
  %75 = insertvalue [9 x i8] %74, i8 %tmp10.unpack183.unpack338, 2
  %tmp10.unpack183.elt339 = getelementptr inbounds nuw i8, ptr %res, i64 75
  %tmp10.unpack183.unpack340 = load i8, ptr %tmp10.unpack183.elt339, align 1
  %76 = insertvalue [9 x i8] %75, i8 %tmp10.unpack183.unpack340, 3
  %tmp10.unpack183.elt341 = getelementptr inbounds nuw i8, ptr %res, i64 76
  %tmp10.unpack183.unpack342 = load i8, ptr %tmp10.unpack183.elt341, align 1
  %77 = insertvalue [9 x i8] %76, i8 %tmp10.unpack183.unpack342, 4
  %tmp10.unpack183.elt343 = getelementptr inbounds nuw i8, ptr %res, i64 77
  %tmp10.unpack183.unpack344 = load i8, ptr %tmp10.unpack183.elt343, align 1
  %78 = insertvalue [9 x i8] %77, i8 %tmp10.unpack183.unpack344, 5
  %tmp10.unpack183.elt345 = getelementptr inbounds nuw i8, ptr %res, i64 78
  %tmp10.unpack183.unpack346 = load i8, ptr %tmp10.unpack183.elt345, align 1
  %79 = insertvalue [9 x i8] %78, i8 %tmp10.unpack183.unpack346, 6
  %tmp10.unpack183.elt347 = getelementptr inbounds nuw i8, ptr %res, i64 79
  %tmp10.unpack183.unpack348 = load i8, ptr %tmp10.unpack183.elt347, align 1
  %80 = insertvalue [9 x i8] %79, i8 %tmp10.unpack183.unpack348, 7
  %tmp10.unpack183.elt349 = getelementptr inbounds nuw i8, ptr %res, i64 80
  %tmp10.unpack183.unpack350 = load i8, ptr %tmp10.unpack183.elt349, align 1
  %tmp10.unpack183351 = insertvalue [9 x i8] %80, i8 %tmp10.unpack183.unpack350, 8
  %81 = insertvalue [16 x [9 x i8]] %72, [9 x i8] %tmp10.unpack183351, 8
  %tmp10.elt184 = getelementptr inbounds nuw i8, ptr %res, i64 81
  %tmp10.unpack185.unpack = load i8, ptr %tmp10.elt184, align 1
  %82 = insertvalue [9 x i8] poison, i8 %tmp10.unpack185.unpack, 0
  %tmp10.unpack185.elt352 = getelementptr inbounds nuw i8, ptr %res, i64 82
  %tmp10.unpack185.unpack353 = load i8, ptr %tmp10.unpack185.elt352, align 1
  %83 = insertvalue [9 x i8] %82, i8 %tmp10.unpack185.unpack353, 1
  %tmp10.unpack185.elt354 = getelementptr inbounds nuw i8, ptr %res, i64 83
  %tmp10.unpack185.unpack355 = load i8, ptr %tmp10.unpack185.elt354, align 1
  %84 = insertvalue [9 x i8] %83, i8 %tmp10.unpack185.unpack355, 2
  %tmp10.unpack185.elt356 = getelementptr inbounds nuw i8, ptr %res, i64 84
  %tmp10.unpack185.unpack357 = load i8, ptr %tmp10.unpack185.elt356, align 1
  %85 = insertvalue [9 x i8] %84, i8 %tmp10.unpack185.unpack357, 3
  %tmp10.unpack185.elt358 = getelementptr inbounds nuw i8, ptr %res, i64 85
  %tmp10.unpack185.unpack359 = load i8, ptr %tmp10.unpack185.elt358, align 1
  %86 = insertvalue [9 x i8] %85, i8 %tmp10.unpack185.unpack359, 4
  %tmp10.unpack185.elt360 = getelementptr inbounds nuw i8, ptr %res, i64 86
  %tmp10.unpack185.unpack361 = load i8, ptr %tmp10.unpack185.elt360, align 1
  %87 = insertvalue [9 x i8] %86, i8 %tmp10.unpack185.unpack361, 5
  %tmp10.unpack185.elt362 = getelementptr inbounds nuw i8, ptr %res, i64 87
  %tmp10.unpack185.unpack363 = load i8, ptr %tmp10.unpack185.elt362, align 1
  %88 = insertvalue [9 x i8] %87, i8 %tmp10.unpack185.unpack363, 6
  %tmp10.unpack185.elt364 = getelementptr inbounds nuw i8, ptr %res, i64 88
  %tmp10.unpack185.unpack365 = load i8, ptr %tmp10.unpack185.elt364, align 1
  %89 = insertvalue [9 x i8] %88, i8 %tmp10.unpack185.unpack365, 7
  %tmp10.unpack185.elt366 = getelementptr inbounds nuw i8, ptr %res, i64 89
  %tmp10.unpack185.unpack367 = load i8, ptr %tmp10.unpack185.elt366, align 1
  %tmp10.unpack185368 = insertvalue [9 x i8] %89, i8 %tmp10.unpack185.unpack367, 8
  %90 = insertvalue [16 x [9 x i8]] %81, [9 x i8] %tmp10.unpack185368, 9
  %tmp10.elt186 = getelementptr inbounds nuw i8, ptr %res, i64 90
  %tmp10.unpack187.unpack = load i8, ptr %tmp10.elt186, align 1
  %91 = insertvalue [9 x i8] poison, i8 %tmp10.unpack187.unpack, 0
  %tmp10.unpack187.elt369 = getelementptr inbounds nuw i8, ptr %res, i64 91
  %tmp10.unpack187.unpack370 = load i8, ptr %tmp10.unpack187.elt369, align 1
  %92 = insertvalue [9 x i8] %91, i8 %tmp10.unpack187.unpack370, 1
  %tmp10.unpack187.elt371 = getelementptr inbounds nuw i8, ptr %res, i64 92
  %tmp10.unpack187.unpack372 = load i8, ptr %tmp10.unpack187.elt371, align 1
  %93 = insertvalue [9 x i8] %92, i8 %tmp10.unpack187.unpack372, 2
  %tmp10.unpack187.elt373 = getelementptr inbounds nuw i8, ptr %res, i64 93
  %tmp10.unpack187.unpack374 = load i8, ptr %tmp10.unpack187.elt373, align 1
  %94 = insertvalue [9 x i8] %93, i8 %tmp10.unpack187.unpack374, 3
  %tmp10.unpack187.elt375 = getelementptr inbounds nuw i8, ptr %res, i64 94
  %tmp10.unpack187.unpack376 = load i8, ptr %tmp10.unpack187.elt375, align 1
  %95 = insertvalue [9 x i8] %94, i8 %tmp10.unpack187.unpack376, 4
  %tmp10.unpack187.elt377 = getelementptr inbounds nuw i8, ptr %res, i64 95
  %tmp10.unpack187.unpack378 = load i8, ptr %tmp10.unpack187.elt377, align 1
  %96 = insertvalue [9 x i8] %95, i8 %tmp10.unpack187.unpack378, 5
  %tmp10.unpack187.elt379 = getelementptr inbounds nuw i8, ptr %res, i64 96
  %tmp10.unpack187.unpack380 = load i8, ptr %tmp10.unpack187.elt379, align 1
  %97 = insertvalue [9 x i8] %96, i8 %tmp10.unpack187.unpack380, 6
  %tmp10.unpack187.elt381 = getelementptr inbounds nuw i8, ptr %res, i64 97
  %tmp10.unpack187.unpack382 = load i8, ptr %tmp10.unpack187.elt381, align 1
  %98 = insertvalue [9 x i8] %97, i8 %tmp10.unpack187.unpack382, 7
  %tmp10.unpack187.elt383 = getelementptr inbounds nuw i8, ptr %res, i64 98
  %tmp10.unpack187.unpack384 = load i8, ptr %tmp10.unpack187.elt383, align 1
  %tmp10.unpack187385 = insertvalue [9 x i8] %98, i8 %tmp10.unpack187.unpack384, 8
  %99 = insertvalue [16 x [9 x i8]] %90, [9 x i8] %tmp10.unpack187385, 10
  %tmp10.elt188 = getelementptr inbounds nuw i8, ptr %res, i64 99
  %tmp10.unpack189.unpack = load i8, ptr %tmp10.elt188, align 1
  %100 = insertvalue [9 x i8] poison, i8 %tmp10.unpack189.unpack, 0
  %tmp10.unpack189.elt386 = getelementptr inbounds nuw i8, ptr %res, i64 100
  %tmp10.unpack189.unpack387 = load i8, ptr %tmp10.unpack189.elt386, align 1
  %101 = insertvalue [9 x i8] %100, i8 %tmp10.unpack189.unpack387, 1
  %tmp10.unpack189.elt388 = getelementptr inbounds nuw i8, ptr %res, i64 101
  %tmp10.unpack189.unpack389 = load i8, ptr %tmp10.unpack189.elt388, align 1
  %102 = insertvalue [9 x i8] %101, i8 %tmp10.unpack189.unpack389, 2
  %tmp10.unpack189.elt390 = getelementptr inbounds nuw i8, ptr %res, i64 102
  %tmp10.unpack189.unpack391 = load i8, ptr %tmp10.unpack189.elt390, align 1
  %103 = insertvalue [9 x i8] %102, i8 %tmp10.unpack189.unpack391, 3
  %tmp10.unpack189.elt392 = getelementptr inbounds nuw i8, ptr %res, i64 103
  %tmp10.unpack189.unpack393 = load i8, ptr %tmp10.unpack189.elt392, align 1
  %104 = insertvalue [9 x i8] %103, i8 %tmp10.unpack189.unpack393, 4
  %tmp10.unpack189.elt394 = getelementptr inbounds nuw i8, ptr %res, i64 104
  %tmp10.unpack189.unpack395 = load i8, ptr %tmp10.unpack189.elt394, align 1
  %105 = insertvalue [9 x i8] %104, i8 %tmp10.unpack189.unpack395, 5
  %tmp10.unpack189.elt396 = getelementptr inbounds nuw i8, ptr %res, i64 105
  %tmp10.unpack189.unpack397 = load i8, ptr %tmp10.unpack189.elt396, align 1
  %106 = insertvalue [9 x i8] %105, i8 %tmp10.unpack189.unpack397, 6
  %tmp10.unpack189.elt398 = getelementptr inbounds nuw i8, ptr %res, i64 106
  %tmp10.unpack189.unpack399 = load i8, ptr %tmp10.unpack189.elt398, align 1
  %107 = insertvalue [9 x i8] %106, i8 %tmp10.unpack189.unpack399, 7
  %tmp10.unpack189.elt400 = getelementptr inbounds nuw i8, ptr %res, i64 107
  %tmp10.unpack189.unpack401 = load i8, ptr %tmp10.unpack189.elt400, align 1
  %tmp10.unpack189402 = insertvalue [9 x i8] %107, i8 %tmp10.unpack189.unpack401, 8
  %108 = insertvalue [16 x [9 x i8]] %99, [9 x i8] %tmp10.unpack189402, 11
  %tmp10.elt190 = getelementptr inbounds nuw i8, ptr %res, i64 108
  %tmp10.unpack191.unpack = load i8, ptr %tmp10.elt190, align 1
  %109 = insertvalue [9 x i8] poison, i8 %tmp10.unpack191.unpack, 0
  %tmp10.unpack191.elt403 = getelementptr inbounds nuw i8, ptr %res, i64 109
  %tmp10.unpack191.unpack404 = load i8, ptr %tmp10.unpack191.elt403, align 1
  %110 = insertvalue [9 x i8] %109, i8 %tmp10.unpack191.unpack404, 1
  %tmp10.unpack191.elt405 = getelementptr inbounds nuw i8, ptr %res, i64 110
  %tmp10.unpack191.unpack406 = load i8, ptr %tmp10.unpack191.elt405, align 1
  %111 = insertvalue [9 x i8] %110, i8 %tmp10.unpack191.unpack406, 2
  %tmp10.unpack191.elt407 = getelementptr inbounds nuw i8, ptr %res, i64 111
  %tmp10.unpack191.unpack408 = load i8, ptr %tmp10.unpack191.elt407, align 1
  %112 = insertvalue [9 x i8] %111, i8 %tmp10.unpack191.unpack408, 3
  %tmp10.unpack191.elt409 = getelementptr inbounds nuw i8, ptr %res, i64 112
  %tmp10.unpack191.unpack410 = load i8, ptr %tmp10.unpack191.elt409, align 1
  %113 = insertvalue [9 x i8] %112, i8 %tmp10.unpack191.unpack410, 4
  %tmp10.unpack191.elt411 = getelementptr inbounds nuw i8, ptr %res, i64 113
  %tmp10.unpack191.unpack412 = load i8, ptr %tmp10.unpack191.elt411, align 1
  %114 = insertvalue [9 x i8] %113, i8 %tmp10.unpack191.unpack412, 5
  %tmp10.unpack191.elt413 = getelementptr inbounds nuw i8, ptr %res, i64 114
  %tmp10.unpack191.unpack414 = load i8, ptr %tmp10.unpack191.elt413, align 1
  %115 = insertvalue [9 x i8] %114, i8 %tmp10.unpack191.unpack414, 6
  %tmp10.unpack191.elt415 = getelementptr inbounds nuw i8, ptr %res, i64 115
  %tmp10.unpack191.unpack416 = load i8, ptr %tmp10.unpack191.elt415, align 1
  %116 = insertvalue [9 x i8] %115, i8 %tmp10.unpack191.unpack416, 7
  %tmp10.unpack191.elt417 = getelementptr inbounds nuw i8, ptr %res, i64 116
  %tmp10.unpack191.unpack418 = load i8, ptr %tmp10.unpack191.elt417, align 1
  %tmp10.unpack191419 = insertvalue [9 x i8] %116, i8 %tmp10.unpack191.unpack418, 8
  %117 = insertvalue [16 x [9 x i8]] %108, [9 x i8] %tmp10.unpack191419, 12
  %tmp10.elt192 = getelementptr inbounds nuw i8, ptr %res, i64 117
  %tmp10.unpack193.unpack = load i8, ptr %tmp10.elt192, align 1
  %118 = insertvalue [9 x i8] poison, i8 %tmp10.unpack193.unpack, 0
  %tmp10.unpack193.elt420 = getelementptr inbounds nuw i8, ptr %res, i64 118
  %tmp10.unpack193.unpack421 = load i8, ptr %tmp10.unpack193.elt420, align 1
  %119 = insertvalue [9 x i8] %118, i8 %tmp10.unpack193.unpack421, 1
  %tmp10.unpack193.elt422 = getelementptr inbounds nuw i8, ptr %res, i64 119
  %tmp10.unpack193.unpack423 = load i8, ptr %tmp10.unpack193.elt422, align 1
  %120 = insertvalue [9 x i8] %119, i8 %tmp10.unpack193.unpack423, 2
  %tmp10.unpack193.elt424 = getelementptr inbounds nuw i8, ptr %res, i64 120
  %tmp10.unpack193.unpack425 = load i8, ptr %tmp10.unpack193.elt424, align 1
  %121 = insertvalue [9 x i8] %120, i8 %tmp10.unpack193.unpack425, 3
  %tmp10.unpack193.elt426 = getelementptr inbounds nuw i8, ptr %res, i64 121
  %tmp10.unpack193.unpack427 = load i8, ptr %tmp10.unpack193.elt426, align 1
  %122 = insertvalue [9 x i8] %121, i8 %tmp10.unpack193.unpack427, 4
  %tmp10.unpack193.elt428 = getelementptr inbounds nuw i8, ptr %res, i64 122
  %tmp10.unpack193.unpack429 = load i8, ptr %tmp10.unpack193.elt428, align 1
  %123 = insertvalue [9 x i8] %122, i8 %tmp10.unpack193.unpack429, 5
  %tmp10.unpack193.elt430 = getelementptr inbounds nuw i8, ptr %res, i64 123
  %tmp10.unpack193.unpack431 = load i8, ptr %tmp10.unpack193.elt430, align 1
  %124 = insertvalue [9 x i8] %123, i8 %tmp10.unpack193.unpack431, 6
  %tmp10.unpack193.elt432 = getelementptr inbounds nuw i8, ptr %res, i64 124
  %tmp10.unpack193.unpack433 = load i8, ptr %tmp10.unpack193.elt432, align 1
  %125 = insertvalue [9 x i8] %124, i8 %tmp10.unpack193.unpack433, 7
  %tmp10.unpack193.elt434 = getelementptr inbounds nuw i8, ptr %res, i64 125
  %tmp10.unpack193.unpack435 = load i8, ptr %tmp10.unpack193.elt434, align 1
  %tmp10.unpack193436 = insertvalue [9 x i8] %125, i8 %tmp10.unpack193.unpack435, 8
  %126 = insertvalue [16 x [9 x i8]] %117, [9 x i8] %tmp10.unpack193436, 13
  %tmp10.elt194 = getelementptr inbounds nuw i8, ptr %res, i64 126
  %tmp10.unpack195.unpack = load i8, ptr %tmp10.elt194, align 1
  %127 = insertvalue [9 x i8] poison, i8 %tmp10.unpack195.unpack, 0
  %tmp10.unpack195.elt437 = getelementptr inbounds nuw i8, ptr %res, i64 127
  %tmp10.unpack195.unpack438 = load i8, ptr %tmp10.unpack195.elt437, align 1
  %128 = insertvalue [9 x i8] %127, i8 %tmp10.unpack195.unpack438, 1
  %tmp10.unpack195.elt439 = getelementptr inbounds nuw i8, ptr %res, i64 128
  %tmp10.unpack195.unpack440 = load i8, ptr %tmp10.unpack195.elt439, align 1
  %129 = insertvalue [9 x i8] %128, i8 %tmp10.unpack195.unpack440, 2
  %tmp10.unpack195.elt441 = getelementptr inbounds nuw i8, ptr %res, i64 129
  %tmp10.unpack195.unpack442 = load i8, ptr %tmp10.unpack195.elt441, align 1
  %130 = insertvalue [9 x i8] %129, i8 %tmp10.unpack195.unpack442, 3
  %tmp10.unpack195.elt443 = getelementptr inbounds nuw i8, ptr %res, i64 130
  %tmp10.unpack195.unpack444 = load i8, ptr %tmp10.unpack195.elt443, align 1
  %131 = insertvalue [9 x i8] %130, i8 %tmp10.unpack195.unpack444, 4
  %tmp10.unpack195.elt445 = getelementptr inbounds nuw i8, ptr %res, i64 131
  %tmp10.unpack195.unpack446 = load i8, ptr %tmp10.unpack195.elt445, align 1
  %132 = insertvalue [9 x i8] %131, i8 %tmp10.unpack195.unpack446, 5
  %tmp10.unpack195.elt447 = getelementptr inbounds nuw i8, ptr %res, i64 132
  %tmp10.unpack195.unpack448 = load i8, ptr %tmp10.unpack195.elt447, align 1
  %133 = insertvalue [9 x i8] %132, i8 %tmp10.unpack195.unpack448, 6
  %tmp10.unpack195.elt449 = getelementptr inbounds nuw i8, ptr %res, i64 133
  %tmp10.unpack195.unpack450 = load i8, ptr %tmp10.unpack195.elt449, align 1
  %134 = insertvalue [9 x i8] %133, i8 %tmp10.unpack195.unpack450, 7
  %tmp10.unpack195.elt451 = getelementptr inbounds nuw i8, ptr %res, i64 134
  %tmp10.unpack195.unpack452 = load i8, ptr %tmp10.unpack195.elt451, align 1
  %tmp10.unpack195453 = insertvalue [9 x i8] %134, i8 %tmp10.unpack195.unpack452, 8
  %135 = insertvalue [16 x [9 x i8]] %126, [9 x i8] %tmp10.unpack195453, 14
  %tmp10.elt196 = getelementptr inbounds nuw i8, ptr %res, i64 135
  %tmp10.unpack197.unpack = load i8, ptr %tmp10.elt196, align 1
  %136 = insertvalue [9 x i8] poison, i8 %tmp10.unpack197.unpack, 0
  %tmp10.unpack197.elt454 = getelementptr inbounds nuw i8, ptr %res, i64 136
  %tmp10.unpack197.unpack455 = load i8, ptr %tmp10.unpack197.elt454, align 1
  %137 = insertvalue [9 x i8] %136, i8 %tmp10.unpack197.unpack455, 1
  %tmp10.unpack197.elt456 = getelementptr inbounds nuw i8, ptr %res, i64 137
  %tmp10.unpack197.unpack457 = load i8, ptr %tmp10.unpack197.elt456, align 1
  %138 = insertvalue [9 x i8] %137, i8 %tmp10.unpack197.unpack457, 2
  %tmp10.unpack197.elt458 = getelementptr inbounds nuw i8, ptr %res, i64 138
  %tmp10.unpack197.unpack459 = load i8, ptr %tmp10.unpack197.elt458, align 1
  %139 = insertvalue [9 x i8] %138, i8 %tmp10.unpack197.unpack459, 3
  %tmp10.unpack197.elt460 = getelementptr inbounds nuw i8, ptr %res, i64 139
  %tmp10.unpack197.unpack461 = load i8, ptr %tmp10.unpack197.elt460, align 1
  %140 = insertvalue [9 x i8] %139, i8 %tmp10.unpack197.unpack461, 4
  %tmp10.unpack197.elt462 = getelementptr inbounds nuw i8, ptr %res, i64 140
  %tmp10.unpack197.unpack463 = load i8, ptr %tmp10.unpack197.elt462, align 1
  %141 = insertvalue [9 x i8] %140, i8 %tmp10.unpack197.unpack463, 5
  %tmp10.unpack197.elt464 = getelementptr inbounds nuw i8, ptr %res, i64 141
  %tmp10.unpack197.unpack465 = load i8, ptr %tmp10.unpack197.elt464, align 1
  %142 = insertvalue [9 x i8] %141, i8 %tmp10.unpack197.unpack465, 6
  %tmp10.unpack197.elt466 = getelementptr inbounds nuw i8, ptr %res, i64 142
  %tmp10.unpack197.unpack467 = load i8, ptr %tmp10.unpack197.elt466, align 1
  %143 = insertvalue [9 x i8] %142, i8 %tmp10.unpack197.unpack467, 7
  %tmp10.unpack197.elt468 = getelementptr inbounds nuw i8, ptr %res, i64 143
  %tmp10.unpack197.unpack469 = load i8, ptr %tmp10.unpack197.elt468, align 1
  %tmp10.unpack197470 = insertvalue [9 x i8] %143, i8 %tmp10.unpack197.unpack469, 8
  %tmp10198 = insertvalue [16 x [9 x i8]] %135, [9 x i8] %tmp10.unpack197470, 15
  %tmp11 = load i64, ptr %idx, align 4
  %fun_call_tmp12 = call {} @"u64:drop"(i64 %tmp11)
  %tmp13.unpack.unpack = load i8, ptr %res, align 1
  %144 = insertvalue [9 x i8] poison, i8 %tmp13.unpack.unpack, 0
  %tmp13.unpack.elt502 = getelementptr inbounds nuw i8, ptr %res, i64 1
  %tmp13.unpack.unpack503 = load i8, ptr %tmp13.unpack.elt502, align 1
  %145 = insertvalue [9 x i8] %144, i8 %tmp13.unpack.unpack503, 1
  %tmp13.unpack.elt504 = getelementptr inbounds nuw i8, ptr %res, i64 2
  %tmp13.unpack.unpack505 = load i8, ptr %tmp13.unpack.elt504, align 1
  %146 = insertvalue [9 x i8] %145, i8 %tmp13.unpack.unpack505, 2
  %tmp13.unpack.elt506 = getelementptr inbounds nuw i8, ptr %res, i64 3
  %tmp13.unpack.unpack507 = load i8, ptr %tmp13.unpack.elt506, align 1
  %147 = insertvalue [9 x i8] %146, i8 %tmp13.unpack.unpack507, 3
  %tmp13.unpack.elt508 = getelementptr inbounds nuw i8, ptr %res, i64 4
  %tmp13.unpack.unpack509 = load i8, ptr %tmp13.unpack.elt508, align 1
  %148 = insertvalue [9 x i8] %147, i8 %tmp13.unpack.unpack509, 4
  %tmp13.unpack.elt510 = getelementptr inbounds nuw i8, ptr %res, i64 5
  %tmp13.unpack.unpack511 = load i8, ptr %tmp13.unpack.elt510, align 1
  %149 = insertvalue [9 x i8] %148, i8 %tmp13.unpack.unpack511, 5
  %tmp13.unpack.elt512 = getelementptr inbounds nuw i8, ptr %res, i64 6
  %tmp13.unpack.unpack513 = load i8, ptr %tmp13.unpack.elt512, align 1
  %150 = insertvalue [9 x i8] %149, i8 %tmp13.unpack.unpack513, 6
  %tmp13.unpack.elt514 = getelementptr inbounds nuw i8, ptr %res, i64 7
  %tmp13.unpack.unpack515 = load i8, ptr %tmp13.unpack.elt514, align 1
  %151 = insertvalue [9 x i8] %150, i8 %tmp13.unpack.unpack515, 7
  %tmp13.unpack.elt516 = getelementptr inbounds nuw i8, ptr %res, i64 8
  %tmp13.unpack.unpack517 = load i8, ptr %tmp13.unpack.elt516, align 1
  %tmp13.unpack518 = insertvalue [9 x i8] %151, i8 %tmp13.unpack.unpack517, 8
  %152 = insertvalue [16 x [9 x i8]] poison, [9 x i8] %tmp13.unpack518, 0
  %tmp13.elt471 = getelementptr inbounds nuw i8, ptr %res, i64 9
  %tmp13.unpack472.unpack = load i8, ptr %tmp13.elt471, align 1
  %153 = insertvalue [9 x i8] poison, i8 %tmp13.unpack472.unpack, 0
  %tmp13.unpack472.elt519 = getelementptr inbounds nuw i8, ptr %res, i64 10
  %tmp13.unpack472.unpack520 = load i8, ptr %tmp13.unpack472.elt519, align 1
  %154 = insertvalue [9 x i8] %153, i8 %tmp13.unpack472.unpack520, 1
  %tmp13.unpack472.elt521 = getelementptr inbounds nuw i8, ptr %res, i64 11
  %tmp13.unpack472.unpack522 = load i8, ptr %tmp13.unpack472.elt521, align 1
  %155 = insertvalue [9 x i8] %154, i8 %tmp13.unpack472.unpack522, 2
  %tmp13.unpack472.elt523 = getelementptr inbounds nuw i8, ptr %res, i64 12
  %tmp13.unpack472.unpack524 = load i8, ptr %tmp13.unpack472.elt523, align 1
  %156 = insertvalue [9 x i8] %155, i8 %tmp13.unpack472.unpack524, 3
  %tmp13.unpack472.elt525 = getelementptr inbounds nuw i8, ptr %res, i64 13
  %tmp13.unpack472.unpack526 = load i8, ptr %tmp13.unpack472.elt525, align 1
  %157 = insertvalue [9 x i8] %156, i8 %tmp13.unpack472.unpack526, 4
  %tmp13.unpack472.elt527 = getelementptr inbounds nuw i8, ptr %res, i64 14
  %tmp13.unpack472.unpack528 = load i8, ptr %tmp13.unpack472.elt527, align 1
  %158 = insertvalue [9 x i8] %157, i8 %tmp13.unpack472.unpack528, 5
  %tmp13.unpack472.elt529 = getelementptr inbounds nuw i8, ptr %res, i64 15
  %tmp13.unpack472.unpack530 = load i8, ptr %tmp13.unpack472.elt529, align 1
  %159 = insertvalue [9 x i8] %158, i8 %tmp13.unpack472.unpack530, 6
  %tmp13.unpack472.elt531 = getelementptr inbounds nuw i8, ptr %res, i64 16
  %tmp13.unpack472.unpack532 = load i8, ptr %tmp13.unpack472.elt531, align 1
  %160 = insertvalue [9 x i8] %159, i8 %tmp13.unpack472.unpack532, 7
  %tmp13.unpack472.elt533 = getelementptr inbounds nuw i8, ptr %res, i64 17
  %tmp13.unpack472.unpack534 = load i8, ptr %tmp13.unpack472.elt533, align 1
  %tmp13.unpack472535 = insertvalue [9 x i8] %160, i8 %tmp13.unpack472.unpack534, 8
  %161 = insertvalue [16 x [9 x i8]] %152, [9 x i8] %tmp13.unpack472535, 1
  %tmp13.elt473 = getelementptr inbounds nuw i8, ptr %res, i64 18
  %tmp13.unpack474.unpack = load i8, ptr %tmp13.elt473, align 1
  %162 = insertvalue [9 x i8] poison, i8 %tmp13.unpack474.unpack, 0
  %tmp13.unpack474.elt536 = getelementptr inbounds nuw i8, ptr %res, i64 19
  %tmp13.unpack474.unpack537 = load i8, ptr %tmp13.unpack474.elt536, align 1
  %163 = insertvalue [9 x i8] %162, i8 %tmp13.unpack474.unpack537, 1
  %tmp13.unpack474.elt538 = getelementptr inbounds nuw i8, ptr %res, i64 20
  %tmp13.unpack474.unpack539 = load i8, ptr %tmp13.unpack474.elt538, align 1
  %164 = insertvalue [9 x i8] %163, i8 %tmp13.unpack474.unpack539, 2
  %tmp13.unpack474.elt540 = getelementptr inbounds nuw i8, ptr %res, i64 21
  %tmp13.unpack474.unpack541 = load i8, ptr %tmp13.unpack474.elt540, align 1
  %165 = insertvalue [9 x i8] %164, i8 %tmp13.unpack474.unpack541, 3
  %tmp13.unpack474.elt542 = getelementptr inbounds nuw i8, ptr %res, i64 22
  %tmp13.unpack474.unpack543 = load i8, ptr %tmp13.unpack474.elt542, align 1
  %166 = insertvalue [9 x i8] %165, i8 %tmp13.unpack474.unpack543, 4
  %tmp13.unpack474.elt544 = getelementptr inbounds nuw i8, ptr %res, i64 23
  %tmp13.unpack474.unpack545 = load i8, ptr %tmp13.unpack474.elt544, align 1
  %167 = insertvalue [9 x i8] %166, i8 %tmp13.unpack474.unpack545, 5
  %tmp13.unpack474.elt546 = getelementptr inbounds nuw i8, ptr %res, i64 24
  %tmp13.unpack474.unpack547 = load i8, ptr %tmp13.unpack474.elt546, align 1
  %168 = insertvalue [9 x i8] %167, i8 %tmp13.unpack474.unpack547, 6
  %tmp13.unpack474.elt548 = getelementptr inbounds nuw i8, ptr %res, i64 25
  %tmp13.unpack474.unpack549 = load i8, ptr %tmp13.unpack474.elt548, align 1
  %169 = insertvalue [9 x i8] %168, i8 %tmp13.unpack474.unpack549, 7
  %tmp13.unpack474.elt550 = getelementptr inbounds nuw i8, ptr %res, i64 26
  %tmp13.unpack474.unpack551 = load i8, ptr %tmp13.unpack474.elt550, align 1
  %tmp13.unpack474552 = insertvalue [9 x i8] %169, i8 %tmp13.unpack474.unpack551, 8
  %170 = insertvalue [16 x [9 x i8]] %161, [9 x i8] %tmp13.unpack474552, 2
  %tmp13.elt475 = getelementptr inbounds nuw i8, ptr %res, i64 27
  %tmp13.unpack476.unpack = load i8, ptr %tmp13.elt475, align 1
  %171 = insertvalue [9 x i8] poison, i8 %tmp13.unpack476.unpack, 0
  %tmp13.unpack476.elt553 = getelementptr inbounds nuw i8, ptr %res, i64 28
  %tmp13.unpack476.unpack554 = load i8, ptr %tmp13.unpack476.elt553, align 1
  %172 = insertvalue [9 x i8] %171, i8 %tmp13.unpack476.unpack554, 1
  %tmp13.unpack476.elt555 = getelementptr inbounds nuw i8, ptr %res, i64 29
  %tmp13.unpack476.unpack556 = load i8, ptr %tmp13.unpack476.elt555, align 1
  %173 = insertvalue [9 x i8] %172, i8 %tmp13.unpack476.unpack556, 2
  %tmp13.unpack476.elt557 = getelementptr inbounds nuw i8, ptr %res, i64 30
  %tmp13.unpack476.unpack558 = load i8, ptr %tmp13.unpack476.elt557, align 1
  %174 = insertvalue [9 x i8] %173, i8 %tmp13.unpack476.unpack558, 3
  %tmp13.unpack476.elt559 = getelementptr inbounds nuw i8, ptr %res, i64 31
  %tmp13.unpack476.unpack560 = load i8, ptr %tmp13.unpack476.elt559, align 1
  %175 = insertvalue [9 x i8] %174, i8 %tmp13.unpack476.unpack560, 4
  %tmp13.unpack476.elt561 = getelementptr inbounds nuw i8, ptr %res, i64 32
  %tmp13.unpack476.unpack562 = load i8, ptr %tmp13.unpack476.elt561, align 1
  %176 = insertvalue [9 x i8] %175, i8 %tmp13.unpack476.unpack562, 5
  %tmp13.unpack476.elt563 = getelementptr inbounds nuw i8, ptr %res, i64 33
  %tmp13.unpack476.unpack564 = load i8, ptr %tmp13.unpack476.elt563, align 1
  %177 = insertvalue [9 x i8] %176, i8 %tmp13.unpack476.unpack564, 6
  %tmp13.unpack476.elt565 = getelementptr inbounds nuw i8, ptr %res, i64 34
  %tmp13.unpack476.unpack566 = load i8, ptr %tmp13.unpack476.elt565, align 1
  %178 = insertvalue [9 x i8] %177, i8 %tmp13.unpack476.unpack566, 7
  %tmp13.unpack476.elt567 = getelementptr inbounds nuw i8, ptr %res, i64 35
  %tmp13.unpack476.unpack568 = load i8, ptr %tmp13.unpack476.elt567, align 1
  %tmp13.unpack476569 = insertvalue [9 x i8] %178, i8 %tmp13.unpack476.unpack568, 8
  %179 = insertvalue [16 x [9 x i8]] %170, [9 x i8] %tmp13.unpack476569, 3
  %tmp13.elt477 = getelementptr inbounds nuw i8, ptr %res, i64 36
  %tmp13.unpack478.unpack = load i8, ptr %tmp13.elt477, align 1
  %180 = insertvalue [9 x i8] poison, i8 %tmp13.unpack478.unpack, 0
  %tmp13.unpack478.elt570 = getelementptr inbounds nuw i8, ptr %res, i64 37
  %tmp13.unpack478.unpack571 = load i8, ptr %tmp13.unpack478.elt570, align 1
  %181 = insertvalue [9 x i8] %180, i8 %tmp13.unpack478.unpack571, 1
  %tmp13.unpack478.elt572 = getelementptr inbounds nuw i8, ptr %res, i64 38
  %tmp13.unpack478.unpack573 = load i8, ptr %tmp13.unpack478.elt572, align 1
  %182 = insertvalue [9 x i8] %181, i8 %tmp13.unpack478.unpack573, 2
  %tmp13.unpack478.elt574 = getelementptr inbounds nuw i8, ptr %res, i64 39
  %tmp13.unpack478.unpack575 = load i8, ptr %tmp13.unpack478.elt574, align 1
  %183 = insertvalue [9 x i8] %182, i8 %tmp13.unpack478.unpack575, 3
  %tmp13.unpack478.elt576 = getelementptr inbounds nuw i8, ptr %res, i64 40
  %tmp13.unpack478.unpack577 = load i8, ptr %tmp13.unpack478.elt576, align 1
  %184 = insertvalue [9 x i8] %183, i8 %tmp13.unpack478.unpack577, 4
  %tmp13.unpack478.elt578 = getelementptr inbounds nuw i8, ptr %res, i64 41
  %tmp13.unpack478.unpack579 = load i8, ptr %tmp13.unpack478.elt578, align 1
  %185 = insertvalue [9 x i8] %184, i8 %tmp13.unpack478.unpack579, 5
  %tmp13.unpack478.elt580 = getelementptr inbounds nuw i8, ptr %res, i64 42
  %tmp13.unpack478.unpack581 = load i8, ptr %tmp13.unpack478.elt580, align 1
  %186 = insertvalue [9 x i8] %185, i8 %tmp13.unpack478.unpack581, 6
  %tmp13.unpack478.elt582 = getelementptr inbounds nuw i8, ptr %res, i64 43
  %tmp13.unpack478.unpack583 = load i8, ptr %tmp13.unpack478.elt582, align 1
  %187 = insertvalue [9 x i8] %186, i8 %tmp13.unpack478.unpack583, 7
  %tmp13.unpack478.elt584 = getelementptr inbounds nuw i8, ptr %res, i64 44
  %tmp13.unpack478.unpack585 = load i8, ptr %tmp13.unpack478.elt584, align 1
  %tmp13.unpack478586 = insertvalue [9 x i8] %187, i8 %tmp13.unpack478.unpack585, 8
  %188 = insertvalue [16 x [9 x i8]] %179, [9 x i8] %tmp13.unpack478586, 4
  %tmp13.elt479 = getelementptr inbounds nuw i8, ptr %res, i64 45
  %tmp13.unpack480.unpack = load i8, ptr %tmp13.elt479, align 1
  %189 = insertvalue [9 x i8] poison, i8 %tmp13.unpack480.unpack, 0
  %tmp13.unpack480.elt587 = getelementptr inbounds nuw i8, ptr %res, i64 46
  %tmp13.unpack480.unpack588 = load i8, ptr %tmp13.unpack480.elt587, align 1
  %190 = insertvalue [9 x i8] %189, i8 %tmp13.unpack480.unpack588, 1
  %tmp13.unpack480.elt589 = getelementptr inbounds nuw i8, ptr %res, i64 47
  %tmp13.unpack480.unpack590 = load i8, ptr %tmp13.unpack480.elt589, align 1
  %191 = insertvalue [9 x i8] %190, i8 %tmp13.unpack480.unpack590, 2
  %tmp13.unpack480.elt591 = getelementptr inbounds nuw i8, ptr %res, i64 48
  %tmp13.unpack480.unpack592 = load i8, ptr %tmp13.unpack480.elt591, align 1
  %192 = insertvalue [9 x i8] %191, i8 %tmp13.unpack480.unpack592, 3
  %tmp13.unpack480.elt593 = getelementptr inbounds nuw i8, ptr %res, i64 49
  %tmp13.unpack480.unpack594 = load i8, ptr %tmp13.unpack480.elt593, align 1
  %193 = insertvalue [9 x i8] %192, i8 %tmp13.unpack480.unpack594, 4
  %tmp13.unpack480.elt595 = getelementptr inbounds nuw i8, ptr %res, i64 50
  %tmp13.unpack480.unpack596 = load i8, ptr %tmp13.unpack480.elt595, align 1
  %194 = insertvalue [9 x i8] %193, i8 %tmp13.unpack480.unpack596, 5
  %tmp13.unpack480.elt597 = getelementptr inbounds nuw i8, ptr %res, i64 51
  %tmp13.unpack480.unpack598 = load i8, ptr %tmp13.unpack480.elt597, align 1
  %195 = insertvalue [9 x i8] %194, i8 %tmp13.unpack480.unpack598, 6
  %tmp13.unpack480.elt599 = getelementptr inbounds nuw i8, ptr %res, i64 52
  %tmp13.unpack480.unpack600 = load i8, ptr %tmp13.unpack480.elt599, align 1
  %196 = insertvalue [9 x i8] %195, i8 %tmp13.unpack480.unpack600, 7
  %tmp13.unpack480.elt601 = getelementptr inbounds nuw i8, ptr %res, i64 53
  %tmp13.unpack480.unpack602 = load i8, ptr %tmp13.unpack480.elt601, align 1
  %tmp13.unpack480603 = insertvalue [9 x i8] %196, i8 %tmp13.unpack480.unpack602, 8
  %197 = insertvalue [16 x [9 x i8]] %188, [9 x i8] %tmp13.unpack480603, 5
  %tmp13.elt481 = getelementptr inbounds nuw i8, ptr %res, i64 54
  %tmp13.unpack482.unpack = load i8, ptr %tmp13.elt481, align 1
  %198 = insertvalue [9 x i8] poison, i8 %tmp13.unpack482.unpack, 0
  %tmp13.unpack482.elt604 = getelementptr inbounds nuw i8, ptr %res, i64 55
  %tmp13.unpack482.unpack605 = load i8, ptr %tmp13.unpack482.elt604, align 1
  %199 = insertvalue [9 x i8] %198, i8 %tmp13.unpack482.unpack605, 1
  %tmp13.unpack482.elt606 = getelementptr inbounds nuw i8, ptr %res, i64 56
  %tmp13.unpack482.unpack607 = load i8, ptr %tmp13.unpack482.elt606, align 1
  %200 = insertvalue [9 x i8] %199, i8 %tmp13.unpack482.unpack607, 2
  %tmp13.unpack482.elt608 = getelementptr inbounds nuw i8, ptr %res, i64 57
  %tmp13.unpack482.unpack609 = load i8, ptr %tmp13.unpack482.elt608, align 1
  %201 = insertvalue [9 x i8] %200, i8 %tmp13.unpack482.unpack609, 3
  %tmp13.unpack482.elt610 = getelementptr inbounds nuw i8, ptr %res, i64 58
  %tmp13.unpack482.unpack611 = load i8, ptr %tmp13.unpack482.elt610, align 1
  %202 = insertvalue [9 x i8] %201, i8 %tmp13.unpack482.unpack611, 4
  %tmp13.unpack482.elt612 = getelementptr inbounds nuw i8, ptr %res, i64 59
  %tmp13.unpack482.unpack613 = load i8, ptr %tmp13.unpack482.elt612, align 1
  %203 = insertvalue [9 x i8] %202, i8 %tmp13.unpack482.unpack613, 5
  %tmp13.unpack482.elt614 = getelementptr inbounds nuw i8, ptr %res, i64 60
  %tmp13.unpack482.unpack615 = load i8, ptr %tmp13.unpack482.elt614, align 1
  %204 = insertvalue [9 x i8] %203, i8 %tmp13.unpack482.unpack615, 6
  %tmp13.unpack482.elt616 = getelementptr inbounds nuw i8, ptr %res, i64 61
  %tmp13.unpack482.unpack617 = load i8, ptr %tmp13.unpack482.elt616, align 1
  %205 = insertvalue [9 x i8] %204, i8 %tmp13.unpack482.unpack617, 7
  %tmp13.unpack482.elt618 = getelementptr inbounds nuw i8, ptr %res, i64 62
  %tmp13.unpack482.unpack619 = load i8, ptr %tmp13.unpack482.elt618, align 1
  %tmp13.unpack482620 = insertvalue [9 x i8] %205, i8 %tmp13.unpack482.unpack619, 8
  %206 = insertvalue [16 x [9 x i8]] %197, [9 x i8] %tmp13.unpack482620, 6
  %tmp13.elt483 = getelementptr inbounds nuw i8, ptr %res, i64 63
  %tmp13.unpack484.unpack = load i8, ptr %tmp13.elt483, align 1
  %207 = insertvalue [9 x i8] poison, i8 %tmp13.unpack484.unpack, 0
  %tmp13.unpack484.elt621 = getelementptr inbounds nuw i8, ptr %res, i64 64
  %tmp13.unpack484.unpack622 = load i8, ptr %tmp13.unpack484.elt621, align 1
  %208 = insertvalue [9 x i8] %207, i8 %tmp13.unpack484.unpack622, 1
  %tmp13.unpack484.elt623 = getelementptr inbounds nuw i8, ptr %res, i64 65
  %tmp13.unpack484.unpack624 = load i8, ptr %tmp13.unpack484.elt623, align 1
  %209 = insertvalue [9 x i8] %208, i8 %tmp13.unpack484.unpack624, 2
  %tmp13.unpack484.elt625 = getelementptr inbounds nuw i8, ptr %res, i64 66
  %tmp13.unpack484.unpack626 = load i8, ptr %tmp13.unpack484.elt625, align 1
  %210 = insertvalue [9 x i8] %209, i8 %tmp13.unpack484.unpack626, 3
  %tmp13.unpack484.elt627 = getelementptr inbounds nuw i8, ptr %res, i64 67
  %tmp13.unpack484.unpack628 = load i8, ptr %tmp13.unpack484.elt627, align 1
  %211 = insertvalue [9 x i8] %210, i8 %tmp13.unpack484.unpack628, 4
  %tmp13.unpack484.elt629 = getelementptr inbounds nuw i8, ptr %res, i64 68
  %tmp13.unpack484.unpack630 = load i8, ptr %tmp13.unpack484.elt629, align 1
  %212 = insertvalue [9 x i8] %211, i8 %tmp13.unpack484.unpack630, 5
  %tmp13.unpack484.elt631 = getelementptr inbounds nuw i8, ptr %res, i64 69
  %tmp13.unpack484.unpack632 = load i8, ptr %tmp13.unpack484.elt631, align 1
  %213 = insertvalue [9 x i8] %212, i8 %tmp13.unpack484.unpack632, 6
  %tmp13.unpack484.elt633 = getelementptr inbounds nuw i8, ptr %res, i64 70
  %tmp13.unpack484.unpack634 = load i8, ptr %tmp13.unpack484.elt633, align 1
  %214 = insertvalue [9 x i8] %213, i8 %tmp13.unpack484.unpack634, 7
  %tmp13.unpack484.elt635 = getelementptr inbounds nuw i8, ptr %res, i64 71
  %tmp13.unpack484.unpack636 = load i8, ptr %tmp13.unpack484.elt635, align 1
  %tmp13.unpack484637 = insertvalue [9 x i8] %214, i8 %tmp13.unpack484.unpack636, 8
  %215 = insertvalue [16 x [9 x i8]] %206, [9 x i8] %tmp13.unpack484637, 7
  %tmp13.elt485 = getelementptr inbounds nuw i8, ptr %res, i64 72
  %tmp13.unpack486.unpack = load i8, ptr %tmp13.elt485, align 1
  %216 = insertvalue [9 x i8] poison, i8 %tmp13.unpack486.unpack, 0
  %tmp13.unpack486.elt638 = getelementptr inbounds nuw i8, ptr %res, i64 73
  %tmp13.unpack486.unpack639 = load i8, ptr %tmp13.unpack486.elt638, align 1
  %217 = insertvalue [9 x i8] %216, i8 %tmp13.unpack486.unpack639, 1
  %tmp13.unpack486.elt640 = getelementptr inbounds nuw i8, ptr %res, i64 74
  %tmp13.unpack486.unpack641 = load i8, ptr %tmp13.unpack486.elt640, align 1
  %218 = insertvalue [9 x i8] %217, i8 %tmp13.unpack486.unpack641, 2
  %tmp13.unpack486.elt642 = getelementptr inbounds nuw i8, ptr %res, i64 75
  %tmp13.unpack486.unpack643 = load i8, ptr %tmp13.unpack486.elt642, align 1
  %219 = insertvalue [9 x i8] %218, i8 %tmp13.unpack486.unpack643, 3
  %tmp13.unpack486.elt644 = getelementptr inbounds nuw i8, ptr %res, i64 76
  %tmp13.unpack486.unpack645 = load i8, ptr %tmp13.unpack486.elt644, align 1
  %220 = insertvalue [9 x i8] %219, i8 %tmp13.unpack486.unpack645, 4
  %tmp13.unpack486.elt646 = getelementptr inbounds nuw i8, ptr %res, i64 77
  %tmp13.unpack486.unpack647 = load i8, ptr %tmp13.unpack486.elt646, align 1
  %221 = insertvalue [9 x i8] %220, i8 %tmp13.unpack486.unpack647, 5
  %tmp13.unpack486.elt648 = getelementptr inbounds nuw i8, ptr %res, i64 78
  %tmp13.unpack486.unpack649 = load i8, ptr %tmp13.unpack486.elt648, align 1
  %222 = insertvalue [9 x i8] %221, i8 %tmp13.unpack486.unpack649, 6
  %tmp13.unpack486.elt650 = getelementptr inbounds nuw i8, ptr %res, i64 79
  %tmp13.unpack486.unpack651 = load i8, ptr %tmp13.unpack486.elt650, align 1
  %223 = insertvalue [9 x i8] %222, i8 %tmp13.unpack486.unpack651, 7
  %tmp13.unpack486.elt652 = getelementptr inbounds nuw i8, ptr %res, i64 80
  %tmp13.unpack486.unpack653 = load i8, ptr %tmp13.unpack486.elt652, align 1
  %tmp13.unpack486654 = insertvalue [9 x i8] %223, i8 %tmp13.unpack486.unpack653, 8
  %224 = insertvalue [16 x [9 x i8]] %215, [9 x i8] %tmp13.unpack486654, 8
  %tmp13.elt487 = getelementptr inbounds nuw i8, ptr %res, i64 81
  %tmp13.unpack488.unpack = load i8, ptr %tmp13.elt487, align 1
  %225 = insertvalue [9 x i8] poison, i8 %tmp13.unpack488.unpack, 0
  %tmp13.unpack488.elt655 = getelementptr inbounds nuw i8, ptr %res, i64 82
  %tmp13.unpack488.unpack656 = load i8, ptr %tmp13.unpack488.elt655, align 1
  %226 = insertvalue [9 x i8] %225, i8 %tmp13.unpack488.unpack656, 1
  %tmp13.unpack488.elt657 = getelementptr inbounds nuw i8, ptr %res, i64 83
  %tmp13.unpack488.unpack658 = load i8, ptr %tmp13.unpack488.elt657, align 1
  %227 = insertvalue [9 x i8] %226, i8 %tmp13.unpack488.unpack658, 2
  %tmp13.unpack488.elt659 = getelementptr inbounds nuw i8, ptr %res, i64 84
  %tmp13.unpack488.unpack660 = load i8, ptr %tmp13.unpack488.elt659, align 1
  %228 = insertvalue [9 x i8] %227, i8 %tmp13.unpack488.unpack660, 3
  %tmp13.unpack488.elt661 = getelementptr inbounds nuw i8, ptr %res, i64 85
  %tmp13.unpack488.unpack662 = load i8, ptr %tmp13.unpack488.elt661, align 1
  %229 = insertvalue [9 x i8] %228, i8 %tmp13.unpack488.unpack662, 4
  %tmp13.unpack488.elt663 = getelementptr inbounds nuw i8, ptr %res, i64 86
  %tmp13.unpack488.unpack664 = load i8, ptr %tmp13.unpack488.elt663, align 1
  %230 = insertvalue [9 x i8] %229, i8 %tmp13.unpack488.unpack664, 5
  %tmp13.unpack488.elt665 = getelementptr inbounds nuw i8, ptr %res, i64 87
  %tmp13.unpack488.unpack666 = load i8, ptr %tmp13.unpack488.elt665, align 1
  %231 = insertvalue [9 x i8] %230, i8 %tmp13.unpack488.unpack666, 6
  %tmp13.unpack488.elt667 = getelementptr inbounds nuw i8, ptr %res, i64 88
  %tmp13.unpack488.unpack668 = load i8, ptr %tmp13.unpack488.elt667, align 1
  %232 = insertvalue [9 x i8] %231, i8 %tmp13.unpack488.unpack668, 7
  %tmp13.unpack488.elt669 = getelementptr inbounds nuw i8, ptr %res, i64 89
  %tmp13.unpack488.unpack670 = load i8, ptr %tmp13.unpack488.elt669, align 1
  %tmp13.unpack488671 = insertvalue [9 x i8] %232, i8 %tmp13.unpack488.unpack670, 8
  %233 = insertvalue [16 x [9 x i8]] %224, [9 x i8] %tmp13.unpack488671, 9
  %tmp13.elt489 = getelementptr inbounds nuw i8, ptr %res, i64 90
  %tmp13.unpack490.unpack = load i8, ptr %tmp13.elt489, align 1
  %234 = insertvalue [9 x i8] poison, i8 %tmp13.unpack490.unpack, 0
  %tmp13.unpack490.elt672 = getelementptr inbounds nuw i8, ptr %res, i64 91
  %tmp13.unpack490.unpack673 = load i8, ptr %tmp13.unpack490.elt672, align 1
  %235 = insertvalue [9 x i8] %234, i8 %tmp13.unpack490.unpack673, 1
  %tmp13.unpack490.elt674 = getelementptr inbounds nuw i8, ptr %res, i64 92
  %tmp13.unpack490.unpack675 = load i8, ptr %tmp13.unpack490.elt674, align 1
  %236 = insertvalue [9 x i8] %235, i8 %tmp13.unpack490.unpack675, 2
  %tmp13.unpack490.elt676 = getelementptr inbounds nuw i8, ptr %res, i64 93
  %tmp13.unpack490.unpack677 = load i8, ptr %tmp13.unpack490.elt676, align 1
  %237 = insertvalue [9 x i8] %236, i8 %tmp13.unpack490.unpack677, 3
  %tmp13.unpack490.elt678 = getelementptr inbounds nuw i8, ptr %res, i64 94
  %tmp13.unpack490.unpack679 = load i8, ptr %tmp13.unpack490.elt678, align 1
  %238 = insertvalue [9 x i8] %237, i8 %tmp13.unpack490.unpack679, 4
  %tmp13.unpack490.elt680 = getelementptr inbounds nuw i8, ptr %res, i64 95
  %tmp13.unpack490.unpack681 = load i8, ptr %tmp13.unpack490.elt680, align 1
  %239 = insertvalue [9 x i8] %238, i8 %tmp13.unpack490.unpack681, 5
  %tmp13.unpack490.elt682 = getelementptr inbounds nuw i8, ptr %res, i64 96
  %tmp13.unpack490.unpack683 = load i8, ptr %tmp13.unpack490.elt682, align 1
  %240 = insertvalue [9 x i8] %239, i8 %tmp13.unpack490.unpack683, 6
  %tmp13.unpack490.elt684 = getelementptr inbounds nuw i8, ptr %res, i64 97
  %tmp13.unpack490.unpack685 = load i8, ptr %tmp13.unpack490.elt684, align 1
  %241 = insertvalue [9 x i8] %240, i8 %tmp13.unpack490.unpack685, 7
  %tmp13.unpack490.elt686 = getelementptr inbounds nuw i8, ptr %res, i64 98
  %tmp13.unpack490.unpack687 = load i8, ptr %tmp13.unpack490.elt686, align 1
  %tmp13.unpack490688 = insertvalue [9 x i8] %241, i8 %tmp13.unpack490.unpack687, 8
  %242 = insertvalue [16 x [9 x i8]] %233, [9 x i8] %tmp13.unpack490688, 10
  %tmp13.elt491 = getelementptr inbounds nuw i8, ptr %res, i64 99
  %tmp13.unpack492.unpack = load i8, ptr %tmp13.elt491, align 1
  %243 = insertvalue [9 x i8] poison, i8 %tmp13.unpack492.unpack, 0
  %tmp13.unpack492.elt689 = getelementptr inbounds nuw i8, ptr %res, i64 100
  %tmp13.unpack492.unpack690 = load i8, ptr %tmp13.unpack492.elt689, align 1
  %244 = insertvalue [9 x i8] %243, i8 %tmp13.unpack492.unpack690, 1
  %tmp13.unpack492.elt691 = getelementptr inbounds nuw i8, ptr %res, i64 101
  %tmp13.unpack492.unpack692 = load i8, ptr %tmp13.unpack492.elt691, align 1
  %245 = insertvalue [9 x i8] %244, i8 %tmp13.unpack492.unpack692, 2
  %tmp13.unpack492.elt693 = getelementptr inbounds nuw i8, ptr %res, i64 102
  %tmp13.unpack492.unpack694 = load i8, ptr %tmp13.unpack492.elt693, align 1
  %246 = insertvalue [9 x i8] %245, i8 %tmp13.unpack492.unpack694, 3
  %tmp13.unpack492.elt695 = getelementptr inbounds nuw i8, ptr %res, i64 103
  %tmp13.unpack492.unpack696 = load i8, ptr %tmp13.unpack492.elt695, align 1
  %247 = insertvalue [9 x i8] %246, i8 %tmp13.unpack492.unpack696, 4
  %tmp13.unpack492.elt697 = getelementptr inbounds nuw i8, ptr %res, i64 104
  %tmp13.unpack492.unpack698 = load i8, ptr %tmp13.unpack492.elt697, align 1
  %248 = insertvalue [9 x i8] %247, i8 %tmp13.unpack492.unpack698, 5
  %tmp13.unpack492.elt699 = getelementptr inbounds nuw i8, ptr %res, i64 105
  %tmp13.unpack492.unpack700 = load i8, ptr %tmp13.unpack492.elt699, align 1
  %249 = insertvalue [9 x i8] %248, i8 %tmp13.unpack492.unpack700, 6
  %tmp13.unpack492.elt701 = getelementptr inbounds nuw i8, ptr %res, i64 106
  %tmp13.unpack492.unpack702 = load i8, ptr %tmp13.unpack492.elt701, align 1
  %250 = insertvalue [9 x i8] %249, i8 %tmp13.unpack492.unpack702, 7
  %tmp13.unpack492.elt703 = getelementptr inbounds nuw i8, ptr %res, i64 107
  %tmp13.unpack492.unpack704 = load i8, ptr %tmp13.unpack492.elt703, align 1
  %tmp13.unpack492705 = insertvalue [9 x i8] %250, i8 %tmp13.unpack492.unpack704, 8
  %251 = insertvalue [16 x [9 x i8]] %242, [9 x i8] %tmp13.unpack492705, 11
  %tmp13.elt493 = getelementptr inbounds nuw i8, ptr %res, i64 108
  %tmp13.unpack494.unpack = load i8, ptr %tmp13.elt493, align 1
  %252 = insertvalue [9 x i8] poison, i8 %tmp13.unpack494.unpack, 0
  %tmp13.unpack494.elt706 = getelementptr inbounds nuw i8, ptr %res, i64 109
  %tmp13.unpack494.unpack707 = load i8, ptr %tmp13.unpack494.elt706, align 1
  %253 = insertvalue [9 x i8] %252, i8 %tmp13.unpack494.unpack707, 1
  %tmp13.unpack494.elt708 = getelementptr inbounds nuw i8, ptr %res, i64 110
  %tmp13.unpack494.unpack709 = load i8, ptr %tmp13.unpack494.elt708, align 1
  %254 = insertvalue [9 x i8] %253, i8 %tmp13.unpack494.unpack709, 2
  %tmp13.unpack494.elt710 = getelementptr inbounds nuw i8, ptr %res, i64 111
  %tmp13.unpack494.unpack711 = load i8, ptr %tmp13.unpack494.elt710, align 1
  %255 = insertvalue [9 x i8] %254, i8 %tmp13.unpack494.unpack711, 3
  %tmp13.unpack494.elt712 = getelementptr inbounds nuw i8, ptr %res, i64 112
  %tmp13.unpack494.unpack713 = load i8, ptr %tmp13.unpack494.elt712, align 1
  %256 = insertvalue [9 x i8] %255, i8 %tmp13.unpack494.unpack713, 4
  %tmp13.unpack494.elt714 = getelementptr inbounds nuw i8, ptr %res, i64 113
  %tmp13.unpack494.unpack715 = load i8, ptr %tmp13.unpack494.elt714, align 1
  %257 = insertvalue [9 x i8] %256, i8 %tmp13.unpack494.unpack715, 5
  %tmp13.unpack494.elt716 = getelementptr inbounds nuw i8, ptr %res, i64 114
  %tmp13.unpack494.unpack717 = load i8, ptr %tmp13.unpack494.elt716, align 1
  %258 = insertvalue [9 x i8] %257, i8 %tmp13.unpack494.unpack717, 6
  %tmp13.unpack494.elt718 = getelementptr inbounds nuw i8, ptr %res, i64 115
  %tmp13.unpack494.unpack719 = load i8, ptr %tmp13.unpack494.elt718, align 1
  %259 = insertvalue [9 x i8] %258, i8 %tmp13.unpack494.unpack719, 7
  %tmp13.unpack494.elt720 = getelementptr inbounds nuw i8, ptr %res, i64 116
  %tmp13.unpack494.unpack721 = load i8, ptr %tmp13.unpack494.elt720, align 1
  %tmp13.unpack494722 = insertvalue [9 x i8] %259, i8 %tmp13.unpack494.unpack721, 8
  %260 = insertvalue [16 x [9 x i8]] %251, [9 x i8] %tmp13.unpack494722, 12
  %tmp13.elt495 = getelementptr inbounds nuw i8, ptr %res, i64 117
  %tmp13.unpack496.unpack = load i8, ptr %tmp13.elt495, align 1
  %261 = insertvalue [9 x i8] poison, i8 %tmp13.unpack496.unpack, 0
  %tmp13.unpack496.elt723 = getelementptr inbounds nuw i8, ptr %res, i64 118
  %tmp13.unpack496.unpack724 = load i8, ptr %tmp13.unpack496.elt723, align 1
  %262 = insertvalue [9 x i8] %261, i8 %tmp13.unpack496.unpack724, 1
  %tmp13.unpack496.elt725 = getelementptr inbounds nuw i8, ptr %res, i64 119
  %tmp13.unpack496.unpack726 = load i8, ptr %tmp13.unpack496.elt725, align 1
  %263 = insertvalue [9 x i8] %262, i8 %tmp13.unpack496.unpack726, 2
  %tmp13.unpack496.elt727 = getelementptr inbounds nuw i8, ptr %res, i64 120
  %tmp13.unpack496.unpack728 = load i8, ptr %tmp13.unpack496.elt727, align 1
  %264 = insertvalue [9 x i8] %263, i8 %tmp13.unpack496.unpack728, 3
  %tmp13.unpack496.elt729 = getelementptr inbounds nuw i8, ptr %res, i64 121
  %tmp13.unpack496.unpack730 = load i8, ptr %tmp13.unpack496.elt729, align 1
  %265 = insertvalue [9 x i8] %264, i8 %tmp13.unpack496.unpack730, 4
  %tmp13.unpack496.elt731 = getelementptr inbounds nuw i8, ptr %res, i64 122
  %tmp13.unpack496.unpack732 = load i8, ptr %tmp13.unpack496.elt731, align 1
  %266 = insertvalue [9 x i8] %265, i8 %tmp13.unpack496.unpack732, 5
  %tmp13.unpack496.elt733 = getelementptr inbounds nuw i8, ptr %res, i64 123
  %tmp13.unpack496.unpack734 = load i8, ptr %tmp13.unpack496.elt733, align 1
  %267 = insertvalue [9 x i8] %266, i8 %tmp13.unpack496.unpack734, 6
  %tmp13.unpack496.elt735 = getelementptr inbounds nuw i8, ptr %res, i64 124
  %tmp13.unpack496.unpack736 = load i8, ptr %tmp13.unpack496.elt735, align 1
  %268 = insertvalue [9 x i8] %267, i8 %tmp13.unpack496.unpack736, 7
  %tmp13.unpack496.elt737 = getelementptr inbounds nuw i8, ptr %res, i64 125
  %tmp13.unpack496.unpack738 = load i8, ptr %tmp13.unpack496.elt737, align 1
  %tmp13.unpack496739 = insertvalue [9 x i8] %268, i8 %tmp13.unpack496.unpack738, 8
  %269 = insertvalue [16 x [9 x i8]] %260, [9 x i8] %tmp13.unpack496739, 13
  %tmp13.elt497 = getelementptr inbounds nuw i8, ptr %res, i64 126
  %tmp13.unpack498.unpack = load i8, ptr %tmp13.elt497, align 1
  %270 = insertvalue [9 x i8] poison, i8 %tmp13.unpack498.unpack, 0
  %tmp13.unpack498.elt740 = getelementptr inbounds nuw i8, ptr %res, i64 127
  %tmp13.unpack498.unpack741 = load i8, ptr %tmp13.unpack498.elt740, align 1
  %271 = insertvalue [9 x i8] %270, i8 %tmp13.unpack498.unpack741, 1
  %tmp13.unpack498.elt742 = getelementptr inbounds nuw i8, ptr %res, i64 128
  %tmp13.unpack498.unpack743 = load i8, ptr %tmp13.unpack498.elt742, align 1
  %272 = insertvalue [9 x i8] %271, i8 %tmp13.unpack498.unpack743, 2
  %tmp13.unpack498.elt744 = getelementptr inbounds nuw i8, ptr %res, i64 129
  %tmp13.unpack498.unpack745 = load i8, ptr %tmp13.unpack498.elt744, align 1
  %273 = insertvalue [9 x i8] %272, i8 %tmp13.unpack498.unpack745, 3
  %tmp13.unpack498.elt746 = getelementptr inbounds nuw i8, ptr %res, i64 130
  %tmp13.unpack498.unpack747 = load i8, ptr %tmp13.unpack498.elt746, align 1
  %274 = insertvalue [9 x i8] %273, i8 %tmp13.unpack498.unpack747, 4
  %tmp13.unpack498.elt748 = getelementptr inbounds nuw i8, ptr %res, i64 131
  %tmp13.unpack498.unpack749 = load i8, ptr %tmp13.unpack498.elt748, align 1
  %275 = insertvalue [9 x i8] %274, i8 %tmp13.unpack498.unpack749, 5
  %tmp13.unpack498.elt750 = getelementptr inbounds nuw i8, ptr %res, i64 132
  %tmp13.unpack498.unpack751 = load i8, ptr %tmp13.unpack498.elt750, align 1
  %276 = insertvalue [9 x i8] %275, i8 %tmp13.unpack498.unpack751, 6
  %tmp13.unpack498.elt752 = getelementptr inbounds nuw i8, ptr %res, i64 133
  %tmp13.unpack498.unpack753 = load i8, ptr %tmp13.unpack498.elt752, align 1
  %277 = insertvalue [9 x i8] %276, i8 %tmp13.unpack498.unpack753, 7
  %tmp13.unpack498.elt754 = getelementptr inbounds nuw i8, ptr %res, i64 134
  %tmp13.unpack498.unpack755 = load i8, ptr %tmp13.unpack498.elt754, align 1
  %tmp13.unpack498756 = insertvalue [9 x i8] %277, i8 %tmp13.unpack498.unpack755, 8
  %278 = insertvalue [16 x [9 x i8]] %269, [9 x i8] %tmp13.unpack498756, 14
  %tmp13.elt499 = getelementptr inbounds nuw i8, ptr %res, i64 135
  %tmp13.unpack500.unpack = load i8, ptr %tmp13.elt499, align 1
  %279 = insertvalue [9 x i8] poison, i8 %tmp13.unpack500.unpack, 0
  %tmp13.unpack500.elt757 = getelementptr inbounds nuw i8, ptr %res, i64 136
  %tmp13.unpack500.unpack758 = load i8, ptr %tmp13.unpack500.elt757, align 1
  %280 = insertvalue [9 x i8] %279, i8 %tmp13.unpack500.unpack758, 1
  %tmp13.unpack500.elt759 = getelementptr inbounds nuw i8, ptr %res, i64 137
  %tmp13.unpack500.unpack760 = load i8, ptr %tmp13.unpack500.elt759, align 1
  %281 = insertvalue [9 x i8] %280, i8 %tmp13.unpack500.unpack760, 2
  %tmp13.unpack500.elt761 = getelementptr inbounds nuw i8, ptr %res, i64 138
  %tmp13.unpack500.unpack762 = load i8, ptr %tmp13.unpack500.elt761, align 1
  %282 = insertvalue [9 x i8] %281, i8 %tmp13.unpack500.unpack762, 3
  %tmp13.unpack500.elt763 = getelementptr inbounds nuw i8, ptr %res, i64 139
  %tmp13.unpack500.unpack764 = load i8, ptr %tmp13.unpack500.elt763, align 1
  %283 = insertvalue [9 x i8] %282, i8 %tmp13.unpack500.unpack764, 4
  %tmp13.unpack500.elt765 = getelementptr inbounds nuw i8, ptr %res, i64 140
  %tmp13.unpack500.unpack766 = load i8, ptr %tmp13.unpack500.elt765, align 1
  %284 = insertvalue [9 x i8] %283, i8 %tmp13.unpack500.unpack766, 5
  %tmp13.unpack500.elt767 = getelementptr inbounds nuw i8, ptr %res, i64 141
  %tmp13.unpack500.unpack768 = load i8, ptr %tmp13.unpack500.elt767, align 1
  %285 = insertvalue [9 x i8] %284, i8 %tmp13.unpack500.unpack768, 6
  %tmp13.unpack500.elt769 = getelementptr inbounds nuw i8, ptr %res, i64 142
  %tmp13.unpack500.unpack770 = load i8, ptr %tmp13.unpack500.elt769, align 1
  %286 = insertvalue [9 x i8] %285, i8 %tmp13.unpack500.unpack770, 7
  %tmp13.unpack500.elt771 = getelementptr inbounds nuw i8, ptr %res, i64 143
  %tmp13.unpack500.unpack772 = load i8, ptr %tmp13.unpack500.elt771, align 1
  %tmp13.unpack500773 = insertvalue [9 x i8] %286, i8 %tmp13.unpack500.unpack772, 8
  %tmp13501 = insertvalue [16 x [9 x i8]] %278, [9 x i8] %tmp13.unpack500773, 15
  %fun_call_tmp14 = call {} @"[[u8; 9]; 16]:drop"([16 x [9 x i8]] %tmp13501)
  ret [16 x [9 x i8]] %tmp10198
}

define [9 x i8] @"[u8; 9]:clone"(ptr %0) {
entry:
  %idx = alloca i64, align 8
  %res = alloca [9 x i8], align 1
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store i8 0, ptr %res, align 1
  %res.repack25 = getelementptr inbounds nuw i8, ptr %res, i64 1
  store i8 0, ptr %res.repack25, align 1
  %res.repack26 = getelementptr inbounds nuw i8, ptr %res, i64 2
  store i8 0, ptr %res.repack26, align 1
  %res.repack27 = getelementptr inbounds nuw i8, ptr %res, i64 3
  store i8 0, ptr %res.repack27, align 1
  %res.repack28 = getelementptr inbounds nuw i8, ptr %res, i64 4
  store i8 0, ptr %res.repack28, align 1
  %res.repack29 = getelementptr inbounds nuw i8, ptr %res, i64 5
  store i8 0, ptr %res.repack29, align 1
  %res.repack30 = getelementptr inbounds nuw i8, ptr %res, i64 6
  store i8 0, ptr %res.repack30, align 1
  %res.repack31 = getelementptr inbounds nuw i8, ptr %res, i64 7
  store i8 0, ptr %res.repack31, align 1
  %res.repack32 = getelementptr inbounds nuw i8, ptr %res, i64 8
  store i8 0, ptr %res.repack32, align 1
  br label %cond

then:                                             ; preds = %cond
  %tmp2 = load i64, ptr %idx, align 4
  %tmp_index.i23 = getelementptr i8, ptr %res, i64 %tmp2
  %tmp4 = load ptr, ptr %self, align 8
  %tmp_index.i = getelementptr i8, ptr %tmp4, i64 %tmp2
  %fun_call_tmp7 = call i8 @"u8:clone"(ptr %tmp_index.i)
  store i8 %fun_call_tmp7, ptr %tmp_index.i23, align 1
  %tmp.i = add i64 %tmp2, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp.i19 = icmp ult i64 %storemerge, 9
  br i1 %tmp.i19, label %then, label %else

else:                                             ; preds = %cond
  %tmp10.unpack = load i8, ptr %res, align 1
  %1 = insertvalue [9 x i8] poison, i8 %tmp10.unpack, 0
  %tmp10.elt33 = getelementptr inbounds nuw i8, ptr %res, i64 1
  %tmp10.unpack34 = load i8, ptr %tmp10.elt33, align 1
  %2 = insertvalue [9 x i8] %1, i8 %tmp10.unpack34, 1
  %tmp10.elt35 = getelementptr inbounds nuw i8, ptr %res, i64 2
  %tmp10.unpack36 = load i8, ptr %tmp10.elt35, align 1
  %3 = insertvalue [9 x i8] %2, i8 %tmp10.unpack36, 2
  %tmp10.elt37 = getelementptr inbounds nuw i8, ptr %res, i64 3
  %tmp10.unpack38 = load i8, ptr %tmp10.elt37, align 1
  %4 = insertvalue [9 x i8] %3, i8 %tmp10.unpack38, 3
  %tmp10.elt39 = getelementptr inbounds nuw i8, ptr %res, i64 4
  %tmp10.unpack40 = load i8, ptr %tmp10.elt39, align 1
  %5 = insertvalue [9 x i8] %4, i8 %tmp10.unpack40, 4
  %tmp10.elt41 = getelementptr inbounds nuw i8, ptr %res, i64 5
  %tmp10.unpack42 = load i8, ptr %tmp10.elt41, align 1
  %6 = insertvalue [9 x i8] %5, i8 %tmp10.unpack42, 5
  %tmp10.elt43 = getelementptr inbounds nuw i8, ptr %res, i64 6
  %tmp10.unpack44 = load i8, ptr %tmp10.elt43, align 1
  %7 = insertvalue [9 x i8] %6, i8 %tmp10.unpack44, 6
  %tmp10.elt45 = getelementptr inbounds nuw i8, ptr %res, i64 7
  %tmp10.unpack46 = load i8, ptr %tmp10.elt45, align 1
  %8 = insertvalue [9 x i8] %7, i8 %tmp10.unpack46, 7
  %tmp10.elt47 = getelementptr inbounds nuw i8, ptr %res, i64 8
  %tmp10.unpack48 = load i8, ptr %tmp10.elt47, align 1
  %tmp1049 = insertvalue [9 x i8] %8, i8 %tmp10.unpack48, 8
  %tmp11 = load i64, ptr %idx, align 4
  %fun_call_tmp12 = call {} @"u64:drop"(i64 %tmp11)
  %tmp13.unpack = load i8, ptr %res, align 1
  %9 = insertvalue [9 x i8] poison, i8 %tmp13.unpack, 0
  %tmp13.elt50 = getelementptr inbounds nuw i8, ptr %res, i64 1
  %tmp13.unpack51 = load i8, ptr %tmp13.elt50, align 1
  %10 = insertvalue [9 x i8] %9, i8 %tmp13.unpack51, 1
  %tmp13.elt52 = getelementptr inbounds nuw i8, ptr %res, i64 2
  %tmp13.unpack53 = load i8, ptr %tmp13.elt52, align 1
  %11 = insertvalue [9 x i8] %10, i8 %tmp13.unpack53, 2
  %tmp13.elt54 = getelementptr inbounds nuw i8, ptr %res, i64 3
  %tmp13.unpack55 = load i8, ptr %tmp13.elt54, align 1
  %12 = insertvalue [9 x i8] %11, i8 %tmp13.unpack55, 3
  %tmp13.elt56 = getelementptr inbounds nuw i8, ptr %res, i64 4
  %tmp13.unpack57 = load i8, ptr %tmp13.elt56, align 1
  %13 = insertvalue [9 x i8] %12, i8 %tmp13.unpack57, 4
  %tmp13.elt58 = getelementptr inbounds nuw i8, ptr %res, i64 5
  %tmp13.unpack59 = load i8, ptr %tmp13.elt58, align 1
  %14 = insertvalue [9 x i8] %13, i8 %tmp13.unpack59, 5
  %tmp13.elt60 = getelementptr inbounds nuw i8, ptr %res, i64 6
  %tmp13.unpack61 = load i8, ptr %tmp13.elt60, align 1
  %15 = insertvalue [9 x i8] %14, i8 %tmp13.unpack61, 6
  %tmp13.elt62 = getelementptr inbounds nuw i8, ptr %res, i64 7
  %tmp13.unpack63 = load i8, ptr %tmp13.elt62, align 1
  %16 = insertvalue [9 x i8] %15, i8 %tmp13.unpack63, 7
  %tmp13.elt64 = getelementptr inbounds nuw i8, ptr %res, i64 8
  %tmp13.unpack65 = load i8, ptr %tmp13.elt64, align 1
  %tmp1366 = insertvalue [9 x i8] %16, i8 %tmp13.unpack65, 8
  %fun_call_tmp14 = call {} @"[u8; 9]:drop"([9 x i8] %tmp1366)
  ret [9 x i8] %tmp1049
}

define {} @"[u8; 9]:drop"([9 x i8] %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca [9 x i8], align 1
  %.elt = extractvalue [9 x i8] %0, 0
  store i8 %.elt, ptr %self, align 1
  %self.repack13 = getelementptr inbounds nuw i8, ptr %self, i64 1
  %.elt14 = extractvalue [9 x i8] %0, 1
  store i8 %.elt14, ptr %self.repack13, align 1
  %self.repack15 = getelementptr inbounds nuw i8, ptr %self, i64 2
  %.elt16 = extractvalue [9 x i8] %0, 2
  store i8 %.elt16, ptr %self.repack15, align 1
  %self.repack17 = getelementptr inbounds nuw i8, ptr %self, i64 3
  %.elt18 = extractvalue [9 x i8] %0, 3
  store i8 %.elt18, ptr %self.repack17, align 1
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %.elt20 = extractvalue [9 x i8] %0, 4
  store i8 %.elt20, ptr %self.repack19, align 1
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 5
  %.elt22 = extractvalue [9 x i8] %0, 5
  store i8 %.elt22, ptr %self.repack21, align 1
  %self.repack23 = getelementptr inbounds nuw i8, ptr %self, i64 6
  %.elt24 = extractvalue [9 x i8] %0, 6
  store i8 %.elt24, ptr %self.repack23, align 1
  %self.repack25 = getelementptr inbounds nuw i8, ptr %self, i64 7
  %.elt26 = extractvalue [9 x i8] %0, 7
  store i8 %.elt26, ptr %self.repack25, align 1
  %self.repack27 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt28 = extractvalue [9 x i8] %0, 8
  store i8 %.elt28, ptr %self.repack27, align 1
  br label %cond

then:                                             ; preds = %cond
  %tmp1 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %self, i64 %tmp1
  %tmp3 = load i8, ptr %tmp_index.i, align 1
  %fun_call_tmp4 = call {} @"u8:drop"(i8 %tmp3)
  %tmp.i = add i64 %tmp1, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp.i11 = icmp ult i64 %storemerge, 9
  br i1 %tmp.i11, label %then, label %else

else:                                             ; preds = %cond
  %tmp7 = load i64, ptr %idx, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
  ret {} zeroinitializer
}

define {} @"[[u8; 9]; 16]:drop"([16 x [9 x i8]] %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca [16 x [9 x i8]], align 1
  %.elt = extractvalue [16 x [9 x i8]] %0, 0
  %.elt.elt = extractvalue [9 x i8] %.elt, 0
  store i8 %.elt.elt, ptr %self, align 1
  %self.repack43 = getelementptr inbounds nuw i8, ptr %self, i64 1
  %.elt.elt44 = extractvalue [9 x i8] %.elt, 1
  store i8 %.elt.elt44, ptr %self.repack43, align 1
  %self.repack45 = getelementptr inbounds nuw i8, ptr %self, i64 2
  %.elt.elt46 = extractvalue [9 x i8] %.elt, 2
  store i8 %.elt.elt46, ptr %self.repack45, align 1
  %self.repack47 = getelementptr inbounds nuw i8, ptr %self, i64 3
  %.elt.elt48 = extractvalue [9 x i8] %.elt, 3
  store i8 %.elt.elt48, ptr %self.repack47, align 1
  %self.repack49 = getelementptr inbounds nuw i8, ptr %self, i64 4
  %.elt.elt50 = extractvalue [9 x i8] %.elt, 4
  store i8 %.elt.elt50, ptr %self.repack49, align 1
  %self.repack51 = getelementptr inbounds nuw i8, ptr %self, i64 5
  %.elt.elt52 = extractvalue [9 x i8] %.elt, 5
  store i8 %.elt.elt52, ptr %self.repack51, align 1
  %self.repack53 = getelementptr inbounds nuw i8, ptr %self, i64 6
  %.elt.elt54 = extractvalue [9 x i8] %.elt, 6
  store i8 %.elt.elt54, ptr %self.repack53, align 1
  %self.repack55 = getelementptr inbounds nuw i8, ptr %self, i64 7
  %.elt.elt56 = extractvalue [9 x i8] %.elt, 7
  store i8 %.elt.elt56, ptr %self.repack55, align 1
  %self.repack57 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt.elt58 = extractvalue [9 x i8] %.elt, 8
  store i8 %.elt.elt58, ptr %self.repack57, align 1
  %self.repack13 = getelementptr inbounds nuw i8, ptr %self, i64 9
  %.elt14 = extractvalue [16 x [9 x i8]] %0, 1
  %.elt14.elt = extractvalue [9 x i8] %.elt14, 0
  store i8 %.elt14.elt, ptr %self.repack13, align 1
  %self.repack13.repack59 = getelementptr inbounds nuw i8, ptr %self, i64 10
  %.elt14.elt60 = extractvalue [9 x i8] %.elt14, 1
  store i8 %.elt14.elt60, ptr %self.repack13.repack59, align 1
  %self.repack13.repack61 = getelementptr inbounds nuw i8, ptr %self, i64 11
  %.elt14.elt62 = extractvalue [9 x i8] %.elt14, 2
  store i8 %.elt14.elt62, ptr %self.repack13.repack61, align 1
  %self.repack13.repack63 = getelementptr inbounds nuw i8, ptr %self, i64 12
  %.elt14.elt64 = extractvalue [9 x i8] %.elt14, 3
  store i8 %.elt14.elt64, ptr %self.repack13.repack63, align 1
  %self.repack13.repack65 = getelementptr inbounds nuw i8, ptr %self, i64 13
  %.elt14.elt66 = extractvalue [9 x i8] %.elt14, 4
  store i8 %.elt14.elt66, ptr %self.repack13.repack65, align 1
  %self.repack13.repack67 = getelementptr inbounds nuw i8, ptr %self, i64 14
  %.elt14.elt68 = extractvalue [9 x i8] %.elt14, 5
  store i8 %.elt14.elt68, ptr %self.repack13.repack67, align 1
  %self.repack13.repack69 = getelementptr inbounds nuw i8, ptr %self, i64 15
  %.elt14.elt70 = extractvalue [9 x i8] %.elt14, 6
  store i8 %.elt14.elt70, ptr %self.repack13.repack69, align 1
  %self.repack13.repack71 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt14.elt72 = extractvalue [9 x i8] %.elt14, 7
  store i8 %.elt14.elt72, ptr %self.repack13.repack71, align 1
  %self.repack13.repack73 = getelementptr inbounds nuw i8, ptr %self, i64 17
  %.elt14.elt74 = extractvalue [9 x i8] %.elt14, 8
  store i8 %.elt14.elt74, ptr %self.repack13.repack73, align 1
  %self.repack15 = getelementptr inbounds nuw i8, ptr %self, i64 18
  %.elt16 = extractvalue [16 x [9 x i8]] %0, 2
  %.elt16.elt = extractvalue [9 x i8] %.elt16, 0
  store i8 %.elt16.elt, ptr %self.repack15, align 1
  %self.repack15.repack75 = getelementptr inbounds nuw i8, ptr %self, i64 19
  %.elt16.elt76 = extractvalue [9 x i8] %.elt16, 1
  store i8 %.elt16.elt76, ptr %self.repack15.repack75, align 1
  %self.repack15.repack77 = getelementptr inbounds nuw i8, ptr %self, i64 20
  %.elt16.elt78 = extractvalue [9 x i8] %.elt16, 2
  store i8 %.elt16.elt78, ptr %self.repack15.repack77, align 1
  %self.repack15.repack79 = getelementptr inbounds nuw i8, ptr %self, i64 21
  %.elt16.elt80 = extractvalue [9 x i8] %.elt16, 3
  store i8 %.elt16.elt80, ptr %self.repack15.repack79, align 1
  %self.repack15.repack81 = getelementptr inbounds nuw i8, ptr %self, i64 22
  %.elt16.elt82 = extractvalue [9 x i8] %.elt16, 4
  store i8 %.elt16.elt82, ptr %self.repack15.repack81, align 1
  %self.repack15.repack83 = getelementptr inbounds nuw i8, ptr %self, i64 23
  %.elt16.elt84 = extractvalue [9 x i8] %.elt16, 5
  store i8 %.elt16.elt84, ptr %self.repack15.repack83, align 1
  %self.repack15.repack85 = getelementptr inbounds nuw i8, ptr %self, i64 24
  %.elt16.elt86 = extractvalue [9 x i8] %.elt16, 6
  store i8 %.elt16.elt86, ptr %self.repack15.repack85, align 1
  %self.repack15.repack87 = getelementptr inbounds nuw i8, ptr %self, i64 25
  %.elt16.elt88 = extractvalue [9 x i8] %.elt16, 7
  store i8 %.elt16.elt88, ptr %self.repack15.repack87, align 1
  %self.repack15.repack89 = getelementptr inbounds nuw i8, ptr %self, i64 26
  %.elt16.elt90 = extractvalue [9 x i8] %.elt16, 8
  store i8 %.elt16.elt90, ptr %self.repack15.repack89, align 1
  %self.repack17 = getelementptr inbounds nuw i8, ptr %self, i64 27
  %.elt18 = extractvalue [16 x [9 x i8]] %0, 3
  %.elt18.elt = extractvalue [9 x i8] %.elt18, 0
  store i8 %.elt18.elt, ptr %self.repack17, align 1
  %self.repack17.repack91 = getelementptr inbounds nuw i8, ptr %self, i64 28
  %.elt18.elt92 = extractvalue [9 x i8] %.elt18, 1
  store i8 %.elt18.elt92, ptr %self.repack17.repack91, align 1
  %self.repack17.repack93 = getelementptr inbounds nuw i8, ptr %self, i64 29
  %.elt18.elt94 = extractvalue [9 x i8] %.elt18, 2
  store i8 %.elt18.elt94, ptr %self.repack17.repack93, align 1
  %self.repack17.repack95 = getelementptr inbounds nuw i8, ptr %self, i64 30
  %.elt18.elt96 = extractvalue [9 x i8] %.elt18, 3
  store i8 %.elt18.elt96, ptr %self.repack17.repack95, align 1
  %self.repack17.repack97 = getelementptr inbounds nuw i8, ptr %self, i64 31
  %.elt18.elt98 = extractvalue [9 x i8] %.elt18, 4
  store i8 %.elt18.elt98, ptr %self.repack17.repack97, align 1
  %self.repack17.repack99 = getelementptr inbounds nuw i8, ptr %self, i64 32
  %.elt18.elt100 = extractvalue [9 x i8] %.elt18, 5
  store i8 %.elt18.elt100, ptr %self.repack17.repack99, align 1
  %self.repack17.repack101 = getelementptr inbounds nuw i8, ptr %self, i64 33
  %.elt18.elt102 = extractvalue [9 x i8] %.elt18, 6
  store i8 %.elt18.elt102, ptr %self.repack17.repack101, align 1
  %self.repack17.repack103 = getelementptr inbounds nuw i8, ptr %self, i64 34
  %.elt18.elt104 = extractvalue [9 x i8] %.elt18, 7
  store i8 %.elt18.elt104, ptr %self.repack17.repack103, align 1
  %self.repack17.repack105 = getelementptr inbounds nuw i8, ptr %self, i64 35
  %.elt18.elt106 = extractvalue [9 x i8] %.elt18, 8
  store i8 %.elt18.elt106, ptr %self.repack17.repack105, align 1
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 36
  %.elt20 = extractvalue [16 x [9 x i8]] %0, 4
  %.elt20.elt = extractvalue [9 x i8] %.elt20, 0
  store i8 %.elt20.elt, ptr %self.repack19, align 1
  %self.repack19.repack107 = getelementptr inbounds nuw i8, ptr %self, i64 37
  %.elt20.elt108 = extractvalue [9 x i8] %.elt20, 1
  store i8 %.elt20.elt108, ptr %self.repack19.repack107, align 1
  %self.repack19.repack109 = getelementptr inbounds nuw i8, ptr %self, i64 38
  %.elt20.elt110 = extractvalue [9 x i8] %.elt20, 2
  store i8 %.elt20.elt110, ptr %self.repack19.repack109, align 1
  %self.repack19.repack111 = getelementptr inbounds nuw i8, ptr %self, i64 39
  %.elt20.elt112 = extractvalue [9 x i8] %.elt20, 3
  store i8 %.elt20.elt112, ptr %self.repack19.repack111, align 1
  %self.repack19.repack113 = getelementptr inbounds nuw i8, ptr %self, i64 40
  %.elt20.elt114 = extractvalue [9 x i8] %.elt20, 4
  store i8 %.elt20.elt114, ptr %self.repack19.repack113, align 1
  %self.repack19.repack115 = getelementptr inbounds nuw i8, ptr %self, i64 41
  %.elt20.elt116 = extractvalue [9 x i8] %.elt20, 5
  store i8 %.elt20.elt116, ptr %self.repack19.repack115, align 1
  %self.repack19.repack117 = getelementptr inbounds nuw i8, ptr %self, i64 42
  %.elt20.elt118 = extractvalue [9 x i8] %.elt20, 6
  store i8 %.elt20.elt118, ptr %self.repack19.repack117, align 1
  %self.repack19.repack119 = getelementptr inbounds nuw i8, ptr %self, i64 43
  %.elt20.elt120 = extractvalue [9 x i8] %.elt20, 7
  store i8 %.elt20.elt120, ptr %self.repack19.repack119, align 1
  %self.repack19.repack121 = getelementptr inbounds nuw i8, ptr %self, i64 44
  %.elt20.elt122 = extractvalue [9 x i8] %.elt20, 8
  store i8 %.elt20.elt122, ptr %self.repack19.repack121, align 1
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 45
  %.elt22 = extractvalue [16 x [9 x i8]] %0, 5
  %.elt22.elt = extractvalue [9 x i8] %.elt22, 0
  store i8 %.elt22.elt, ptr %self.repack21, align 1
  %self.repack21.repack123 = getelementptr inbounds nuw i8, ptr %self, i64 46
  %.elt22.elt124 = extractvalue [9 x i8] %.elt22, 1
  store i8 %.elt22.elt124, ptr %self.repack21.repack123, align 1
  %self.repack21.repack125 = getelementptr inbounds nuw i8, ptr %self, i64 47
  %.elt22.elt126 = extractvalue [9 x i8] %.elt22, 2
  store i8 %.elt22.elt126, ptr %self.repack21.repack125, align 1
  %self.repack21.repack127 = getelementptr inbounds nuw i8, ptr %self, i64 48
  %.elt22.elt128 = extractvalue [9 x i8] %.elt22, 3
  store i8 %.elt22.elt128, ptr %self.repack21.repack127, align 1
  %self.repack21.repack129 = getelementptr inbounds nuw i8, ptr %self, i64 49
  %.elt22.elt130 = extractvalue [9 x i8] %.elt22, 4
  store i8 %.elt22.elt130, ptr %self.repack21.repack129, align 1
  %self.repack21.repack131 = getelementptr inbounds nuw i8, ptr %self, i64 50
  %.elt22.elt132 = extractvalue [9 x i8] %.elt22, 5
  store i8 %.elt22.elt132, ptr %self.repack21.repack131, align 1
  %self.repack21.repack133 = getelementptr inbounds nuw i8, ptr %self, i64 51
  %.elt22.elt134 = extractvalue [9 x i8] %.elt22, 6
  store i8 %.elt22.elt134, ptr %self.repack21.repack133, align 1
  %self.repack21.repack135 = getelementptr inbounds nuw i8, ptr %self, i64 52
  %.elt22.elt136 = extractvalue [9 x i8] %.elt22, 7
  store i8 %.elt22.elt136, ptr %self.repack21.repack135, align 1
  %self.repack21.repack137 = getelementptr inbounds nuw i8, ptr %self, i64 53
  %.elt22.elt138 = extractvalue [9 x i8] %.elt22, 8
  store i8 %.elt22.elt138, ptr %self.repack21.repack137, align 1
  %self.repack23 = getelementptr inbounds nuw i8, ptr %self, i64 54
  %.elt24 = extractvalue [16 x [9 x i8]] %0, 6
  %.elt24.elt = extractvalue [9 x i8] %.elt24, 0
  store i8 %.elt24.elt, ptr %self.repack23, align 1
  %self.repack23.repack139 = getelementptr inbounds nuw i8, ptr %self, i64 55
  %.elt24.elt140 = extractvalue [9 x i8] %.elt24, 1
  store i8 %.elt24.elt140, ptr %self.repack23.repack139, align 1
  %self.repack23.repack141 = getelementptr inbounds nuw i8, ptr %self, i64 56
  %.elt24.elt142 = extractvalue [9 x i8] %.elt24, 2
  store i8 %.elt24.elt142, ptr %self.repack23.repack141, align 1
  %self.repack23.repack143 = getelementptr inbounds nuw i8, ptr %self, i64 57
  %.elt24.elt144 = extractvalue [9 x i8] %.elt24, 3
  store i8 %.elt24.elt144, ptr %self.repack23.repack143, align 1
  %self.repack23.repack145 = getelementptr inbounds nuw i8, ptr %self, i64 58
  %.elt24.elt146 = extractvalue [9 x i8] %.elt24, 4
  store i8 %.elt24.elt146, ptr %self.repack23.repack145, align 1
  %self.repack23.repack147 = getelementptr inbounds nuw i8, ptr %self, i64 59
  %.elt24.elt148 = extractvalue [9 x i8] %.elt24, 5
  store i8 %.elt24.elt148, ptr %self.repack23.repack147, align 1
  %self.repack23.repack149 = getelementptr inbounds nuw i8, ptr %self, i64 60
  %.elt24.elt150 = extractvalue [9 x i8] %.elt24, 6
  store i8 %.elt24.elt150, ptr %self.repack23.repack149, align 1
  %self.repack23.repack151 = getelementptr inbounds nuw i8, ptr %self, i64 61
  %.elt24.elt152 = extractvalue [9 x i8] %.elt24, 7
  store i8 %.elt24.elt152, ptr %self.repack23.repack151, align 1
  %self.repack23.repack153 = getelementptr inbounds nuw i8, ptr %self, i64 62
  %.elt24.elt154 = extractvalue [9 x i8] %.elt24, 8
  store i8 %.elt24.elt154, ptr %self.repack23.repack153, align 1
  %self.repack25 = getelementptr inbounds nuw i8, ptr %self, i64 63
  %.elt26 = extractvalue [16 x [9 x i8]] %0, 7
  %.elt26.elt = extractvalue [9 x i8] %.elt26, 0
  store i8 %.elt26.elt, ptr %self.repack25, align 1
  %self.repack25.repack155 = getelementptr inbounds nuw i8, ptr %self, i64 64
  %.elt26.elt156 = extractvalue [9 x i8] %.elt26, 1
  store i8 %.elt26.elt156, ptr %self.repack25.repack155, align 1
  %self.repack25.repack157 = getelementptr inbounds nuw i8, ptr %self, i64 65
  %.elt26.elt158 = extractvalue [9 x i8] %.elt26, 2
  store i8 %.elt26.elt158, ptr %self.repack25.repack157, align 1
  %self.repack25.repack159 = getelementptr inbounds nuw i8, ptr %self, i64 66
  %.elt26.elt160 = extractvalue [9 x i8] %.elt26, 3
  store i8 %.elt26.elt160, ptr %self.repack25.repack159, align 1
  %self.repack25.repack161 = getelementptr inbounds nuw i8, ptr %self, i64 67
  %.elt26.elt162 = extractvalue [9 x i8] %.elt26, 4
  store i8 %.elt26.elt162, ptr %self.repack25.repack161, align 1
  %self.repack25.repack163 = getelementptr inbounds nuw i8, ptr %self, i64 68
  %.elt26.elt164 = extractvalue [9 x i8] %.elt26, 5
  store i8 %.elt26.elt164, ptr %self.repack25.repack163, align 1
  %self.repack25.repack165 = getelementptr inbounds nuw i8, ptr %self, i64 69
  %.elt26.elt166 = extractvalue [9 x i8] %.elt26, 6
  store i8 %.elt26.elt166, ptr %self.repack25.repack165, align 1
  %self.repack25.repack167 = getelementptr inbounds nuw i8, ptr %self, i64 70
  %.elt26.elt168 = extractvalue [9 x i8] %.elt26, 7
  store i8 %.elt26.elt168, ptr %self.repack25.repack167, align 1
  %self.repack25.repack169 = getelementptr inbounds nuw i8, ptr %self, i64 71
  %.elt26.elt170 = extractvalue [9 x i8] %.elt26, 8
  store i8 %.elt26.elt170, ptr %self.repack25.repack169, align 1
  %self.repack27 = getelementptr inbounds nuw i8, ptr %self, i64 72
  %.elt28 = extractvalue [16 x [9 x i8]] %0, 8
  %.elt28.elt = extractvalue [9 x i8] %.elt28, 0
  store i8 %.elt28.elt, ptr %self.repack27, align 1
  %self.repack27.repack171 = getelementptr inbounds nuw i8, ptr %self, i64 73
  %.elt28.elt172 = extractvalue [9 x i8] %.elt28, 1
  store i8 %.elt28.elt172, ptr %self.repack27.repack171, align 1
  %self.repack27.repack173 = getelementptr inbounds nuw i8, ptr %self, i64 74
  %.elt28.elt174 = extractvalue [9 x i8] %.elt28, 2
  store i8 %.elt28.elt174, ptr %self.repack27.repack173, align 1
  %self.repack27.repack175 = getelementptr inbounds nuw i8, ptr %self, i64 75
  %.elt28.elt176 = extractvalue [9 x i8] %.elt28, 3
  store i8 %.elt28.elt176, ptr %self.repack27.repack175, align 1
  %self.repack27.repack177 = getelementptr inbounds nuw i8, ptr %self, i64 76
  %.elt28.elt178 = extractvalue [9 x i8] %.elt28, 4
  store i8 %.elt28.elt178, ptr %self.repack27.repack177, align 1
  %self.repack27.repack179 = getelementptr inbounds nuw i8, ptr %self, i64 77
  %.elt28.elt180 = extractvalue [9 x i8] %.elt28, 5
  store i8 %.elt28.elt180, ptr %self.repack27.repack179, align 1
  %self.repack27.repack181 = getelementptr inbounds nuw i8, ptr %self, i64 78
  %.elt28.elt182 = extractvalue [9 x i8] %.elt28, 6
  store i8 %.elt28.elt182, ptr %self.repack27.repack181, align 1
  %self.repack27.repack183 = getelementptr inbounds nuw i8, ptr %self, i64 79
  %.elt28.elt184 = extractvalue [9 x i8] %.elt28, 7
  store i8 %.elt28.elt184, ptr %self.repack27.repack183, align 1
  %self.repack27.repack185 = getelementptr inbounds nuw i8, ptr %self, i64 80
  %.elt28.elt186 = extractvalue [9 x i8] %.elt28, 8
  store i8 %.elt28.elt186, ptr %self.repack27.repack185, align 1
  %self.repack29 = getelementptr inbounds nuw i8, ptr %self, i64 81
  %.elt30 = extractvalue [16 x [9 x i8]] %0, 9
  %.elt30.elt = extractvalue [9 x i8] %.elt30, 0
  store i8 %.elt30.elt, ptr %self.repack29, align 1
  %self.repack29.repack187 = getelementptr inbounds nuw i8, ptr %self, i64 82
  %.elt30.elt188 = extractvalue [9 x i8] %.elt30, 1
  store i8 %.elt30.elt188, ptr %self.repack29.repack187, align 1
  %self.repack29.repack189 = getelementptr inbounds nuw i8, ptr %self, i64 83
  %.elt30.elt190 = extractvalue [9 x i8] %.elt30, 2
  store i8 %.elt30.elt190, ptr %self.repack29.repack189, align 1
  %self.repack29.repack191 = getelementptr inbounds nuw i8, ptr %self, i64 84
  %.elt30.elt192 = extractvalue [9 x i8] %.elt30, 3
  store i8 %.elt30.elt192, ptr %self.repack29.repack191, align 1
  %self.repack29.repack193 = getelementptr inbounds nuw i8, ptr %self, i64 85
  %.elt30.elt194 = extractvalue [9 x i8] %.elt30, 4
  store i8 %.elt30.elt194, ptr %self.repack29.repack193, align 1
  %self.repack29.repack195 = getelementptr inbounds nuw i8, ptr %self, i64 86
  %.elt30.elt196 = extractvalue [9 x i8] %.elt30, 5
  store i8 %.elt30.elt196, ptr %self.repack29.repack195, align 1
  %self.repack29.repack197 = getelementptr inbounds nuw i8, ptr %self, i64 87
  %.elt30.elt198 = extractvalue [9 x i8] %.elt30, 6
  store i8 %.elt30.elt198, ptr %self.repack29.repack197, align 1
  %self.repack29.repack199 = getelementptr inbounds nuw i8, ptr %self, i64 88
  %.elt30.elt200 = extractvalue [9 x i8] %.elt30, 7
  store i8 %.elt30.elt200, ptr %self.repack29.repack199, align 1
  %self.repack29.repack201 = getelementptr inbounds nuw i8, ptr %self, i64 89
  %.elt30.elt202 = extractvalue [9 x i8] %.elt30, 8
  store i8 %.elt30.elt202, ptr %self.repack29.repack201, align 1
  %self.repack31 = getelementptr inbounds nuw i8, ptr %self, i64 90
  %.elt32 = extractvalue [16 x [9 x i8]] %0, 10
  %.elt32.elt = extractvalue [9 x i8] %.elt32, 0
  store i8 %.elt32.elt, ptr %self.repack31, align 1
  %self.repack31.repack203 = getelementptr inbounds nuw i8, ptr %self, i64 91
  %.elt32.elt204 = extractvalue [9 x i8] %.elt32, 1
  store i8 %.elt32.elt204, ptr %self.repack31.repack203, align 1
  %self.repack31.repack205 = getelementptr inbounds nuw i8, ptr %self, i64 92
  %.elt32.elt206 = extractvalue [9 x i8] %.elt32, 2
  store i8 %.elt32.elt206, ptr %self.repack31.repack205, align 1
  %self.repack31.repack207 = getelementptr inbounds nuw i8, ptr %self, i64 93
  %.elt32.elt208 = extractvalue [9 x i8] %.elt32, 3
  store i8 %.elt32.elt208, ptr %self.repack31.repack207, align 1
  %self.repack31.repack209 = getelementptr inbounds nuw i8, ptr %self, i64 94
  %.elt32.elt210 = extractvalue [9 x i8] %.elt32, 4
  store i8 %.elt32.elt210, ptr %self.repack31.repack209, align 1
  %self.repack31.repack211 = getelementptr inbounds nuw i8, ptr %self, i64 95
  %.elt32.elt212 = extractvalue [9 x i8] %.elt32, 5
  store i8 %.elt32.elt212, ptr %self.repack31.repack211, align 1
  %self.repack31.repack213 = getelementptr inbounds nuw i8, ptr %self, i64 96
  %.elt32.elt214 = extractvalue [9 x i8] %.elt32, 6
  store i8 %.elt32.elt214, ptr %self.repack31.repack213, align 1
  %self.repack31.repack215 = getelementptr inbounds nuw i8, ptr %self, i64 97
  %.elt32.elt216 = extractvalue [9 x i8] %.elt32, 7
  store i8 %.elt32.elt216, ptr %self.repack31.repack215, align 1
  %self.repack31.repack217 = getelementptr inbounds nuw i8, ptr %self, i64 98
  %.elt32.elt218 = extractvalue [9 x i8] %.elt32, 8
  store i8 %.elt32.elt218, ptr %self.repack31.repack217, align 1
  %self.repack33 = getelementptr inbounds nuw i8, ptr %self, i64 99
  %.elt34 = extractvalue [16 x [9 x i8]] %0, 11
  %.elt34.elt = extractvalue [9 x i8] %.elt34, 0
  store i8 %.elt34.elt, ptr %self.repack33, align 1
  %self.repack33.repack219 = getelementptr inbounds nuw i8, ptr %self, i64 100
  %.elt34.elt220 = extractvalue [9 x i8] %.elt34, 1
  store i8 %.elt34.elt220, ptr %self.repack33.repack219, align 1
  %self.repack33.repack221 = getelementptr inbounds nuw i8, ptr %self, i64 101
  %.elt34.elt222 = extractvalue [9 x i8] %.elt34, 2
  store i8 %.elt34.elt222, ptr %self.repack33.repack221, align 1
  %self.repack33.repack223 = getelementptr inbounds nuw i8, ptr %self, i64 102
  %.elt34.elt224 = extractvalue [9 x i8] %.elt34, 3
  store i8 %.elt34.elt224, ptr %self.repack33.repack223, align 1
  %self.repack33.repack225 = getelementptr inbounds nuw i8, ptr %self, i64 103
  %.elt34.elt226 = extractvalue [9 x i8] %.elt34, 4
  store i8 %.elt34.elt226, ptr %self.repack33.repack225, align 1
  %self.repack33.repack227 = getelementptr inbounds nuw i8, ptr %self, i64 104
  %.elt34.elt228 = extractvalue [9 x i8] %.elt34, 5
  store i8 %.elt34.elt228, ptr %self.repack33.repack227, align 1
  %self.repack33.repack229 = getelementptr inbounds nuw i8, ptr %self, i64 105
  %.elt34.elt230 = extractvalue [9 x i8] %.elt34, 6
  store i8 %.elt34.elt230, ptr %self.repack33.repack229, align 1
  %self.repack33.repack231 = getelementptr inbounds nuw i8, ptr %self, i64 106
  %.elt34.elt232 = extractvalue [9 x i8] %.elt34, 7
  store i8 %.elt34.elt232, ptr %self.repack33.repack231, align 1
  %self.repack33.repack233 = getelementptr inbounds nuw i8, ptr %self, i64 107
  %.elt34.elt234 = extractvalue [9 x i8] %.elt34, 8
  store i8 %.elt34.elt234, ptr %self.repack33.repack233, align 1
  %self.repack35 = getelementptr inbounds nuw i8, ptr %self, i64 108
  %.elt36 = extractvalue [16 x [9 x i8]] %0, 12
  %.elt36.elt = extractvalue [9 x i8] %.elt36, 0
  store i8 %.elt36.elt, ptr %self.repack35, align 1
  %self.repack35.repack235 = getelementptr inbounds nuw i8, ptr %self, i64 109
  %.elt36.elt236 = extractvalue [9 x i8] %.elt36, 1
  store i8 %.elt36.elt236, ptr %self.repack35.repack235, align 1
  %self.repack35.repack237 = getelementptr inbounds nuw i8, ptr %self, i64 110
  %.elt36.elt238 = extractvalue [9 x i8] %.elt36, 2
  store i8 %.elt36.elt238, ptr %self.repack35.repack237, align 1
  %self.repack35.repack239 = getelementptr inbounds nuw i8, ptr %self, i64 111
  %.elt36.elt240 = extractvalue [9 x i8] %.elt36, 3
  store i8 %.elt36.elt240, ptr %self.repack35.repack239, align 1
  %self.repack35.repack241 = getelementptr inbounds nuw i8, ptr %self, i64 112
  %.elt36.elt242 = extractvalue [9 x i8] %.elt36, 4
  store i8 %.elt36.elt242, ptr %self.repack35.repack241, align 1
  %self.repack35.repack243 = getelementptr inbounds nuw i8, ptr %self, i64 113
  %.elt36.elt244 = extractvalue [9 x i8] %.elt36, 5
  store i8 %.elt36.elt244, ptr %self.repack35.repack243, align 1
  %self.repack35.repack245 = getelementptr inbounds nuw i8, ptr %self, i64 114
  %.elt36.elt246 = extractvalue [9 x i8] %.elt36, 6
  store i8 %.elt36.elt246, ptr %self.repack35.repack245, align 1
  %self.repack35.repack247 = getelementptr inbounds nuw i8, ptr %self, i64 115
  %.elt36.elt248 = extractvalue [9 x i8] %.elt36, 7
  store i8 %.elt36.elt248, ptr %self.repack35.repack247, align 1
  %self.repack35.repack249 = getelementptr inbounds nuw i8, ptr %self, i64 116
  %.elt36.elt250 = extractvalue [9 x i8] %.elt36, 8
  store i8 %.elt36.elt250, ptr %self.repack35.repack249, align 1
  %self.repack37 = getelementptr inbounds nuw i8, ptr %self, i64 117
  %.elt38 = extractvalue [16 x [9 x i8]] %0, 13
  %.elt38.elt = extractvalue [9 x i8] %.elt38, 0
  store i8 %.elt38.elt, ptr %self.repack37, align 1
  %self.repack37.repack251 = getelementptr inbounds nuw i8, ptr %self, i64 118
  %.elt38.elt252 = extractvalue [9 x i8] %.elt38, 1
  store i8 %.elt38.elt252, ptr %self.repack37.repack251, align 1
  %self.repack37.repack253 = getelementptr inbounds nuw i8, ptr %self, i64 119
  %.elt38.elt254 = extractvalue [9 x i8] %.elt38, 2
  store i8 %.elt38.elt254, ptr %self.repack37.repack253, align 1
  %self.repack37.repack255 = getelementptr inbounds nuw i8, ptr %self, i64 120
  %.elt38.elt256 = extractvalue [9 x i8] %.elt38, 3
  store i8 %.elt38.elt256, ptr %self.repack37.repack255, align 1
  %self.repack37.repack257 = getelementptr inbounds nuw i8, ptr %self, i64 121
  %.elt38.elt258 = extractvalue [9 x i8] %.elt38, 4
  store i8 %.elt38.elt258, ptr %self.repack37.repack257, align 1
  %self.repack37.repack259 = getelementptr inbounds nuw i8, ptr %self, i64 122
  %.elt38.elt260 = extractvalue [9 x i8] %.elt38, 5
  store i8 %.elt38.elt260, ptr %self.repack37.repack259, align 1
  %self.repack37.repack261 = getelementptr inbounds nuw i8, ptr %self, i64 123
  %.elt38.elt262 = extractvalue [9 x i8] %.elt38, 6
  store i8 %.elt38.elt262, ptr %self.repack37.repack261, align 1
  %self.repack37.repack263 = getelementptr inbounds nuw i8, ptr %self, i64 124
  %.elt38.elt264 = extractvalue [9 x i8] %.elt38, 7
  store i8 %.elt38.elt264, ptr %self.repack37.repack263, align 1
  %self.repack37.repack265 = getelementptr inbounds nuw i8, ptr %self, i64 125
  %.elt38.elt266 = extractvalue [9 x i8] %.elt38, 8
  store i8 %.elt38.elt266, ptr %self.repack37.repack265, align 1
  %self.repack39 = getelementptr inbounds nuw i8, ptr %self, i64 126
  %.elt40 = extractvalue [16 x [9 x i8]] %0, 14
  %.elt40.elt = extractvalue [9 x i8] %.elt40, 0
  store i8 %.elt40.elt, ptr %self.repack39, align 1
  %self.repack39.repack267 = getelementptr inbounds nuw i8, ptr %self, i64 127
  %.elt40.elt268 = extractvalue [9 x i8] %.elt40, 1
  store i8 %.elt40.elt268, ptr %self.repack39.repack267, align 1
  %self.repack39.repack269 = getelementptr inbounds nuw i8, ptr %self, i64 128
  %.elt40.elt270 = extractvalue [9 x i8] %.elt40, 2
  store i8 %.elt40.elt270, ptr %self.repack39.repack269, align 1
  %self.repack39.repack271 = getelementptr inbounds nuw i8, ptr %self, i64 129
  %.elt40.elt272 = extractvalue [9 x i8] %.elt40, 3
  store i8 %.elt40.elt272, ptr %self.repack39.repack271, align 1
  %self.repack39.repack273 = getelementptr inbounds nuw i8, ptr %self, i64 130
  %.elt40.elt274 = extractvalue [9 x i8] %.elt40, 4
  store i8 %.elt40.elt274, ptr %self.repack39.repack273, align 1
  %self.repack39.repack275 = getelementptr inbounds nuw i8, ptr %self, i64 131
  %.elt40.elt276 = extractvalue [9 x i8] %.elt40, 5
  store i8 %.elt40.elt276, ptr %self.repack39.repack275, align 1
  %self.repack39.repack277 = getelementptr inbounds nuw i8, ptr %self, i64 132
  %.elt40.elt278 = extractvalue [9 x i8] %.elt40, 6
  store i8 %.elt40.elt278, ptr %self.repack39.repack277, align 1
  %self.repack39.repack279 = getelementptr inbounds nuw i8, ptr %self, i64 133
  %.elt40.elt280 = extractvalue [9 x i8] %.elt40, 7
  store i8 %.elt40.elt280, ptr %self.repack39.repack279, align 1
  %self.repack39.repack281 = getelementptr inbounds nuw i8, ptr %self, i64 134
  %.elt40.elt282 = extractvalue [9 x i8] %.elt40, 8
  store i8 %.elt40.elt282, ptr %self.repack39.repack281, align 1
  %self.repack41 = getelementptr inbounds nuw i8, ptr %self, i64 135
  %.elt42 = extractvalue [16 x [9 x i8]] %0, 15
  %.elt42.elt = extractvalue [9 x i8] %.elt42, 0
  store i8 %.elt42.elt, ptr %self.repack41, align 1
  %self.repack41.repack283 = getelementptr inbounds nuw i8, ptr %self, i64 136
  %.elt42.elt284 = extractvalue [9 x i8] %.elt42, 1
  store i8 %.elt42.elt284, ptr %self.repack41.repack283, align 1
  %self.repack41.repack285 = getelementptr inbounds nuw i8, ptr %self, i64 137
  %.elt42.elt286 = extractvalue [9 x i8] %.elt42, 2
  store i8 %.elt42.elt286, ptr %self.repack41.repack285, align 1
  %self.repack41.repack287 = getelementptr inbounds nuw i8, ptr %self, i64 138
  %.elt42.elt288 = extractvalue [9 x i8] %.elt42, 3
  store i8 %.elt42.elt288, ptr %self.repack41.repack287, align 1
  %self.repack41.repack289 = getelementptr inbounds nuw i8, ptr %self, i64 139
  %.elt42.elt290 = extractvalue [9 x i8] %.elt42, 4
  store i8 %.elt42.elt290, ptr %self.repack41.repack289, align 1
  %self.repack41.repack291 = getelementptr inbounds nuw i8, ptr %self, i64 140
  %.elt42.elt292 = extractvalue [9 x i8] %.elt42, 5
  store i8 %.elt42.elt292, ptr %self.repack41.repack291, align 1
  %self.repack41.repack293 = getelementptr inbounds nuw i8, ptr %self, i64 141
  %.elt42.elt294 = extractvalue [9 x i8] %.elt42, 6
  store i8 %.elt42.elt294, ptr %self.repack41.repack293, align 1
  %self.repack41.repack295 = getelementptr inbounds nuw i8, ptr %self, i64 142
  %.elt42.elt296 = extractvalue [9 x i8] %.elt42, 7
  store i8 %.elt42.elt296, ptr %self.repack41.repack295, align 1
  %self.repack41.repack297 = getelementptr inbounds nuw i8, ptr %self, i64 143
  %.elt42.elt298 = extractvalue [9 x i8] %.elt42, 8
  store i8 %.elt42.elt298, ptr %self.repack41.repack297, align 1
  br label %cond

then:                                             ; preds = %cond
  %tmp1 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr [9 x i8], ptr %self, i64 %tmp1
  %tmp3.unpack = load i8, ptr %tmp_index.i, align 1
  %1 = insertvalue [9 x i8] poison, i8 %tmp3.unpack, 0
  %tmp3.elt299 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 1
  %tmp3.unpack300 = load i8, ptr %tmp3.elt299, align 1
  %2 = insertvalue [9 x i8] %1, i8 %tmp3.unpack300, 1
  %tmp3.elt301 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 2
  %tmp3.unpack302 = load i8, ptr %tmp3.elt301, align 1
  %3 = insertvalue [9 x i8] %2, i8 %tmp3.unpack302, 2
  %tmp3.elt303 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 3
  %tmp3.unpack304 = load i8, ptr %tmp3.elt303, align 1
  %4 = insertvalue [9 x i8] %3, i8 %tmp3.unpack304, 3
  %tmp3.elt305 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 4
  %tmp3.unpack306 = load i8, ptr %tmp3.elt305, align 1
  %5 = insertvalue [9 x i8] %4, i8 %tmp3.unpack306, 4
  %tmp3.elt307 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 5
  %tmp3.unpack308 = load i8, ptr %tmp3.elt307, align 1
  %6 = insertvalue [9 x i8] %5, i8 %tmp3.unpack308, 5
  %tmp3.elt309 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 6
  %tmp3.unpack310 = load i8, ptr %tmp3.elt309, align 1
  %7 = insertvalue [9 x i8] %6, i8 %tmp3.unpack310, 6
  %tmp3.elt311 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 7
  %tmp3.unpack312 = load i8, ptr %tmp3.elt311, align 1
  %8 = insertvalue [9 x i8] %7, i8 %tmp3.unpack312, 7
  %tmp3.elt313 = getelementptr inbounds nuw i8, ptr %tmp_index.i, i64 8
  %tmp3.unpack314 = load i8, ptr %tmp3.elt313, align 1
  %tmp3315 = insertvalue [9 x i8] %8, i8 %tmp3.unpack314, 8
  %fun_call_tmp4 = call {} @"[u8; 9]:drop"([9 x i8] %tmp3315)
  %tmp5 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp5, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp.i11 = icmp ult i64 %storemerge, 16
  br i1 %tmp.i11, label %then, label %else

else:                                             ; preds = %cond
  %tmp7 = load i64, ptr %idx, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
  ret {} zeroinitializer
}

define i1 @"[[u8; 9]; 16]:eq"(ptr %0, ptr %1) {
entry:
  %idx = alloca i64, align 8
  %other = alloca ptr, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store ptr %1, ptr %other, align 8
  br label %cond

common.ret:                                       ; preds = %then2, %else
  %common.ret.op = phi i1 [ true, %else ], [ false, %then2 ]
  ret i1 %common.ret.op

cond:                                             ; preds = %else4, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %else4 ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp.i32 = icmp ult i64 %storemerge, 16
  br i1 %tmp.i32, label %cond3, label %else

else:                                             ; preds = %cond
  %tmp24 = load i64, ptr %idx, align 4
  %fun_call_tmp25 = call {} @"u64:drop"(i64 %tmp24)
  br label %common.ret

then2:                                            ; preds = %cond3
  %tmp15 = load i64, ptr %idx, align 4
  %fun_call_tmp16 = call {} @"u64:drop"(i64 %tmp15)
  br label %common.ret

cond3:                                            ; preds = %cond
  %tmp5 = load ptr, ptr %self, align 8
  %tmp6 = load i64, ptr %idx, align 4
  %tmp_index.i40 = getelementptr [9 x i8], ptr %tmp5, i64 %tmp6
  %ptr.i35 = insertvalue { ptr, i64 } undef, ptr %tmp_index.i40, 0
  %len.i36 = insertvalue { ptr, i64 } %ptr.i35, i64 9, 1
  %tmp9 = load ptr, ptr %other, align 8
  %tmp_index.i = getelementptr [9 x i8], ptr %tmp9, i64 %tmp6
  %ptr.i = insertvalue { ptr, i64 } undef, ptr %tmp_index.i, 0
  %len.i = insertvalue { ptr, i64 } %ptr.i, i64 9, 1
  %fun_call_tmp13 = call i1 @"[u8]:eq"({ ptr, i64 } %len.i36, { ptr, i64 } %len.i)
  br i1 %fun_call_tmp13, label %else4, label %then2

else4:                                            ; preds = %cond3
  %tmp21 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp21, 1
  br label %cond
}

define i1 @"[u8]:eq"({ ptr, i64 } %0, { ptr, i64 } %1) {
entry:
  %idx = alloca i64, align 8
  %other = alloca { ptr, i64 }, align 8
  %self = alloca { ptr, i64 }, align 8
  %.elt = extractvalue { ptr, i64 } %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack46 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt47 = extractvalue { ptr, i64 } %0, 1
  store i64 %.elt47, ptr %self.repack46, align 8
  %.elt48 = extractvalue { ptr, i64 } %1, 0
  store ptr %.elt48, ptr %other, align 8
  %other.repack49 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %.elt50 = extractvalue { ptr, i64 } %1, 1
  store i64 %.elt50, ptr %other.repack49, align 8
  br label %cond

common.ret:                                       ; preds = %then4, %else
  %common.ret.op = phi i1 [ true, %else ], [ false, %then4 ]
  ret i1 %common.ret.op

cond:                                             ; preds = %else6, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %else6 ]
  store i64 %storemerge, ptr %idx, align 4
  %tmp1.elt51 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp1.unpack52 = load i64, ptr %tmp1.elt51, align 8
  %tmp.i32 = icmp ult i64 %storemerge, %tmp1.unpack52
  br i1 %tmp.i32, label %cond5, label %else

else:                                             ; preds = %cond
  %tmp24 = load i64, ptr %idx, align 4
  %fun_call_tmp25 = call {} @"u64:drop"(i64 %tmp24)
  br label %common.ret

then4:                                            ; preds = %cond5
  %tmp15 = load i64, ptr %idx, align 4
  %fun_call_tmp16 = call {} @"u64:drop"(i64 %tmp15)
  br label %common.ret

cond5:                                            ; preds = %cond
  %tmp7.unpack = load ptr, ptr %self, align 8
  %tmp8 = load i64, ptr %idx, align 4
  %tmp_index.i41 = getelementptr i8, ptr %tmp7.unpack, i64 %tmp8
  %tmp10.unpack = load ptr, ptr %other, align 8
  %tmp11 = load i64, ptr %idx, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp10.unpack, i64 %tmp11
  %tmp.i35 = load i8, ptr %tmp_index.i41, align 1
  %tmp1.i = load i8, ptr %tmp_index.i, align 1
  %tmp2.i.not = icmp eq i8 %tmp.i35, %tmp1.i
  br i1 %tmp2.i.not, label %else6, label %then4

else6:                                            ; preds = %cond5
  %tmp21 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp21, 1
  br label %cond
}

define %"TileMap<W = \22i64 16\22, H = \22i64 9\22>" @"TileMap<W = \22i64 16\22, H = \22i64 9\22>:new"() {
entry:
  ret %"TileMap<W = \22i64 16\22, H = \22i64 9\22>" zeroinitializer
}

define double @"f64:clone"(ptr %0) {
entry:
  %tmp1 = load double, ptr %0, align 8
  ret double %tmp1
}

define {} @"f64:drop"(double %0) {
entry:
  ret {} zeroinitializer
}

define i1 @"&Particle:eq"(ptr %0, ptr %1) {
entry:
  %other.i = alloca ptr, align 8
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %other.i)
  store ptr %tmp3, ptr %other.i, align 8
  %tmp.i9.i.i = load double, ptr %tmp1, align 8
  %tmp1.i10.i.i = load double, ptr %tmp3, align 8
  %tmp2.i11.i.i = fcmp oeq double %tmp.i9.i.i, %tmp1.i10.i.i
  %tmp3.i.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4.y.i.i = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp.i.i.i = load double, ptr %tmp3.i.i, align 8
  %tmp1.i.i.i = load double, ptr %tmp4.y.i.i, align 8
  %tmp2.i.i.i = fcmp oeq double %tmp.i.i.i, %tmp1.i.i.i
  %tmp6.i.i = and i1 %tmp2.i11.i.i, %tmp2.i.i.i
  %tmp3.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp4.i = load ptr, ptr %other.i, align 8
  %tmp4.size.i = getelementptr inbounds nuw i8, ptr %tmp4.i, i64 16
  %tmp.i31.i = load double, ptr %tmp3.i, align 8
  %tmp1.i32.i = load double, ptr %tmp4.size.i, align 8
  %tmp2.i33.i = fcmp oeq double %tmp.i31.i, %tmp1.i32.i
  %tmp6.i = and i1 %tmp6.i.i, %tmp2.i33.i
  %tmp7.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp8.i = load ptr, ptr %other.i, align 8
  %tmp8.speed.i = getelementptr inbounds nuw i8, ptr %tmp8.i, i64 24
  %tmp.i26.i = load double, ptr %tmp7.i, align 8
  %tmp1.i27.i = load double, ptr %tmp8.speed.i, align 8
  %tmp2.i28.i = fcmp oeq double %tmp.i26.i, %tmp1.i27.i
  %tmp10.i = and i1 %tmp6.i, %tmp2.i28.i
  %tmp11.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp12.i = load ptr, ptr %other.i, align 8
  %tmp12.offset.i = getelementptr inbounds nuw i8, ptr %tmp12.i, i64 32
  %tmp.i21.i = load double, ptr %tmp11.i, align 8
  %tmp1.i22.i = load double, ptr %tmp12.offset.i, align 8
  %tmp2.i23.i = fcmp oeq double %tmp.i21.i, %tmp1.i22.i
  %tmp14.i = and i1 %tmp10.i, %tmp2.i23.i
  %tmp15.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 40
  %tmp16.i = load ptr, ptr %other.i, align 8
  %tmp16.color.i = getelementptr inbounds nuw i8, ptr %tmp16.i, i64 40
  %tmp.i.i = load i8, ptr %tmp15.i, align 1
  %tmp1.i.i = load i8, ptr %tmp16.color.i, align 1
  %tmp2.i.i = icmp eq i8 %tmp.i.i, %tmp1.i.i
  %tmp18.i = and i1 %tmp14.i, %tmp2.i.i
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %other.i)
  ret i1 %tmp18.i
}

define %"Vec<T = Particle>" @"Vec<T = Particle>:new"() {
entry:
  %fun_call_tmp = call %"Vec<T = Particle>" @"Vec<T = Particle>:with_capacity"(i64 4)
  ret %"Vec<T = Particle>" %fun_call_tmp
}

define %"Vec<T = Particle>" @"Vec<T = Particle>:with_capacity"(i64 %0) {
entry:
  %fun_call_tmp = call ptr @"mem:alloc<T = Particle>"(i64 %0)
  %tmp_agg = insertvalue %"Vec<T = Particle>" undef, ptr %fun_call_tmp, 0
  %tmp_agg2 = insertvalue %"Vec<T = Particle>" %tmp_agg, i64 0, 1
  %tmp_constructor = insertvalue %"Vec<T = Particle>" %tmp_agg2, i64 %0, 2
  %fun_call_tmp4 = call {} @"u64:drop"(i64 %0)
  ret %"Vec<T = Particle>" %tmp_constructor
}

define ptr @"mem:alloc<T = Particle>"(i64 %0) {
entry:
  %len = alloca i64, align 8
  store i64 %0, ptr %len, align 4
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, 1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %tmp.i11 = mul i64 %0, 41
  %fun_call_tmp6 = call ptr @c_malloc(i64 %tmp.i11)
  %tmp7 = load i64, ptr %len, align 4
  %fun_call_tmp8 = call {} @"u64:drop"(i64 %tmp7)
  ret ptr %fun_call_tmp6
}

define {} @"Vec<T = Particle>:push"(ptr %0, %Particle %1) {
entry:
  %value = alloca %Particle, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store %Particle %1, ptr %value, align 8
  %fun_call_tmp = call {} @"Vec<T = Particle>:reserve"(ptr %0, i64 1)
  %tmp2 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp2 to i64
  %tmp3.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load i64, ptr %tmp3.len, align 4
  %tmp.i24 = mul i64 %tmp4, 41
  %tmp.i21 = add i64 %tmp.i24, %cast
  %cast8 = inttoptr i64 %tmp.i21 to ptr
  %fun_call_tmp9 = call {} @"mem:copy<T = Particle>"(ptr %cast8, ptr nonnull %value)
  %tmp10 = load ptr, ptr %self, align 8
  %tmp10.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  %tmp11 = load i64, ptr %tmp10.len, align 4
  %tmp.i = add i64 %tmp11, 1
  %tmp13.len = getelementptr inbounds nuw i8, ptr %tmp10, i64 8
  store i64 %tmp.i, ptr %tmp13.len, align 4
  %tmp14 = load %Particle, ptr %value, align 8
  %tmp1526 = insertvalue %"mem:Manually_Drop<T = Particle>" poison, %Particle %tmp14, 0
  %fun_call_tmp16 = call {} @"mem:Manually_Drop<T = Particle>:drop"(%"mem:Manually_Drop<T = Particle>" %tmp1526)
  ret {} zeroinitializer
}

define {} @"Vec<T = Particle>:reserve"(ptr %0, i64 %1) {
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
  %fun_call_tmp21 = call ptr @"mem:alloc<T = Particle>"(i64 %tmp20)
  store ptr %fun_call_tmp21, ptr %new_ptr, align 8
  %tmp23.len = getelementptr inbounds nuw i8, ptr %tmp19, i64 8
  %tmp25 = load ptr, ptr %tmp19, align 8
  %tmp26 = load i64, ptr %tmp23.len, align 4
  %fun_call_tmp27 = call {} @"mem:copy_range<T = Particle>"(ptr %fun_call_tmp21, ptr %tmp25, i64 %tmp26)
  %tmp28 = load ptr, ptr %self, align 8
  %tmp29 = load ptr, ptr %tmp28, align 8
  %fun_call_tmp30 = call {} @"mem:free<T = Particle>"(ptr %tmp29)
  %tmp32 = load ptr, ptr %new_ptr, align 8
  store ptr %tmp32, ptr %tmp28, align 8
  br label %merge
}

define {} @"mem:copy_range<T = Particle>"(ptr %0, ptr %1, i64 %2) {
entry:
  %tmp.i = mul i64 %2, 41
  %fun_call_tmp4 = call {} @c_memcpy(ptr %0, ptr %1, i64 %tmp.i)
  %fun_call_tmp6 = call {} @"u64:drop"(i64 %2)
  ret {} zeroinitializer
}

define {} @"mem:free<T = Particle>"(ptr %0) {
entry:
  %fun_call_tmp = call ptr @"mem:heap_count"()
  %tmp = load i64, ptr %fun_call_tmp, align 4
  %tmp.i = add i64 %tmp, -1
  %fun_call_tmp2 = call ptr @"mem:heap_count"()
  store i64 %tmp.i, ptr %fun_call_tmp2, align 4
  %fun_call_tmp4 = call {} @c_free(ptr %0)
  ret {} %fun_call_tmp4
}

define {} @"mem:copy<T = Particle>"(ptr %0, ptr %1) {
entry:
  %fun_call_tmp2 = call {} @c_memcpy(ptr %0, ptr %1, i64 41)
  ret {} zeroinitializer
}

define {} @"mem:Manually_Drop<T = Particle>:drop"(%"mem:Manually_Drop<T = Particle>" %0) {
entry:
  ret {} zeroinitializer
}

define %Particle @"Particle:spawn"(%"Vec_2<T = u64>" %0) {
entry:
  %tmp9 = alloca %"Range<T = f64>", align 8
  %tmp8 = alloca %"Range<T = f64>", align 8
  %tmp716 = alloca %"Range<T = f64>", align 8
  %tmp6 = alloca %"Range<T = f64>", align 8
  %tmp5 = alloca %"Range<T = f64>", align 8
  %color = alloca i8, align 1
  %tmp4 = alloca %"Range<T = u64>", align 8
  %tmp3 = alloca i64, align 8
  %tmp2 = alloca i64, align 8
  %tmp1 = alloca %"Range<T = u64>", align 8
  %screen_size = alloca %"Vec_2<T = u64>", align 8
  %.elt = extractvalue %"Vec_2<T = u64>" %0, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack105 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt106 = extractvalue %"Vec_2<T = u64>" %0, 1
  store i64 %.elt106, ptr %screen_size.repack105, align 4
  store i64 0, ptr %tmp1, align 4
  %tmp1.repack107 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  store i64 3, ptr %tmp1.repack107, align 4
  %fun_call_tmp1 = call i64 @"Vec_2<T = u64>:rand"(ptr nonnull %tmp1)
  store i64 %fun_call_tmp1, ptr %tmp2, align 4
  store i64 0, ptr %tmp3, align 4
  %tmp2.i = icmp eq i64 %fun_call_tmp1, 0
  br i1 %tmp2.i, label %then, label %merge

merge:                                            ; preds = %entry, %then
  %result.off0 = phi i8 [ %extract.t, %then ], [ 7, %entry ]
  store i8 %result.off0, ptr %color, align 1
  %tmp7 = load i64, ptr %screen_size, align 4
  %cast8 = uitofp i64 %tmp7 to double
  store double 0.000000e+00, ptr %tmp5, align 8
  %tmp5.repack112 = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  store double %cast8, ptr %tmp5.repack112, align 8
  %fun_call_tmp10 = call double @"Vec_2<T = f64>:rand"(ptr nonnull %tmp5)
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp11 = load i64, ptr %screen_size.y, align 4
  %cast12 = uitofp i64 %tmp11 to double
  store double 0.000000e+00, ptr %tmp6, align 8
  %tmp6.repack114 = getelementptr inbounds nuw i8, ptr %tmp6, i64 8
  store double %cast12, ptr %tmp6.repack114, align 8
  %fun_call_tmp14 = call double @"Vec_2<T = f64>:rand"(ptr nonnull %tmp6)
  %tmp_agg = insertvalue %"Vec_2<T = f64>" undef, double %fun_call_tmp10, 0
  %tmp_constructor = insertvalue %"Vec_2<T = f64>" %tmp_agg, double %fun_call_tmp14, 1
  store double 1.000000e+00, ptr %tmp716, align 8
  %tmp716.repack116 = getelementptr inbounds nuw i8, ptr %tmp716, i64 8
  store double 2.400000e+00, ptr %tmp716.repack116, align 8
  %fun_call_tmp17 = call double @"Vec_2<T = f64>:rand"(ptr nonnull %tmp716)
  store double 0.000000e+00, ptr %tmp8, align 8
  %tmp8.repack117 = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  store double 5.000000e+00, ptr %tmp8.repack117, align 8
  %fun_call_tmp19 = call double @"Vec_2<T = f64>:rand"(ptr nonnull %tmp8)
  %tmp.i68 = fadd double %fun_call_tmp19, 2.500000e-01
  store double 0.000000e+00, ptr %tmp9, align 8
  %tmp9.repack118 = getelementptr inbounds nuw i8, ptr %tmp9, i64 8
  store double 1.000000e+00, ptr %tmp9.repack118, align 8
  %fun_call_tmp22 = call double @"Vec_2<T = f64>:rand"(ptr nonnull %tmp9)
  %tmp23 = load i8, ptr %color, align 1
  %tmp_agg24 = insertvalue %Particle undef, %"Vec_2<T = f64>" %tmp_constructor, 0
  %tmp_agg25 = insertvalue %Particle %tmp_agg24, double %fun_call_tmp17, 1
  %tmp_agg26 = insertvalue %Particle %tmp_agg25, double %tmp.i68, 2
  %tmp_agg27 = insertvalue %Particle %tmp_agg26, double %fun_call_tmp22, 3
  %tmp_constructor28 = insertvalue %Particle %tmp_agg27, i8 %tmp23, 4
  %tmp29.unpack = load double, ptr %tmp9, align 8
  %tmp29.elt119 = getelementptr inbounds nuw i8, ptr %tmp9, i64 8
  %tmp29.unpack120 = load double, ptr %tmp29.elt119, align 8
  %fun_call_tmp.i102 = call {} @"f64:drop"(double %tmp29.unpack120)
  %fun_call_tmp2.i104 = call {} @"f64:drop"(double %tmp29.unpack)
  %tmp31.unpack = load double, ptr %tmp8, align 8
  %tmp31.elt122 = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  %tmp31.unpack123 = load double, ptr %tmp31.elt122, align 8
  %fun_call_tmp.i97 = call {} @"f64:drop"(double %tmp31.unpack123)
  %fun_call_tmp2.i99 = call {} @"f64:drop"(double %tmp31.unpack)
  %tmp33.unpack = load double, ptr %tmp716, align 8
  %tmp33.elt125 = getelementptr inbounds nuw i8, ptr %tmp716, i64 8
  %tmp33.unpack126 = load double, ptr %tmp33.elt125, align 8
  %fun_call_tmp.i92 = call {} @"f64:drop"(double %tmp33.unpack126)
  %fun_call_tmp2.i94 = call {} @"f64:drop"(double %tmp33.unpack)
  %tmp35.unpack = load double, ptr %tmp6, align 8
  %tmp35.elt128 = getelementptr inbounds nuw i8, ptr %tmp6, i64 8
  %tmp35.unpack129 = load double, ptr %tmp35.elt128, align 8
  %fun_call_tmp.i87 = call {} @"f64:drop"(double %tmp35.unpack129)
  %fun_call_tmp2.i89 = call {} @"f64:drop"(double %tmp35.unpack)
  %tmp37.unpack = load double, ptr %tmp5, align 8
  %tmp37.elt131 = getelementptr inbounds nuw i8, ptr %tmp5, i64 8
  %tmp37.unpack132 = load double, ptr %tmp37.elt131, align 8
  %fun_call_tmp.i82 = call {} @"f64:drop"(double %tmp37.unpack132)
  %fun_call_tmp2.i84 = call {} @"f64:drop"(double %tmp37.unpack)
  %tmp39 = load i8, ptr %color, align 1
  %fun_call_tmp40 = call {} @"u8:drop"(i8 %tmp39)
  %tmp41 = load i64, ptr %tmp3, align 4
  %fun_call_tmp42 = call {} @"u64:drop"(i64 %tmp41)
  %tmp43 = load i64, ptr %tmp2, align 4
  %fun_call_tmp44 = call {} @"u64:drop"(i64 %tmp43)
  %tmp45.unpack = load i64, ptr %tmp1, align 4
  %tmp45.elt134 = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp45.unpack135 = load i64, ptr %tmp45.elt134, align 4
  %fun_call_tmp.i58 = call {} @"u64:drop"(i64 %tmp45.unpack135)
  %fun_call_tmp2.i60 = call {} @"u64:drop"(i64 %tmp45.unpack)
  %tmp47.unpack = load i64, ptr %screen_size, align 4
  %tmp47.elt137 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp47.unpack138 = load i64, ptr %tmp47.elt137, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp47.unpack138)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp47.unpack)
  ret %Particle %tmp_constructor28

then:                                             ; preds = %entry
  store i64 1, ptr %tmp4, align 4
  %tmp4.repack108 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  store i64 8, ptr %tmp4.repack108, align 4
  %fun_call_tmp4 = call i64 @"Vec_2<T = u64>:rand"(ptr nonnull %tmp4)
  %tmp.unpack = load i64, ptr %tmp4, align 4
  %tmp.elt109 = getelementptr inbounds nuw i8, ptr %tmp4, i64 8
  %tmp.unpack110 = load i64, ptr %tmp.elt109, align 4
  %fun_call_tmp.i63 = call {} @"u64:drop"(i64 %tmp.unpack110)
  %fun_call_tmp2.i65 = call {} @"u64:drop"(i64 %tmp.unpack)
  %extract.t = trunc i64 %fun_call_tmp4 to i8
  br label %merge
}

define i64 @"Vec_2<T = u64>:rand"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call i64 @"rng:next"()
  %tmp.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load i64, ptr %tmp.end, align 4
  %tmp3 = load i64, ptr %0, align 4
  %tmp.i13 = sub i64 %tmp1, %tmp3
  %tmp.i16 = urem i64 %fun_call_tmp, %tmp.i13
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load i64, ptr %tmp6, align 4
  %tmp.i = add i64 %tmp.i16, %tmp7
  ret i64 %tmp.i
}

define i64 @"rng:next"() {
entry:
  %tmp = load i64, ptr @seed, align 4
  %tmp.i8 = mul i64 %tmp, 6364136223846793005
  %tmp.i = add i64 %tmp.i8, 1442695040888963407
  store i64 %tmp.i, ptr @seed, align 4
  %tmp.i10 = lshr i64 %tmp.i, 33
  %fun_call_tmp5 = call {} @"u64:drop"(i64 %tmp.i)
  ret i64 %tmp.i10
}

define double @"Vec_2<T = f64>:rand"(ptr %0) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %fun_call_tmp = call i64 @"rng:next"()
  %cast = uitofp i64 %fun_call_tmp to double
  %tmp.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp1 = load double, ptr %tmp.end, align 8
  %tmp3 = load double, ptr %0, align 8
  %tmp.i13 = fsub double %tmp1, %tmp3
  %tmp.i16 = frem double %cast, %tmp.i13
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load double, ptr %tmp6, align 8
  %tmp.i = fadd double %tmp.i16, %tmp7
  ret double %tmp.i
}

define %Player @"Particle:new"() {
entry:
  %tmp0 = alloca %"Range<T = u64>", align 8
  store i64 0, ptr %tmp0, align 4
  %tmp0.repack17 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  store i64 16, ptr %tmp0.repack17, align 4
  %fun_call_tmp1 = call i64 @"Vec_2<T = u64>:rand"(ptr nonnull %tmp0)
  %tmp.i = shl i64 %fun_call_tmp1, 2
  %cast = uitofp i64 %tmp.i to double
  %tmp_agg = insertvalue %"Vec_2<T = f64>" undef, double %cast, 0
  %tmp_constructor = insertvalue %"Vec_2<T = f64>" %tmp_agg, double 1.800000e+01, 1
  %tmp_agg6 = insertvalue %Player undef, %"Vec_2<T = f64>" %tmp_constructor, 0
  %tmp_constructor7 = insertvalue %Player %tmp_agg6, %"Vec_2<T = f64>" zeroinitializer, 1
  %tmp.unpack = load i64, ptr %tmp0, align 4
  %tmp.elt18 = getelementptr inbounds nuw i8, ptr %tmp0, i64 8
  %tmp.unpack19 = load i64, ptr %tmp.elt18, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp.unpack19)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp.unpack)
  ret %Player %tmp_constructor7
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
  %tmp8 = load ptr, ptr %self, align 8
  %tmp8.tick = getelementptr inbounds nuw i8, ptr %tmp8, i64 72
  %tmp9 = load i64, ptr %tmp8.tick, align 4
  %tmp.i = add i64 %tmp9, 1
  %tmp11.tick = getelementptr inbounds nuw i8, ptr %tmp8, i64 72
  store i64 %tmp.i, ptr %tmp11.tick, align 4
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
  %fun_call_tmp3 = call ptr @"Vec<T = char>:next"(ptr nonnull %tmp_iter1)
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp2 = call i1 @"Vec<T = char>:has_next"(ptr nonnull %tmp_iter1)
  br i1 %fun_call_tmp2, label %then, label %else

else:                                             ; preds = %cond
  %tmp8 = load ptr, ptr %self, align 8
  %tmp8.len = getelementptr inbounds nuw i8, ptr %tmp8, i64 8
  store i64 0, ptr %tmp8.len, align 4
  ret {} zeroinitializer
}

declare i1 @c_kbhit()

declare i8 @c_getch()

define { ptr, i64 } @"Vec<T = Particle>:deref_mut"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Particle>:as_mut_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = Particle>:as_mut_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

define %"SliceMutIter<T = Particle>" @"[Particle]:iter_mut"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i15 = mul i64 %.elt21, 41
  %tmp.i = add i64 %tmp.i15, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"SliceMutIter<T = Particle>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"SliceMutIter<T = Particle>" %tmp_agg, ptr %cast7, 1
  ret %"SliceMutIter<T = Particle>" %tmp_constructor
}

define ptr @"&mut Particle:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
}

define i1 @"&mut Particle:eq"(ptr %0, ptr %1) {
entry:
  %other.i = alloca ptr, align 8
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  call void @llvm.lifetime.start.p0(i64 8, ptr nonnull %other.i)
  store ptr %tmp3, ptr %other.i, align 8
  %tmp.i9.i.i = load double, ptr %tmp1, align 8
  %tmp1.i10.i.i = load double, ptr %tmp3, align 8
  %tmp2.i11.i.i = fcmp oeq double %tmp.i9.i.i, %tmp1.i10.i.i
  %tmp3.i.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 8
  %tmp4.y.i.i = getelementptr inbounds nuw i8, ptr %tmp3, i64 8
  %tmp.i.i.i = load double, ptr %tmp3.i.i, align 8
  %tmp1.i.i.i = load double, ptr %tmp4.y.i.i, align 8
  %tmp2.i.i.i = fcmp oeq double %tmp.i.i.i, %tmp1.i.i.i
  %tmp6.i.i = and i1 %tmp2.i11.i.i, %tmp2.i.i.i
  %tmp3.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 16
  %tmp4.i = load ptr, ptr %other.i, align 8
  %tmp4.size.i = getelementptr inbounds nuw i8, ptr %tmp4.i, i64 16
  %tmp.i31.i = load double, ptr %tmp3.i, align 8
  %tmp1.i32.i = load double, ptr %tmp4.size.i, align 8
  %tmp2.i33.i = fcmp oeq double %tmp.i31.i, %tmp1.i32.i
  %tmp6.i = and i1 %tmp6.i.i, %tmp2.i33.i
  %tmp7.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 24
  %tmp8.i = load ptr, ptr %other.i, align 8
  %tmp8.speed.i = getelementptr inbounds nuw i8, ptr %tmp8.i, i64 24
  %tmp.i26.i = load double, ptr %tmp7.i, align 8
  %tmp1.i27.i = load double, ptr %tmp8.speed.i, align 8
  %tmp2.i28.i = fcmp oeq double %tmp.i26.i, %tmp1.i27.i
  %tmp10.i = and i1 %tmp6.i, %tmp2.i28.i
  %tmp11.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 32
  %tmp12.i = load ptr, ptr %other.i, align 8
  %tmp12.offset.i = getelementptr inbounds nuw i8, ptr %tmp12.i, i64 32
  %tmp.i21.i = load double, ptr %tmp11.i, align 8
  %tmp1.i22.i = load double, ptr %tmp12.offset.i, align 8
  %tmp2.i23.i = fcmp oeq double %tmp.i21.i, %tmp1.i22.i
  %tmp14.i = and i1 %tmp10.i, %tmp2.i23.i
  %tmp15.i = getelementptr inbounds nuw i8, ptr %tmp1, i64 40
  %tmp16.i = load ptr, ptr %other.i, align 8
  %tmp16.color.i = getelementptr inbounds nuw i8, ptr %tmp16.i, i64 40
  %tmp.i.i = load i8, ptr %tmp15.i, align 1
  %tmp1.i.i = load i8, ptr %tmp16.color.i, align 1
  %tmp2.i.i = icmp eq i8 %tmp.i.i, %tmp1.i.i
  %tmp18.i = and i1 %tmp14.i, %tmp2.i.i
  call void @llvm.lifetime.end.p0(i64 8, ptr nonnull %other.i)
  ret i1 %tmp18.i
}

define i1 @"Vec<T = Particle>:has_next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"Vec<T = Particle>:next"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 41
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"Particle:update"(ptr %0, %"Vec_2<T = u64>" %1) {
entry:
  %tmp231 = alloca %"Range<T = f64>", align 8
  %screen_size = alloca %"Vec_2<T = u64>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"Vec_2<T = u64>" %1, 0
  store i64 %.elt, ptr %screen_size, align 4
  %screen_size.repack65 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %.elt66 = extractvalue %"Vec_2<T = u64>" %1, 1
  store i64 %.elt66, ptr %screen_size.repack65, align 4
  %tmp1 = load double, ptr %0, align 8
  %tmp2 = load ptr, ptr %self, align 8
  %tmp2.speed = getelementptr inbounds nuw i8, ptr %tmp2, i64 24
  %tmp3 = load double, ptr %tmp2.speed, align 8
  %tmp.i49 = fadd double %tmp1, %tmp3
  store double %tmp.i49, ptr %tmp2, align 8
  %tmp5.pos.y = getelementptr inbounds nuw i8, ptr %tmp2, i64 8
  %tmp6 = load double, ptr %tmp5.pos.y, align 8
  %tmp7 = load ptr, ptr %self, align 8
  %tmp7.offset = getelementptr inbounds nuw i8, ptr %tmp7, i64 32
  %tmp8 = load double, ptr %tmp7.offset, align 8
  %fun_call_tmp9 = call double @"f64:sin"(double %tmp8)
  %tmp.i46 = fadd double %tmp6, %fun_call_tmp9
  %tmp11.pos.y = getelementptr inbounds nuw i8, ptr %tmp7, i64 8
  store double %tmp.i46, ptr %tmp11.pos.y, align 8
  %tmp12 = load ptr, ptr %self, align 8
  %tmp12.offset = getelementptr inbounds nuw i8, ptr %tmp12, i64 32
  %tmp13 = load double, ptr %tmp12.offset, align 8
  %tmp14.speed = getelementptr inbounds nuw i8, ptr %tmp12, i64 24
  %tmp15 = load double, ptr %tmp14.speed, align 8
  %tmp.i52 = fmul double %tmp15, 3.125000e-02
  %fun_call_tmp17 = call double @"f64:min"(double 5.000000e-02, double %tmp.i52)
  %tmp.i43 = fadd double %tmp13, %fun_call_tmp17
  %tmp19 = load ptr, ptr %self, align 8
  %tmp19.offset = getelementptr inbounds nuw i8, ptr %tmp19, i64 32
  store double %tmp.i43, ptr %tmp19.offset, align 8
  %tmp20 = load ptr, ptr %self, align 8
  %tmp21 = load double, ptr %tmp20, align 8
  %tmp22 = load i64, ptr %screen_size, align 4
  %cast = uitofp i64 %tmp22 to double
  %tmp.i40 = fadd double %cast, 4.000000e+00
  %tmp.i56 = fcmp ogt double %tmp21, %tmp.i40
  br i1 %tmp.i56, label %then, label %merge

merge:                                            ; preds = %entry, %then
  %tmp35.unpack = load i64, ptr %screen_size, align 4
  %tmp35.elt72 = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp35.unpack73 = load i64, ptr %tmp35.elt72, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp35.unpack73)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp35.unpack)
  ret {} zeroinitializer

then:                                             ; preds = %entry
  %tmp25 = load ptr, ptr %self, align 8
  store double -4.000000e+00, ptr %tmp25, align 8
  %tmp27.pos.y = getelementptr inbounds nuw i8, ptr %tmp25, i64 8
  %screen_size.y = getelementptr inbounds nuw i8, ptr %screen_size, i64 8
  %tmp28 = load i64, ptr %screen_size.y, align 4
  %cast29 = uitofp i64 %tmp28 to double
  store double 0.000000e+00, ptr %tmp231, align 8
  %tmp231.repack67 = getelementptr inbounds nuw i8, ptr %tmp231, i64 8
  store double %cast29, ptr %tmp231.repack67, align 8
  %fun_call_tmp32 = call double @"Vec_2<T = f64>:rand"(ptr nonnull %tmp231)
  store double %fun_call_tmp32, ptr %tmp27.pos.y, align 8
  %tmp33.unpack = load double, ptr %tmp231, align 8
  %tmp33.elt69 = getelementptr inbounds nuw i8, ptr %tmp231, i64 8
  %tmp33.unpack70 = load double, ptr %tmp33.elt69, align 8
  %fun_call_tmp.i61 = call {} @"f64:drop"(double %tmp33.unpack70)
  %fun_call_tmp2.i63 = call {} @"f64:drop"(double %tmp33.unpack)
  br label %merge
}

define double @"f64:sin"(double %0) {
entry:
  %fun_call_tmp = call double @c_sin(double %0)
  %fun_call_tmp2 = call {} @"f64:drop"(double %0)
  ret double %fun_call_tmp
}

declare double @c_sin(double)

define double @"f64:min"(double %0, double %1) {
entry:
  %other = alloca double, align 8
  %self = alloca double, align 8
  store double %0, ptr %self, align 8
  store double %1, ptr %other, align 8
  %tmp = load double, ptr %self, align 8
  %tmp1 = load double, ptr %other, align 8
  %tmp.i = fcmp olt double %tmp, %tmp1
  %tmp2 = load double, ptr %self, align 8
  %tmp4 = load double, ptr %other, align 8
  %result = select i1 %tmp.i, double %tmp2, double %tmp4
  %tmp5 = load double, ptr %other, align 8
  %fun_call_tmp6 = call {} @"f64:drop"(double %tmp5)
  %tmp7 = load double, ptr %self, align 8
  %fun_call_tmp8 = call {} @"f64:drop"(double %tmp7)
  ret double %result
}

define {} @"Particle:update.94"(ptr %0, ptr %1) {
entry:
  %new_pos = alloca %"Vec_2<T = f64>", align 8
  %tmp7 = alloca i8, align 1
  %tmp631 = alloca i8, align 1
  %tmp519 = alloca i8, align 1
  %key_press = alloca ptr, align 8
  %tmp_iter3 = alloca %"SliceIter<T = char>", align 8
  %floor_height = alloca double, align 8
  %key_presses = alloca ptr, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store ptr %1, ptr %key_presses, align 8
  store double 2.800000e+01, ptr %floor_height, align 8
  %tmp.velocity.y = getelementptr inbounds nuw i8, ptr %0, i64 24
  %tmp2 = load double, ptr %tmp.velocity.y, align 8
  %tmp.i106 = fadd double %tmp2, 7.000000e-02
  %tmp4.velocity.y = getelementptr inbounds nuw i8, ptr %0, i64 24
  store double %tmp.i106, ptr %tmp4.velocity.y, align 8
  %tmp5 = load ptr, ptr %self, align 8
  %tmp5.velocity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  %tmp6 = load double, ptr %tmp5.velocity, align 8
  %tmp.i112 = fmul double %tmp6, 8.000000e-01
  %tmp8.velocity = getelementptr inbounds nuw i8, ptr %tmp5, i64 16
  store double %tmp.i112, ptr %tmp8.velocity, align 8
  %tmp9 = load ptr, ptr %key_presses, align 8
  %fun_call_tmp10 = call { ptr, i64 } @"Vec<T = char>:deref"(ptr %tmp9)
  %fun_call_tmp11 = call %"SliceIter<T = char>" @"[char]:iter"({ ptr, i64 } %fun_call_tmp10)
  %fun_call_tmp11.elt = extractvalue %"SliceIter<T = char>" %fun_call_tmp11, 0
  store ptr %fun_call_tmp11.elt, ptr %tmp_iter3, align 8
  %tmp_iter3.repack127 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  %fun_call_tmp11.elt128 = extractvalue %"SliceIter<T = char>" %fun_call_tmp11, 1
  store ptr %fun_call_tmp11.elt128, ptr %tmp_iter3.repack127, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp13 = call ptr @"Vec<T = char>:next"(ptr nonnull %tmp_iter3)
  store ptr %fun_call_tmp13, ptr %key_press, align 8
  %tmp18 = load ptr, ptr %key_press, align 8
  store i8 119, ptr %tmp519, align 1
  %tmp.i93 = load i8, ptr %tmp18, align 1
  %tmp2.i95 = icmp eq i8 %tmp.i93, 119
  %tmp21 = load ptr, ptr %self, align 8
  %tmp21.pos.y = getelementptr inbounds nuw i8, ptr %tmp21, i64 8
  %tmp.i117 = load double, ptr %tmp21.pos.y, align 8
  %tmp1.i118 = load double, ptr %floor_height, align 8
  %tmp2.i119 = fcmp oeq double %tmp.i117, %tmp1.i118
  %tmp.i126 = and i1 %tmp2.i95, %tmp2.i119
  br i1 %tmp.i126, label %then15, label %cond28

cond:                                             ; preds = %merge26, %entry
  %fun_call_tmp12 = call i1 @"Vec<T = char>:has_next"(ptr nonnull %tmp_iter3)
  br i1 %fun_call_tmp12, label %then, label %else

else:                                             ; preds = %cond
  %tmp56 = load ptr, ptr %self, align 8
  %tmp56.velocity = getelementptr inbounds nuw i8, ptr %tmp56, i64 16
  %tmp57.velocity = getelementptr inbounds nuw i8, ptr %tmp56, i64 16
  %tmp58 = load double, ptr %tmp57.velocity, align 8
  %fun_call_tmp60 = call double @"f64:clamp"(double %tmp58, double -2.000000e+00, double 2.000000e+00)
  store double %fun_call_tmp60, ptr %tmp56.velocity, align 8
  %tmp62.unpack = load double, ptr %tmp56, align 8
  %2 = insertvalue %"Vec_2<T = f64>" poison, double %tmp62.unpack, 0
  %tmp62.elt129 = getelementptr inbounds nuw i8, ptr %tmp56, i64 8
  %tmp62.unpack130 = load double, ptr %tmp62.elt129, align 8
  %tmp62131 = insertvalue %"Vec_2<T = f64>" %2, double %tmp62.unpack130, 1
  %tmp63 = load ptr, ptr %self, align 8
  %tmp63.velocity = getelementptr inbounds nuw i8, ptr %tmp63, i64 16
  %tmp64.unpack = load double, ptr %tmp63.velocity, align 8
  %3 = insertvalue %"Vec_2<T = f64>" poison, double %tmp64.unpack, 0
  %tmp64.elt132 = getelementptr inbounds nuw i8, ptr %tmp63, i64 24
  %tmp64.unpack133 = load double, ptr %tmp64.elt132, align 8
  %tmp64134 = insertvalue %"Vec_2<T = f64>" %3, double %tmp64.unpack133, 1
  %fun_call_tmp65 = call %"Vec_2<T = f64>" @"Vec_2<T = f64>:add"(%"Vec_2<T = f64>" %tmp62131, %"Vec_2<T = f64>" %tmp64134)
  %fun_call_tmp65.elt = extractvalue %"Vec_2<T = f64>" %fun_call_tmp65, 0
  store double %fun_call_tmp65.elt, ptr %new_pos, align 8
  %new_pos.repack135 = getelementptr inbounds nuw i8, ptr %new_pos, i64 8
  %fun_call_tmp65.elt136 = extractvalue %"Vec_2<T = f64>" %fun_call_tmp65, 1
  store double %fun_call_tmp65.elt136, ptr %new_pos.repack135, align 8
  %new_pos.y = getelementptr inbounds nuw i8, ptr %new_pos, i64 8
  %tmp68 = load double, ptr %floor_height, align 8
  %fun_call_tmp69 = call double @"f64:min"(double %fun_call_tmp65.elt136, double %tmp68)
  store double %fun_call_tmp69, ptr %new_pos.y, align 8
  %tmp70 = load ptr, ptr %self, align 8
  %tmp71.unpack = load double, ptr %new_pos, align 8
  store double %tmp71.unpack, ptr %tmp70, align 8
  %tmp70.repack140 = getelementptr inbounds nuw i8, ptr %tmp70, i64 8
  store double %fun_call_tmp69, ptr %tmp70.repack140, align 8
  %tmp72.elt142 = getelementptr inbounds nuw i8, ptr %new_pos, i64 8
  %tmp72.unpack143 = load double, ptr %tmp72.elt142, align 8
  %fun_call_tmp.i = call {} @"f64:drop"(double %tmp72.unpack143)
  %fun_call_tmp2.i = call {} @"f64:drop"(double %tmp71.unpack)
  %tmp76 = load double, ptr %floor_height, align 8
  %fun_call_tmp77 = call {} @"f64:drop"(double %tmp76)
  ret {} zeroinitializer

then15:                                           ; preds = %then
  %tmp24 = load ptr, ptr %self, align 8
  %tmp24.velocity.y = getelementptr inbounds nuw i8, ptr %tmp24, i64 24
  store double -1.200000e+00, ptr %tmp24.velocity.y, align 8
  br label %cond28

merge26:                                          ; preds = %cond38, %then37, %then27
  %tmp47 = load i8, ptr %tmp7, align 1
  %fun_call_tmp48 = call {} @"char:drop"(i8 %tmp47)
  %tmp49 = load i8, ptr %tmp631, align 1
  %fun_call_tmp50 = call {} @"char:drop"(i8 %tmp49)
  %tmp51 = load i8, ptr %tmp519, align 1
  %fun_call_tmp52 = call {} @"char:drop"(i8 %tmp51)
  br label %cond

then27:                                           ; preds = %cond28
  %tmp33 = load ptr, ptr %self, align 8
  %tmp33.velocity = getelementptr inbounds nuw i8, ptr %tmp33, i64 16
  %tmp34 = load double, ptr %tmp33.velocity, align 8
  %tmp.i109 = fadd double %tmp34, -2.000000e+00
  %tmp36.velocity = getelementptr inbounds nuw i8, ptr %tmp33, i64 16
  store double %tmp.i109, ptr %tmp36.velocity, align 8
  br label %merge26

cond28:                                           ; preds = %then, %then15
  %tmp30 = load ptr, ptr %key_press, align 8
  store i8 97, ptr %tmp631, align 1
  %tmp.i88 = load i8, ptr %tmp30, align 1
  %tmp2.i90 = icmp eq i8 %tmp.i88, 97
  br i1 %tmp2.i90, label %then27, label %cond38

then37:                                           ; preds = %cond38
  %tmp42 = load ptr, ptr %self, align 8
  %tmp42.velocity = getelementptr inbounds nuw i8, ptr %tmp42, i64 16
  %tmp43 = load double, ptr %tmp42.velocity, align 8
  %tmp.i103 = fadd double %tmp43, 2.000000e+00
  %tmp45.velocity = getelementptr inbounds nuw i8, ptr %tmp42, i64 16
  store double %tmp.i103, ptr %tmp45.velocity, align 8
  br label %merge26

cond38:                                           ; preds = %cond28
  %tmp40 = load ptr, ptr %key_press, align 8
  store i8 100, ptr %tmp7, align 1
  %tmp.i = load i8, ptr %tmp40, align 1
  %tmp2.i = icmp eq i8 %tmp.i, 100
  br i1 %tmp2.i, label %then37, label %merge26
}

define double @"f64:clamp"(double %0, double %1, double %2) {
entry:
  %fun_call_tmp = call double @"f64:max"(double %0, double %1)
  %fun_call_tmp3 = call double @"f64:min"(double %fun_call_tmp, double %2)
  %fun_call_tmp5 = call {} @"f64:drop"(double %2)
  %fun_call_tmp7 = call {} @"f64:drop"(double %1)
  %fun_call_tmp9 = call {} @"f64:drop"(double %0)
  ret double %fun_call_tmp3
}

define double @"f64:max"(double %0, double %1) {
entry:
  %other = alloca double, align 8
  %self = alloca double, align 8
  store double %0, ptr %self, align 8
  store double %1, ptr %other, align 8
  %tmp = load double, ptr %self, align 8
  %tmp1 = load double, ptr %other, align 8
  %tmp.i = fcmp ogt double %tmp, %tmp1
  %tmp2 = load double, ptr %self, align 8
  %tmp4 = load double, ptr %other, align 8
  %result = select i1 %tmp.i, double %tmp2, double %tmp4
  %tmp5 = load double, ptr %other, align 8
  %fun_call_tmp6 = call {} @"f64:drop"(double %tmp5)
  %tmp7 = load double, ptr %self, align 8
  %fun_call_tmp8 = call {} @"f64:drop"(double %tmp7)
  ret double %result
}

define %"Vec_2<T = f64>" @"Vec_2<T = f64>:add"(%"Vec_2<T = f64>" %0, %"Vec_2<T = f64>" %1) {
entry:
  %other = alloca %"Vec_2<T = f64>", align 8
  %self = alloca %"Vec_2<T = f64>", align 8
  %.elt = extractvalue %"Vec_2<T = f64>" %0, 0
  store double %.elt, ptr %self, align 8
  %self.repack19 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt20 = extractvalue %"Vec_2<T = f64>" %0, 1
  store double %.elt20, ptr %self.repack19, align 8
  %.elt21 = extractvalue %"Vec_2<T = f64>" %1, 0
  store double %.elt21, ptr %other, align 8
  %other.repack22 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %.elt23 = extractvalue %"Vec_2<T = f64>" %1, 1
  store double %.elt23, ptr %other.repack22, align 8
  %tmp = load double, ptr %self, align 8
  %tmp.i11 = fadd double %tmp, %.elt21
  %self.y = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp2 = load double, ptr %self.y, align 8
  %tmp.i = fadd double %tmp2, %.elt23
  %tmp_agg = insertvalue %"Vec_2<T = f64>" undef, double %tmp.i11, 0
  %tmp_constructor = insertvalue %"Vec_2<T = f64>" %tmp_agg, double %tmp.i, 1
  %tmp5.unpack = load double, ptr %other, align 8
  %tmp5.elt24 = getelementptr inbounds nuw i8, ptr %other, i64 8
  %tmp5.unpack25 = load double, ptr %tmp5.elt24, align 8
  %fun_call_tmp.i16 = call {} @"f64:drop"(double %tmp5.unpack25)
  %fun_call_tmp2.i18 = call {} @"f64:drop"(double %tmp5.unpack)
  %tmp7.unpack = load double, ptr %self, align 8
  %tmp7.elt27 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %tmp7.unpack28 = load double, ptr %tmp7.elt27, align 8
  %fun_call_tmp.i = call {} @"f64:drop"(double %tmp7.unpack28)
  %fun_call_tmp2.i = call {} @"f64:drop"(double %tmp7.unpack)
  ret %"Vec_2<T = f64>" %tmp_constructor
}

define {} @"pixel:Game:fill"(ptr %0, i8 %1) {
entry:
  %tmp_iter2 = alloca %"SliceMutIter<T = u8>", align 8
  %new_color = alloca i8, align 1
  store i8 %1, ptr %new_color, align 1
  %tmp.buffer = getelementptr inbounds nuw i8, ptr %0, i64 16
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %tmp.buffer)
  %fun_call_tmp1 = call %"SliceMutIter<T = u8>" @"[u8]:iter_mut"({ ptr, i64 } %fun_call_tmp)
  %fun_call_tmp1.elt = extractvalue %"SliceMutIter<T = u8>" %fun_call_tmp1, 0
  store ptr %fun_call_tmp1.elt, ptr %tmp_iter2, align 8
  %tmp_iter2.repack16 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %fun_call_tmp1.elt17 = extractvalue %"SliceMutIter<T = u8>" %fun_call_tmp1, 1
  store ptr %fun_call_tmp1.elt17, ptr %tmp_iter2.repack16, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call ptr @"Vec<T = u8>:next.116"(ptr nonnull %tmp_iter2)
  %tmp5 = load i8, ptr %new_color, align 1
  store i8 %tmp5, ptr %fun_call_tmp3, align 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %fun_call_tmp2 = call i1 @"Vec<T = u8>:has_next.115"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp2, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load i8, ptr %new_color, align 1
  %fun_call_tmp11 = call {} @"u8:drop"(i8 %tmp10)
  ret {} zeroinitializer
}

define %"SliceMutIter<T = u8>" @"[u8]:iter_mut"({ ptr, i64 } %0) {
entry:
  %.elt21 = extractvalue { ptr, i64 } %0, 1
  %ref.i = extractvalue { ptr, i64 } %0, 0
  %cast = ptrtoint ptr %ref.i to i64
  %tmp.i = add i64 %.elt21, %cast
  %cast7 = inttoptr i64 %tmp.i to ptr
  %tmp_agg = insertvalue %"SliceMutIter<T = u8>" undef, ptr %ref.i, 0
  %tmp_constructor = insertvalue %"SliceMutIter<T = u8>" %tmp_agg, ptr %cast7, 1
  ret %"SliceMutIter<T = u8>" %tmp_constructor
}

define ptr @"&mut u8:clone"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  ret ptr %tmp1
}

define i1 @"&mut u8:eq"(ptr %0, ptr %1) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp3 = load ptr, ptr %1, align 8
  %tmp.i = load i8, ptr %tmp1, align 1
  %tmp1.i = load i8, ptr %tmp3, align 1
  %tmp2.i = icmp eq i8 %tmp.i, %tmp1.i
  ret i1 %tmp2.i
}

define i1 @"Vec<T = u8>:has_next.115"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %tmp2.end = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp3 = load ptr, ptr %tmp2.end, align 8
  %tmp.i = icmp ult ptr %tmp1, %tmp3
  ret i1 %tmp.i
}

define ptr @"Vec<T = u8>:next.116"(ptr %0) {
entry:
  %tmp1 = load ptr, ptr %0, align 8
  %cast = ptrtoint ptr %tmp1 to i64
  %tmp.i = add i64 %cast, 1
  %cast6 = inttoptr i64 %tmp.i to ptr
  store ptr %cast6, ptr %0, align 8
  ret ptr %tmp1
}

define {} @"Particle:render"(ptr %0, ptr %1) {
entry:
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %tmp2 = load double, ptr %0, align 8
  %cast = fptoui double %tmp2 to i64
  %tmp3.pos.y = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load double, ptr %tmp3.pos.y, align 8
  %cast5 = fptoui double %tmp4 to i64
  %tmp_agg = insertvalue %"Vec_2<T = u64>" undef, i64 %cast, 0
  %tmp_constructor = insertvalue %"Vec_2<T = u64>" %tmp_agg, i64 %cast5, 1
  %tmp6 = load ptr, ptr %self, align 8
  %tmp6.size = getelementptr inbounds nuw i8, ptr %tmp6, i64 16
  %tmp7 = load double, ptr %tmp6.size, align 8
  %cast8 = fptoui double %tmp7 to i64
  %cast11 = fptoui double %tmp7 to i64
  %tmp_agg12 = insertvalue %"Vec_2<T = u64>" undef, i64 %cast8, 0
  %tmp_constructor13 = insertvalue %"Vec_2<T = u64>" %tmp_agg12, i64 %cast11, 1
  %tmp14 = load ptr, ptr %self, align 8
  %tmp14.color = getelementptr inbounds nuw i8, ptr %tmp14, i64 40
  %tmp15 = load i8, ptr %tmp14.color, align 1
  %fun_call_tmp = call {} @"pixel:Game:paint_rect"(ptr %1, %"Vec_2<T = u64>" %tmp_constructor, %"Vec_2<T = u64>" %tmp_constructor13, i8 %tmp15)
  ret {} zeroinitializer
}

define {} @"pixel:Game:paint_rect"(ptr %0, %"Vec_2<T = u64>" %1, %"Vec_2<T = u64>" %2, i8 %3) {
entry:
  %idx = alloca i64, align 8
  %x = alloca i64, align 8
  %tmp_iter7 = alloca %"Range<T = u64>", align 8
  %y = alloca i64, align 8
  %tmp_iter5 = alloca %"Range<T = u64>", align 8
  %offset = alloca i64, align 8
  %color = alloca i8, align 1
  %size = alloca %"Vec_2<T = u64>", align 8
  %pos = alloca %"Vec_2<T = u64>", align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  %.elt = extractvalue %"Vec_2<T = u64>" %1, 0
  store i64 %.elt, ptr %pos, align 4
  %pos.repack102 = getelementptr inbounds nuw i8, ptr %pos, i64 8
  %.elt103 = extractvalue %"Vec_2<T = u64>" %1, 1
  store i64 %.elt103, ptr %pos.repack102, align 4
  %.elt104 = extractvalue %"Vec_2<T = u64>" %2, 0
  store i64 %.elt104, ptr %size, align 4
  %size.repack105 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %.elt106 = extractvalue %"Vec_2<T = u64>" %2, 1
  store i64 %.elt106, ptr %size.repack105, align 4
  store i8 %3, ptr %color, align 1
  %pos.y = getelementptr inbounds nuw i8, ptr %pos, i64 8
  %tmp = load i64, ptr %pos.y, align 4
  %tmp1 = load ptr, ptr %self, align 8
  %tmp2 = load i64, ptr %tmp1, align 4
  %tmp.i73 = mul i64 %tmp, %tmp2
  %tmp3 = load i64, ptr %pos, align 4
  %tmp.i67 = add i64 %tmp.i73, %tmp3
  store i64 %tmp.i67, ptr %offset, align 4
  %size.y = getelementptr inbounds nuw i8, ptr %size, i64 8
  %tmp5 = load i64, ptr %size.y, align 4
  store i64 0, ptr %tmp_iter5, align 4
  %tmp_iter5.repack107 = getelementptr inbounds nuw i8, ptr %tmp_iter5, i64 8
  store i64 %tmp5, ptr %tmp_iter5.repack107, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp8 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter5)
  store i64 %fun_call_tmp8, ptr %y, align 4
  %tmp10 = load i64, ptr %size, align 4
  store i64 0, ptr %tmp_iter7, align 4
  %tmp_iter7.repack118 = getelementptr inbounds nuw i8, ptr %tmp_iter7, i64 8
  store i64 %tmp10, ptr %tmp_iter7.repack118, align 4
  br label %cond13

cond:                                             ; preds = %else14, %entry
  %fun_call_tmp7 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter5)
  br i1 %fun_call_tmp7, label %then, label %else

else:                                             ; preds = %cond
  %tmp50.unpack = load i64, ptr %tmp_iter5, align 4
  %tmp50.elt109 = getelementptr inbounds nuw i8, ptr %tmp_iter5, i64 8
  %tmp50.unpack110 = load i64, ptr %tmp50.elt109, align 4
  %fun_call_tmp.i93 = call {} @"u64:drop"(i64 %tmp50.unpack110)
  %fun_call_tmp2.i95 = call {} @"u64:drop"(i64 %tmp50.unpack)
  %tmp52 = load i64, ptr %offset, align 4
  %fun_call_tmp53 = call {} @"u64:drop"(i64 %tmp52)
  %tmp54 = load i8, ptr %color, align 1
  %fun_call_tmp55 = call {} @"u8:drop"(i8 %tmp54)
  %tmp56.unpack = load i64, ptr %size, align 4
  %tmp56.elt112 = getelementptr inbounds nuw i8, ptr %size, i64 8
  %tmp56.unpack113 = load i64, ptr %tmp56.elt112, align 4
  %fun_call_tmp.i86 = call {} @"u64:drop"(i64 %tmp56.unpack113)
  %fun_call_tmp2.i88 = call {} @"u64:drop"(i64 %tmp56.unpack)
  %tmp58.unpack = load i64, ptr %pos, align 4
  %tmp58.elt115 = getelementptr inbounds nuw i8, ptr %pos, i64 8
  %tmp58.unpack116 = load i64, ptr %tmp58.elt115, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp58.unpack116)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp58.unpack)
  ret {} zeroinitializer

then12:                                           ; preds = %cond13
  %fun_call_tmp16 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter7)
  store i64 %fun_call_tmp16, ptr %x, align 4
  %tmp17 = load i64, ptr %offset, align 4
  %tmp18 = load i64, ptr %y, align 4
  %tmp19 = load ptr, ptr %self, align 8
  %tmp20 = load i64, ptr %tmp19, align 4
  %tmp.i70 = mul i64 %tmp18, %tmp20
  %tmp.i64 = add i64 %tmp17, %tmp.i70
  %tmp23 = load i64, ptr %x, align 4
  %tmp.i = add i64 %tmp.i64, %tmp23
  store i64 %tmp.i, ptr %idx, align 4
  %tmp29 = load i64, ptr %idx, align 4
  %tmp30 = load ptr, ptr %self, align 8
  %tmp30.buffer = getelementptr inbounds nuw i8, ptr %tmp30, i64 16
  %fun_call_tmp31 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %tmp30.buffer)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp31, 1
  %end.i = insertvalue %"Range<T = u64>" { i64 0, i64 undef }, i64 %len.i, 1
  %fun_call_tmp34 = call i1 @"u64:in_range"(i64 %tmp29, %"Range<T = u64>" %end.i)
  br i1 %fun_call_tmp34, label %then26, label %merge25

cond13:                                           ; preds = %merge25, %then
  %fun_call_tmp15 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter7)
  br i1 %fun_call_tmp15, label %then12, label %else14

else14:                                           ; preds = %cond13
  %tmp45.unpack = load i64, ptr %tmp_iter7, align 4
  %tmp45.elt120 = getelementptr inbounds nuw i8, ptr %tmp_iter7, i64 8
  %tmp45.unpack121 = load i64, ptr %tmp45.elt120, align 4
  %fun_call_tmp.i98 = call {} @"u64:drop"(i64 %tmp45.unpack121)
  %fun_call_tmp2.i100 = call {} @"u64:drop"(i64 %tmp45.unpack)
  %tmp47 = load i64, ptr %y, align 4
  %fun_call_tmp48 = call {} @"u64:drop"(i64 %tmp47)
  br label %cond

merge25:                                          ; preds = %then12, %then26
  %tmp40 = load i64, ptr %idx, align 4
  %fun_call_tmp41 = call {} @"u64:drop"(i64 %tmp40)
  %tmp42 = load i64, ptr %x, align 4
  %fun_call_tmp43 = call {} @"u64:drop"(i64 %tmp42)
  br label %cond13

then26:                                           ; preds = %then12
  %tmp35 = load ptr, ptr %self, align 8
  %tmp35.buffer = getelementptr inbounds nuw i8, ptr %tmp35, i64 16
  %fun_call_tmp36 = call { ptr, i64 } @"Vec<T = u8>:deref_mut"(ptr nonnull %tmp35.buffer)
  %tmp37 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp36, 0
  %tmp_index.i = getelementptr i8, ptr %ref.i, i64 %tmp37
  %tmp39 = load i8, ptr %color, align 1
  store i8 %tmp39, ptr %tmp_index.i, align 1
  br label %merge25
}

define i1 @"u64:in_range"(i64 %0, %"Range<T = u64>" %1) {
entry:
  %range = alloca %"Range<T = u64>", align 8
  %self = alloca i64, align 8
  store i64 %0, ptr %self, align 4
  %.elt = extractvalue %"Range<T = u64>" %1, 0
  store i64 %.elt, ptr %range, align 4
  %range.repack27 = getelementptr inbounds nuw i8, ptr %range, i64 8
  %.elt28 = extractvalue %"Range<T = u64>" %1, 1
  store i64 %.elt28, ptr %range.repack27, align 4
  %tmp4 = icmp uge i64 %0, %.elt
  %tmp5 = load i64, ptr %self, align 4
  %tmp.i = icmp ult i64 %tmp5, %.elt28
  %tmp.i26 = and i1 %tmp4, %tmp.i
  %fun_call_tmp10 = call {} @"u64:drop"(i64 %0)
  %tmp11.unpack = load i64, ptr %range, align 4
  %tmp11.elt29 = getelementptr inbounds nuw i8, ptr %range, i64 8
  %tmp11.unpack30 = load i64, ptr %tmp11.elt29, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp11.unpack30)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp11.unpack)
  %tmp13 = load i64, ptr %self, align 4
  %fun_call_tmp14 = call {} @"u64:drop"(i64 %tmp13)
  ret i1 %tmp.i26
}

define {} @"TileMap<W = \22i64 16\22, H = \22i64 9\22>:render"(ptr %0, ptr %1) {
entry:
  %tile = alloca i8, align 1
  %x = alloca i64, align 8
  %tmp_iter4 = alloca %"Range<T = u64>", align 8
  %y = alloca i64, align 8
  %tmp_iter2 = alloca %"Range<T = u64>", align 8
  %frame = alloca ptr, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store ptr %1, ptr %frame, align 8
  store i64 0, ptr %tmp_iter2, align 4
  %tmp_iter2.repack89 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  store i64 9, ptr %tmp_iter2.repack89, align 4
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp2 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter2)
  store i64 %fun_call_tmp2, ptr %y, align 4
  store i64 0, ptr %tmp_iter4, align 4
  %tmp_iter4.repack93 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  store i64 16, ptr %tmp_iter4.repack93, align 4
  br label %cond6

cond:                                             ; preds = %else7, %entry
  %fun_call_tmp1 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter2)
  br i1 %fun_call_tmp1, label %then, label %else

else:                                             ; preds = %cond
  %tmp48.unpack = load i64, ptr %tmp_iter2, align 4
  %tmp48.elt90 = getelementptr inbounds nuw i8, ptr %tmp_iter2, i64 8
  %tmp48.unpack91 = load i64, ptr %tmp48.elt90, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp48.unpack91)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp48.unpack)
  ret {} zeroinitializer

then5:                                            ; preds = %cond6
  %fun_call_tmp9 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter4)
  store i64 %fun_call_tmp9, ptr %x, align 4
  %tmp = load ptr, ptr %self, align 8
  %tmp_index.i86 = getelementptr [9 x i8], ptr %tmp, i64 %fun_call_tmp9
  %tmp12 = load i64, ptr %y, align 4
  %tmp_index.i = getelementptr i8, ptr %tmp_index.i86, i64 %tmp12
  %tmp14 = load i8, ptr %tmp_index.i, align 1
  store i8 %tmp14, ptr %tile, align 1
  %tmp19 = load i8, ptr %tile, align 1
  %tmp.i75.not = icmp eq i8 %tmp19, 0
  br i1 %tmp.i75.not, label %merge15, label %then16

cond6:                                            ; preds = %merge15, %then
  %fun_call_tmp8 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter4)
  br i1 %fun_call_tmp8, label %then5, label %else7

else7:                                            ; preds = %cond6
  %tmp43.unpack = load i64, ptr %tmp_iter4, align 4
  %tmp43.elt94 = getelementptr inbounds nuw i8, ptr %tmp_iter4, i64 8
  %tmp43.unpack95 = load i64, ptr %tmp43.elt94, align 4
  %fun_call_tmp.i80 = call {} @"u64:drop"(i64 %tmp43.unpack95)
  %fun_call_tmp2.i82 = call {} @"u64:drop"(i64 %tmp43.unpack)
  %tmp45 = load i64, ptr %y, align 4
  %fun_call_tmp46 = call {} @"u64:drop"(i64 %tmp45)
  br label %cond

merge15:                                          ; preds = %then5, %then16
  %tmp38 = load i8, ptr %tile, align 1
  %fun_call_tmp39 = call {} @"u8:drop"(i8 %tmp38)
  %tmp40 = load i64, ptr %x, align 4
  %fun_call_tmp41 = call {} @"u64:drop"(i64 %tmp40)
  br label %cond6

then16:                                           ; preds = %then5
  %tmp21 = load ptr, ptr %frame, align 8
  %tmp22 = load i64, ptr %x, align 4
  %tmp.i68 = shl i64 %tmp22, 2
  %tmp24 = load i64, ptr %y, align 4
  %tmp.i65 = shl i64 %tmp24, 2
  %tmp_agg = insertvalue %"Vec_2<T = u64>" undef, i64 %tmp.i68, 0
  %tmp_constructor = insertvalue %"Vec_2<T = u64>" %tmp_agg, i64 %tmp.i65, 1
  %tmp26 = load i8, ptr %tile, align 1
  %fun_call_tmp27 = call {} @"pixel:Game:paint_rect"(ptr %tmp21, %"Vec_2<T = u64>" %tmp_constructor, %"Vec_2<T = u64>" { i64 4, i64 4 }, i8 %tmp26)
  %tmp28 = load ptr, ptr %frame, align 8
  %tmp29 = load i64, ptr %x, align 4
  %tmp.i62 = shl i64 %tmp29, 2
  %tmp.i56 = or disjoint i64 %tmp.i62, 1
  %tmp32 = load i64, ptr %y, align 4
  %tmp.i59 = shl i64 %tmp32, 2
  %tmp.i = or disjoint i64 %tmp.i59, 1
  %tmp_agg35 = insertvalue %"Vec_2<T = u64>" undef, i64 %tmp.i56, 0
  %tmp_constructor36 = insertvalue %"Vec_2<T = u64>" %tmp_agg35, i64 %tmp.i, 1
  %fun_call_tmp37 = call {} @"pixel:Game:paint_rect"(ptr %tmp28, %"Vec_2<T = u64>" %tmp_constructor36, %"Vec_2<T = u64>" { i64 2, i64 2 }, i8 0)
  br label %merge15
}

define {} @"Particle:render.121"(ptr %0, ptr %1) {
entry:
  %frame = alloca ptr, align 8
  %self = alloca ptr, align 8
  store ptr %0, ptr %self, align 8
  store ptr %1, ptr %frame, align 8
  %tmp2 = load double, ptr %0, align 8
  %cast = fptoui double %tmp2 to i64
  %tmp3.pos.y = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp4 = load double, ptr %tmp3.pos.y, align 8
  %cast5 = fptoui double %tmp4 to i64
  %tmp_agg = insertvalue %"Vec_2<T = u64>" undef, i64 %cast, 0
  %tmp_constructor = insertvalue %"Vec_2<T = u64>" %tmp_agg, i64 %cast5, 1
  %fun_call_tmp = call {} @"pixel:Game:paint_rect"(ptr %1, %"Vec_2<T = u64>" %tmp_constructor, %"Vec_2<T = u64>" { i64 2, i64 1 }, i8 1)
  %tmp6 = load ptr, ptr %frame, align 8
  %tmp7 = load ptr, ptr %self, align 8
  %tmp8 = load double, ptr %tmp7, align 8
  %cast9 = fptoui double %tmp8 to i64
  %tmp10.pos.y = getelementptr inbounds nuw i8, ptr %tmp7, i64 8
  %tmp11 = load double, ptr %tmp10.pos.y, align 8
  %cast12 = fptoui double %tmp11 to i64
  %tmp.i34 = add i64 %cast12, 1
  %tmp_agg14 = insertvalue %"Vec_2<T = u64>" undef, i64 %cast9, 0
  %tmp_constructor15 = insertvalue %"Vec_2<T = u64>" %tmp_agg14, i64 %tmp.i34, 1
  %fun_call_tmp16 = call {} @"pixel:Game:paint_rect"(ptr %tmp6, %"Vec_2<T = u64>" %tmp_constructor15, %"Vec_2<T = u64>" { i64 2, i64 1 }, i8 3)
  %tmp17 = load ptr, ptr %frame, align 8
  %tmp18 = load ptr, ptr %self, align 8
  %tmp19 = load double, ptr %tmp18, align 8
  %cast20 = fptoui double %tmp19 to i64
  %tmp21.pos.y = getelementptr inbounds nuw i8, ptr %tmp18, i64 8
  %tmp22 = load double, ptr %tmp21.pos.y, align 8
  %cast23 = fptoui double %tmp22 to i64
  %tmp.i = add i64 %cast23, 2
  %tmp_agg25 = insertvalue %"Vec_2<T = u64>" undef, i64 %cast20, 0
  %tmp_constructor26 = insertvalue %"Vec_2<T = u64>" %tmp_agg25, i64 %tmp.i, 1
  %fun_call_tmp27 = call {} @"pixel:Game:paint_rect"(ptr %tmp17, %"Vec_2<T = u64>" %tmp_constructor26, %"Vec_2<T = u64>" { i64 2, i64 2 }, i8 4)
  ret {} zeroinitializer
}

define {} @"pixel:Game:render"(ptr %0) {
entry:
  %fun_call_tmp = call {} @"pixel:Game:render.122"(ptr %0)
  %tmp1.tick_speed = getelementptr inbounds nuw i8, ptr %0, i64 64
  %tmp2 = load i64, ptr %tmp1.tick_speed, align 4
  %cast = uitofp i64 %tmp2 to double
  %tmp.i = fdiv double 1.000000e+00, %cast
  %fun_call_tmp4 = call {} @sleep(double %tmp.i)
  ret {} zeroinitializer
}

define {} @"pixel:Game:render.122"(ptr %0) {
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
  %fun_call_tmp4 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter1)
  store i64 %fun_call_tmp4, ptr %y, align 4
  %tmp6 = load ptr, ptr %self, align 8
  %tmp7 = load i64, ptr %tmp6, align 4
  store i64 0, ptr %tmp_iter3, align 4
  %tmp_iter3.repack63 = getelementptr inbounds nuw i8, ptr %tmp_iter3, i64 8
  store i64 %tmp7, ptr %tmp_iter3.repack63, align 4
  br label %cond10

cond:                                             ; preds = %else11, %entry
  %fun_call_tmp3 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter1)
  br i1 %fun_call_tmp3, label %then, label %else

else:                                             ; preds = %cond
  %fun_call_tmp36 = call {} @print({ ptr, i64 } { ptr @global_string.131, i64 4 })
  %tmp37.unpack = load i64, ptr %tmp_iter1, align 4
  %tmp37.elt60 = getelementptr inbounds nuw i8, ptr %tmp_iter1, i64 8
  %tmp37.unpack61 = load i64, ptr %tmp37.elt60, align 4
  %fun_call_tmp.i = call {} @"u64:drop"(i64 %tmp37.unpack61)
  %fun_call_tmp2.i = call {} @"u64:drop"(i64 %tmp37.unpack)
  ret {} zeroinitializer

then9:                                            ; preds = %cond10
  %fun_call_tmp13 = call i64 @"Vec_2<T = u64>:next"(ptr nonnull %tmp_iter3)
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
  %fun_call_tmp12 = call i1 @"Vec_2<T = u64>:has_next"(ptr nonnull %tmp_iter3)
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
  %fun_call_tmp10 = call {} @"Vec<T = char>:drop.35"(%"Vec<T = char>" %tmp928)
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
  %fun_call_tmp3 = call {} @"[&[char]; 8]:drop"([8 x { ptr, i64 }] [{ ptr, i64 } { ptr @global_string.123, i64 5 }, { ptr, i64 } { ptr @global_string.124, i64 5 }, { ptr, i64 } { ptr @global_string.125, i64 5 }, { ptr, i64 } { ptr @global_string.126, i64 5 }, { ptr, i64 } { ptr @global_string.127, i64 5 }, { ptr, i64 } { ptr @global_string.128, i64 5 }, { ptr, i64 } { ptr @global_string.129, i64 5 }, { ptr, i64 } { ptr @global_string.130, i64 5 }])
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
  %fun_call_tmp10 = call {} @"Vec<T = char>:drop.35"(%"Vec<T = char>" %tmp928)
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
  %fun_call_tmp1 = call %"Vec<T = char>" @"[char]:to_string"({ ptr, i64 } { ptr @global_string.132, i64 1 })
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

define {} @"Vec<T = Particle>:drop.133"(%"Vec<T = Particle>" %0) {
entry:
  %idx = alloca i64, align 8
  %self = alloca %"Vec<T = Particle>", align 8
  %.elt = extractvalue %"Vec<T = Particle>" %0, 0
  store ptr %.elt, ptr %self, align 8
  %self.repack21 = getelementptr inbounds nuw i8, ptr %self, i64 8
  %.elt22 = extractvalue %"Vec<T = Particle>" %0, 1
  store i64 %.elt22, ptr %self.repack21, align 8
  %self.repack23 = getelementptr inbounds nuw i8, ptr %self, i64 16
  %.elt24 = extractvalue %"Vec<T = Particle>" %0, 2
  store i64 %.elt24, ptr %self.repack23, align 8
  br label %cond

then:                                             ; preds = %cond
  %fun_call_tmp3 = call { ptr, i64 } @"Vec<T = Particle>:deref"(ptr nonnull %self)
  %tmp4 = load i64, ptr %idx, align 4
  %ref.i = extractvalue { ptr, i64 } %fun_call_tmp3, 0
  %tmp_index.i = getelementptr %Particle, ptr %ref.i, i64 %tmp4
  %tmp6 = load %Particle, ptr %tmp_index.i, align 8
  %tmp.i18 = extractvalue %Particle %tmp6, 4
  %fun_call_tmp.i = call {} @"u8:drop"(i8 %tmp.i18)
  %tmp1.i = extractvalue %Particle %tmp6, 3
  %fun_call_tmp2.i = call {} @"f64:drop"(double %tmp1.i)
  %tmp3.i = extractvalue %Particle %tmp6, 2
  %fun_call_tmp4.i = call {} @"f64:drop"(double %tmp3.i)
  %tmp5.i = extractvalue %Particle %tmp6, 1
  %fun_call_tmp6.i = call {} @"f64:drop"(double %tmp5.i)
  %tmp7.i = extractvalue %Particle %tmp6, 0
  %tmp.i.i = extractvalue %"Vec_2<T = f64>" %tmp7.i, 1
  %fun_call_tmp.i.i = call {} @"f64:drop"(double %tmp.i.i)
  %tmp1.i.i = extractvalue %"Vec_2<T = f64>" %tmp7.i, 0
  %fun_call_tmp2.i.i = call {} @"f64:drop"(double %tmp1.i.i)
  %tmp8 = load i64, ptr %idx, align 4
  %tmp.i = add i64 %tmp8, 1
  br label %cond

cond:                                             ; preds = %then, %entry
  %storemerge = phi i64 [ 0, %entry ], [ %tmp.i, %then ]
  store i64 %storemerge, ptr %idx, align 4
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Particle>:deref"(ptr nonnull %self)
  %len.i = extractvalue { ptr, i64 } %fun_call_tmp, 1
  %tmp.i16 = icmp ult i64 %storemerge, %len.i
  br i1 %tmp.i16, label %then, label %else

else:                                             ; preds = %cond
  %tmp10 = load ptr, ptr %self, align 8
  %fun_call_tmp11 = call {} @"mem:free<T = Particle>"(ptr %tmp10)
  %tmp12 = load i64, ptr %idx, align 4
  %fun_call_tmp13 = call {} @"u64:drop"(i64 %tmp12)
  ret {} zeroinitializer
}

define { ptr, i64 } @"Vec<T = Particle>:deref"(ptr %0) {
entry:
  %fun_call_tmp = call { ptr, i64 } @"Vec<T = Particle>:as_slice"(ptr %0)
  ret { ptr, i64 } %fun_call_tmp
}

define { ptr, i64 } @"Vec<T = Particle>:as_slice"(ptr %0) {
entry:
  %tmp1.len = getelementptr inbounds nuw i8, ptr %0, i64 8
  %tmp2 = load ptr, ptr %0, align 8
  %tmp3 = load i64, ptr %tmp1.len, align 4
  %ptr1.i = insertvalue { ptr, i64 } undef, ptr %tmp2, 0
  %len2.i = insertvalue { ptr, i64 } %ptr1.i, i64 %tmp3, 1
  ret { ptr, i64 } %len2.i
}

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
  %fun_call_tmp5 = call {} @"Vec<T = char>:drop.35"(%"Vec<T = char>" %tmp416)
  %tmp6 = load i64, ptr %heap_count, align 4
  %fun_call_tmp7 = call {} @"u64:drop"(i64 %tmp6)
  ret {} zeroinitializer
}

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(i64 immarg, ptr captures(none)) #0

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(i64 immarg, ptr captures(none)) #0

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
