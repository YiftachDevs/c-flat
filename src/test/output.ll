; ModuleID = 'main_module'
source_filename = "main_module"

define void @main() {
entry:
  call void @"debug<f32>"(float 0x3FD99999A0000000)
  ret void
}

define void @"debug<f32>"(float %value) {
entry:
  ret void
}
