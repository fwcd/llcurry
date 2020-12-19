; Demonstrates how generated LLVM code could interface with
; the Curry runtime. Creates a simple integer node and prints
; it.
;
; If you have built the runtime, you can interpret this module
; module by running:
;
;     lli --extra-module ../../runtime/runtime.ll runtime-interfacing.ll

%struct.CurryNode = type opaque

declare %struct.CurryNode* @curryNodeNewInteger(i64)

declare void @curryNodePrint(%struct.CurryNode*)

define i32 @main() {
    %node = call %struct.CurryNode* @curryNodeNewInteger(i64 42)
    call void @curryNodePrint(%struct.CurryNode* %node)
    ret i32 0
}
