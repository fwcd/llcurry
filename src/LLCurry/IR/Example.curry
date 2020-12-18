module LLCurry.IR.Example
    ( helloWorldLLProg
    ) where

import LLCurry.IR.Types

-- A 'hello world' program in LLVM IR.
-- Useful for testing the pretty-printer.
helloWorldLLProg :: LLProg
helloWorldLLProg = LLProg
    { llProgGlobals = [ LLConstantDecl "hello" (LLValue (LLArrayType 13 (LLBasicType "i8")) (LLLitString "Hello World\\0A\\00"))
                      , LLFuncDecl (LLBasicType "i32") "puts" [LLPtrType (LLBasicType "i8")]
                      , LLFuncDef  (LLBasicType "i32") "main" []
                          [ LLBasicBlock Nothing
                              [ LLLocalAssign "hello2" (LLGetElementPtrInst (LLArrayType 13 (LLBasicType "i8")) (LLValue (LLPtrType (LLArrayType 13 (LLBasicType "i8"))) (LLGlobalVar "hello"))
                                    [ LLValue (LLBasicType "i64") (LLLitInt 0)
                                    , LLValue (LLBasicType "i64") (LLLitInt 0)
                                    ])
                              , LLCallInst (LLBasicType "i32") "puts" [LLValue (LLPtrType (LLBasicType "i8")) (LLLocalVar "hello2")]
                              , LLReturnInst (LLValue (LLBasicType "i32") (LLLitInt 0))
                              ]
                          ]
                      ]
    }
