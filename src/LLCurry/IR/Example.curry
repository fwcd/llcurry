module LLCurry.IR.Example
    ( helloWorldLLProg
    ) where

import LLCurry.IR.Types

-- TODO: Global constants
-- TODO: GEP instruction
-- TODO: Define basic types like i32, i8, i1, ... somewhere for convenience

-- A 'hello world' program in LLVM IR.
-- Useful for testing the pretty-printer.
helloWorldLLProg :: LLProg
helloWorldLLProg = LLProg
    { llProgFuncs = [ LLFuncDecl (LLBasicType "i32") "puts" [LLPtrType (LLBasicType "i8")]
                    , LLFuncDef  (LLBasicType "i32") "main" []
                        [ LLBasicBlock Nothing
                            [ LLCallInst (LLBasicType "i32") "puts" [LLValue (LLPtrType (LLBasicType "i8")) (LLLocalVar "hello")]
                            , LLReturnInst (LLValue (LLBasicType "i32") (LLLitInt 0))
                            ]
                        ]
                    ]
    }
