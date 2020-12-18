module LLCurry.IR.Types
    ( LLProg (..)
    ) where

-- Useful references:
-- * https://llvm.org/docs/LangRef.html
-- * https://llvm.org/doxygen/dir_c3e93f23a4a31c717998b98ce143b7c0.html
-- * https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
-- * https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/README.html

-- A program in LLVM IR.
data LLProg = LLProg { llProgFuncs :: [LLFunc]
                     }
    deriving (Show, Eq)

-- A function in LLVM IR.
data LLFunc = LLFunc { llFuncType :: LLType -- The return type
                     , llFuncName :: String
                     , llFuncArgs :: [LLValue]
                     }
    deriving (Show, Eq)

-- A labelled list of statements in LLVM IR.
data LLBasicBlock = LLBasicBlock { llBasicBlockName :: String
                                 , llBasicBlockInsts :: [LLInst]
                                 }
    deriving (Show, Eq)

-- An instruction/statement in LLVM IR.
data LLInst = -- * Terminator instructions
              LLBranchInst
            | LLSwitchInst
            | LLCallInst
            | LLReturnInst
              -- * Unary and binary operations
            | LLUnaryInst LLUnaryOp LLValue
            | LLBinaryInst LLBinaryOp LLValue LLValue
              -- * Memory access and addressing instructions
            | LLAllocaInst LLType (Maybe (LLType, Int))
            | LLLoadInst LLType LLValue
            | LLStoreInst LLValue LLValue
    deriving (Show, Eq)

-- An unary operator in LLVM IR.
data LLUnaryOp = LLFNeg  -- Floating-point negation
    deriving (Show, Eq)

-- A binary operator in LLVM IR.
data LLBinaryOp = LLAdd  -- Addition
                | LLFAdd -- Floating-point addition
                | LLSub  -- Subtraction
                | LLFSub -- Floating-point subtraction
                | LLMul  -- Multiplication
                | LLFMul -- Floating-point multiplication
                | LLUDiv -- Unsigned integer division
                | LLSDiv -- Signed integer division
                | LLFDiv -- Floating-point division
                | LLURem -- Unsigned integer division remainder
                | LLSRem -- Signed integer division remainder
                  -- * Bitwise operators
                | LLShl  -- Bitwise left-shift
                | LLLShr -- Logical right-shift
                | LLAShr -- Arithmetic right-shift
                | LLAnd  -- Bitwise logical and
                | LLOr   -- Bitwise logical inclusive or
                | LLXor  -- Bitwise logical exclusive or
    deriving (Show, Eq)

-- A typed value in LLVM IR.
data LLValue = LLValue { llValType :: LLType
                       , llValName :: String
                       }
    deriving (Show, Eq)

-- A type in LLVM IR.
data LLType = LLBasicType String -- A primitive type, e.g. double, i32, ...
            | LLFuncType  LLType [LLType]
            | LLPtrType   LLType
            | LLArrayType Int LLType
    deriving (Show, Eq)
