module LLCurry.IR.Types
    ( LLProg (..), LLGlobal (..), LLBasicBlock (..)
    , LLInst (..), LLUnaryOp (..), LLBinaryOp (..), LLReturnValue (..)
    , LLLabel (..), LLValue (..), LLUntyped (..), LLType (..)
    , i1, i8, i32, i64, float, double, void_
    , makeBasicBlock
    ) where

-- Useful references:
-- * https://llvm.org/docs/LangRef.html
-- * https://llvm.org/doxygen/dir_c3e93f23a4a31c717998b98ce143b7c0.html
-- * https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html
-- * https://mapping-high-level-constructs-to-llvm-ir.readthedocs.io/en/latest/README.html
-- * http://www.cs.cmu.edu/afs/cs/academic/class/15745-s14/public/lectures/L6-LLVM-Part2-1up.pdf

------------------------------------------------
-- IR syntax tree                             --
------------------------------------------------

-- A program in LLVM IR.
data LLProg = LLProg { llProgGlobals :: [LLGlobal]
                     }
    deriving (Show, Eq)

-- A global declaration or definition in LLVM IR.
data LLGlobal = -- * A constant
                LLConstantDecl   { llConstantName :: String
                                 , llConstantValue :: LLValue
                                 }
                -- * A structure declaration
              | LLTypeDecl       { llTypeName :: String
                                 , llTypeFields :: [LLType]
                                 }
                -- * An opaque type declaration
              | LLOpaqueTypeDecl { llTypeName :: String
                                 }
                -- * A function definition
              | LLFuncDef        { llFuncType :: LLType -- The return type
                                 , llFuncName :: String
                                 , llFuncArgs :: [LLValue]
                                 , llFuncBlocks :: [LLBasicBlock]
                                 }
                -- * An (external) function declaration
              | LLFuncDecl       { llFuncType :: LLType -- The return type
                                 , llFuncName :: String
                                 , llFuncArgTypes :: [LLType]
                                 }
    deriving (Show, Eq)

-- A labelled list of statements in LLVM IR.
data LLBasicBlock = LLBasicBlock { llBasicBlockName :: Maybe String
                                 , llBasicBlockInsts :: [LLInst]
                                 }
    deriving (Show, Eq)

-- A return value in LLVM IR.
data LLReturnValue = LLReturnValue LLValue
                   | LLReturnVoid
    deriving (Show, Eq)

-- An instruction/statement in LLVM IR.
data LLInst = LLLocalAssign String LLInst                       -- not an actual instruction, used to represent SSA-assignments
              -- * Terminator instructions
            | LLReturnInst LLReturnValue                        -- Returned value
            | LLUncondBranchInst LLLabel                        -- Block label (unconditional jump)
            | LLCondBranchInst LLValue LLLabel LLLabel          -- Condition value, ifTrue block label, ifFalse block label
            | LLSwitchInst LLValue LLLabel [(LLValue, LLLabel)] -- Condition value, otherwise block label, values and block labels
            | LLUnreachable
              -- * Unary and binary operations
            | LLUnaryInst LLUnaryOp LLValue                     -- Operator, operand
            | LLBinaryInst LLBinaryOp LLValue LLValue
              -- * Memory access and addressing instructions
            | LLAllocaInst LLType (Maybe Int)                   -- Allocated type, optionally a number of elements
            | LLLoadInst LLType LLValue                         -- Loaded type, pointer
            | LLStoreInst LLValue LLValue                       -- Stored value, pointer
            | LLGetElementPtrInst LLType LLValue [LLValue]      -- Base type, base pointer, indexes
              -- * Other instructions
            | LLCallInst LLType String [LLValue]                -- Return type, function name, function args
    deriving (Show, Eq)

-- A referenced label.
newtype LLLabel = LLLabel String
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
                       , llValUntyped :: LLUntyped
                       }
    deriving (Show, Eq)

-- An untyped value in LLVM IR.
data LLUntyped = LLLitBool Bool
               | LLLitInt Int
               | LLLitFloat Float
               | LLLitString String
               | LLLitNull
               | LLLitStruct [LLValue]
               | LLLitArray [LLValue]
               | LLLocalVar String
               | LLGlobalVar String
    deriving (Show, Eq)

-- A type in LLVM IR.
data LLType = LLVoidType
            | LLBasicType  String -- A primitive type, e.g. double, i32, ...
            | LLFuncType   LLType [LLType]
            | LLPtrType    LLType
            | LLArrayType  Int LLType
            | LLStructType String
    deriving (Show, Eq)

------------------------------------------------
-- Common LLVM types & convenience functions  --
------------------------------------------------

i1 :: LLType
i1 = LLBasicType "i1"

i8 :: LLType
i8 = LLBasicType "i8"

i32 :: LLType
i32 = LLBasicType "i32"

i64 :: LLType
i64 = LLBasicType "i64"

float :: LLType
float = LLBasicType "float"

double :: LLType
double = LLBasicType "double"

void_ :: LLType
void_ = LLBasicType "void"

makeBasicBlock :: [LLInst] -> LLBasicBlock
makeBasicBlock = LLBasicBlock Nothing
