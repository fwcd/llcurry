module LLCurry.IR.Translate
    ( TrM
    , runTrM
    , trIProg
    ) where

import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import Control.Monad.Trans.State  ( State, runState, get, modify )
import ICurry.Types               ( IProg (..), IFunction (..) )
import LLCurry.IR.Types           ( LLProg (..), LLBasicBlock (..), LLInst (..)
                                  , LLGlobal (..), LLValue (..), LLUntyped (..)
                                  , LLType (..)
                                  , i8, i64
                                  )

------------------------------------------------
-- Utilities                                  --
------------------------------------------------

data TrState = TrState { nextId :: Int
                       , blockStack :: [LLBasicBlock]
                       , globals :: [LLGlobal]
                       }

--- The monad holding state during translation.
type TrM = ExceptT String (State TrState)

--- Runs the translation monad using the default initial state.
runTrM :: TrM a -> Either String a
runTrM m = fst $ runState (runExceptT m) initial
    where initial = TrState { nextId = 0
                            , blockStack = []
                            , globals = []
                            }

--- Fetches the translation state.
getTrState :: TrM TrState
getTrState = lift get

--- Modifies the translation state.
modifyTrState :: (TrState -> TrState) -> TrM ()
modifyTrState f = lift $ modify f

--- Fetches a fresh identifier.
freshId :: TrM Int
freshId = do
    st <- getTrState
    let i = nextId st
    modifyTrState $ \s -> s { nextId = i + 1 }
    return i

--- Pushes a new basic block onto the block stack.
pushBlock :: LLBasicBlock -> TrM ()
pushBlock b = do
    st <- getTrState
    modifyTrState $ \s -> s { blockStack = b : blockStack st }

--- Pops the topmost basic block from the block stack.
--- Throws an error if it does not succeeds.
popBlock :: TrM LLBasicBlock
popBlock = do
    st <- getTrState
    case blockStack st of
        (b:bs) -> do
            modifyTrState $ \s -> s { blockStack = bs }
            return b
        [] -> throwE "Cannot pop from empty block stack!"

--- Adds an instruction to the topmost block.
addInst :: LLInst -> TrM ()
addInst inst = do
    b <- popBlock
    pushBlock b { llBasicBlockInsts = llBasicBlockInsts b ++ [inst] }

--- Adds a global declaration.
addGlobal :: LLGlobal -> TrM ()
addGlobal g = modifyTrState $ \s -> s { globals = globals s ++ [g] }

------------------------------------------------
-- Runtime types and instructions             --
------------------------------------------------

--- The type name for applied Curry constructors.
curryNodeName :: String
curryNodeName = "CurryNode"

--- A type that represents an applied Curry constructor.
curryNodeType :: LLType
curryNodeType = LLStructType curryNodeName

--- A type that represents a pointer to an applied Curry constructor.
curryNodePtrType :: LLType
curryNodePtrType = LLPtrType curryNodeType

--- A type that represents applied Curry constructors at runtime.
--- Should be consistent with curryNodeByteSize.
curryNodeDecl :: LLGlobal
curryNodeDecl = LLTypeDecl curryNodeName
    [ i64 -- The type index
    , i64 -- The constructor index
    , i64 -- The arity
    -- TODO: Ref-counting?
    , curryNodePtrType
    ]

--- The size of a Curry node in bytes.
--- Should be consistent with curryNodeDecl.
--- TODO: This currently assumes a 64-bit target
---       (The child ptr type is not necessarily
---       64 bits in size)
curryNodeByteSize :: Int
curryNodeByteSize = 4 * 64

--- Adds the declarations needed by the Curry runtime,
--- e.g. the Curry node structure.
addRuntimeDecls :: TrM ()
addRuntimeDecls = addGlobal curryNodeDecl

--- Allocates a new Curry node and returns the name of
--- the pointer.
allocCurryNode :: TrM String
allocCurryNode = do
    i <- freshId
    let ident = "currynode" ++ show i
    -- TODO: This assumes a 64-bit system
    -- TODO: Declare malloc
    addInst $ LLCallInst (LLPtrType i8) "malloc" [LLValue i64 (LLLitInt curryNodeByteSize)]
    -- TODO: Retain
    return ident

--- TODO: Retain/release functions for Curry nodes

------------------------------------------------
-- Translation                                --
------------------------------------------------

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> TrM LLProg
trIProg (IProg mod imps types funcs) = do
    addRuntimeDecls
    llFuncs <- mapM trIFunction funcs
    return $ LLProg llFuncs

--- Translates an ICurry function to LLVM IR.
trIFunction :: IFunction -> TrM LLGlobal
trIFunction (IFunction qn ar vis _ body) = error "TODO: Function generation not implemented"
