module LLCurry.IR.Translate
    ( TrM
    , runTrM
    , trIProg
    ) where

import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import Control.Monad.Trans.State  ( State, runState, get, modify )
import ICurry.Types               ( IProg (..) )
import LLCurry.IR.Types           ( LLProg (..), LLBasicBlock (..), LLInst (..) )

------------------------------------------------
-- Utilities                                  --
------------------------------------------------

data TrState = TrState { nextId :: Int
                       , blockStack :: [LLBasicBlock]
                       }

--- The monad holding state during translation.
type TrM = ExceptT String (State TrState)

--- Runs the translation monad using the default initial state.
runTrM :: TrM a -> Either String a
runTrM m = fst $ runState (runExceptT m) initial
    where initial = TrState { nextId = 0
                            , blockStack = []
                            }

--- Fetches the translation state.
getTrState :: TrM TrState
getTrState = lift get

--- Modifies the translation state.
modifyTrState :: (TrState -> TrState) -> TrM ()
modifyTrState f = lift $ modify f

--- Fetches a fresh identifier.
freshId :: TrM String
freshId = do
    st <- getTrState
    let i = nextId st
    modifyTrState $ \s -> s { nextId = i + 1 }
    return $ show i

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

------------------------------------------------
-- Translation                                --
------------------------------------------------

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> TrM LLProg
trIProg = error "TODO: Implement ICurry -> LLVM IR translation!"
