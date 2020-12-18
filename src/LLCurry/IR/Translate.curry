module LLCurry.IR.Translate
    ( TrM
    , runTrM
    , trIProg
    ) where

import Control.Monad.Trans.State ( State, runState, get, modify )
import ICurry.Types              ( IProg (..) )

------------------------------------------------
-- Utilities                                  --
------------------------------------------------

data TrState = TrState { nextId :: Int
                       }

--- The monad holding state during translation.
type TrM = State TrState

--- Runs the translation monad using the default initial state.
runTrM :: TrM a -> a
runTrM m = fst $ runState m initial
    where initial = TrState { nextId = 0
                            }

--- Fetches a fresh identifier.
freshId :: TrM String
freshId = do
    st <- get
    let i = nextId st
    modify (\s -> s { nextId = i + 1 })
    return $ show i

------------------------------------------------
-- Translation                                --
------------------------------------------------

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> TrM LLProg
trIProg = error "TODO: Implement ICurry -> LLVM IR translation!"
