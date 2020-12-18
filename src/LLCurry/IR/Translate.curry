module LLCurry.IR.Translate ( trIProg ) where

import Control.Monad.Trans.State ( State, runState )
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

------------------------------------------------
-- Translation                                --
------------------------------------------------

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> LLProg
trIProg = error "TODO: Implement ICurry -> LLVM IR translation!"
