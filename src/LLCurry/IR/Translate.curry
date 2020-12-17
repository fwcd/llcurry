module LLCurry.IR.Translate ( trIProg ) where

import ICurry.Types ( IProg (..) )

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> LLProg
trIProg = error "TODO: Implement ICurry -> LLVM IR translation!"
