module LLCurry.IR.Pretty where

import LLCurry.IR.Types     ( LLProg (..) )
import LLCurry.Utils.Pretty ( Pretty (..) )

-- TODO: Pretty instances for IR structures

instance Pretty LLProg where
    pretty = error "TODO: Implement LLVM IR pretty-printing!"
