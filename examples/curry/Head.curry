-- Compiles to LLVM IR and should work.

import Prelude hiding ( head )

head (x:_) = x
