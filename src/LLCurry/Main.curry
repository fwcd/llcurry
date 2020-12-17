module LLCurry.Main ( main ) where

import ICurry.Files          ( readICurry )
import ICurry.Types          ( IProg (..) )
import LLCurry.IR.Pretty
import LLCurry.IR.Translate  ( trIProg )
import LLCurry.IR.Types      ( LLProg (..) )
import LLCurry.Utils.Pretty  ( Pretty (..) )
import System.Environment    ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    iProg <- case args of
        []          -> error "Please specify the module name of a Curry program that has been compiled to ICurry!"
        (modName:_) -> readICurry modName
    -- Translate to LLVM IR
    let llProg = trIProg (iProg :: IProg)
    -- TODO: Save to *.ll file
    putStrLn $ pretty (llProg :: LLProg)
