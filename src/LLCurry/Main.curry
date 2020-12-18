module LLCurry.Main ( main ) where

import ICurry.Files          ( readICurry )
import ICurry.Types          ( IProg (..) )
import LLCurry.IR.Pretty
import LLCurry.IR.Translate  ( trIProg, runTrM )
import LLCurry.IR.Types      ( LLProg (..) )
import System.Environment    ( getArgs )
import Text.Pretty           ( Pretty (..), pPrint )

main :: IO ()
main = do
    args <- getArgs
    iProg <- case args of
        []          -> error "Please specify the module name of a Curry program that has been compiled to ICurry!"
        (modName:_) -> readICurry modName
    -- Translate to LLVM IR
    case runTrM $ trIProg iProg of
        Right llProg -> putStrLn $ pPrint $ pretty (llProg :: LLProg) -- TODO: Save to *.ll file
        Left e       -> error $ "Compilation failed: " ++ e
