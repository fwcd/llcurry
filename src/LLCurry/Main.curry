module LLCurry.Main ( main ) where

import ICurry.Files          ( readICurry )
import ICurry.Types          ( IProg (..) )
import LLCurry.IR.Pretty
import LLCurry.IR.Translate  ( trIProg, runTrM )
import LLCurry.IR.Types      ( LLProg (..) )
import System.CurryPath      ( inCurrySubdir )
import System.Environment    ( getArgs )
import System.FilePath       ( (<.>) )
import Text.Pretty           ( Pretty (..), pPrint )

main :: IO ()
main = do
    args <- getArgs

    -- Read ICurry file (which is assumed to have already been compiled)
    (modName, iProg) <- case args of
        []    -> error "Please specify the module name of a Curry program that has been compiled to ICurry!"
        (m:_) -> ((,) m) <$> readICurry m

    -- Translate to LLVM IR and save *.ll file into .curry folder
    case runTrM $ trIProg iProg of
        Right llProg -> do
            let llOutputPath = inCurrySubdir $ modName <.> "ll"
                llOutput = pPrint $ pretty (llProg :: LLProg)
            writeFile llOutputPath llOutput
        Left e       -> error $ "Compilation failed: " ++ e
