module LLCurry.Utils.General
    ( forM_
    ) where

--- Flipped version of mapM_. Useful for imperative-style
--- 'for-each loops'.
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_
