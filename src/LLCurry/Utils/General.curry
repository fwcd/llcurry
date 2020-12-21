module LLCurry.Utils.General
    ( forM_
    , foldlM
    , forFoldlM, forFoldlM_
    ) where

import Control.Monad ( void )

--- Flipped version of mapM_. Useful for imperative-style
--- 'for-each loops'.
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

--- A monadic left-fold.
foldlM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM f x = foldlM' (return x)
    where foldlM' acc []     = acc
          foldlM' acc (y:ys) = acc >>= \a -> foldlM' (f a y) ys

--- Flipped version of foldlM. Useful for imperative-style
--- 'aggregating loops'.
forFoldlM :: Monad m => b -> [a] -> (b -> a -> m b) -> m b
forFoldlM x xs f = foldlM f x xs

--- A version of forFoldlM that discards the result.
forFoldlM_ :: Monad m => b -> [a] -> (b -> a -> m b) -> m ()
forFoldlM_ x xs = void . forFoldlM_ x xs
