module LLCurry.Utils.Pretty ( Pretty(..) ) where

class Pretty a where
    pretty :: a -> String
