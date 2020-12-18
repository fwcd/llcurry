module LLCurry.IR.Pretty where

import LLCurry.IR.Types     ( LLProg (..)
                            , LLValue (..), LLLit (..), LLType (..)
                            )
import Text.Pretty          ( Pretty (..)
                            , (<+>), (<>), ($$), (<$+$>)
                            , nest, hcat, punctuate
                            , parens, brackets, braces
                            , comma, space, char, int, text
                            )

-- TODO: Pretty instances for IR structures

-- Spaces per indentation level.
level :: Int
level = 4

instance Pretty LLProg where
    pretty = error "TODO: Implement LLVM IR pretty-printing!"

instance Pretty LLValue where
    pretty val = case val of
        LLConstant t l -> pretty t <+> pretty l
        LLVariable t s -> pretty t <+> text ('%' : s)

instance Pretty LLLit where
    pretty lit = case lit of
        LLLitBool b | b         -> text "true"
                    | otherwise -> text "false"
        LLLitInt i              -> int i
        LLLitNull               -> text "null"
        LLLitStruct vs          -> braces   $ hcat $ punctuate (comma <> space) $ map pretty vs
        LLLitArray vs           -> brackets $ hcat $ punctuate (comma <> space) $ map pretty vs

instance Pretty LLType where
    pretty ty = case ty of
        LLVoidType      -> text "void"
        LLBasicType s   -> text s
        LLFuncType r ps -> pretty r <+> parens (hcat $ punctuate (comma <> space) $ map pretty ps)
        LLPtrType t     -> pretty t <+> char '*'
        LLArrayType n t -> brackets $ int n <+> char 'x' <+> pretty t
        LLStructType s  -> text ('%' : s)
