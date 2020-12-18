module LLCurry.IR.Pretty where

import LLCurry.IR.Types     ( LLProg (..)
                            , LLBinaryOp (..), LLUnaryOp (..)
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

instance Pretty LLUnaryOp where
    pretty op = case op of
        LLFNeg -> text "fneg"

instance Pretty LLBinaryOp where
    pretty op = case op of
        LLAdd  -> text "add"
        LLFAdd -> text "fadd"
        LLSub  -> text "sub"
        LLFSub -> text "fsub"
        LLMul  -> text "mul"
        LLFMul -> text "fmul"
        LLUDiv -> text "udiv"
        LLSDiv -> text "sdiv"
        LLFDiv -> text "fdiv"
        LLURem -> text "urem"
        LLSRem -> text "srem"
        LLShl  -> text "shl"
        LLLShr -> text "lshr"
        LLAShr -> text "ashr"
        LLAnd  -> text "and"
        LLOr   -> text "or"
        LLXor  -> text "xor"

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
