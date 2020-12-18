module LLCurry.IR.Pretty where

import Prelude hiding ( empty )

import LLCurry.IR.Types     ( LLProg (..), LLFunc (..), LLInst (..), LLBasicBlock (..)
                            , LLBinaryOp (..), LLUnaryOp (..)
                            , LLValue (..), LLLabel (..), LLUntyped (..), LLType (..)
                            )
import Text.Pretty          ( Pretty (..), Doc
                            , (<+>), (<>), ($$), (<$+$>)
                            , nest, hcat, vcat, punctuate, align, indent
                            , parens, brackets, braces, lbrace, rbrace
                            , comma, colon, space, char, int, text
                            , empty
                            )

-- Spaces per indentation level.
level :: Int
level = 4

-- Comma-separates and concatenates documents.
commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate (comma <> space)

instance Pretty LLFunc where
    pretty f = (text "define" <+> pretty (llFuncType f)
                              <+> text ('@' : llFuncName f) <> parens (commaSep $ map pretty $ llFuncArgs f)
                              <+> lbrace)
                              $$ indent level (vcat $ map pretty $ llFuncBlocks f)
                              $$ rbrace
instance Pretty LLBasicBlock where
    pretty bb = maybe empty ((<> colon) . text) (llBasicBlockName bb)
           $$ vcat (map pretty $ llBasicBlockInsts bb)

instance Pretty LLProg where
    pretty = error "TODO: Implement LLVM IR pretty-printing!"

instance Pretty LLInst where
    pretty inst = case inst of
        LLReturnInst r         -> text "ret" <+> pretty r
        LLUncondBranchInst b   -> text "br" <+> pretty b
        LLCondBranchInst c t f -> text "br" <+> pretty c <> comma
                                            <+> pretty t <> comma
                                            <+> pretty f
        LLSwitchInst c o bs    -> text "switch" <+> pretty c <> comma
                                                <+> pretty o
                                                <+> brackets (align $ vcat $ map (\(v, l) -> pretty v <> comma <+> pretty l) bs)
        LLUnaryInst op v       -> pretty op <+> pretty v
        LLBinaryInst op l r    -> pretty op <+> pretty (llValType l) <+> pretty (llValUntyped l) <> comma
                                                                     <+> pretty (llValUntyped r) -- TODO: Assert that left and right types are equal
        LLAllocaInst t c       -> text "alloca" <+> pretty t <> maybe empty ((comma <+>) . (text "i32" <+>) . int) c
        LLLoadInst t p         -> text "load" <+> pretty t <> comma <+> pretty p
        LLStoreInst s p        -> text "store" <+> pretty s <> comma <+> pretty p
        LLCallInst t n as      -> text "call" <+> pretty t <+> text ('@' : n) <> parens (commaSep $ map pretty as)

instance Pretty LLLabel where
    pretty (LLLabel l) = text "label" <+> text ('%' : l)

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
    pretty (LLValue t u) = pretty t <+> pretty u

instance Pretty LLUntyped where
    pretty lit = case lit of
        LLLitBool b | b         -> text "true"
                    | otherwise -> text "false"
        LLLitInt i              -> int i
        LLLitNull               -> text "null"
        LLLitStruct vs          -> braces   $ commaSep $ map pretty vs
        LLLitArray vs           -> brackets $ commaSep $ map pretty vs
        LLLocalVar v            -> text ('%' : v)
        LLGlobalVar v           -> text ('@' : v)

instance Pretty LLType where
    pretty ty = case ty of
        LLVoidType      -> text "void"
        LLBasicType s   -> text s
        LLFuncType r ps -> pretty r <+> parens (commaSep $ map pretty ps)
        LLPtrType t     -> pretty t <+> char '*'
        LLArrayType n t -> brackets $ int n <+> char 'x' <+> pretty t
        LLStructType s  -> text ('%' : s)
