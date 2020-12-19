module LLCurry.IR.Pretty where

import Prelude hiding ( empty )

import LLCurry.IR.Types     ( LLProg (..), LLGlobal (..), LLInst (..), LLBasicBlock (..)
                            , LLBinaryOp (..), LLUnaryOp (..)
                            , LLValue (..), LLLabel (..), LLUntyped (..), LLType (..)
                            )
import Text.Pretty          ( Pretty (..), Doc
                            , (<+>), (<>), ($$), (<$+$>)
                            , nest, hcat, vcat, vsepBlank, punctuate, align, indent
                            , parens, brackets, braces, lbrace, rbrace, dquotes
                            , comma, colon, space, char, int, float, text
                            , empty
                            )

-- Spaces per indentation level.
level :: Int
level = 4

-- Comma-separates and concatenates documents.
commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate (comma <> space)

instance Pretty LLProg where
    pretty p = vsepBlank $ map pretty $ llProgGlobals p

instance Pretty LLGlobal where
    pretty f = case f of
        LLConstantDecl n v  -> text ('@' : n) <+> char '='
                                              <+> text "constant" <+> pretty v
        LLTypeDecl n fs     -> text ('%' : n) <+> char '='
                                              <+> text "type"
                                              <+> lbrace
                                              <+> commaSep (map pretty fs)
                                              <+> rbrace
        LLOpaqueTypeDecl n  -> text ('%' : n) <+> char '='
                                              <+> text "type"
                                              <+> text "opaque"
        LLFuncDef t n as bs -> (text "define" <+> pretty t
                                              <+> text ('@' : n) <> parens (commaSep $ map pretty as)
                                              <+> lbrace)
                                              $$ indent level (vcat $ map pretty bs)
                                              $$ rbrace
        LLFuncDecl t n as   -> text "declare" <+> pretty t
                                              <+> text ('@' : n) <> parens (commaSep $ map pretty as)

instance Pretty LLBasicBlock where
    pretty bb = maybe empty ((<> colon) . text) (llBasicBlockName bb)
           $$ vcat (map pretty $ llBasicBlockInsts bb)

instance Pretty LLInst where
    pretty inst = case inst of
        LLLocalAssign l i          -> text ('%' : l) <+> char '=' <+> pretty i
        LLReturnInst r             -> text "ret" <+> pretty r
        LLUncondBranchInst b       -> text "br" <+> pretty b
        LLCondBranchInst c t f     -> text "br" <+> pretty c <> comma
                                                <+> pretty t <> comma
                                                <+> pretty f
        LLSwitchInst c o bs        -> text "switch" <+> pretty c <> comma
                                                    <+> pretty o
                                                    <+> brackets (align $ vcat $ map (\(v, l) -> pretty v <> comma <+> pretty l) bs)
        LLUnaryInst op v           -> pretty op <+> pretty v
        LLBinaryInst op l r        -> pretty op <+> pretty (llValType l) <+> pretty (llValUntyped l) <> comma
                                                                         <+> pretty (llValUntyped r) -- TODO: Assert that left and right types are equal
        LLAllocaInst t c           -> text "alloca" <+> pretty t <> maybe empty ((comma <+>) . (text "i32" <+>) . int) c
        LLLoadInst t p             -> text "load" <+> pretty t <> comma <+> pretty p
        LLStoreInst s p            -> text "store" <+> pretty s <> comma <+> pretty p
        LLGetElementPtrInst b p is -> text "getelementptr" <+> pretty b <> comma <+> pretty p <> hcat (map ((comma <+>) . pretty) is)
        LLCallInst t n as          -> text "call" <+> pretty t <+> text ('@' : n) <> parens (commaSep $ map pretty as)

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
        LLLitFloat f            -> float f
        LLLitString s           -> char 'c' <> dquotes (text s)
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
        LLPtrType t     -> pretty t <> char '*'
        LLArrayType n t -> brackets $ int n <+> char 'x' <+> pretty t
        LLStructType s  -> text ('%' : s)
