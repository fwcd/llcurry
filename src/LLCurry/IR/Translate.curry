module LLCurry.IR.Translate
    ( TrM
    , runTrM
    , trIProg
    ) where

import Control.Monad              ( void, join )
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import Control.Monad.Trans.State  ( State, runState, get, modify )
import Data.Char                  ( ord )
import ICurry.Types               ( IProg (..), IFunction (..), IFuncBody (..)
                                  , IBlock (..), IQName, IVarIndex, IAssign (..)
                                  , IExpr (..), ILiteral (..), IStatement (..)
                                  , IVarDecl (..), IConsBranch (..), ILitBranch (..)
                                  )
import LLCurry.IR.Types           ( LLProg (..), LLBasicBlock (..), LLInst (..)
                                  , LLGlobal (..), LLValue (..), LLUntyped (..)
                                  , LLType (..), LLLabel (..)
                                  , i8, i64, double, void_, makeBasicBlock
                                  )
import LLCurry.Utils.General      ( forM_, forFoldlM )

------------------------------------------------
-- Utilities                                  --
------------------------------------------------

data TrState = TrState { nextId :: Int
                       , blockStack :: [LLBasicBlock]
                       , globals :: [LLGlobal]
                       }

--- The monad holding state during translation.
type TrM = ExceptT String (State TrState)

--- Runs the translation monad using the default initial state.
runTrM :: TrM a -> Either String a
runTrM m = fst $ runState (runExceptT m) initial
    where initial = TrState { nextId = 0
                            , blockStack = []
                            , globals = []
                            }

--- Fetches the translation state.
getTrState :: TrM TrState
getTrState = lift get

--- Modifies the translation state.
modifyTrState :: (TrState -> TrState) -> TrM ()
modifyTrState f = lift $ modify f

--- Fetches a fresh id.
freshId :: TrM Int
freshId = do
    st <- getTrState
    let i = nextId st
    modifyTrState $ \s -> s { nextId = i + 1 }
    return i

--- Fetches a fresh, prefixed identifier.
freshName :: String -> TrM String
freshName pf = do
    i <- freshId
    return $ pf ++ ('_' : show i)

--- Pushes a new basic block onto the block stack.
pushBlock :: LLBasicBlock -> TrM ()
pushBlock b = do
    st <- getTrState
    modifyTrState $ \s -> s { blockStack = b : blockStack st }

--- Pops the topmost basic block from the block stack.
--- Throws an error if it does not succeeds.
popBlock :: TrM LLBasicBlock
popBlock = do
    st <- getTrState
    case blockStack st of
        (b:bs) -> do
            modifyTrState $ \s -> s { blockStack = bs }
            return b
        [] -> throwE "Cannot pop from empty block stack!"

--- Adds an instruction to the topmost block.
addInst :: LLInst -> TrM ()
addInst inst = do
    b <- popBlock
    pushBlock b { llBasicBlockInsts = llBasicBlockInsts b ++ [inst] }

--- Adds a global declaration.
addGlobal :: LLGlobal -> TrM ()
addGlobal g = modifyTrState $ \s -> s { globals = globals s ++ [g] }

------------------------------------------------
-- Runtime interfacing                        --
------------------------------------------------

--- Curry objects are represented _dynamically_ at
--- runtime, i.e. there is a single structure that
--- can represent any applied Curry constructor
--- (called 'CurryNode'). This structure additionally
--- holds information about which type it belongs
--- to, the constructor index and its reference count
--- (WIP).
---
--- Every Curry object is heap-allocated using 'malloc'
--- and reference-counted.

--- The type name for applied Curry constructors.
curryNodeName :: String
curryNodeName = "struct.CurryNode"

--- A type that represents a Curry node.
curryNodeType :: LLType
curryNodeType = LLStructType curryNodeName

--- A type that represents a pointer to a Curry node.
curryNodePtrType :: LLType
curryNodePtrType = LLPtrType curryNodeType

--- The type of a generated Curry function. Takes a pointer to a
--- Curry node pointer (an array at runtime).
curryFunctionType :: LLType
curryFunctionType = LLFuncType curryNodePtrType [curryNodePtrType]

--- The type that represents a point to a generated Curry function.
curryFunctionPtrType :: LLType
curryFunctionPtrType = LLPtrType curryFunctionType

--- Adds the declarations needed to interface with the Curry runtime.
addRuntimeDecls :: TrM ()
addRuntimeDecls = do
    addGlobal $ LLOpaqueTypeDecl curryNodeName
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewData" [i8, i64, i64]
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewFunction" [i8, curryFunctionPtrType]
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewInteger" [i64]
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewFloating" [double]
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewCharacter" [i8]
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewPlaceholder" []
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeNewFailure" []
    addGlobal $ LLFuncDecl curryNodePtrType "curryNodeAccess" [curryNodePtrType, i64]
    addGlobal $ LLFuncDecl i64 "curryNodeGetConstructor" [curryNodePtrType]
    addGlobal $ LLFuncDecl double "curryNodeGetFloating" [curryNodePtrType]
    addGlobal $ LLFuncDecl i64 "curryNodeGetInteger" [curryNodePtrType]
    addGlobal $ LLFuncDecl i8 "curryNodeGetCharacter" [curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodeEvaluate" [curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodeAssign" [curryNodePtrType, curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodeRetain" [curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodeRelease" [curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodeDataApply" [curryNodePtrType, curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodeFunctionApply" [curryNodePtrType, curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryNodePrint" [curryNodePtrType]
    addGlobal $ LLFuncDecl void_ "curryExempt" []

--- TODO: Retain/release functions for Curry nodes

------------------------------------------------
-- Translation                                --
------------------------------------------------

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> TrM LLProg
trIProg (IProg mod imps types funcs) = do
    -- TODO: Imports & types!
    -- TODO: Forward-declare all locally defined functions
    --       and/or sort them topologically
    addRuntimeDecls
    llFuncs <- mapM trIFunction funcs
    st <- getTrState
    return $ LLProg $ globals st ++ llFuncs

--- Translates an ICurry function to LLVM IR.
trIFunction :: IFunction -> TrM LLGlobal
trIFunction (IFunction qn arity vis _ body) = case body of
    -- TODO: Use visibility!
    -- TODO: Figure out whether to use qn or name for external functions
    IExternal name  -> return LLFuncDecl { llFuncType = curryNodePtrType
                                         , llFuncName = name
                                         , llFuncArgTypes = [curryNodePtrType]
                                         }
    IFuncBody block -> do
        bs <- trIBlock Nothing block
        return $ LLFuncDef { llFuncType = curryNodePtrType 
                           , llFuncName = trIQName qn
                           , llFuncArgs = [LLValue curryNodePtrType $ LLLocalVar $ varName 0]
                           , llFuncBlocks = bs
                           }

--- Translates an ICurry block to LLVM IR blocks.
trIBlock :: Maybe String -> IBlock -> TrM [LLBasicBlock]
trIBlock label (IBlock vs as stmt) = do
    pushBlock $ LLBasicBlock label []
    mapM_ trIVarDecl vs
    mapM_ trIAssign as
    b <- popBlock
    bs <- trIStatement Nothing stmt
    return $ b : bs

--- Translates a variable declaration to LLVM IR instructions.
trIVarDecl :: IVarDecl -> TrM ()
trIVarDecl d = case d of
    IVarDecl  i -> addInst $ LLLocalAssign (varName i) $ LLCallInst curryNodePtrType "curryNodeNewPlaceholder" []
    IFreeDecl _ -> throwE "TODO: Free declarations are not implemented yet!"

--- Translates a variable assignment to LLVM IR instructions
--- and adds them to the topmost block.
trIAssign :: IAssign -> TrM ()
trIAssign a = case a of
    IVarAssign i expr     -> do
        n <- trExpr expr
        addInst $ LLCallInst void_ "curryNodeAssign"
            [ LLValue curryNodePtrType $ LLLocalVar $ varName i
            , LLValue curryNodePtrType $ LLLocalVar n
            ]
    INodeAssign i js expr -> throwE "TODO: Node assign is not implemented yet!"

--- Translates a statement to LLVM IR blocks.
trIStatement :: Maybe String -> IStatement -> TrM [LLBasicBlock]
trIStatement label stmt = case stmt of
    IExempt -> do
        pushBlock $ LLBasicBlock label []
        addInst $ LLCallInst void_ "curryExempt" []
        addInst LLUnreachable
        b <- popBlock
        return [b]
    IReturn e -> do
        pushBlock $ LLBasicBlock label []
        n <- trExpr e
        addInst $ LLReturnInst $ LLValue curryNodePtrType $ LLLocalVar n
        b <- popBlock
        return [b]
    ICaseCons i brs -> do
        el <- freshName "end"
        be <- head <$> trIStatement (Just el) IExempt -- TODO: Handle the 'otherwise' case in a better way?
        bs <- mapM trIConsBranch brs
        pushBlock $ LLBasicBlock Nothing []
        cn <- freshName "constr"
        let v = LLValue curryNodePtrType $ LLLocalVar $ varName i
        addInst $ LLCallInst void_ "curryNodeEvaluate" [v]
        addInst $ LLLocalAssign cn $ LLCallInst i64 "curryNodeGetConstructor" [v]
        addInst $ LLSwitchInst (LLValue i64 $ LLLocalVar cn) (LLLabel el)
            [(LLValue i64 $ LLLitInt c, LLLabel n)
            | (IConsBranch (_, _, c) a _, (LLBasicBlock (Just n) _ : _)) <- zip brs bs]
        b <- popBlock
        return $ b : (join bs ++ [be])
    ICaseLit i brs@(br:_) -> do
        el <- freshName "end"
        be <- head <$> trIStatement (Just el) IExempt -- TODO: Handle the 'otherwise' case in a better way?
        bs <- mapM trILitBranch brs
        pushBlock $ LLBasicBlock Nothing []
        cn <- freshName "constr"
        let v = LLValue curryNodePtrType $ LLLocalVar $ varName i
        addInst $ LLCallInst void_ "curryNodeEvaluate" [v]
        -- We assume that literal case expressions only contain one type of values
        -- and therefore determine it from the first branch.
        addInst $ LLLocalAssign cn $ case br of
            (ILitBranch (IInt _) _)   -> LLCallInst i64 "curryNodeGetInteger" [v]
            (ILitBranch (IFloat _) _) -> LLCallInst double "curryNodeGetFloating" [v]
            (ILitBranch (IChar _) _)  -> LLCallInst i8 "curryNodeGetCharacter" [v]
        let cv = flip LLValue (LLLocalVar cn) $ case br of
                    (ILitBranch (IInt _) _)   -> i64
                    (ILitBranch (IFloat _) _) -> double
                    (ILitBranch (IChar _) _)  -> i8
        addInst $ LLSwitchInst cv (LLLabel el)
            [(trILiteral lit, LLLabel n)
            | (ILitBranch lit _, (LLBasicBlock (Just n) _ : _)) <- zip brs bs]
        b <- popBlock
        return $ b : (join bs ++ [be])
    _         -> throwE $ "TODO: Tried to translate unsupported statement " ++ show stmt

--- Translates a constructor branch to LLVM IR blocks.
trIConsBranch :: IConsBranch -> TrM [LLBasicBlock]
trIConsBranch (IConsBranch _ _ b) = do
    n <- freshName "ccase"
    trIBlock (Just n) b

--- Translates a constructor branch to LLVM IR blocks.
trILitBranch :: ILitBranch -> TrM [LLBasicBlock]
trILitBranch (ILitBranch _ b) = do
    n <- freshName "lcase"
    trIBlock (Just n) b

--- Translates an expression to LLVM IR instructions. Returns the
--- variable name the expression was assigned to.
trExpr :: IExpr -> TrM String
trExpr e = do
    case e of
        IVar i         -> do
            -- Translate variable reference
            return $ varName i
        ILit lit       -> do
            -- Translate literal
            n <- freshName "lit"
            let fn = case lit of
                        IInt _   -> "curryNodeNewInteger"
                        IChar _  -> "curryNodeNewCharacter"
                        IFloat _ -> "curryNodeNewFloating"
                v = trILiteral lit
            addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType fn [v]
            return n
        IVarAccess j js -> do
            -- Translate node indexing
            forFoldlM (varName j) js $ \m j' -> do
                n <- freshName "access"
                addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType "curryNodeAccess"
                    [ LLValue curryNodePtrType $ LLLocalVar m
                    , LLValue i64 $ LLLitInt j'
                    ]
                return n
        IFCall qn@(_, ln, _) as -> do
            -- Translate function call
            n <- freshName $ "fcall_" ++ escapeString ln
            addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType "curryNodeNewFunction"
                [ LLValue i8 $ LLLitInt $ length as -- arity
                , LLValue curryFunctionPtrType $ LLGlobalVar $ trIQName qn
                ]
            forM_ as $ \a -> do
                an <- trExpr a
                addInst $ LLCallInst void_ "curryNodeFunctionApply"
                    [ LLValue curryNodePtrType $ LLLocalVar n
                    , LLValue curryNodePtrType $ LLLocalVar an
                    ]
            return n
        IFPCall qn@(_, ln, _) missing as -> do
            -- Translate partial function call
            n <- freshName $ "fpcall_" ++ escapeString ln
            addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType "curryNodeNewFunction"
                [ LLValue i8 $ LLLitInt $ missing + length as -- arity
                , LLValue curryFunctionPtrType $ LLGlobalVar $ trIQName qn
                ]
            forM_ as $ \a -> do
                an <- trExpr a
                addInst $ LLCallInst void_ "curryNodeFunctionApply"
                    [ LLValue curryNodePtrType $ LLLocalVar n
                    , LLValue curryNodePtrType $ LLLocalVar an
                    ]
            return n
        ICCall qn@(_, ln, _) as -> do
            -- Translate constructor call
            n <- freshName $ "ccall_" ++ escapeString ln
            addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType "curryNodeNewData"
                [ LLValue i8 $ LLLitInt $ length as -- arity
                , LLValue i64 $ LLLitInt 0 -- TODO: Generate type id from qn
                , LLValue i64 $ LLLitInt 0 -- TODO: Generate constructor index from qn
                ]
            forM_ as $ \a -> do
                an <- trExpr a
                addInst $ LLCallInst void_ "curryNodeDataApply"
                    [ LLValue curryNodePtrType $ LLLocalVar n
                    , LLValue curryNodePtrType $ LLLocalVar an
                    ]
            return n
        ICPCall qn@(_, ln, _) missing as -> do
            -- Translate partial constructor call
            n <- freshName $ "pccall_" ++ escapeString ln
            addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType "curryNodeNewData"
                [ LLValue i8 (LLLitInt $ missing + length as) -- arity
                , LLValue i64 (LLLitInt 0) -- TODO: Generate type id from qn
                , LLValue i64 (LLLitInt 0) -- TODO: Generate constructor index from qn
                ]
            forM_ as $ \a -> do
                an <- trExpr a
                addInst $ LLCallInst void_ "curryNodeDataApply"
                    [ LLValue curryNodePtrType $ LLLocalVar n
                    , LLValue curryNodePtrType $ LLLocalVar an
                    ]
            return n
        IOr e1 e2 -> do
            -- Translate non-deterministic choice
            n <- freshName "choice"
            e1n <- trExpr e1
            e2n <- trExpr e2
            addInst $ LLLocalAssign n $ LLCallInst curryNodePtrType "curryNodeNewChoice"
                [ LLValue curryNodePtrType $ LLLocalVar e1n
                , LLValue curryNodePtrType $ LLLocalVar e2n
                ]
            return n
        _ -> throwE $ "TODO: Tried to translate unsupported expression " ++ show e

--- Translates a literal to an LLVM IR value.
trILiteral :: ILiteral -> LLValue
trILiteral lit = case lit of
    IInt i   -> LLValue i64 $ LLLitInt i
    IChar c  -> LLValue i8 $ LLLitInt $ ord c
    IFloat f -> LLValue double $ LLLitFloat f

--- Combines a qualified ICurry name to a single name/identifier.
trIQName :: IQName -> String
trIQName (modName, locName, n) = "qn_" ++ escapeString modName ++ "_" ++ escapeString locName ++ "_" ++ show n

--- Fetches the name of a variable identified by an index.
varName :: IVarIndex -> String
varName i = "var_" ++ show i

--- 'Escapes' a string for use in identifiers.
escapeString :: String -> String
escapeString = concatMap escapeChar

--- 'Escapes' a character for use in identifiers.
--- Source: https://git.ps.informatik.uni-kiel.de/curry/curry2go/-/blob/d76cceab303773ef34474da6cb39ce42ae5ff1ff/src/Curry2Go/Compiler.curry#L369
escapeChar :: Char -> String
escapeChar c = case c of
    '$'  -> "dol"
    ')'  -> "rb"
    '('  -> "lb"
    '+'  -> "add"
    ','  -> "comma"
    '.'  -> "-"
    '#'  -> "hash"
    '-'  -> "sub"
    '*'  -> "mul"
    '/'  -> "slash"
    '%'  -> "percent"
    '['  -> "lsb"
    ']'  -> "rsb"
    '{'  -> "lcb"
    '}'  -> "rcb"
    ':'  -> "col"
    '^'  -> "pow"
    '@'  -> "at"
    '!'  -> "excl"
    '?'  -> "qstn"
    '&'  -> "and"
    '='  -> "eq"
    '<'  -> "lt"
    '>'  -> "gt"
    ';'  -> "semi"
    '|'  -> "strt"
    '\\' -> "bslash"
    '\'' -> "squote"
    '"'  -> "dquote"
    '~'  -> "tilde"
    '`'  -> "accent"
    _    -> [c]
