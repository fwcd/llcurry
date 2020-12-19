module LLCurry.IR.Translate
    ( TrM
    , runTrM
    , trIProg
    ) where

import Control.Monad              ( void )
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import Control.Monad.Trans.State  ( State, runState, get, modify )
import Data.Char                  ( ord )
import ICurry.Types               ( IProg (..), IFunction (..), IFuncBody (..)
                                  , IBlock (..), IQName, IVarIndex, IAssign (..)
                                  , IExpr (..), ILiteral (..)
                                  )
import LLCurry.IR.Types           ( LLProg (..), LLBasicBlock (..), LLInst (..)
                                  , LLGlobal (..), LLValue (..), LLUntyped (..)
                                  , LLType (..)
                                  , i8, i64, double, makeBasicBlock
                                  )

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
-- Runtime types and instructions             --
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
curryNodeName = "CurryNode"

--- A type that represents an applied Curry constructor.
curryNodeType :: LLType
curryNodeType = LLStructType curryNodeName

--- A type that represents a pointer to an applied Curry constructor.
curryNodePtrType :: LLType
curryNodePtrType = LLPtrType curryNodeType

--- A type that represents applied Curry constructors at runtime.
--- Should be consistent with curryNodeByteSize.
curryNodeDecl :: LLGlobal
curryNodeDecl = LLTypeDecl curryNodeName
    [ i64 -- The type index
    , i64 -- The constructor index
    , i64 -- The arity
    -- TODO: Ref-counting?
    , curryNodePtrType
    ]

--- The size of a Curry node in bytes.
--- Should be consistent with curryNodeDecl.
--- TODO: This currently assumes a 64-bit target
---       (The child ptr type is not necessarily
---       64 bits in size)
curryNodeByteSize :: Int
curryNodeByteSize = 4 * 64

--- Adds the declarations needed by the Curry runtime,
--- e.g. the Curry node structure.
addRuntimeDecls :: TrM ()
addRuntimeDecls = addGlobal curryNodeDecl

--- Allocates a new Curry node and returns the name of
--- the pointer. Optionally takes a custom name for the
--- pointer.
allocCurryNode :: Maybe String -> TrM String
allocCurryNode name = do
    i <- freshId
    let n = maybe ("node_" ++ show i) id name
    -- TODO: This assumes a 64-bit system
    -- TODO: Declare malloc
    addInst $ LLLocalAssign n $ LLCallInst (LLPtrType i8) "malloc" [LLValue i64 (LLLitInt curryNodeByteSize)]
    -- TODO: Retain
    return n

--- TODO: Retain/release functions for Curry nodes

------------------------------------------------
-- Translation                                --
------------------------------------------------

--- Translates the given ICurry program to LLVM IR.
trIProg :: IProg -> TrM LLProg
trIProg (IProg mod imps types funcs) = do
    -- TODO: Imports & types!
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
                                         , llFuncArgTypes = replicate arity curryNodePtrType
                                         }
    IFuncBody block -> do
        b <- trIBlock block
        return $ LLFuncDef { llFuncType = curryNodePtrType 
                           , llFuncName = trIQName qn
                           , llFuncArgs = map (LLValue curryNodePtrType . LLLocalVar . argName) [0 .. (arity - 1)]
                           , llFuncBlocks = [b]
                           }

--- Translates an ICurry block to LLVM IR.
trIBlock :: IBlock -> TrM LLBasicBlock
trIBlock (IBlock vs as stmt) = do
    pushBlock $ makeBasicBlock []
    mapM_ trIAssign as
    -- TODO: Handle statement
    popBlock

--- Translates a variable assignment to LLVM IR instructions
--- and adds them to the topmost block.
trIAssign :: IAssign -> TrM ()
trIAssign a = case a of
    IVarAssign i expr     -> void $ trExpr (Just $ varName i) expr
    INodeAssign i js expr -> throwE "TODO: Node assign is not implemented yet!"

--- Translates an expression to LLVM IR instructions, optionally
--- with a custom identifier. Returns the variable name of the
--- expression.
trExpr :: Maybe String -> IExpr -> TrM String
trExpr name e = do
    case e of
        IVar i         -> do
            -- Translate variable reference
            return $ varName i
        ILit lit       -> do
            -- Translate literal
            i <- freshId
            let v = case lit of
                        IInt i   -> LLValue i64 $ LLLitInt i
                        IChar c  -> LLValue i8 $ LLLitInt $ ord c
                        IFloat f -> LLValue double $ LLLitFloat f
                ty = llValType v
                n = maybe ("lit_" ++ show i) id name
            -- FIXME: This is probably not quite correct, since literals
            --        should be stored in Curry nodes too.
            addInst $ LLLocalAssign n $ LLAllocaInst ty Nothing
            addInst $ LLStoreInst v (LLValue (LLPtrType ty) (LLLocalVar n))
            return n
        IVarAccess i is -> do
            -- Translate node indexing
            -- TODO: Figure out whether this is the correct indexing order
            --       or whether 'is' should be reversed.
            i <- freshId
            let ivs = map (LLValue i64 . LLLitInt) $ 0 : is
                n = maybe ("access_" ++ show i) id name
            addInst $ LLLocalAssign n $ LLGetElementPtrInst curryNodeType (LLValue curryNodePtrType (LLLocalVar $ varName i)) ivs
            return n
        IFCall qn as -> do
            -- Translate function call
            i <- freshId
            ans <- mapM (trExpr Nothing) as
            let avs = map (LLValue curryNodePtrType . LLLocalVar) ans
                n = maybe ("call_" ++ show i) id name
            addInst $ LLLocalAssign n $ LLCallInst curryNodeType (trIQName qn) avs
            return n
        ICCall qn as -> do
            -- Translate constructor call
            n <- allocCurryNode name
            -- TODO: Translate constructor argument expression and
            --       use store instructions to add them to the node
            --       Perhaps 'allocCurryNode' should take an argument
            --       that determines the child count? Note that this
            --       would also affect the size-in-bytes of a curry node.
            return n
        _            -> throwE $ "TODO: Tried to translate unsupported expression " ++ show e

--- Combines a qualified ICurry name to a single name/identifier.
trIQName :: IQName -> String
trIQName (loc, mod, n) = "qn_" ++ escapeString loc ++ "_" ++ escapeString mod ++ "_" ++ show n

--- Fetches the name of a variable identified by an index.
varName :: IVarIndex -> String
varName i = "var_" ++ show i

--- Fetches the name/identifier of the nth function argument.
argName :: Int -> String
argName i = "arg_" ++ show i

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
