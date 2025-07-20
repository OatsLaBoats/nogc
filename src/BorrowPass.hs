{-# LANGUAGE NondecreasingIndentation #-}

module BorrowPass (runBorrowPass) where

-- This static pass will convert functions to use borrowing where possible
-- 1. It converts what it can to using using borrows
-- 2. Converts every subsequent chain that uses it to a borrow
-- 3. Removes most cloning ops

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ir
import Control.Monad.State
import Control.Monad

data Context = Context
    { getFunctionMap :: Map Ir.Identifier Ir.Type
    , getFunctionAnalyzedSet :: Set Ir.Identifier
    , getFunctionBeingAnalyzedSet :: Set Ir.Identifier -- Used to not fall into infinite recursion
    , getFunctionDataMap :: Map Ir.Identifier Ir.Construct

    , getConstMap :: Map Ir.Identifier Ir.Type
    , getIr :: [Ir.Construct]
    }

addFunctionAnalyzed :: Ir.Identifier -> Context -> Context
addFunctionAnalyzed name ctx = ctx { getFunctionAnalyzedSet = Set.insert name $ getFunctionAnalyzedSet ctx }

addFunctionBeingAnalyzed :: Ir.Identifier -> Context -> Context
addFunctionBeingAnalyzed name ctx = ctx { getFunctionBeingAnalyzedSet = Set.insert name $ getFunctionBeingAnalyzedSet ctx }

removeFunctionBeingAnalyzed :: Ir.Identifier -> Context -> Context
removeFunctionBeingAnalyzed name ctx = ctx { getFunctionBeingAnalyzedSet = Set.delete name $ getFunctionBeingAnalyzedSet ctx }

addFunction :: Ir.Identifier -> Ir.Type -> Context -> Context
addFunction name type' ctx = ctx { getFunctionMap = Map.insert name type' $ getFunctionMap ctx }

getFunction :: Ir.Identifier -> Context -> Ir.Type
getFunction name ctx = fromJust $ Map.lookup name $ getFunctionMap ctx

getFunctionData :: Ir.Identifier -> Context -> Ir.Construct
getFunctionData name ctx = fromJust $ Map.lookup name $ getFunctionDataMap ctx

addFunctionData :: Ir.Identifier -> Ir.Construct -> Context -> Context
addFunctionData name construct ctx = ctx { getFunctionDataMap = Map.insert name construct $ getFunctionDataMap ctx }

isFunctionAnalyzed :: Ir.Identifier -> Context -> Bool
isFunctionAnalyzed name ctx = Set.member name $ getFunctionAnalyzedSet ctx

isFunctionBeingAnalyzed :: Ir.Identifier -> Context -> Bool
isFunctionBeingAnalyzed name ctx = Set.member name $ getFunctionBeingAnalyzedSet ctx

addConstruct :: Ir.Construct -> Context -> Context
addConstruct construct ctx = ctx { getIr = construct : getIr ctx }

runBorrowPass :: [Ir.Construct] -> [Ir.Construct]
runBorrowPass ir = getIr $ execState (constructPass1 ir) context
    where
        context = Context
            { getFunctionMap = functionMap
            , getFunctionAnalyzedSet = functionAnalyzedSet
            , getFunctionBeingAnalyzedSet = Set.empty
            , getFunctionDataMap = functionDataMap

            , getConstMap = constMap
            , getIr = []
            }

        functionDataMap = foldl
            (\acc c -> case c of
                Ir.Function name _ _ _ -> Map.insert name c acc
                _ -> acc)
            Map.empty ir

        functionAnalyzedSet = foldl
            (\acc c -> case c of
                Ir.Extern name _ -> Set.insert name acc
                _ -> acc)
            Set.empty ir

        functionMap = foldl
            (\acc c -> case c of
                Ir.Function name _ _ _ -> Map.insert name (Ir.functionConstructToType c) acc
                Ir.Extern name type' -> Map.insert name type' acc
                _ -> acc)
            Map.empty ir

        constMap = foldl
            (\acc c -> case c of
                Ir.Constant name type' _ -> Map.insert name type' acc
                _ -> acc)
            Map.empty ir

-- In pass 1 we convert all the function parameters to be owned or borrowed
constructPass1 :: [Ir.Construct] -> State Context ()
constructPass1 ir = case ir of
    (c : cs) -> do
        if Ir.isFunction c then analyzeFunctionParams c else modify $ addConstruct c
        constructPass1 cs
    _ -> pure ()

analyzeFunctionParams :: Ir.Construct -> State Context ()
analyzeFunctionParams construct = do
    fnAnalyzed <- gets $ isFunctionAnalyzed name
    if fnAnalyzed then pure () else do

    modify $ addFunctionBeingAnalyzed name
    newParams <- mapM paramPred params
    let newConstruct = Ir.Function name newParams retType expr

    modify $ removeFunctionBeingAnalyzed name
    modify $ addFunctionAnalyzed name
    modify $ addFunctionData name newConstruct
    modify $ addFunction name $ Ir.FunctionT (map snd newParams) retType
    modify $ addConstruct newConstruct
    where
        paramPred :: (Ir.Identifier, Ir.Type) -> State Context (Ir.Identifier, Ir.Type)
        paramPred (name', type') = do
            owned <- isBindingOwned name' expr
            pure (name', if owned then Ir.OwnedT type' else Ir.BorrowedT Ir.Foreign type')

        (name, params, retType, expr) = case construct of
            Ir.Function name' params' retType' expr' -> (name', params', retType', expr')
            _ -> undefined

isBindingOwned :: Ir.Identifier -> Ir.Expr -> State Context Bool
isBindingOwned name expr = case expr of
    Ir.Run callee params -> do
        fnBeingAnalyzed <- gets $ isFunctionBeingAnalyzed callee
        if fnBeingAnalyzed then pure False else do

        fnAnalyzed <- gets $ isFunctionAnalyzed callee
        unless fnAnalyzed $ do
            construct <- gets $ getFunctionData callee
            analyzeFunctionParams construct
        
        
        functionType <- gets $ getFunction callee
        let index = getCloneIndex name params
        let type' = Ir.getFunctionParamsType functionType !! index
        pure $ Ir.isOwnedType type'

    Ir.Chain action cont -> do
        owned1 <- isBindingOwned name action
        owned2 <- isBindingOwned name cont
        pure $ owned1 || owned2

    _ -> pure False

getCloneIndex :: Ir.Identifier -> [Ir.Expr] -> Int
getCloneIndex name = loop 0
    where
        loop index params = case params of
            ((Ir.Clone name') : xs) -> if name == name' then index else loop (index + 1) xs
            (_ : xs) -> loop (index + 1) xs
            _ -> undefined
