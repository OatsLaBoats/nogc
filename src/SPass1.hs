module SPass1 (runSPass1) where

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

data Context = Context
    { getFunctionMap :: Map Ir.Identifier Ir.Type
    , getFunctionAnalyzedSet :: Set Ir.Identifier
    , getFunctionBeingAnalyzedSet :: Set Ir.Identifier -- Used to not fall into infinite recursion
    , getFunctionDataMap :: Map Ir.Identifier Ir.Construct

    , getConstMap :: Map Ir.Identifier Ir.Type
    , getIr :: [Ir.Construct]
    }

addFunctionBeingAnalyzed :: Ir.Identifier -> Context -> Context
addFunctionBeingAnalyzed name ctx = ctx { getFunctionBeingAnalyzedSet = Set.insert name $ getFunctionBeingAnalyzedSet ctx }

removeFunctionBeingAnalyzed :: Ir.Identifier -> Context -> Context
removeFunctionBeingAnalyzed name ctx = ctx { getFunctionBeingAnalyzedSet = Set.delete name $ getFunctionBeingAnalyzedSet ctx }

getFunctionData :: Ir.Identifier -> Context -> Ir.Construct
getFunctionData name ctx = fromJust $ Map.lookup name $ getFunctionDataMap ctx

isFunctionAnalyzed :: Ir.Identifier -> Context -> Bool
isFunctionAnalyzed name ctx = Set.member name $ getFunctionAnalyzedSet ctx

isFunctionBeingAnalyzed :: Ir.Identifier -> Context -> Bool
isFunctionBeingAnalyzed name ctx = Set.member name $ getFunctionBeingAnalyzedSet ctx

addConstruct :: Ir.Construct -> Context -> Context
addConstruct construct ctx = ctx { getIr = (construct : getIr ctx) }

runSPass1 :: [Ir.Construct] -> [Ir.Construct]
runSPass1 ir = reverse $ getIr $ foldl (flip constructPass1) context ir
    where
        context = Context
            { getFunctionMap = functionMap
            , getFunctionAnalyzedSet = functionAnalyzedSet
            , getFunctionBeingAnalyzedSet = Set.empty
            , getFunctionDataMap = functionDataMap

            , getConstMap = constMap
            , getIr = ir
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
constructPass1 :: Ir.Construct -> Context -> Context
constructPass1 construct ctx = case construct of
    Ir.Function _ _ _ _ -> analyzeFunctionParams construct ctx
    _ -> addConstruct construct ctx

analyzeFunctionParams :: Ir.Construct -> Context -> Context
analyzeFunctionParams construct ctx =
    if isFunctionAnalyzed name ctx then ctx else
    let ctx' = addFunctionBeingAnalyzed name ctx in
    let newParams = 
            map (\(name', type') -> -- TODO: We need to swap to fold since we are now dealing with state
                let (ctx1, owned) = isBindingOwned name expr ctx' in
                if owned
                then (name, Ir.OwnedT type')
                else (name, Ir.BorrowedT (Ir.Foreign 1) type') -- TODO: Figure out lifetimes cause this won't work
            )
    in undefined
    where
        (name, params, retType, expr) = case construct of
            Ir.Function name params retType expr -> (name, params, retType, expr)
            _ -> undefined

-- TODO: Need to handle recursion
isBindingOwned :: Ir.Identifier -> Ir.Expr -> Context -> (Context, Bool)
isBindingOwned name expr ctx = case expr of
    Ir.Run callee params ->
        --if isFunctionBeingAnalyzed callee ctx then

        let ctx' = if isFunctionAnalyzed callee ctx then ctx else
                let construct = getFunctionData callee ctx in
                analyzeFunctionParams construct ctx
        in

        let functionType = fromJust $ Map.lookup callee $ getFunctionMap ctx' in -- TODO: Make a helper for this
        let index = getCloneIndex name params in
        let type' = getFunctionParamsFromType functionType !! index in
        (ctx, isOwnedType type')

    Ir.Chain action cont ->
        let (ctx1, owned1) = isBindingOwned name action ctx in
        let (ctx2, owned2) = isBindingOwned name cont ctx1 in
        (if owned1 then ctx1 else ctx2, if owned1 then True else owned2) -- NOTE: Use lazyness to do this and not overdo it
    _ -> (ctx, False)


-- TODO: Some these should be moved to the Ir file as they are common helpers
getFunctionParamsFromType :: Ir.Type -> [Ir.Type]
getFunctionParamsFromType type' = case type' of
    Ir.FunctionT params _ -> params
    _ -> undefined

isBorrowedOrOwnedType :: Ir.Type -> Bool
isBorrowedOrOwnedType type' = isBorrowedType type' || isOwnedType type'

isBorrowedType :: Ir.Type -> Bool
isBorrowedType type' = case type' of
    Ir.BorrowedT _ _ -> True
    _ -> False

isOwnedType :: Ir.Type -> Bool
isOwnedType type' = case type' of
    Ir.OwnedT _ -> True
    _ -> False

getCloneIndex :: Ir.Identifier -> [Ir.Expr] -> Int
getCloneIndex name params = loop 0 params
    where
        loop index params = case params of
            ((Ir.Clone name') : xs) -> if name == name' then index else loop (index + 1) xs
            (_ : xs) -> loop (index + 1) xs
            _ -> undefined
