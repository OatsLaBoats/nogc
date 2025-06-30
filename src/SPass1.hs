module SPass1 (runSPass1) where

-- This static pass will convert functions to use borrowing where possible
-- 1. It converts what it can to using using borrows
-- 2. Removes most cloning ops

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Ir

data Context = Context
    { getFunctionMap :: Map Ir.Identifier Ir.Type
    , getFunctionAnalizedMap :: Map Ir.Identifier ()
    , getFunctionDataMap :: Map Ir.Identifier Ir.Construct
    , getConstMap :: Map Ir.Identifier Ir.Type
    , getIr :: [Ir.Construct]
    }

isFunctionAnalyzed :: Ir.Identifier -> Context -> Bool
isFunctionAnalyzed name ctx = Map.member name $ getFunctionAnalizedMap ctx

addConstruct :: Ir.Construct -> Context -> Context
addConstruct construct ctx = ctx { getIr = (construct : getIr ctx) }

runSPass1 :: [Ir.Construct] -> [Ir.Construct]
runSPass1 ir = reverse $ getIr $ foldl (flip constructPass1) context ir
    where
        context = Context
            { getFunctionMap = functionMap
            , getFunctionAnalizedMap = functionAnalyzedMap
            , getFunctionDataMap = functionDataMap
            , getConstMap = constMap
            , getIr = ir
            }

        functionDataMap = foldl
            (\acc c -> case c of
                Ir.Function name _ _ _ -> Map.insert name c acc
                _ -> acc)
            Map.empty ir

        functionAnalyzedMap = foldl
            (\acc c -> case c of
                Ir.Extern name _ -> Map.insert name () acc
                _ -> acc)
            Map.empty ir

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
    Ir.Function _ _ _ _ -> functionPass1 construct ctx
    _ -> addConstruct construct ctx

functionPass1 :: Ir.Construct -> Context -> Context
functionPass1 construct ctx = case construct of
    Ir.Function name params retType expr ->
        if isFunctionAnalyzed name ctx then ctx else
        undefined
    _ -> undefined
