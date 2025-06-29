module SPass1 (runSPass1) where

-- This static pass will convert functions to use borrowing where possible
-- 1. It converts what it can to using using borrows
-- 2. Removes most cloning ops

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Ir

data Context = Context
    { getFunctionMap :: Map Ir.Identifier Ir.Type
    , getConstMap :: Map Ir.Identifier Ir.Type
    }

runSPass1 :: [Ir.Construct] -> [Ir.Construct]
runSPass1 ir = undefined
    where
        context = Context
            { getFunctionMap = functionMap
            , getConstMap = constMap
            }

        functionMap = foldl
            (\acc c -> case c of
                Ir.Function name _ _ _-> Map.insert name (Ir.functionConstructToType c) acc
                _ -> acc)
            Map.empty ir

        constMap = foldl
            (\acc c -> case c of
                Ir.Constant name type' _ -> Map.insert name type' acc
                _ -> acc)
            Map.empty ir
