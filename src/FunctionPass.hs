module FunctionPass () where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Ir

-- Transforms LambdaRun to Run as needed

data Context = Context
    { getFunctionSet :: Set Ir.Identifier
    }

runFunctionPass :: [Ir.Construct] -> [Ir.Construct]
runFunctionPass ir = undefined
