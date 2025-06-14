module Main where

import System.Exit

import Ast
import qualified Ir
import qualified Codegen as C
import qualified Typecheck as T

main :: IO ()
main = do
    let code = library ++ program
    
    case T.typecheck code of
        Just msg -> putStrLn msg >> exitFailure
        Nothing -> pure ()

    let output = C.generateOutput Ir.myCompiledIr
    let cSource = C.generateC output
    writeFile "output.c" cSource

library :: [Binding]
library
    = Extern "ng_printLn" (FunctionT [StringT] UnitT)
    : Extern "ng_addInt" (FunctionT [OwnedT IntT, OwnedT IntT] IntT)
    : []

program :: [Binding]
program
    = Binding "anInt" IntT (IntL 10)
    : Binding "aString" StringT (StringL "Hello World")
    : Binding "main" (FunctionT [] UnitT) (Lambda [] 
        (Do (Call (Get "printLn") [Get "aString"]) UnitL))
    : []
