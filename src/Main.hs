module Main where

import System.Exit

import Ast
import qualified Ir (generateIr)
import qualified Codegen as C
import qualified Typecheck as T

main :: IO ()
main = do
    let code = library ++ program
    
    case T.typecheck code of
        Just msg -> putStrLn msg >> exitFailure
        Nothing -> pure ()

    let ir = Ir.generateIr code
    putStrLn $ show ir

    let output = C.generateOutput ir
    let cSource = C.generateC output
    writeFile "output.c" cSource

library :: [Binding]
library
    = Extern "ng_printLn" (FunctionT [StringT] (OwnedT UnitT))
    : Extern "ng_addInt" (FunctionT [OwnedT IntT, IntT] (OwnedT IntT))
    : []

program :: [Binding]
program
    = Binding "anInt" IntT (IntL 10)
    : Binding "aString" StringT (StringL "Hello World")
    : Binding "main" (FunctionT [] UnitT) (Lambda [] UnitT 
        (Do (Call (Get "ng_printLn") [Get "aString"]) UnitL))
    : []
