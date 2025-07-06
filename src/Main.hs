module Main where

import System.Exit

import Ast
import qualified Ir
import qualified SPass1
import qualified Codegen as C
import qualified Typecheck as T

-- TODO: Fix all the order of declaration errors all over the place

main :: IO ()
main = do
    let code = library ++ program
    
    case T.typecheck code of
        Just msg -> putStrLn msg >> exitFailure
        Nothing -> pure ()

    let ir = Ir.generateIr code
    let ir1 = SPass1.runSPass1 ir
    putStrLn $ Ir.prettyShowIr ir1

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

    : Binding "myAdd" (FunctionT [IntT, IntT] IntT)
        (Lambda [("x", IntT), ("y", IntT)] IntT 
            (Call (Get "ng_addInt") [Get "x", Get "y"]))

    : Binding "myAdd2" (FunctionT [IntT, IntT] IntT)
        (Lambda [("x", IntT), ("y", IntT)] IntT 
            (Call (Get "myAdd") [Get "x", Get "y"]))

    : Binding "main" (FunctionT [] UnitT) (Lambda [] UnitT 
        (Do (Call (Get "ng_printLn") [Get "aString"]) UnitL))

    : []
