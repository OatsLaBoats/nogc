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
    = Declaration "printLn" (NgFunction [NgString] NgUnit)
    : []

program :: [Binding]
program
    = Binding "anInt" NgInt (IntLit 10)
    : Binding "aString" NgString (StringLit "Hello World")
    : Binding "main" (NgFunction [] NgUnit) (Lambda [] 
        (Do (Call (Get "printLn") [Get "aString"]) UnitLit))
    : []
