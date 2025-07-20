
{-# LANGUAGE NondecreasingIndentation #-}

module Codegen where

import Data.Function
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Debug.Trace
import qualified Ir
import Control.Monad.State

-- TODO: Use the 'restrict' keyword for optimization purposes
--       https://en.cppreference.com/w/c/language/restrict.html

-- TODO: Clean this up to use State or Reader

ng_entryPoint = "ng_main"
ng_String = "struct ng_String"
ng_Int = "ng_Int"
ng_Bool = "ng_Bool"
ng_StringSlice = "struct ng_StringSlice"

-- Codegen should be super simple and straight forward if you need to do something special that should be done in the IR
data Context = Context
    { getLocals :: Map Ir.Identifier Ir.Type
    , getVarIndex :: Int
    }

getLocalType :: Ir.Identifier -> Context -> Ir.Type
getLocalType name ctx = fromJust $ Map.lookup name $ getLocals ctx

addLocal :: Ir.Identifier -> Ir.Type -> Context -> Context
addLocal name type' ctx = ctx { getLocals = Map.insert name type' (getLocals ctx) }

data Output = Output
    { getIncludes :: [String]
    , getGlobals :: [String]
    , getInitializers :: [String]
    , getFunctions :: [String]
    , getDeclarations :: [String]
    , getStructs :: [String]
    }

addGlobal :: String -> Output -> Output
addGlobal global output = output { getGlobals = global : (getGlobals output) }

addFunction :: String -> Output -> Output
addFunction function output = output { getFunctions = function : (getFunctions output) }

addDeclaration :: String -> Output -> Output
addDeclaration declaration output = output { getDeclarations = declaration : (getDeclarations output) }

generateC :: Output -> String
generateC output = includes ++ "\n" ++ structs ++ "\n" ++ globals ++ "\n" ++ declarations ++ "\n" ++ functions ++ "\n" ++ mainFunction
    where
        includes = concatMap (\x -> "#include \"" ++ x ++ "\"\n") (getIncludes output)
        globals = unlines $ getGlobals output
        declarations = unlines $ getDeclarations output
        functions = unlines $ getFunctions output
        structs = unlines $ getStructs output
        mainFunction = "int main(void){ng_main();return 0;}"

generateOutput :: [Ir.Construct] -> Output
generateOutput ir = foldl predicate output ir
    where
        predicate acc construct = case construct of
            Ir.Constant name _ expr -> case expr of
                Ir.StringL s ->
                    addGlobal ("static const " ++ ng_String ++ " " ++ name ++ "=" ++ generateStringL s ++ ";") acc
                Ir.IntL n ->
                    addGlobal ("static const " ++ ng_Int ++ " " ++ name ++ "=" ++ show n ++ ";") acc
                _ -> acc

            Ir.Function fnName params returnType expr ->
                let name = if fnName == "main" then ng_entryPoint else fnName in
                let locals = foldl (\acc' (name1, t) -> Map.insert name1 t acc') context params in
                let signature = generateFunctionSignature name params returnType in
                let body = generateFunctionBody expr returnType (Context locals 0) in
                addDeclaration (signature ++ ";") . addFunction (signature ++ body) $ acc
            _ -> acc

        context = foldl
            (\acc c -> case c of
                Ir.Constant name t _ -> Map.insert name t acc
                Ir.Function name params rt _ -> Map.insert name (Ir.FunctionT (map snd params) rt) acc
                Ir.Extern name t -> Map.insert name t acc
                Ir.Lambda id' params rt _ _ -> undefined
                _ -> undefined)
            Map.empty
            ir

        output = Output
            { getIncludes = ["builtins.h"]
            , getGlobals = []
            , getInitializers = []
            , getFunctions = []
            , getDeclarations = []
            , getStructs = []
            }

lambdaName :: Ir.Id -> String
lambdaName id' = "lambda_" ++ show id'

closureName :: Ir.Id -> String
closureName id' = "closure_" ++ show id'

generateFunctionSignature :: Ir.Identifier -> [(Ir.Identifier, Ir.Type)] -> Ir.Type -> String
generateFunctionSignature name params returnType =
    "static " ++ genReturnType ++ " " ++ name ++ "(" ++ genParams ++ ")"
    where
        genReturnType = case returnType of
            Ir.UnitT -> "ng_Unit"
            _ -> typeToString returnType

        genParams = if null params then "ng_Unit" else
            foldl (\acc (n, t) ->
                acc ++
                (if null acc then "" else ",") ++ typeToString t ++ " " ++ n)
                "" params

generateFunctionBody :: Ir.Expr -> Ir.Type -> Context -> String
generateFunctionBody expr returnType ctx =
    let (ExprOutput _ result code) = evalState (generateExpr expr) ctx in
    case returnType of
        Ir.UnitT -> "{\n" ++ code ++ "}\n"
        _ -> "{\n" ++ code ++ "return " ++ result ++ ";\n" ++ "}\n"

data ExprOutput = ExprOutput
    { getType :: Ir.Type
    , getResult :: String
    , getCode :: String
    }

boolToString :: Bool -> String
boolToString b = if b then "true" else "false"

getTempVar :: State Context String
getTempVar = do
    ctx <- get
    let varIndex = getVarIndex ctx
    put ctx { getVarIndex = varIndex + 1 }
    pure $ "_ng_tmpvar_" ++ show varIndex

-- All expression results are stored in a temporary variable before being used.
-- This bypasses the limitations of C and the compiler optimizes it all away anyway.
-- That being said some of these expressions can be done inline to make the output more readable.
-- Not every expression is multiline and requires setup.
-- In order to do that though we would need to add a Type to the return tuple
--
-- SourceCode is the required setup code for the expression that needs to run before it
-- ExprResult is the actualt expression you can pass to function calls and such
generateExpr :: Ir.Expr -> State Context ExprOutput
generateExpr expr = case expr of
    Ir.StringL str -> pure $ ExprOutput Ir.StringT (generateStringL str) ""
    Ir.StringSliceL str -> pure $ ExprOutput Ir.StringSliceT (generateStringSliceL str) ""
    Ir.IntL n -> pure $ ExprOutput Ir.IntT (show n) ""
    Ir.BoolL b -> pure $ ExprOutput Ir.BoolT (boolToString b) ""
    Ir.UnitL -> pure $ ExprOutput Ir.UnitT "" ""

    Ir.Label name -> pure $ ExprOutput Ir.UnitT "" (name ++ ":;\n")
    Ir.Jump label -> pure $ ExprOutput Ir.UnitT "" ("goto " ++ label ++ ";\n")

    Ir.Cond cond trueBranch falseBranch -> do
        var <- getTempVar
        out1 <- generateExpr cond
        out2 <- generateExpr trueBranch
        out3 <- generateExpr falseBranch
        
        if Ir.isUnit (getType out2) && Ir.isUnit (getType out3) then
            pure $ ExprOutput Ir.UnitT "" $
                getCode out1 ++
                "if(" ++ getResult out1 ++ "){\n" ++
                    getCode out2 ++
                "}\n" ++ "else{\n" ++
                    getCode out3 ++
                "}\n"
        else
            let type2 = getType out2
                type3 = getType out3 in 
            pure $ ExprOutput (if Ir.isUnit type2 then type3 else type2) var $
                generateVariable type2 var ++ "\n" ++
                getCode out1 ++
                "if(" ++ getResult out1 ++ "){\n" ++
                    getCode out2 ++
                    (if Ir.isUnit type2 then "" else generateAssignment var $ getResult out2 ++ "\n") ++
                "}\n" ++ "else{\n" ++
                    getCode out3 ++
                    (if Ir.isUnit type3 then "" else generateAssignment var $ getResult out3 ++ "\n") ++
                "}\n"

    Ir.Chain action cont -> do
        out1 <- generateExpr action
        out2 <- generateExpr cont
        pure $ ExprOutput (getType out2) (getResult out2) (getCode out1 ++ getCode out2)

    Ir.Clone name -> do
        varType <- gets $ getLocalType name
        case varType of
            Ir.IntT -> pure $ ExprOutput Ir.IntT name ""
            Ir.BoolT -> pure $ ExprOutput Ir.BoolT name ""
            Ir.StringT -> pure $ ExprOutput Ir.StringT (cloneString $ sliceString name) ""
            Ir.StringSliceT -> pure $ ExprOutput Ir.StringSliceT (cloneString name) ""
            _ -> undefined
        
    Ir.Move name -> do
        varType <- gets $ getLocalType name
        pure $ ExprOutput varType name ""

    Ir.Slice name -> do
        varType <- gets $ getLocalType name
        case varType of
            Ir.StringT -> pure $ ExprOutput Ir.StringSliceT (sliceString name) ""
            Ir.StringSliceT -> pure $ ExprOutput Ir.StringSliceT name ""
            _ -> undefined

    Ir.Run fn params -> do
        fnType <- gets $ getLocalType fn
        let retType = Ir.getFunctionReturnType fnType

        let paramPred e = do
            out <- generateExpr e
            pure (getResult out, getCode out)

        zippedResult <- mapM paramPred params
        let (params', paramCode) = unzip zippedResult
        let paramPassingCode = foldl (\acc p -> acc ++ (if null acc then "" else ",") ++ p) "" params'

        if Ir.isUnit retType then
            pure $ ExprOutput Ir.UnitT "" 
                (concat paramCode ++
                fn ++ "(" ++ paramPassingCode ++ ");\n")
        else
            -- The difference here is that we are returning the function call as an expression instead of a statement
            pure $ ExprOutput retType
                (fn ++ "(" ++ paramPassingCode ++ ");\n")
                (concat paramCode)

    Ir.Mutate name value -> do
        out <- generateExpr value
        pure $ ExprOutput Ir.UnitT "" (getCode out ++ generateAssignment name (getResult out) ++ "\n")

    Ir.Def name type' value -> do
        out <- generateExpr value
        modify $ addLocal name type'
        pure $ ExprOutput Ir.UnitT "" (getCode out ++ generateInitializedVariable type' name (getResult out) ++ "\n")

    Ir.Drop vars -> do
        locals <- gets getLocals
        let code = concatMap
                (\x -> case fromJust $ Map.lookup x locals of
                    Ir.StringT -> "ng_dropString(" ++ x ++ ");\n"
                    _ -> undefined) vars
        pure $ ExprOutput Ir.UnitT "" code
        
    _ -> undefined
    where
        generateVariable type' name = typeToString type' ++ " " ++ name ++ ";"
        generateAssignment name e = name ++ "=" ++ e ++ ";"
        generateInitializedVariable type' name e = typeToString type' ++ " " ++ name ++ "=" ++ e ++ ";"

generateStringL :: String -> String
generateStringL s = "(" ++ ng_String ++ "){.cap=0,.len=" ++ show (length s) ++ ",.mem=\"" ++ s ++ "\"}"

generateStringSliceL :: String -> String
generateStringSliceL s = "(" ++ ng_StringSlice ++ "){.len=" ++ show (length s) ++ ",.mem=\"" ++ s ++ "\"}"

sliceString :: String -> String
sliceString name = "ng_sliceString(" ++ name ++ ")"

cloneString :: String -> String
cloneString name = "ng_cloneString(" ++ name ++ ")"

typeToString :: Ir.Type -> String
typeToString t = case t of
    Ir.StringT -> ng_String
    Ir.IntT -> ng_Int
    Ir.BoolT -> ng_Bool
    Ir.StringSliceT -> ng_StringSlice
    _ -> undefined
