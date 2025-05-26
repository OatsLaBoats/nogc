module Codegen where

import Data.Function
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Ir

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

data Output = Output
    { getIncludes :: [String]
    , getGlobals :: [String]
    , getInitializers :: [String]
    , getFunctions :: [String]
    , getDeclarations :: [String]
    }

addGlobal :: String -> Output -> Output
addGlobal global output = output { getGlobals = global : (getGlobals output) }

addFunction :: String -> Output -> Output
addFunction function output = output { getFunctions = function : (getFunctions output) }

addDeclaration :: String -> Output -> Output
addDeclaration declaration output = output { getDeclarations = declaration : (getDeclarations output) }

generateC :: Output -> String
generateC output = includes ++ "\n" ++ globals ++ "\n" ++ declarations ++ "\n" ++ functions ++ "\n" ++ mainFunction
    where
        includes = concat $ map (\x -> "#include \"" ++ x ++ "\"\n") (getIncludes output)
        globals = concat $ map (++"\n") (getGlobals output)
        declarations = concat $ map (++"\n") (getDeclarations output)
        functions = concat $ map (++"\n") (getFunctions output)
        mainFunction = "int main(void){ng_main();return 0;}"

generateOutput :: [Ir.Construct] -> Output
generateOutput ir = foldl pred output ir
    where
        pred acc construct = case construct of
            Ir.Static name _ expr -> case expr of
                Ir.StringL s ->
                    addGlobal ("static const " ++ ng_String ++ " " ++ name ++ "=" ++ generateStringL s ++ ";") acc
                Ir.IntL n ->
                    addGlobal ("static const " ++ ng_Int ++ " " ++ name ++ "=" ++ show n ++ ";") acc
                _ -> acc
            Ir.Function name' params returnType expr ->
                let name = if name' == "main" then ng_entryPoint else name' in
                let locals = foldl (\acc (name, t) -> Map.insert name t acc) context params in
                let signature = generateFunctionSignature name params returnType in
                let body = generateFunctionBody (Context locals 0) expr returnType in
                addDeclaration (signature ++ ";") . addFunction (signature ++ body) $ acc
            _ -> acc

        context = foldl
            (\acc c -> case c of
                Ir.Static name t _ -> Map.insert name t acc
                Ir.Function name params rt _ -> Map.insert name (Ir.FunctionT (map snd params) rt) acc
                Ir.Extern name t -> Map.insert name t acc)
            Map.empty
            ir

        output = Output
            { getIncludes = ["builtins.h"]
            , getGlobals = []
            , getInitializers = []
            , getFunctions = []
            , getDeclarations = []
            }

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
                (if null acc then "" else ",") ++ (typeToString t) ++ " " ++ n)
                "" params

generateFunctionBody :: Context -> Ir.Expr -> Ir.Type -> String
generateFunctionBody ctx expr returnType =
    let (src, res, _) = generateExpr ctx expr in
    case returnType of
        Ir.UnitT -> "{\n" ++ src ++ "}\n"
        _ -> "{\n" ++ src ++ "return " ++ res ++ ";\n" ++ "}\n"

type ExprResult = String
type VarId = Int

-- All expression results are stored in a temporary variable before being used.
-- This bypasses the limitations of C and the compiler optimizes it all away anyway.
generateExpr :: Context -> Ir.Expr -> (String, ExprResult, Context)
generateExpr ctx@(Context locals varIndex) expr = case expr of
    Ir.StringL str ->
        let var = generateTempVarName ctx in
        (generateVariable Ir.StringT var ++ "\n" ++
         generateAssignment var (generateStringL str) ++ "\n",
         var, Context locals $ varIndex + 1)
    Ir.StringSliceL str ->
        let var = generateTempVarName ctx in
        (generateVariable Ir.StringSliceT var ++ "\n" ++
         generateAssignment var (generateStringSliceL str) ++ "\n",
         var, Context locals $ varIndex + 1)
    Ir.IntL n ->
        let var = generateTempVarName ctx in
        (generateVariable Ir.IntT var ++ "\n" ++
         generateAssignment var (show n) ++ "\n",
         var, Context locals $ varIndex + 1)
    Ir.BoolL b ->
        let var = generateTempVarName ctx in
        (generateVariable Ir.BoolT var ++ "\n" ++
         generateAssignment var (if b then "true" else "false") ++ "\n",
         var, Context locals $ varIndex + 1)
    Ir.UnitL -> ("", "", ctx)

    Ir.Chain action cont ->
        let (src1, _, ctx1) = generateExpr ctx action in
        let (src2, res2, ctx2) = generateExpr ctx1 cont in
        (src1 ++ src2, res2, ctx2)

    Ir.Clone name -> case fromJust $ Map.lookup name locals of
        Ir.IntT -> ("", name, ctx)
        Ir.BoolT -> ("", name, ctx)

        Ir.StringT ->
            let var = generateTempVarName ctx in
            (generateVariable Ir.StringT var ++ "\n" ++
             generateAssignment var (cloneString $ sliceString name) ++ "\n",
             var, Context locals (varIndex + 1))

        Ir.StringSliceT ->
            let var = generateTempVarName ctx in
            (generateVariable Ir.StringT var ++ "\n" ++
             generateAssignment var (cloneString name) ++ "\n",
             var, Context locals (varIndex + 1))

        _ -> undefined

    Ir.Move name -> ("", name, ctx)

    Ir.Slice name -> case fromJust $ Map.lookup name locals of
        Ir.StringT ->
            let var = generateTempVarName ctx in
            (generateVariable Ir.StringSliceT var ++ "\n" ++ 
             generateAssignment var (sliceString name) ++ "\n",
             var, Context locals (varIndex + 1))

        Ir.StringSliceT -> ("", name, ctx)
        _ -> undefined

    Ir.Run fn params ->
        -- I don't understand the indendation error here
        let retType = case fromJust $ Map.lookup fn locals of
                Ir.FunctionT _ rt -> rt
                _ -> undefined
        in

        -- This is unreadable.
        let (params', paramSrc, ctx'@(Context locals' varIndex')) = foldl (\(acc, src, ctx1) e ->
                let (src', res', ctx2) = generateExpr ctx1 e 
                in (acc ++ [res'], src ++ src', ctx2))
                ([], "", ctx) params
        in

        case retType of
            Ir.UnitT -> 
                (paramSrc ++ fn ++ "(" ++ (foldl (\acc p -> acc ++ (if null acc then "" else ",") ++ p) "" params') ++ ");\n", "", ctx')
            _ ->
                let var = generateTempVarName ctx' in
                (paramSrc ++
                 generateVariable retType var ++ "\n" ++
                 generateAssignment var
                 (fn ++ "(" ++ (foldl (\acc p -> acc ++ (if null acc then "" else ",") ++ p) "" params') ++ ")") ++ "\n",
                 var,
                 Context locals' $ varIndex' + 1)

    Ir.Mutate name value ->
        let (src1, res1, ctx1) = generateExpr ctx value in
        (src1 ++ generateAssignment name res1 ++ "\n", "", ctx1)

    Ir.Def name tp value ->
        let (src1, res1, Context locals1 varIndex1) = generateExpr ctx value in
        let ctx1 = Context (Map.insert name tp locals1) varIndex1 in
        (src1 ++ generateVariable tp name ++ "\n" ++ generateAssignment name res1 ++ "\n", "", ctx1)

    Ir.Drop vars ->
        (foldl
            (\acc x -> case fromJust $ Map.lookup x locals of
                Ir.StringT -> acc ++ "ng_dropString(" ++ x ++ ");\n"
                _ -> undefined)
            "" vars, "", ctx)

    _ -> undefined
    where
        generateTempVarName (Context _ i) = "_ng_tmpvar_" ++ show i
        generateVariable type' name = typeToString type' ++ " " ++ name ++ ";"
        generateAssignment name e = name ++ "=" ++ e ++ ";"

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
