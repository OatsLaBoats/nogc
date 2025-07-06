module Typecheck
    ( typecheck
    ) where

import Ast

import qualified Data.Map as Map
import Data.Map (Map)

type Context = Map Name Type

typecheck :: [Binding] -> Maybe String
typecheck bindings = loop bindings
    where
        ctx = foldl (\acc binding -> Map.insert (bindingName binding) (bindingType binding) acc) Map.empty bindings

        loop [] = Nothing
        loop (x:xs) =
            case typecheckBinding ctx x of
                Nothing -> loop xs
                msg -> msg

typecheckBinding :: Context -> Binding -> Maybe String
typecheckBinding _ (Extern _ _) = Nothing
typecheckBinding ctx (Binding name btype expr) = 
    case typecheckExpr ctx expr of
        Left msg -> Just msg
        Right exprType ->
            if not $ compareType btype exprType
            then Just ("Binding type error at: " ++ name)
            else Nothing

typecheckExpr :: Context -> Expr -> Either String Type
typecheckExpr ctx expr = case expr of
    UnitL -> Right UnitT
    IntL _ -> Right IntT
    StringL _ -> Right StringT

    Do doExpr cont ->
        pure doExpr >>= typecheckExpr ctx >> pure cont >>= typecheckExpr ctx

    Let name letType letExpr cont -> do
        exprType <- typecheckExpr ctx letExpr
        let newCtx = Map.insert name letType ctx
        if letType /= exprType
            then Left "Let type error"
            else typecheckExpr newCtx cont

    Lambda params retType body -> do
        let newCtx = foldl (\acc (name, paramType) -> Map.insert name paramType acc) ctx params
        exprType <- typecheckExpr newCtx body
        let lambdaType = FunctionT (map snd params) exprType

        if not $ compareType exprType retType 
            then Left "Mismatched lambda return type with expr type"
            else Right lambdaType

    Get name -> maybe (Left $ name ++ " is undefined") Right $ Map.lookup name ctx

    Call callee params -> do
        calleeType <- typecheckExpr ctx callee
        paramTypes <- sequence $ map (typecheckExpr ctx) params
        case calleeType of
            FunctionT calleeParamTypes retType ->
                if not $ compareTypeList paramTypes calleeParamTypes
                then Left "Call type error"
                else Right retType
            _ -> Left "Can only call lambdas"
