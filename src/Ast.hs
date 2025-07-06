module Ast where

data Type
    = UnitT
    | IntT
    | StringT
    | FunctionT [Type] Type
    | OwnedT Type -- This is used to ensure that some function parameters are owned Only useful for parameters as the return type is always owned
    deriving (Eq, Show)

getFunctionReturnType :: Type -> Type
getFunctionReturnType type' = case type' of
    FunctionT _ rt -> rt
    _ -> undefined

compareTypeList :: [Type] -> [Type] -> Bool
compareTypeList (x : xs) (y : ys) = if not $ compareType x y then False else compareTypeList xs ys
compareTypeList [] [] = True
compareTypeList _ _ = False

compareType :: Type -> Type -> Bool
compareType a b = unwrapOwnedType a == unwrapOwnedType b

unwrapOwnedType :: Type -> Type
unwrapOwnedType type' = case type' of
    OwnedT t -> t
    FunctionT params retType ->
        FunctionT (map unwrapOwnedType params) (unwrapOwnedType retType)
    _ -> type'

data Binding
    = Binding Name Type Expr
    | Extern Name Type

bindingName :: Binding -> Name
bindingName (Binding name _ _) = name
bindingName (Extern name _) = name

bindingType :: Binding -> Type
bindingType (Binding _ btype _) = btype
bindingType (Extern _ btype) = btype

type Name = String

data Expr
    = UnitL
    | IntL Int
    | StringL String
    | Let Name Type Expr Expr
    | Do Expr Expr

    -- TODO: Weird design but whatever
    | Lambda [(Name, Type)] Type Expr -- Params Ret-type Expr
    | Get Name
    | Call Expr [Expr] -- Runs a lambda expression
