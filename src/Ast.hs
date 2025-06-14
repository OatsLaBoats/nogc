module Ast where

data Type
    = UnitT
    | IntT
    | StringT
    | FunctionT [Type] Type
    | OwnedT Type -- This is used to ensure that some function parameters are owned Only useful for parameters as the return type is always owned
    deriving Eq

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
    | Lambda [(Name, Type)] Expr -- Params Ret-type Expr
    | Get Name
    | Call Expr [Expr] -- Runs a lambda expression
