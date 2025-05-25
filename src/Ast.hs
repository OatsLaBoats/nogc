module Ast where

data Type
    = NgUnit
    | NgInt 
    | NgString
    | NgFunction [Type] Type
    deriving Eq

data Binding
    = Binding Name Type Expr
    | Declaration Name Type

bindingName :: Binding -> Name
bindingName (Binding name _ _) = name
bindingName (Declaration name _) = name

bindingType :: Binding -> Type
bindingType (Binding _ btype _) = btype
bindingType (Declaration _ btype) = btype

type Name = String

data Expr
    = UnitLit
    | IntLit Int
    | StringLit String
    | Let Name Type Expr Expr
    | Do Expr Expr
    | Lambda [(Name, Type)] Expr -- Params Ret-type Expr
    | Get Name
    | Call Expr [Expr] -- Runs a lambda expression
