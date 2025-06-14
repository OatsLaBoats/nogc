module Ir where

import qualified Ast
import qualified Data.Map as Map
import Data.Map (Map)

-- I think the IR should be rather low level but still support functional abstractions
-- Maybe we should be more specific with borrow and mutate and add more intrinsics
-- The IR can be designed to have multiple stages. For example we can include pointers even if
-- they are only used to optimize the output code and not useful for static analyis.

-- The context required for the translation of the AST to the IR 
data Context = Context
    { getLambdaIndex :: Int
    , getConstructs :: [Construct]
    , getFunctionMap :: Map Identifier Type
    }

addConstruct :: Construct -> Context -> Context
addConstruct construct context =
    context { getConstructs = construct : getConstructs context }

addFunction :: Identifier -> Type -> Context -> Context
addFunction identifier type' context =
    context { getFunctionMap = Map.insert identifier type' $ getFunctionMap context }

type Identifier = String
type Id = Int
type Field = (Identifier, Type)
type Param = (Identifier, Type)

data Type 
    = UnitT
    | BoolT
    | IntT
    | StringT
    | StringSliceT
    | FunctionT [Type] Type
    | RefT Type
    | OwnedT Type
    | RecordT Identifier -- Has to be a construct
    | LambdaT Id
    deriving Show

data Construct
    = Function Identifier [Param] Type Expr
    | Extern Identifier Type
    | Constant Identifier Type Expr
    | Record Identifier [Field]
    | Lambda Id [Field] [Param] Type Expr -- id capturedVariables params retType body

data Expr
    -- These are useful for tail recursion later
    = Label String
    | Jump String

    | Cond Expr Expr Expr -- condition trueBranch falseBranch

    -- These are used to chain expressions into something resembling procedural code
    | Chain Expr Expr

    -- Literals
    | StringL String
    | StringSliceL String
    | IntL Int
    | BoolL Bool
    | UnitL

    -- Captures locals and makes a lambda
    | Capture Id

    -- Memory allocation instructions
    | Drop [Identifier]

    -- Passing values values
    | Borrow Identifier -- This never gets to the code generator

    | Move Identifier
    | Clone Identifier
    | Ref Identifier
    | Slice Identifier

    | Def Identifier Type Expr -- Creates a new value returns Unit
    | Mutate Identifier Expr -- Changes the value of a variable returns Unit

    -- Execute functions
    | Run Identifier [Expr]
    | RunLambda Expr

data MemType = Heap | Arena

generateIr :: [Ast.Binding] -> [Construct]
generateIr = getConstructs . foldl predicate context
    where
        context = Context 0 [] Map.empty
        predicate acc b = case b of
            Ast.Extern name type' -> addConstruct (Extern name (astTypeToIrType type')) acc
            Ast.Binding name type' expr -> generateBinding name type' expr acc

generateBinding :: Ast.Name -> Ast.Type -> Ast.Expr -> Context -> Context
generateBinding name type' expr ctx = case type' of
    Ast.FunctionT _ retType ->
        let params = case expr of
                Ast.Lambda ps _ -> map (\(nm, tp) -> (nm, astTypeToIrType tp)) ps
                _ -> []
        in
        addConstruct (Function name params (astTypeToIrType retType) resExpr2) ctx2
    _ ->
        addConstruct (Constant name (astTypeToIrType type') resExpr1) ctx1
    where
        (ctx1, resExpr1) = generateExpr expr ctx

        -- Remove the lambda if we are dealing with a function
        (ctx2, resExpr2) = case expr of
            Ast.Lambda _ expr' -> generateExpr expr' ctx
            _ -> undefined

generateExpr :: Ast.Expr -> Context -> (Context, Expr)
generateExpr expr ctx = case expr of
    Ast.UnitL -> simple UnitL
    Ast.IntL n -> simple $ IntL n
    Ast.StringL s -> simple $ StringL s
    Ast.Let name type' action cont ->
        let (ctx1, actionExpr) = generateExpr action ctx in
        let (ctx2, contExpr) = generateExpr cont ctx1 in
        (ctx2, Chain (Def name (astTypeToIrType type') actionExpr) contExpr)
    Ast.Do action cont ->
        let (ctx1, actionExpr) = generateExpr action ctx in
        let (ctx2, contExpr) = generateExpr cont ctx1 in
        (ctx2, Chain actionExpr contExpr)
    Ast.Lambda params lexpr -> undefined
    Ast.Get name -> simple $ Clone name

    -- Needs to check if a function is a lambda or a normal function and run accordingly
    Ast.Call callExpr params -> undefined
    where
        simple e = (ctx, e)

astTypeToIrType :: Ast.Type -> Type
astTypeToIrType type' = case type' of
    Ast.UnitT -> UnitT
    Ast.IntT -> IntT
    Ast.StringT -> StringT
    Ast.FunctionT params retType -> FunctionT (map astTypeToIrType params) (astTypeToIrType retType)
    Ast.OwnedT innerType -> OwnedT $ astTypeToIrType innerType

isUnit :: Type -> Bool
isUnit UnitT = True
isUnit _ = False

-- How does interop with c work when we don't know if the C code takes a reference or an owned value?
-- I think we have to just pass everything by reference and just ask people to not modify the value...
-- At least for external c interop. Compiler builtins are special since the compiler understands them.

-- For fun lets try to hand compile this program
-- No optimization
myCompiledIr
    = Extern "ng_printLn" (FunctionT [StringSliceT] UnitT)
    : Extern "ng_addInt" (FunctionT [IntT, IntT] IntT)
    : Extern "ng_subInt" (FunctionT [IntT, IntT] IntT)
    : Extern "ng_eqInt" (FunctionT [IntT, IntT] BoolT)

    : Constant "aString" StringT (StringL "Hello, Great Queen Lyra!\\n")
    : Constant "anInt" IntT (IntL 10)

    : Function "main" [] UnitT 
        (Chain (Run "ng_printLn" [Slice "aString"])
        (Chain (Def "ls" StringT (StringL "You look lovely today!"))
        (Chain (Run "testMath" [IntL 10, IntL 4])
        (Chain (Cond (BoolL True) (IntL 10) (IntL 20))
        (Chain (Run "ng_printLn" [StringSliceL "You look great!"]) (Drop ["ls"])
        )))))

    : Function "testMath" [("a", IntT), ("b", IntT)] IntT
        (Run "ng_addInt" [Clone "a", Run "ng_subInt" [IntL 10, Clone "b"]])

    : Function "testFunc" [("x", BoolT)] IntT
        (Cond (Clone "x") (IntL 10) (IntL 20))

    : Function "fibonacci" [("i", IntT)] IntT
        (Cond (Run "ng_eqInt" [Clone "i", IntL 0]) (IntL 0)
        (Cond (Run "ng_eqInt" [Clone "i", IntL 1]) (IntL 1)
            (Run "ng_addInt" 
                [Run "fibonacci" [Run "ng_subInt" [Clone "i", IntL 1]],
                 Run "fibonacci" [Run "ng_subInt" [Clone "i", IntL 2]]]
        )))

    : Function "fib" [("n", IntT)] IntT
        (Run "_fib" [Clone "n", IntL 0, IntL 1, IntL 0])

    : Function "_fib" [("n", IntT), ("a", IntT), ("b", IntT), ("index", IntT)] IntT
        (Chain (Label "tailcall")
        (Cond (Run "ng_eqInt" [Clone "index", Clone "n"]) (Clone "a")
        (Chain (Def "c" IntT (Clone "a"))
        (Chain (Mutate "a" (Clone "b"))
        (Chain (Mutate "b" (Run "ng_addInt" [Clone "c", Clone "b"]))
        (Chain (Mutate "index" (Run "ng_addInt" [Clone "index", IntL 1]))
        (Jump "tailcall")
        ))))))
        
    : []
