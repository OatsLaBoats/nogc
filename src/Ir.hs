module Ir where

-- I think the IR should be rather low level but still support functional abstractions
-- Maybe we should be more specific with borrow and mutate and add more intrinsics
-- The IR can be designed to have multiple stages. For example we can include pointers even if
-- they are only used to optimize the output code and not useful for static analyis.

type Identifier = String

data Type 
    = UnitT
    | BoolT
    | IntT
    | StringT
    | StringSliceT
    | FunctionT [Type] Type
    | BorrowT Type
    | RefT Type
    deriving Show

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

    : Static "aString" StringT (StringL "Hello, Great Queen Lyra!\\n")
    : Static "anInt" IntT (IntL 10)

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

data Construct
    = Function Identifier [(Identifier, Type)] Type Expr
    | Extern Identifier Type
    | Static Identifier Type Expr

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
    | Run Identifier [Expr]

data MemType = Heap | Arena
