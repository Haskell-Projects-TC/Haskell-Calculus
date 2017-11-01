module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]


unOpfuncs = [ (Neg, (negate))
            , (Sin, (sin))
            , (Cos, (cos))
            , (Log, (log))
            ]

binOpfuncs = [ (Add, (+))
             , (Mul, (*))
             , (Div, (/))
             ]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp key values
  = fromJust (lookup key values) 

eval :: Exp -> Env -> Double
eval (Val x) _
  = x
eval (Id str) env
  = lookUp str env
eval (BinApp binOp exp exp') env
  = (lookUp binOp binOpfuncs) (eval exp env) (eval exp' env)
eval (UnApp unOp exp) env
  = (lookUp unOp unOpfuncs) (eval exp env)

diff :: Exp -> String -> Exp
diff (BinApp Mul exp exp') str
  = BinApp Add (BinApp Mul (diff exp str) (Val 1.0)) (BinApp Mul (diff exp' str) (Val 1.0))
--diff (BinApp Add exp exp') str
--  = 
--diff (BinApp Div exp exp') str
--  =
diff (UnApp Sin exp) str
  = BinApp Mul (diff exp str) (UnApp Cos exp)
diff (UnApp Cos exp) str
  = BinApp Mul (UnApp Neg (diff exp str)) (UnApp Sin exp)

maclaurin :: Exp -> Double -> Int -> Double
maclaurin = error "TODO: implement maclaurin"

showExp :: Exp -> String
showExp = error "TODO: implement showExp"

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
