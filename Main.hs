data AST a = Const a | 
             Variable String |
             Add (AST a) (AST a) | 
             Sub (AST a) (AST a) | 
             Mul (AST a) (AST a) | 
             Div (AST a) (AST a) | 
             Abs (AST a) | 
             Sgn (AST a) | 
             Sqrt (AST a) |
             Sin (AST a) |
             Cos (AST a) | 
             Sinh (AST a) |
             Cosh (AST a) |
             Asin (AST a) |
             Acos (AST a) | 
             Atan (AST a) |
             Asinh (AST a) |
             Acosh (AST a) |
             Atanh (AST a) |
             Exp (AST a) |
             Log (AST a)
           deriving Show

instance (Num a) => Num (AST a)
    where
      a + b = Add a b
      a - b = Sub a b
      a * b = Mul a b
      abs = Abs
      signum = Sgn 
      fromInteger = Const . fromInteger
instance Fractional a => Fractional (AST a)
    where
      a / b = Div a b
      fromRational = Const . fromRational
instance Floating a => Floating (AST a)
    where
      sin = Sin
      cos = Cos
      sinh = Sinh
      cosh = Cosh
      asin = Asin
      acos = Acos
      atan = Atan
      asinh = Asinh
      acosh = Acosh
      atanh = Atanh
      exp = Exp
      log = Log
      sqrt = Sqrt
      pi = Const pi

parseAST :: Show a => AST a -> String
parseAST (Const x) = show x
parseAST (Variable x) = x
parseAST (Add x y) = "(" ++ parseAST x ++ ")+(" ++ parseAST y ++ ")"
parseAST (Sub x y) = "(" ++ parseAST x ++ ")-(" ++ parseAST y ++ ")"
parseAST (Mul x y) = "(" ++ parseAST x ++ ")*(" ++ parseAST y ++ ")"
parseAST (Div x y) = "(" ++ parseAST x ++ ")/(" ++ parseAST y ++ ")"
parseAST (Abs x) = "abs("++parseAST x++")"
parseAST (Sgn x) = "sgn("++parseAST x++")"
parseAST (Sqrt x) = "sqrt("++parseAST x++")"
parseAST (Sin x) = "sin("++parseAST x++")"
parseAST (Cos x) = "cos("++parseAST x++")"
parseAST (Sinh x) = "sinh("++parseAST x++")"
parseAST (Cosh x) = "cosh("++parseAST x++")"
parseAST (Asin x) = "asin("++parseAST x++")"
parseAST (Acos x) = "acos("++parseAST x++")"
parseAST (Atan x) = "atan("++parseAST x++")"
parseAST (Asinh x) = "asinh("++parseAST x++")"
parseAST (Acosh x) = "acosh("++parseAST x++")"
parseAST (Atanh x) = "atanh("++parseAST x++")"
parseAST (Exp x) = "exp("++parseAST x++")"
parseAST (Log x) = "log("++parseAST x++")"

getVariables :: AST a -> [AST a]
getVariables (Variable x) = [Variable x]
getVariables (Const _) = []
getVariables (Add x y) = getVariables x ++ getVariables y
getVariables (Sub x y) = getVariables x ++ getVariables y
getVariables (Mul x y) = getVariables x ++ getVariables y
getVariables (Div x y) = getVariables x ++ getVariables y
getVariables (Abs x) = getVariables x
getVariables (Sgn x) = getVariables x
getVariables (Sqrt x) = getVariables x
getVariables (Sin x) = getVariables x
getVariables (Cos x) = getVariables x
getVariables (Sinh x) = getVariables x
getVariables (Cosh x) = getVariables x
getVariables (Asin x) = getVariables x
getVariables (Acos x) = getVariables x
getVariables (Atan x) = getVariables x
getVariables (Asinh x) = getVariables x
getVariables (Acosh x) = getVariables x
getVariables (Atanh x) = getVariables x
getVariables (Exp x) = getVariables x
getVariables (Log x) = getVariables x

testValue :: Floating a => a -> a -> a -> a
testValue a b c = (-b + sqrt(b*b-4*a*c))/(2*a)

main :: IO ()
main = do
  print $ testValue 1 0 (-1 :: Double)
  let ast = testValue 1 0 $ Variable "c" :: AST Double
  print . parseAST $ ast
  print . getVariables $ ast
