{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy

type Holed = [Either String Int]

[peggy|
top :: Double = expr !.

arglist :: [Holed]
  = expr "+" fact { $1 + $2 }
  / expr "-" fact { $1 - $2 }
  / fact

argmatrix :: [[Holed]]

fact :: Double
  = fact "*" term { $1 * $2 }
  / fact "/" term { $1 / $2 }
  / term

term :: Double
  = "(" expr ")"
  / number

number ::: Double
  = [1-9] [0-9]* { read ($1 : $2) }
|]

main = print . parseString top "<stdin>" =<< getContents


{-
 a b c
 a1 b1 c1
 d1 e1 f1
 :dig
 ▶ 
   'x' 'y' 'z'
	▶ aererwwre ı ı ı ı




     aererwwre x a b c
	  aererwwre y a b c
	  aererwwre z a b c
     aererwwre x a1 b1 c1
	  aererwwre y a1 b1 c1
	  aererwwre z a1 b1 c1
     aererwwre x d1 e1 f1
	  aererwwre y d1 e1 f1
	  aererwwre z d1 e1 f1





 'Holed'mogeneous
-}
