{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}

import Text.Peggy
import Text.Groom

type Holed = [Either String Int]
--data Term = A Holed | L [Term]

[peggy|

atom ::: String
	= '\"' ('\"' [\"] / [^\"])* '\"'
	/ '\'' ('\'' [\'] / [^\'])* '\''
	/ '▷'  ('◁'[◁]/[^◁])* '◁'
	/ '$'  ('$'[$]/[^$])* '$'

term :: Holed
	= atom { [$1] } / application { $1 }

application :: Holed
	= matrix term { $2 }

matrix :: [[Holed]]
	= '{' ((term, ","),";") '}'

|]

main = putStrLn . groom . parseString term "<stdin>" =<< getContents


{-
 a b c
 a1 b1 c1
 d1 e1 f1
 ▶ 
   'x' 'y' 'z'
	▶ aererwwre ı ı ı ı





     aererwwre x ı ı ı
	  aererwwre y ı ı ı
	  aererwwre z ı ı ı





     aererwwre x a b c
	  aererwwre y a b c
	  aererwwre z a b c
     aererwwre x a1 b1 c1
	  aererwwre y a1 b1 c1
	  aererwwre z a1 b1 c1
     aererwwre x d1 e1 f1
	  aererwwre y d1 e1 f1
	  aererwwre z d1 e1 f1



a
b
c
▶	ı123
djk
▶  ı456



a123
b123
c123
djk
	ı456


a123456
b123456
c123456
djk456


{	{ a , b , c } ,
	{ a1 , b1 , c1 } ,
	{ d1 , e1 , f1 } 	}
	{ { x , y , z } ı , { y } , { z } } 333ıııı








 'Holed'mogeneous
-}
