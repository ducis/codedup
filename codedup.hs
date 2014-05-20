{-
runghc codedup.hs < t0.in
-}
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts,TupleSections,LambdaCase #-}

import Text.Peggy
import Text.Groom
import Data.Either
import Control.Applicative
import Data.List
import qualified Data.Vector as V
import Data.Maybe
type Shard = Either String (Int,Int)
type Holed = [Shard]
--data Term = A Holed | L [Term]

substitute::(String, [[Holed]]) -> Holed -> Holed
substitute (sep, mat) h = intercalate [Left sep] [sub (V.fromList row) (cycle row) h | row <- mat]
	where
	sub v = f where
		f = curry $ \case 
			(xs, Left y:ys) -> Left y : f xs ys
			--([], Right y:ys) -> Right y : f [] ys
			(x:xs, Right (0,-1):ys) -> x ++ f xs ys
			(xs, Right (0,i):ys) | i>=0 -> (v V.! i) ++ f xs ys
			(xs, Right (y,i):ys) | y>0 -> Right (y-1,i) : f xs ys
			(_, []) -> []

-- TODO : concat

[peggy|

--root0 :: [Holed] --String
--	= term+ -- { concat $ lefts $ concat $1 }

root :: String
	= term+ { unlines $ map (concat.lefts) $1 }

mixture :: String
	= ("▷▷" root "◁◁" / . {[$1]})* {concat $1}

-- atom ::: String
	-- = '▷'  ('◁'[◁]/[^◁])* '◁'
	-- / '$'  ('$'[$]/[^$])* '$'
	-- / [a-z0-9ı]+
	
hole :: Shard
	= 'ı' [0-9]* (':' [0-9]+)? {Right (read ('0':$1), maybe (-1) read $2)} 

atom ::: Holed
	= '▷' (
			hole/
			('◁'[◁]/[^◁ı])+{Left $1}
		)* '◁'
	/'$' (
			hole/
			('$'[$]/[^$ı])+{Left $1}
		)* '$'
	/	(
			hole/
			[a-z0-9]+{Left $1} 
		)+

term :: Holed
	= "(" terms ")" / application { $1 } / atom 

terms :: Holed
	= term+ {concat $1}

application :: Holed
	= ( 
	"{" matrix "}" { ("\n",$1) } / 
	"[" matrix "]" { ("",$1) }  --TODO: additional delimiter
	) term { substitute $1 $2 }

matrix :: [[Holed]]
	= ((term, ","),";")

|]

main = either print putStrLn. parseString mixture "<stdin>" =<< getContents
--main = either print (putStrLn.groom). parseString root "<stdin>" =<< getContents
