{-
the N Queens problem. The propositional variables
correspond to the positions on the board.
It shows how to construct an OBDD
and how to check some of its properties.

BUILD:  ghc --make Queens
RUN  :  ./Queens 7
-}

import Buddy (BDD)
import qualified Buddy as B

import Control.Monad ( guard, sequence, forM )
import Control.Applicative

import System.Environment ( getArgs )
import qualified Data.Set 
import qualified Data.Map as M

type Position = (Int,Int)

positions :: Int -> [ Position ]
positions n = do 
    a <- [ 1 .. n ]
    b <- [ 1 .. n ]
    return (a,b)

threatens :: Position -> Position -> Bool
threatens (a,b) (c,d) = 
       a == c     -- same column
    || b == d     -- same row
    || a+b == c+d -- same diagonal
    || a-b == c-d -- same antidiagonal

board n = 
    B.and =<< sequence ( each_column_is_occupied n ++ no_threats n ) 

for = flip map

each_column_is_occupied n = for [1..n] $ \ a ->
    B.or =<< forM [ 1 .. n ] ( \ b -> B.unit (a,b) True )
                            
each_row_is_occupied n = forM [ 1 .. n ] $ \ a -> 
    B.or =<< forM [ 1 .. n ] ( \ b ->  B.unit (b,a) True ) 

no_threats n = for (positions n) ( \ p ->
    B.and =<< forM (positions n) ( \ q ->
        if p /= q && threatens p q
        then do a <- B.unit p True ; b <- B.unit q False
                B.imp a b
        else B.constant True
             ) )

main = do
    args <- getArgs
    case args of
      [] -> mainf 8
      [arg] -> mainf $ read arg

mainf n = do 
    (c,m) <- B.run (positions n) $ do
         b <- board n
         c <- B.satcount b
         m <- B.model b
         return (c, m)
    print c
    print $ M.filter id m
    B.printstat
    



