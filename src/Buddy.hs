{-# language GeneralizedNewtypeDeriving #-}

module Buddy 
       
( BDD, Transaction, Store
, init, execute, done, run
, unit, constant
, (&&), (||), not
, implies, nand, nor, restrict
, satcount, satcountln
, restrict_probability
, and, or
, and_plain, or_plain
, monadic
, dispose
, printstat, message
, logged
)
       
where

import Foreign.C
import Foreign.Marshal.Array ( withArray )
import Buddy.Interface
  
import Prelude hiding ( and, or, not, (&&), (||), init )
import qualified Prelude
import Control.Monad ( foldM, sequence, void )  
import Control.Monad.State.Strict

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Time

newtype BDD v = BDD { unBDD :: Bdd }

newtype Store v = Store ( Map v CInt )

newtype Transaction v a = 
  Transaction { unTransaction 
                :: StateT (Store v) IO a } 
  deriving Monad

restrict_probability
  :: Ord v => (M.Map v Bool) 
     -> Double
     -> BDD v 
     -> Transaction v CDouble
restrict_probability p prob (BDD x) = Transaction $ do
    Store m <- get
    let   mm = M.fromList 
             $ do (v,i) <- M.toList m ; return (i,v)
          a = do (i,v) <- M.toAscList mm
                 return $ case M.lookup (mm M.! i) p of
                      Nothing -> -1
                      Just False -> 0
                      Just True -> 1
    lift $ withArray a $ \ a -> 
      bdd_restrict_probability a (realToFrac prob) x 

satcount (BDD x) = Transaction $ lift $ bdd_satcount x
satcountln (BDD x) = Transaction $ lift $ bdd_satcountln x

unit v pos = Transaction $ do
    Store m <- get
    case M.lookup v m of
        Nothing -> error "Buddy.unit: undeclared variable"
        Just i -> lift $ fmap BDD 
                  $ case pos of 
                    True -> bdd_ithvar i
                    False -> bdd_nithvar i

constant pos = Transaction $ lift $ fmap BDD $ case pos of
    True -> bdd_true ; False -> bdd_false

helper = Transaction . lift . fmap BDD . referenced

not (BDD x) = helper $ bdd_not x
BDD x && BDD y = helper $ bdd_apply x y bddop_and
BDD x || BDD y = helper $ bdd_apply x y bddop_or
nand (BDD x) (BDD y) = helper $ bdd_apply x y bddop_nand
nor (BDD x) (BDD y) = helper $ bdd_apply x y bddop_nor
implies (BDD x) (BDD y) = helper $ bdd_apply x y bddop_imp

restrict (BDD x) (BDD y) = helper $ bdd_restrict x y

referenced a = do b <- a ; bdd_addref b

init :: Ord v => [v] -> IO ( Store v )
init vars = flip execStateT undefined $ do
    let m = M.fromList $ zip vars [0..] 
    put $ Store m 
    let s = fromIntegral $ M.size m
    -- lift $ bdd_init (s * 256) (10^4)
    lift $ bdd_init (2 * 10^8) (2 * 10^6)
    lift $ bdd_setvarnum s

done s = flip execStateT s $ do
    lift bdd_done

run :: Ord v => [v] -> Transaction v a -> IO a
run vars action = do
    s <- init vars
    out <- execute s action
    done s
    return out

execute s action = flip evalStateT s $ do
    unTransaction action
  
and_plain [] = constant  True ; and_plain (x:xs) = foldM (&&) x xs
or_plain  [] = constant False ; or_plain  (x:xs) = foldM (||) x xs

and xs = fold_with_dispose True (&&) xs
or  xs = fold_with_dispose False (||) xs

-- and xs = binary_fold True (&&) xs
-- or  xs = binary_fold False (||) xs

binary_fold nil cons =
   let run [] = constant nil
       run [x] = return x
       run (x:y:zs) = do
           xy <- cons x y
           run (zs ++ [xy])
   in  run         

dispose (BDD x) =  
  Transaction $ lift $ void $ bdd_delref x

fold_with_dispose nil cons = 
   let run first [] = constant nil
       run first [x] = return x
       run first (x:y:ys) = do
           xy <- cons x y
           when (Prelude.not first) $ dispose x
           run False (xy : ys)
   in  run True


monadic f actions = do
   xs <- sequence actions
   f xs
   
printstat = bdd_printstat

message s = Transaction $ lift $ putStrLn s

logged msg t = do
    start <- Transaction $ lift $ getCurrentTime
    x <- t
    end <- Transaction $ lift $ getCurrentTime
    message $ unwords [ msg, show $ diffUTCTime end start ]
    return x
