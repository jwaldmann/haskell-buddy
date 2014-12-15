{-# language GeneralizedNewtypeDeriving #-}

module Buddy 
       
( BDD, Transaction, Store
, init, execute, done, run
, unit, constant
, (&&), (||), not, xor
, imp, biimp, nand, nor, restrict
, satisfiable, model, fold
, satcount, satcountln
, and, or
, plain_and, plain_or
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
import Control.Applicative 

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Time

newtype BDD v = BDD { unBDD :: Bdd } deriving (Eq, Ord)

data Store v = Store { fore :: Map v CInt
                     , back :: Map CInt v }  

newtype Transaction v a = 
  Transaction { unTransaction 
                :: StateT (Store v) IO a } 
  deriving (Functor, Monad, Applicative)

satcount :: BDD v -> Transaction v CDouble
satcount (BDD x) = Transaction $ lift $ bdd_satcount x

satcountln :: BDD v -> Transaction v CDouble
satcountln (BDD x) = Transaction $ lift $ bdd_satcountln x

satisfiable :: BDD v -> Transaction v Bool
satisfiable (BDD x) = Transaction $ do
    f <- lift bdd_false
    return $ f /= x

terminal :: BDD v -> Transaction v Bool
terminal (BDD x) = Transaction $ do
    f <- lift bdd_false
    t <- lift bdd_true
    return $ (x == f) Prelude.|| (x == t)

model :: Ord v => BDD v -> Transaction v (M.Map v Bool)
model b = do
    let walk b = do
          t <- terminal b
          if t then return M.empty
               else do v <- var b
                       l <- low b ; ls <- satisfiable l
                       h <- high b ; hs <- satisfiable h
                       if ls then M.insert v False <$> walk l
                             else M.insert v True  <$> walk h
    bf <- fullsatone b
    walk bf

fullsatone :: BDD v -> Transaction v (BDD v)
fullsatone (BDD x) = Transaction $ lift $ BDD <$> bdd_fullsatone x

fold :: (Bool -> a)
     -> (v -> a -> a -> a)
     -> BDD v    
     -> Transaction v a
fold leaf branch b = do
    let walk c b = do
          t <- terminal b
          if t then do
            s <- satisfiable b
            return ( leaf s, c)
          else case M.lookup b c of
              Just res -> return (res, c)
              Nothing -> do
                 (l, c1) <- low b >>= walk c
                 (r, c2) <- high b >>= walk c1
                 v <- var b
                 let res = branch v l r
                     c3 = M.insert b res c2
                 return (res, c3)
    fst <$> walk M.empty b          

var :: BDD v -> Transaction v v
var (BDD x) = Transaction $ do
   s <- get
   v <- lift $ bdd_var x
   return $ back s M.! v

low :: BDD v -> Transaction v (BDD v)
low (BDD x) = Transaction $ lift $ BDD <$> bdd_low x

high :: BDD v -> Transaction v (BDD v)
high (BDD x) = Transaction $ lift $ BDD <$> bdd_high x
    
unit v pos = Transaction $ do
    s <- get
    case M.lookup v $ fore s of
        Nothing -> error "Buddy.unit: undeclared variable"
        Just i -> lift $ fmap BDD 
                  $ case pos of 
                    True -> bdd_ithvar i
                    False -> bdd_nithvar i

constant pos = Transaction $ lift $ fmap BDD $ case pos of
    True -> bdd_true ; False -> bdd_false

helper = Transaction . lift . fmap BDD . referenced

not (BDD x) = helper $ bdd_not x
BDD x && BDD y = helper $ bdd_and x y
BDD x || BDD y = helper $ bdd_or x y
nand (BDD x) (BDD y) = helper $ bdd_apply x y bddop_nand
nor (BDD x) (BDD y) = helper $ bdd_apply x y bddop_nor
xor (BDD x) (BDD y) = helper $ bdd_xor x y 
imp (BDD x) (BDD y) = helper $ bdd_imp x y
biimp (BDD x) (BDD y) = helper $ bdd_biimp x y 

restrict (BDD x) (BDD y) = helper $ bdd_restrict x y

referenced a = do b <- a ; bdd_addref b

init :: Ord v => [v] -> IO ( Store v )
init vars = flip execStateT undefined $ do
    let f = M.fromList $ zip vars [0..]
        b = M.fromList $ zip [0..] vars
    put $ Store { fore = f, back = b }
    let s = fromIntegral $ M.size f
    lift $ bdd_init (10 ^ 8) (10 ^ 6) -- ??
    -- lift $ bdd_init (2 * 10^8) (2 * 10^6)
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
  
and xs = binary_fold_with_dispose True (&&) xs
or  xs = binary_fold_with_dispose False (||) xs

plain_and [] = constant True
plain_and (x:xs) = foldM (&&) x xs

plain_or [] = constant False
plain_or (x:xs) = foldM (||) x xs

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

binary_fold_with_dispose nil cons = 
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
