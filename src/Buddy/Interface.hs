{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -O0 #-}

module Buddy.Interface where

  
import Foreign
import Foreign.C
import Foreign.Ptr


bddop_and = 0 :: CInt
bddop_xor = 1 :: CInt
bddop_or = 2 :: CInt
bddop_nand = 3 :: CInt
bddop_nor  =     4 :: CInt
bddop_imp   =    5 :: CInt
bddop_biimp  =   6 :: CInt
bddop_diff    =  7 :: CInt
bddop_less     = 8 :: CInt
bddop_invimp   = 9 :: CInt


--    /* Should *not* be used in bdd_apply calls !!! */
bddop_not     = 10 :: CInt
bddop_simplify = 11 :: CInt

type Bdd = CInt

foreign import ccall unsafe "bdd.h bdd_init" bdd_init :: CInt -> CInt -> IO ()
foreign import ccall unsafe "bdd.h bdd_setvarnum" bdd_setvarnum :: CInt -> IO ()
foreign import ccall unsafe "bdd.h bdd_done" bdd_done :: IO ()
foreign import ccall unsafe "bdd.h bdd_true" bdd_true :: IO Bdd
foreign import ccall unsafe "bdd.h bdd_false" bdd_false :: IO Bdd
foreign import ccall unsafe "bdd.h bdd_ithvar" bdd_ithvar :: CInt -> IO Bdd
foreign import ccall unsafe "bdd.h bdd_nithvar" bdd_nithvar :: CInt -> IO Bdd
foreign import ccall unsafe "bdd.h bdd_addref" bdd_addref :: Bdd -> IO Bdd
foreign import ccall unsafe "bdd.h bdd_delref" bdd_delref :: Bdd -> IO Bdd

foreign import ccall unsafe "bdd.h bdd_not" bdd_not :: Bdd -> IO Bdd
foreign import ccall unsafe "bdd.h bdd_apply" bdd_apply :: Bdd -> Bdd -> CInt -> IO Bdd
foreign import ccall unsafe "bdd.h bdd_restrict" bdd_restrict :: Bdd -> Bdd -> IO Bdd

foreign import ccall unsafe "bdd.h bdd_satcount" bdd_satcount :: Bdd -> IO CDouble

foreign import ccall unsafe "bdd.h bdd_restrict_probability" bdd_restrict_probability :: Ptr CInt -> CDouble -> Bdd -> IO CDouble

foreign import ccall unsafe "bdd.h bdd_satcountln" bdd_satcountln :: Bdd -> IO CDouble
foreign import ccall unsafe "bdd.h bdd_nodecount" bdd_nodecount :: Bdd -> IO CInt

foreign import ccall unsafe "bdd.h bdd_printstat" bdd_printstat :: IO ()
