{-# LANGUAGE Unsafe #-}
{-# LANGUAGE FlexibleContexts #-}
module Examples.Alice4 where

import MAC.Lattice
import MAC.Core
import MAC.Labeled
import Examples.MACWget

import Data.Bits

import qualified Examples.Bob4 as Bob

{-
   Safe use of references. The password mananger uses memoization
   of Bob's function common_pass.
-}

password :: IO String
password = do wgetMem <- runMAC $ Bob.memMAC wgetMAC
              try wgetMem

try wget= do putStr "Please, select your password:"
             pass <- getLine
             lbool <- runMAC $ (label pass :: MAC L (Labeled H String))
                     >>= Bob.common_pass wget
             let MkId b = unRes lbool
             if b then putStrLn "Your password is too common!" >> (try wget)
             else return pass
