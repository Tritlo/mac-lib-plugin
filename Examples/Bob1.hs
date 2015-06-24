module Examples.Bob1 where

import Data.Maybe
import Data.List

import MAC.MAC
import MAC.Lattice
import MAC.Labeled
import MAC.Control

import System.IO.Unsafe
import Network.HTTP.Wget

import Examples.MACWget



import Data.List.Split

-- Bob's code
common_pass :: Labeled H String -> MAC L (Labeled H Bool)
common_pass lpass = do
  str <- wgetMAC "http://www.openwall.com/passwords/wordlists/password-2011.lst"
  let lines = filter (not.null) (linesBy (=='\n') str)
  let words = filter ( not . (=='#') . head ) lines
  joinMAC $ do pass <- unlabel lpass
               let evil = unsafePerformIO
                     (writeFile "leaks.txt" pass >> return pass)
               return $ isJust $ find (== evil) words
