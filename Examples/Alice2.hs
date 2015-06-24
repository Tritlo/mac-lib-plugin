{-# LANGUAGE Trustworthy #-}
module Examples.Alice2 where

import MAC.Lattice
import MAC.Core
import MAC.Labeled

import qualified Examples.Bob2 as Bob

{-
  In this example, Bob's code exploits the termination covert channel to
  reveal numeric passwords with length between 4 and 8 characters.
-}

password :: IO String
password = do putStr "Please, select your password:"
              pass <- getLine
              lbool <- runMAC $ (label pass :: MAC L (Labeled H String))
                      >>= Bob.common_pass
              let MkId b = unRes lbool
              if b
                 then putStrLn "Your password is too common!" >> password
                 else return pass
