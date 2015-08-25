module Alice0 where

import qualified Bob0 as Bob
type Pass = String

{-

 Alice wants to write a piece of code which ask users for passwords. To increse
 security, Alice would like to verify the pass against a list of the most common
 used ones.

 With that in mind, Alice found that another Haskell programmer, Bob, wrote the
 function common_pass :: String -> IO Bool, which validates the password against
 a dictionary of the most common used passwords.

 If Alice decides to use Bob's code, there are no guarantees that Bob is not
 leaking Alice's users passwords.

-}


password :: IO Pass
password = do
  putStr "Please, select your password:"
  pass <- getLine
  b <- Bob.common_pass pass
  if b then do putStrLn "Your password is too common!"
               password
  else return pass
