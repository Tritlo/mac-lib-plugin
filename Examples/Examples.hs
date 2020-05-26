{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE Safe #-}

-- | Example secure computations involving pure values and side-effects. To see how to run the computationts defined here, please check the module "SafeExample".
module Examples where

import MAC.MAC
import MAC.Lattice
import MAC.Labeled
import MAC.Control

import MAC.Exception

import Control.Exception
import Data.Typeable

low :: MAC L (Labeled L String)
low = label "public data"

high :: MAC H (Labeled H String)
high = label "secret data"

toLabeled :: (Less l l', Less l' l') => MAC l' a -> MAC l (Labeled l' a)
toLabeled = joinMAC

{-
   Computing a value from public and secret information
-}
example1 :: MAC L (Labeled H String)
example1 = do ll <- low                -- This sets the current label here to L
              l  <- unlabel ll
              joinMAC $ do hh <- high  -- This set the current label here to H
                           h  <- unlabel hh
                           return (l ++ h)


{-
   In the next example, if we write the line

       h <- unlabel hh

    without example2's type signature, Haskell does not know the
    current label of this computation. It only knows that it
    should be above (or equal) to H. There are two ways to fix
    this:

     - using MAC.fix (no need for type signature)

       fix (Proxy :: Proxy H)
       h <- unlabel hh

     - giving the type signature and use unlabel.
-}
example2 :: MAC L (Labeled H String)
example2 = do hh <- example1
              joinMAC $ do  h <- unlabel hh
                            return (h ++ h)

data Info = ThisException | ThatException deriving (Typeable, Show)
instance Exception Info where

{-
   The example shows that no computation is done after an exception is thrown.
-}

example3 :: MAC L (Labeled H String)
example3 = do example1
              throwMAC ThatException
              example2

{-
   Throwing an execption!
-}
example4 :: MAC L Int
example4 = return $ error "chau"

{-
   Throwing and catching a synchronous exception
-}
example4strict :: MAC L Int
example4strict = return $! 5 `div` 0

example5 :: MAC L Int
example5 = catchMAC
                example4strict
                (\(_e :: SomeException) -> return 42)

{-
   An exception can be returned, but not triggered, i.e., an asynchronous
   exception. This is not a proble since it is within the same monad family
   member.
-}
example6 :: MAC L Int
example6 = catchMAC
                example4
                (\(_e :: SomeException) -> return 42)
           >>= \_ -> return 10

{-
   The exception gets triggered in this case.
-}
example6_1 :: MAC L Int
example6_1 = catchMAC
                (error "chau")
                (\(_e :: SomeException) -> return 42)


{-
   The exception gets caught.
-}
example7 :: MAC L Int
example7 = catchMAC
                ( example3 >> return 1 )
                (\(_e :: SomeException) -> return 42)


{-
   The exception does not escape toLabeled.
-}
example8 :: MAC L Int
example8 = do example1
              toLabeled $ do _ <- high
                             throwMAC ThatException
              return 42

example9 :: MAC L Int
example9 = joinMAC (high >> error "chau") >> return 10
