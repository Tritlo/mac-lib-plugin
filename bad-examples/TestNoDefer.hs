{-# OPTIONS_GHC -fplugin MAC.Plugin #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where


import GHC.TypeLits()
import MAC.Core
import MAC.Labeled
import MAC.Prelude
import qualified Prelude

post :: Public String -> Secret String -> Public String -> Public String
post usr pwd msg = usr <> (" says: " :: Public String) <> sanitize pwd msg

sanitize :: Secret String -> Public String -> Public String
sanitize pwd msg = unwords (map hide_pwd (words msg))
  where
    hidden_pwd = concat (replicate (length pwd) ("*" :: Public String))
    hide_pwd word = if word == pwd then hidden_pwd else word

test :: MAC l (Public String)
test = do
  let user = "AzureDiamond"           :: Public String
  let pwd  = "hunter2"                :: Secret String
  let msg  = "my password is hunter2" :: Public String
  return (post user pwd msg)

main :: Prelude.IO ()
main = do  runMAC test Prelude.>>= Prelude.print
           Prelude.print ("This should be private!" :: Secret String)
