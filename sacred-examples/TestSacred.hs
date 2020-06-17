 {-# OPTIONS_GHC  -fplugin=SACRED.Plugin
                  -fplugin-opt=SACRED.Plugin:debug
                  -dcore-lint
                 #-}
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

import MAC.Labeled
import MAC.Prelude
import qualified Prelude
import SACRED.Configure
import GHC.TypeLits(TypeError, ErrorMessage(..))
import MAC.Core (Res(..))

type instance Default Label = L

type instance Relate Label (n :: Label) (m :: Label) =
    TypeError (Text "Forbidden flow from Secret (H) to Public (L)!")

type family LabelPpr (k :: Label) where
    LabelPpr L = Text "Public"
    LabelPpr H = Text "Secret"
    LabelPpr l = Text "Labeled " :<>: ShowType l

type instance Ignore (Less n m) =
    TypeError (Text "Forbidden flow from Secret (H) to Public (L)!")

type instance Promote a (Labeled l b) =
     TypeError (Text "Unlabeled ‘"
                :<>: ShowType a :<>: Text "‘ used as a ‘" 
                :<>: LabelPpr l :<>: Text " " 
                :<>: ShowType b :<>: Text "‘."
                :$$: Text "Perhaps you intended to use ‘box‘?")


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

promotedList :: Public [[Integer]]
promotedList = [[0 :: Integer]]

something :: Labeled l Bool
something = True

main :: Prelude.IO ()
main = do runMAC test Prelude.>>= Prelude.print
          Prelude.print ("This should be private!" :: Secret String)
          Prelude.print (True :: Public Bool)
          Prelude.print promotedList
          Prelude.print something
