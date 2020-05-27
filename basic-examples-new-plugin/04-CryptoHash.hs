{-# OPTIONS_GHC -fplugin MAC.Plugin #-}
--{-# OPTIONS_GHC -fplugin MAC.Plugin -fplugin-opt=MAC.Plugin:defer #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Main where

import MAC.Labeled
import MAC.Prelude
import Data.ByteString
import qualified Data.ByteString.UTF8 as BSU
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Prelude


main :: Prelude.IO()

-- This works great, it's just public, no issues here
-- main = Prelude.print (bsToString (hash ( bsFromString (box "Language-Based Security" :: Public String))))

{-
 Without plugin, this fails with the error "Couldn't match
 type H with L.
 With the plugin and no defer, the error message is "Forbidden Flow
 from Secret (H) to Public (L)"."
 With the defer option enabled, we only get the warning and can run the code.
-}
main = Prelude.print (bsToString (hash ( bsFromString (box "Language-Based Security" :: Secret String))))


{-
 ALWAYS Recommended to bind boxed value to a specific security label,
 either Secret or Public, otherwise the behavior will be ambiguos and the
 type may be matched depending on the context.
 With and without plugin, this fails with error "Couldn't match
 type 'Max2 l L' with L [...] The type variable 'l' is ambiguous".
 This means that the compiler does not know whether the label given
 to the string is Secret (H) or Public (L).
-}
--main = Prelude.print (bsToString (hash ( bsFromString (box "Language-Based Security"))))





{-
 The code below shows how to 'wrap' any function with the
 'UnOp' type and 'unOp' funct given in the MAC.Prelude.
 (Same works for Binary and Ternary operations, ofc)
-}

-- Hash function from Crypto.Hash

hash :: UnOp l ByteString ByteString
hash = unOp SHA256.hash

-- String <-> ByteString

bsFromString :: UnOp l String ByteString
bsFromString = unOp BSU.fromString

bsToString :: UnOp l ByteString String
bsToString = unOp BSU.toString
