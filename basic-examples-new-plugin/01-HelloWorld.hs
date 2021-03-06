--{-# OPTIONS_GHC -fplugin MAC.Plugin #-}
{-# OPTIONS_GHC -fplugin MAC.Plugin -fplugin-opt=MAC.Plugin:defer #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import MAC.Labeled
import MAC.Prelude
import qualified Prelude




main :: Prelude.IO()
main = helloWorld1




-- Hello, Public World
helloWorld :: Prelude.IO ()
helloWorld = Prelude.print (box "Hello, Public World" :: Public String)


{-
 When the plugin is disabled, this won't compile and
 will instead trigger the error "Couldn't match type H with L".
 When the plugin is enable with NO Defer flag, it will not compile and
 instead give the more meaningful error "Forbidden Flow from Secret (H) to
 Public (L)."
 When the plugin is enabled with Defer flag, it will just trigger
 the warning "Forbidden flow from H to L" and run.
-}
helloWorld1 :: Prelude.IO ()
helloWorld1 = Prelude.print (box "Hello, Secret World" :: Secret String)

