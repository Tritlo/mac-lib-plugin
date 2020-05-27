{-# OPTIONS_GHC -fplugin MAC.Plugin #-}
--{-# OPTIONS_GHC -fplugin MAC.Plugin -fplugin-opt=MAC.Plugin:defer #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import MAC.Labeled
import MAC.Prelude
import qualified Prelude



publicZero  :: Public Int
publicOne   :: Public Int
secretZero  :: Secret Int
secretOne   :: Secret Int
publicZero = box 0
publicOne  = box 1
secretZero = box 0
secretOne  = box 1



secretFive :: Public Int
{-
 With no plugin, this will fail w/ error "Couldn't match Int with MAC.Core.Res L (Id Int)".
 With plugin enabled but no Defer flag, we get a better error: "Unlabeled Int used as a
 Public Int. Perhaps you intended to use 'box'?"
 With plugin and defer option, the above error will become a warning"
-}
secretFive  = 5





main :: Prelude.IO()
main = printPubFac (box 5)
--main = printSecFac (box 5)
--main = printSecFac1 (box 5)






-- Public Factorial
pubFac :: Public Int -> Public Int
pubFac n = if n == publicZero then publicOne else (n * pubFac (n - publicOne))

printPubFac :: Public Int -> Prelude.IO()
printPubFac = Prelude.print . pubFac






-- Secret Factorial
secFac :: Secret Int -> Secret Int
secFac n = if n == secretZero then secretOne else (n * secFac (n - secretOne))

{-
 When the plugin is disabled, this won't compile and
 will instead trigger the error "Couldn't match type H with L".
 When the plugin is enabled with no defer option, we get a "Forbidden Flow"
 error message.
 When the plugin is enabled and the defer option set, it will just trigger
 the warning "Forbidden flow from Secret (H) to Public (L)" and run.
-}
printSecFac :: Secret Int -> Prelude.IO()
printSecFac = Prelude.print . secFac






-- `Mixed` Factorial

{-
 When the plugin is disabled, this won't compile and
 will instead trigger the error "Couldn't match type L with H".
 When the plugin is enabled with defer flag, it will just trigger
 the warning "Forbidden flow from Secret (H) to Public (L)" and run.
 Note that the forbidden flow now happens because of the
 types declared in the function signature that attempt to convert
 something secret to public.
-}
secFac1 :: Secret Int -> Public Int
secFac1 n = if n == secretZero then secretOne else (n * secFac1 (n - secretOne))

printSecFac1 :: Secret Int -> Prelude.IO()
printSecFac1 = Prelude.print . secFac1
