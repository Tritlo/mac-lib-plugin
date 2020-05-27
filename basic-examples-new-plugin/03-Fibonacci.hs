{-# OPTIONS_GHC -fplugin MAC.Plugin -fplugin-opt=MAC.Plugin:defer #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import MAC.Labeled
import MAC.Prelude
import qualified Prelude




publicZero  :: Public Int
publicOne   :: Public Int
publicTwo   :: Public Int
secretZero  :: Secret Int
secretOne   :: Secret Int
secretTwo   :: Secret Int
publicZero = box 0
publicOne  = box 1
publicTwo  = box 2
secretZero = box 0
secretOne  = box 1
secretTwo  = box 2




main :: Prelude.IO()
main = printPubFib (box 10)
--main = printSecFib (box 10)
--main = printSecFib1 (box 10)





-- Public Fibonacci
pubFib :: Public Int -> Public Int
pubFib n = if n == publicZero then publicZero else
             if n == publicOne then publicOne
                    else pubFib ( n - publicOne ) + pubFib (n - publicTwo)


printPubFib :: Public Int -> Prelude.IO()
printPubFib = Prelude.print . pubFib






-- Secret Fibonacci
secFib :: Secret Int -> Secret Int
secFib n = if n == secretZero then secretZero else
             if n == secretOne then secretOne
                    else secFib ( n - secretOne) + secFib (n - secretTwo)

{-
 When the plugin is disabled, this won't compile and
 will instead trigger the error "Couldn't match type H with L".
 When the plugin is enabled, it will just trigger
 the warning "Forbidden flow from Secret (H) to Public (L)" and run.
-}
printSecFib :: Secret Int -> Prelude.IO()
printSecFib = Prelude.print . secFib






-- `Mixed` Fibonacci

{-
 When the plugin is disabled, this won't compile and
 will instead trigger the error "Found Forbidden Flow from H to L".
 When the plugin is enabled, it will just trigger
 the warning "Forbidden flow from Secret (H) to Public (L)" and run.
 Note that the forbidden flow now happens because not because of the print
 but simply because we are attempting to convert from H to L in a pure function.
-}
secFib1 :: Secret Int -> Public Int
secFib1 n = if n == secretZero then publicZero else
             if n == secretOne then publicOne
                    else secFib1 (n - secretOne) + secFib1 (n - secretTwo)


printSecFib1 :: Secret Int -> Prelude.IO()
printSecFib1 = Prelude.print . secFib1