--{-# OPTIONS_GHC -fplugin MAC.Plugin #-}
--{-# OPTIONS_GHC -fplugin MAC.Plugin -fplugin-opt=MAC.Plugin:defer #-}
{-# LANGUAGE RebindableSyntax #-}


module Main where

import MAC.Labeled
import MAC.Prelude
import qualified Prelude as Prelude




publicProbe :: Public Char
publicProbe = box 'c'
secretProbe :: Secret Char
secretProbe = box 'c'




main:: Prelude.IO()

--main = Prelude.print (comparePublicInt (box 1))


{-
 Here, even though a specific label is not give, Secret or Public,
 the compiler does not complain. This is because the signature of the
 'comparePublic' function is clear. However, I thinks it is still
 recommended to apply a clear security label.
-}
main = Prelude.print (comparePublic (box 'a'))



{-
 As in previous examples, this won't compile without plugin and give
 a warning when the plugin is enabled.
-}
-- main = Prelude.print (compareSecretWprobe (box 'a' :: Secret Char))



comparePublicInt :: Public Int -> Public Bool
comparePublicInt i = i < ( box 3 :: Public Int )

comparePublic :: Public Char -> Public Bool
comparePublic c = c < ( box 'c' :: Public Char )


-- Here, comparing with a Public char is totally fine, since the returned bool is Secret
compareSecretWprobe :: Secret Char -> Secret Bool
compareSecretWprobe c = c < publicProbe

{-
 But this is obviously an information leak.
 Without plugin, it fails with the error "Couldn't match L with H".
 This happens because, since the returned bool is Public, the result of the
 comparison is expected to be Public (L) which is not possible given that
 one of the two values is Secret.
 With plugin, a warning is given.
-}
compareSecretWprobe1 :: Secret Char -> Public Bool
compareSecretWprobe1 c = c < publicProbe

comparePublicWprobe :: Public Char -> Public Bool
comparePublicWprobe c = c < publicProbe
