module MAC.PluginTh where
import Language.Haskell.TH
import Data.Maybe


lookupStableName :: String -> ExpQ
lookupStableName n = do mid <- fromJust <$> lookupTypeName n
                        let (p,m,b) = (fromJust (namePackage mid),
                                       fromJust (nameModule mid),
                                       nameBase mid)
                        stringE $ '$':(p++"$"++m++"$"++b)
