{-# LANGUAGE Trustworthy #-}
module Examples.MACWget where

import MAC.Lattice
import MAC.Core (MAC, ioTCB)

import Network.HTTP.Wget

{-
  For simplicity, when wgetMAC gets called with http://bob.evil as a domain, it
  will write the request to a file
-}
wgetMAC :: String -> MAC L String
wgetMAC s | take (length domain) s == domain =
              ioTCB (appendFile "leaks.txt" (s ++ "\n")) >> return "launch"
          | otherwise = ioTCB (wget s [] [])
          where domain = "http://bob.evil"
