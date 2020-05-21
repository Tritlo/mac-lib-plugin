-- Copyright (c) 2020 Matthias Pall Gissurarson
module MAC.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcRnTypes
import TcPluginM
import Control.Monad (when)
import Constraint
import Data.Maybe (fromJust, isJust)
import Data.IORef
import Data.List (nub)
import PrelNames
import TcEvidence (EvTerm, evCoercion)
import ErrUtils

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . flowPlugin, pluginRecompile = purePlugin }

getMsgType :: TcPluginM Type
getMsgType = do {
     typeErrorCon <- tcLookupTyCon errorMessageTypeErrorFamName
   ; textCon <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
   ; return $ mkTyConApp typeErrorCon [constraint, mkTyConApp textCon [msg]]}
 where msg :: Type
       msg = mkStrLitTy $ fsLit "Forbidden flow from H to L"
       constraint = mkTyConApp constraintKindTyCon []

flowPlugin :: [CommandLineOption] -> TcPlugin
flowPlugin opts = TcPlugin initialize solve stop
  where
     defer = case opts of
               ["defer"] -> True
               _ -> False
     initialize = tcPluginIO $ newIORef []
     solve warns _ _ wanted = do {
        ; dflags <- unsafeTcPluginTcM $ getDynFlags
        ; msgType <- getMsgType
        ; let hToL = filter (isIllegalFlow dflags) wanted
        ; if (not $ null $ hToL)
          then do { let locs = map (\ct -> (fromJust $ illegalFlowTy dflags ct,
                                            RealSrcSpan $ ctLocSpan $ ctLoc ct)) hToL
                  ; tcPluginIO $ modifyIORef warns (locs ++)
                  ; if defer
                    then do {
                      ; let res = map fakeEvidence hToL
                      ; return $ TcPluginOk res []}
                    else do {
                      ; let res = map (changeIrredTy msgType) hToL
                      ; return $ TcPluginContradiction res }}
          else return $ TcPluginOk [][] }
     stop warns = do {
                dflags <- unsafeTcPluginTcM $ getDynFlags
             ; tcPluginIO $ do { w <- readIORef warns
                               ; when (defer && (not $ null $ w)) $ do {
                               ; flip mapM_ (reverse $ nub w) $ \(t, l) ->
                                   warn dflags l $ "Illegal flow from H to L!"}
                               ; return () }}
warn :: DynFlags -> SrcSpan -> String -> IO ()
warn dflags loc msg =
    putLogMsg dflags NoReason SevWarning loc (defaultErrStyle dflags) (text msg)

illegalFlowTy :: DynFlags -> Ct -> Maybe String
illegalFlowTy dflags (CIrredCan (CtWanted predty _ _ _) True) =
   case (showSDoc dflags $ ppr predty) of
         x@"H ~ L" -> Just x
         x@"L ~ H" -> Just x
         _ -> Nothing
illegalFlowTy _ _ = Nothing

isIllegalFlow :: DynFlags -> Ct -> Bool
isIllegalFlow dflags ct = isJust $ illegalFlowTy dflags ct

-- Is mkReflCo Phantom correct here? We could also use
-- mkCoVarCo with the hole in the evidence.
fakeEvidence :: Ct -> (EvTerm, Ct)
fakeEvidence ct = (evCoercion $ mkReflCo Phantom anyTy, ct)

changeIrredTy :: Type -> Ct -> Ct
changeIrredTy nt can@(CIrredCan w@(CtWanted _ _ _ _) True) =
   can {cc_ev = w {ctev_pred = nt}, cc_insol = False}
changeIrredTy _ ct = ct

