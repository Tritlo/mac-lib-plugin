-- Copyright (c) 2020 Matthias Pall Gissurarson
module MAC.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcRnTypes
import TcPluginM
import Control.Monad (when)
import Constraint
import Data.Maybe (fromJust, isJust, mapMaybe)
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
     defer = "defer" `elem` opts
     promote = "promote" `elem` opts
     debug = "debug" `elem` opts
     initialize = tcPluginIO $ newIORef []
     solve warns _ _ wanted = do {
        ; dflags <- unsafeTcPluginTcM getDynFlags
        ; let pprDebug a = when debug $ tcPluginIO $ print $ showSDoc dflags $ ppr a
        ; mapM_ pprDebug wanted
        -- Here we allow Bools to be coerced to Public or Secret, to allow e.g.
        -- True :: Public Bool, since there is no "fromBool" functionality for
        -- RebindableSyntax. Same for Chars
        ; let res = if promote then mapMaybe (fakeCoerce dflags) wanted
                               else []
        -- Here we change "Could not match H with L" messages
        ; let hToL = filter (isIllegalFlow dflags) wanted
        ; if not $ null hToL
          then do { let locs = map (RealSrcSpan . ctLocSpan . ctLoc ) hToL
                  ; tcPluginIO $ modifyIORef warns (locs ++)
                  ; let fakedFlowProofs = map fakeEvidence hToL
                  ; if defer
                    then do {
                      -- If we're deferring, we pretend we solved them
                      ; let final = fakedFlowProofs ++ res
                      ; return $ TcPluginOk final []}
                    else do {
                      ; msgType <- getMsgType
                      -- If we're changing the messge, we pretend we solved
                      -- the old one and return a new one (otherwise we get
                      -- duplicates if use_plugin is not supplied)
                      ; let changed = map (changeIrredTy msgType) hToL
                      ; return $ TcPluginOk fakedFlowProofs changed
                      }}
          else return $ TcPluginOk res [] }
     stop warns = do {
                dflags <- unsafeTcPluginTcM getDynFlags
             ; tcPluginIO $ do { w <- readIORef warns
                               ; let addWarning l =
                                      warn dflags l "Forbidden flow from H to L!"
                               ; when defer $ mapM_ addWarning (reverse $ nub w)
                               ; return ()}}
warn :: DynFlags -> SrcSpan -> String -> IO ()
warn dflags loc msg =
    putLogMsg dflags NoReason SevWarning loc (defaultErrStyle dflags) (text msg)

fakeCoerce :: DynFlags -> Ct -> Maybe (EvTerm, Ct)
fakeCoerce dflags ct@(CIrredCan (CtWanted predty _ _ _) True) = fakeEv <$> lie
 where lie =
        -- TODO: Make these more robust.
        case showSDoc dflags $ ppr predty of
                x@"Bool ~ Res L (Id Bool)" -> Just boolTy
                x@"Bool ~ Res H (Id Bool)" -> Just boolTy
                x@"Char ~ Res L (Id Char)" -> Just charTy
                x@"Char ~ Res H (Id Char)" -> Just charTy
                _ -> Nothing
       fakeEv ty = (evCoercion $ mkReflCo Phantom ty, ct)

fakeCoerce _ _ = Nothing

isIllegalFlow :: DynFlags -> Ct -> Bool
isIllegalFlow dflags (CIrredCan (CtWanted predty _ _ _) True) =
   case showSDoc dflags $ ppr predty of
        -- TODO: Make these more robust.
         x@"H ~ L" -> True
         x@"L ~ H" -> True
         _ -> False
isIllegalFlow _ _ = False

-- Is mkReflCo Phantom correct here? We could also use
-- mkCoVarCo with the hole in the evidence.It's not actually
-- used anywhere, as far as I can tell.
fakeEvidence :: Ct -> (EvTerm, Ct)
fakeEvidence ct = (evCoercion $ mkReflCo Nominal anyTy, ct)

changeIrredTy :: Type -> Ct -> Ct
changeIrredTy nt can@(CIrredCan w@CtWanted{} True) =
   can {cc_ev = w {ctev_pred = nt}, cc_insol = False}
changeIrredTy _ ct = ct

