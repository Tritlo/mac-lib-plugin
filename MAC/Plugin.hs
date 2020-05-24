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

getMsgType :: String -> TcPluginM Type
getMsgType str = do {
     typeErrorCon <- tcLookupTyCon errorMessageTypeErrorFamName
   ; textCon <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
   ; return $ mkTyConApp typeErrorCon [constraint, mkTyConApp textCon [msg]]}
 where msg :: Type
       msg = mkStrLitTy $ fsLit str
       constraint = mkTyConApp constraintKindTyCon []


data Log = ForbiddenFlow SrcSpan
         | Promotion SrcSpan deriving (Eq, Show)

flowPlugin :: [CommandLineOption] -> TcPlugin
flowPlugin opts = TcPlugin initialize solve stop
  where
     defer = "defer" `elem` opts
     debug = "debug" `elem` opts
     initialize = tcPluginIO $ newIORef []
     solve warns _ _ wanted = do {
        ; dflags <- unsafeTcPluginTcM getDynFlags
        ; let pprDebug a = when debug $ tcPluginIO $ print $ showSDoc dflags $ ppr a
        ; mapM_ pprDebug wanted
        -- Here we allow Bools to be coerced to Public or Secret, to allow e.g.
        -- True :: Public Bool, since there is no "fromBool" functionality for
        -- RebindableSyntax. Same for Chars
        ; let proms = filter (isJust . fakeProm dflags) wanted
        ; let promLocs = map (Promotion . RealSrcSpan . ctLocSpan . ctLoc) proms
        -- Here we change "Could not match H with L" messages
        ; let hToL = filter (isIllegalFlow dflags) wanted
        ; do { let locs = map (ForbiddenFlow . RealSrcSpan . ctLocSpan . ctLoc ) hToL
             ; let fakedFlowProofs = map fakeEvidence hToL
             ; let fakedPromote = map (fromJust . fakeProm dflags) proms
             ; if defer
               then do {
                 ; tcPluginIO $ modifyIORef warns (locs ++)
                 ; tcPluginIO $ modifyIORef warns (promLocs ++)
                 -- If we're deferring, we pretend we solved them
                 ; let final = fakedFlowProofs ++ fakedPromote
                 ; return $ TcPluginOk final []}
               else do {
                 ; flowMsgTy <- getMsgType "Forbidden flow from H to L"
                 ; promMsgTy <- getMsgType "Unlabeled value used as a labeled value. Perhaps you meant to use 'promote' or '^#'?"
                 -- If we're changing the messge, we pretend we solved
                 -- the old one and return a new one (otherwise we get
                 -- duplicates if use_plugin is not supplied)
                 ; let changedFlow = map (changeIrredTy flowMsgTy) hToL
                 ; let changedPromote = map (changeIrredTy promMsgTy) proms
                 ; let changed = changedFlow ++ changedPromote
                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; return $ TcPluginOk faked changed }}}
     stop warns = do {
                dflags <- unsafeTcPluginTcM getDynFlags
             ; tcPluginIO $ do { w <- readIORef warns
                               ; let addWarning entry =
                                        case entry of
                                            ForbiddenFlow l -> warn dflags l "forbidden flow from H to L!"
                                            Promotion l -> warn dflags l "unlabeled value used as a labeled value!"
                               ; mapM_ addWarning (reverse $ nub w) }}
warn :: DynFlags -> SrcSpan -> String -> IO ()
warn dflags loc msg =
    putLogMsg dflags NoReason SevWarning loc (defaultErrStyle dflags) (text msg)

fakeProm :: DynFlags -> Ct -> Maybe (EvTerm, Ct)
fakeProm dflags ct@(CIrredCan (CtWanted predty _ _ _) True) = fakeEv <$> lie
 where lie =
        -- TODO: Make these more robust.
        case showSDoc dflags $ ppr predty of
                x@"Bool ~ Res L (Id Bool)" -> Just boolTy
                x@"Bool ~ Res H (Id Bool)" -> Just boolTy
                x@"Char ~ Res L (Id Char)" -> Just charTy
                x@"Char ~ Res H (Id Char)" -> Just charTy
                _ -> Nothing
       fakeEv ty = (evCoercion $ mkReflCo Phantom ty, ct)

fakeProm _ _ = Nothing

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

