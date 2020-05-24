-- Copyright (c) 2020 Matthias Pall Gissurarson
module MAC.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcRnTypes
import TcPluginM
import Control.Monad (when)
import Constraint
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.IORef
import Data.List (nub, sort)
import Data.Function (on)
import PrelNames
import TcEvidence (EvTerm, evCoercion)
import ErrUtils

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . flowPlugin,
                         pluginRecompile = purePlugin }

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

logSrc :: Log -> SrcSpan
logSrc (ForbiddenFlow l) = l
logSrc (Promotion l) = l

instance Ord Log where
  compare = compare `on` logSrc

addWarning :: DynFlags -> Log -> IO()
addWarning dflags log = warn msg
  where
    warn =
      putLogMsg dflags NoReason SevWarning (logSrc log) (defaultErrStyle dflags)
    msg = case log of
            ForbiddenFlow _ -> text "forbidden flow from H to L!"
            Promotion _ -> text "unlabeled value used as a labeled value!"

flowPlugin :: [CommandLineOption] -> TcPlugin
flowPlugin opts = TcPlugin initialize solve stop
  where
     defer = "defer" `elem` opts
     debug = "debug" `elem` opts
     initialize = tcPluginIO $ newIORef []
     solve warns given derived wanted = do {
        ; dflags <- unsafeTcPluginTcM getDynFlags
        ; let pprDebug str a =
                when debug $
                  tcPluginIO $ putStrLn (str ++ " " ++ showSDoc dflags (ppr a))
        ; pprDebug "Solving" empty
        ; mapM_ (pprDebug "Given:") given
        ; mapM_ (pprDebug "Derived:") derived
        ; mapM_ (pprDebug "Wanted:") wanted
        -- Here we allow Bools to be coerced to Public or Secret, to allow e.g.
        -- True :: Public Bool, since there is no "fromBool" functionality for
        -- RebindableSyntax. Same for Chars
        ; let proms = filter (isJust . fakeProm dflags) wanted
        ; let promLocs = map (Promotion . RealSrcSpan . ctLocSpan . ctLoc) proms
        ; let hToL = filter (isIllegalFlow dflags) wanted
        ; do { let locs = map (ForbiddenFlow . RealSrcSpan . ctLocSpan . ctLoc ) hToL
             ; if defer
               then do {
                 -- If we're deferring, we warn, but pretend we solved them
                 ; tcPluginIO $ modifyIORef warns ((promLocs ++ locs) ++)
                 ; let fakedFlowProofs = map fakeEvidence hToL
                 ; let fakedPromote = map (fromJust . fakeProm dflags) proms
                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; pprDebug "Faked:" $ ppr faked
                 ; return $ TcPluginOk faked []}
               else do {
                 ; flowMsgTy <- getMsgType "Forbidden flow from H to L"
                 ; promMsgTy <- getMsgType
                     "Unlabeled value used as a labeled value. Perhaps you meant to use 'promote'?"
                 -- If we're changing the messge, we pretend we solved the old
                 -- one and return a new one with an improved error message.
                 ; let changedFlow = map (changeIrredTy flowMsgTy) hToL
                 ; let changedPromote = map (changeIrredTy promMsgTy) proms
                 ; let changed = changedFlow ++ changedPromote
                 ; pprDebug "Changed:" $ ppr changed
                 ; return $ TcPluginContradiction changed }}}
     stop warns = do {
                dflags <- unsafeTcPluginTcM getDynFlags
             ; tcPluginIO $ readIORef warns >>= mapM_ (addWarning dflags) . sort . nub }

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

-- TODO: Make these more robust.
isIllegalFlow :: DynFlags -> Ct -> Bool
isIllegalFlow dflags (CIrredCan (CtWanted predty _ _ _) True) =
   case showSDoc dflags $ ppr predty of
         x@"H ~ L" -> True
         x@"L ~ H" -> True
         _ -> False
isIllegalFlow dflags (CDictCan (CtWanted predty _ _ _) _ _ _) =
   case showSDoc dflags $ ppr predty of
         x@"Less H L" -> True
         _ -> False
isIllegalFlow _ _ = False

-- Is mkReflCo Phantom correct here? We could also use
-- mkCoVarCo with the hole in the evidence.It's not actually
-- used anywhere, as far as I can tell.
fakeEvidence :: Ct -> (EvTerm, Ct)
fakeEvidence ct = (evCoercion $ mkReflCo Phantom anyTy, ct)

changeIrredTy :: Type -> Ct -> Ct
changeIrredTy nt can@(CIrredCan w@CtWanted{} True) =
   CIrredCan {cc_ev = w {ctev_pred = nt}, cc_insol = True}
changeIrredTy _ ct = ct

