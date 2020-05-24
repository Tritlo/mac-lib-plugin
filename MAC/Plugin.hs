-- Copyright (c) 2020 Matthias Pall Gissurarson
{-# LANGUAGE  TemplateHaskell #-}
module MAC.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcRnTypes
import TcPluginM
import Control.Monad (when, guard)
import Constraint
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.IORef
import Data.List (nub, sort)
import Data.Function (on)
import PrelNames
import TcEvidence (EvTerm, evCoercion)
import ErrUtils
import MAC.PluginTh
import qualified MAC.Lattice
import qualified MAC.Core
import qualified MAC.Labeled

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
        ; let proms = filter (isJust . fakeProm) wanted
        ; let promLocs = map (Promotion . RealSrcSpan . ctLocSpan . ctLoc) proms
        ; let hToL = filter isIllegalFlow wanted
        ; do { let locs = map (ForbiddenFlow . RealSrcSpan . ctLocSpan . ctLoc ) hToL
             ; if defer
               then do {
                 -- If we're deferring, we warn, but pretend we solved them
                 ; tcPluginIO $ modifyIORef warns ((promLocs ++ locs) ++)
                 ; let fakedFlowProofs = map fakeEvidence hToL
                 ; let fakedPromote = mapMaybe fakeProm proms
                 ; let promsToCheck = mapMaybe checkProm proms
                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; mapM_ (pprDebug "Faked:" . ppr)  faked
                 ; mapM_ (pprDebug "To Check:" . ppr) promsToCheck
                 ; return $ TcPluginOk faked promsToCheck}
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

fakeProm :: Ct -> Maybe (EvTerm, Ct)
fakeProm ct@(CIrredCan (CtWanted predty _ _ _) True) = fakeEv <$> lie
 where lie = do prom <- unwrapPromotion predty
                (_, [_,_, ty1, _]) <- splitTyConApp_maybe prom
                return ty1
       fakeEv ty = (evCoercion $ mkReflCo Phantom ty, ct)
fakeProm _ = Nothing

checkProm :: Ct -> Maybe Ct
checkProm ct@(CIrredCan w@(CtWanted predty _ _ _) True) =
    case unwrapPromotion predty of
        Just unwrapped -> Just (ct {cc_ev=w {ctev_pred = unwrapped}, cc_insol=False})
        _ -> Nothing
checkProm _ = Nothing

unwrapPromotion :: Type -> Maybe Type
unwrapPromotion t =
    do (primEq, [k1, k2, ty1, ty2]) <- splitTyConApp_maybe t
       guard (isPrimEqTyCon primEq && isTYPE k1 && isTYPE k2 && isMACRes ty2)
       (_, [l, ty]) <- splitTyConApp_maybe ty2
       guard (isLabel l && isMACId ty)
       (_, [wrappedTy]) <- splitTyConApp_maybe ty
       return $ mkTyConApp primEq [k1, k2, ty1, wrappedTy]

-- Is mkReflCo Phantom correct here? We could also use
-- mkCoVarCo with the hole in the evidence.It's not actually
-- used anywhere, as far as I can tell.
fakeEvidence :: Ct -> (EvTerm, Ct)
fakeEvidence ct = (evCoercion $ mkReflCo Phantom anyTy, ct)

changeIrredTy :: Type -> Ct -> Ct
changeIrredTy nt can@(CIrredCan w@CtWanted{} True) =
   CIrredCan {cc_ev = w {ctev_pred = nt}, cc_insol = True}
changeIrredTy _ ct = ct

-- In these functions, we rely on the fact that the MAC.Lattice.L that we're
-- using refer to the same MAC.Lattice.L as the user is using. Otherwise, we
-- might accidentally 'accept' a MAC.Lattice.L from another package.
isSameName :: String -> TyCon -> Bool
isSameName str con = str == nameStableString (getName con)

isLabelL :: Type -> Bool
isLabelL t =
  case splitTyConApp_maybe t of
    Just (tyCon, []) -> isSameName name tyCon
    _ -> False
  where name = $(lookupStableName "MAC.Lattice.L")

isLabelH :: Type -> Bool
isLabelH t =
 case splitTyConApp_maybe t of
   Just (tyCon, []) -> isSameName name tyCon
   _ -> False
  where name = $(lookupStableName "MAC.Lattice.H")

isLabel :: Type -> Bool
isLabel t = isLabelL t || isLabelH t

isLessTy :: Type -> Bool
isLessTy t =
  case splitTyConApp_maybe t of
    Just (tyCon, [k1,k2,l,h]) ->
      isSameName name tyCon && isLabel l && isLabel h && isTYPE k1 && isTYPE k2
    _ -> False
  where name = $(lookupStableName "MAC.Lattice.Less")

isPrimEqTyCon :: TyCon -> Bool
isPrimEqTyCon tyCon = getUnique tyCon == eqPrimTyConKey

isTYPE :: Type -> Bool
isTYPE t = case splitTyConApp_maybe t of
            Just (t, args) ->  getUnique t == tYPETyConKey
            _ -> False

isMACRes :: Type -> Bool
isMACRes t =
  case splitTyConApp_maybe t of
    Just (tyCon, [l,h]) -> isSameName name tyCon && isLabel l
    _ -> False
  where name = $(lookupStableName "MAC.Core.Res")

isMACId :: Type -> Bool
isMACId t =
  case splitTyConApp_maybe t of
    Just (tyCon, [l]) -> isSameName name tyCon
    _ -> False
  where name = $(lookupStableName "MAC.Labeled.Id")

isFlowEq :: Type -> Bool
isFlowEq t =
   case splitTyConApp_maybe t of
    Just (tyCon, [k1,k2,l,h]) ->
      getUnique tyCon == eqPrimTyConKey &&
      isLabel l && isLabel h && isTYPE k1 && isTYPE k2
    _ -> False

isIllegalFlow :: Ct -> Bool
isIllegalFlow (CIrredCan (CtWanted predty _ _ _) True) = isFlowEq predty
isIllegalFlow (CDictCan (CtWanted predty _ _ _) _ _ _) = isLessTy predty
isIllegalFlow _ = False
