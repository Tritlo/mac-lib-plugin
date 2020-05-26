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
         | Promotion SrcSpan ((String, String), String) deriving (Eq, Show)

logSrc :: Log -> SrcSpan
logSrc (ForbiddenFlow l) = l
logSrc (Promotion l _) = l

instance Ord Log where
  compare = compare `on` logSrc

addWarning :: DynFlags -> Log -> IO()
addWarning dflags log = warn msg
  where
    warn =
      putLogMsg dflags NoReason SevWarning (logSrc log) (defaultErrStyle dflags)
    msg = case log of
            ForbiddenFlow _ -> text (flowMsg ++ "!")
            Promotion _ ((ty1, ty2), l) -> text $ promMsg ty1 ty2 l

flowMsg :: String
flowMsg = "Forbidden flow from Secret (H) to Public (L)"

promMsg :: String -> String -> String -> String
promMsg ty1 ty2 l = "Unlabeled '"++ ty1 ++ "' used as a '"
                    ++ (if l == "L" then "Public" else "Secret")
                    ++ " "++ ty2 ++"'. Perhaps you intended to use 'box'?"

logPromotion :: DynFlags -> Ct -> Log
logPromotion dflags ct = Promotion loc ts
    where loc = RealSrcSpan $ ctLocSpan $ ctLoc ct
          Just ts = getPromTys dflags ct

logForbiddenFlow :: Ct -> Log
logForbiddenFlow ct = ForbiddenFlow loc
    where loc = RealSrcSpan $ ctLocSpan $ ctLoc ct

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
        ; let pLogs = map (logPromotion dflags) proms
        ; let hToL = filter isIllegalFlow wanted
        ; let promsToCheck = mapMaybe checkProm proms
        ; do { let ffLogs = map logForbiddenFlow hToL
             ; if defer
               then do {
                 -- If we're deferring, we add a warning, but we 'solve' the
                 -- flow constraints (H ~ L, L ~ H and Less H L). We also
                 -- allow unlabled values to be "promoted" to labeled values.
                 ; tcPluginIO $ modifyIORef warns ((pLogs ++ ffLogs) ++)
                 ; let fakedFlowProofs = map fakeEvidence hToL
                 ; let fakedPromote = mapMaybe fakeProm proms
                 ; let promsToCheck = mapMaybe checkProm proms

                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; mapM_ (pprDebug "Faked:" . ppr)  faked
                 ; mapM_ (pprDebug "To Check:" . ppr) promsToCheck
                 ; return $ TcPluginOk faked promsToCheck}
               else do {
                 ; flowMsgTy <- getMsgType flowMsg
                 ; let promMsgTy ct = do {
                        (Just ((ty1, ty2), l)) <- return $ getPromTys dflags ct
                       ; getMsgType $ promMsg ty1 ty2 l}
                 -- If we're changing the messge, we pretend we solved the old
                 -- one and return a new one with an improved error message.
                 ; let changedFlow = map (changeIrredTy flowMsgTy) hToL
                 ; changedPromote <- mapM (\ct -> (`changeIrredTy` ct)
                                                  <$> promMsgTy ct) proms
                 ; let changed = changedFlow ++ changedPromote
                 ; pprDebug "Changed:" $ ppr changed
                 ; return $ TcPluginContradiction changed }}}
     stop warns =
        do { dflags <- unsafeTcPluginTcM getDynFlags
           ; tcPluginIO $ readIORef warns >>=
                          mapM_ (addWarning dflags) . sort . nub }

fakeProm :: Ct -> Maybe (EvTerm, Ct)
fakeProm ct = fakeEv <$> lie
 where lie = do (prom, _) <- unwrapPromotion (ctPred ct)
                (_, [_,_, ty1, _]) <- splitTyConApp_maybe prom
                return ty1
       fakeEv ty = (evCoercion $ mkReflCo Phantom ty, ct)

checkProm :: Ct -> Maybe Ct
checkProm ct@(CIrredCan w@(CtWanted predty _ _ _) True) =
    case unwrapPromotion predty of
        Just (unwrapped, _) -> Just (ct {cc_ev=w {ctev_pred = unwrapped},
                                                  cc_insol=False})
        _ -> Nothing
checkProm _ = Nothing

getPromTys :: DynFlags -> Ct -> Maybe ((String, String), String)
getPromTys dflags ct = do { (p,l) <- unwrapPromotion (ctPred ct)
                          ; (_, [_,_,ty1,ty2]) <- splitTyConApp_maybe p
                          ; return ((showSDoc dflags $ ppr ty1,
                                     showSDoc dflags $ ppr ty2),
                                     showSDoc dflags $ ppr l)}

unwrapPromotion :: Type -> Maybe (Type, Type)
unwrapPromotion t =
    do (primEq, [k1, k2, ty1, ty2]) <- splitTyConApp_maybe t
       guard (isPrimEqTyCon primEq && isTYPE k1 && isTYPE k2 && isMACRes ty2)
       (_, [l, ty]) <- splitTyConApp_maybe ty2
       guard (isLabel l && isMACId ty)
       (_, [wrappedTy]) <- splitTyConApp_maybe ty
       return (mkTyConApp primEq [k1, k2, ty1, wrappedTy], l)

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
      isSameName name tyCon &&
      isLabel l && isLabel h &&
      isTYPE k1 && isTYPE k2
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
isIllegalFlow ct = isFlowEq (ctPred ct) || isLessTy (ctPred ct)
