-- Copyright (c) 2020 Matthias Pall Gissurarson
{-# LANGUAGE DataKinds #-}
module MAC.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcRnTypes
import TcPluginM
import Control.Monad (when, filterM)
import Constraint
import Data.Maybe (catMaybes)
import Data.IORef
import Data.List (nub, sort)
import Data.Function (on)
import PrelNames
import TcEvidence (EvTerm, evCoercion)
import ErrUtils
import qualified MAC.Lattice
import qualified MAC.Core
import qualified MAC.Labeled

import GHC.Hs 

-- We have to add an import of GHC.TypeLits() to the module, otherwise we
-- can get funny messages about interface files being missing
addTypeLitImport :: HsParsedModule -> HsParsedModule
addTypeLitImport pm@HsParsedModule{hpm_module=L l m@HsModule{hsmodImports = imps}}
   = pm{hpm_module = L l m{hsmodImports = imp:imps}}
  where imp = noLoc (simpleImportDecl (moduleName gHC_TYPELITS)) {ideclHiding = Just (False, noLoc [])}


plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . flowPlugin
                       , parsedResultAction = \_ _ -> return . addTypeLitImport
                       , pluginRecompile = purePlugin }

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

logPromotion :: DynFlags -> Ct -> TcPluginM Log
logPromotion dflags ct = do Just ts <- getPromTys dflags ct
                            return $ Promotion loc ts
    where loc = RealSrcSpan $ ctLocSpan $ ctLoc ct

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
        ; fakedPromote <- mapMaybeM fakeProm wanted
        ; pLogs <- mapM (logPromotion dflags . snd) fakedPromote
        ; hToL <- filterM isIllegalFlow wanted
        ; promsToCheck <- mapMaybeM (checkProm . snd) fakedPromote
        ; do { let ffLogs = map logForbiddenFlow hToL
             ; if defer
               then do {
                 -- If we're deferring, we add a warning, but we 'solve' the
                 -- flow constraints (H ~ L, L ~ H and Less H L). We also
                 -- allow unlabled values to be "promoted" to labeled values.
                 ; tcPluginIO $ modifyIORef warns ((pLogs ++ ffLogs) ++)
                 ; let fakedFlowProofs = map fakeEvidence hToL
                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; mapM_ (pprDebug "Faked:" . ppr)  faked
                 ; mapM_ (pprDebug "To Check:" . ppr) promsToCheck
                 ; return $ TcPluginOk faked promsToCheck}
               else do {
                 ; flowMsgTy <- getMsgType flowMsg
                 ; let promMsgTy ct = do {
                        (Just ((ty1, ty2), l)) <- getPromTys dflags ct
                       ; getMsgType $ promMsg ty1 ty2 l}
                 -- If we're changing the messge, we pretend we solved the old
                 -- one and return a new one with an improved error message.
                 ; let changedFlow = map (changeIrredTy flowMsgTy) hToL
                 ; changedPromote <- mapM (\(_, ct) -> (`changeIrredTy` ct)
                                                       <$> promMsgTy ct) fakedPromote
                 ; let changed = changedFlow ++ changedPromote
                 ; pprDebug "Changed:" $ ppr changed
                 ; return $ TcPluginContradiction changed }}}
     stop warns =
        do { dflags <- unsafeTcPluginTcM getDynFlags
           ; tcPluginIO $ readIORef warns >>=
                          mapM_ (addWarning dflags) . sort . nub }

fakeProm :: Ct -> TcPluginM (Maybe (EvTerm, Ct))
fakeProm ct = do l <- lie
                 return $ fakeEv <$> l
 where lie = do uwr <- unwrapPromotion (ctPred ct)
                return $ do (prom, _) <- uwr
                            (_, [_,_, ty1, _]) <- splitTyConApp_maybe prom
                            return ty1
       fakeEv ty = (evCoercion $ mkReflCo Phantom ty, ct)

checkProm :: Ct -> TcPluginM (Maybe Ct)
checkProm ct@(CIrredCan w@(CtWanted predty _ _ _) True) =
   do uwr <- unwrapPromotion predty 
      case uwr of
          Just (unwrapped, _) -> return $ Just (ct {cc_ev=w {ctev_pred = unwrapped},
                                                             cc_insol=False})
          _ -> return Nothing
checkProm _ = return Nothing

getPromTys :: DynFlags -> Ct -> TcPluginM (Maybe ((String, String), String))
getPromTys dflags ct = do  uwr <- unwrapPromotion (ctPred ct)
                           return $ do (p,l) <- uwr
                                       (_, [_,_,ty1,ty2]) <- splitTyConApp_maybe p
                                       return ((showSDoc dflags $ ppr ty1,
                                                showSDoc dflags $ ppr ty2),
                                                showSDoc dflags $ ppr l)

unwrapPromotion :: Type -> TcPluginM (Maybe (Type, Type))
unwrapPromotion t =
    do  case splitTyConApp_maybe t of
          Just (primEq, [k1, k2, ty1, ty2]) |
             isPrimEqTyCon primEq && isTYPE k1 && isTYPE k2 ->
                do imr <- isMACRes ty2
                   if not imr
                   then return Nothing
                   else case splitTyConApp_maybe ty2 of
                          Just (_, [l, ty]) ->
                             do g <- isLabel l `andM` isMACId ty
                                if not g
                                then return Nothing
                                else case splitTyConApp_maybe ty of
                                  Just (_, [wrappedTy]) ->
                                     return $ Just (mkTyConApp primEq [k1, k2, ty1, wrappedTy], l)
                                  _ -> return Nothing
                          _ -> return Nothing
          _ -> return Nothing

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
isSameName :: String -> String -> TyCon -> TcPluginM Bool
isSameName mod str con =
   do env <- getTopEnv
      modLookup <- findImportedModule (mkModuleName mod) (Just $ mkFastString "mac")
      case modLookup of 
        Found _ mod -> do name <- lookupOrig mod $ mkTcOcc str
                          return $ name == getName con
        _ -> return False

isLabelL :: Type -> TcPluginM Bool
isLabelL t =
  case splitTyConApp_maybe t of
    Just (tyCon, []) -> isSameName "MAC.Lattice" "L" tyCon
    _ -> return False

isLabelH :: Type -> TcPluginM Bool
isLabelH t =
 case splitTyConApp_maybe t of
   Just (tyCon, []) -> isSameName "MAC.Lattice" "H" tyCon
   _ -> return False

isLabel :: Type -> TcPluginM Bool
isLabel t = do a <- isLabelL t
               if a then return True
                    else isLabelH t

isLessTy :: Type -> TcPluginM Bool
isLessTy t =
  case splitTyConApp_maybe t of
    Just (tyCon, [k1,k2,l,h]) | isTYPE k1 && isTYPE k2 ->
                                  isSameName "MAC.Lattice" "Less" tyCon `andM`
                                  isLabel l `andM` isLabel h
    _ -> return False

isPrimEqTyCon :: TyCon -> Bool
isPrimEqTyCon tyCon = getUnique tyCon == eqPrimTyConKey

isTYPE :: Type -> Bool
isTYPE t = case splitTyConApp_maybe t of
            Just (t, args) ->  getUnique t == tYPETyConKey
            _ -> False

isMACRes :: Type -> TcPluginM Bool
isMACRes t =
  case splitTyConApp_maybe t of
    Just (tyCon, [l,h]) -> isSameName "MAC.Core" "Res" tyCon `andM` isLabel l
    _ -> return False

isMACId :: Type -> TcPluginM Bool
isMACId t =
  case splitTyConApp_maybe t of
    Just (tyCon, [l]) -> isSameName "MAC.Labeled" "Id" tyCon
    _ -> return False


isFlowEq :: Type -> TcPluginM Bool
isFlowEq t =
   case splitTyConApp_maybe t of
    Just (tyCon, [k1,k2,l,h]) | getUnique tyCon == eqPrimTyConKey &&
                                isTYPE k1 && isTYPE k2 ->
                                isLabel l `andM` isLabel h
    _ -> return False

isIllegalFlow :: Ct -> TcPluginM Bool
isIllegalFlow ct = isFlowEq (ctPred ct) `orM` isLessTy (ctPred ct)

-- utilities
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

orM :: Monad m => m Bool -> m Bool -> m Bool
orM a b = do a' <- a
             if a' then return a' else b

andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = do a' <- a
              if not a' then return a' else b
