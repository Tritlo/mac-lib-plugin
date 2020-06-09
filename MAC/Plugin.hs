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

import TysPrim (equalityTyCon)
import Finder (findPluginModule)

import GHC.Hs 

-- We have to add an import of GHC.TypeLits() to the module, otherwise we
-- can get funny messages about interface files being missing
addTypeLitImport :: HsParsedModule -> HsParsedModule
addTypeLitImport pm@HsParsedModule{hpm_module=L l m@HsModule{hsmodImports = imps}}
   = pm{hpm_module = L l m{hsmodImports = imp:imps}}
  where imp = L l (simpleImportDecl (moduleName gHC_TYPELITS)) {
                    ideclHiding = Just (False, L l []),
                    ideclAs = Just (L l (mkModuleName ("MAC.Plugin.GHC.TypeLits"))),
                    ideclImplicit = True }

-- If we're deferring, we need MkRes and MkId to be in scope, so that the Coercible a (Res l (Id a))
-- constraints generated during promotion are solveable.
addResAndIdImport :: [CommandLineOption] -> HsParsedModule -> HsParsedModule
addResAndIdImport opts pm@HsParsedModule{hpm_module=L l m@HsModule{hsmodImports = imps}}
 = if not defer then pm
   else pm{hpm_module = L l m{hsmodImports = resImp:idImp:imps}}
  where defer = "defer" `elem` opts
        imp :: String -> OccName -> GenLocated SrcSpan (ImportDecl GhcPs)
        imp mod con = loc (simpleImportDecl (mkModuleName mod)) {
          ideclHiding = Just (False, loc [loc $ IEThingAbs noExtField (occToLie con)]),
          ideclAs = Just (loc (mkModuleName ("MAC.Plugin." ++ mod))), -- Make sure we don't clash
          ideclImplicit = True -- If we import it implicitly, we don't get unused-import warnings.
          }
        loc = L l
        -- We import only the constructors
        resImp = imp "MAC.Core" (mkDataOcc "MkRes")
        idImp = imp "MAC.Labeled" (mkDataOcc "MkId")
        occToLie :: OccName -> LIEWrappedName RdrName
        occToLie = loc . IEName . loc . Unqual

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = Just . flowPlugin
                       , parsedResultAction = \opts _ -> return . addResAndIdImport opts . addTypeLitImport
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
                    ++ (if (l == "'L" || l == "L") then "Public" else "Secret")
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
     debug = True --"debug" `elem` opts
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
        ; promsToCheck <- mapMaybeM checkProm fakedPromote
        ; do { let ffLogs = map logForbiddenFlow hToL
             ; if defer
               then do {
                 -- If we're deferring, we add a warning, but we 'solve' the flow constraints H ~ L, L ~ H,  and
                 -- Less H L. We also allow unlabled values to be "promoted" to labeled values.
                 ; tcPluginIO $ modifyIORef warns ((pLogs ++ ffLogs) ++)
                 ; let fakedFlowProofs = map fakeEvidence hToL
                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; mapM_ (pprDebug "Faked:" . ppr) faked
                 ; mapM_ (pprDebug "To Check:" . ppr) promsToCheck
                 ; return $ TcPluginOk faked promsToCheck}
               else do {
                 ; flowMsgTy <- getMsgType flowMsg
                 ; let promMsgTy ct = do { Just ((ty1, ty2), l) <- getPromTys dflags ct
                                         ; getMsgType $ promMsg ty1 ty2 l}
                 -- If we're changing the message, we pretend we solved the old one and return a new one with an
                 -- improved error message.
                 ; let changedFlow = map (changeIrredTy flowMsgTy) hToL
                 ; changedPromote <- mapM (\(_, ct) -> (`changeIrredTy` ct) <$> promMsgTy ct) fakedPromote
                 ; let changed = changedFlow ++ changedPromote
                 ; pprDebug "Changed:" $ ppr changed
                 ; return $ TcPluginContradiction changed }}}
     stop warns =
        do { dflags <- unsafeTcPluginTcM getDynFlags
           ; tcPluginIO $ readIORef warns >>=
                          mapM_ (addWarning dflags) . sort . nub }

fakeProm :: Ct -> TcPluginM (Maybe (EvTerm, Ct))
fakeProm ct = lie >>= fakeEv
 where lie = unwrapPromotion (ctPred ct)
       fakeEv Nothing = return Nothing
       fakeEv (Just _) = return $ Just (evCoercion co, ct)
         where co = mkReflCo Representational (ctPred ct)

checkProm :: (EvTerm, Ct) -> TcPluginM (Maybe Ct)
checkProm (evt, ct@(CIrredCan w@(CtWanted predty _ _ _) True)) =
   do (_, args) <- return $ splitTyConApp predty
      return $ Just (CNonCanonical {cc_ev=w {ctev_pred = mkTyConApp (equalityTyCon Representational) args}})
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
  do case splitTyConApp_maybe t of
       Just (primEq, [k1, k2, ty1, ty2]) | isPrimEqTyCon primEq
                                        && isTYPE k1 && isTYPE k2 ->
         do imr <- isMACRes ty2
            if not imr
            then return Nothing
            else case splitTyConApp_maybe ty2 of
              Just (_, [l, ty]) ->
                do g <- isLabelTy l `andM` isMACId ty
                   if not g
                   then return Nothing
                   else case splitTyConApp_maybe ty of
                     Just (_, [wrappedTy]) ->
                        -- Using ~R# (eqPrimRepresentational) makes us solve Coercible, which is exactly what we want!
                        return $ Just (mkTyConApp (equalityTyCon Representational) [k1, k2, ty1, wrappedTy], l)
                     _ -> return Nothing
              _ -> return Nothing
       _ -> return Nothing

-- Is mkReflCo Phantom correct here? We could also use mkCoVarCo with the hole
-- in the evidence. It's not actually used anywhere, as far as I can tell.
fakeEvidence :: Ct -> (EvTerm, Ct)
fakeEvidence ct = (evCoercion $ mkReflCo Phantom (ctPred ct), ct)

changeIrredTy :: Type -> Ct -> Ct
changeIrredTy nt can@(CIrredCan w@CtWanted{} True) =
   CNonCanonical {cc_ev = w {ctev_pred = nt}}
changeIrredTy _ ct = ct

-- In these functions, we rely on the fact that the MAC.Lattice.L that we're
-- using refer to the same MAC.Lattice.L as the user is using. Otherwise, we
-- might accidentally 'accept' a MAC.Lattice.L from another package.
isSameName :: String -> OccName -> TyCon -> TcPluginM Bool
isSameName mod occ con =
   do env <- getTopEnv
      modLookup <- tcPluginIO $ findPluginModule env (mkModuleName mod)
      case modLookup of 
        Found _ mod -> do name <- lookupOrig mod occ
                          return $ case isPromotedDataCon_maybe con of
                                    Just c -> name == getName c
                                    _ ->  name == getName con
        _ -> return False

isLabelTy :: Type -> TcPluginM Bool
isLabelTy t = isL "L" `orM` isL "H"
  where isL l = case splitTyConApp_maybe t of
                  Just (tyCon, _) -> isSameName "MAC.Lattice" (mkDataOcc l) tyCon
                  _ -> return False

isLessTy :: Type -> TcPluginM Bool
isLessTy t =
  case splitTyConApp_maybe t of
    Just (tyCon, [k1,k2]) -> isLabelTy k1 `andM` isLabelTy k2 `andM`
                             isSameName "MAC.Lattice" (mkClsOcc "Less") tyCon
    _ -> return False

isPrimEqTyCon :: TyCon -> Bool
isPrimEqTyCon tyCon = getUnique tyCon == eqPrimTyConKey

isTYPE :: Type -> Bool
isTYPE t = case splitTyConApp_maybe t of
            Just (t, args) ->  getUnique t == tYPETyConKey
            _ -> False

isLabelKind :: Type -> TcPluginM Bool
isLabelKind t =
  case splitTyConApp_maybe t of
    Just (tyCon, _) -> isSameName "MAC.Lattice" (mkTcOcc "Label") tyCon
    _ -> return False

isMACRes :: Type -> TcPluginM Bool
isMACRes t =
  case splitTyConApp_maybe t of
    Just (tyCon, [l,h]) ->isLabelTy l `andM` isSameName "MAC.Core" (mkTcOcc "Res") tyCon
    _ -> return False

isMACId :: Type -> TcPluginM Bool
isMACId t =
  case splitTyConApp_maybe t of
    Just (tyCon, [l]) -> isSameName "MAC.Labeled" (mkTcOcc "Id") tyCon
    _ -> return False


isFlowEq :: Type -> TcPluginM Bool
isFlowEq t =
   case splitTyConApp_maybe t of
    Just (tyCon, [k1,k2,l,h]) | getUnique tyCon == eqPrimTyConKey ->
                                isLabelKind k1 `andM` isLabelKind k2 `andM`
                                isLabelTy l `andM` isLabelTy h
    _ -> return False

isIllegalFlow :: Ct -> TcPluginM Bool
isIllegalFlow ct = isFlowEq (ctPred ct) `orM` isLessTy (ctPred ct)

-- Utils
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

orM :: Monad m => m Bool -> m Bool -> m Bool
orM a b = do a' <- a
             if a' then return a' else b

andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = do a' <- a
              if not a' then return a' else b
