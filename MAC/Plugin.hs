{-# LANGUAGE RecordWildCards #-}
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

import Predicate (EqRel(NomEq))
import TysPrim (equalityTyCon)
import Finder (findPluginModule)

import Prelude hiding ((<>))
import FamInstEnv (FamInstMatch(..), FamInst(..), lookupFamInstEnv, FamInstEnvs)

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

getMsgType :: DynFlags -> PDoc -> TcPluginM Type
getMsgType dflags err = do {
     typeErrorCon <- tcLookupTyCon errorMessageTypeErrorFamName
   ; textCon <- promoteDataCon <$> tcLookupDataCon typeErrorTextDataConName
   ; appendCon <- promoteDataCon <$> tcLookupDataCon typeErrorAppendDataConName
   ; vAppendCon <- promoteDataCon <$> tcLookupDataCon typeErrorVAppendDataConName
   ; showTyCon <- promoteDataCon <$> tcLookupDataCon typeErrorShowTypeDataConName
   ; let msg = renderPType textCon appendCon vAppendCon showTyCon err
   ; return $ mkTyConApp typeErrorCon [constraint, msg] }
 where constraint = mkTyConApp constraintKindTyCon []
       renderPType tc ac vc sc err = render' err
         where render' (Append a b) = mkTyConApp ac [render' a, render' b]
               render' (VAppend a b) = mkTyConApp vc [render' a, render' b]
               render' (Text s) =  mkTyConApp tc [mkStrLitTy $ fsLit s]
               render' (ShowTy ty) = mkTyConApp sc [anyTy, ty]
               render' (ShowDoc sd) = render' $ Text $ showSDoc dflags sd

data Log = ForbiddenFlow SrcSpan
         | Promotion SrcSpan ((Type, Type), Type)
         | Defaulting TyCoVar Kind Type SrcSpan

logSrc :: Log -> SrcSpan
logSrc (ForbiddenFlow l) = l
logSrc (Promotion l _) = l
logSrc (Defaulting _ _ _ l) = l

instance Ord Log where
  compare = compare `on` logSrc

instance Eq Log where
  Promotion l1 ((t1a,t1b),t1c) == Promotion l2 ((t2a,t2b),t2c) =
     l1 == l2 && (t1a `eqType` t2a) && (t1b `eqType` t2b) &&   (t1c `eqType` t2c)
  ForbiddenFlow l1 == ForbiddenFlow l2 = l1 == l2
  Defaulting v1 k1 t1 l1 == Defaulting v2 k2 t2 l2 =
    l1 == l2 && k1 `eqType` k2 && t1 `eqType` t2
  _ == _ = False

addWarning :: DynFlags -> Log -> IO()
addWarning dflags log = warn msg
  where
    warn =
      putLogMsg dflags NoReason SevWarning (logSrc log) (defaultErrStyle dflags)
    msg = case log of
            ForbiddenFlow _ -> renderPDoc flowMsg
            Promotion _ ((ty1, ty2), l) -> renderPDoc $ promMsg ty1 ty2 l
            Defaulting var kind ty _ -> renderPDoc $ defaultingMsg var kind ty

-- PDoc is a simple format that matches the UserTypeErrorMessage format, with an
-- additional ShowDoc constructor for convenience. We render this to both a
-- type error and an SDoc
data PDoc = Append PDoc PDoc
          | VAppend PDoc PDoc
          | Text String
          | ShowTy Type
          | ShowDoc SDoc

(<:>) :: PDoc -> PDoc -> PDoc
(<:>) = Append
(<::>) :: PDoc -> PDoc -> PDoc
(<::>) a b = Append (Append a (Text " ")) b
($:$) :: PDoc -> PDoc -> PDoc
($:$) = VAppend

renderPDoc :: PDoc -> SDoc
renderPDoc (Append a b) = renderPDoc a <> renderPDoc b
renderPDoc (VAppend a b) = renderPDoc a $$ renderPDoc b
renderPDoc (Text a)  = text a
renderPDoc (ShowTy t) = ppr t
renderPDoc (ShowDoc t) = t

highName :: PDoc
highName = Text "Secret"

lowName :: PDoc
lowName = Text "Public"

boxName :: PDoc
boxName = Text "box"

flowMsg :: PDoc
flowMsg = Text "Forbidden flow from" <::> highName <::> Text "(H)"
          <::> Text "to" <::> lowName <::> Text "(L)" <:> Text "!"

promMsg :: Type -> Type -> Type -> PDoc
promMsg ty1 ty2 l = Text "Unlabeled" <::> ShowDoc (quotes (ppr ty1))
                    <::> Text "used as a"
                    <::> ShowDoc (quotes (boxedAs <+> ppr ty2) <> dot)
                    $:$ Text "Perhaps you intended to use"
                    <::> ShowDoc (quotes $ renderPDoc boxName) <:> Text "?"
  where boxedAs = case ppLabelMaybe l of
                    Just "L" -> renderPDoc lowName
                    Just "H" -> renderPDoc highName
                    Just t -> text "Labeled" <+> text t
                    _ -> text "Labeled" <+> ppr l

defaultingMsg :: TyCoVar -> Kind -> Type -> PDoc
defaultingMsg var kind ty = Text "Defaulting ambiguous" <::> ShowTy kind
                            <::> Text "to" <::> tyRender
 where tyRender = ShowDoc $ quotes $ case ppLabelMaybe ty of
                                       Just "H" -> renderPDoc $ highName
                                       Just "L" -> renderPDoc $ lowName
                                       Just t -> text t
                                       _ -> ppr ty
       varRender = ppr $ mkTyVarTy var

ppLabelMaybe :: Type -> Maybe String
ppLabelMaybe l = do (tyCon,_) <- splitTyConApp_maybe l
                    return $ occNameString (getOccName tyCon)


logDefaulting :: (TyCoVar, Kind, Type, CtLoc) -> Log
logDefaulting (var, kind, ty, loc) = Defaulting var kind ty rloc
  where rloc = RealSrcSpan $ ctLocSpan loc

logPromotion :: DynFlags -> Ct -> TcPluginM Log
logPromotion dflags ct = do Just ts <- getPromTys ct
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
     nodef = "no-default" `elem` opts
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
        ; (hToL, nonHToL) <- spanM isIllegalFlow wanted
        ; promsToCheck <- mapMaybeM checkProm fakedPromote
        -- Kind defaulting
        ; instEnvs <- getFamInstEnvs
        ; defaultToTyCon <- getDefaultTyCon
        -- We don't want to kind default in cases like `l ~ L`, since otherwise
        -- we will end up in an infinte loop!
        ; let kindDefaults =
                if nodef then []
                else concatMap (getTyVarDefaults instEnvs defaultToTyCon) nonHToL
        ; do { let ffLogs = map logForbiddenFlow hToL
             ; let kdlogs = map logDefaulting kindDefaults
             ; if defer
               then do {
                 -- If we're deferring, we add a warning, but we 'solve' the
                 -- flow constraints H ~ L, L ~ H,  and  Less H L. We also
                 -- allow unlabled values to be "promoted" to labeled values.
                 -- If we encounter an ambiguous type variable of the kind Label,
                 -- we default it to whatever is specified by the Default type
                 -- family.
                 ; tcPluginIO $ modifyIORef warns ((pLogs ++ ffLogs ++ kdlogs) ++)
                 ; let fakedFlowProofs = map fakeEvidence hToL
                 ; let faked = fakedFlowProofs ++ fakedPromote
                 ; defCts <- mapM (defaultingCt defaultToTyCon) kindDefaults
                 ; let proofs =  promsToCheck ++ defCts
                 ; mapM_ (pprDebug "Faked:" . ppr) faked
                 ; mapM_ (pprDebug "To Check:" . ppr) proofs
                 ; return $ TcPluginOk faked proofs}
               else do {
                 ; flowMsgTy <- getMsgType dflags flowMsg
                 ; let promMsgTy ct = do { Just ((ty1, ty2), l) <- getPromTys ct
                                         ; getMsgType dflags $ promMsg ty1 ty2 l}
                 -- If we're changing the message, we pretend we solved the old
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
fakeProm ct = lie >>= fakeEv
 where lie = unwrapPromotion (ctPred ct)
       fakeEv Nothing = return Nothing
       fakeEv (Just _) = return $ Just (evCoercion co, ct)
         where co = mkReflCo Representational (ctPred ct)

checkProm :: (EvTerm, Ct) -> TcPluginM (Maybe Ct)
checkProm (evt, ct@(CIrredCan w@(CtWanted predty _ _ _) True)) =
   do (_, args) <- return $ splitTyConApp predty
      return $ Just (CNonCanonical {
                      cc_ev=w {
                        ctev_pred = mkTyConApp (equalityTyCon Representational) args}})
checkProm _ = return Nothing

getPromTys :: Ct -> TcPluginM (Maybe ((Type, Type), Type))
getPromTys  ct = do  uwr <- unwrapPromotion (ctPred ct)
                     return $ do (p,l) <- uwr
                                 (_, [_,_,ty1,ty2]) <- splitTyConApp_maybe p
                                 return ((ty1, ty2), l)

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
                        -- Using ~R# (eqPrimRepresentational) makes us solve
                        -- Coercible, which is exactly what we want!
                        return $ Just (mkTyConApp (equalityTyCon Representational)
                                                  [k1, k2, ty1, wrappedTy], l)
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
  where isL l =
          case splitTyConApp_maybe t of
             Just (tyCon, _) -> isSameName "MAC.Lattice" (mkDataOcc l) tyCon
             Nothing | isTyVarTy t -> isLabelKind  $ varType (getTyVar "isTyVarTy lied!" t)
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
    Just (tyCon, [l,h]) ->
      isLabelTy l `andM` isSameName "MAC.Core" (mkTcOcc "Res") tyCon
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

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM f xs = do y <- filterM f xs
                n <- filterM (fmap not <$> f) xs
                return (y,n)

orM :: Monad m => m Bool -> m Bool -> m Bool
orM a b = do a' <- a
             if a' then return a' else b

andM :: Monad m => m Bool -> m Bool -> m Bool
andM a b = do a' <- a
              if not a' then return a' else b

-- Kind defaults

defaultingCt :: TyCon -> (TyCoVar, Kind, Type, CtLoc) -> TcPluginM Ct
defaultingCt defaultTyCon (var, kind, def, loc) =
   do ev <- getEv
      return $ CTyEqCan {cc_ev = ev, cc_tyvar = var,
                         cc_rhs = eqTo, cc_eq_rel=eqRel }
 where eqTo = mkTyConApp defaultTyCon [kind]
       predTy = mkTyConApp (equalityTyCon Nominal)
                           [kind, kind, mkTyVarTy var, eqTo]
       eqRel = NomEq
       getEv = do ref <- tcPluginIO $ newIORef Nothing
                  let hole = CoercionHole {ch_co_var = var, ch_ref=ref}
                  return $ CtWanted {ctev_pred = predTy,
                                     ctev_nosh = WDeriv,
                                     ctev_dest = HoleDest hole,
                                     ctev_loc = loc}


getDefaultTyCon :: TcPluginM TyCon
getDefaultTyCon =
   do env <- getTopEnv
      modLookup <- tcPluginIO $
                     findPluginModule env (mkModuleName mod)
      case modLookup of
         Found _ mod  ->
            do name <- lookupOrig mod (mkTcOcc "Default")
               tcLookupTyCon name
         NoPackage uid ->
            pprPanic ("NoPackage when looking for" ++ mod++"!") uid
         FoundMultiple m ->
            pprPanic ("Multiple modules found when looking for" ++ mod ++"!") (ppr m)
         NotFound {..} -> pprPanic (mod ++ "not found!") empty
  where mod = "MAC.KindDefaults"

getTyVarDefaults :: FamInstEnvs -> TyCon -> Ct -> [(TyCoVar, Kind, Type, CtLoc)]
getTyVarDefaults famInsts defaultToTyCon ct = catMaybes $ map getDefault tvs
  where tvs = tyCoVarsOfCtList ct
        lookup kind = lookupFamInstEnv famInsts defaultToTyCon [kind]
        getDefault ty =
           case lookup (varType ty) of
              [FamInstMatch {fim_instance=FamInst{fi_rhs=def}}] ->
                 Just (ty, varType ty, def, ctLoc ct)
              _ -> Nothing
