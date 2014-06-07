{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--module Sat where

import qualified Minisat
import Minisat ( Var, Lit, MinisatM )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Applicative
import Control.Monad

import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.System
import Distribution.Verbosity
import Distribution.Version
import Distribution.Package ( PackageName(..), PackageId, Package(..), InstalledPackageId,
                              Dependency(..), packageName, PackageIdentifier(..) )
import Distribution.Simple.GHC as GHC
import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Client.PackageIndex
import Distribution.Client.IndexUtils as Idx
import qualified Distribution.Client.PackageIndex as PI
import Distribution.Client.Types ( Repo(..), RemoteRepo(..), SourcePackageDb(..)
                                 , SourcePackage(..), InstalledPackage(..) )
import Distribution.Text ( display )
import Network.URI ( parseURI )
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Monoid
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import Data.IORef

------------------------------------------------------------------------------
-- Domain Variables
------------------------------------------------------------------------------

-- PackageId = PackageName + Version

type FlagAssignment = PD.FlagAssignment

data PackageConfig = PackageConfig
  { pkgCfgPackageId :: !PackageId
  , pkgCfgFlags     :: !FlagAssignment
  } deriving (Eq, Ord, Show)

displayPkgCfg :: PackageConfig -> String
displayPkgCfg pc =
  display (pkgCfgPackageId pc) <> "[" <> intercalate "," ppFlags <> "]"
 where
   ppFlags = [ (if isSet then '+' else '-'):name 
             | (PD.FlagName name, isSet) <- pkgCfgFlags pc ]

type PackageCandidates = Dependency  -- package name + version range

--  InstalledPackageId = item in the package DB, a way to refer to a
--  PackageConfig that has been installed in a DB

------------------------------------------------------------------------------
-- Domain Utils
------------------------------------------------------------------------------

data CabalEnv = CabalEnv
  { ceCompilerId        :: !CompilerId
  , ceInstalledPackages :: !(PackageIndex InstalledPackage)
  , ceAvailablePackages :: !(PackageIndex SourcePackage)
  }

installedVersions :: CabalEnv -> PackageName -> VersionRange
                  -> IO [InstalledPackage]
installedVersions env name versions =
  return $! lookupDependency (ceInstalledPackages env)
                             (Dependency name versions)

hardwiredPackages :: S.Set PackageName
hardwiredPackages = S.fromList [ PackageName "base" ]

-- TODO: better name?
availableVersions :: CabalEnv -> PackageName -> VersionRange
                  -> IO [SourcePackage]
availableVersions _env name _versions | name `S.member` hardwiredPackages
    -- Cannot re-install these packages
                                          = return [] 
availableVersions env name versions =
  return $! lookupDependency (ceAvailablePackages env)
                             (Dependency name versions)

packageConfigs :: CabalEnv -> PackageId -> IO [PackageConfig]
packageConfigs = undefined

installedDeps :: CabalEnv -> InstalledPackageId -> IO [InstalledPackageId]
installedDeps = undefined


defaultUserRepo :: IO Repo
defaultUserRepo = do
  home <- getHomeDirectory
  let localDir = home </> ".cabal" </> "packages" </> "hackage.haskell.org/"
  let Just repoURI = parseURI "http://hackage.haskell.org"
  return $ Repo{ repoKind = Left $ RemoteRepo "hackage.haskell.org" repoURI
               , repoLocalDir = localDir }

newCabalEnv :: {- TODO: Config -> -} IO CabalEnv
newCabalEnv = do
  let verbosity = normal

  let pkgDbStack = [GlobalPackageDB, UserPackageDB]
  (compiler, _optPlatform, programConfig)
      <- configure verbosity Nothing Nothing  defaultProgramConfiguration

  putStrLn "Reading package databases..."
  idx <- Idx.convert <$> GHC.getInstalledPackages verbosity pkgDbStack programConfig

  repo <- defaultUserRepo
  
  srcdb <- getSourcePackages verbosity [repo]
  let srcIdx = packageIndex srcdb

  return $ CabalEnv{ ceCompilerId = compilerId compiler
                   , ceInstalledPackages = idx
                   , ceAvailablePackages = srcIdx
                   }

------------------------------------------------------------------------------
-- Package Constraint Generation
------------------------------------------------------------------------------

type GenConstraintsState = S.Set (Either InstalledPackageId PackageId)

type GenM a = StateT GenConstraintsState S a

runGenM :: GenM a -> S a
runGenM m = evalStateT m S.empty

liftS :: S a -> GenM a
liftS = lift

processItem :: Either InstalledPackageId PackageId -> GenM () -> GenM ()
processItem key body = do
  done <- get
  if S.member key done then
    return ()
   else do
     put $! S.insert key done
     body

addConstraintsInstalledPackage :: CabalEnv -> InstalledPackage -> GenM ()
addConstraintsInstalledPackage env (InstalledPackage inst deps) = do
  let instPkgId = IPI.installedPackageId inst
  processItem (Left instPkgId) $ do
    let instDeps = IPI.depends inst
    mapM_ (liftS . addConstraint)
          [ DependsOnInstalled instPkgId dep | dep <- instDeps ]
    forM_ deps $ \depPkgId -> do
      [installed] <- liftIO $ installedVersions env (pkgName depPkgId)
                       (thisVersion (pkgVersion depPkgId))
      addConstraintsInstalledPackage env installed

------------------------------------------------------------------------------
-- Constraint Language
------------------------------------------------------------------------------

data Constraint
  = WantPkgName !PackageName
  | WantPkgId !PackageId
  | DependsOn !PackageConfig !PackageName [Version] (Maybe InstalledPackageId)
  | DependsOnInstalled !InstalledPackageId !InstalledPackageId
  | HasConfigs !PackageId [FlagAssignment] (Maybe InstalledPackageId)
  | OnlyOneConfig PackageId [FlagAssignment] (Maybe InstalledPackageId)
  | OnlyOneVersion PackageName [Version]
  deriving (Eq, Ord, Show)

data ConstraintVar
  = VPkgId !PackageId
  | VPkgCfg !PackageConfig
  | VPkgName !PackageName
  | VPkgInst !InstalledPackageId
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- Solver Monad
------------------------------------------------------------------------------

data SolverEnv = SolverEnv

data SolverState = SolverState
  { ssConstraints :: S.Set Constraint
  , ssConstraintVars :: M.Map ConstraintVar Var
  }

newtype S a = S (ReaderT SolverEnv (StateT SolverState MinisatM) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SolverState, MonadReader SolverEnv)

runS :: SolverEnv -> S a -> IO a
runS env (S m) = Minisat.runMinisatM $ evalStateT (runReaderT m env) s0
  where s0 = SolverState S.empty M.empty

minisat :: MinisatM a -> S a
minisat m = S (lift (lift m))

logM :: String -> S ()
logM _ = return ()
--logM s = liftIO $ putStrLn s

satColour :: String -> String
satColour s = "\x1b[32m" <> s <>  "\x1b[0m"

------------------------------------------------------------------------------
-- Minisat Utils
------------------------------------------------------------------------------

no :: Var -> Lit
no = Minisat.negLit . Minisat.mkLit

yes :: Var -> Lit
yes = Minisat.mkLit

addClause' :: [Lit] -> S ()
addClause' ls = do
  liftIO $ print ls
  ok <- minisat $ Minisat.addClause ls
  unless ok $ error "Unsat"

-- | Encode the constraint that only one of the variables may be true.
atMostOne :: [Var] -> S ()
atMostOne [] = return ()
atMostOne [v] = return ()
atMostOne vs = do
   logM $ satColour $ intercalate " + " (map show vs) <> " <= 1"
   go vs
 where
   -- This is quadratic in the number of variables.  There are more efficient
   -- encodings, but they are quite complicated and some of them actually cause
   -- some performance issues for certain SAT solvers.  (Search for "cardinality
   -- constraints" and "SAT".)
   go [] = return ()
   go (v:vs) = do
     forM_ vs $ \v' -> do
       --logM $ satColour $ show (no v) <> " || " <> show (no v')
       addClause' [no v, no v']
     go vs

------------------------------------------------------------------------------
-- Solver API
------------------------------------------------------------------------------

getVar :: ConstraintVar -> S Var
getVar x = do
  s <- get
  case M.lookup x (ssConstraintVars s) of
    Just v -> return v
    Nothing -> do
      v <- minisat Minisat.freshVar
      put $! s{ ssConstraintVars = M.insert x v (ssConstraintVars s) }
      return v

hasConstraint :: Constraint -> S Bool
hasConstraint c = do
  cs <- gets ssConstraints
  return $! S.member c cs

-- | Add constraint and fail immediately if system is known to be unsatisfiable.
addConstraint :: Constraint -> S ()
addConstraint c = do
  case c of
    WantPkgId pkgId -> do
      p <- getVar (VPkgId pkgId)
      addClause' [yes p]
    WantPkgName pkgName -> do
      v <- getVar (VPkgName pkgName)
      addClause' [yes v]
    OnlyOneVersion name versions -> do
      let ids = [ PackageIdentifier name version | version <- versions ]
      vIds <- mapM (getVar . VPkgId) ids
      atMostOne vIds
    OnlyOneConfig pkgId flagss mbInstPkg -> do
      let configs = [ PackageConfig pkgId flags | flags <- flagss ]
      vConfigs <- mapM (getVar . VPkgCfg) configs
      mbVInstPkg <- getMaybeVar (VPkgInst <$> mbInstPkg)
      atMostOne (maybeToList mbVInstPkg ++ vConfigs)
    HasConfigs pkg [] Nothing -> error "Invalid constraint HasConfigs"
    HasConfigs pkgId flagss mbInstPkg -> do
      let configs = [ PackageConfig pkgId flags | flags <- flagss ]
      vPkgId <- getVar (VPkgId pkgId)
      vConfigs <- mapM (getVar . VPkgCfg) configs
      mbVInstPkg <- getMaybeVar (VPkgInst <$> mbInstPkg)
      addClause' (no vPkgId : map yes (maybeToList mbVInstPkg ++ vConfigs))
    DependsOnInstalled instPkgId depId -> do
      vInstPkgId <- getVar (VPkgInst instPkgId)
      vDepId <- getVar (VPkgInst depId)
      addClause' [no vInstPkgId, yes vDepId]
    DependsOn pkgCfg depPkgId depVersions mbInstPkg -> do
      vCfg <- getVar (VPkgCfg pkgCfg)
      let depIds = map (PackageIdentifier depPkgId) depVersions
      vDepIds <- mapM (getVar . VPkgId) depIds
      mbVInstPkg <- getMaybeVar (VPkgInst <$> mbInstPkg)
      addClause' (no vCfg : map yes (maybeToList mbVInstPkg ++ vDepIds))
  s <- get
  put $! s{ ssConstraints = S.insert c (ssConstraints s) }
 where
   getMaybeVar :: Maybe ConstraintVar -> S (Maybe Var)
   getMaybeVar Nothing = return Nothing
   getMaybeVar (Just v) = Just <$> getVar v

{-
isSatisfied :: Constraint -> S (Maybe Bool)
isSatisfied c = case c of
  WantPkgId pkgId -> do
    vPkgId <- getVar (VPkgId pkgId)
    minisat $ modelValue vPkgId
  WantPkgName name -> do
    vName <- getVar (VPkgName name)
    minisat $ modelValue vName
  _ -> return Nothing
-}
solve :: S (Maybe (M.Map ConstraintVar Bool))
solve = do
  ok <- minisat $ Minisat.solve True []
  if not ok then
    return Nothing
   else do
    vars <- gets ssConstraintVars
    models <- forM (M.toList vars) $ \(cvar, val) -> do
                mbVal <- minisat (Minisat.modelValue val)
                return $ do model <- mbVal
                            return (cvar, model)
    return $! Just $! M.fromList (catMaybes models)

------------------------------------------------------------------------------
-- Printing
------------------------------------------------------------------------------

getConstraints :: S [Constraint]
getConstraints = S.toList <$> gets ssConstraints


renderConstraint :: (ConstraintVar -> Maybe Bool) -> Constraint -> String
renderConstraint env constraint = case constraint of
  WantPkgId pkg -> "want(" <> rPkg pkg <> ")"
  WantPkgName name -> "want(" <> rName name <> ")"
  HasConfigs pkg flags mbInstalled ->
    "hasConfigs(" <> rPkg pkg <> "," <>
       intercalate "," (map (rCfg . PackageConfig pkg) flags) <>
       ")"
  OnlyOneConfig pkg flags mbInstalled ->
    "onlyOneConfig(" <>
       intercalate "," (map (rCfg . PackageConfig pkg) flags) <>
       ")"
  DependsOnInstalled src dst ->
    "dependsOnInst(" <> rInst src <> "," <> rInst dst <> ")"
  _ -> ""
 where
   rMaybeBool str Nothing      = str
   rMaybeBool str (Just True)  = green str
   rMaybeBool str (Just False) = boldBlack str
   rPkg pkg = rMaybeBool (display pkg) (env (VPkgId pkg))
   rName name = rMaybeBool (display name) (env (VPkgName name))
   rCfg cfg = rMaybeBool (displayPkgCfg cfg) (env (VPkgCfg cfg))
   rInst inst = rMaybeBool (display inst) (env (VPkgInst inst))

green :: String -> String
green s = "\x1b[32m" <> s <> "\x1b[0m"

boldBlack :: String -> String
boldBlack s = "\x1b[30;1m" <> s <> "\x1b[0m"

------------------------------------------------------------------------------
-- Driver
------------------------------------------------------------------------------

main = do
  env <- newCabalEnv
  insts <- installedVersions env (PackageName "containers") anyVersion
  runS SolverEnv $ do
    runGenM $ do
      mapM_ (addConstraintsInstalledPackage env) insts
{-
    let pkg_foo_11 = PackageIdentifier (PackageName "foo") (Version [1,1] [])
        cfg_foo_11 = PackageConfig pkg_foo_11 []
    let pkg_bar_20 = PackageIdentifier (PackageName "bar") (Version [2,0] [])
        cfg_bar_20a = PackageConfig pkg_bar_20 [(f1, True)]
        cfg_bar_20b = PackageConfig pkg_bar_20 [(f1, False)]
        f1 = PD.FlagName "new_base"
    addConstraint (WantPkgId pkg_foo_11)
    addConstraint (HasConfigs pkg_foo_11 [[]] Nothing)
    addConstraint (OnlyOneConfig pkg_foo_11 [[]] Nothing)
    addConstraint (WantPkgId pkg_bar_20)
    addConstraint (HasConfigs pkg_bar_20 [[(f1, True)], [(f1, False)]] Nothing)
    addConstraint (OnlyOneConfig pkg_bar_20 [[(f1, True)], [(f1, False)]] Nothing)
-}
    mbSol <- solve 
    case mbSol of
      Nothing -> liftIO $ putStrLn "No solution"
      Just sol -> do
        cs <- getConstraints
        liftIO $ putStrLn $ unlines $
          map (renderConstraint (`M.lookup` sol)) cs


[-u, +a, -b, -c]
[-u]
