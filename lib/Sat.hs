{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--module Sat where

import Minisat
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
                              Dependency(..), packageName )
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

{-
  Goal: we want a list of installation tasks

   - Install fresh package X-3 with flags [+f1,-f2]
   - Install fresh package Y-2 with flags [-f3]
   - Use installed/cached package Z-3-<hash123>

  Each package has a bunch of pre-conditions that must be satisfied.

   - Package Z-1 must be available; this can be satisfied by specifying
     the installed package-id of a package or referring to an earlier
     installation task.
   - Build tool B-3 must be available.

  

  Package Requirements:
 
    reqs :: PkgName -> Version -> FlagAssignment -> [Requirements]

-}


-- PackageId = PackageName + Version

data PackageConfig = PackageConfig
  { pkgCfgPackageId :: !PackageId
  , pkgCfgFlags     :: !FlagAssignment
  } deriving (Eq, Ord, Show)

type PackageCandidates = Dependency  -- package name + version range

--  InstalledPackageId = item in the package DB, a way to refer to a
--  PackageConfig that has been installed in a DB

data CabalEnv

availableVersions :: CabalEnv -> PackageName -> IO [PackageId]
availableVersions = undefined

packageConfigs :: CabalEnv -> PackageId -> IO [PackageConfig]
packageConfigs = undefined

data Constraint
  = DependsOn !PackageConfig !PackageName [Version]
  | HasConfigs !PackageId [PackageConfig]
  | OnlyOneConfig [PackageConfig]
  | OnlyOneVersion PackageName [Version]
  | Want !PackageId
  deriving (Eq, Ord, Show)


{-

Constraint Language
-------------------

  dependsOn(<PackageConfig>,  <Package, [Version]>)
    = dependsOn1(<Package, Version, Flags>, <Package, Version>, 

-}

data Goal
  = HavePackage PackageName Version




data Solution
  = UseInstalled InstalledPackageId
    -- Constraints: same dependencies as installed
  | InstallPackage PackageName PackageVersion
    -- 

-- | Fully describes a package to be installed.  It describes configuration
-- options and the full dependency DAG.  If we canonicalise and hash these we can
-- create keys based on which we can hash build results.
data UniquePackageId = UniquePackageId
  { upiName       :: !PackageName
  , upiVersion    :: !Version
  , upiBuildFlags :: !FlagAssignment
  , upiDepends    :: [UniquePackageId]
  } deriving (Eq, Ord, Show)




-- main = do
--   runMinisatM $ do
--     xs <- forM [1..10000] $ \i -> do
--       mkLit <$> freshVar
--     -- x <- mkLit <$> freshVar
--     -- y <- mkLit <$> freshVar
--     addClause (take 20 xs)
--     -- liftIO . print =<< addClause [x, y]
--     -- liftIO . print =<< solve True [x, y]
--     -- liftIO . print =<< solve True [negLit x, negLit y]

-- main = do
--   runMinisatM $ do
--     v1 <- freshVar
--     v2 <- freshVar
--     v3 <- freshVar
--     addClause [no v1, no v2]
--     addClause [no v1, no v3]
--     addClause [no v2, no v3]
--     addClause [yes v1, yes v2, yes v3]
--     liftIO . print =<< solve True []
--     forM_ [v1, v2, v3] $ \v -> do
--       x <- modelValue v
--       liftIO $ print (v, x)




main = do
  let verbosity = normal
  let pkgDbStack = [GlobalPackageDB, UserPackageDB]
  (compiler, _optPlatform, programConfig)
      <- configure verbosity Nothing Nothing  defaultProgramConfiguration

  putStrLn "Reading databases..."
  idx <- Idx.convert <$> GHC.getInstalledPackages verbosity pkgDbStack programConfig
  print (length (allPackages idx))

  let Just repoURI = parseURI "http://hackage.haskell.org"
  let repo = Repo{ repoKind = Left $ RemoteRepo "hackage.haskell.org" repoURI
                 , repoLocalDir = "/Users/nominolo/.cabal/packages/hackage.haskell.org/"
                 }
  
  srcdb <- getSourcePackages verbosity [repo]
  let srcIdx = packageIndex srcdb
      env0 = SolverEnv{ seCompilerId = compilerId compiler
                      , seInstalledPackages = idx
                      , seAvailablePackages = srcIdx
                      }
   
  putStrLn "Solving..."
  runS env0 $ do
    --- let dep = Dependency (PackageName "template-haskell") anyVersion
    let dep = Dependency (PackageName "aeson") (laterVersion $ Version [0, 7] [])
    -- let dep = Dependency (PackageName "primitive") (laterVersion $ Version [0, 5] [])
    vs <- want1 dep
    -- We want at least one of the packages that satisfy this
    logM $ satColour $ show (map yes vs)
    minisat $ addClause' (map yes vs)

    tasks <- gets ssAddedConstraints
    let pkgCandidates = M.fromListWith (++)
          [ (packageName (tiPkgId ti), vars)
          | (ti, vars) <- M.toList tasks
          ]
    forM_ (M.toList pkgCandidates) $ \(pkg, vars) -> do
      atMostOne vars

    ok <- minisat $ solve True []
    liftIO $ putStrLn $ "Solvable: " <> show ok

    -- forM_ (M.toList tasks) $ \(ti, vars) -> do
      
    --liftIO $ putStrLn $ unlines $ showTaskItems tasks
    forM (M.toList tasks) $ \(ti, vs) -> do
      oks <- mapM (minisat . modelValue) vs
      logM $ showTi ti <> " -> " <> 
               (intercalate ", " [ showDep v {- <> "=" <> showLBool val -}
                                 | (v, val@(Just True)) <- zip vs oks ])

showLBool :: Maybe Bool -> String
showLBool Nothing      = "?"
showLBool (Just True)  = "T"
showLBool (Just False) = "F"
    -- forM_ (allPackages idx) $ \pkg -> do
    --   genConstrsInstalledPackage pkg
{-
  let availIdx = packageIndex srcdb
  -- (availIdx, deps)
  --     <- readRepoIndex verbosity repo ReadPackageIndexStrict
  print (length (PI.allPackages availIdx))
  mapM_ (print . packageInfoId) (PI.lookupPackageName availIdx (PackageName "lens"))
-}

data Node
  = Existing InstalledPackage
  | Available SourcePackage

data Item
  = Installed !InstalledPackageId
  | Avail !PackageId PD.FlagAssignment
  deriving (Eq, Ord, Show)

data TaskItem
  = TiInstalled{ tiPkgId :: !PackageId, tiInstPkgId :: !InstalledPackageId }
  | TiSource { tiPkgId :: !PackageId }
  deriving (Eq, Ord, Show)

data SolverEnv = SolverEnv
  { seCompilerId        :: CompilerId
  , seInstalledPackages :: PackageIndex InstalledPackage
  , seAvailablePackages :: PackageIndex SourcePackage
  }

data SolverState = SolverState
  { ssPkgIdToVar :: M.Map Item Var
  , ssAddedConstraints :: M.Map TaskItem [Var]
  }

addClause' :: [Lit] -> MinisatM ()
addClause' ls = do
  ok <- addClause ls
  unless ok $ error "Unsat"

newtype S a = S (ReaderT SolverEnv (StateT SolverState MinisatM) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SolverState, MonadReader SolverEnv)

runS :: SolverEnv -> S a -> IO a
runS env (S m) = runMinisatM $ evalStateT (runReaderT m env) s0
  where s0 = SolverState M.empty M.empty

minisat :: MinisatM a -> S a
minisat m = S (lift (lift m))

markAsProcessed :: TaskItem -> [Var] -> S ()
markAsProcessed i vars = do
  s <- get
  put $! s{ ssAddedConstraints = M.insert i vars (ssAddedConstraints s) }

getProcessed :: TaskItem -> S (Maybe [Var])
getProcessed i = do
  s <- gets ssAddedConstraints
  return $! M.lookup i s

packageVar :: Item -> S Var
packageVar pkgId = do
  s <- get
  case M.lookup pkgId (ssPkgIdToVar s) of
    Just v  -> return v
    Nothing -> do
      v <- minisat freshVar
      --liftIO $ print (v, pkgId)
      put $! s{ ssPkgIdToVar = M.insert pkgId v (ssPkgIdToVar s) }
      return v

implies :: Var -> Var -> S ()
implies v1 v2 = do
  --liftIO $ print [negLit (mkLit v1), mkLit v2]
  logM $ satColour $ show v1 <> " => " <> show v2
  minisat $ addClause' [negLit (mkLit v1), mkLit v2]

impliesAny :: Var -> [Var] -> S ()
impliesAny v1 vs = do
  logM $ satColour $ show v1 <> " => " <> intercalate " || " (map show vs)
  minisat $ addClause' (negLit (mkLit v1) : map mkLit vs)

showTaskItems :: M.Map TaskItem [Var] -> [String]
showTaskItems m =
  [ showTi ti <> " -> [" <> intercalate "," (map showDep vs) <> "]"
  | (ti, vs) <- M.toList m ]

showTi (TiInstalled _ ipi) = display ipi
showTi (TiSource pid) = display pid

{-
genConstrsInstalledPackage :: IPI.InstalledPackageInfo -> S ()
genConstrsInstalledPackage ipi = do
  let pkgId = packageId ipi
  pvar <- packageVar (Avail pkgId)
  let deps = IPI.depends ipi
  forM_ deps $ \dep -> do
    dvar <- packageVar (Installed dep)
    ok <- implies pvar dvar 
    when (not ok) $ error "Unsat"
  return ()
-}

data FlagAssignment = FlagAssignment !String !Bool

data PlanItem
  = UseInstalled !InstalledPackageId
  | InstallPackage !PackageId [FlagAssignment]

logM :: String -> S ()
--logM _ = return ()
logM s = liftIO $ putStrLn s

satColour :: String -> String
satColour s = "\x1b[32m" <> s <>  "\x1b[0m"

showDep :: Var -> String
showDep v = satColour (show v)

addConstraintsInstalledPackage :: IPI.InstalledPackageInfo -> S [Var]
addConstraintsInstalledPackage info = do
  let iPkgId = IPI.installedPackageId info
      pkgId = IPI.sourcePackageId info
  mb_vars <- getProcessed (TiInstalled pkgId iPkgId)
  case mb_vars of
    Just vars -> return vars
    Nothing -> do
      pkgVar <- packageVar $! Installed iPkgId
      markAsProcessed (TiInstalled pkgId iPkgId) [pkgVar]

      depVars <- mapM (packageVar . Installed) (IPI.depends info)
      logM $ display pkgId <> "{" <> showDep pkgVar <> "} => [" <>
        (intercalate ", " [ display depId <> "{" <> showDep depVar <> "}"
                          | (depId, depVar) <- zip (IPI.depends info) depVars ])
        <> "]"
      forM_ depVars $ \depVar -> implies pkgVar depVar
      return [pkgVar]

want1 :: Dependency -> S [Var]
want1 dep@(Dependency pkgName versionRange) = do
  SolverEnv _ installedPackages sourcePackages <- ask
  is <- case lookupDependency installedPackages dep of
          [] -> return []  -- TODO: do something?
          installed -> do
            --logM $ "Installed: " <> display pkgName <> " " <> show (length installed)
            forM installed $ \(InstalledPackage instPkgInfo deps) -> do
              vs <- addConstraintsInstalledPackage instPkgInfo
              mapM wantPackageId deps
              return vs

  ns <- case lookupDependency sourcePackages dep of
          [] -> return []  -- TODO: Print warning if package cannot be reinstalled
          sources 
            | pkgName == PackageName "base"  -- Cannot reinstall base
            -> return []

            | otherwise
            -> forM sources $ \source -> do
                 addConstraintsSourcePackage source

  let vars = concat $ is ++ ns
  return vars

wantPackageId :: PackageId -> S [Var]
wantPackageId pkgId = do
  installedPackages <- asks seInstalledPackages
  case lookupPackageId installedPackages pkgId of
    Nothing -> return []   -- TODO: Probably should mark original package as broken
    Just (InstalledPackage instPkgInfo deps) -> do
      var <- addConstraintsInstalledPackage instPkgInfo
      concat <$> mapM wantPackageId deps

addConstraintsSourcePackage :: SourcePackage -> S [Var]
addConstraintsSourcePackage srcPkg = do
  compiler <- asks seCompilerId
  let pkgId = packageInfoId srcPkg
  mb_vars <- getProcessed (TiSource pkgId)
  case mb_vars of
    Just vars -> return vars
    Nothing -> do

      let flags0 = PD.genPackageFlags (packageDescription srcPkg)
      logM $ "Package: " <> display pkgId <> " Flags: [" <>
        showFlagAssignment [ (PD.flagName f, PD.flagDefault f) | f <- flags0 ] <> "]"

      let flagss = flagAssignments flags0

      pkgVars <- mapM (packageVar . Avail pkgId) flagss
      markAsProcessed (TiSource pkgId) pkgVars

      forM (zip pkgVars flagss) $ \(pkgVar, flags) -> do

        logM $ display pkgId <> ":" <> showFlagAssignment flags <>
               "{" <> showDep pkgVar <> "}"

        -- TODO: If a flag is flexible but would not change the dependencies, pick
        -- the default value.
        let pd = configuredPackage compiler srcPkg flags
            deps = fromMaybe [] $ do
                     lib <- PD.library pd
                     let bi = PD.libBuildInfo lib
                     return $ PD.targetBuildDepends bi
        logM $ "..deps=" <> intercalate "," (map display deps)
             
        forM_ deps $ \dep ->
          impliesAny pkgVar =<< want1 dep

        return pkgVar


------------------------------------------------------------------------------

no :: Var -> Lit
no = negLit . mkLit

yes :: Var -> Lit
yes = mkLit

-- | Encode the constraint that only one of the variables may be true.
--
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
       minisat $ addClause' [no v, no v']
     go vs

showFlagAssignment :: PD.FlagAssignment -> String
showFlagAssignment flags =
  intercalate "," [ (if yes then "+" else "-") <> name
                  | (PD.FlagName name, yes) <- flags
                  ]

-- want :: [Dependency] -> S ()
-- want = mapM_ want1

flagAssignments :: [PD.Flag] -> [[(PD.FlagName, Bool)]]
flagAssignments flags =
  let choices = [ (name, if manual then [dflt] else
                    if dflt then [True, False] else [False, True] )
                | PD.MkFlag{ PD.flagName = name
                           , PD.flagDefault = dflt
                           , PD.flagManual = manual } <- flags ]
  in go choices
 where
   go [] = [[]]
   go ((n, cs) : rest) =
     [ (n, c) : other
     | c <- cs
     , other <- go rest ]
     

configuredPackage :: CompilerId -> SourcePackage -> PD.FlagAssignment -> PD.PackageDescription
configuredPackage compiler srcPkg flags =
  case PD.finalizePackageDescription flags (const True)
         buildPlatform compiler [] (packageDescription srcPkg) of 
    Left _ -> error "Unexpected configuredPackage failure"
    Right (pd, _flags) -> pd
