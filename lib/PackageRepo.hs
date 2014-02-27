module PackageRepo where

import PackageIndex
import Distribution.Version

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'SourcePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
readRepoIndex :: Verbosity -> Repo -> ReadPackageIndexMode
              -> IO (PackageIndex SourcePackage, [Dependency])
readRepoIndex verbosity repo mode =
  let indexFile = repoLocalDir repo </> "00-index.tar"
      cacheFile = repoLocalDir repo </> "00-index.cache"
  in handleNotFound $ do
    warnIfIndexIsOld indexFile
    whenCacheOutOfDate indexFile cacheFile $ do
      info verbosity "Updating the index cache file..."
      updatePackageIndexCacheFile indexFile cacheFile
    readPackageIndexCacheFile mkAvailablePackage indexFile cacheFile mode

  where
    mkAvailablePackage pkgEntry =
      SourcePackage {
        packageInfoId      = pkgid,
        packageDescription = packageDesc pkgEntry,
        packageSource      = case pkgEntry of
          NormalPackage _ _ _ _       -> RepoTarballPackage repo pkgid Nothing
          BuildTreeRef  _  _ _ path _ -> LocalUnpackedPackage path,
        packageDescrOverride = case pkgEntry of
          NormalPackage _ _ pkgtxt _ -> Just pkgtxt
          _                          -> Nothing
      }
      where
        pkgid = packageId pkgEntry

    handleNotFound action = catchIO action $ \e -> if isDoesNotExistError e
      then do
        case repoKind repo of
          Left  remoteRepo -> warn verbosity $
               "The package list for '" ++ remoteRepoName remoteRepo
            ++ "' does not exist. Run 'cabal update' to download it."
          Right _localRepo -> warn verbosity $
               "The package list for the local repo '" ++ repoLocalDir repo
            ++ "' is missing. The repo is invalid."
        return mempty
      else ioError e

    isOldThreshold = 15 --days
    warnIfIndexIsOld indexFile = do
      dt <- getFileAge indexFile
      when (dt >= isOldThreshold) $ case repoKind repo of
        Left  remoteRepo -> warn verbosity $
             "The package list for '" ++ remoteRepoName remoteRepo
          ++ "' is " ++ show dt ++ " days old.\nRun "
          ++ "'cabal update' to get the latest list of available packages."
        Right _localRepo -> return ()
