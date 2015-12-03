-- |
-- Module : Haskonf
-- Haskonf, a small library to configure Haskell applications in Haskell.
-- TODO: Make use of monad transformers to support non-IO-() signatures.
module Haskonf ( build,
                 buildForce,
                 rebuild,
                 buildDo,
                 defaultFlags,
                 defaultFlagsVerbose,
                 appDir,
                 binName,
                 runFrom,
                 copyReal,
                 copyConfig,
                 doesConfigExist ) where

import           Control.Exception.Extensible (SomeException (..), bracket, try)
import qualified Control.Exception.Extensible as E
import           Control.Monad                (filterM, when)
import           Control.Monad.Fix            (fix)
import           Data.List                    ((\\))
import           Data.Maybe                   (isJust)
import           System.Directory             (copyFile, doesDirectoryExist,
                                               doesFileExist,
                                               getAppUserDataDirectory,
                                               getDirectoryContents,
                                               getModificationTime)
import           System.Environment.FindBin   (getProgPath)
import           System.Exit                  (ExitCode (..))
import           System.FilePath              (takeDirectory, takeExtension,
                                               (</>))
import           System.Info                  (arch, os)
import           System.IO                    (IOMode (..), hClose, openFile)
import           System.Posix.Process         (executeFile, getAnyProcessStatus)
import           System.Posix.Signals         (Handler (..), installHandler,
                                               openEndedPipe, sigCHLD)
import           System.Process               (runProcess, waitForProcess)

-- |
-- Copies example file into app directory.
copyReal :: String -> FilePath -> IO ()
copyReal pname file = appDir pname >>= (copyFile file) . (flip (</>) $ pname ++ ".hs")

-- |
-- Copies config bundled with the application.
copyConfig :: String -> IO ()
copyConfig pname = do
  selfPath <- getProgPath
  let cfg = (takeDirectory selfPath) </> pname ++ ".hs"
  copyReal pname cfg

-- |
-- Checks if configuration source exists in the application directory.
doesConfigExist :: String -> IO Bool
doesConfigExist pname = do
  dir <- appDir pname
  let cfg = dir </> pname ++ ".hs"
  doesFileExist cfg

-- |
-- Primitive IoC function.
-- Takes three arguments — name of currently executed program,
-- path to the directory, containing target program and target
-- program's name. If current program's name and target program's
-- name is the same, does nothing (returns pure ()), else —
-- runs executeFile accordingly.
-- Fourth argument is arglist passed to top program.
runFrom :: String -> FilePath -> String -> [String] -> IO ()
runFrom x _ z _
  | x == z = pure ()
runFrom _ y z a = executeFile (y </> z) False a Nothing

-- |
-- Given application name, return path to application data
-- directory.
appDir :: String -> IO FilePath
appDir = getAppUserDataDirectory

-- |
-- Given application name, build underlying application, if
-- something was changed.
build :: String -> IO Bool
build pname = buildDo pname Nothing False

-- |
-- Given application name, forcefully rebuild underlying application.
buildForce :: String -> IO Bool
buildForce = rebuild

-- |
-- Alias for @buildForce@.
rebuild :: String -> IO Bool
rebuild pname = buildDo pname Nothing True

-- |
-- The most important function of haskonf.
-- Takes application name, ghc flags, force flag and returns status
-- of (re-)building of underlying application.
buildDo :: String -> Maybe [String] -> Bool -> IO Bool
buildDo pname Nothing   force = (flip . buildDo) pname force $ Just $ defaultFlags pname
buildDo pname (Just fs) force = do
  dir <- getAppUserDataDirectory pname
  let binn = binName pname
      binf = dir </> binn
      base = dir </> pname
      err  = base ++ ".errors"
      src  = base ++ ".hs"
      lib  = dir </> "lib"
  libTs <- mapM getModTime . Prelude.filter isSource =<< allFiles lib
  srcT <- getModTime src
  binT <- getModTime binf
  if force || any (binT <) (srcT : libTs)
    then do
      uninstallSignalHandlers
      status <- bracket (openFile err WriteMode) hClose $ \h -> waitForProcess =<< runProcess "ghc" fs (Just dir)
                                                                Nothing Nothing Nothing (Just h)
      installSignalHandlers
      return (status == ExitSuccess)
    else
      return True
  where
    getModTime f = E.catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
    isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
    allFiles t = do
      let prep = map (t</>) . Prelude.filter (`notElem` [".",".."])
      cs <- prep <$> E.catch (getDirectoryContents t) (\(SomeException _) -> return [])
      ds <- filterM doesDirectoryExist cs
      concat . ((cs \\ ds):) <$> mapM allFiles ds

-- |
-- Given application name, returns underlying application's binary name.
binName :: String -> String
binName pname = pname ++ "-" ++ arch ++ "-" ++ os

-- |
-- Given application name, returns default ghc flags.
defaultFlags :: String -> [String]
defaultFlags pname =  ["--make", pname ++ ".hs", "-i", "-ilib", "-fforce-recomp",
                       "-main-is", "main", "-v0", "-o", binName pname]

-- |
-- Given application name, returns default ghc flags with verbose output.
defaultFlagsVerbose :: String -> [String]
defaultFlagsVerbose pname =  ["--make", pname ++ ".hs", "-i", "-ilib", "-fforce-recomp",
                              "-main-is", "main", "-v", "-o", binName pname]

-- |
-- A pair of functions to ignore SIGPIPE to avoid termination when a pipe is full,
-- and SIGCHLD to avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: IO ()
installSignalHandlers = do
    _ <- installHandler openEndedPipe Ignore Nothing
    _ <- installHandler sigCHLD Ignore Nothing
    _ <- (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

-- |
-- A pair of functions to ignore SIGPIPE to avoid termination when a pipe is full,
-- and SIGCHLD to avoid zombie processes, and clean up any extant zombie processes.
uninstallSignalHandlers :: IO ()
uninstallSignalHandlers = do
    _ <- installHandler openEndedPipe Default Nothing
    _ <- installHandler sigCHLD Default Nothing
    return ()
