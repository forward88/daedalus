{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module MacInstaller
    ( main
    , SigningConfig(..)
    , signingConfig
    , signInstaller
    , importCertificate
    , deleteCertificate
    , run
    , run'
    ) where

---
--- An overview of Mac .pkg internals:    http://www.peachpit.com/articles/article.aspx?p=605381&seqNum=2
---

import           Universum

import           Control.Monad (unless)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, renameFile)
import           System.FilePath ((</>), FilePath)
import           System.FilePath.Glob (glob)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
import qualified Filesystem.Path as P
import           Turtle (Shell, ExitCode (..), echo, proc, procs, inproc, which, Managed, with, chmod, writable, printf, format, (%), l, pwd, cd, sh, mktree)
import           Turtle.Line (unsafeTextToLine)

import           RewriteLibs (chain)

import           System.IO (hSetBuffering, BufferMode(NoBuffering))

import           Config
import           Types



main :: Options -> IO ()
main opts@Options{..} = do
  hSetBuffering stdout NoBuffering

  let appRoot = "../release/darwin-x64/Daedalus-darwin-x64/Daedalus.app"

  echo "Generating configuration file:  launcher-config.yaml"
  generateConfig (ConfigRequest Macos64 oCluster Launcher) "./dhall" "launcher-config.yaml"
  echo "Generating configuration file:  wallet-topology.yaml"
  generateConfig (ConfigRequest Macos64 oCluster Topology) "./dhall" "wallet-topology.yaml"

  tempInstaller <- makeInstaller opts appRoot

  signInstaller signingConfig (toText tempInstaller) oOutput
  checkSignature oOutput

  run "rm" [toText tempInstaller]
  echo $ "Generated " <> unsafeTextToLine oOutput

  when (oTestInstaller == TestInstaller) $ do
    echo $ "--test-installer passed, will test the installer for installability"
    procs "sudo" ["installer", "-dumplog", "-verbose", "-target", "/", "-pkg", oOutput] empty

makeScriptsDir :: Options -> Managed T.Text
makeScriptsDir Options{..} = case oBackend of
  Cardano _ -> pure "data/scripts"
  Mantis    -> pure "[DEVOPS-533]"

npmPackage :: Shell ()
npmPackage = do
  mktree "release"
  echo "~~~ Installing nodejs dependencies..."
  procs "npm" ["install"] empty
  export "NODE_ENV" "production"
  echo "~~~ Running electron packager script..."
  procs "npm" ["run", "package"] empty
  size <- inproc "du" ["-sh", "release"] empty
  printf ("Size of Electron app is " % l % "\n") size

withDir :: P.FilePath -> IO a -> IO a
withDir d = bracket (pwd >>= \old -> (cd d >> pure old)) cd . const

makeInstaller :: Options -> FilePath -> IO FilePath
makeInstaller opts@Options{..} appRoot = do
  let dir     = appRoot </> "Contents/MacOS"
      resDir  = appRoot </> "Contents/Resources"
  createDirectoryIfMissing False "dist"

  echo "Creating icons ..."
  procs "iconutil" ["--convert", "icns", "--output", "icons/electron.icns"
                   , "icons/electron.iconset"] mempty

  withDir ".." . sh $ npmPackage

  echo "~~~ Preparing files ..."
  case oBackend of
    Cardano bridge -> do
      -- Executables
      forM ["cardano-launcher", "cardano-node"] $ \f -> do
        copyFile (bridge </> "bin" </> f) (dir </> f)
        chmod writable (decodeString $ dir </> f)

      -- Config files (from daedalus-bridge)
      copyFile (bridge </> "config/configuration.yaml") (dir </> "configuration.yaml")
      copyFile (bridge </> "config/log-config-prod.yaml") (dir </> "log-config-prod.yaml")

      -- Genesis (from daedalus-bridge)
      genesisFiles <- glob "*genesis*.json"
      procs "cp" (fmap toText (genesisFiles <> [dir])) mempty

      -- Config yaml (generated from dhall files)
      copyFile "launcher-config.yaml" (dir </> "launcher-config.yaml")
      copyFile "wallet-topology.yaml" (dir </> "wallet-topology.yaml")

      -- SSL
      copyFile "build-certificates-unix.sh" (dir </> "build-certificates-unix.sh")
      copyFile "ca.conf"     (dir </> "ca.conf")
      copyFile "server.conf" (dir </> "server.conf")
      copyFile "client.conf" (dir </> "client.conf")

      -- Rewrite libs paths and bundle them
      void $ chain dir $ fmap toText [dir </> "cardano-launcher", dir </> "cardano-node"]

    Mantis -> pure () -- DEVOPS-533

  -- Prepare launcher
  de <- doesFileExist (dir </> "Frontend")
  unless de $ renameFile (dir </> "Daedalus") (dir </> "Frontend")
  run "chmod" ["+x", toText (dir </> "Frontend")]
  writeLauncherFile dir

  with (makeScriptsDir opts) $ \scriptsDir -> do
    let
      pkgargs :: [ T.Text ]
      pkgargs =
           [ "--identifier"
           , "org."<> fromAppName oAppName <>".pkg"
           -- data/scripts/postinstall is responsible for running build-certificates
           , "--scripts", scriptsDir
           , "--component"
           , T.pack appRoot
           , "--install-location"
           , "/Applications"
           , "dist/temp.pkg"
           ]
    run "ls" [ "-ltrh", scriptsDir ]
    run "pkgbuild" pkgargs

  run "productbuild" [ "--product", "data/plist"
                     , "--package", "dist/temp.pkg"
                     , "dist/temp2.pkg"
                     ]

  run "rm" ["dist/temp.pkg"]
  pure "dist/temp2.pkg"

writeLauncherFile :: FilePath -> IO FilePath
writeLauncherFile dir = do
  writeFile path $ unlines contents
  run "chmod" ["+x", toText path]
  pure path
  where
    path = dir </> "Daedalus"
    contents =
      [ "#!/usr/bin/env bash"
      , "cd \"$(dirname $0)\""
      , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Secrets-1.0\""
      , "mkdir -p \"$HOME/Library/Application Support/Daedalus/Logs/pub\""
      , "./cardano-launcher"
      ]

data SigningConfig = SigningConfig
  { signingIdentity         :: T.Text
  , signingKeyChain         :: Maybe T.Text
  , signingKeyChainPassword :: Maybe T.Text
  } deriving (Show, Eq)

signingConfig :: SigningConfig
signingConfig = SigningConfig
  { signingIdentity = "Developer ID Installer: Input Output HK Limited (89TW38X994)"
  , signingKeyChain = Nothing
  , signingKeyChainPassword = Nothing
  }

-- | Runs "security import -x"
importCertificate :: SigningConfig -> FilePath -> Maybe Text -> IO ExitCode
importCertificate SigningConfig{..} cert password = do
  let optArg s = map toText . maybe [] (\p -> [s, p])
      certPass = optArg "-P" password
      keyChain = optArg "-k" signingKeyChain
  productSign <- optArg "-T" . fmap (toText . encodeString) <$> which "productsign"
  let args = ["import", toText cert, "-x"] ++ keyChain ++ certPass ++ productSign
  -- echoCmd "security" args
  proc "security" args mempty

--- | Remove our certificate from the keychain
deleteCertificate :: SigningConfig -> IO ExitCode
deleteCertificate SigningConfig{..} = run' "security" args
  where
    args = ["delete-certificate", "-c", signingIdentity] ++ keychain
    keychain = maybe [] pure signingKeyChain

-- | Creates a new installer package with signature added.
signInstaller :: SigningConfig -> T.Text -> T.Text -> IO ()
signInstaller SigningConfig{..} src dst =
  run "productsign" $ sign ++ keychain ++ [ src, dst ]
  where
    sign = [ "--sign", signingIdentity ]
    keychain = maybe [] (\k -> [ "--keychain", k]) signingKeyChain

-- | Use pkgutil to verify that signing worked.
checkSignature :: T.Text -> IO ()
checkSignature pkg = run "pkgutil" ["--check-signature", pkg]

-- | Print the command then run it. Raises an exception on exit
-- failure.
run :: T.Text -> [T.Text] -> IO ()
run cmd args = do
    echoCmd cmd args
    procs cmd args mempty

-- | Print the command then run it.
run' :: T.Text -> [T.Text] -> IO ExitCode
run' cmd args = do
    echoCmd cmd args
    proc cmd args mempty

echoCmd :: T.Text -> [T.Text] -> IO ()
echoCmd cmd args = echo . unsafeTextToLine $ T.intercalate " " (cmd : args)
