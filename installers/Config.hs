{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Config
  ( ConfigRequest, mkStubConfigRequest
  , checkAllConfigs
  , generateOSConfigs
  , OS(..), Cluster(..), Config(..)
  , optReadLower, argReadLower
  , Options(..), optionsParser
  , Command(..), commandParser
  -- Re-export Turtle:
  , options
  ) where

import qualified Control.Exception                as Ex
import           Control.Monad                       (forM_)

import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Optional                       (Optional)
import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text, pack, unpack, intercalate, toLower, unlines)
import qualified Data.Yaml                        as YAML

import qualified Dhall                            as Dhall
import qualified Dhall.JSON                       as Dhall

import qualified GHC.IO.Encoding                  as GHC

import qualified System.Environment               as Sys
import qualified System.IO                        as Sys
import qualified System.IO.Temp                   as Sys
import qualified System.Exit                      as Sys

import           Turtle                              (optional, format, (%), s)
import           Turtle.Options

import           Prelude                      hiding (unlines, writeFile)
import           Types



-- | Enum-instanced sum types as case-insensitive option values.
--
-- λ> data Foo = Bar | Baz deriving (Show, Enum, Bounded)
-- λ> let x = enumFromTo minBound maxBound :: [Foo]
-- λ> x
-- [Bar,Baz]
-- λ> fmap ((fmap toLower) . show) x
-- ["bar","baz"]
diagReadCaseInsensitive :: (Bounded a, Enum a, Read a, Show a) => String -> Maybe a
diagReadCaseInsensitive str = diagRead $ toLower $ pack str
  where mapping    = Map.fromList [ (lshowText x, x) | x <- enumFromTo minBound maxBound ]
        diagRead x = Just $ flip fromMaybe (Map.lookup x mapping)
                     (errorT $ format ("Couldn't parse '"%s%"' as one of: "%s)
                               (pack str) (intercalate ", " $ Map.keys mapping))

optReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optReadLower = opt (diagReadCaseInsensitive . unpack)
argReadLower :: (Bounded a, Enum a, Read a, Show a) => ArgName -> Optional HelpMessage -> Parser a
argReadLower = arg (diagReadCaseInsensitive . unpack)

data ConfigRequest = ConfigRequest
  { cfrCardano           :: Text
  , cfrConfigFiles       :: Text
  , cfrDaedalusFrontend  :: Text
  , cfrDhallRoot         :: Text
  } deriving (Eq, Show)

renderConfigRequest :: ConfigRequest -> Text
renderConfigRequest ConfigRequest{..} = unlines
  [ "{ cardano          = \""<> cfrCardano          <> "\""
  , ", configFiles      = \""<> cfrConfigFiles      <> "\""
  , ", daedalusFrontend = \""<> cfrDaedalusFrontend <> "\""
  , "}"]

data Command
  = GenConfig ConfigRequest
  | GenInstaller
  deriving (Eq, Show)

data Options = Options
  { oAPI           :: API
  , oBuildJob      :: Maybe BuildJob
  , oCluster       :: Cluster
  , oAppName       :: AppName
  , oDaedalusVer   :: Version
  , oOutput        :: Text
  , oPullReq       :: Maybe PullReq
  , oTestInstaller :: TestInstaller
  , oCI            :: CI
  }

commandParser :: Parser Command
commandParser = (fromMaybe GenInstaller <$>) . optional $
  subcommandGroup "Subcommands:"
  [ ("config",     "Build configs for an OS",
      (GenConfig <$>) $
      ConfigRequest
       <$> (fromMaybe "/nix/store/HASH-cardano-sl.stub"
                      <$> (optional $ optText "cardano"            's' "Path to cardano-sl"))
       <*> (fromMaybe "/nix/store/HASH-config-files.stub"
                      <$> (optional $ optText "config-files"       'c' "Config files directory"))
       <*> (fromMaybe "/nix/store/HASH-daedalus-frontend.stub"
                      <$> (optional $ optText "daedalus-frontend"  'f' "Daedalus frontend directory"))
       <*> optText "dhall-root"  'r' "Directory containing Dhall config files")
  , ("installer",  "Build an installer",
      pure GenInstaller)
  ]

optionsParser :: Parser Options
optionsParser = Options
  <$> (fromMaybe Cardano <$> (optional $
                   optReadLower "api"                 'a' "Backend API:  cardano or etc"))
  <*> (optional      $
      (BuildJob     <$> optText "build-job"           'b' "CI Build Job/ID"))
  <*> (fromMaybe Mainnet    <$> (optional $
                   optReadLower "cluster"             'c' "Cluster the resulting installer will target:  mainnet or staging"))
  <*> (fromMaybe "daedalus" <$> (optional $
      (AppName      <$> optText "appname"             'n' "Application name:  daedalus or..")))
  <*> (fromMaybe "dev"   <$> (optional $
      (Version      <$> optText "daedalus-version"    'v' "Daedalus version string")))
  <*> (fromMaybe (error "--output not specified for 'installer' subcommand")
       <$> (optional $  optText "output"              'o' "Installer output file"))
  <*> (optional   $
      (PullReq      <$> optText "pull-request"        'r' "Pull request #"))
  <*> (testInstaller
                    <$> switch  "test-installer"      't' "Test installers after building")
  <*> pure Buildkite -- NOTE: this is filled in by auto-detection



dhallTopExpr :: ConfigRequest -> Text -> Config -> OS -> Cluster -> Text
dhallTopExpr ConfigRequest{..} installation cfg os cluster
  | Launcher <- cfg = format (s%" "%s%" ("%s%" "%s%" "%s%" )") (comp Launcher) (comp cluster) (comp os) (comp cluster) installation
  | Topology <- cfg = format (s%" "%s)                         (comp Topology) (comp cluster)
  where comp x = format (s%"/"%s%".dhall") cfrDhallRoot (lshowText x)

forOSConfigValues :: (Cluster -> Config -> YAML.Value -> IO a) -> ConfigRequest -> OS -> IO ()
forOSConfigValues action cfreq os = do
  tmpdir       <- fromMaybe "/tmp" <$> Sys.lookupEnv "TMPDIR"
  installation <- Sys.writeTempFile tmpdir "installation-dhall-" $ unpack $ renderConfigRequest cfreq
  sequence [ action cluster cfg =<<
             (Dhall.detailed $ Dhall.codeToValue "(stdin)" $ dhallTopExpr cfreq (pack installation) cfg os cluster)
           | cluster <- enumFromTo minBound maxBound
           , cfg     <- enumFromTo minBound maxBound ]
  pure ()

mkStubConfigRequest :: Text -> ConfigRequest
mkStubConfigRequest cfrDhallRoot = ConfigRequest
  { cfrCardano          = ""
  , cfrConfigFiles      = ""
  , cfrDaedalusFrontend = ""
  , .. }

checkAllConfigs :: Text -> IO ()
checkAllConfigs = forM_ (enumFromTo minBound maxBound) . forOSConfigValues (\_ _ _ -> pure ()) . mkStubConfigRequest

generateOSConfigs :: ConfigRequest -> OS -> IO ()
generateOSConfigs = forOSConfigValues $ \_ config val -> do
  GHC.setLocaleEncoding GHC.utf8
  BS.writeFile (configFilename config) $ YAML.encode val

-- | Generic error handler: be it encoding/decoding, file IO, parsing or type-checking.
handle :: IO a -> IO a
handle = Ex.handle handler
  where
    handler :: Ex.SomeException -> IO a
    handler e = do
        Sys.hPutStrLn Sys.stderr ""
        Sys.hPrint    Sys.stderr e
        Sys.exitFailure
