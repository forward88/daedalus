{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Config
  ( checkAllConfigs
  , generateConfig, generateOSConfigs
  , ConfigRequest(..)
  , OS(..), Cluster(..), Config(..)
  , optReadLower, argReadLower
  , Options(..), optionsParser
  , Command(..), commandParser
  -- Re-export Turtle:
  , options
  ) where

import qualified Control.Exception                as Ex
import           Control.Monad                       (forM_)

import           Data.ByteString                     (writeFile)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Optional                       (Optional)
import           Data.Text                           (Text, pack, unpack, intercalate, toLower)
import qualified Data.Yaml                        as YAML

import qualified Dhall                            as Dhall
import qualified Dhall.JSON                       as Dhall

import qualified GHC.IO.Encoding                  as GHC

import qualified System.IO                        as Sys
import qualified System.Exit                      as Sys

import           Turtle                              (optional, format, (%), s)
import           Turtle.Options

import           Prelude                     hiding (writeFile)
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

data Command
  = GenConfig
    { cmdDhallRoot :: Text
    }
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
      GenConfig
      <$> (argText "DIR" "Directory with Dhall config files"))
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
  <*>                   optText "output"              'o' "Installer output file"
  <*> (optional   $
      (PullReq      <$> optText "pull-request"        'r' "Pull request #"))
  <*> (testInstaller
                    <$> switch  "test-installer"      't' "Test installers after building")
  <*> pure Buildkite -- NOTE: this is filled in by auto-detection



dhallTopExpr :: Text -> Config -> OS -> Cluster -> Text
dhallTopExpr path Launcher os cluster = path <> "/launcher.dhall ( "<>path<>"/" <> lshowText cluster <> ".dhall "<>path<>"/" <> lshowText os <> ".dhall ) "<>path<>"/" <> lshowText os <> ".dhall"
dhallTopExpr path Topology os cluster = path <> "/topology.dhall ( "<>path<>"/" <> lshowText cluster <> ".dhall "<>path<>"/" <> lshowText os <> ".dhall )"

forOSConfigValues :: (Cluster -> Config -> YAML.Value -> IO a) -> Text -> OS -> IO ()
forOSConfigValues action configRoot os = sequence
  [ action cluster cfg =<<
    (Dhall.detailed $ Dhall.codeToValue "(stdin)" $ dhallTopExpr configRoot cfg os cluster)
  | cluster <- enumFromTo minBound maxBound
  , cfg     <- enumFromTo minBound maxBound ]
  >> pure ()

checkAllConfigs :: Text -> IO ()
checkAllConfigs = forM_ (enumFromTo minBound maxBound) .
                  forOSConfigValues (\_ _ _ -> pure ())

generateOSConfigs :: Text -> OS -> IO ()
generateOSConfigs = forOSConfigValues $ \_ config val -> do
  GHC.setLocaleEncoding GHC.utf8
  writeFile (configFilename config) $ YAML.encode val

generateConfig :: ConfigRequest -> FilePath -> FilePath -> IO ()
generateConfig ConfigRequest{..} configRoot outFile = handle $ do
  GHC.setLocaleEncoding GHC.utf8

  let inText = dhallTopExpr (pack configRoot) rConfig rOS rCluster

  writeFile outFile =<<
    YAML.encode <$> Dhall.detailed (Dhall.codeToValue "(stdin)" inText)

-- | Generic error handler: be it encoding/decoding, file IO, parsing or type-checking.
handle :: IO a -> IO a
handle = Ex.handle handler
  where
    handler :: Ex.SomeException -> IO a
    handler e = do
        Sys.hPutStrLn Sys.stderr ""
        Sys.hPrint    Sys.stderr e
        Sys.exitFailure
