{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CabalNew.Templates
    ( stubProgram
    , templateTo
    , templateFile
    , appendTemplate
    , copyDataFile
    ) where


import qualified Data.ByteString.Lazy      as BL
import           Data.Text
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Text.Lazy            as TL
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude
import qualified Prelude                   as P
import           Shelly
import           Text.Hastache
import           Text.Hastache.Context

import           CabalNew.Cabal
import           CabalNew.Git
import           CabalNew.Types
import           Paths_cabal_new


stubProgram :: GitLevel -> Bool -> String -> String -> Sh ()
stubProgram gitLevel isExecutable projectName mainFile = when isExecutable $
    withCommit gitLevel "Added stub main file." $ do
        copyDataFile "templates/Main.hs" $ FS.decodeString mainFile
        setMainIs cabalFile mainFile
    where cabalFile = FS.decodeString projectName FS.<.> "cabal"

templateTo :: CabalNew -> FS.FilePath -> (Text -> Sh ()) -> Sh ()
templateTo cabalNew dataFileName f = do
    dataFile <- liftIO . getDataFileName $ FS.encodeString dataFileName
    f . TL.toStrict =<<
        liftIO (hastacheFile defaultConfig dataFile $ mkGenericContext cabalNew)

templateFile :: CabalNew -> FS.FilePath -> FS.FilePath -> Sh ()
templateFile cabalNew dataFileName outputFileName =
    templateTo cabalNew dataFileName $ writefile outputFileName

appendTemplate :: CabalNew -> FS.FilePath -> FS.FilePath -> Sh ()
appendTemplate cabalNew dataFileName appendFileName =
    templateTo cabalNew dataFileName $ appendfile appendFileName

copyDataFile :: FS.FilePath -> FS.FilePath -> Sh ()
copyDataFile dataFileName outputFileName = do
    dataFile <- FS.decodeString <$> liftIO (getDataFileName dataFileName')
    cp dataFile outputFileName
    where dataFileName' = FS.encodeString dataFileName
