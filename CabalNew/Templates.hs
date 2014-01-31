{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CabalNew.Templates
    ( execStub
    , stubProgram
    , templateFile
    ) where


import qualified Data.ByteString.Lazy      as B
import           Data.Text
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


execStub :: Text
execStub = "\
    \{-# LANGUAGE OverloadedStrings #-}\n\n\
    \module Main where\n\n\
    \main :: IO ()\n\
    \main = undefined\n\n"

stubProgram :: Bool -> String -> String -> Sh ()
stubProgram isExecutable projectName mainFile = when isExecutable $
    withCommit "Added stub main file." $ do
        writefile mainPath execStub
        setMainIs cabalFile mainFile
    where mainPath  = FS.decodeString mainFile
          cabalFile = FS.decodeString projectName FS.<.> "cabal"

templateFile :: CabalNew -> P.FilePath -> P.FilePath -> IO ()
templateFile cabalNew dataFileName outputFileName = do
    dataFile <- getDataFileName dataFileName
    output   <- hastacheFile defaultConfig dataFile $ mkGenericContext cabalNew
    B.writeFile outputFileName output
