{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Templates
    ( execStub
    , stubProgram
    ) where


import           Data.Text
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly

import           CabalNew.Cabal
import           CabalNew.Git


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
