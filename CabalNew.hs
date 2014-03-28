{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           ClassyPrelude             hiding ((</>), (<>))
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative       (execParser)
import           Shelly

import           CabalNew.Cabal
import           CabalNew.Files
import           CabalNew.Git
import           CabalNew.Opts
import           CabalNew.Templates
import           CabalNew.Types
import           CabalNew.Utils

default (T.Text)


main :: IO ()
main = do
    config@CabalNew{..} <- execParser opts
    shelly $ verbosely $ do
        rootDir <- configDir projectRootDir
        let config'    = config { projectRootDir = FS.encodeString rootDir }
            projectDir = rootDir </> T.pack projectName
            mainFile   = toTitleCase True projectName ++ ".hs"

        mkdir_p projectDir
        chdir projectDir $ do
            init config
            patchProject config'
            stubProgram projectGitLevel projectExecutable projectName mainFile
            sandbox
            publish privateProject projectGitLevel $ T.pack projectSynopsis
        echo "done."

init :: CabalNew -> Sh ()
init config = do
    git_ gitLevel "init" []
    withCommit gitLevel "cabal init" (cabalInit config)
    where gitLevel = projectGitLevel config

patchProject :: CabalNew -> Sh ()
patchProject config@CabalNew{..} = withCommit projectGitLevel "apply hs project" $ do
    templateFile config "templates/README.md.mustache" "README.md"
    templateFile config "templates/env.mustache" ".env"
    templateFile config "templates/Guardfile.mustache" "Guardfile"
    unless (projectGitLevel == Gitless) $
        copyDataFile "templates/gitignore" ".gitignore"
    when (projectGitLevel == GitHere) $
        copyDataFile "templates/ctags" ".git/hooks/ctags"
    mkdir_p "specs"
    templateFile config "templates/Specs.hs.mustache" "specs/Specs.hs"
    appendTemplate config "templates/specs.cabal.mustache" cabalFile
    run "stylish-haskell" ["--defaults"] >>= writefile ".stylish-haskell.yaml"
    unless privateProject $
        appendTemplate config "templates/repo.cabal.mustache" cabalFile
    where cabalFile = FS.decodeString projectName FS.<.> "cabal"

