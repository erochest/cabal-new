{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           ClassyPrelude             hiding ((</>), (<>))
import           Data.Monoid
import qualified Data.Text                 as T
import           Filesystem                (getHomeDirectory)
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
            mainFile   = "Main.hs"

        mkdir_p projectDir
        chdir projectDir $ do
            init config
            patchProject config'
            stubProgram projectGitLevel projectExecutable projectName mainFile
            sandbox
            publish privateProject projectGitLevel $ T.pack projectSynopsis

        when projectTmuxifier $
            tmuxLayout config

        echo "done."

init :: CabalNew -> Sh ()
init config = do
    git_ gitLevel "init" []
    withCommit gitLevel "cabal init" (cabalInit config)
    where gitLevel = projectGitLevel config

patchProject :: CabalNew -> Sh ()
patchProject config@CabalNew{..} = withCommit projectGitLevel "apply hs project" $ do
    copyDataFile "templates/Makefile" "Makefile"
    copyDataFile "templates/project.vim" ".project.vim"
    copyDataFile "templates/ghci" ".ghci"
    templateFile config "templates/README.md.mustache" "README.md"
    templateFile config "templates/env.mustache" ".env"
    templateFile config "templates/Guardfile.mustache" "Guardfile"
    unless (projectGitLevel == Gitless) $
        copyDataFile "templates/gitignore" ".gitignore"
    when (projectGitLevel == GitHere) $
        copyDataFile "templates/ctags" ".git/hooks/ctags"
    mkdir_p "specs"
    copyDataFile "templates/Specs.hs" "specs/Specs.hs"
    when projectExecutable $
        appendTemplate config "templates/executable.cabal.mustache" cabalFile
    appendTemplate config "templates/specs.cabal.mustache" cabalFile
    run "stylish-haskell" ["--defaults"] >>= writefile ".stylish-haskell.yaml"
    unless privateProject $
        appendTemplate config "templates/repo.cabal.mustache" cabalFile
    where cabalFile = FS.decodeString projectName FS.<.> "cabal"

tmuxLayout :: CabalNew -> Sh ()
tmuxLayout config@CabalNew{..} = do
    tmuxLayouts <- (</> ".tmux-layouts") <$> liftIO getHomeDirectory
    echo $ "Copying tmuxifier layout to " <> toTextIgnore tmuxLayouts
    templateFile config "templates/tmux-layouts.window.sh.mustache"
        . (tmuxLayouts </>)
        . FS.decodeString
        $ projectName ++ ".window.sh"
