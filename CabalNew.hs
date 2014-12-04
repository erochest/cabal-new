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
import           CabalNew.Common
import           CabalNew.Files
import           CabalNew.Git
import           CabalNew.Opts
import           CabalNew.Types
import           CabalNew.Yesod

default (T.Text)


main :: IO ()
main = do
    config@CabalNew{..} <- execParser opts
    shelly $ verbosely $ do
        rootDir <- configDir projectRootDir
        let config'    = config { projectRootDir = FS.encodeString rootDir }
            projectDir = rootDir </> T.pack projectName

        mkdir_p projectDir
        chdir projectDir $ do
            init config

            patch <- case projectTarget of
                Executable -> cabalProject config' projectDir
                Library    -> cabalProject config' projectDir
                Yesod      -> yesodProject config' projectDir
                GhcJs      -> cabalProject config' projectDir

            withCommit projectGitLevel "apply hs project" $ do
                patch
                patchProject config'

            sandbox config'
            publish privateProject projectGitLevel $ T.pack projectSynopsis

        when projectTmuxifier $
            tmuxLayout config

        echo "done."
