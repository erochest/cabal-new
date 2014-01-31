{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           ClassyPrelude       hiding ((</>), (<>))
import qualified Data.Text           as T
import           Options.Applicative (execParser)
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
    config <- execParser opts
    shelly $ verbosely $ do
        rootDir  <- configDir $ projectRootDir  config
        patchDir <- configDir $ projectPatchDir config
        let projectDir = rootDir </> T.pack (projectName config)
            mainFile   = toTitleCase True (projectName config) ++ ".hs"

        mkdir_p projectDir

        chdir projectDir $ do
            init config
            patchProject projectDir patchDir
            withCommit "README.md" $ touchfile "README.md"
            stubProgram (projectExecutable config) (projectName config) mainFile
            sandbox
            publish $ privateProject config
        echo "done."

init :: CabalNew -> Sh ()
init config = git_ "init" [] >> withCommit "cabal init" (cabalInit config)

patchProject :: FilePath -> FilePath -> Sh ()
patchProject projectDir patchDir = withCommit "apply hs project" $
    chdir patchDir $
        run_ (patchDir </> "apply-project") [toTextIgnore projectDir]

