{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Sandbox
    ( sandboxInit
    , sandboxProject
    ) where


import           ClassyPrelude
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly

import           CabalNew.Cabal
import           CabalNew.Git
import           CabalNew.Templates
import           CabalNew.Types
import           CabalNew.Utils


sandboxInit :: CabalNew -> Sh ()
sandboxInit config = do
    templateFile config "templates/Makefile.sandbox.mustache" "Makefile"
    copyDataFile "templates/Main.sandbox.hs"
        . (FS.<.> "hs")
        . FS.decodeString
        . toTitleCase True
        $ projectName config
    run_ "make" ["init"]

sandboxProject :: CabalNew -> FilePath -> Sh (Sh ())
sandboxProject config projectDir = do
    withCommit (projectGitLevel config) "sandbox init" $
        sandboxInit config
    return $ do
        templateFile config "templates/env.mustache" ".env"
        when (projectGitLevel config == GitHere) $
            copyDataFile "templates/ctags" ".git/hooks/ctags"
        run "stylish-haskell" ["--defaults"] >>= writefile ".stylish-haskell.yaml"
