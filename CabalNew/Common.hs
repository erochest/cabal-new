{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}


module CabalNew.Common
    ( init
    , patchProject
    , tmuxLayout
    ) where


import           Data.Monoid
import           Filesystem                (getHomeDirectory)
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude                   hiding (FilePath, init)
import           Shelly

import           CabalNew.Cabal
import           CabalNew.Git
import           CabalNew.Templates
import           CabalNew.Types


init :: CabalNew -> Sh ()
init config = git_ (projectGitLevel config) "init" []

patchProject :: CabalNew -> Sh ()
patchProject config@CabalNew{..} = do
    templateFile config "templates/README.md.mustache" "README.md"
    templateFile config "templates/env.mustache" ".env"
    when (projectGitLevel == GitHere) $
        copyDataFile "templates/ctags" ".git/hooks/ctags"
    run "stylish-haskell" ["--defaults"] >>= writefile ".stylish-haskell.yaml"
    unless privateProject $
        appendTemplate config "templates/repo.cabal.mustache" cabalFile
    where cabalFile = FS.decodeString projectName FS.<.> "cabal"

tmuxLayout :: CabalNew -> Sh ()
tmuxLayout config@CabalNew{..} = do
    tmuxLayouts <-  (</> (".tmux-layouts" :: FilePath))
                <$> liftIO getHomeDirectory
    echo $ "Copying tmuxifier layout to " <> toTextIgnore tmuxLayouts
    templateFile config "templates/tmux-layouts.window.sh.mustache"
        . (tmuxLayouts </>)
        . FS.decodeString
        $ projectName ++ ".window.sh"
