{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CabalNew.Cabal
    ( cabal_
    , cabalInit
    , cabalSandbox_
    , setMainIs
    , sandbox
    , cabalProject
    ) where


import           ClassyPrelude             hiding (FilePath)
import qualified Data.Char                 as C
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly

import           CabalNew.Files
import           CabalNew.Git
import           CabalNew.Templates
import           CabalNew.Types
import           CabalNew.Utils


cabalCmd :: CabalNew -> FilePath
cabalCmd c | projectTarget c == GhcJs = "cabal-js"
           | otherwise                = "cabal"

cabal_ :: CabalNew -> T.Text -> [T.Text] -> Sh ()
cabal_ c = command1_ (cabalCmd c) []

cabalInit :: CabalNew -> Sh ()
cabalInit c@CabalNew{..} =
    cabal_ c "init" $ catMaybes [ Just  "--non-interactive"
                                , Just  "--is-library"
                                , Just  "--main-is=Main.hs"
                                , ifSet "license"  projectLicense
                                , ifSet "email"    projectEmail
                                , ifSet "synopsis" projectSynopsis
                                ]

cabalSandbox_ :: CabalNew -> T.Text -> [T.Text] -> Sh ()
cabalSandbox_ c cmdName args =
    command1_ (cabalCmd c) [] "sandbox" $ cmdName : args

setMainIs :: FilePath -> String -> Sh ()
setMainIs cabalPath mainFile = sed cabalPath $ \line ->
    if "-- main-is:" `T.isInfixOf` line
        then takeWhile C.isSpace line <> "main-is:             " <> T.pack mainFile
        else line

sandbox :: CabalNew -> Sh ()
sandbox c = do
    cabalSandbox_ c "init" []
    cabal_ c "install" ["-j", "--only-dependencies", "--enable-tests"]
    cabal_ c "configure" ["--enable-tests"]

cabalProject :: CabalNew -> FilePath -> Sh (Sh ())
cabalProject config@CabalNew{..} _projectDir = do
    let mainFile = "Main.hs"
        projectExecutable = projectTarget == Executable
    withCommit projectGitLevel "cabal init" $
        cabalInit config
    stubProgram projectGitLevel projectExecutable projectName mainFile
    when (projectTarget == GhcJs) $
        copyDataFile "templates/ghcjs.cabal.config" "cabal.config"
    return $ do
        copyDataFile "templates/ghci" ".ghci"
        unless (projectGitLevel == Gitless) $
            copyDataFile "templates/gitignore" ".gitignore"
        mkdir_p "specs"
        copyDataFile "templates/Specs.hs" "specs/Specs.hs"
        when (projectTarget == Executable) $
            appendTemplate config "templates/executable.cabal.mustache" cabalFile
        appendTemplate config "templates/specs.cabal.mustache" cabalFile
        where cabalFile = FS.decodeString projectName FS.<.> "cabal"
