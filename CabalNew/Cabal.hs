{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CabalNew.Cabal
    ( cabal_
    , cabalInit
    , setMainIs
    , sandbox
    , cabalProject
    ) where


import           ClassyPrelude
import qualified Data.Char                 as C
import qualified Data.Text                 as T
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly

import           CabalNew.Files
import           CabalNew.Git
import           CabalNew.Templates
import           CabalNew.Types
import           CabalNew.Utils

cabal_ :: T.Text -> [T.Text] -> Sh ()
cabal_ = command1_ "cabal" []

cabalInit :: CabalNew -> Sh ()
cabalInit CabalNew{..} =
    cabal_ "init" $ catMaybes [ Just  "--non-interactive"
                              , Just  "--is-library"
                              , Just  "--main-is=Main.hs"
                              , ifSet "license"  projectLicense
                              , ifSet "email"    projectEmail
                              , ifSet "synopsis" projectSynopsis
                              ]

setMainIs :: FilePath -> String -> Sh ()
setMainIs cabalPath mainFile = sed cabalPath $ \line ->
    if "-- main-is:" `T.isInfixOf` line
        then takeWhile C.isSpace line <> "main-is:             " <> T.pack mainFile
        else line

sandbox :: Sh ()
sandbox = cabal_ "sandbox" ["init"] >> run_ "sandbox-init" ["--enable-tests"]

cabalProject :: CabalNew -> FilePath -> Sh (Sh ())
cabalProject config@CabalNew{..} _projectDir = do
    let mainFile = "Main.hs"
        projectExecutable = projectTarget == Executable
    withCommit projectGitLevel "cabal init" $
        cabalInit config
    stubProgram projectGitLevel projectExecutable projectName mainFile
    return $ do
        templateFile config "templates/Makefile-cabal.mustache" "Makefile"
        copyDataFile "templates/ghci" ".ghci"
        unless (projectGitLevel == Gitless) $
            copyDataFile "templates/gitignore" ".gitignore"
        mkdir_p "specs"
        copyDataFile "templates/Specs.hs" "specs/Specs.hs"
        when (projectTarget == Executable) $
            appendTemplate config "templates/executable.cabal.mustache" cabalFile
        appendTemplate config "templates/specs.cabal.mustache" cabalFile
        where cabalFile = FS.decodeString projectName FS.<.> "cabal"
