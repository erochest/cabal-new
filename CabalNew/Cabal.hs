{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module CabalNew.Cabal
    ( cabal_
    , cabalInit
    , setMainIs
    , sandbox
    ) where


import           ClassyPrelude
import qualified Data.Char      as C
import qualified Data.Text      as T
import           Shelly

import           CabalNew.Files
import           CabalNew.Types
import           CabalNew.Utils

cabal_ :: T.Text -> [T.Text] -> Sh ()
cabal_ = command1_ "cabal" []

cabalInit :: CabalNew -> Sh ()
cabalInit CabalNew{..} =
    cabal_ "init" $ catMaybes [ Just   "--non-interactive"
                              , ifSet  "license"       projectLicense
                              , ifSet  "email"         projectEmail
                              , ifSet  "synopsis"      projectSynopsis
                              , ifTrue "is-library"    projectLibrary
                              , ifTrue "is-executable" projectExecutable
                              ]

setMainIs :: FilePath -> String -> Sh ()
setMainIs cabalPath mainFile = sed cabalPath $ \line ->
    if "-- main-is:" `T.isInfixOf` line
        then takeWhile C.isSpace line <> "main-is:             " <> T.pack mainFile
        else line

sandbox :: Sh ()
sandbox = cabal_ "sandbox" ["init"] >> run_ "sandbox-init" ["--enable-tests"]

