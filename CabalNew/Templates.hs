{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Templates
    ( stubProgram
    , templateTo
    , templateFile
    , appendTemplate
    , copyDataFile
    ) where


import           Data.Text
import qualified Data.Text.Lazy            as TL
import qualified Filesystem.Path.CurrentOS as FS
import           Prelude
import           Shelly
import           Text.Hastache
import           Text.Hastache.Context

import           CabalNew.Git
import           CabalNew.Types
import           Paths_cabal_new


stubProgram :: GitLevel -> Bool -> String -> String -> Sh ()
stubProgram gitLevel isExecutable _projectName mainFile = when isExecutable $
    withCommit gitLevel "Added stub main file." $
        copyDataFile "templates/Main.hs" $ FS.decodeString mainFile

templateTo :: CabalNew -> FS.FilePath -> (Text -> Sh ()) -> Sh ()
templateTo cabalNew dataFileName f = do
    dataFile <- liftIO . getDataFileName $ FS.encodeString dataFileName
    f . TL.toStrict =<<
        liftIO (hastacheFile defaultConfig dataFile $ mkStrContext context)
    where
        context :: Monad m => String -> MuType m
        context "build"   = MuVariable $ case projectTarget cabalNew of
                                             Yesod -> "yesod" :: Text
                                             _     -> "${CABAL}"
        context "isyesod" = MuBool $ projectTarget cabalNew == Yesod
        context "isjs"    = MuBool $ projectTarget cabalNew == GhcJs
        context "projectRootDir"   = MuVariable $ projectRootDir cabalNew
        context "projectName"      = MuVariable $ projectName cabalNew
        context "projectGitLevel"  = MuVariable . show $ projectGitLevel cabalNew
        context "privateProject"   = MuBool $ privateProject cabalNew
        context "projectLicense"   = MuVariable $ projectLicense cabalNew
        context "projectEmail"     = MuVariable $ projectEmail cabalNew
        context "projectSynopsis"  = MuVariable $ projectSynopsis cabalNew
        context "projectCategory"  = MuVariable $ projectCategory cabalNew
        context "projectTarget"    = MuVariable . show $ projectTarget cabalNew
        context "projectTmuxifier" = MuBool $ projectTmuxifier cabalNew
        context _ = MuNothing

templateFile :: CabalNew -> FS.FilePath -> FS.FilePath -> Sh ()
templateFile cabalNew dataFileName outputFileName =
    templateTo cabalNew dataFileName $ writefile outputFileName

appendTemplate :: CabalNew -> FS.FilePath -> FS.FilePath -> Sh ()
appendTemplate cabalNew dataFileName appendFileName =
    templateTo cabalNew dataFileName $ appendfile appendFileName

copyDataFile :: FS.FilePath -> FS.FilePath -> Sh ()
copyDataFile dataFileName outputFileName = do
    dataFile <- FS.decodeString <$> liftIO (getDataFileName dataFileName')
    cp dataFile outputFileName
    where dataFileName' = FS.encodeString dataFileName
