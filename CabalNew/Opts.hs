{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Opts
    ( opts
    , opts'
    ) where


import           CabalNew.Types
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative


opts' :: Parser CabalNew
opts' =   CabalNew
      <$> strOption (  short 'r' <> long "root-dir"
                    <> value "~/p/"
                    <> help "The root directory for all projects\
                            \ (default '~/p/').")
      <*> strOption (  short 'p' <> long "project-name"
                    <> help "The project name.")
      <*> strOption (  short 'a' <> long "apply-project"
                    <> value (FS.encodeString "~/p/hs-project/")
                    <> help "The directory containing the project\
                            \ to apply a patch on this project\
                            \ with (default '~/p/hs-project/').")
      <*> switch    (  short 'P' <> long "private"
                    <> help "Don't publish this repository to github.")
      <*> strOption (  short 'l' <> long "license" <> value "Apache-2.0"
                    <> help "The cabal option for the license.\
                            \ (defaults 'Apache-2.0').")
      <*> strOption (  short 'e' <> long "email"
                    <> help "The cabal option for the email.")
      <*> strOption (  short 's' <> long "synopsis"
                    <> help "The cabal option for the synopsis.")
      <*> strOption (  short 'c' <> long "category"
                    <> help "The cabal option for the category.")
      <*> switch    (  long "is-library"
                    <> help "The cabal option for the library.")
      <*> switch    (  long "is-executable"
                    <> help "The cabal option for the executable.")

opts :: ParserInfo CabalNew
opts  = info (helper <*> opts') (  fullDesc
                                <> progDesc "Create a new Haskell project\
                                            \ with cabal, git, sandbox-init,\
                                            \ and everything else."
                                <> header "cabal-new - a utility to initialize\
                                          \ a new Haskell project."
                                )

