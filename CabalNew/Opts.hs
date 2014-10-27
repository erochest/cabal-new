{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Opts
    ( opts
    , opts'
    ) where


import           CabalNew.Types
import           Data.Char           (toLower)
import           Data.Version
import           Options.Applicative
import           Paths_cabal_new     (version)


opts' :: Parser CabalNew
opts' =   CabalNew
      <$> strOption  (  short 'r' <> long "root-dir"
                     <> value "~/p/"
                     <> help "The root directory for all projects\
                             \ (default '~/p/').")
      <*> strOption  (  short 'p' <> long "project-name"
                     <> help "The project name.")
      <*> nullOption (  short 'g' <> long "git"
                     <> value GitHere
                     <> reader readGitLevel
                     <> help "The level of git interaction to use. 'Here' (the\
                             \ default) means that the new project directory\
                             \ will be a git repository. 'Parent' means that\
                             \ there is a git repository in a parent directory,\
                             \ and the current project will be added to that.\
                             \ 'None' means not to use git in any way.")
      <*> switch     (  short 'P' <> long "private"
                     <> help "Don't publish this repository to github.")
      <*> strOption  (  short 'l' <> long "license" <> value "Apache-2.0"
                     <> help "The cabal option for the license.\
                             \ (defaults 'Apache-2.0').")
      <*> strOption  (  short 'e' <> long "email"
                     <> help "The cabal option for the email.")
      <*> strOption  (  short 's' <> long "synopsis"
                     <> help "The cabal option for the synopsis.")
      <*> strOption  (  short 'c' <> long "category"
                     <> help "The cabal option for the category.")
      <*> switch     (  short 'L' <> long "is-library"
                     <> help "The cabal option for the library.")
      <*> switch     (  short 'E' <> long "is-executable"
                     <> help "The cabal option for the executable.")
      <*> switch     (  short 'T' <> long "tmuxifier"
                     <> help "Generate and place a tmuxifier layout.")

opts :: ParserInfo CabalNew
opts  = info (helper <*> opts')
             (  fullDesc
             <> progDesc "Create a new Haskell project\
                         \ with cabal, git, sandbox-init,\
                         \ and everything else."
             <> header ("cabal-new - " ++ showVersion version ++
                        " - a utility to initialize\
                        \ a new Haskell project.")
             )

readGitLevel :: Monad m => String -> m GitLevel
readGitLevel level = go $ map toLower level
    where go ('h':_) = return GitHere
          go ('p':_) = return ParentGit
          go ('n':_) = return Gitless
          go _       = fail $  "Invalid --git value: '" ++ level
                            ++ "'. Please supply one of 'here', 'parent',\
                               \ or 'none'."

