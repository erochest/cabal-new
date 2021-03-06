{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Opts
    ( opts
    , opts'
    ) where


import           CabalNew.Types
import           Data.Char           (toLower)
import           Data.Version
import           Options.Applicative
import           Options.Applicative.Types
import           Paths_cabal_new     (version)


opts' :: Parser CabalNew
opts' =   CabalNew
      <$> strOption  (  short 'r' <> long "root-dir"
                     <> value "~/p/"
                     <> help "The root directory for all projects\
                             \ (default '~/p/').")
      <*> strOption  (  short 'p' <> long "project-name"
                     <> help "The project name.")
      <*> gitOption  (  short 'g' <> long "git"
                     <> value GitHere
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
      <*> targetOpt  (  short 't' <> long "target" <> value Executable
                     <> help "The type of project (compile target):\
                             \ 'Executable', 'Library', 'Yesod',\
                             \ 'JavaScript', or 'Sandbox'.\
                             \ Default is Executable.")
      <*> backendOpt (  short 'b' <> long "yesod-backend" <> value Nothing
                     <> help "The backend for a Yesod project. One of\
                             \ 's' (SQLite), 'p' (PostgreSQL),\
                             \ 'pf' (PostgreSQL+Fay), 'mongo' (MongoDB),\
                             \ 'mysql' (MySQL), or 'simple' (no database).\
                             \ If used at all, it defaults to 'p'.")
      <*> switch     (  short 'T' <> long "tmuxifier"
                     <> help "Generate and place a tmuxifier layout.")

gitOption :: Mod OptionFields GitLevel -> Parser GitLevel
gitOption = option (readGitLevel =<< readerAsk)

targetOpt :: Mod OptionFields CabalTarget -> Parser CabalTarget
targetOpt = option (readTargetOption =<< readerAsk)

backendOpt :: Mod OptionFields (Maybe YesodBackend) -> Parser (Maybe YesodBackend)
backendOpt = option (optional (str >>= readBackendOption))

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

readTargetOption :: Monad m => String -> m CabalTarget
readTargetOption target = go $ map toLower target
    where go ('l':_) = return Library
          go ('e':_) = return Executable
          go ('y':_) = return Yesod
          go ('j':_) = return GhcJs
          go ('s':_) = return Sandbox
          go _       = fail $  "Invalid --target value: '" ++ target
                            ++ "'. Please supply one of 'executable' or\
                               \ 'library'."

readBackendOption :: Monad m => String -> m YesodBackend
readBackendOption backend = go $ map toLower backend
    where go "s"      = return Sqlite
          go "p"      = return Postgres
          go "pf"     = return PostFay
          go "mongo"  = return MongoDB
          go "mysql"  = return MySQL
          go "simple" = return Simple
          go _        = fail $ "Invalid backend: " ++ backend
