{-# LANGUAGE DeriveDataTypeable #-}


module CabalNew.Types
    ( CabalNew(..)
    , CabalTarget(..)
    , YesodBackend(..)
    , GitLevel(..)
    ) where


import           Data.Data


data GitLevel = GitHere | ParentGit | Gitless
              deriving (Show, Eq, Data, Typeable)

data CabalTarget = Executable
                 | Library
                 | Yesod
                 | GhcJs
                 | Sandbox
                 deriving (Show, Eq, Data, Typeable)

data YesodBackend = Sqlite
                  | Postgres
                  | PostFay
                  | MongoDB
                  | MySQL
                  | Simple
                  deriving (Show, Eq, Data, Typeable)

data CabalNew = CabalNew
              { projectRootDir   :: String
              , projectName      :: String
              , projectGitLevel  :: GitLevel
              , privateProject   :: Bool
              , projectLicense   :: String
              , projectEmail     :: String
              , projectSynopsis  :: String
              , projectCategory  :: String
              , projectTarget    :: CabalTarget
              , projectBackend   :: Maybe YesodBackend
              , projectTmuxifier :: Bool
              } deriving (Data, Typeable, Show)

