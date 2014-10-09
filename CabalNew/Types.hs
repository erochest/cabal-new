{-# LANGUAGE DeriveDataTypeable #-}


module CabalNew.Types
    ( CabalNew(..)
    , GitLevel(..)
    ) where


import           Data.Data


data GitLevel = GitHere | ParentGit | Gitless
              deriving (Show, Eq, Data, Typeable)

data CabalNew = CabalNew
              { projectRootDir    :: String
              , projectName       :: String
              , projectGitLevel   :: GitLevel
              , privateProject    :: Bool
              , projectLicense    :: String
              , projectEmail      :: String
              , projectSynopsis   :: String
              , projectCategory   :: String
              , projectLibrary    :: Bool
              , projectExecutable :: Bool
              , projectTmuxifier  :: Bool
              } deriving (Data, Typeable, Show)

