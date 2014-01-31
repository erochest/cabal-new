module CabalNew.Types
    ( CabalNew(..)
    ) where


data CabalNew = CabalNew
              { projectRootDir    :: String
              , projectName       :: String
              , projectPatchDir   :: String
              , privateProject    :: Bool
              , projectLicense    :: String
              , projectEmail      :: String
              , projectSynopsis   :: String
              , projectCategory   :: String
              , projectLibrary    :: Bool
              , projectExecutable :: Bool
              } deriving (Show)

