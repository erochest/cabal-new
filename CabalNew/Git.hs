{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Git
    ( git_
    , gitAddAllCommit
    , withCommit
    , publish
    ) where


import           Control.Applicative
import           Data.Monoid
import qualified Data.Text           as T
import           Shelly

import           CabalNew.Types


git_ :: GitLevel -> T.Text -> [T.Text] -> Sh ()
git_ Gitless   c      _    = echo $ "Running gitless. Skipping 'git " <> c <> "'."
git_ ParentGit "init" _    = echo   "Using parent repository. Skipping 'git init'."
git_ ParentGit c      args = git_' c args
git_ GitHere   c      args = git_' c args

git_' :: T.Text -> [T.Text] -> Sh ()
git_' = command1_ "git" []

gitAddAllCommit :: GitLevel -> T.Text -> Sh ()
gitAddAllCommit gitLevel msg =  git_ gitLevel "add"    ["."]
                             >> git_ gitLevel "commit" ["-m", msg]

withCommit :: GitLevel -> T.Text -> Sh a -> Sh a
withCommit gitLevel msg op = op <* gitAddAllCommit gitLevel msg

publish :: Bool -> GitLevel -> T.Text -> Sh ()
publish True  _         _   = return ()
publish False Gitless   _   = return ()
publish False GitHere   msg = publish' GitHere msg
publish False ParentGit msg = publish' ParentGit msg

publish' :: GitLevel -> T.Text -> Sh ()
publish' gitLevel descr =  run_ "hub" ["create", "-d", descr]
                        >> git_ gitLevel "push" ["-u", "origin", "master"]
