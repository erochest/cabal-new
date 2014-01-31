{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Git
    ( git_
    , gitAddAllCommit
    , withCommit
    , publish
    ) where


import           Control.Applicative
import qualified Data.Text           as T
import           Shelly


git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

gitAddAllCommit :: T.Text -> Sh ()
gitAddAllCommit msg = git_ "add" ["."] >> git_ "commit" ["-m", msg]

withCommit :: T.Text -> Sh a -> Sh a
withCommit msg op = op <* gitAddAllCommit msg

publish :: Bool -> Sh ()
publish isPrivate = unless isPrivate $
    run_ "hub" ["create"] >> git_ "push" ["-u", "origin", "master"]
