{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Yesod
    ( yesodInit
    , yesodProject
    ) where


import           ClassyPrelude
import qualified Data.Text          as T
import           Shelly

import           CabalNew.Git
import           CabalNew.Types


yesodInit :: CabalNew -> Sh ()
yesodInit config =
    setStdin stdin' >> run_ "yesod" ["init", "--bare"]
    where
        stdin' = T.unlines [ T.pack (projectName config)
                           , maybe "p" backend $ projectBackend config
                           ]
        backend b = case b of
                        Sqlite   -> "s"
                        Postgres -> "p"
                        PostFay  -> "pf"
                        MongoDB  -> "mongo"
                        MySQL    -> "mysql"
                        Simple   -> "simple"

yesodProject :: CabalNew -> FilePath -> Sh (Sh ())
yesodProject config _projectDir = do
    withCommit (projectGitLevel config) "yesod init" $
        yesodInit config
    return (return ())
