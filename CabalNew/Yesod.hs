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
import           CabalNew.Templates
import           CabalNew.Types


yesodInit :: CabalNew -> Sh ()
yesodInit config =
    setStdin stdin' >> run_ "yesod" ["init", "--bare"]
    where
        stdin' = T.unlines [ T.pack (projectName config)
                           , "p"
                           ]

yesodProject :: CabalNew -> FilePath -> Sh (Sh ())
yesodProject config _projectDir = do
    withCommit (projectGitLevel config) "yesod init" $
        yesodInit config
    return (return ())
