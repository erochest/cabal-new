{-# LANGUAGE OverloadedStrings #-}


module CabalNew.Utils
    ( ifSet
    , ifTrue
    , toTitleCase
    ) where


import qualified Data.Char   as C
import           Data.Monoid
import           Data.Text
import qualified Data.Text   as T


ifSet :: Text -> String -> Maybe Text
ifSet _ ""    = Nothing
ifSet key val = Just $ "--" <> key <> "=" <> T.pack val

ifTrue :: Text -> Bool -> Maybe Text
ifTrue _ False  = Nothing
ifTrue key True = Just $ "--" <> key

toTitleCase :: Bool -> String -> String
toTitleCase _     []       = []
toTitleCase True  (c:cs)   = C.toUpper c : toTitleCase False cs
toTitleCase _     ('-':cs) = toTitleCase True cs
toTitleCase False (c:cs)   = c : toTitleCase False cs
