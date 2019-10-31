{-# LANGUAGE TemplateHaskell #-}
module Model.DataType where

import ClassyPrelude.Yesod
import Database.Persist.TH ()

data DataType = DataType
    deriving (Show, Read, Eq, Ord)
derivePersistField "DataType"
