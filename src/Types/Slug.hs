{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Slug where

import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy
import qualified Data.Text as T

-- | A 'Slug' is a pretty-url for a 'Quote' or a 'QuoteCategory'.
newtype Slug = Slug T.Text deriving (Eq, Ord, Show)

deriveSafeCopy 0 'base ''Slug

instance ToJSON Slug where
  toJSON (Slug s) = String s

instance FromJSON Slug where
  parseJSON (String s) = return . Slug $ s
  parseJSON _          = mzero
