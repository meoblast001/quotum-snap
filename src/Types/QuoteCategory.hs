{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Types.QuoteCategory where

import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable

type Slug = T.Text

data QuoteCategory =
  QuoteCategory {
    quoteCategoryName :: T.Text
  , quoteCategorySlug :: Slug
  , quoteCategoryEnabled :: Bool
  } deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''QuoteCategory
