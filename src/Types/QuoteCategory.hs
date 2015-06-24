{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.QuoteCategory where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Types.Slug

data QuoteCategory =
  QuoteCategory {
    quoteCategoryName :: T.Text
  , quoteCategorySlug :: Slug
  , quoteCategoryEnabled :: Bool
  } deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''QuoteCategory

instance ToJSON QuoteCategory where
  toJSON (QuoteCategory name slug enabled) =
    object ["name" .= name, "slug" .= slug, "enabled" .= enabled]

instance FromJSON QuoteCategory where
  parseJSON (Object v) = QuoteCategory
                         <$> v .: "name"
                         <*> v .: "slug"
                         <*> v .: "enabled"
  parseJSON _          = mzero
