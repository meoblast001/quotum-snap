{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.Quote where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Types.Slug

data Quote =
  Quote {
    quoteSlug :: Slug
  , quoteTitle :: T.Text
  , quoteContents :: T.Text
  , quoteCategoryList :: [Slug]
  } deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''Quote

instance ToJSON Quote where
  toJSON (Quote slug title contents categories) =
    object ["slug" .= slug, "title" .= title, "contents" .= contents, "categories" .= categories]

instance FromJSON Quote where
  parseJSON (Object v) = Quote
                         <$> v .: "slug"
                         <*> v .: "title"
                         <*> v .: "contents"
                         <*> v .: "categories"
  parseJSON _          = mzero
