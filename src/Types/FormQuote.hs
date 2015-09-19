{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.FormQuote where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (mzero)
import Data.Aeson
import Data.SafeCopy
import Data.Set (Set)
import qualified Data.Text as T
import Data.Typeable
import Types.Slug

data FormQuote =
  FormQuote {
    formQuoteSlug :: Slug
  , formQuoteTitle :: T.Text
  , formQuoteContents :: T.Text
  , formQuoteCategoryList :: Set T.Text
  } deriving (Eq, Ord, Show, Typeable)

deriveSafeCopy 0 'base ''FormQuote

instance ToJSON FormQuote where
  toJSON (FormQuote slug title contents categories) =
    object [ "slug" .= slug
           , "title" .= title
           , "contents" .= contents
           , "categories" .= categories
           ]

instance FromJSON FormQuote where
  parseJSON (Object v) = FormQuote
                         <$> v .: "slug"
                         <*> v .: "title"
                         <*> v .: "contents"
                         <*> v .: "categories"
  parseJSON _          = mzero
