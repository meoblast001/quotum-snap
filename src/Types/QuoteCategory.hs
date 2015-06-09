{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types.QuoteCategory where

import Control.Lens
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Heist
import qualified Heist.Interpreted as I
import Text.Digestive

type Slug = T.Text

data QuoteCategory =
  QuoteCategory {
    _name :: T.Text
  , _slug :: Slug
  , _enabled :: Bool
  } deriving (Eq, Show, Typeable)

makeLenses ''QuoteCategory
deriveSafeCopy 0 'base ''QuoteCategory

quoteCategoryForm :: Monad m => Form T.Text m QuoteCategory
quoteCategoryForm =
  QuoteCategory <$> "name" .: nonEmptyText
                <*> "slug" .: nonEmptyText
                <*> "enabled" .: bool (Just True)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing

splicesFromQuoteCategory :: Monad n => QuoteCategory -> Splices (I.Splice n)
splicesFromQuoteCategory qc = do
  "name" ## qc ^. name . to I.textSplice
  "slug" ## qc ^. slug . to I.textSplice
  "enabled" ## qc ^. enabled . to (I.textSplice . T.pack . show)
