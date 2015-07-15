{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module : Forms.Quote
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- 'Quote' form.
module Forms.Quote where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Lens
import Control.Monad
import Text.Digestive
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Maybe
import Application
import Heist
import qualified Heist.Interpreted as I
import Types.Quote
import Types.QuoteCategory
import Types.Slug
import Lenses
import Snap.Snaplet.AcidState

quoteForm :: Monad m => Form T.Text m Quote
quoteForm =
  Quote <$> (Slug <$> "slug" .: nonEmptyText)
        <*> "title" .: nonEmptyText
        <*> "contents" .: nonEmptyText
        <*> (map toQuoteCategory . T.split (== ',') <$> "categories" .: nonEmptyText) -- TODO: This won't typecheck.
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing
    toQuoteCategory :: T.Text -> Query AppState (Maybe QuoteCategory)
    toQuoteCategory slug' =
      searchQuoteCategory (Slug slug')
