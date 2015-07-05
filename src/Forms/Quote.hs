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
import Text.Digestive
import qualified Data.Text as T
import Heist
import qualified Heist.Interpreted as I
import Types.QuoteCategory
import Types.Slug
import Lenses

quoteForm :: Monad m => Form T.Text m Quote
quoteForm =
  Quote <$> (Slug <$> "slug" .: nonEmptyText)
        <*> "title" .: nonEmptyText
        <*> "contents" .: nonEmptyText
        <*> (T.split (== ',') <$> "categories" .: nonEmptyText) -- TODO: This won't typecheck.
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing
