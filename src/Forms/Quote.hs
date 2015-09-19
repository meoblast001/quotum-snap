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

import Application
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.Maybe
import Text.Digestive
import qualified Data.Text as T
import qualified Data.Set as S
import Types.FormQuote
import Types.Slug
import Types.Quote
import Snap.Snaplet.AcidState

quoteForm :: Monad m => Form T.Text m FormQuote
quoteForm =
  FormQuote <$> (Slug <$> "slug" .: nonEmptyText)
        <*> "title" .: nonEmptyText
        <*> "contents" .: nonEmptyText
        <*> (S.fromList . (map Slug) . T.split (== ',') <$>
             "categories" .: nonEmptyText)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing

saveQuoteForm :: FormQuote -> AppHandler Quote
saveQuoteForm formQuote = do
  let slugList = S.toList $ formQuoteCategoryList formQuote
      slugToQuery slug' = query (FindQuoteCategoryBySlug slug')
  categories' <- catMaybes <$> mapM slugToQuery slugList
  let quote = Quote (formQuoteSlug formQuote)
                    (formQuoteTitle formQuote)
                    (formQuoteContents formQuote)
                    (S.fromList categories')
  update (SaveQuote quote)
  return quote
