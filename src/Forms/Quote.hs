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
import Text.Digestive
import qualified Data.Text as T
import qualified Data.Set as S
import Types.FormQuote
import Types.Slug
import Snap.Snaplet.AcidState

quoteForm :: Monad m => Form T.Text m FormQuote
quoteForm =
  FormQuote <$> (Slug <$> "slug" .: nonEmptyText)
        <*> "title" .: nonEmptyText
        <*> "contents" .: nonEmptyText
        <*> (S.fromList . T.split (== ',') <$> "categories" .: nonEmptyText)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing
