{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module : Forms.QuoteCategory
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- 'QuoteCategory' form.
module Forms.QuoteCategory where

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

quoteCategoryForm :: Monad m => Form T.Text m QuoteCategory
quoteCategoryForm =
  QuoteCategory <$> "name" .: nonEmptyText
                <*> (Slug <$> "slug" .: nonEmptyText)
                <*> "enabled" .: bool (Just True)
                <*> pure []
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing

splicesFromQuoteCategory :: Monad n => QuoteCategory -> Splices (I.Splice n)
splicesFromQuoteCategory qc = do
  "name" ## qc ^. name . to I.textSplice
  "slug" ## qc ^. slug . _Slug . to I.textSplice
  "enabled" ## qc ^. enabled . to (I.textSplice . T.pack . show)
