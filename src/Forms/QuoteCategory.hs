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

import Application
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Text.Digestive
import qualified Data.Text as T
import Snap
import Types.QuoteCategory

quoteCategoryForm :: Monad m => Form T.Text m QuoteCategory
quoteCategoryForm =
  QuoteCategory <$> "name" .: nonEmptyText
                <*> "slug" .: nonEmptyText
                <*> "enabled" .: bool (Just True)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing
