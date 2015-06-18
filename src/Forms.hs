{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Forms where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Text.Digestive
import qualified Data.Text as T

import Types.QuoteCategory

quoteCategoryForm :: Monad m => Form T.Text m QuoteCategory
quoteCategoryForm =
  QuoteCategory <$> "name" .: nonEmptyText
                <*> "slug" .: nonEmptyText
                <*> "enabled" .: bool (Just True)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing
