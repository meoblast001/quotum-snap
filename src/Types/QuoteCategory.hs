{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Types.QuoteCategory where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Text.Digestive

type Slug = T.Text

data QuoteCategory =
  QuoteCategory {
    quoteCategoryName :: T.Text
  , quoteCategorySlug :: Slug
  , quoteCategoryEnabled :: Bool
  } deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''QuoteCategory

quoteCategoryForm :: Monad m => Form T.Text m QuoteCategory
quoteCategoryForm =
  QuoteCategory <$> "name" .: nonEmptyText
                <*> "slug" .: nonEmptyText
                <*> "enabled" .: bool (Just True)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing
