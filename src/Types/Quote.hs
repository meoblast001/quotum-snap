{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
module Types.Quote where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Types.QuoteCategory

data Quote =
  Quote {
    quoteSlug :: Slug
  , quoteTitle :: T.Text
  , quoteContents :: T.Text
  } deriving (Eq, Show, Typeable)

deriveSafeCopy 0 'base ''Quote
