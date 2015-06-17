{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
module Types.Quote where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Lens
import Data.SafeCopy
import qualified Data.Text as T
import Data.Typeable
import Types.QuoteCategory

data Quote =
  Quote {
    _quoteSlug :: Slug
  , _quoteTitle :: T.Text
  , _quoteContents :: T.Text
  } deriving (Eq, Show, Typeable)

makeFields ''Quote
deriveSafeCopy 0 'base ''Quote
