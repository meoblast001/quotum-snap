{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lenses where

import Control.Lens
import qualified Data.Text as T

import Types.Login
import Types.Quote
import Types.QuoteCategory
import Types.Slug

makeFields ''Login
makeFields ''Quote
makeFields ''QuoteCategory

_Slug :: Iso' Slug T.Text
_Slug = iso (\(Slug s) -> s) Slug
