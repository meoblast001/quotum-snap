{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Lenses where

import Control.Lens

import Types.Login
import Types.Quote
import Types.QuoteCategory
import Types.Slug

makeFields ''Login
makeFields ''Quote
makeFields ''QuoteCategory

-- Generate an Iso here called _Slug
makePrisms ''Slug
