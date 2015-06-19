{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lenses where

import Control.Lens

import Types.Login
import Types.Quote
import Types.QuoteCategory

makeFields ''Login
makeFields ''Quote
makeFields ''QuoteCategory
