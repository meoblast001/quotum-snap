{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : Application
-- Copyright : (C) 2015 Braden Walters
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Braden Walters <vc@braden-walters.info>
-- Stability : experimental
--
-- This module defines our application's state type and an alias for its
-- handler monad.
module Application where

import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.AcidState
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

import Data.SafeCopy
import Data.Typeable
import Types.QuoteCategory

data AppState =
  AppState {
    _categories :: [QuoteCategory]
  } deriving (Eq, Show, Typeable)

makeLenses ''AppState

data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _acid :: Snaplet (Acid AppState)
  , _auth :: Snaplet (AuthManager App) }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAcid App AppState where
  getAcidStore = view (acid . snapletValue)

type AppHandler = Handler App App

deriveSafeCopy 0 'base ''AppState

addQuoteCategory :: QuoteCategory -> Update AppState ()
addQuoteCategory qc = categories %= (:) qc

allQuoteCategories :: Query AppState [QuoteCategory]
allQuoteCategories = view categories

deleteQuoteCategory :: QuoteCategory -> Update AppState ()
deleteQuoteCategory qc = do
  qcs <- use categories
  categories .= filter (== qc) qcs

makeAcidic ''AppState
  [
    'addQuoteCategory
  , 'allQuoteCategories
  , 'deleteQuoteCategory
  ]
