{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}

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
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
#endif
import qualified Data.Map as M
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
    _categories :: M.Map Slug QuoteCategory
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
addQuoteCategory qc = categories %= M.insert (qc ^. slug) qc

allQuoteCategories :: Query AppState [QuoteCategory]
allQuoteCategories = map snd <$> M.toList <$> view categories

searchQuoteCategory :: Slug -> Query AppState (Maybe QuoteCategory)
searchQuoteCategory slug' = M.lookup slug' <$> view categories

deleteQuoteCategory :: Slug -> Update AppState ()
deleteQuoteCategory slug' = categories %= M.delete slug'

makeAcidic ''AppState
  [
    'addQuoteCategory
  , 'allQuoteCategories
  , 'searchQuoteCategory
  , 'deleteQuoteCategory
  ]
