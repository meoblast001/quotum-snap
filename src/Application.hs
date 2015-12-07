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
import Data.SafeCopy
import Data.Typeable
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.AcidState
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.Sass

import qualified Lenses as L
import Types.QuoteCategory
import Types.Quote
import Types.Slug

data AppState =
  AppState {
    _categories :: M.Map Slug QuoteCategory
  , _quotes     :: M.Map Slug Quote
  } deriving (Eq, Show, Typeable)

makeLenses ''AppState

data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _acid :: Snaplet (Acid AppState)
  , _auth :: Snaplet (AuthManager App)
  , _sass :: Snaplet Sass
  }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAcid App AppState where
  getAcidStore = view (acid . snapletValue)

type AppHandler = Handler App App

deriveSafeCopy 0 'base ''AppState

saveQuoteCategory :: QuoteCategory -> Update AppState ()
saveQuoteCategory qc = categories %= M.insert (qc ^. L.slug) qc

allQuoteCategories :: Query AppState [QuoteCategory]
allQuoteCategories = map snd <$> M.toList <$> view categories

findQuoteCategoryBySlug :: Slug -> Query AppState (Maybe QuoteCategory)
findQuoteCategoryBySlug slug' = M.lookup slug' <$> view categories

deleteQuoteCategory :: Slug -> Update AppState ()
deleteQuoteCategory slug' = categories %= M.delete slug'

saveQuote :: Quote -> Update AppState ()
saveQuote quote = quotes %= M.insert (quote ^. L.slug) quote

allQuotes :: Query AppState [Quote]
allQuotes = map snd <$> M.toList <$> view quotes

findQuoteBySlug :: Slug -> Query AppState (Maybe Quote)
findQuoteBySlug slug' = M.lookup slug' <$> view quotes

deleteQuote :: Slug -> Update AppState ()
deleteQuote slug' = quotes %= M.delete slug'

makeAcidic ''AppState
  [
    'saveQuoteCategory
  , 'allQuoteCategories
  , 'findQuoteCategoryBySlug
  , 'deleteQuoteCategory
  , 'saveQuote
  , 'allQuotes
  , 'findQuoteBySlug
  , 'deleteQuote
  ]
