{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Database
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.AcidState
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

data App = App
  { _heist :: Snaplet (Heist App)
  , _sess :: Snaplet SessionManager
  , _acid :: Snaplet (Acid Database)
  , _auth :: Snaplet (AuthManager App) }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasAcid App Database where
  getAcidStore = view (acid . snapletValue)

type AppHandler = Handler App App
