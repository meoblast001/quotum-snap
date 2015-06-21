{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module : Auth.Logout
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Functions and handlers for logging the user out.
module Auth.Logout where

import Application
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth

-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
