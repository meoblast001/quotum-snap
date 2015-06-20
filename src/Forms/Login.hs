{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module : Forms.Login
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Login form.
module Forms.Login where

import Application
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Text.Digestive
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Auth
import Types.Login

loginForm :: Form T.Text (Handler App (AuthManager App)) Login
loginForm =
  Login
    <$> "username" .: check usernameEmpty (not . T.null) (text Nothing)
    <*> "password" .: check passwordEmpty (not . T.null) (text Nothing)
    <*> "remember" .: bool (Just False)
  where
    -- TODO: i18n?
    usernameEmpty = "Username must not be empty"
    passwordEmpty = "Password must not be empty"
