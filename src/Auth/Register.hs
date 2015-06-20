{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module : Auth.Register
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Functions and handlers for performing registration.
module Auth.Register where

import Application
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Lens
import Control.Monad.IO.Class
import Data.Text.Encoding
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Text.Digestive.Heist
import Text.Digestive.Snap hiding (method)

import Auth.Login
import Lenses
import Forms.Login

-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do
  (view', result) <- runForm "new_user" loginForm
  case result of
    Just user -> do
      auth' <- createUser
                (user ^. username)
                (user ^. password . to encodeUtf8)
      case auth' of
        Left e  -> liftIO (print e) >> heistLocal (bindDigestiveSplices view') (render "new_user")
        Right _ -> handleLoginSubmit view' "new_user" user
    Nothing -> heistLocal (bindDigestiveSplices view') $ render "new_user"
