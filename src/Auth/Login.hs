{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

-- |
-- Module : Auth.Login
-- Copyright : (C) 2015 Ricky Elrod
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
--
-- Functions and handlers for performing authentication.
module Auth.Login where

import Application
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Maybe
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Text.Digestive.Heist
import Text.Digestive.Snap hiding (method)
import Text.Digestive.View (View)

import Lenses
import Types.Login
import Forms.Login

-- | Handle login form+submit
handleLogin :: Handler App (AuthManager App) ()
handleLogin = do
  (view', result) <- runForm "login" loginForm
  case result of
    Just user -> handleLoginSubmit view' "home" user
    Nothing -> heistLocal (bindDigestiveSplices view') $ render "home"

handleLoginSubmit :: View T.Text -> ByteString -> Login -> Handler App (AuthManager App) ()
handleLoginSubmit view' site user = do
  loginAttempt <- loginByUsername
                    (user ^. username)
                    (user ^. password . to (ClearText . encodeUtf8))
                    (user ^. remember)
  case loginAttempt of
    Left s -> do
      liftIO $ print s
      heistLocal (bindDigestiveSplices view') $ render site
    Right _ -> redirect . fromMaybe "/" =<< getQueryParam "r"
