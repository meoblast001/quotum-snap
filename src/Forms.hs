{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Forms where

import Application
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Text.Digestive
import qualified Data.Text as T
import Snap
import Snap.Snaplet.Auth
import Types.Login
import Types.QuoteCategory

quoteCategoryForm :: Monad m => Form T.Text m QuoteCategory
quoteCategoryForm =
  QuoteCategory <$> "name" .: nonEmptyText
                <*> "slug" .: nonEmptyText
                <*> "enabled" .: bool (Just True)
  where
    nonEmptyText =
      check "Field cannot be blank" (not . T.null) $ text Nothing

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
