{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module : Site
-- Copyright : (C) 2015 Braden Walters
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Braden Walters <vc@braden-walters.info>
-- Stability : experimental
--
-- This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site ( app ) where

import Application
import Database
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Monoid
import qualified Data.Text as T
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.AcidState
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.Acid
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Text.Digestive.Heist
import Text.Digestive.Snap hiding (method)
import Heist
import qualified Heist.Interpreted as I

import Types.QuoteCategory

-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err

-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
  loginUser "login" "password" Nothing (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"

-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

allQuoteCategorySplices :: [QuoteCategory] -> Splices (SnapletISplice App)
allQuoteCategorySplices qcs = "allQuoteCategories" ## (renderQuoteCategories qcs)
  where
    renderQuoteCategories = I.mapSplices $ I.runChildrenWith . splicesFromQuoteCategory


-- | Render new category form/handle new category form submit
handleNewCategory :: Handler App (AuthManager App) ()
handleNewCategory = do
  (view, result) <- runForm "form" quoteCategoryForm
  case result of
   Just x  -> do
     update (AddQuoteCategory x)
     categories' <- query AllQuoteCategories
     let splices = I.bindSplices (allQuoteCategorySplices categories')
                   . (bindDigestiveSplices view)
     heistLocal splices (render "list_categories")
   Nothing -> do
     categories' <- query AllQuoteCategories
     let splices = I.bindSplices (allQuoteCategorySplices categories')
                   . (bindDigestiveSplices view)
     heistLocal splices (render "list_categories")

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login", with auth handleLoginSubmit)
         , ("/logout", with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/category", with auth handleNewCategory)
         , ("/categories", with auth handleNewCategory)
         , ("", serveDirectory "static") ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "quotum" "Quotum Quote Database" Nothing $ do
  heist' <- nestSnaplet "" heist $ heistInit "templates"
  sess' <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  acid' <- nestSnaplet "acid" acid $ acidInit (AppState [])
  auth' <- nestSnaplet "auth" auth $
           initAcidAuthManager defAuthSettings sess
  addRoutes routes
  addAuthSplices heist' auth
  return $ App heist' sess' acid' auth'
