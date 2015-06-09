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
import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
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
handleLogin authError = heistLocal (I.bindSplices errs) $ render "index"
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
allQuoteCategorySplices qcs = "allQuoteCategories" ## renderQuoteCategories qcs
  where
    renderQuoteCategories = I.mapSplices $ I.runChildrenWith . splicesFromQuoteCategory

-- | Render category list of quotes.
handleViewCategory :: Handler App (AuthManager App) ()
handleViewCategory = do
  slugMaybe <- getParam "slug"
  case decodeUtf8 <$> slugMaybe of
    Just slug' -> do
      categories' <- query AllQuoteCategories
      let categoryMaybe = categories' ^? traverse . filtered (has (slug . only slug'))
      case categoryMaybe of
        Nothing -> redirect "/"    -- TODO: A 404 message would be better.
        Just category' ->
          let splices = "categoryName" ## category' ^. name . to I.textSplice
          in renderWithSplices "view_category" splices
    -- TODO: A 400 message would be better.
    Nothing -> redirect "/"

-- | Render new category form/handle new category form submit
handleNewCategory :: Handler App (AuthManager App) ()
handleNewCategory = do
  (view', result) <- runForm "form" quoteCategoryForm
  case result of
   Just x -> update (AddQuoteCategory x) >> renderAllCategories view'
   Nothing -> renderAllCategories view'
  where
    renderAllCategories view' = do
      categories' <- query AllQuoteCategories
      let enabledCategories = filter _enabled categories'
          splices = I.bindSplices (allQuoteCategorySplices enabledCategories)
                  . bindDigestiveSplices view'
      heistLocal splices (render "list_categories")

handlePendingCategories :: Handler App (AuthManager App) ()
handlePendingCategories =  do
  categories' <- query AllQuoteCategories
  let splices = I.bindSplices (
        allQuoteCategorySplices (filter (not . _enabled) categories'))
  heistLocal splices (render "list_categories")

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login", with auth handleLoginSubmit)
         , ("/logout", with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/category/:slug", with auth handleViewCategory)
         , ("/category", with auth handleNewCategory)
         , ("/categories", with auth handleNewCategory)
         , ("/categories/pending", with auth handlePendingCategories)
         , ("", serveDirectory "static") ]

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "quotum" "Quotum Quote Database" Nothing $ do
  heist' <- nestSnaplet "" heist $ heistInit "templates"
  sess' <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  acid' <- nestSnaplet "acid" acid $ acidInit (AppState M.empty)
  auth' <- nestSnaplet "auth" auth $
           initAcidAuthManager defAuthSettings sess
  addRoutes routes
  addAuthSplices heist' auth
  return $ App heist' sess' acid' auth'
