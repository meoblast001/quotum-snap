{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

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
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.AcidState
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.Acid
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Sass
import Snap.Util.FileServe
import Text.Digestive.Heist
import Text.Digestive.Snap hiding (method)
import Heist
import qualified Heist.Interpreted as I

import Auth.Login
import Auth.Logout
import Auth.Register
import Forms.Quote
import Forms.QuoteCategory
import qualified Lenses as L
import Types.Quote
import Types.QuoteCategory
import Types.Slug

allQuoteCategorySplices :: [QuoteCategory] -> Splices (SnapletISplice App)
allQuoteCategorySplices qcs = "allQuoteCategories" ## renderQuoteCategories qcs
  where
    renderQuoteCategories =
      I.mapSplices $ I.runChildrenWith . splicesFromQuoteCategory

-- | Render category list of quotes.
handleViewCategory :: Handler App (AuthManager App) ()
handleViewCategory = do
  slugMaybe <- getParam "slug"
  case decodeUtf8 <$> slugMaybe of
    Just slug' -> do
      categoryMaybe <- query (FindQuoteCategoryBySlug (Slug slug'))
      case categoryMaybe of
        Nothing -> renderTemplateAs 404 "error404"
        Just category' ->
          let splices = "categoryName" ## category' ^. L.name . to I.textSplice
          in renderWithSplices "view_category" splices
    -- TODO: A 400 message would be better.
    Nothing -> redirect "/"

-- | Render new category form/handle new category form submit
handleNewCategory :: Handler App (AuthManager App) ()
handleNewCategory = do
  (view', result) <- runForm "form" quoteCategoryForm
  case result of
   Just x -> update (SaveQuoteCategory x) >> renderAllCategories view'
   Nothing -> renderAllCategories view'
  where
    renderAllCategories view' = do
      categories' <- query AllQuoteCategories
      let enabledCategories = filter quoteCategoryEnabled categories'
          splices = I.bindSplices (allQuoteCategorySplices enabledCategories)
                  . bindDigestiveSplices view'
      heistLocal splices (render "list_categories")

handlePendingCategories :: Handler App (AuthManager App) ()
handlePendingCategories = do
  categories' <- query AllQuoteCategories
  let splices = I.bindSplices (
        allQuoteCategorySplices (filter (not . quoteCategoryEnabled) categories'))
  heistLocal splices (render "list_categories")

handleViewQuotes :: Handler App (AuthManager App) ()
handleViewQuotes = do
  mayCategorySlug <- getParam "category"
  case decodeUtf8 <$> mayCategorySlug of
    Just categorySlug -> do
      mayCategory <- query $ FindQuoteCategoryBySlug (Slug categorySlug)
      case mayCategory of
        Just category' -> do
          quotes' <- catMaybes <$> mapM
                     (\slug' -> query $ FindQuoteBySlug slug')
                     (category' ^. L.quotes)
          let splices = I.bindSplices (quotesSplices category' quotes')
          heistLocal splices (render "view_quotes")
        -- TODO: A 404 message would be better.
        Nothing -> redirect "/"
    -- TODO: A 400 message would be better.
    Nothing -> redirect "/"
  where
    quotesSplices :: QuoteCategory -> [Quote] -> Splices (SnapletISplice App)
    quotesSplices category' quotes' = do
      "categoryName" ## category' ^. L.name . to I.textSplice
      "quotes" ## renderQuotes quotes'
    renderQuotes = I.mapSplices $ I.runChildrenWith . splicesFromQuote

renderTemplateAs :: Int -> ByteString -> Handler App (AuthManager App) ()
renderTemplateAs code temp = do
  modifyResponse $ setResponseCode code
  render temp

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("", ifTop . with auth $ handleLogin)
         , ("/login", with auth handleLogin)
         , ("/logout", with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/category/:slug", with auth handleViewCategory)
         , ("/categories", with auth (needsUser handleNewCategory))
         , ("/categories/pending", with auth handlePendingCategories)
         , ("/quotes/:category", with auth handleViewQuotes)
         , ("/styles", with sass sassServe)
         , ("/static", serveDirectory "static")
         , ("", with auth (renderTemplateAs 404 "error404"))
         ]
  where
    needsUser successHandler = do
      loggedIn <- isLoggedIn
      req <- getRequest
      if loggedIn then successHandler else redirect ("/?r=" <> rqURI req)

-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "quotum" "Quotum Quote Database" Nothing $ do
  heist' <- nestSnaplet "" heist $ heistInit "templates"
  sess' <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  acid' <- nestSnaplet "acid" acid $ acidInit (AppState M.empty M.empty)
  auth' <- nestSnaplet "auth" auth $
           initAcidAuthManager defAuthSettings sess
  sass' <- nestSnaplet "sass" sass initSass
  addRoutes routes
  addAuthSplices heist' auth
  return $ App heist' sess' acid' auth' sass'
