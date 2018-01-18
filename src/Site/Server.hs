{-# LANGUAGE OverloadedStrings #-}

module Site.Server (runSite) where

import Site.Data.Config
import Util.HTML
import Util.Reloadable

import Site.Subpage
import Site.Home
import Site.Blog
import Site.CV

import Happstack.Lite
import Control.Monad
import Control.Concurrent.MVar
import System.Process

import Happstack.Server       (redirect, result)
import Data.Text              (Text)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M

-- | Run the site from the given directory.
runSite :: FilePath -> IO ()
runSite root = do

  index <- newEmptyMVar
  cache <- newEmptyMVar
  cv    <- makeCV root
  
  let config = Config { blogCache   = cache
                      , blogIndex   = index
                      , resumeCache = cv
                      , siteRoot    = root
                      }
  refreshBlog   config

  serve Nothing (site config)

-- | Invalidate the blog cache.
refreshBlogPage :: Config -> ServerPart Response
refreshBlogPage config = do
  refreshBlog   config
  ok (toResponse ("refreshed blog!" :: Text))

-- | Invalidate the CV cache.
refreshResumePage :: Config -> ServerPart Response
refreshResumePage config = do
  invalidate (resumeCache config)
  ok (toResponse ("refreshed resume!" :: Text))

-- | Route requests to the correct handler, or default to home.
site :: Config -> ServerPart Response
site config =
    msum [ dir "home"  (homePage config)
         , dir "cv"    (cvPage   config)
         , dir "blog"  (path $ \entry -> blogPage entry config)
         , dir "blog"  (blogIndexPage config)
         -- Image resources
         , dir "me" (me config)
         -- Administration
         , dir "refresh-blog" (refreshBlogPage config)
         , dir "refresh-cv"   (refreshResumePage config)
         , dir "pull"         (pull config)
         ] `mappend` homePage config
      
-- | Serve the home page.
homePage :: Config -> ServerPart Response
homePage _ = ok $ subpage Home home
      
-- | Serve the CV page.
cvPage :: Config -> ServerPart Response
cvPage config = access (resumeCache config) >>= (ok . subpage CV)

-- | Serve the single image on this site, for now.
me :: Config -> ServerPart Response
me config = serveFile (asContentType "image/jpeg") (siteRoot config ++ "/images/banjo.jpg")

-- | Serve the blog index.
blogIndexPage :: Config -> ServerPart Response
blogIndexPage config = do
  index <- liftIO $ readMVar (blogIndex config)
  ok $ subpage Blog (makeBlogIndex index)

-- | Serve a particular blog entry.
blogPage :: Text -> Config -> ServerPart Response
blogPage entry config = do
  cache <- liftIO $ readMVar (blogCache config)
  case M.lookup entry cache of
    Nothing   -> notFound (toResponse ("No such blog entry!" :: Text))
    Just body -> access body >>= (ok . subpage Blog)

pull :: Config -> ServerPart Response
pull config = do
  git <- liftIO $ readProcess "git" [ "--git-dir=" ++ siteRoot config ++ "/.git"
                                    , "--work-tree=" ++ siteRoot config
                                    , "pull" ] ""
  ok $ toResponse $ HTML (pre_ [] (toHtml git))
