{-# LANGUAGE OverloadedStrings #-}

module Site.Static (buildSite) where

import Site.Data.Config
import Util.HTML
import Util.Reloadable

import Site.Subpage
import Site.Home
import Site.Blog
import Site.CV

import Control.Monad
import Control.Concurrent.MVar
import System.Process
import System.Directory
import System.FilePath
import Happstack.Server (Response, rsBody)

import Data.Text              (Text)
import Data.Text.Encoding     (decodeUtf8)
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS

body :: Response -> Text
body = decodeUtf8 . BS.toStrict . rsBody

-- | Build a static version of the site.
buildSite :: FilePath -> FilePath -> IO ()
buildSite root output = do

  -- Build a site config that we can pass around
  index <- newEmptyMVar
  cache <- newEmptyMVar
  cv    <- makeCV root
  
  let config = Config { blogCache   = cache
                      , blogIndex   = index
                      , resumeCache = cv
                      , siteRoot    = root
                      }
  refreshBlog config
  
  -- Build the directory structure
  mapM_ (createDirectoryIfMissing True) [ output </> "images"
                                        , output </> "blog" ]

  images <- listDirectory (root </> "images")
  forM_ images $ \img -> copyFile (root   </> "images" </> takeFileName img)
                                  (output </> "images" </> takeFileName img)

  -- Render the homepage
  T.writeFile (output </> "index.html") (body $ subpage Home home)

  -- Render the CV page
  cvBody <- access cv
  T.writeFile (output </> "cv.html") (body $ subpage CV cvBody)

  -- Render the blog index page
  index <- makeBlogIndex <$> readMVar (blogIndex config)
  T.writeFile (output </> "blog" </> "index.html") (body $ subpage Blog index)

  -- Render the individual blog entries
  entries <- readMVar (blogCache config)
  forM_ (M.toList entries) $ \(name, entry) -> do
    contents <- access entry
    T.writeFile (output </> "blog" </> (T.unpack name ++ ".html")) (body $ subpage Blog contents)

  -- Set up .htaccess file
  let blogRoutes = map (\x -> (x, "./blog/" `T.append` x `T.append` ".html")) (M.keys entries)
      htaccess = [ "RewriteEngine On" ] ++
        [ "RewriteRule ^" `T.append` x `T.append` "$ " `T.append` y
        | (x,y) <- [ ("home", "./index.html")
                   , ("me",   "./images/banjo.jpg")
                   , ("cv",   "./cv.html")
                   , ("blog", "./blog/index.html") ] ++ blogRoutes ]

  T.writeFile (output </> ".htaccess") (T.unlines htaccess)
