{-# LANGUAGE OverloadedStrings #-}

module Site.Static (buildSite) where

import Site.Data.Config
import Util.HTML
import Util.Reloadable

import Site.Subpage
import Site.Home
import Site.Blog
import Site.CV
import Site.Data.Blog

import Control.Monad
import Control.Concurrent.MVar
import System.Process
import System.Directory
import System.FilePath
import Happstack.Server (Response, rsBody)

import Data.Text              (Text)
import Data.Text.Encoding     (decodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Data.Yaml              (decodeFile)

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
                      , useTracker  = True
                      }
  refreshBlog config

  -- Build the directory structure
  mapM_ (createDirectoryIfMissing True) [ output </> "images"
                                        , output </> "blog" ]

  images <- listDirectory (root </> "images")
  forM_ images $ \img -> copyFile (root   </> "images" </> takeFileName img)
                                  (output </> "images" </> takeFileName img)

  -- Render the homepage
  T.writeFile (output </> "index.html") (body $ subpage config Home home)

  -- Render the CV page
  cvBody <- access cv
  T.writeFile (output </> "cv.html") (body $ subpage config CV cvBody)

  -- Render the blog index page
  index <- readMVar (blogIndex config)
  T.writeFile (output </> "blog" </> "index.html")
              (body $ subpage config Blog $ makeBlogIndex index)

  -- Render the individual blog entries
  entries <- readMVar (blogCache config)
  forM_ (M.toList entries) $ \(name, entry) -> do
    contents <- access entry
    T.writeFile (output </> "blog" </> (T.unpack name ++ ".html"))
                (body $ subpage config Blog contents)

  -- Render any hidden blog entries
  let yaml = siteRoot config ++ "/blog.yaml"
  addlEntries <- maybe (error "missing blog index") id <$> decodeFile yaml
  addlRoutes <- forM (filter hidden addlEntries) $ \entry -> do
    let name = page entry
    liftIO (putStrLn $ "additional entry: " ++ T.unpack name)
    contents <- access =<< blogEntry entry (siteRoot config) Nothing Nothing
    T.writeFile (output </> "blog" </> (T.unpack name ++ ".html"))
                (body $ subpage config Blog contents)
    return ("blog/" `T.append` name,
            "./blog/" `T.append` name `T.append` ".html")

  -- Render the RSS feed
  T.writeFile (output </> "blog.rss") (rss index)
  
  -- Set up .htaccess file
  let blogRoutes = map (\x -> ("blog/" `T.append` x,
                               "./blog/" `T.append` x `T.append` ".html"))
                       (M.keys entries)
      htaccess = [ "RewriteEngine On" ] ++
        [ "RewriteRule ^" `T.append` x `T.append` "$ " `T.append` y
        | (x,y) <- [ ("home", "./index.html")
                   , ("me",   "./images/banjo.jpg")
                   , ("cv",   "./cv.html")
                   , ("rss",  "./blog.rss")
                   , ("blog", "./blog/index.html") ]
                   ++ blogRoutes ++ addlRoutes ]

  T.writeFile (output </> ".htaccess") (T.unlines htaccess)
