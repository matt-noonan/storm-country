{-# LANGUAGE OverloadedStrings #-}

module Site.Blog
       ( blogEntry
       , makeBlogIndex
       , refreshBlog
       ) where

import Site.Data.Config
import Site.Data.Blog
import Util.HTML
import Util.Reloadable

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Text.Markdown
import System.IO.Unsafe
import System.Process

import Data.List        (sortOn)
import Data.Ord         (Down(..))
import Data.Yaml        (decodeFile)
import Data.Text        (Text)
import Data.Text.Lazy   (fromStrict)
import Data.Traversable (for)

import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Site.Data.Blog  as B
import qualified Text.Blaze.Html as Blaze
import qualified Data.Map        as M

-- | Re-load the blog index and invalidate the rendered-article cache.
refreshBlog :: MonadIO m => Config -> m ()
refreshBlog config = liftIO $ do
  let yaml = siteRoot config ++ "/blog.yaml"

  entries <- maybe (error "missing blog index") id <$> decodeFile yaml
  let index = filter B.published (sortOn (Down . B.date) entries)
      nexts = Nothing : map Just index
      prevs = drop 1 (map Just index) ++ [Nothing]
      
  cache <- for (zip3 prevs index nexts) $ \(prev, entry, next) -> do
             body <- blogEntry entry (siteRoot config) prev next
             return (B.page entry, body)

  _ <- tryTakeMVar (blogIndex config)
  _ <- tryTakeMVar (blogCache config)

  putMVar (blogCache config) (M.fromList cache)
  putMVar (blogIndex config) index

-- | Generate the blog index page for a given list of entries
makeBlogIndex :: [ BlogEntry ] -> HTML
makeBlogIndex index = HTML $ do
  h2_ ("Index" :: Html ())
  forM_ (filter B.published index) $ \entry -> do
    div_ [ class_ "row" ] $ do
      div_ [ class_ "col-md-9" ] $
        a_ [ href_ ("/blog/" `T.append` B.page entry) ] (toHtml $ B.title entry)
      div_ [ class_ "col-md-3 small" ] (toHtml (show $ B.date entry))

  when (null $ filter B.published index) $
    p_ "There is nothing here yet!"
  

-- | Generate an individual blog page, lazily. The page is created
--   as an initially-empty @Reloadable@, but will not be rendered
--   until it is first requested.
blogEntry :: MonadIO m
          => B.BlogEntry       -- ^ Data for this entry
          -> FilePath              -- ^ Root of blog data directory
          -> Maybe B.BlogEntry -- ^ Data for previous entry, if any
          -> Maybe B.BlogEntry -- ^ Data for next entry, if any
          -> m (Reloadable HTML)
          
blogEntry entry root prev next = do
  
    let mdSettings = def { msBlockCodeRenderer = pygmentize }
        (mainTitle, subTitle0) = T.breakOn ": " (B.title entry)
        subTitle = if T.null subTitle0 then "" else T.drop 2 subTitle0
        
    onReload $ do
      md <- liftIO $ T.readFile (root ++ "/" ++ B.source entry)
      return $ HTML $ do

        -- Next / prev
        div_ [ class_ "row" ] $ do
          div_ [ class_ "col" ] $ do
            div_ [ class_ "text-left" ] $ do
              case prev of
                Just e  -> a_ [ href_ ("/blog/" `T.append` B.page e) ] $ do
                  "◄ " >> toHtml (B.title e)
                Nothing -> ""
          div_ [ class_ "col" ] $ do
            div_ [ class_ "text-right" ] $ do
              case next of
                Just e  ->  a_ [ href_ ("/blog/" `T.append` B.page e) ] $ do
                  toHtml (B.title e) >> " ►"
                Nothing -> ""
              
        -- Heading
        div_ [ class_ "jumbotron jumbotron-fluid text-center bg-info text-white" ] $ do
          h1_ (toHtml mainTitle)
          when (not $ T.null subTitle) (h3_ $ toHtml subTitle)

        -- Tags and publication date
        div_ [ class_ "row" ] $ do
          div_ [ class_ "col-md-8" ] $ do
            div_ [ class_ "text-left" ] $ do
              forM_ (B.tags entry) $ \tag -> do
                span_ [ class_ "badge badge-pill badge-secondary" ] (toHtml tag)
          div_ [ class_ "col-md-4" ] $ do
            div_ [ class_ "font-italic text-right" ] (toHtml $ show (B.date entry))

        -- Body
        toHtmlRaw $ renderHtml $ markdown mdSettings (fromStrict md)

-- | Use @pygmentize@ to do syntax coloring of code fragments.
pygmentize :: Maybe Text -> (Text, Blaze.Html) -> Blaze.Html
pygmentize (Just lang) (src, _) = Blaze.preEscapedToHtml $ renderText block
  where
    block = div_ [ class_ "card code-card bg-light" ] body
    body = unsafePerformIO (toHtmlRaw <$> pyg)
    pyg = T.pack <$> readProcess "pygmentize" [ "-f", "html"
                                              , "-l", T.unpack lang
                                              , "-O", "style=colorful"
                                              ] (T.unpack src)
pygmentize Nothing (src, _) = Blaze.preEscapedToHtml $ renderText block
  where
    block = div_ [ class_ "card code-card bg-light" ] $ do
                (pre_ $ code_ $ toHtml src)

