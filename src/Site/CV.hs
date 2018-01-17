{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Site.CV ( makeCV ) where

import Util.HTML
import Util.Reloadable
import Control.Monad.IO.Class

import Control.Monad (forM_)
import Data.Yaml     (decodeFileEither)

import qualified Data.Text    as T
import qualified Site.Data.CV as CV

-- | Make a lazy renderer for the CV page.
makeCV :: MonadIO m => FilePath -> m (Reloadable HTML)
makeCV root = onReload $ flip fmap (decodeFileEither $ root ++ "/cv.yaml") $ \case
  Left err -> HTML (toHtml $ show err)
  Right cv -> HTML $ do

    h3_ ("Professional history by category" :: Html ())
    div_ [ id_ "accordion" ] $ do
      div_ [ class_ "card" ] $ do

        forM_ (zip (CV.categories cv) [1..]) $ \(cat, tagN) -> do
          let tag = T.pack ("collapse" ++ show tagN)
              category = CV.catName cat
          div_ [ class_ "card-header" ] $ do
            a_ [ class_ "card-link", href_ ("#" `T.append` tag)
             , makeAttribute "data-toggle" "collapse"
             , makeAttribute "data-parent" "#accordion"
             ] $ do div_ [ class_ "row" ] $ do
                      div_ [ class_ "col-md-6" ] $
                        div_ [ class_ "text-left" ] (toHtml category)
                      div_ [ class_ "col-md-6" ] $
                        div_ [ class_ "small text-right" ] (toHtml $ CV.catDuration cat)
                        
          div_ [ class_ "collapse", id_ tag ] $ do
            div_ [ class_ "card-body" ] $ do
              forM_ (CV.history cv) $ \job -> do
                let allTasks = concatMap CV.tasks (CV.positions job)
                forM_ (filter ((== category) . CV.category) allTasks) $ \task -> do
                
                  div_ [ class_ "row" ] $ do
                    div_ [ class_ "col-md-7" ] $ do
                      h6_ (toHtml $ CV.task task)
                    div_ [ class_ "col-md-5 text-right" ] $ do
                      span_ [ class_ "small" ] $ do
                        toHtml (CV.organization job)
                        ", "
                        toHtml (CV.when task)
                
                  p_ (toHtml $ CV.description task)

    div_ [ class_ "text-center" ] $ h4_ ("❧" :: Html ())
    
    h3_ ("Skills" :: Html ())
    div_ [ class_ "card" ] $ do
      div_ [ class_ "card-body" ] $ do
        forM_ (CV.skills cv) $ \sk -> do
          p_ $ do h5_ (toHtml $ CV.skillCategory sk)
                  forM_ (CV.skill sk) (p_ . toHtml)

    div_ [ class_ "text-center" ] $ h4_ ("❧" :: Html ())
    
    h3_ ("Publications" :: Html ())
    div_ [ class_ "card" ] $ do
     div_ [ class_ "card-body" ] $ do
       forM_ (CV.publications cv) $ \pub -> p_ $ do
         let name = toHtml (CV.pubTitle pub)
         span_ [ class_ "font-weight-bold" ] $ case CV.url pub of
           Just url -> a_ [ href_ url ] name
           Nothing  -> name
         ", " >> toHtml (CV.pubLoc pub) 
         span_ [ class_ "small" ] $ toHtml (" (" `T.append` CV.pubDate pub `T.append` ")")

    div_ [ class_ "text-center" ] $ h4_ ("❧" :: Html ())
    
    h3_ ("Education" :: Html ())
    div_ [ class_ "card" ] $ do
      div_ [ class_ "card-body" ] $ do
        forM_ (CV.education cv) $ \edu -> p_ $ do
          span_ [ class_ "font-weight-bold" ] (toHtml $ CV.degree edu)
          " " >> toHtml (CV.subject edu) >> ", "
          toHtml (CV.issuedBy edu) >> " (" >> toHtml (CV.issuedOn edu) >> "). "
          case CV.details edu of
            Just deets -> toHtml deets
            Nothing    -> ""

