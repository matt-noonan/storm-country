{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Site.Subpage
       ( Subpage(..)
       , subpages
       , subpageRoute
       , subpage
       ) where

import Happstack.Lite

import Util.HTML

import Data.Text (Text)
import qualified Data.Text as T

import Data.String (IsString(..))

import Control.Monad (forM_)

import System.IO.Unsafe
import System.Process

-- | Subpages are used to chunk portions of the site together. The
--   sidebar / menu displays a list of subpages, with the current one
--   highlighted.
data Subpage
    = Home
    | Blog
    | Music
    | CV
    | FractalStream
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A list of all subpages.
subpages :: [Subpage]
subpages = [minBound .. maxBound]

-- | The primary route corresponding to each subpage.
subpageRoute :: IsString str => Subpage -> str
subpageRoute = \case
  Home     -> "home"
  Blog     -> "blog"
  Music    -> "music"
  CV       -> "cv"
  FractalStream -> "fractalstream"
  
-- | Build up a subpage from the contents to render.
subpage :: Subpage -> HTML -> Response
subpage sp (HTML body) = pageTemplate (HTML $ toHtml $ show sp) $ HTML $ do
  div_ [ class_ "container-fluid" ] $ do
    div_ [ class_ "row"] $ do
      div_ [ class_ "col-md-3 col-lg-3 col-xl-3" ] $ do
        nav_ [ class_ "navbar navbar-light d-none d-md-block" ] $ do
          div_ [ class_ "navbar-brand" ]  "Matt Noonan"
          ul_ [ class_ "navbar-nav nav" ] $ do
            forM_ subpages $ \p -> do
              let liClass = if p == sp then "nav-item active bolder" else "nav-item"
              li_ [ class_ liClass ] $ do
                let route = subpageRoute p
                a_ [ class_ "nav-link"
                   , href_ ("/" `T.append` route)
                   ] (toHtml $ show p)
                    
        nav_ [ class_ "navbar navbar-dark bg-secondary d-block d-md-none" ] $ do
          div_ [ class_ "row" ] $ do
            div_ [ class_ "col" ] $ div_ [ class_ "navbar-brand" ]  "Matt Noonan"
            div_ [ class_ "col" ] $ do
              div_ [ class_ "text-right" ] $ do
                button_ [ type_ "button",  class_ "navbar-toggle btn btn-secondary mr-md-auto",
                          makeAttribute "data-toggle" "collapse",
                          makeAttribute "data-target" "#navbar" ] $ do
                  span_ [ class_ "navbar-toggler-icon" ] ""
          div_ [ class_ "navbar-collapse collapse", id_ "navbar" ] $ do
            ul_ [ class_ "navbar-nav nav" ] $ do
              forM_ subpages $ \p -> do
                let liClass = if p == sp then "nav-item active bolder" else "nav-item"
                li_ [ class_ liClass ] $ do
                  let route = subpageRoute p
                  a_ [ class_ "nav-link"
                     , href_ ("/" `T.append` route)
                     ] (toHtml $ show p)
                    
      div_ [ class_ "col-md-8 col-lg-7 col-xl-6" ] body
      div_ [ class_ "col-md-1 col-lg-2 col-xl-3" ] emptyTag

  script_ [ src_         "https://code.jquery.com/jquery-3.2.1.slim.min.js"
          , integrity_   "sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN"
          , crossorigin_ "anonymous" ] emptyTag
  script_ [ src_         "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"
          , integrity_   "sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q"
          , crossorigin_ "anonymous" ] emptyTag
  script_ [ src_         "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/js/bootstrap.min.js"
          , integrity_   "sha384-a5N7Y/aK3qNeh15eJKGWxsqtnX/wWdSZSKp+81YjTmS15nvnvxKHuzaWwXHDli+4"
          , crossorigin_ "anonymous" ] emptyTag
  script_ [ type_        "text/javascript"
          , src_         "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML"
          , async_ "" ] emptyTag

-- | Create a page from a title and body, injecting various scripts and styles.
pageTemplate :: HTML -> HTML -> Response
pageTemplate (HTML title) (HTML body) = toResponse (HTML thePage)
  where
    thePage :: Html ()
    thePage = html_ $ do 
      head_ $ do
        script_ [ async_ ""
                , src_   "https://www.googletagmanager.com/gtag/js?id=UA-96111345-2" ] emptyTag
        script_ [] (toHtmlRaw ("window.dataLayer = window.dataLayer || [];function gtag(){dataLayer.push(arguments);}gtag('js', new Date());gtag('config', 'UA-96111345-2');" :: Text))

        meta_ [ charset_ "utf-8" ]
        meta_ [ name_    "viewport"
              , content_ "width=device-width, initial-scale=1" ]
        title_ title
        link_ [ rel_ "stylesheet"
              , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css"
              , integrity_ "sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy"
              , crossorigin_ "anonymous" ]
        style_ pygstyle
        style_ "code { color:sienna; }"
        style_ "body { font-weight:300; }"
        style_ ".bolder { font-weight:600; }"
        style_ ".code-card { box-shadow: 0 3px 3px 0 rgba(0,0,0,0.2); margin-top: 20px; margin-bottom: 20px; padding: 15px; padding-bottom: 0px; }"
      body_ body

-- | Grab @pygmentize@'s color scheme as CSS.
pygstyle :: Text
pygstyle = unsafePerformIO (T.pack <$> pyg)
  where
    pyg = readProcess "pygmentize" [ "-f", "html"
                                   , "-S", "default"
                                   ] ""
