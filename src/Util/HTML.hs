{-# LANGUAGE OverloadedStrings #-}

module Util.HTML
  ( HTML(..)
  , emptyTag
  , asText

  , module Lucid
  , makeAttribute
  , renderHtml
  ) where

import Happstack.Lite (ToMessage(..))
import Data.Text.Lazy (toStrict)
import Data.Text      (Text)
import Lucid
import Lucid.Base (makeAttribute)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import Data.String (IsString(..))

newtype HTML = HTML (Html ())

emptyTag :: Html ()
emptyTag = ""

instance ToMessage HTML where
    toContentType _ = "text/html; charset=UTF-8"
    toMessage (HTML x) = renderBS x

instance IsString HTML where
  fromString = HTML . fromString

asText :: HTML -> Text
asText (HTML body) = toStrict (renderText body)
