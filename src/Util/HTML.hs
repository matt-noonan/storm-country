{-# LANGUAGE OverloadedStrings #-}

module Util.HTML
  ( HTML(..)
  , emptyTag

  , module Lucid
  , makeAttribute
  , renderHtml
  ) where

import Happstack.Lite (ToMessage(..))
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

