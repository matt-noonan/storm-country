{-# LANGUAGE OverloadedStrings #-}

module Site.Home (home) where

import Util.HTML

-- | The landing page.
home :: HTML
home = HTML $ do
  div_ [ class_ "card bg-light border" ] $ do
    img_ [ class_ "card-img-top", src_ "me" ]
    div_ [ class_ "card-body" ] $ do
      h4_ [ class_ "card-title" ] "Hello!"
      p_ [ class_ "card-text" ] $ do
        "I'm a mathematician, software developer, educator, and banjo player in Ithaca NY, USA."
      p_ $ a_ [ href_ "mailto:matt.noonan@gmail.com" ] "matt.noonan@gmail.com"
      p_ $ a_ [ href_ "https://github.com/matt-noonan" ] "GitHub"
      p_ $ a_ [ href_ "https://twitter.com/BanjoTragedy" ] "Twitter"
      p_ $ a_ [ href_ "https://books.google.com/books?id=kHYpAAAAYAAJ&pg=PA1&lpg=PA1&dq=tess+of+the+storm+country+ithaca&source=bl&ots=OnNNWwmKFW&sig=CNdXPMJXa5nx8tvHsW0EwJIF6P8&hl=en&sa=X&ved=0ahUKEwjVg4Tt8NfYAhXrkeAKHQMsAfg4FBDoAQgzMAI#v=onepage&q&f=false" ] "Storm country?"
