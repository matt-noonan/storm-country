module Site.Data.Config ( Config(..) ) where

import Data.Map  (Map)
import Data.Text (Text)

import Util.Reloadable
import Util.HTML

import Control.Concurrent.MVar

import Site.Data.Blog

data Config = Config
  { blogCache   :: MVar (Map Text (Reloadable HTML))
  , blogIndex   :: MVar [ BlogEntry ]
  , resumeCache :: Reloadable HTML
  , siteRoot    :: FilePath
  , useTracker  :: Bool
  }

