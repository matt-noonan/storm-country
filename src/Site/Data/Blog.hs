{-# LANGUAGE OverloadedStrings #-}

module Site.Data.Blog
  ( BlogEntry(..)
  , PostDate(..)
  ) where

import Data.Text (Text)
import Data.String (IsString(..))

import Data.Aeson (FromJSON(..), withObject, withText, (.:))
  
data PostDate = PostDate
  { year  :: Int
  , month :: Int
  , day  :: Int }
  deriving (Eq, Ord)

instance Show PostDate where
  show date = toMonthName (month date) ++
          " "  ++ showOrd (day date) ++
          ", " ++ show (year date)

showOrd :: Int -> String
showOrd n = show n ++ tag (abs n)
  where
    tag 1 = "st"
    tag 2 = "nd"
    tag 3 = "rd"
    tag _ = "th"

toMonthName :: IsString a => Int -> a
toMonthName = ([ "January"
               , "February"
               , "March"
               , "April"
               , "May"
               , "June"
               , "July"
               , "August"
               , "September"
               , "October"
               , "November"
               , "December"
               ] !!) . subtract 1
  
instance FromJSON PostDate where
  parseJSON v = withObject "object"
    (\o -> PostDate <$> o .: "year"
                    <*> o .: "month"
                    <*> o .: "day") v
 
data BlogEntry = BlogEntry
  { title  :: Text
  , page   :: Text
  , source :: FilePath
  , tags   :: [ Text ]
  , date   :: PostDate
  }

instance FromJSON BlogEntry where
  parseJSON v = withObject "object"
    (\o -> BlogEntry
           <$> o .: "title"
           <*> o .: "page"
           <*> o .: "source"
           <*> o .: "tags"
           <*> o .: "date") v
 
