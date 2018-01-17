{-# LANGUAGE OverloadedStrings #-}

module Site.Data.CV
  ( CV          (..)
  , Job         (..)
  , Position    (..)
  , Task        (..)
  , Skill       (..)
  , Publication (..)
  , Education   (..)
  , Category    (..)
  ) where

import Prelude hiding (lines)
import Data.Text (Text, lines)

import Data.String (IsString(..))

import Data.Aeson (FromJSON(..), withObject, withText, (.:), (.:?))

data CV = CV
  { history      :: [Job]
  , categories   :: [Category]
  , skills       :: [Skill]
  , publications :: [Publication]
  , honors       :: [Text]
  , education    :: [Education]
  }

instance FromJSON CV where
  parseJSON v = withObject "object"
    (\o -> CV <$> o .: "history"
              <*> o .: "work-categories"
              <*> o .: "skills"
              <*> o .: "publications"
              <*> o .: "honors"
              <*> o .: "education") v

data Category = Category
  { catName :: Text
  , catDuration :: Text }

instance FromJSON Category where
  parseJSON v = withObject "category"
    (\o -> Category <$> o .: "category" <*> o .: "duration") v
    
data Job = Job
  { organization :: Text
  , location     :: Text
  , jobTimeframe :: Text
  , positions    :: [Position]
  }

instance FromJSON Job where
  parseJSON v = withObject "job"
    (\o -> Job <$> o .: "organization"
               <*> o .: "location"
               <*> o .: "when"
               <*> o .: "positions") v

data Position = Position
  { title        :: Text
  , posTimeframe :: Text
  , tasks        :: [Task]
  }

instance FromJSON Position where
  parseJSON v = withObject "position"
    (\o -> Position <$> o .: "title"
                    <*> o .: "when"
                    <*> o .: "tasks") v
    
data Task = Task
  { task        :: Text
  , when        :: Text
  , category    :: Text
  , description :: Text
  }

instance FromJSON Task where
  parseJSON v = withObject "task"
    (\o -> Task <$> o .: "task"
                <*> o .: "when"
                <*> o .: "category"
                <*> o .: "description") v

data Skill = Skill
  { skillCategory :: Text
  , skill         :: [Text] }

instance FromJSON Skill where
  parseJSON v = withObject "skill"
    (\o -> Skill <$> o .: "category"
                 <*> (fmap lines $ o .: "skill")) v

data Publication = Publication
  { pubTitle :: Text
  , url      :: Maybe Text
  , pubDate  :: Text
  , pubLoc   :: Text
  }

instance FromJSON Publication where
  parseJSON v = withObject "publication"
    (\o -> Publication <$> o .:  "title"
                       <*> o .:? "url"
                       <*> o .:  "when"
                       <*> o .:  "where") v

data Education = Education
  { degree   :: Text
  , subject  :: Text
  , issuedBy :: Text
  , issuedOn :: Text
  , details  :: Maybe Text
  }

instance FromJSON Education where
  parseJSON v = withObject "education"
    (\o -> Education <$> o .: "degree"
                     <*> o .: "subject"
                     <*> o .: "from"
                     <*> o .: "when"
                     <*> o .:? "details") v
