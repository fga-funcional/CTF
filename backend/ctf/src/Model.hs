{-# LANGUAGE DeriveGeneric #-}
module Model where

import GHC.Generics


data Flag = Flag 
  { idFlag :: Int 
  , title :: String
  , description :: String
  , value :: Int
  , color :: String
  , answer :: String
  , captured :: Bool
  , sectionId :: Int
  } deriving (Show, Generic)

data Section = Section
 { idSection :: Int
 , flags :: [Flag]
 , name :: String
 } deriving (Show, Generic)

data SectionDB = SectionDB
 { idSectionDB :: Int
 , nameDB :: String
 } deriving (Show, Generic)

data Player = Player
  {
    aliasPlayer :: String
    , points :: Int
  } deriving (Show, Generic)