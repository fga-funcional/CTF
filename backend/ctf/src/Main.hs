{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import qualified Data.Char as C
import qualified Data.Text as T
import Data.HashMap.Strict (fromList)
import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty
import Network.Wai.Middleware.Cors
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

instance ToJSON Flag
instance FromJSON Flag
instance ToJSON Section
instance FromJSON Section
instance ToJSON Player
instance FromJSON Player

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

instance FromRow Flag where
  fromRow = Flag <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow SectionDB where
    fromRow = SectionDB <$> field <*> field
  

getFirstFlag :: IO ()
getFirstFlag = do
  conn <- open "ctf.db"
  r <- query_ conn "SELECT * from flag" :: IO [Flag]
  mapM_ print r
  close conn

mapSectionDB :: SectionDB -> [Flag] -> Section
mapSectionDB secDB fs = 
  do
    Section (idSectionDB (secDB)) (filter (\x -> sectionId x == idSectionDB (secDB)) fs) (nameDB (secDB))

  
getAllSections :: IO [Section]
getAllSections = do
  conn <- open "ctf.db"
  r <- query_ conn "SELECT * from section" :: IO [SectionDB]
  x <- query_ conn "SELECT * from flag" :: IO [Flag]
  let s = [(mapSectionDB y x) | y  <- r ] -- Mapeia os resultados do Banco pra Section
  return s
 
data Player = Player
  {
    aliasPlayer :: String
    , points :: Int
  } deriving (Show, Generic)


k= "8320987112741390144276341183223364380754172606361245952449277696409600000000000000"
example_flags =
  [ Flag 1 "Fibonacci" "Qual o quinto número de fibonacci?" 1 "white" "5" False 1
    , Flag 2 "Factorial" "Qual fatorial de 60?" 2 "white" k False 1
  ]

example_flags2 =
  [ Flag 1 "ieaofiaeo" "Qual o quinto número de fibonacci?" 1 "white" "5" False 1
  ]
  
sections = 
  [
    Section 1 example_flags "Matematica",
    Section 2 example_flags2 "Português"
  ]

urls = fromList
  [ ("Say hello", String "/hello")
  , ("List of flags", String "/flags")
  , ("List of sections", String "/sections")
  ]

matchesSectionId :: Int -> Section -> Bool
matchesSectionId id section = idSection section == id

matchesFlagId :: Int -> Flag -> Bool
matchesFlagId id flag = idFlag flag == id

main = do
  putStrLn "Starting Server..."
  secs <- getAllSections
  scotty 3000 $ do
    middleware simpleCors
    get "/" $ do
      json $ Object urls


    get "/hello" $ do
      text "hello world!"

    get "/flags" $ do
      json example_flags
    
    post "/flags/:id" $ do
      json example_flags
    
    get "/sections" $ do
      json secs
    
    get "/sections/:id" $ do
      id <- param "id"
      json (head $ filter (matchesSectionId id) secs)
    
    post "/ranking" $ do
      player <- jsonData :: ActionM Player
      json player