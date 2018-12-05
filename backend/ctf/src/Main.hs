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
  } deriving (Show, Generic)

data Section = Section
 { idSection :: Int
 , flags :: [Flag]
 , name :: String
 } deriving (Show, Generic)

data Player = Player
  {
    aliasPlayer :: String
    , points :: Int
  } deriving (Show, Generic)


k= "8320987112741390144276341183223364380754172606361245952449277696409600000000000000"
example_flags =
  [ Flag 1 "Fibonacci" "Qual o quinto número de fibonacci?" 1 "white" "5" False
    , Flag 2 "Factorial" "Qual fatorial de 60?" 2 "white" k False
  ]

example_flags2 =
  [ Flag 1 "ieaofiaeo" "Qual o quinto número de fibonacci?" 1 "white" "5" False
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
      json sections
    
    get "/sections/:id" $ do
      id <- param "id"
      json (head $ filter (matchesSectionId id) sections)
    
    get "/sections/:id" $ do
      id <- param "id"
      json (filter (matchesFlagId id) example_flags)

    post "/ranking" $ do
      player <- jsonData :: ActionM Player
      json player