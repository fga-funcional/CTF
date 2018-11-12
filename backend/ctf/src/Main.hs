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

data Flag = Flag 
  { title :: String
  , description :: String
  , value :: Int
  , color :: String
  , answer :: String
  , captured :: Bool
  } deriving (Show, Generic)


k= "8320987112741390144276341183223364380754172606361245952449277696409600000000000000"
flags =
  [ Flag "Fibonacci" "Qual o quinto número de fibonacci?" 1 "white" "5" False
    , Flag "Factorial" "Qual fatorial de 60?" 2 "white" k False
  ]

urls = fromList
  [ ("Say hello", String "/hello")
  , ("List of flags", String "/flags")
  ]

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
    middleware simpleCors
    get "/" $ do
      json $ Object urls


    get "/hello" $ do
      text "hello world!"

    get "/flags" $ do
      json flags
    
    post "/flags/:id" $ do
      json flags
  