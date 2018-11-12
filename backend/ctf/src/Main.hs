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
  , captured :: Bool
  } deriving (Show, Generic)


flags =
  [ Flag "Fibonacci" "Try1" 1 "white" False
    , Flag "Collatz" "Try2" 2 "white" False
    , Flag "Factorial" "Try55" 2 "white" False
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
