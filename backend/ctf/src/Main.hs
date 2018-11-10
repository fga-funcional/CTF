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

instance ToJSON Flag
instance FromJSON Flag

data Flag = Flag 
  { title :: String
  , description :: String
  , value :: Int
  , color :: String
  } deriving (Show, Generic)


flags =
  [ Flag "Fibonacci" "Find some fibonacci numbers" 1 "white"
    , Flag "Collatz" "Sequence of numbers" 2 "white"
    , Flag "Factorial" "Blah" 2 "white"
  ]

urls = fromList
  [ ("Say hello", String "/hello")
  , ("List of flags", String "/flags")
  ]

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
  
    get "/" $ do
      json $ Object urls


    get "/hello" $ do
      text "hello world!"

    get "/flags" $ do
      json flags
