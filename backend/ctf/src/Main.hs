{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import Data.Convertible
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
import Control.Monad.IO.Class
import Model
import Database

matchesSectionId :: Int -> Section -> Bool
matchesSectionId id section = idSection section == id

matchesFlagId :: Int -> Flag -> Bool
matchesFlagId id flag = idFlag flag == id

main = do
  putStrLn "Starting Server..."
  secs <- getAllSections
  
  scotty 3000 $ do
    middleware simpleCors
    
    get "/sections" $ do
      json secs
    
    get "/ranking" $ do
      players <- liftIO $ getPlayers
      json players

    get "/sections/:id" $ do
      id <- param "id"
      json (head $ filter (matchesSectionId id) secs)
    
    post "/ranking" $ do
      -- addHeader "Access-Control-Allow-Origin" "http://localhost:8000"
      player <- jsonData :: ActionM Player
      
      liftIO $  insertPlayer (Player (aliasPlayer player) (points player))
      json player
