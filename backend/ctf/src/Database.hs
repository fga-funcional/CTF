{-# LANGUAGE OverloadedStrings #-}
module Database where

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import Model

instance ToJSON Flag
instance FromJSON Flag
instance ToJSON Section
instance FromJSON Section
instance ToJSON Player
instance FromJSON Player


instance FromRow Flag where
    fromRow = Flag <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
  
instance FromRow SectionDB where
    fromRow = SectionDB <$> field <*> field

instance FromRow Player where
    fromRow = Player <$> field <*> field

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

getPlayers :: IO [Player]
getPlayers = do
  conn <- open "ctf.db"
  r <- query_ conn "SELECT * from player" :: IO [Player]
  close conn
  return r

insertPlayer :: Player -> IO ()
insertPlayer p = do
  conn <- connectSqlite3 "ctf.db"
  let point = points p
  let name = aliasPlayer p
  stmt <- prepare conn "INSERT INTO player VALUES(?, ?)"
  Database.HDBC.execute stmt [toSql $ show name, toSql $ toInteger point]
  commit conn
  disconnect conn

getFirstFlag :: IO ()
getFirstFlag = do
  conn <- open "ctf.db"
  r <- query_ conn "SELECT * from flag" :: IO [Flag]
  mapM_ print r
  close conn