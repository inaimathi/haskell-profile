module Main where

import Control.Monad.Random
import Data.Char
import Data.Maybe (fromJust)

import Criterion.Main
import Control.Monad.Trans  (liftIO)
import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)
import qualified Data.ByteString.Char8 as BS

import Database.MongoDB (Pipe)

import Data.Acid (AcidState, openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Advanced (update', query')

import Database.HDBC (IConnection, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC.MySQL (connectMySQL, defaultMySQLConnectInfo)
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import qualified Model.AcidState as Acid
import qualified Model.HDBC as HDBC
import qualified Model.Mongo as Mongo

main = do
  acid <- openLocalState Acid.initialDB
  mongo <- Mongo.newConn
  sqlite <- Database.HDBC.Sqlite3.connectSqlite3 "GoGetDB"
--  mysql <- Database.HDBC.MySQL.connectMySQL defaultMySQLConnectInfo
  defaultMain 
    [ 
      benchBlock "AcidState" acid 
      (insertUserAcid, insertItemAcid, getUserAcid, (\acid -> query' acid Acid.GetAccounts)),
      bgroup "HDBC" 
      [ 
--        hdbcBlock "MySQL" mysql,
        hdbcBlock "SQLite" sqlite
      ], 
      benchBlock "MongoDB" mongo 
      (insertUserMongo, insertItemMongo, getUserMongo, Mongo.getAccounts)
    ]
  Mongo.close mongo
  Database.HDBC.disconnect sqlite
--  Database.HDBC.disconnect mysql
  createCheckpointAndClose acid

benchBlock dbName conn (insert, insertItem, getUser, listUsers) =
  bgroup dbName 
  [
    bgroup "~50000 users (large)" 
    [
      bench "New Account" . whnfIO $ insert conn "password"
    , bench "List Accounts" . whnfIO $ listUsers conn
    , bench "Single Item Insert" . whnfIO $ insertItem conn 45678
    , bench "Account Query" . whnfIO $ getUser conn 45678
    ]
  -- , bgroup "~1000 users (medium)" 
  --   [
  --     bench "Some New Accounts" . whnfIO $ mapM (insert conn) $ replicate 10 "password"
  --   , bench "List Accounts" . whnfIO $ listUsers conn
  --   , bench "Single Item Insert" . whnfIO $ insertItem conn 789
  --   , bench "Account Query" . whnfIO $ getUser conn 789
  --   ]
  -- , bgroup "~100000 users (large)"
  --   [
  --     bench "Fuckton of New Accounts" . whnfIO $ mapM (insert conn) $ replicate 1000 "password"
  --   , bench "List Accounts" . whnfIO $ listUsers conn
  --   , bench "Single Item Insert" . whnfIO $ insertItem conn 56789
  --   , bench "Account Query" . whnfIO $ getUser conn 56789
  --   ]
  ]

----- AcidState
insertUserAcid :: AcidState Acid.GoGetDB -> String -> IO Acid.Account
insertUserAcid db pass = do
  name <- randString 80
  encPass <- encryptPass defaultParams . Pass $ BS.pack pass
  acct <- update' db . Acid.NewAccount name $ unEncryptedPass encPass
  return acct

insertItemAcid :: AcidState Acid.GoGetDB -> Integer -> IO Acid.Item
insertItemAcid db userId = do
  name <- randString 80
  user <- query' db $ Acid.AccountById userId
  let item = Acid.Item { Acid.itemName=name, Acid.itemComment="A comment", Acid.itemCount=5, Acid.itemStatus=Acid.Need }
  update' db $ Acid.NewItem (fromJust user) item
  return item

getUserAcid :: AcidState Acid.GoGetDB -> Integer -> IO Acid.Account
getUserAcid db userId = do
  acct <- query' db $ Acid.AccountById userId
  return $ fromJust acct

----- Mongo
insertUserMongo :: Pipe -> String -> IO Mongo.Account
insertUserMongo conn pass = do
  name <- randString 80
  encPass <- encryptPass defaultParams . Pass $ BS.pack pass
  Mongo.newAccount conn name . BS.unpack $ unEncryptedPass encPass

insertItemMongo :: Pipe -> Integer -> IO Mongo.Item
insertItemMongo conn userId = do
  name <- randString 80
  user <- Mongo.accountById conn userId
  let item = Mongo.Item { Mongo.itemName=name, Mongo.itemComment="A comment", Mongo.itemCount=5, Mongo.itemStatus=Mongo.Need }
  Mongo.newItem conn (Mongo.accountName $ fromJust user) item
  return item

getUserMongo conn userId = do
  acct <- Mongo.accountById conn userId
  return $ fromJust acct

----- HDBC
insertUserHDBC :: IConnection c => c -> String -> IO Integer
insertUserHDBC conn pass = do
  name <- randString 80
  encPass <- encryptPass defaultParams . Pass $ BS.pack pass
  HDBC.newAccount conn name $ unEncryptedPass encPass

insertItemHDBC :: IConnection c => c -> Integer -> IO Integer
insertItemHDBC conn userId = do
  name <- randString 80
  user <- HDBC.accountById conn userId
  let item = HDBC.Item { HDBC.itemName=name, HDBC.itemComment="A comment", HDBC.itemCount=5, HDBC.itemStatus=HDBC.Need }
  HDBC.newItem conn (HDBC.accountName $ fromJust user) item

getUserHDBC :: IConnection c => c -> Integer -> IO HDBC.Account
getUserHDBC conn userId = do
  acct <- HDBC.accountById conn userId
  return $ fromJust acct

---------- Utility
clearAllDB = do
  mongo <- Mongo.newConn
  sqlite <- Database.HDBC.Sqlite3.connectSqlite3 "GoGetDB"
  mysql <- Database.HDBC.MySQL.connectMySQL defaultMySQLConnectInfo
  
  Mongo.clearDB mongo
  HDBC.deleteTables sqlite
  HDBC.createTablesSqlite sqlite
  HDBC.deleteTables mysql
  HDBC.createTables mysql

  Mongo.close mongo
  Database.HDBC.disconnect sqlite
  Database.HDBC.disconnect mysql

pushUsers conn insert num = do
  putStrLn "Inserting users..."
  mapM_ insBlock $ replicate num $ replicate 1000 "password"
  putStrLn $ concat ["Inserted ", show $ 1000*num, " user records"]
  where insBlock passes = do
          mapM_ (insert conn) passes
          putStrLn "Inserted 1000..."

randString :: Int -> IO String
randString len = do
  values <- evalRandIO $ sequence $ replicate len randChar
  return $ map chr values
  where randChar = getRandomR (48, 122)

hdbcBlock :: IConnection c => String -> c -> Benchmark
hdbcBlock = flip3 benchBlock (insertUserHDBC, insertItemHDBC, getUserHDBC, HDBC.getAccounts)

flip3 :: (a -> b -> c -> res) -> c -> a -> b -> res
flip3 fn z x y = fn x y z