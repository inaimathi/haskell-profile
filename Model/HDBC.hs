{-# LANGUAGE OverloadedStrings #-}

module Model.HDBC ( newAccount, updateAccount, accountByName, accountById, getAccounts
                  , newItem, deleteItem, changeItem 
                  , createTables, createTablesSqlite, deleteTables, clearDB
                  , Item(..), Account(..), ItemStatus(..)) where

import Database.HDBC

import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Convertible.Base
import Data.Aeson

import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

---------- Item-Related types
data ItemStatus = Need | Got deriving (Eq, Ord, Enum, Read, Show)

data Item = Item { itemName :: String, 
                   itemComment :: String, 
                   itemStatus :: ItemStatus, 
                   itemCount :: Integer } deriving (Eq, Ord, Show)

itemFromSql [_,SqlByteString name,SqlByteString comment,SqlByteString status,SqlByteString count] =
  Item { itemName=unpack name, 
         itemComment=unpack comment, 
         itemStatus=read $ unpack status, 
         itemCount=read $ unpack count }

instance ToJSON Item where
  toJSON (Item name comment status count) = object [ "name" .= name
                                                   , "comment" .= comment
                                                   , "status" .= show status
                                                   , "count" .= count 
                                                   ]

---------- Account
data Account = Account { accountId :: Integer
                       , accountName :: String 
                       , accountPassphrase :: ByteString
                       , accountItems :: [Item]
                       } deriving (Eq, Show) 

instance Ord Account where
  a `compare` b = (accountId a) `compare` (accountId b)

accountFromSql [SqlByteString id, SqlByteString name, SqlByteString pass] items =
  Account { accountId=read $ unpack id, accountName=unpack name, accountPassphrase=pass, accountItems=items }
accountFromSql [SqlInt32 id, SqlByteString name, SqlByteString pass] items = 
  Account { accountId=fromIntegral id, accountName=unpack name, accountPassphrase=pass, accountItems=items }

instance ToJSON Account where
  toJSON (Account id name _ items) = object [ "id" .= id
                                            , "name" .= name
                                            , "items" .= items]

---------- Utility
withCommit :: IConnection c => c -> (c -> IO a) -> IO a
withCommit conn fn = do
--  conn <- connectSqlite3 "GoGetDB"
  res <- fn conn
  commit conn
  return res

createTables :: IConnection c => c -> IO [Integer]
createTables conn = withCommit conn q
  where q conn = mapM (\s -> run conn s []) 
            ["CREATE TABLE accounts (id INTEGER PRIMARY KEY AUTO_INCREMENT, name VARCHAR(120), passphrase VARCHAR(250))",
             "CREATE TABLE items (user VARCHAR(120), name VARCHAR(120), comment VARCHAR(120), status VARCHAR(4), count INTEGER)"]

createTablesSqlite :: IConnection c => c -> IO [Integer]
createTablesSqlite conn = withCommit conn q
  where q conn = mapM (\s -> run conn s []) 
            ["CREATE TABLE accounts (id INTEGER PRIMARY KEY, name VARCHAR(120), passphrase VARCHAR(250))",
             "CREATE TABLE items (user VARCHAR(120), name VARCHAR(120), comment VARCHAR(120), status VARCHAR(4), count INTEGER)"]

deleteTables :: IConnection c => c -> IO [Integer]
deleteTables conn = withCommit conn q
  where q conn = do
          mapM (\s -> run conn s []) ["DROP TABLE accounts", "DROP TABLE items"]

clearDB :: IConnection c => c -> IO [Integer]
clearDB conn = withCommit conn q
  where q conn = do
          mapM (\s -> run conn s []) ["DELETE FROM accounts", "DELETE FROM items"]

---------- Insertion Functions
newAccount :: IConnection c => c -> String -> ByteString -> IO Integer
newAccount conn name passphrase = withCommit conn ins
  where ins conn = run conn "INSERT INTO accounts (name, passphrase) VALUES (?, ?)" [toSql name, toSql passphrase]

updateAccount acct = True

newItem :: IConnection c => c -> String -> Item -> IO Integer
newItem conn userName item = withCommit conn ins
  where ins conn = run conn 
                   "INSERT INTO items (user, name, comment, status, count) VALUES (?, ?, ?, ?, ?)" $
                   [toSql userName, toSql $ itemName item, toSql $ itemComment item, toSql . show $ itemStatus item, toSql $ itemCount item]

deleteItem :: IConnection c => c -> String -> Item -> IO Integer
deleteItem conn userName item = withCommit conn del
  where del conn = run conn "DELETE FROM items WHERE name = ?" [ toSql $ itemName item ]                            

changeItem :: IConnection c => c -> String -> Item -> IO Integer
changeItem conn userName item = withCommit conn up
  where up conn = run conn "UPDATE items SET comment=?, status=?, count=? WHERE (user=? AND name=?)" 
                  [toSql $ itemComment item, toSql . show $ itemStatus item, toSql $ itemCount item,
                   toSql userName, toSql $ itemName item]

---------- Query Functions
getAccounts :: IConnection c => c -> IO [[SqlValue]]
getAccounts conn = withCommit conn q
  where q conn = quickQuery' conn "SELECT * FROM accounts" []

getAccountBy conn column value = withCommit conn q
  where qString = "SELECT * FROM accounts WHERE " ++ column ++ " = ?"
        q conn = do
          res <- quickQuery' conn qString [toSql value]
          case res of
            [] -> return $ Nothing
            (u@[_, SqlByteString name, _]:rest) -> do
              items <- getAccountItems conn $ unpack name
              return $ Just $ accountFromSql u items

accountByName :: IConnection c => c -> String -> IO (Maybe Account)
accountByName conn name = getAccountBy conn "name" name

accountById :: IConnection c => c -> Integer -> IO (Maybe Account)
accountById conn id = getAccountBy conn "id" id

getAccountItems :: IConnection c => c -> String -> IO [Item]
getAccountItems conn name = withCommit conn q
  where q conn = do
          res <- quickQuery' conn "SELECT * FROM items WHERE user = ?" [toSql name]
          return $ map itemFromSql res