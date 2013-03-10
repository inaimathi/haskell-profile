{-# LANGUAGE OverloadedStrings #-}

module Model.Mongo ( newAccount, accountByName, accountById, getAccounts
                   , newItem, deleteItem, changeItem 
                   , clearDB, newConn, close
                   , Account(..), Item(..), ItemStatus(..)) where

import Database.MongoDB
import Control.Monad.Trans (liftIO)

import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Aeson

import qualified Data.Text as Text

import Crypto.Scrypt (EncryptedPass(..), Pass(..), defaultParams, encryptPass, verifyPass)

---------- Item-Related types
data ItemStatus = Need | Got deriving (Eq, Ord, Enum, Read, Show)

data Item = Item { itemName :: String, 
                   itemComment :: String, 
                   itemStatus :: ItemStatus, 
                   itemCount :: Integer } deriving (Eq, Ord, Show)

itemFromMongo [nameField, commentField, statusField, countField] = 
  Item { itemName =  name, itemComment = comment, itemStatus = status, itemCount = count }
  where Database.MongoDB.String n = value nameField
        name = Text.unpack n
        Database.MongoDB.String c = value commentField
        comment = Text.unpack c
        Database.MongoDB.String s = value statusField
        status = read $ Text.unpack s
        Int32 co = value countField
        count = fromIntegral co

mongoFromItem item = ["name" =: (itemName item), "comment" =: (itemComment item), "status" =: (show $ itemStatus item), "count" =: (itemCount item)]

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

accountFromMongo [uuidField, idField, itemsField, nameField, passphraseField] =
  accountFromMongo [idField, itemsField, nameField, passphraseField]
accountFromMongo [idField, itemsField, nameField, passphraseField] =
  Account { accountId=id, accountName=name, accountPassphrase=pass, accountItems=items }
  where Int32 userId = value idField
        id = fromIntegral userId
        Database.MongoDB.String n = value nameField
        name = Text.unpack n
        Database.MongoDB.String p = value passphraseField
        pass = pack $ Text.unpack p
        Database.MongoDB.Array is = value itemsField
        items = map (\(Doc i) -> itemFromMongo i) is

instance ToJSON Account where
  toJSON (Account id name _ items) = object [ "id" .= id
                                            , "name" .= name
                                            , "items" .= items]
                                     
---------- Utility
newConn :: IO Pipe
newConn = runIOE $ connect $ host "127.0.0.1"

run pipe act = access pipe master "GoGetDB" act

clearDB pipe = run pipe $ delete $ select [] "accounts"

---------- Insertion Functions
newAccount :: Val v => Pipe -> v -> v -> IO Account
newAccount pipe name pass = do
  Right ct <- run pipe $ count $ select [] "accounts"
  let u = ["id" =: succ ct, "items" =: ([] :: [Database.MongoDB.Value]), "name" =: name, "passphrase" =: pass]
  run pipe $ insert "accounts" u
  return $ accountFromMongo u

newItem pipe uName item = run pipe $ modify (select ["name" =: uName] "accounts") ["$push" =: ["items" =: (mongoFromItem item)]]

deleteItem pipe uName item = run pipe $ modify (select ["name" =: uName] "accounts") ["$pull" =: ["items" =: (mongoFromItem item)]]

changeItem pipe uName item = True

---------- Query Functions
getAccounts pipe = do
  res <- run pipe $ find (select [] "accounts") >>= rest
  return $ case res of
    Right accounts -> accounts
    _ -> []

getAccountBy pipe property value = do
  res <- run pipe $ findOne $ select [property =: value] "accounts"
  return $ case res of
    Right (Just acct) -> Just $ accountFromMongo acct
    _ -> Nothing

accountById :: Pipe -> Integer -> IO (Maybe Account)
accountById pipe id = getAccountBy pipe "id" id

accountByName :: Pipe -> String -> IO (Maybe Account)
accountByName pipe name = getAccountBy pipe "name" name