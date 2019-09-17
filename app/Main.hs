#!/usr/bin/env stack
{- stack
   --resolver lts-12.6
   --install-ghc
   runghc
   --package http-client-tls
   --package filepath
   --package aeson
-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

-- You can run it as a bash script as well if you want



module Main where

import System.Environment   
import System.IO  
import Data.List 

import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8
import qualified System.Directory as SD
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status  (statusCode)
import System.FilePath
import System.IO
import Data.String ( fromString )
import Control.Monad (forM, liftM, unless, void)
import Control.Monad.Catch
import Data.Aeson (Value, FromJSON, encode, decode, object, (.=))
import Data.Text hiding (words, writeFile, readFile, unlines)
import Data.Maybe


dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("register", register)  
            , ("login", login)  
            ]  
   
main = do
    -- | creates a manager
    manager <- newManager tlsManagerSettings
    -- | loop that reads input in an interactive way
    loopOver manager


-- | Poor Mans loop
-- reads from stding once the program starts
-- You should create specifinc things for the cust commands
loopOver :: Manager -> IO()
loopOver manager = do
  hSetBuffering stdin LineBuffering
  input <- words <$> getLine
  putStrLn $ show input
  case input of
    -- | Here add your handlers
    ["cust", "register", mail, pwd] -> putStrLn "Do something"
    _ -> putStrLn "Invalid command"
  unless (input == ["quit"]) $ loopOver manager

  
register :: [String] -> IO ()  
register [username, password] = putStrLn $ "register " ++ username ++ " " ++ password
  
login :: [String] -> IO ()  
login [username, password] = do  
    putStrLn $ "login " ++ username ++ " " ++ password


-- Data type for the user requests
-- Make them match with the ones on the backend as well
-- You can write them once in the appropriate folder of your haskell-rest-api project
-- And import them here. Did this out of rushedom

-- data for Json Types
data UserLogin = UserLogin {
  status    :: Int
, message   :: Text
, datafield :: Maybe UserData
} deriving (Show, Generic)
--
data UserData = UserData {
  user :: User
, token :: String
} deriving (Show, Generic)
--
data User = User {
  _id        :: Text
, username  :: Text
} deriving (Show, Generic)

data Customer = Customer {
  name  :: Text
, email :: Text
, phone :: Text
} deriving (Show, Generic)

instance FromJSON UserLogin
instance FromJSON UserData
instance FromJSON User

instance FromJSON Customer


-- | Example of how to make a http request
buildGETRequest :: (MonadThrow m) => String -> String -> m Request
buildGETRequest url tkn =
  do
    req <- parseRequest url
    let fullReq = req {method = "GET"
      , requestHeaders =
        [("x-access-token", C8.pack tkn)]
      }
    return fullReq


-- | Example of how to get a response from a Request type
getResponse:: Request -> Manager -> IO BS.ByteString
getResponse req manager = responseBody <$> httpLbs req manager


-- build functions for Registering a user, encoding/ decoding the request from server
-- also for handling token authentication

-- maybe you will need the fi