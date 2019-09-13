module Main where

import System.Environment   
import System.IO  
import Data.List  

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("register", register)  
            , ("login", login)  
            ]  
   
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  
  
register :: [String] -> IO ()  
register [username, password] = putStrLn $ "register " ++ username ++ " " ++ password
  
login :: [String] -> IO ()  
login [username, password] = do  
    putStrLn $ "login " ++ username ++ " " ++ password
