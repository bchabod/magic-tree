{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Client where

import System.IO
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import qualified Data.ByteString.Char8 as B
import Auth

import Crypto.Cipher
import Crypto.Cipher.Types
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)

clientSecret = B.pack "v2VxGDC61jV6E45J"
Right cKey = makeKey clientSecret
aesClient :: AES128
aesClient = cipherInit cKey

getToken :: Maybe Int -- User ID
         -> ClientM B.ByteString
getToken = client api

requestToken :: ClientM (B.ByteString)
requestToken = do
  token <- getToken (Just 1)
  return $ ecbDecrypt aesClient token

startClient :: IO ()
startClient = do
    putStr "Enter a command: "
    hFlush stdout
    str <- getLine
    case str of
        "quit" -> return ()
        "exit" -> return ()
        "token" -> do
            manager <- newManager defaultManagerSettings
            res <- runClientM requestToken (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
            case res of
                Left err -> do
                    putStrLn $ "Error: " ++ show err
                    startClient
                Right (t) -> do
                    let token = decode (BL.fromStrict $ fst . B.spanEnd (== (chr 0)) $ t) :: Maybe Token
                    case token of
                        Nothing -> putStrLn $ "Could not decode token" ++ show t
                        Just tk -> putStrLn $ "Received token from server with timeout: " ++ show (timeoutC tk)
                    startClient
        _   -> do
          putStrLn "Invalid input."
          startClient