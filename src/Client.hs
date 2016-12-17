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

import Crypto.Cipher
import Crypto.Cipher.Types
import qualified Data.ByteString.Lazy as BL

import Auth
import FileServer

clientSecret = B.pack "v2VxGDC61jV6E45J"
Right cKey = makeKey clientSecret
aesClient :: AES128
aesClient = cipherInit cKey

tokenApi :: Maybe Int -> ClientM B.ByteString
tokenApi = client api

requestToken :: ClientM (B.ByteString)
requestToken = do
  token <- tokenApi (Just 1)
  return $ ecbDecrypt aesClient token

getToken :: IO (Maybe Token)
getToken = do
  manager <- newManager defaultManagerSettings
  res <- runClientM requestToken (ClientEnv manager (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> do
      return Nothing
    Right (t) -> do
      return (decode (BL.fromStrict $ unpad $ t) :: Maybe Token)

downloadApi :: B.ByteString -> ClientM B.ByteString
uploadApi :: B.ByteString -> ClientM B.ByteString
downloadApi :<|> uploadApi = client apif

downloadFile :: Token -> String -> ClientM (B.ByteString)
downloadFile t p = do
  let Right sessionKey = makeKey $ B.pack $ sessionKeyC t
  let aesSession = (cipherInit sessionKey) :: AES128
  let encodedPath = B.unpack $ ecbEncrypt aesSession (pad $ B.pack p)
  let downloadForm = BL.toStrict $ encode (DownloadForm {ticketD = (ticket t), pathD = encodedPath})
  file <- downloadApi downloadForm
  return file

startClient :: IO ()
startClient = do
  putStr "Enter a command: "
  hFlush stdout
  str <- getLine
  case str of
    "quit" -> return ()
    "exit" -> return ()
    "token" -> do
      token <- getToken
      case token of
          Nothing -> putStrLn $ "Could not decode token..."
          Just tk -> putStrLn $ "Received token from server with timeout: " ++ show (timeoutC tk)
      startClient
    "download" -> do
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          manager <- newManager defaultManagerSettings
          res <- runClientM (downloadFile tk "a.txt") (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
          case res of
            Left err -> putStrLn $ "Could not download file..."
            Right (f) -> do
              putStrLn $ "Received file: "
              let Right sessionKey = makeKey $ B.pack $ sessionKeyC tk
              let aesSession = (cipherInit sessionKey) :: AES128
              print $ unpad $ ecbDecrypt aesSession $ f
    _   -> do
      putStrLn "Invalid input."
      startClient