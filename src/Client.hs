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
import DirService
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

uploadFile :: Token -> String -> String -> ClientM (B.ByteString)
uploadFile t p f = do
  let Right sessionKey = makeKey $ B.pack $ sessionKeyC t
  let aesSession = (cipherInit sessionKey) :: AES128
  let encodedPath = B.unpack $ ecbEncrypt aesSession (pad $ B.pack p)
  let encodedFile = B.unpack $ ecbEncrypt aesSession (pad $ B.pack f)
  let uploadForm = BL.toStrict $ encode (UploadForm {ticketU = (ticket t), pathU = encodedPath, fileU = encodedFile})
  file <- uploadApi uploadForm
  return file

dirServiceApi :: B.ByteString -> ClientM B.ByteString
dirServiceApi = client apid

requestShard :: Token -> String -> ClientM (B.ByteString)
requestShard t s = do
  let Right sessionKey = makeKey $ B.pack $ sessionKeyC t
  let aesSession = (cipherInit sessionKey) :: AES128
  let encodedName = B.unpack $ ecbEncrypt aesSession (pad $ B.pack s)
  let addressForm = BL.toStrict $ encode (AddressForm {ticketA = (ticket t), pathA = encodedName})
  shard <- dirServiceApi addressForm
  return shard

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
      -- Ask Auth Server for token
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          -- Ask Directory Server for shard address and port
          manager <- newManager defaultManagerSettings
          shard <- runClientM (requestShard tk "shard1") (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
          case shard of
            Left err -> putStrLn "Could not get correct shard info from Dir. Service"
            Right sh -> do
              let Right sessionKey = makeKey $ B.pack $ sessionKeyC tk
              let aesSession = (cipherInit sessionKey) :: AES128
              let rawConfig = unpad $ ecbDecrypt aesSession $ sh
              let Just config = decode (BL.fromStrict $ unpad $ rawConfig) :: Maybe Config
              -- Request the file from shard
              res <- runClientM (downloadFile tk "a.txt") (ClientEnv manager (BaseUrl Http (address config) (port config) ""))
              case res of
                Left err -> putStrLn $ "Could not download file..."
                Right (f) -> do
                  putStrLn $ "Received file: "
                  print $ unpad $ ecbDecrypt aesSession $ f
      startClient

    "upload" -> do
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          manager <- newManager defaultManagerSettings
          shard <- runClientM (requestShard tk "shard1") (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
          case shard of
            Left err -> putStrLn "Could not get correct shard info from Dir. Service"
            Right sh -> do
              let Right sessionKey = makeKey $ B.pack $ sessionKeyC tk
              let aesSession = (cipherInit sessionKey) :: AES128
              let rawConfig = unpad $ ecbDecrypt aesSession $ sh
              let Just config = decode (BL.fromStrict $ unpad $ rawConfig) :: Maybe Config
              res <- runClientM (uploadFile tk "b.txt" "trololo") (ClientEnv manager (BaseUrl Http (address config) (port config) ""))
              case res of
                Left err -> putStrLn $ "Could not upload file..."
                Right (f) -> do
                  putStrLn $ "Server response: "
                  putStrLn $ B.unpack f
      startClient
    _   -> do
      putStrLn "Invalid input."
      startClient