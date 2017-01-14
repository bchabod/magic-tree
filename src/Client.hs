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
import Lock

clientSecret = B.pack "v2VxGDC61jV6E45J"
clientId = 1
Right cKey = makeKey clientSecret
aesClient :: AES128
aesClient = cipherInit cKey

-- Auth Client
tokenApi :: Maybe Int -> ClientM B.ByteString
tokenApi = client api

requestToken :: ClientM (B.ByteString)
requestToken = do
  token <- tokenApi (Just clientId)
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

-- File Server Client
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

-- Directory Service Client
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

-- Lock Service Client
lockApi :: B.ByteString -> ClientM B.ByteString
releaseApi :: B.ByteString -> ClientM B.ByteString
isFreeApi :: B.ByteString -> ClientM B.ByteString
lockApi :<|> releaseApi :<|> isFreeApi = client apil

lockAction :: (B.ByteString -> ClientM B.ByteString) -> Token -> String -> ClientM (B.ByteString)
lockAction a t p = do
  let Right sessionKey = makeKey $ B.pack $ sessionKeyC t
  let aesSession = (cipherInit sessionKey) :: AES128
  let encodedPath = B.unpack $ ecbEncrypt aesSession (pad $ B.pack p)
  let lockForm = BL.toStrict $ encode (LockForm {ticketLF = (ticket t), pathLF = encodedPath})
  response <- a lockForm
  return response

-- Main client UI
startClient :: IO ()
startClient = do
  putStr "Enter a command: "
  hFlush stdout
  rawLine <- getLine
  let str = words $ rawLine
  if (length str == 0)
    then startClient
  else if (str !! 0 == "quit")
    then return ()
  else if (str !! 0 == "exit")
    then return ()

  else if (str !! 0 == "token")
    then do
      token <- getToken
      case token of
          Nothing -> putStrLn $ "Could not decode token..."
          Just tk -> putStrLn $ "Received token from server with timeout: " ++ show (timeoutC tk)
      startClient

  else if (str !! 0 == "download")
    then do
      token <- getToken -- Ask Auth Server for token
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          -- Ask Directory Server for shard address and port
          manager <- newManager defaultManagerSettings
          shard <- runClientM (requestShard tk (str !! 1)) (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
          case shard of
            Left err -> putStrLn "Could not get correct shard info from Dir. Service"
            Right sh -> do
              let Right sessionKey = makeKey $ B.pack $ sessionKeyC tk
              let aesSession = (cipherInit sessionKey) :: AES128
              let rawConfig = unpad $ ecbDecrypt aesSession $ sh
              let Just config = decode (BL.fromStrict $ unpad $ rawConfig) :: Maybe Config
              -- Request the file from shard
              res <- runClientM (downloadFile tk (str !! 2)) (ClientEnv manager (BaseUrl Http (address config) (port config) ""))
              case res of
                Left err -> putStrLn $ "Could not download file..."
                Right (f) -> do
                  putStrLn $ "Received file: "
                  print $ unpad $ ecbDecrypt aesSession $ f
      startClient

  else if (str !! 0 == "upload")
    then do
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          manager <- newManager defaultManagerSettings
          shard <- runClientM (requestShard tk (str !! 1)) (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
          case shard of
            Left err -> putStrLn "Could not get correct shard info from Dir. Service"
            Right sh -> do
              let Right sessionKey = makeKey $ B.pack $ sessionKeyC tk
              let aesSession = (cipherInit sessionKey) :: AES128
              let rawConfig = unpad $ ecbDecrypt aesSession $ sh
              let Just config = decode (BL.fromStrict $ unpad $ rawConfig) :: Maybe Config
              res <- runClientM (uploadFile tk (str !! 2) (str !! 3)) (ClientEnv manager (BaseUrl Http (address config) (port config) ""))
              case res of
                Left err -> putStrLn $ "Could not upload file..."
                Right (f) -> do
                  putStrLn $ "Server response: "
                  putStrLn $ B.unpack f
      startClient

  else if (str !! 0 == "lock")
    then do
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          manager <- newManager defaultManagerSettings
          res <- runClientM (lockAction lockApi tk (str !! 1)) (ClientEnv manager (BaseUrl Http "localhost" 8084 ""))
          case res of
            Left err -> putStrLn $ "Could not lock file..."
            Right (f) -> putStrLn $ "Locked."
      startClient

  else if (str !! 0 == "release")
    then do
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          manager <- newManager defaultManagerSettings
          res <- runClientM (lockAction releaseApi tk (str !! 1)) (ClientEnv manager (BaseUrl Http "localhost" 8084 ""))
          case res of
            Left err -> putStrLn $ "Could not release file..."
            Right (f) -> putStrLn $ "Released."
      startClient

  else if (str !! 0 == "isfree")
    then do
      token <- getToken
      case token of
        Nothing -> putStrLn $ "Could not decode token..."
        Just tk -> do
          manager <- newManager defaultManagerSettings
          res <- runClientM (lockAction isFreeApi tk (str !! 1)) (ClientEnv manager (BaseUrl Http "localhost" 8084 ""))
          case res of
            Left err -> putStrLn $ "Could not check lock..."
            Right (l) -> do
              putStrLn $ "Server response: "
              putStrLn $ B.unpack l
      startClient
  else do
    putStrLn "Invalid input."
    startClient