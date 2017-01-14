{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module FileServer where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.List

import Data.Time
import Data.Time.Clock.POSIX
import System.Entropy
import Crypto.Cipher
import Crypto.Cipher.Types
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Auth
import DirService

data DownloadForm = DownloadForm
  { ticketD        :: [Char]
  , pathD          :: [Char]
  }
$(deriveJSON defaultOptions ''DownloadForm)

data UploadForm = UploadForm
  { ticketU        :: [Char]
  , pathU          :: [Char]
  , fileU          :: [Char]
  }
$(deriveJSON defaultOptions ''UploadForm)

type FileAPI = "download" :> ReqBody '[OctetStream] B.ByteString :> Get '[OctetStream] B.ByteString
      :<|> "upload" :> ReqBody '[OctetStream] B.ByteString :> Get '[OctetStream] B.ByteString

startFile :: String -> IO ()
startFile shard = do
  rawConfigs <- readFile ("config.json")
  let (Just config) = decode (BL.fromStrict $ B.pack $ rawConfigs) :: Maybe [Config]
  let shardConfig = find (\c -> (shardName c) == shard) config
  case shardConfig of
    Nothing -> putStrLn "Could not find associated config for this shard"
    Just sc -> run (port sc) $ fileApplication shard

apif :: Proxy FileAPI
apif = Proxy

fileApplication :: String -> Application
fileApplication shard = serve apif $ serverFile shard

serverFile :: String -> Server FileAPI
serverFile shard = download
    :<|> upload

    where
      download :: B.ByteString -> Handler B.ByteString
      download f = do
        let form = decode (BL.fromStrict $ f) :: Maybe DownloadForm
        case form of
            Nothing -> do
              liftIO $ putStrLn $ "Could not decode download request..."
              return $ B.pack "ERROR"
            Just decodedForm -> do
              let (Just ticket) = decode (BL.fromStrict $ unpad $ ecbDecrypt aesServer $ B.pack $ ticketD decodedForm) :: Maybe Ticket
              timeNow <- liftIO $ round `fmap` getPOSIXTime
              if timeNow < timeoutS ticket
                then do
                  let Right sessionKey = makeKey $ B.pack $ sessionKeyS ticket
                  let aesSession = (cipherInit sessionKey) :: AES128
                  let realPath = B.unpack $ unpad $ ecbDecrypt aesSession $ B.pack $ pathD decodedForm
                  contents <- liftIO $ DB.readFile ("files/" ++ shard ++ "/" ++ realPath)
                  let encryptedFile = ecbEncrypt aesSession (pad $ contents)
                  liftIO $ putStrLn $ "Successfully served file: " ++ shard ++ "/" ++ realPath
                  return encryptedFile
              else do
                liftIO $ putStrLn "Invalid download request (ticket expired)"
                return $ B.pack "ERROR"

      upload :: B.ByteString -> Handler B.ByteString
      upload f = do
        let form = decode (BL.fromStrict $ f) :: Maybe UploadForm
        case form of
            Nothing -> do
              liftIO $ putStrLn $ "Could not decode upload request..."
              return $ B.pack "ERROR"
            Just decodedForm -> do
              let (Just ticket) = decode (BL.fromStrict $ unpad $ ecbDecrypt aesServer $ B.pack $ ticketU decodedForm) :: Maybe Ticket
              timeNow <- liftIO $ round `fmap` getPOSIXTime
              if timeNow < timeoutS ticket
                then do
                  let Right sessionKey = makeKey $ B.pack $ sessionKeyS ticket
                  let aesSession = (cipherInit sessionKey) :: AES128
                  let realPath = B.unpack $ unpad $ ecbDecrypt aesSession $ B.pack $ pathU decodedForm
                  let realFile = unpad $ ecbDecrypt aesSession $ B.pack $ fileU decodedForm
                  liftIO $ writeFile ("files/" ++ shard ++ "/" ++ realPath) (B.unpack realFile)
                  liftIO $ putStrLn $ "Successfully handled upload request for file: " ++ shard ++ "/" ++ realPath
                  return $ B.pack "OK"
              else do
                liftIO $ putStrLn "Invalid upload request (ticket expired)"
                return $ B.pack "ERROR"