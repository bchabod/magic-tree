{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module FileServer where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Control.Monad
import Control.Monad.Trans
import Control.Monad.List

import Data.Time
import Data.Time.Clock.POSIX
import System.Entropy
import Crypto.Cipher
import Crypto.Cipher.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Auth

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

startFile :: IO ()
startFile = run 8081 fileApplication

apif :: Proxy FileAPI
apif = Proxy

fileApplication :: Application
fileApplication = serve apif serverFile

serverFile :: Server FileAPI
serverFile = download
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
                  contents <- liftIO $ readFile ("files/" ++ realPath)
                  let encryptedFile = ecbEncrypt aesSession (pad $ B.pack contents)
                  return encryptedFile
              else do
                liftIO $ putStrLn "Invalid download requst (ticket expired)"
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
                  liftIO $ writeFile ("files/" ++ realPath) (B.unpack realFile)
                  return $ B.pack "OK"
              else do
                liftIO $ putStrLn "Invalid upload requst (ticket expired)"
                return $ B.pack "ERROR"