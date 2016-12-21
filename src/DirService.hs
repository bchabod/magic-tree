{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module DirService where

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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Auth

data Config = Config
  { shardName        :: [Char]
  , address          :: [Char]
  , port             :: Int
  }
$(deriveJSON defaultOptions ''Config)

data AddressForm = AddressForm
  { ticketA        :: [Char]
  , pathA          :: [Char]
  }
$(deriveJSON defaultOptions ''AddressForm)

type AddressAPI = "getAddress" :> ReqBody '[OctetStream] B.ByteString :> Get '[OctetStream] B.ByteString

startDirService :: IO ()
startDirService = run 8081 dirApplication

apid :: Proxy AddressAPI
apid = Proxy

dirApplication :: Application
dirApplication = serve apid serverDir

serverDir :: Server AddressAPI
serverDir = serveShard
  where
    serveShard :: B.ByteString -> Handler B.ByteString
    serveShard raw = do
      let form = decode (BL.fromStrict $ raw) :: Maybe AddressForm
      case form of
          Nothing -> do
            liftIO $ putStrLn $ "Could not decode address request..."
            return $ B.pack "ERROR"
          Just decodedForm -> do
            let (Just ticket) = decode (BL.fromStrict $ unpad $ ecbDecrypt aesServer $ B.pack $ ticketA decodedForm) :: Maybe Ticket
            timeNow <- liftIO $ round `fmap` getPOSIXTime
            if timeNow < timeoutS ticket
              then do
                let Right sessionKey = makeKey $ B.pack $ sessionKeyS ticket
                let aesSession = (cipherInit sessionKey) :: AES128
                let shard = B.unpack $ unpad $ ecbDecrypt aesSession $ B.pack $ pathA decodedForm
                rawConfigs <- liftIO $ readFile ("config.json")
                let (Just config) = decode (BL.fromStrict $ B.pack $ rawConfigs) :: Maybe [Config]
                let shardConfig = find (\c -> (shardName c) == shard) config
                case shardConfig of
                  Nothing -> do
                    liftIO $ putStrLn "Could not find associated config for this shard"
                    return $ B.pack "ERROR"
                  Just sc -> do
                    let p = port sc
                    let a = address sc
                    let response = BL.toStrict $ encode (Config {shardName = shard, port = p, address = a})
                    let encryptedResponse = ecbEncrypt aesSession (pad $ response)
                    liftIO $ putStrLn $ "Replied to config request for shard: " ++ shard
                    return encryptedResponse
            else do
              liftIO $ putStrLn "Invalid download request (ticket expired)"
              return $ B.pack "ERROR"
