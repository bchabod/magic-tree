{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lock where

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

data LockForm = LockForm
  { ticketLF          :: [Char]
  , pathLF            :: [Char]
  }
$(deriveJSON defaultOptions ''LockForm)

data Lock = Lock
  { userIdL          :: Int
  , pathL            :: [Char]
  } deriving ( Eq )
$(deriveJSON defaultOptions ''Lock)

type LockAPI = "lock" :> ReqBody '[OctetStream] B.ByteString :> Get '[OctetStream] B.ByteString
          :<|> "release" :> ReqBody '[OctetStream] B.ByteString :> Get '[OctetStream] B.ByteString
          :<|> "isfree" :> ReqBody '[OctetStream] B.ByteString :> Get '[OctetStream] B.ByteString

startLock :: IO ()
startLock = run 8084 lockApplication

apil :: Proxy LockAPI
apil = Proxy

getLock :: B.ByteString -> IO (Maybe Lock)
getLock raw = do
  let (Just form) = decode (BL.fromStrict $ raw) :: Maybe LockForm
  let (Just ticket) = decode (BL.fromStrict $ unpad $ ecbDecrypt aesServer $ B.pack $ ticketLF form) :: Maybe Ticket
  timeNow <- liftIO $ round `fmap` getPOSIXTime
  if timeNow < timeoutS ticket
    then do
      let Right sessionKey = makeKey $ B.pack $ sessionKeyS ticket
      let aesSession = (cipherInit sessionKey) :: AES128
      let realPath = B.unpack $ unpad $ ecbDecrypt aesSession $ B.pack $ pathLF form
      return $ Just (Lock {userIdL = userIdS ticket, pathL = realPath})
  else do
        putStrLn "Invalid lock request (ticket expired)"
        return Nothing

lockApplication :: Application
lockApplication = serve apil serverLock

serverLock :: Server LockAPI
serverLock = lock :<|> release :<|> isfree
    where
      lock :: B.ByteString -> Handler B.ByteString
      lock f = do
        (Just decodedLock) <- liftIO $ getLock f
        liftIO $ putStrLn $ "Received lock request from user " ++ (show $ userIdL decodedLock) ++ " on file " ++ (pathL decodedLock)
        contents <- liftIO $ readFile ("files/locks.json")
        let (Just realContents) = decode (BL.fromStrict $ B.pack $ contents) :: Maybe [Lock]
        let newContents = realContents ++ [decodedLock]
        when (length newContents > 0) $ liftIO $ writeFile ("files/locks.json") $ B.unpack $ BL.toStrict $ encode newContents
        return $ B.pack "OK"

      release :: B.ByteString -> Handler B.ByteString
      release f = do
        (Just decodedLock) <- liftIO $ getLock f
        liftIO $ putStrLn $ "Received release request from user " ++ (show $ userIdL decodedLock) ++ " on file " ++ (pathL decodedLock)
        contents <- liftIO $ readFile ("files/locks.json")
        let (Just realContents) = decode (BL.fromStrict $ B.pack $ contents) :: Maybe [Lock]
        let newContents = filter (\le -> le /= decodedLock) realContents
        when (length realContents > 0) $ liftIO $ writeFile ("files/locks.json") $ B.unpack $ BL.toStrict $ encode newContents
        return $ B.pack "OK"

      isfree :: B.ByteString -> Handler B.ByteString
      isfree f = do
        (Just decodedLock) <- liftIO $ getLock f
        liftIO $ putStrLn $ "Received check question from user " ++ (show $ userIdL decodedLock) ++ " on file " ++ (pathL decodedLock)
        contents <- liftIO $ readFile ("files/locks.json")
        let (Just realContents) = decode (BL.fromStrict $ B.pack $ contents) :: Maybe [Lock]
        let collision = find (\x -> (pathL x) == (pathL decodedLock)) realContents
        case collision of
          Nothing -> return $ B.pack "OK"
          Just collision -> return $ B.pack "NO"
