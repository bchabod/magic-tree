{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Auth where

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
import Data.Char (chr)

-- Prepare server secret
serverSecret = B.pack "7od3048S5Z79t84A"
Right sKey = makeKey serverSecret
aesServer :: AES128
aesServer = cipherInit sKey

-- User ID + their secret
users :: [User]
users = [ User 1 "v2VxGDC61jV6E45J"
        , User 2 "E6eAcW89NT13e3xp"]

data User = User
  { userId        :: Int
  , userKey       :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''User)

data Token = Token
  { ticket        :: [Char]
  , sessionKeyC   :: String
  , timeoutC      :: Int
  }
$(deriveJSON defaultOptions ''Token)

data Ticket = Ticket
  { userIdS        :: Int
  , sessionKeyS    :: String
  , timeoutS       :: Int
  }
$(deriveJSON defaultOptions ''Ticket)

type AccessAPI = "request" :> QueryParam "userId" Int :> Get '[OctetStream] B.ByteString

startAuth :: IO ()
startAuth = run 8080 authenticationApp

authenticationApp :: Application
authenticationApp = serve api serverAuth

api :: Proxy AccessAPI
api = Proxy

padData :: B.ByteString -> B.ByteString
padData input = input `B.append` padding
  where
    padding = B.replicate requiredPadding (chr 0)
    requiredPadding = case inputLength `mod` 16 of
      0 -> 0
      x -> 16 - x
    inputLength = B.length input

generateToken :: Int -> String -> Int -> String -> B.ByteString
generateToken id key t s = let ticket = BL.toStrict $ encode (Ticket {userIdS = id, sessionKeyS = s, timeoutS = t})
                               encryptedTicket = ecbEncrypt aesServer (padData ticket)
                               token = BL.toStrict $ encode (Token {ticket = B.unpack $ encryptedTicket, sessionKeyC = s, timeoutC = t})
                               Right userKey = makeKey $ B.pack $ key
                               aesClient = cipherInit userKey :: AES128
                            in ecbEncrypt aesClient (padData token)

serverAuth :: Server AccessAPI
serverAuth id = do
  timeout <- liftIO $ round `fmap` getPOSIXTime
  sessionKey <- liftIO $ getEntropy 16
  return $ case id of
    Nothing -> B.pack "No user ID given"
    Just id -> let filteredUsers = filter (\u -> userId u == id) users in
               case (length $ filteredUsers) of
                 0 -> B.pack "No user found"
                 1 -> generateToken id (userKey $ head $ filteredUsers) (timeout + 60) (B.unpack $ sessionKey)
                 _ -> B.pack "Error: multiple users for that ID"
