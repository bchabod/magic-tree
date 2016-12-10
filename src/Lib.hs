{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
  ( startAuth
  ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Data.Time
import Data.Time.Clock.POSIX

import Crypto.Cipher
import Crypto.Cipher.Types
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr)

-- Prepare server secret
serverSecret = B.pack "7od3048S5Z79t84A"
Right key = makeKey serverSecret
aesServer :: AES128
aesServer = cipherInit key

-- User ID + their secret
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

-- type AccessAPI = "request" :> QueryParam "userId" Int :> QueryParam "server" String :> Get '[PlainText] String
type AccessAPI = "request" :> QueryParam "userId" Int :> Get '[OctetStream] B.ByteString

startAuth :: IO ()
startAuth = run 8080 authenticationServer

authenticationServer :: Application
authenticationServer = serve api server

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

generateToken :: Int -> String -> B.ByteString
generateToken id key = let sessionKey = "blabla"
                           timeout = 10
                           ticket = BL.toStrict $ encode (Ticket {userIdS = id, sessionKeyS = sessionKey, timeoutS = timeout})
                           encryptedTicket = ecbEncrypt aesServer (padData ticket)
                           token = BL.toStrict $ encode (Token {ticket = B.unpack $ encryptedTicket, sessionKeyC = sessionKey, timeoutC = timeout})
                           Right userKey = makeKey $ B.pack $ key
                           aesClient = cipherInit userKey :: AES128
                        in ecbEncrypt aesClient (padData token)

server :: Server AccessAPI
server = requestToken
  where requestToken :: Maybe Int -> Handler B.ByteString
        requestToken id = return $ case id of
          Nothing -> B.pack "No user ID given"
          Just id -> let filteredUsers = filter (\u -> userId u == id) users in
                     case (length $ filteredUsers) of
                       0 -> B.pack "No user found"
                       1 -> generateToken id (userKey $ head $ filteredUsers)
                       _ -> B.pack "Error: multiple users for that ID"

users :: [User]
users = [ User 1 "v2VxGDC61jV6E45J"
        , User 2 "E6eAcW89NT13e3xp"
        ]
