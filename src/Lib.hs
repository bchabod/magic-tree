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

-- User ID + 256 bits secret
data User = User
  { userId        :: Int
  , userKey       :: String
  } deriving (Eq, Show)
$(deriveJSON defaultOptions ''User)

type HandshakeAPI = "handshake" :> QueryParam "userId" Int :> Get '[PlainText] String

startAuth :: IO ()
startAuth = run 8080 authenticationServer

authenticationServer :: Application
authenticationServer = serve api server

api :: Proxy HandshakeAPI
api = Proxy

server :: Server HandshakeAPI
server = getKey
  where getKey :: Maybe Int -> Handler String
        getKey id = return $ case id of
          Nothing -> "No user ID given"
          Just id -> let filteredUsers = filter (\u -> userId u == id) users in
                     case (length $ filteredUsers) of
                       0 -> "No user found"
                       1 -> userKey $ head $ filteredUsers
                       _ -> "Error: multiple users for that ID"

users :: [User]
users = [ User 1 "4wQ0cfD45wfr1LZDQzx4dPk119cCyM0G"
        , User 2 "F9s3Y9O8369sN8p88XJzz8SL2siL4b18"
        ]
