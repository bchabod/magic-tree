module Main where

import System.Environment
import Auth
import Client
import FileServer

main :: IO ()
main = do
  args <- getArgs
  case (head $ args) of
    "auth" -> startAuth
    "client" -> startClient
    "fs" -> startFile