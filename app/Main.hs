module Main where

import System.Environment
import Auth
import Client
import FileServer
import DirService
import BigPixel
import Lock

main :: IO ()
main = do
  args <- getArgs
  case (head $ args) of
    "auth" -> startAuth
    "client" -> startClient
    "fs" -> startFile $ args !! 1
    "ds" -> startDirService
    "bp" -> runBigpixel ["test.bmp"]
    "lock" -> startLock