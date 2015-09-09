module Main where

import Irc

main :: IO ()
main = do
  sock <- connectToServer
  ircConnect  sock
  ircRecv sock
