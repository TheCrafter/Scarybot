module Main where

import Control.Monad.Trans.State.Lazy (evalStateT)
import Irc

runIrcBot :: IrcStateT ()
runIrcBot = do
  connectToServer
  ircConnect
  ircRecv

main :: IO ()
main = do
  state <- getDefaultState
  evalStateT runIrcBot state

