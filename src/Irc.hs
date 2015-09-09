module Irc
( connectToServer
, ircConnect
, ircRecv
) where

import Network.Socket
import Control.Monad (forever)
import Data.List (isPrefixOf, takeWhile)
import System.Exit (exitSuccess)
import qualified Bot

-----------------------------------------------
-- Config --
-----------------------------------------------
server :: HostName
server = "irc.freenode.net"

port   = "6667"
chan   = "#scaryboxstudios"
nick   = "Scarybot"
realname = "ScaryBox Studios IRC Bot"
-----------------------------------------------

connectToServer :: IO Socket
connectToServer = do
  addrinfo <- getAddrInfo Nothing (Just server) (Just port)
  let addr = head addrinfo
  sock <- socket (addrFamily addr) Stream defaultProtocol
  connect sock (addrAddress addr)
  return sock

ircConnect :: Socket -> IO ()
ircConnect sock = do
  mapM (\(s1, s2) -> sendCommand  sock s1 s2 )
    [ ("NICK", nick)
    , ("USER", nick ++ " 0 * :" ++ realname)
    , ("JOIN", chan)
    ]
  return ()

sendCommand :: Socket -> String -> String -> IO ()
sendCommand sock cmd str = do
  let msgToSend = cmd ++ " " ++ str ++ "\r\n"
  -- Print what we send for debug purposes. TODO: Remove it
  putStr $ ">" ++ msgToSend
  send sock msgToSend
  return ()

privMsg :: Socket -> String -> IO ()
privMsg sock str = do
  sendCommand sock "PRIVMSG" $ chan ++ " :" ++ str

ircRecv :: Socket -> IO ()
ircRecv s = forever $ do
  buf <- recv s 4096
  let str = init $ init buf
  -- Print what we receive for debug purposes. TODO: Remove it
  putStrLn str
  if ping str
     then pong str
     else handleResponse s . handleMsg $ str
  where
     ping str = "PING :" `isPrefixOf` str

     pong str = sendCommand s "PONG" (':' : drop 6 str)

-- Get rid of the first part which not containts the actual message
cleanIrcMsg :: String -> String
cleanIrcMsg = drop 1 . dropWhile (/= ':') . drop 1

handleMsg :: String -> Maybe Bot.Response
handleMsg s =
  Bot.handleEvent $ Bot.Msg msg sender
  where
    msg = cleanIrcMsg s

    sender = reverse . drop 1 . reverse . takeWhile (/= '~') . drop 1 $ s

handleResponse :: Socket -> Maybe Bot.Response -> IO ()
handleResponse _ Nothing = return ()
handleResponse s (Just r) =
  case r of
  Bot.SendMsg m  -> privMsg s m
  Bot.LeaveChan  -> sendCommand s "PART" chan
  Bot.JoinChan c -> sendCommand s "JOIN" c
  Bot.Exit       -> sendCommand s "QUIT" ":Scarybot, OUT" >> exitSuccess

