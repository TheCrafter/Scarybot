module Irc
( IrcStateT
, getDefaultState
, connectToServer
, ircConnect
, ircRecv
) where

import Network.Socket
import Control.Monad (forever)
import Data.List (isPrefixOf, takeWhile)
import System.Exit (exitSuccess)
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO)
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

data IrcCon = IrcCon { sock :: Socket }

type IrcStateT a = StateT IrcCon IO a

getDefaultState :: IO IrcCon
getDefaultState = do
  addr <- getAddrInfo'
  newSock <- socket (addrFamily addr) Stream defaultProtocol
  return $ IrcCon newSock

-----------------------------------
-- Irc
-----------------------------------
connectToServer :: IrcStateT ()
connectToServer = do
  state <- get
  let newSock = sock state
  addr <- liftIO $ getAddrInfo'
  liftIO $ connect newSock (addrAddress addr)
  put $ IrcCon newSock

sendToSock :: String -> IrcStateT ()
sendToSock str = do
  state <- get
  let socket = sock state
  let bytesToSend = length str
  
  sended <- liftIO $ send socket str

  -- Check if all data has been sent
  let remainingBytes = bytesToSend - sended
  if remainingBytes > 0
     then sendToSock $ reverse . take remainingBytes . reverse $ str
     else return ()

sendCommand :: String -> String -> IrcStateT ()
sendCommand cmd str = do
  let msgToSend = cmd ++ " " ++ str ++ "\r\n"
  -- Print what we send for debug purposes. TODO: Remove it
  liftIO $ putStr $ ">" ++ msgToSend
  sendToSock msgToSend
  return ()

ircConnect :: IrcStateT ()
ircConnect = do
  mapM (\(s1, s2) -> sendCommand s1 s2 )
    [ ("NICK", nick)
    , ("USER", nick ++ " 0 * :" ++ realname)
    , ("JOIN", chan)
    ]
  return ()

privMsg :: String -> IrcStateT ()
privMsg str = sendCommand "PRIVMSG" $ chan ++ " :" ++ str

ircRecv :: IrcStateT ()
ircRecv = forever $ do
  state <- get
  let socket = sock state
  buf <- liftIO $ recv socket 4096
  let str = init $ init buf
  
  -- Print what we receive for debug purposes. TODO: Remove it
  liftIO $ putStrLn str

  if ping str
     then pong str
     else handleResponse . handleMsg $ str
  where
     ping str = "PING :" `isPrefixOf` str

     pong str = sendCommand "PONG" (':' : drop 6 str)

-----------------------------------
-- Bot
-----------------------------------
handleResponse :: Maybe Bot.Response -> IrcStateT ()
handleResponse Nothing = return ()
handleResponse (Just r) =
  case r of
  Bot.SendMsg m  -> privMsg m
  Bot.LeaveChan  -> sendCommand "PART" chan
  Bot.JoinChan c -> sendCommand "JOIN" c
  Bot.Exit       -> sendCommand "QUIT" ":Scarybot, OUT" >> liftIO exitSuccess

handleMsg :: String -> Maybe Bot.Response
handleMsg s =
  Bot.handleEvent $ Bot.Msg msg sender
  where
    msg = cleanIrcMsg s

    sender = reverse . drop 1 . reverse . takeWhile (/= '~') . drop 1 $ s

-----------------------------------
-- Helper functions
-----------------------------------
-- Get rid of the first part which not containts the actual message
cleanIrcMsg :: String -> String
cleanIrcMsg = drop 1 . dropWhile (/= ':') . drop 1

getAddrInfo' :: IO AddrInfo
getAddrInfo' = do
  addrInfo <- getAddrInfo Nothing (Just server) (Just port)
  return $ head addrInfo
