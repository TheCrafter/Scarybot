module Irc.Core
( IrcStateT
, getDefaultState
, connectToServer
, ircConnect
, ircRecv
) where

import Network.Socket (Socket, recv)
import Control.Monad (forever)
import Data.List (isPrefixOf, takeWhile)
import System.Exit (exitSuccess)
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class (liftIO)
import qualified Bot
import qualified Irc.Network as IrcNet

data IrcCon = IrcCon { sock :: Socket }

type IrcStateT a = StateT IrcCon IO a

-----------------------------------------------
-- Config --
-----------------------------------------------
chan   = "#scaryboxstudios"
nick   = "Scarybot"
realname = "ScaryBox Studios IRC Bot"
-----------------------------------------------

getDefaultState :: IO IrcCon
getDefaultState = do
  newSock <- liftIO $ IrcNet.getDefaultSocket
  return $ IrcCon newSock

-----------------------------------
-- Irc
-----------------------------------
connectToServer :: IrcStateT ()
connectToServer = do
  state <- get
  let newSock = sock state
  liftIO $ IrcNet.connectToServer newSock
  put $ IrcCon newSock

sendCommand :: String -> String -> IrcStateT ()
sendCommand cmd str = do
  state <- get
  let socket = sock state
  let msgToSend = cmd ++ " " ++ str ++ "\r\n"
  -- Print what we send for debug purposes. TODO: Remove it
  liftIO $ putStr $ ">" ++ msgToSend
  liftIO $ IrcNet.sendToSock socket msgToSend
  return ()

ircConnect :: IrcStateT ()
ircConnect = do
  mapM (\(s1, s2) -> sendCommand s1 s2)
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
