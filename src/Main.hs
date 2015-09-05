import Network.Socket
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.List (isPrefixOf)

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

initNetworking :: IO Socket
initNetworking = do
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
     else  handleMsg s (clean str)
  where
     -- Get rid of the first part which not containts the actual message
     clean    = drop 1 . dropWhile (/= ':') . drop 1

     ping str = "PING :" `isPrefixOf` str

     pong str = sendCommand s "PONG" (':' : drop 6 str)


handleMsg :: Socket -> String -> IO ()
handleMsg s str
  | str == "!quit" = sendCommand s "QUIT" ":Scarybot, OUT" >> exitSuccess
  | str == "Hey"   = privMsg s "Hey yourself."
  | otherwise      = return ()


main = do
  sock <- initNetworking
  ircConnect  sock
  ircRecv sock
