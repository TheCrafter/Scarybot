module Irc.Network
( getDefaultSocket
, connectToServer
, sendToSock
) where

import Network.Socket

-----------------------------------------------
-- Config --
-----------------------------------------------
server :: HostName
server = "irc.freenode.net"

port   = "6667"
-----------------------------------------------

getDefaultSocket :: IO Socket
getDefaultSocket = do
  addr <- getAddrInfo'
  socket (addrFamily addr) Stream defaultProtocol

connectToServer :: Socket -> IO ()
connectToServer s = do
  addr <-  getAddrInfo'
  connect s (addrAddress addr)

sendToSock :: Socket -> String -> IO ()
sendToSock s str = do
  let bytesToSend = length str
  sended <- send s str

  -- Chcek if all data has been sent
  let remainingBytes = bytesToSend - sended
  if remainingBytes > 0
     then sendToSock s $ reverse . take remainingBytes . reverse $ str
     else return ()

-----------------------------------
-- Helper functions
-----------------------------------
getAddrInfo' :: IO AddrInfo
getAddrInfo' = do
  addrInfo <- getAddrInfo Nothing (Just server) (Just port)
  return $ head addrInfo
