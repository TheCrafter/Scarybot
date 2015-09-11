module Bot
( Event (..)
, Response (..)
, handleEvent
) where

import Data.List
import Data.List.Split
import Data.Char (toLower)
import Network.HTTP.Base (urlEncode)

data Event = Msg { msg    :: String
                 , sender :: String }

data Response = SendMsg { msgToSend :: String }
--              | GetChanUsers { chanName :: String}
              | LeaveChan
              | JoinChan { chanName :: String }
              | Exit

handleEvent :: Event -> Maybe Response
handleEvent (Msg m s) = case m of
  -- Commands start with '!'
  ('!' : command) -> handleCommand . buildCommand $ command
  
  "Hello" -> Just $ SendMsg $ "Hello, " ++ s ++ "!"

  s       -> if isHey s
                then Just $ SendMsg $ "H" ++ (replicate ((countEInHey s) + 1) 'o')
                else Nothing

-----------------------------------
-- Commands
-----------------------------------
data Command = Command { cmd :: String
                       , arg :: Maybe String }

buildCommand :: String -> Command
buildCommand s =
  Command makeCommand makeArg
  where
    makeCommand = takeWhile ((/=) ' ') s

    makeArg     = let argStr = drop 1 . dropWhile ((/=) ' ') $ s
                  in  if argStr == [] then Nothing else Just argStr

handleCommand :: Command -> Maybe Response
handleCommand (Command command Nothing) =
  case command of
  "quit" -> Just $ Exit
  "part" -> Just $ LeaveChan

handleCommand (Command command (Just arg)) =
  case command of
  "gs" -> Just $ SendMsg $ getSearchUrl "https://www.google.com/search?q=" arg

  "hs" -> Just $ SendMsg $ getSearchUrl "https://www.haskell.org/hoogle/?hoogle=" arg

-----------------------------------
-- Used for Heeey-Hooo
-----------------------------------
isHey :: String -> Bool
isHey s =
  if s == []
     then False
     else let s' = map toLower s
          in  (head s' == 'h') && ((dropWhile (== 'e') . drop 1 $ s) == "y")

countEInHey :: String -> Int
countEInHey s = (length s) - 2

-----------------------------------
-- Helper functions
-----------------------------------
getSearchUrl :: String -> String -> String
getSearchUrl basicUrl str = basicUrl ++ urlEncode str

