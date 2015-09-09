module Bot where

import Data.List
import Data.Char (toLower)

data Event = Msg { msg    :: String
                 , sender :: String }

data Response = SendMsg { msgToSend :: String }
--              | GetChanUsers { chanName :: String}
              | LeaveChan
              | JoinChan { chanName :: String }
              | Exit

handleEvent :: Event -> Maybe Response
handleEvent (Msg m s) = case m of
  "Hello" -> Just $ SendMsg $ "Hello, " ++ s ++ "!"

  "!quit" -> Just $ Exit

  "!part" -> Just $ LeaveChan

  s       -> if isHey s
                then Just $ SendMsg $ "H" ++ (replicate ((countEInHey s) + 1) 'o')
                else Nothing
                

isHey :: String -> Bool
isHey s =
  if s == []
     then False
     else let s' = map toLower s
          in  (head s' == 'h') && ((dropWhile (== 'e') . drop 1 $ s) == "y")

countEInHey :: String -> Int
countEInHey s = (length s) - 2
