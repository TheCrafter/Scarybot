module Bot
( Event (..)
, Response (..)
, handleEvent
) where

import Data.List
import Data.List.Split
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

  -- TODO: Redesign commands
  ('!' : 'g' : 's' : ' ' : str) ->
    Just $ SendMsg $ "https://www.google.com/search?q=" ++ formatSearchQuery str

  s       -> if isHey s
                then Just $ SendMsg $ "H" ++ (replicate ((countEInHey s) + 1) 'o')
                else Nothing
                
formatSearchQuery :: String -> String
formatSearchQuery s = reverse . drop 1 . reverse $ formatted
  where unwrapped = splitOn " " s
        addPlus   = map (\str -> [str ++ "+"]) unwrapped
        formatted = concat . concat $ addPlus


isHey :: String -> Bool
isHey s =
  if s == []
     then False
     else let s' = map toLower s
          in  (head s' == 'h') && ((dropWhile (== 'e') . drop 1 $ s) == "y")

countEInHey :: String -> Int
countEInHey s = (length s) - 2
