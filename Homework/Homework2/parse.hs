module Parse where
import Log

-- Exercise 1

{-
    Parses an individual line from the log file. 
    
    Examples:
    * parseMessage "E 2 562 help help"               -> LogMessage (Error 2) 562 "help help"
    * parseMessage "I 29 la la la"                   -> LogMessage Info 29 "la la la"
    * parseMessage "This is not in the right format" -> Unknown "This is not in the right format"
-}

parseMessage :: String -> LogMessage
parseMessage s = case words s of
    ("E":p:ts:msg)  -> LogMessage (Error (read p)) (read ts) (unwords msg)
    ("W":ts:msg)    -> LogMessage Warning (read ts) (unwords msg)
    ("I":ts:msg)    -> LogMessage Info (read ts) (unwords msg)
    msg             -> Unknown (unwords msg)

-- Parses an entire log file at once and returns its content as a list of LogMessage s
parse :: String -> [LogMessage]
parse file = [parseMessage l | l <- lines file] 