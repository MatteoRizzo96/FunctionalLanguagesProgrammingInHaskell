module WhatWentWrong where
import Log

-- Exercise 2

{-
    Inserts a new LogMessage into an existing MessageTree, producing a new MessageTree.
    The function may assume that it is given a sorted MessageTree, and must produce a new
    sorted MessageTree containing the new LogMessage in addition to the contents of the
    original MessageTree. If insert is given a LogMessage which is Unknown,
    it should return the MessageTree unchanged.
-}

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t            = t
insert _ (Node _ (Unknown _) _) = error "Unknown messages are not allowed in MessageTree"
insert msg Leaf                 = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node l x@(LogMessage _ tsx _) r) = if ts < tsx then Node (insert msg l) x r else Node l x (insert msg r)

-- Exercise 3

{-
    Builds up a MessageTree containing the messages in the list, by successively
    inserting the messages into a MessageTree (beginning with a Leaf)
-}

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

{-
    Takes a sorted MessageTree and produces a list of all the LogMessage s
    it contains, sorted by timestamp from smallest to biggest
-}

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l root r) = inOrder l ++ [root] ++ inOrder r

-- Exercise 5

{-
    Takes an unsorted list of LogMessage s, and returns a list of the messages
    corresponding to any errors with a severity of 50 or greater, sorted by timestamp
-}


byThreshold :: LogMessage -> Bool
byThreshold (LogMessage (Error severity) _ _) = severity >= 50
byThreshold _                                 = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg (Unknown msg)        = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map getMsg (filter byThreshold ((inOrder . build) xs))