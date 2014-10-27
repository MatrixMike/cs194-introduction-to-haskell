module LogAnalysis where
import Log

-- parseMessage "This is not in the right format"
-- parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parseWords :: [String] -> LogMessage
parseWords ("I":t:ws) = LogMessage Info (read t) (unwords ws)
parseWords ("W":t:ws) = LogMessage Warning (read t) (unwords ws)
parseWords ("E":c:t:ws) = LogMessage (Error (read c)) (read t) (unwords ws)
parseWords ws = Unknown (unwords ws)

parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm (Node left current right)
  | valueTime < nodeTime = Node (insert lm left) current right
  | valueTime >= nodeTime = Node left current (insert lm right)
  where (LogMessage _ valueTime _ ) = lm
        (LogMessage _ nodeTime _ ) = current


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = map message $ inOrder $ build $ filter (isSevereError 50) ls
  where message (LogMessage _ _ s) = s

isSevereError :: Int -> LogMessage -> Bool
isSevereError severity (LogMessage (Error n) _ _) = n >= severity
isSevereError _ _ = False



