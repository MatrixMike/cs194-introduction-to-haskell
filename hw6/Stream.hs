-- Exercise 3

module Stream where

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons el s) = el : streamToList s

streamRepeat :: a -> Stream a
streamRepeat el = Cons el (streamRepeat el)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons el s) = Cons (f el) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f el = Cons el (streamFromSeed f (f el))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

zeros = streamFromSeed id 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) s = Cons a (interleaveStreams s as)

-- I copied this. Pretty much no way I would have come up with it
-- it folds a list of infinite streams by interleaving them together
ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])