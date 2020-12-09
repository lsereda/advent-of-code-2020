module Queue where

type Queue a = ([a], [a])

empty :: Queue a
empty = ([], [])

isEmpty :: Queue a -> Bool
isEmpty ([], []) = True
isEmpty _        = False

push :: a -> Queue a -> Queue a
push x (xs, ys) = (x:xs, ys)

pop :: Queue a -> (a, Queue a)
pop ([], [])   = error "Empty queue"
pop (xs, y:ys) = (y, (xs, ys))
pop (xs, [])   = pop ([], reverse xs)

fromList :: [a] -> Queue a
fromList xs = ([], reverse xs)

toList :: Queue a -> [a]
toList (xs, ys) = xs ++ reverse ys