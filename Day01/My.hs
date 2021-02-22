--
-- EPITECH PROJECT, 2021
-- MyHs
-- File description:
-- MyHs
--

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int
myAbs x = if x >= 0 then x else -x

myMin :: Int -> Int -> Int
myMin x y = case x < y of
        True -> x
        False -> y

myMax :: Int -> Int -> Int
myMax x y = case x > y of
        True -> x
        False -> y

myTuple :: a -> b -> (a , b )
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a , b , c )
myTruple a b c = (a, b, c)

myFst :: (a , b ) -> a
myFst (a, b) = a

mySnd :: (a , b ) -> b
mySnd (a, b) = b

mySwap :: (a , b) -> (b , a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead (a:xs) = a
myHead [] = error "Empty list"

myTail :: [a] -> [a]
myTail (a:xs) = xs
myTail [] = error "Empty list"

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myNth :: [ a ] -> Int -> a
myNth (a:xs) 0 = a
myNth (a:xs) i
    | i < 0 = error "Negative index"
    | i >= myLength (a:xs) = error "Index is too large"
    | otherwise = myNth xs (i - 1)

myTake :: Int ->[a]->[a]
myTake i (a:xs)
    | i < 0 = error "Negative index"
    | i >= myLength (a:xs) = (a:xs)
    | i == 0 = []
    | otherwise = a : myTake (i - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop i (a:xs)
    | i >= myLength (a:xs) = []
    | i == 0 = (a:xs)
    | otherwise = myDrop (i - 1) xs


myAppend :: [a] -> [a] -> [a]
myAppend (a:xs) y = a : myAppend xs y
myAppend [] a = a

myReverse :: [a] -> [a]
myReverse l = _copy l []
    where
        _copy :: [a] -> [a] -> [a]
        _copy [] l = l
        _copy (x:xs) l = _copy xs (x:l)

myInit :: [Int] -> [Int]
myInit [] = error "Empty list"
myInit [x] = []
myInit (x:xs) = x:myInit xs

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myZip :: [a] -> [b] -> [(a,b)]
myZip (a:as) (b:bs) = (a,b) : myZip as bs
myZip _      _      = []

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip [] = ([],[])
myUnzip ((a,b):rest) =
    let (as,bs) = myUnzip rest
    in (a:as, b:bs)