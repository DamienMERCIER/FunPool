--
-- EPITECH PROJECT, 2021
-- 02
-- File description:
-- 02
--

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (a : as) = y == a || myElem y as

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0  = Nothing
safeDiv a b  = Just (a `div` b)


myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

safeNth :: [ a ] -> Int -> Maybe a
safeNth (a:xs) i
    | i < 0 = Nothing
    | i >= myLength (a:xs) = Nothing
    | i == 0 = Just a
    | otherwise = safeNth xs (i - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc (Just x) = Just (x + 1)
safeSucc safeDiv = Nothing

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((a,b):rest) =
    if key == a
       then Just b
       else myLookup key rest

maybeDo :: ( a -> b -> c ) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ _ Nothing = Nothing
maybeDo _ Nothing _ = Nothing
maybeDo a (Just b) (Just c) = Just (a b c)