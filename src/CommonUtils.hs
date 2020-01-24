module CommonUtils where

import qualified Data.Set as Set

pair :: (a -> b, a -> c) -> a -> (b, c)
pair (f, g) x = (f x, g x)

cross :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
cross f g = pair (f . fst, g . snd)

addQuotes :: String -> String
addQuotes = ("'" ++) . (++ "'")

addParens :: String -> String
addParens = ("("++) . (++ ")")

addAngles :: String -> String
addAngles = ("<"++) . (++ ">")

addBrackets :: String -> String
addBrackets = ("["++) . (++ "]")

notUnique :: Ord a => [a] -> Bool
notUnique = uncurry (/=) . pair (length,  Set.size . Set.fromList)