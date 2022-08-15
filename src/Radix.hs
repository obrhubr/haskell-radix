{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Radix where

import Data.List ( minimumBy, delete )

data Node = Node {
    label :: String,
    fullword :: String,
    wordNode :: Bool,
    children :: [Node]
} deriving (Show, Eq)

append :: Node -> Node -> Node
append n an = n { children = an : children n }

type Tree = Node;

getMatch :: Node -> Node -> (String, String, String)
getMatch n rn = helper "" (label n) (label rn)
    where helper s (f1:r1) (f2:r2)
            | null r1 || null r2 = if f1 == f2 then (s ++ [f1], r1, r2) else (s, f1 : r1, f2 : r2)
            | f1 == f2 = helper (s ++ [f1]) r1 r2
            | otherwise = (s, f1 : r1, f2 : r2)

add :: Tree -> Node -> Tree
add t n
    | null $ children t = append t n
    | null matches = append t n
    | null biggestMatch = append t n
    | biggestMatch == label n = t { wordNode = True, fullword = fullword n }
    | restRoot == "" = add bmC (n { label = rest })
    | otherwise = do
        let bmC1 = append bmC (Node { wordNode = True, label = restRoot, fullword = fullword bmC, children = children bmC })
        let bmC2 = bmC1 { children = [last (children bmC1)] }
        let bmC3 = append bmC2 (Node { wordNode = True, label = rest, fullword = fullword n, children = [] })
        let bmC4 = bmC3 { wordNode = False, fullword = fullword t ++ biggestMatch, label = biggestMatch }
        t { children = bmC4 : delete bmC (children t) }

    where   bmC = children t !! biggestMatchIndex
            (biggestMatchIndex, (biggestMatch, rest, restRoot)) = minimumBy sortMatches matches
            sortMatches (_, (m1, _, _)) (_, (m2, _, _))     | m1 > m2 = LT
                                                            | otherwise = GT
            matches = zip [0..(length $ children t)] (map (getMatch n) (children t))