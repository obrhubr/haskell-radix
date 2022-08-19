{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main (main) where

import Radix ( addWord, createTree, printTree )

main :: IO ()
main = do
    let t = foldl addWord createTree ["romane", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus" ]

    printTree t