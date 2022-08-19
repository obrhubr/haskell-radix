{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main (main) where

import Radix ( addWord, deleteWord, searchWord, createTree, printTree )

benchmark = "not implemented"

main :: IO ()
main = do
    let t = foldl addWord createTree ["romane", "roman", "romanus", "romulus", "rubens", "ruber", "rubicon", "rubicundus" ]

    let deletedT = foldl deleteWord t ["rubicon"]

    let searchResult = searchWord deletedT "rubicon"
    print searchResult

    printTree deletedT