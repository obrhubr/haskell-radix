module Main (main) where

import Radix

main :: IO ()
main = do
	let t = Node { wordNode = True, label = "", fullword = "", children = [] }
	
	let t2 = add t Node { wordNode = True, label = "waste", fullword = "waste", children = [] }
	let t3 = add t2 Node { wordNode = True, label = "water", fullword = "water", children = [] }
	let t4 = add t3 Node { wordNode = True, label = "watch", fullword = "watch", children = [] }
	let t5 = add t4 Node { wordNode = True, label = "waterfun", fullword = "waterfun", children = [] }
	let t6 = add t5 Node { wordNode = True, label = "woster", fullword = "woster", children = [] }

	print $ show t4
