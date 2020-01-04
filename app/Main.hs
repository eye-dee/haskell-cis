module Main where

import Lib
import DataTypes
import LogAnalysisMain
import Golf
import Hw4

main :: IO ()
main = do
    print (skips "1234")
    print (skips "ABCD")
    print (skips "hello!")
    print (skips [1])
    print (skips [True, False])
    print (localMaxima [2,9,5,6,1])
    print (localMaxima [2,3,4,1,5])
    print (localMaxima [1,2,3,4,5])
    putStr (histogram [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,5,5,5,5,5,5,5,5,5,5])
    putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
    print (fun1 [3,4,5,6])
    print (fun11 [3,4,5,6])
    print (fun2 2)
    print (fun22 2)
    print (fun2 3)
    print (fun22 3)
    print (fun2 4)
    print (fun22 4)
    print (fun2 5)
    print (fun22 5)
    print (fun2 6)
    print (fun22 6)
    print (fun2 7)
    print (fun22 7)
    print (fun2 8)
    print (fun22 8)
    putStr (prettyTree (foldTree [1,2,3,4,5,6,7,8]))
    print (foldTree [1,2,3])
    print $ fibonacci 1
    print $ fibonacci 2
    print $ fibonacci 3
    print $ fibonacci 4
    print $ fibonacci 0
    print $ fibonacci 30
    print $ fibonacci (-1)
    print $ fibonacci (-2)
    print $ fibonacci (-3)
    print $ fibonacci (-4)
    print $ fibonacci (-10)
    print $ seqA 301
    print $ sum'n'count 301111
    print $ sum'n'count (-301111)
    print $ sum'n'count 0
    print $ integration sin pi 0
    print $ integration (\x -> 1) 0 10
