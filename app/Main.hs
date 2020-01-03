module Main where

import Lib
import DataTypes
import LogAnalysisMain
import Golf

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
--    putStr (histogram [1,4,5,4,6,6,3,4,2,4,9])
