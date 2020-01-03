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
