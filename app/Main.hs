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
