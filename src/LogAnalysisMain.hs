module LogAnalysisMain
    ( logAnalysisRun
    ) where

import LogAnalysis
import Log

printLogMessages :: [LogMessage] -> IO ()
printLogMessages [] = do putStr ""
printLogMessages (x:xs) = do
    print x
    printLogMessages xs

logAnalysisRun :: IO ()
logAnalysisRun = do
    x <- testParse parse 5524 "error.log"
    printLogMessages x
