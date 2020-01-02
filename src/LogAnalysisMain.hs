module LogAnalysisMain
    (
    logAnalysisRun, logAnalysisBuildTree
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
    x <- testParse parse 10 "error.log"
    printLogMessages x

logMessageToValue :: LogMessage -> Int
logMessageToValue (LogMessage _ x _) = x

logAnalysisBuildTree :: IO ()
logAnalysisBuildTree = do
    x <- testParse parse 5423 "error.log"
    print (map logMessageToValue (inOrder (buildLogMessageTree x)))
