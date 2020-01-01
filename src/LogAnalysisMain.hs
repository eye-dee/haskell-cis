module LogAnalysisMain
    ( logAnalysisRun
    ) where

import LogAnalysis
import Log

logAnalysisRun :: IO ()
logAnalysisRun = do
    print (testParse parse 10 "error.log")
