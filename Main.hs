import GarbageHat.Analysis
import GarbageHat.Domain
import GarbageHat.Parser

import System.Environment (getArgs)
import Control.Monad (mapM_)
import Data.Decimal (roundTo)
import Data.List (intercalate)
import Data.Set (toList)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usageHelp
    ["-parse", filename] -> report parseOnly filename
    ["-summary", filename] -> report reportSummary filename
    [filename] -> report reportSummary filename
    _ -> usageHelp

usageHelp = do
  putStrLn "garbagehat filename"

report :: ([Event] -> IO ()) -> String -> IO ()
report f filename = do
  input <- readFile filename
  case (parseInput filename input) of
    Left err -> putStrLn $ show err
    Right events -> f events

parseOnly :: [Event] -> IO ()
parseOnly events = mapM_ (putStrLn . show) $ expandStoppedEvents events

reportSummary :: [Event] -> IO ()
reportSummary events = do
  printSummary
  printAnalysis
  where
    statistics = statisticsSummary events
    printSummary = do
      putStrLn "========================================"
      putStrLn "SUMMARY:"
      putStrLn "========================================"
      putStrLn $ "# GC Events: " ++ show (eventCount statistics)
      putStrLn $ "GC Event Types: " ++ (intercalate ", " $ toList $ eventTypes statistics)
      putStrLn $ "Max Heap Space: " ++ show (maxHeapCapacity statistics) ++ "K"
      putStrLn $ "Max Heap Occupancy: " ++ show (maxHeapUsage statistics) ++ "K"
      putStrLn $ "Max Perm Space: " ++ show (maxPermCapacity statistics) ++ "K"
      putStrLn $ "Max Perm Occupancy: " ++ show (maxPermUsage statistics) ++ "K"
      case (throughput statistics) of
        Just t -> putStrLn $ "Throughput: " ++ show (roundTo 2 $ t * 100) ++ "%"
        Nothing -> return ()
      putStrLn $ "Max Pause: " ++ show (getDuration $ maxPause statistics) ++ "ms"
      putStrLn $ "Total Pause: " ++ show (getDuration $ totalPause statistics) ++ "ms"
      case (firstTimestamp statistics, lastTimestamp statistics) of
        (Timestamp (Just ft) _, Timestamp (Just lt) _) -> do
          putStrLn $ "First Timestamp: " ++ show ft ++ "ms"
          putStrLn $ "Last Timestamp: " ++ show lt ++ "ms"
        _ -> putStrLn "No timestamps"
    printAnalysis = do
      putStrLn "========================================"
      putStrLn "ANALYSIS:"
      putStrLn "========================================"

