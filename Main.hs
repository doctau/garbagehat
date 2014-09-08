import GarbageHat.Domain

import System.Environment (getArgs)
import Control.Monad (mapM_)
import Control.Applicative ((<*), (*>), liftA3, Applicative)
import Data.Decimal

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (fractional, nat)

import Debug.Trace (trace, traceShow)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usageHelp
    ["-parse", filename] -> parseOnly filename
    [filename] -> parseOnly filename
    _ -> usageHelp

usageHelp = do
  putStrLn "garbagehat filename"

parseOnly :: String -> IO ()
parseOnly filename = do
  input <- readFile filename
  case (parseInput filename input) of
    Left err -> putStrLn $ show err
    Right events -> mapM_ (putStrLn . show) events

parseInput :: String -> String -> Either ParseError [Event]
parseInput name input = parse parseLines name input

parseLines :: GenParser Char st [Event]
parseLines = endBy parseEvent eol

eol :: GenParser Char st ()
eol = do
  char '\n' >> return ()
  <|> eof

parseEvent :: GenParser Char st Event
parseEvent = do
  try (parseTimestamp >>= timestampedEvent)
  <|> try parseApplicationStopTime
  -- <|> unhandledEvent
  where timestampedEvent t = do
          try (parseParallelScavenge t)
          <|> try (parseParallelSerialOld t)
          <|> try (parseParallelOldCompacting t)
        parseParallelScavenge t = do
          string "[GC"
          optionMaybe $ string "--" -- happens when the JVM is stressed
          young <- surroundedBy (string " [PSYoungGen: ") parseRegion (string "]")
          combined <- surroundedBy (char ' ') parseRegion (string ", ")
          dur <- parseDuration
          string "]"
          timing <- optionMaybe (char ' ' >> parseTimesBlock)
          many (char ' ')
          return $ mkParallelScavengeEvent t dur timing young combined
        parseParallelSerialOld t = do
          string "[Full GC"
          optionMaybe $ string " (System)"
          young <- surroundedBy (string " [PSYoungGen: ") parseRegion (string "]")
          old <- surroundedBy (string " [PSOldGen: ") parseRegion (string "]")
          combined <- surroundedBy (char ' ') parseRegion (string " ")
          perm <- surroundedBy (string "[PSPermGen: ") parseRegion (string "], ")
          dur <- parseDuration
          string "]"
          timing <- optionMaybe (char ' ' >> parseTimesBlock)
          many (char ' ')
          return $ mkParallelSerialOldEvent t dur timing young old combined perm
        parseParallelOldCompacting t = do
          string "[Full GC"
          optionMaybe $ string " (System)"
          young <- surroundedBy (string " [PSYoungGen: ") parseRegion (string "]")
          old <- surroundedBy (string " [ParOldGen: ") parseRegion (string "]")
          combined <- surroundedBy (char ' ') parseRegion (string " ")
          perm <- surroundedBy (string "[PSPermGen: ") parseRegion (string "], ")
          dur <- parseDuration
          string "]"
          timing <- optionMaybe (char ' ' >> parseTimesBlock)
          many (char ' ')
          return $ mkParallelSerialOldEvent t dur timing young old combined perm

parseRegion :: GenParser Char st RegionUsage
parseRegion = do
  before <- parseSize
  string "->"
  after <- parseSize
  capacity <- surroundedBy (string "(") parseSize (string ")")
  return $ RegionUsage before after capacity

parseDuration :: GenParser Char st Duration
parseDuration = fmap Duration $ fractional <* string " secs"

parseSize :: GenParser Char st Integer
parseSize = do
  s <- nat
  optionMaybe $ char ' '
  char 'K'
  return s

parseTimesBlock :: GenParser Char st TimingInfo
parseTimesBlock = do
  user <- string "[Times: user=" *> fractional
  sys <- string " sys=" *> fractional
  real <- string ", real=" *> fractional
  string " secs]"
  return $ TimingInfo user sys real


parseTimestamp :: GenParser Char st Timestamp
parseTimestamp = do
  try parseTime
  <|> parseDateAndTime
  <|> parseDate -- is this needed?
  where parseTime = do
          time <- fractional <* string ": "
          return $ Timestamp time Nothing
        parseDateAndTime = do
          fail "Not yet implemented"
        parseDate = do
          fail "Not yet implemented"
  
parseApplicationStopTime :: GenParser Char st Event
parseApplicationStopTime = do
  ran <- fmap Duration $ surroundedBy (string "Application time: ") fractional (string " seconds\n")
  inner <- endBy parseEvent eol -- there may be GC events between the pair
  stopped <- optionMaybe $ fmap Duration $ surroundedBy (string "Total time for which application threads were stopped: ") fractional (string " seconds")
  return $ mkApplicationStopEvent ran stopped inner


unhandledEvent :: GenParser Char st Event
unhandledEvent = do
  s <- many1 (noneOf "\n")
  return $ mkUnparsableLineEvent s

surroundedBy :: Applicative m => m a -> m b -> m c -> m b
surroundedBy =  liftA3 (\_ a _ -> a)

