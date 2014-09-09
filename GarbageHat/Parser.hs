module GarbageHat.Parser where

import GarbageHat.Domain
import Control.Applicative ((<*), (*>), liftA3, Applicative)
import Data.Decimal

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (fractional, int, nat)

import Debug.Trace (trace, traceShow)


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
  <|> try parsePrintHeapAtGc
  -- <|> unhandledEvent
  where timestampedEvent t = do
          try (parseParallelScavenge t)
          <|> try (parseParNew t)
          <|> try (parseSerialNew t)
          <|> try (parseSerialOld t)
          <|> try (parseSerialSerial t)
          <|> try (parseSerialSerialPerm t)
          <|> try (parseParallelSerialOld t)
          <|> try (parseParallelOldCompacting t)
          <|> try (parseVerboseGcYoungEvent t)
          <|> try (parseVerboseGcOldEvent t)

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

parseParNew t = do
  string "[GC"
  optionMaybe $ string "--" -- happens when the JVM is stressed
  young <- surroundedBy (string " [ParNew: ") parseRegion (string "]")
  combined <- surroundedBy (char ' ') parseRegion (string ", ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkParNewEvent t dur timing young combined

parseSerialNew t = do
  char '['
  optionMaybe $ string "Full" -- old JVM emit this incorrectly
  string "GC"
  young <- parseDefNew
  combined <- surroundedBy (char ' ') parseRegion (string ", ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkSerialNewEvent t dur timing young combined

parseSerialOld t = do
  string "[Full GC"
  optionMaybe $ string " (System)"
  old <- parseTenured
  combined <- surroundedBy (char ' ') parseRegion (string " ")
  perm <- surroundedBy (string "[Perm: ") parseRegion (string "], ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkSerialOldEvent t dur timing old combined perm

parseSerialSerial t = do
  string "[GC"
  optionMaybe $ string " (System)"
  young <- parseDefNew
  old <- parseTenured
  combined <- surroundedBy (char ' ') parseRegion (string ", ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkSerialSerialEvent t dur timing young old combined

parseSerialSerialPerm t = do
  string "[GC"
  optionMaybe $ string " (System)"
  young <- parseDefNew
  old <- parseTenured
  combined <- surroundedBy (char ' ') parseRegion (string " ")
  perm <- surroundedBy (string "[Perm: ") parseRegion (string "], ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkSerialSerialPermEvent t dur timing young old combined perm

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
  return $ mkParallelOldCompactingEvent t dur timing young old combined perm

parseVerboseGcYoungEvent t = do
  combined <- surroundedBy (string "[GC ") parseRegion (string ", ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkVerboseGcYoungEvent t dur timing combined

parseVerboseGcOldEvent t = do
  combined <- surroundedBy (string "[Full GC ") parseRegion (string ", ")
  dur <- parseDuration
  string "]"
  timing <- optionMaybe (char ' ' >> parseTimesBlock)
  many (char ' ')
  return $ mkVerboseGcYoungEvent t dur timing combined

-- helpers
parseDefNew = do
  parseTimestamp >> string ": [DefNew"
  young <- parseRegion
  string ", " >> parseDuration >> string "]"
  return young

parseTenured = do
  parseTimestamp >> string ": [Tenured"
  old <- parseRegion
  string ", " >> parseDuration >> string "]"
  return old

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
  <|> parseDate
  where parseTime = do
          time <- fractional <* string ": "
          return $ Timestamp (Just time) Nothing
        parseDate = do
          year <- nat <* char '-'
          month <- nat <* char '-'
          day <- nat <* char 'T'
          hour <- nat <* char ':'
          min <- nat <* char ':'
          sec <- fractional
          oneOf "-+" >> nat >> string ": " -- GMT offset
          return $ Timestamp Nothing (Just $ show (year,month,day,hour,min,sec))
        parseDateAndTime = do
          Timestamp _ d <- parseDate
          Timestamp t _ <- parseTime
          return $ Timestamp t d
  
parseApplicationStopTime :: GenParser Char st Event
parseApplicationStopTime = do
  ran <- fmap Duration $ surroundedBy (string "Application time: ") fractional (string " seconds\n")
  inner <- endBy parseEvent eol -- there may be GC events between the pair
  stopped <- optionMaybe $ fmap Duration $ surroundedBy (string "Total time for which application threads were stopped: ") fractional (string " seconds")
  return $ mkApplicationStopEvent ran stopped inner

parsePrintHeapAtGc :: GenParser Char st Event
parsePrintHeapAtGc = do
  string "{Heap before GC invocations" >> many (noneOf "\n") >> eol
  many (char ' '  >> many (noneOf "\n") >> eol)
  inner <- endBy parseEvent eol -- there may be GC events between the pair
  string "Heap after GC invocations" >> many (noneOf "\n") >> eol
  many (char ' '  >> many (noneOf "\n") >> eol)
  char '}'
  return $ mkPrintHeapAtGcEvent inner



unhandledEvent :: GenParser Char st Event
unhandledEvent = do
  s <- many1 (noneOf "\n")
  return $ mkUnparsableLineEvent s

surroundedBy :: Applicative m => m a -> m b -> m c -> m b
surroundedBy =  liftA3 (\_ a _ -> a)

