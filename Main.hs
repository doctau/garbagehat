import GarbageHat.Domain

import System.Environment (getArgs)
import Control.Monad (mapM_, liftM2)
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
parseLines = do -- use endBy eol line
    result <- many line
    eof
    return result

line :: GenParser Char st Event
line = do
  result <- parseEvent
  eol
  return result

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
        parseParallelScavenge t = do
          string "[GC"
          optionMaybe $ string "--" -- happens when the JVM is stressed
          -- young gen
          string " [PSYoungGen: "
          young <- parseRegion
          string "]"
          -- combined heap usage
          char ' '
          combined <- parseRegion
          -- timing info
          string ", "
          dur <- parseDuration
          string "]"
          timing <- optionMaybe (char ' ' >> parseTimesBlock)
          many (char ' ')
          return $ mkParallelScavengeEvent t dur timing young combined
        parseParallelSerialOld t = do
          string "[Full GC"
          optionMaybe $ string " (System)" -- happens when the JVM is stressed
          -- young gen
          string " [PSYoungGen: "
          young <- parseRegion
          string "]"
          -- old gen
          try (string " [PSOldGen: ") <|> try (string " [ParOldGen: ")
          old <- parseRegion
          string "]"
          -- combined heap usage
          char ' '
          combined <- parseRegion
          -- perm gen
          string " [PSPermGen: "
          perm <- parseRegion
          string "]"
          -- timing info
          string ", "
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
  string "("
  capacity <- parseSize
  string ")"
  return $ RegionUsage before after capacity

parseDuration :: GenParser Char st Duration
parseDuration = do
  d <- fractional
  string " secs"
  return $ Duration d

parseSize :: GenParser Char st Integer
parseSize = do
  s <- nat
  optionMaybe $ char ' '
  char 'K'
  return s

parseTimesBlock :: GenParser Char st TimingInfo
parseTimesBlock = do
  string "[Times: user="
  user <- fractional
  string " sys="
  sys <- fractional
  string ", real="
  real <- fractional
  string " secs]"
  return $ TimingInfo user sys real


parseTimestamp :: GenParser Char st Timestamp
parseTimestamp = do
  try parseTime
  <|> parseDateAndTime
  <|> parseDate -- is this needed?
  where parseTime = do
          time <- fractional
          string ": "
          return $ Timestamp time Nothing
        parseDateAndTime = do
          fail "Not yet implemented"
        parseDate = do
          fail "Not yet implemented"
  
parseApplicationStopTime :: GenParser Char st Event
parseApplicationStopTime = do
  string "Application time: "
  ran <- fmap Duration fractional
  string " seconds"
  eol
  inner <- many $ do
    e <- parseEvent
    eol
    return e
  stopped <- optionMaybe $ do
    string "Total time for which application threads were stopped: "
    stopped <- fmap Duration fractional
    string " seconds"
    return stopped
  return $ mkApplicationStopEvent ran stopped inner


unhandledEvent :: GenParser Char st Event
unhandledEvent = do
  s <- many1 (noneOf "\n")
  return $ mkUnparsableLineEvent s


--    private static final String REGEX = "^" + JdkRegEx.TIMESTAMP + ": \\[GC(--)? \\[PSYoungGen: " + JdkRegEx.SIZE
--            + "->" + JdkRegEx.SIZE + "\\(" + JdkRegEx.SIZE + "\\)\\] " + JdkRegEx.SIZE + "->" + JdkRegEx.SIZE + "\\("
--            + JdkRegEx.SIZE + "\\), " + JdkRegEx.DURATION + "\\]" + JdkRegEx.TIMES_BLOCK + "?[ ]*$";
--    private static final Pattern PATTERN = Pattern.compile(ParallelScavengeEvent.REGEX);
--
--        this.logEntry = logEntry;
--        Matcher matcher = PATTERN.matcher(logEntry);
--        if (matcher.find()) {
--           timestamp = JdkMath.convertSecsToMillis(matcher.group(1)).longValue();
--            young = Integer.parseInt(matcher.group(3));
--            youngEnd = Integer.parseInt(matcher.group(4));
--            youngAvailable = Integer.parseInt(matcher.group(5));
--            int totalBegin = Integer.parseInt(matcher.group(6));
--            old = totalBegin - young;
--            int totalEnd = Integer.parseInt(matcher.group(7));
--            oldEnd = totalEnd - youngEnd;
--            int totalAllocation = Integer.parseInt(matcher.group(8));
--            oldAllocation = totalAllocation - youngAvailable;
--            duration = JdkMath.convertSecsToMillis(matcher.group(9)).intValue();
--        } else {
--            throw new IllegalArgumentException("log entry did not match " + REGEX);
--        }
