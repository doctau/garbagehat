module GarbageHat.Analysis where

import GarbageHat.Domain

import Data.Decimal

import qualified Data.Set as Set
import Data.List (foldl')
import Control.Monad.Reader

expandStoppedEvents :: [Event] -> [Event]
expandStoppedEvents = concatMap expand where
  -- TODO: use the info to adjust timestamps?
  expand (ApplicationStopEvent_ (ApplicationStopEvent _ _ inner)) = inner
  expand e = [e]


data StatisticsSummary = StatisticsSummary {
  eventCount :: Int, eventTypes :: Set.Set String,
  maxHeapCapacity :: Integer, maxHeapUsage :: Integer,
  maxPermCapacity :: Integer, maxPermUsage :: Integer,
  throughput :: Maybe Decimal,
  maxPause :: Duration, totalPause :: Duration,
  firstTimestamp :: Timestamp, lastTimestamp :: Timestamp
  } deriving (Show)

emptyStatisticsSummary = StatisticsSummary {
  eventCount = 0,
  eventTypes = Set.empty,
  maxHeapCapacity = 0,
  maxHeapUsage = 0,
  maxPermCapacity = 0,
  maxPermUsage = 0,
  throughput = Nothing,
  maxPause = 0,
  totalPause = 0,
  firstTimestamp = Timestamp Nothing Nothing,
  lastTimestamp = Timestamp Nothing Nothing}

-- run through the events, calling the right collation functions
-- recurse on ApplicationStopEvents
statisticsSummary :: [Event] -> StatisticsSummary
statisticsSummary events = computeThroughput $ process emptyStatisticsSummary events
process stats events = foldl' mergeStats stats events
mergeStats :: StatisticsSummary -> Event -> StatisticsSummary
mergeStats stats event = case event of
  ParallelScavengeEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration >=> mergeCombinedData $ stats) e
  ParallelSerialOldEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration >=> mergeCombinedData  >=> mergePermData $ stats) e
  ParallelOldCompactingEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration >=> mergeCombinedData  >=> mergePermData $ stats) e
  ParNewEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData $ stats) e
  SerialNewEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData $ stats) e
  SerialOldEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData >=> mergePermData $ stats) e
  SerialSerialEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData $ stats) e
  SerialSerialPermEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData >=> mergePermData $ stats) e
  VerboseGcOldEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData $ stats) e
  VerboseGcYoungEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData $ stats) e
  G1YoungPause_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration $ stats) e
  G1MixedPause_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration $ stats) e
  G1YoungInitialMarkEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration $ stats) e
  G1CleanupEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration $ stats) e
  G1RemarkEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration $ stats) e
  G1FullGCEvent_ e -> runReader (addEventCountType >=> mergeTimestamp >=> mergeDuration  >=> mergeCombinedData $ stats) e
  G1ConcurrentRootRegionScanStart_  e -> runReader (addEventCountType >=> mergeTimestamp $ stats) e
  G1ConcurrentMarkStart_   e -> runReader (addEventCountType >=> mergeTimestamp $ stats) e
  G1ConcurrentCleanupStart_   e -> runReader (addEventCountType >=> mergeTimestamp $ stats) e
  G1ConcurrentRootRegionScanEnd_   e -> runReader (addEventCountType >=> mergeTimestamp $ stats) e
  G1ConcurrentMarkEnd_   e -> runReader (addEventCountType >=> mergeTimestamp $ stats) e
  G1ConcurrentCleanupEnd_   e -> runReader (addEventCountType >=> mergeTimestamp $ stats) e
  ApplicationStopEvent_ (ApplicationStopEvent _ _ inner) -> process stats inner -- add the event types?
  PrintHeapAtGcEvent_ (PrintHeapAtGcEvent inner) -> process stats inner -- add the event types?
  UnparsableLineEvent_ e -> runReader (addEventCountType stats) e

computeThroughput stats = case (firstTimestamp stats, lastTimestamp stats) of
 (Timestamp (Just start) _, Timestamp (Just end) _) -> stats {throughput = Just $ 1.0 - (getDuration (totalPause stats)  / (end - start))}
 _ -> stats {throughput = Nothing}

-- collate the various pieces of data for the summar
addEventCountType :: NamedEvent a => StatisticsSummary -> Reader a StatisticsSummary
addEventCountType stats =  fmap step ask where
  step e = stats {eventCount = eventCount stats + 1,
                  eventTypes = Set.insert (eventName e) $ eventTypes stats}
mergeTimestamp :: TimestampedEvent a => StatisticsSummary -> Reader a StatisticsSummary
mergeTimestamp stats = fmap step ask where
  step e = stats {firstTimestamp = tsop (<) (firstTimestamp stats) (timestamp e),
                  lastTimestamp  = tsop (>) (lastTimestamp stats) (timestamp e)}
  tsop :: (Decimal -> Decimal -> Bool) -> Timestamp -> Timestamp -> Timestamp
  tsop _ (Timestamp Nothing _) y@(Timestamp (Just _) _) = y
  tsop _ x@(Timestamp (Just _) _) (Timestamp Nothing _) = x
  tsop pickFirst x@(Timestamp (Just a) _) y@(Timestamp (Just b) _) = if pickFirst a b then x else y

mergeDuration :: BlockingEvent a => StatisticsSummary -> Reader a StatisticsSummary
mergeDuration stats = fmap step ask where
  step e = stats {totalPause = totalPause stats + duration e, maxPause = max (maxPause stats) (duration e)}

mergeCombinedData :: CombinedData a => StatisticsSummary -> Reader a StatisticsSummary
mergeCombinedData stats = fmap step ask where
  step e = stats {maxHeapCapacity = maxHeapCapacity stats `max` (regionCapacity . combinedData $ e),
                  maxHeapUsage    = maxHeapCapacity stats `max` (regionBefore . combinedData $ e) `max` (regionAfter . combinedData $ e)}

mergePermData :: PermData a => StatisticsSummary -> Reader a StatisticsSummary
mergePermData stats = fmap step ask where
  step e = stats {maxPermCapacity = maxPermCapacity stats `max` (regionCapacity . permData $ e),
                  maxPermUsage    = maxPermUsage stats `max` (regionBefore . permData $ e) `max` (regionAfter . permData $ e)}

