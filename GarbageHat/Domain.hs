module GarbageHat.Domain where

import Data.Decimal

-- seconds since start and datestamp
data Timestamp = Timestamp Decimal (Maybe String) deriving (Eq, Show)
data TimingInfo = TimingInfo Decimal Decimal Decimal deriving (Eq, Show) -- user sys real
data RegionUsage = RegionUsage {regionbefore :: Integer, regionAfter :: Integer, regionCapacity :: Integer} deriving (Eq, Show)
newtype Duration = Duration Decimal deriving (Eq, Show)

class LogEvent a where
  logText :: a -> String

class TimestampedEvent a where
  timestamp :: a -> Timestamp

class BlockingEvent a where
  duration :: a -> Duration

class TimedEvent a where
  timingInfo :: a -> Maybe TimingInfo

class YoungData a where
  youngData :: a -> RegionUsage

class OldData a where
  oldData :: a -> RegionUsage

class CombinedData a where
  combinedData :: a -> RegionUsage

class PermData a where
  permData :: a -> RegionUsage


data UnparsableLineEvent = UnparsableLineEvent String deriving (Eq, Show)
instance LogEvent UnparsableLineEvent where
  logText (UnparsableLineEvent s) = s

-- Young generation collector used when <code>-XX:+UseParallelGC</code> or <code>-XX:+UseParallelOldGC</code> JVM options specified.
-- Standard format: 19810.091: [GC [PSYoungGen: 27808K->632K(28032K)] 160183K->133159K(585088K), 0.0225213 secs]
-- With 2 dashes after the GC. This seems to happen when the JVM is stressed out doing continuous full GCs:
--   14112.691: [GC-- [PSYoungGen: 313864K->313864K(326656K)] 879670K->1012935K(1025728K), 0.9561947 secs]
-- full entry, timestamp, duration, young, combined
data ParallelScavengeEvent = ParallelScavengeEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent ParallelScavengeEvent where
  timestamp (ParallelScavengeEvent t _ _ _ _) = t
instance BlockingEvent ParallelScavengeEvent where
  duration (ParallelScavengeEvent _ d _ _ _) = d
instance TimedEvent ParallelScavengeEvent where
  timingInfo (ParallelScavengeEvent _ _ t _ _) = t
instance YoungData ParallelScavengeEvent where
  youngData (ParallelScavengeEvent _ _ _ y _) = y
instance CombinedData ParallelScavengeEvent where
  combinedData (ParallelScavengeEvent _ _ _ _ c) = c


-- serial old event (parallel young).
-- same as SerialOldEvent but with different logging
-- Standard: 3.600: [Full GC [PSYoungGen: 5424K->0K(38208K)] [PSOldGen: 488K->5786K(87424K)] 5912K->5786K(125632K) [PSPermGen: 13092K->13094K(131072K)], 0.0699360 secs]
-- Alternate: 4.165: [Full GC (System) [PSYoungGen: 1784K->0K(12736K)] [PSOldGen: 1081K->2855K(116544K)] 2865K->2855K(129280K) [PSPermGen: 8600K->8600K(131072K)], 0.0427680 secs]
data ParallelSerialOldEvent = ParallelSerialOldEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent ParallelSerialOldEvent where
  timestamp (ParallelSerialOldEvent t _ _ _ _ _ _) = t
instance BlockingEvent ParallelSerialOldEvent where
  duration (ParallelSerialOldEvent _ d _ _ _ _ _) = d
instance TimedEvent ParallelSerialOldEvent where
  timingInfo (ParallelSerialOldEvent _ _ t _ _ _ _) = t
instance YoungData ParallelSerialOldEvent where
  youngData (ParallelSerialOldEvent _ _ _ y _ _ _) = y
instance OldData ParallelSerialOldEvent where
  oldData (ParallelSerialOldEvent _ _ _ _ o _ _) = o
instance CombinedData ParallelSerialOldEvent where
  combinedData (ParallelSerialOldEvent _ _ _ _ _ c _) = c
instance PermData ParallelSerialOldEvent where
  permData (ParallelSerialOldEvent _ _ _ _ _ _ p) = p


-- parallel old compacting event
-- New throughput collector introduced in JDK 5 update 6 and significantly enhanced in JDK 6. Enabled with the -XX:+UseParallelOldGC JVM option
-- Standard: 2182.541: [Full GC [PSYoungGen: 1940K->0K(98560K)] [ParOldGen: 813929K->422305K(815616K)] 815869K->422305K(914176K) [PSPermGen: 81960K->81783K(164352K)], 2.4749181 secs]
-- Alternate: 2.417: [Full GC (System) [PSYoungGen: 1788K->0K(12736K)] [ParOldGen: 1084K->2843K(116544K)] 2872K->2843K(129280K) [PSPermGen: 8602K->8593K(131072K)], 0.1028360 secs]
data ParallelOldCompactingEvent = ParallelOldCompactingEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent ParallelOldCompactingEvent where
  timestamp (ParallelOldCompactingEvent t _ _ _ _ _ _) = t
instance BlockingEvent ParallelOldCompactingEvent where
  duration (ParallelOldCompactingEvent _ d _ _ _ _ _) = d
instance TimedEvent ParallelOldCompactingEvent where
  timingInfo (ParallelOldCompactingEvent _ _ t _ _ _ _) = t
instance YoungData ParallelOldCompactingEvent where
  youngData (ParallelOldCompactingEvent _ _ _ y _ _ _) = y
instance OldData ParallelOldCompactingEvent where
  oldData (ParallelOldCompactingEvent _ _ _ _ o _ _) = o
instance CombinedData ParallelOldCompactingEvent where
  combinedData (ParallelOldCompactingEvent _ _ _ _ _ c _) = c
instance PermData ParallelOldCompactingEvent where
  permData (ParallelOldCompactingEvent _ _ _ _ _ _ p) = p


data ApplicationStopEvent = ApplicationStopEvent Duration (Maybe Duration) [Event] deriving (Eq, Show)
instance BlockingEvent ApplicationStopEvent where
  duration (ApplicationStopEvent _ Nothing _) = Duration $ fromIntegral 0
  duration (ApplicationStopEvent _ (Just s) _) = s


data Event = ParallelScavengeEvent_ ParallelScavengeEvent
           | ParallelSerialOldEvent_ ParallelSerialOldEvent
           | ParallelOldCompactingEvent_ ParallelOldCompactingEvent
           | ApplicationStopEvent_ ApplicationStopEvent
           | UnparsableLineEvent_ UnparsableLineEvent
           deriving (Eq)
instance Show Event where
  show (ParallelScavengeEvent_ e) = show e
  show (ParallelOldCompactingEvent_ e) = show e
  show (ParallelSerialOldEvent_ e) = show e
  show (ApplicationStopEvent_ e) = show e
  show (UnparsableLineEvent_ e) = show e

mkParallelScavengeEvent t d ti y c = ParallelScavengeEvent_ (ParallelScavengeEvent t d ti y c)
mkParallelSerialOldEvent t d ti y c o p = ParallelSerialOldEvent_ (ParallelSerialOldEvent t d ti y o c p)
mkParallelOldCompactingEvent t d ti y c o p = ParallelOldCompactingEvent_ (ParallelOldCompactingEvent t d ti y o c p)
mkUnparsableLineEvent s = UnparsableLineEvent_ (UnparsableLineEvent s)
mkApplicationStopEvent d s inner = ApplicationStopEvent_ (ApplicationStopEvent d s inner)

