module GarbageHat.Domain where

import Data.Decimal

-- seconds since start and datestamp
data Timestamp = Timestamp (Maybe Decimal) (Maybe String) deriving (Eq, Show)
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


-- concurrent low-pause collector similar to ParallelScavengeEvent
-- Standard: 0.189: [GC 20.190: [ParNew: 86199K->8454K(91712K), 0.0375060 secs] 89399K->11655K(907328K), 0.0387074 secs]
-- Dated: 2010-02-26T08:31:51.990-0600: [GC [ParNew: 150784K->4291K(169600K), 0.0246670 secs] 150784K->4291K(1029760K), 0.0247500 secs] [Times: user=0.06 sys=0.01, real=0.02 secs]
data ParNewEvent = ParNewEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent ParNewEvent where
  timestamp (ParNewEvent t _ _ _ _) = t
instance BlockingEvent ParNewEvent where
  duration (ParNewEvent _ d _ _ _) = d
instance TimedEvent ParNewEvent where
  timingInfo (ParNewEvent _ _ t _ _) = t
instance YoungData ParNewEvent where
  youngData (ParNewEvent _ _ _ y _) = y
instance CombinedData ParNewEvent where
  combinedData (ParNewEvent _ _ _ _ c) = c

-- old Serial new collector
-- Standard: 7.798: [GC 7.798: [DefNew: 37172K->3631K(39296K), 0.0209300 secs] 41677K->10314K(126720K), 0.0210210 secs]
-- Erroneous 'Full': 142352.790: [Full GC 142352.790: [DefNew: 444956K->28315K(471872K), 0.0971099 secs] 1020658K->604017K(1520448K), 0.0972451 secs
data SerialNewEvent = SerialNewEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent SerialNewEvent where
  timestamp (SerialNewEvent t _ _ _ _) = t
instance BlockingEvent SerialNewEvent where
  duration (SerialNewEvent _ d _ _ _) = d
instance TimedEvent SerialNewEvent where
  timingInfo (SerialNewEvent _ _ t _ _) = t
instance YoungData SerialNewEvent where
  youngData (SerialNewEvent _ _ _ y _) = y
instance CombinedData SerialNewEvent where
  combinedData (SerialNewEvent _ _ _ _ c) = c


-- Serial old collector
-- Enabled with the -XX:+UseSerialGC JVM option
data SerialOldEvent = SerialOldEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent SerialOldEvent where
  timestamp (SerialOldEvent t _ _ _ _ _) = t
instance BlockingEvent SerialOldEvent where
  duration (SerialOldEvent _ d _ _ _ _) = d
instance TimedEvent SerialOldEvent where
  timingInfo (SerialOldEvent _ _ t _ _ _) = t
instance OldData SerialOldEvent where
  oldData (SerialOldEvent _ _ _ o _ _) = o
instance CombinedData SerialOldEvent where
  combinedData (SerialOldEvent _ _ _ _ c _) = c
instance PermData SerialOldEvent where
  permData (SerialOldEvent _ _ _ _ _ p) = p

-- Combined serial new and serial old
-- Standard: 160.678: [GC 160.678: [DefNew: 450682K->450682K(471872K), 0.0000099 secs]160.678: [Tenured: 604639K->552856K(1048576K), 1.1178810 secs] 1055322K->552856K(1520448K), 1.1180562 secs]
data SerialSerialEvent = SerialSerialEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent SerialSerialEvent where
  timestamp (SerialSerialEvent t _ _ _ _ _) = t
instance BlockingEvent SerialSerialEvent where
  duration (SerialSerialEvent _ d _ _ _ _) = d
instance TimedEvent SerialSerialEvent where
  timingInfo (SerialSerialEvent _ _ t _ _ _) = t
instance YoungData SerialSerialEvent where
  youngData (SerialSerialEvent _ _ _ y _ _) = y
instance OldData SerialSerialEvent where
  oldData (SerialSerialEvent _ _ _ _ o _) = o
instance CombinedData SerialSerialEvent where
  combinedData (SerialSerialEvent _ _ _ _ _ c) = c

-- Combined serial new and serial old with perm gen
-- Standard: 3727.365: [GC 3727.365: [DefNew: 400314K->400314K(400384K), 0.0000550 secs]3727.365: [Tenured: 837793K->597490K(889536K), 44.7498530 secs] 1238107K->597490K(1289920K), [Perm : 54745K->54745K(54784K)], 44.7501880 secs] [Times: user=5.32 sys=0.33, real=44.75 secs]
data SerialSerialPermEvent = SerialSerialPermEvent Timestamp Duration (Maybe TimingInfo) RegionUsage RegionUsage RegionUsage RegionUsage deriving (Eq, Show)
instance TimestampedEvent SerialSerialPermEvent where
  timestamp (SerialSerialPermEvent t _ _ _ _ _ _) = t
instance BlockingEvent SerialSerialPermEvent where
  duration (SerialSerialPermEvent _ d _ _ _ _ _) = d
instance TimedEvent SerialSerialPermEvent where
  timingInfo (SerialSerialPermEvent _ _ t _ _ _ _) = t
instance YoungData SerialSerialPermEvent where
  youngData (SerialSerialPermEvent _ _ _ y _ _ _) = y
instance OldData SerialSerialPermEvent where
  oldData (SerialSerialPermEvent _ _ _ _ o _ _) = o
instance CombinedData SerialSerialPermEvent where
  combinedData (SerialSerialPermEvent _ _ _ _ _ c _) = c
instance PermData SerialSerialPermEvent where
  permData (SerialSerialPermEvent _ _ _ _ _ _ p) = p

-- Full collection when only -verbose:gc JVM option specified. It does not appear to be possible to determine the collector.
-- 143132.151: [Full GC 1606823K-&gt;1409859K(2976064K), 12.0855599 secs]
data VerboseGcOldEvent = VerboseGcOldEvent Timestamp Duration (Maybe TimingInfo) RegionUsage deriving (Eq, Show)
instance TimestampedEvent VerboseGcOldEvent where
  timestamp (VerboseGcOldEvent t _ _ _) = t
instance BlockingEvent VerboseGcOldEvent where
  duration (VerboseGcOldEvent _ d _ _) = d
instance TimedEvent VerboseGcOldEvent where
  timingInfo (VerboseGcOldEvent _ _ t _) = t
instance CombinedData VerboseGcOldEvent where
  combinedData (VerboseGcOldEvent _ _ _ c) = c

-- Young collection when only -verbose:gc JVM option specified. It does not appear to be possible to determine the collector.
-- 143132.151: [Full GC 1606823K-&gt;1409859K(2976064K), 12.0855599 secs]
data VerboseGcYoungEvent = VerboseGcYoungEvent Timestamp Duration (Maybe TimingInfo) RegionUsage deriving (Eq, Show)
instance TimestampedEvent VerboseGcYoungEvent where
  timestamp (VerboseGcYoungEvent t _ _ _) = t
instance BlockingEvent VerboseGcYoungEvent where
  duration (VerboseGcYoungEvent _ d _ _) = d
instance TimedEvent VerboseGcYoungEvent where
  timingInfo (VerboseGcYoungEvent _ _ t _) = t
instance CombinedData VerboseGcYoungEvent where -- or is this Young?
  combinedData (VerboseGcYoungEvent _ _ _ c) = c


-- application time and stop time event
data ApplicationStopEvent = ApplicationStopEvent Duration (Maybe Duration) [Event] deriving (Eq, Show)
instance BlockingEvent ApplicationStopEvent where
  duration (ApplicationStopEvent _ Nothing _) = Duration $ fromIntegral 0
  duration (ApplicationStopEvent _ (Just s) _) = s


data Event = ParallelScavengeEvent_ ParallelScavengeEvent
           | ParallelSerialOldEvent_ ParallelSerialOldEvent
           | ParallelOldCompactingEvent_ ParallelOldCompactingEvent
           | ParNewEvent_ ParNewEvent
           | SerialNewEvent_ SerialNewEvent
           | SerialOldEvent_ SerialOldEvent
           | SerialSerialEvent_ SerialSerialEvent
           | SerialSerialPermEvent_ SerialSerialPermEvent
           | VerboseGcOldEvent_ VerboseGcOldEvent
           | VerboseGcYoungEvent_ VerboseGcYoungEvent
           | ApplicationStopEvent_ ApplicationStopEvent
           | UnparsableLineEvent_ UnparsableLineEvent
           deriving (Eq)
instance Show Event where
  show (ParallelScavengeEvent_ e) = show e
  show (ParallelOldCompactingEvent_ e) = show e
  show (ParallelSerialOldEvent_ e) = show e
  show (ParNewEvent_ e) = show e
  show (SerialNewEvent_ e) = show e
  show (SerialOldEvent_ e) = show e
  show (SerialSerialEvent_ e) = show e
  show (SerialSerialPermEvent_ e) = show e
  show (VerboseGcOldEvent_ e) = show e
  show (ApplicationStopEvent_ e) = show e
  show (UnparsableLineEvent_ e) = show e

mkParallelScavengeEvent = ParallelScavengeEvent_ `dot5` ParallelScavengeEvent
mkParallelSerialOldEvent = ParallelSerialOldEvent_ `dot7` ParallelSerialOldEvent
mkParallelOldCompactingEvent = ParallelOldCompactingEvent_ `dot7` ParallelOldCompactingEvent
mkParNewEvent = ParNewEvent_ `dot5` ParNewEvent
mkSerialNewEvent = SerialNewEvent_ `dot5` SerialNewEvent
mkSerialOldEvent = SerialOldEvent_ `dot6` SerialOldEvent
mkSerialSerialEvent = SerialSerialEvent_ `dot6` SerialSerialEvent
mkSerialSerialPermEvent = SerialSerialPermEvent_ `dot7` SerialSerialPermEvent
mkUnparsableLineEvent = UnparsableLineEvent_ . UnparsableLineEvent
mkApplicationStopEvent = ApplicationStopEvent_ `dot3` ApplicationStopEvent
mkVerboseGcOldEvent = VerboseGcOldEvent_ `dot4` VerboseGcOldEvent
mkVerboseGcYoungEvent = VerboseGcYoungEvent_ `dot4` VerboseGcYoungEvent

-- higher arity composing operators
dot2 = ((.).(.))
dot3 = ((.).(.).(.))
dot4 = ((.).(.).(.).(.))
dot5 = ((.).(.).(.).(.).(.))
dot6 = ((.).(.).(.).(.).(.).(.))
dot7 = ((.).(.).(.).(.).(.).(.).(.))
