{-# LANGUAGE CPP, MultiParamTypeClasses #-}

import System.CPUTime
import System.Mem (performGC)

#if __GLASGOW_HASKELL__ > 702
import GHC.Stats
#endif

-- #endimport - do not remove this line!

instance ConvertCurryHaskell C_ProcessInfo C_ProcessInfo where
  toCurry   = id
  fromCurry = id

getProcessInfos :: IO [(C_ProcessInfo, Int)]
#if __GLASGOW_HASKELL__ > 802
getProcessInfos = do
  stats <- getRTSStats
  return [ (C_RunTime           , fromIntegral (mutator_cpu_ns      stats * 1000))
         , (C_ElapsedTime       , fromIntegral (mutator_elapsed_ns  stats * 1000))
         , (C_Heap              , fromIntegral (max_live_bytes      stats))
         , (C_Memory            , fromIntegral (max_live_bytes      stats))
         , (C_GarbageCollections, fromIntegral (gcs                 stats))
         ]
#elif __GLASGOW_HASKELL__ > 702
getProcessInfos = do
  stats <- getGCStats
  return [ (C_RunTime           , floor (mutatorCpuSeconds   stats * 1000))
         , (C_ElapsedTime       , floor (mutatorWallSeconds  stats * 1000))
         , (C_Heap              , fromIntegral (maxBytesUsed stats))
         , (C_Memory            , fromIntegral (maxBytesUsed stats))
         , (C_GarbageCollections, fromIntegral (numGcs       stats))
         ]
#else
getProcessInfos = do
  t <- getCPUTime
  return [(C_RunTime, t `div` (10^9)]
#endif

external_d_C_getProcessInfos :: Cover -> ConstStore ->
   Curry_Prelude.C_IO (Curry_Prelude.OP_List (Curry_Prelude.OP_Tuple2 C_ProcessInfo Curry_Prelude.C_Int))
external_d_C_getProcessInfos _ _ = toCurry getProcessInfos

external_d_C_garbageCollectorOff :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_garbageCollectorOff _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollectorOn :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_garbageCollectorOn _ _ = toCurry (return () :: IO ()) -- not supported

external_d_C_garbageCollect :: Cover -> ConstStore -> Curry_Prelude.C_IO Curry_Prelude.OP_Unit
external_d_C_garbageCollect _ _ = toCurry performGC
