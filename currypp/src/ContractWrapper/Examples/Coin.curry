{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=contracts #-}

import Test.Prop

-- Examples with non-determinististic specifications

coin'spec :: Int
coin'spec = 0 ? 1

coin :: Int
coin = 1 ? 0   --> should be executed without violation

coinCorrect = coin <~> coin'spec


coin3'spec :: Int
coin3'spec = coin'spec

coin3 :: Int
coin3 = coin ? 2  --> should produce a violation

coin3Violation = toError coin3
