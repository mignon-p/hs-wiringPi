import Control.Concurrent
import Control.Monad
import Data.Bits

import System.Hardware.WiringPi

main = do
  wiringPiSetup
  forM_ [0..7] $ \x -> pinMode x OUTPUT
  forever $ forM_ [0..7] $ \x -> do
    digitalWriteByte $ 1 `shiftL` x
    threadDelay 200000
