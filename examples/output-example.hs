import Control.Concurrent
import Control.Monad

import System.Hardware.WiringPi

main = do
  wiringPiSetup
  forM_ [0..7] $ \x -> pinMode x OUTPUT
  forever $ forM_ [0..7] $ \x -> do
    digitalWrite x HIGH
    threadDelay 200000
    digitalWrite x LOW
