import Control.Concurrent
import Control.Monad

import System.Hardware.WiringPi

main = do
  wiringPiSetupGpio
  forM_ [0..7] $ \x -> pinMode x OUTPUT
  forever $ forM_ [0..7] $ \x -> do
    x' <- wpiPinToGpio x
    digitalWrite x' HIGH
    threadDelay 200000
    digitalWrite x' LOW
