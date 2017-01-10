-- Example of using digitalWriteByte.
-- Run with LEDs connected to wiringPi pins 0-7:
--   https://www.flickr.com/photos/107479024@N04/31360240974/
-- Blinks the LEDs in sequence:
--   https://www.flickr.com/photos/107479024@N04/32070893472/

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
