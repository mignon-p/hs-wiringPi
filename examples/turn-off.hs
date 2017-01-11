-- Configures wiringPi pins 0-7 as outputs, and sets them to LOW.

import Control.Monad

import System.Hardware.WiringPi

main = do
  forM_ [0..7] $ \x -> pinMode (Wpi x) OUTPUT
  digitalWriteByte 0
