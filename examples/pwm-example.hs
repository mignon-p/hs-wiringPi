-- Example of using pwmWrite.
-- Run with an LED connected to wiringPi pin 1:
--   https://www.flickr.com/photos/107479024@N04/32201782695/
-- Pulses the LED.

import Control.Concurrent
import Control.Monad

import System.Hardware.WiringPi

pwmPin = 1

main = do
  wiringPiSetup
  pinMode pwmPin PWM_OUTPUT
  forM_ [0, 0.1 ..] $ \x -> do
    let y = sin x
        y' = (y + 1) / 2
        analog = y' * 1024
    pwmWrite pwmPin $ round analog
    threadDelay 50000
