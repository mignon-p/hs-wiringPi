-- Example of using pwmWrite.
-- Run with an LED connected to wiringPi pin 1:
--   https://www.flickr.com/photos/107479024@N04/32201782695/
-- Pulses the LED.
-- Compatible with the hs-wiringPi test board.
--   https://github.com/ppelleti/hs-wiringPi-test-board

import Control.Concurrent
import Control.Monad

import System.Hardware.WiringPi

pwmPin = Wpi 1

main = do
  pinMode pwmPin PWM_OUTPUT
  forM_ [0, 0.1 ..] $ \x -> do
    let y = (sin x + 1) / 2
        y' = y ** 2.8       -- gamma correction
        analog = y' * 1024
    pwmWrite pwmPin $ round analog
    threadDelay 50000
