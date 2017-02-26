-- Example of using digitalWrite.
-- Run with LEDs connected to wiringPi pins 0-7:
--   https://www.flickr.com/photos/107479024@N04/31360240974/
-- Blinks the LEDs in sequence:
--   https://www.flickr.com/photos/107479024@N04/32070893472/
-- Can also be used with the Pi-LITEr:
--   https://pinout.xyz/pinout/pi_lite_r
--   http://store.acmeun.com/products/pi-liter-8-led-strip-for-the-raspberry-pi.html
-- Should also work with the Ladder Board, although you'd need to
-- change "pins" to [0..7] to get the right order:
--   https://projects.drogon.net/the-raspberry-ladder-board/

import Control.Concurrent
import Control.Monad

import System.Hardware.WiringPi

-- This is the order in which the LEDs appear on the Pi-LITEr,
-- by wiringPi pin number.
pins = map Wpi [7, 0, 2, 1, 3, 4, 5, 6]

main = do
  forM_ pins $ \x -> pinMode x OUTPUT
  forever $ forM_ pins $ \x -> do
    digitalWrite x HIGH
    threadDelay 200000
    digitalWrite x LOW
