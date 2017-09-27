-- Example of using digitalWrite.
-- Run with LEDs connected to wiringPi pins 0-7:
--   https://www.flickr.com/photos/107479024@N04/31360240974/
-- Blinks the LEDs in sequence:
--   https://www.flickr.com/photos/107479024@N04/32070893472/
-- Compatible with the hs-wiringPi test board.
--   https://github.com/ppelleti/hs-wiringPi-test-board
-- Should also work with the Ladder Board:
--   https://projects.drogon.net/the-raspberry-ladder-board/
-- Can also be used with the Pi-LITEr if you change the order:
--   https://pinout.xyz/pinout/pi_lite_r
--   http://store.acmeun.com/products/pi-liter-8-led-strip-for-the-raspberry-pi.html

import Control.Concurrent
import Control.Monad

import System.Hardware.WiringPi

-- Uncomment this to use the Pi-LITEr.
-- pins = map Wpi [7, 0, 2, 1, 3, 4, 5, 6]

-- This works for hs-wiringPi test board, or for Ladder Board.
pins = map Wpi [0..7]

main = do
  forM_ pins $ \x -> pinMode x OUTPUT
  forever $ forM_ pins $ \x -> do
    digitalWrite x HIGH
    threadDelay 200000
    digitalWrite x LOW
