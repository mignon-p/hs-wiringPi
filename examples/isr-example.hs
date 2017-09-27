-- Example of using wiringPiISR.
-- Run with LED connected to wiringPi pin 1 (GPIO 18)
-- and button connected to wiringPi pin 25 (GPIO 26).
-- Lights the LED when button is pressed.
--   https://goo.gl/photos/VjPDQ8HW1Dp2vEFW7
-- Compatible with the hs-wiringPi test board.
--   https://github.com/ppelleti/hs-wiringPi-test-board
-- You need to link this program with "-threaded", or deadlock will occur.

import Control.Concurrent
import Control.Monad
import Data.IORef

import System.Hardware.WiringPi

led :: Pin
led = Wpi 1

button :: Pin
button = Wpi 25

-- Is button connected to GND?
activeLow :: Bool
activeLow = True

main = do
  pinMode led OUTPUT
  pinMode button INPUT
  pullUpDnControl button $ if activeLow then PUD_UP else PUD_DOWN
  wiringPiISR button INT_EDGE_BOTH handleButton
  forever $ getChar
 where
  handleButton = do
    val <- digitalRead button
    let inv = if val == LOW then HIGH else LOW
    digitalWrite led $ if activeLow then inv else val
