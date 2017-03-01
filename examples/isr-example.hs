-- Example of using wiringPiISR
-- Run with LED connected to wiringPi pin 1 (GPIO 18)
-- and button connected to wiringPi pin 6 (GPIO 25)
--   https://goo.gl/photos/VjPDQ8HW1Dp2vEFW7
-- Blinks the LED when toggle button
--   https://goo.gl/photos/VjPDQ8HW1Dp2vEFW7

import Control.Concurrent
import Control.Monad
import Data.IORef

import System.Hardware.WiringPi

led :: Pin
led = Wpi 1

button :: Pin
button = Wpi 6

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
