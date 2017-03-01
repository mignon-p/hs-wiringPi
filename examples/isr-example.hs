-- Example of using wiringPiISR
-- Run with LED connected to wiringPi pin 8 (GPIO 14)
-- and button connected to wiringPi pin 10 (GPIO 15)
--   https://goo.gl/photos/VjPDQ8HW1Dp2vEFW7
-- Blinks the LED when toggle button
--   https://goo.gl/photos/VjPDQ8HW1Dp2vEFW7

import Control.Concurrent
import Control.Monad
import Data.IORef

import System.Hardware.WiringPi

led :: Pin
led = Gpio 14

button :: Pin
button = Gpio 15

main = do
  ref <- newIORef LOW
  pinMode led OUTPUT
  pinMode button INPUT
  wiringPiISR button INT_EDGE_BOTH $ handleButton ref
  forever $ getChar
 where
  handleButton ref = do
    val <- readIORef ref
    let next = if val == LOW then HIGH else LOW
    digitalWrite led next
    modifyIORef ref $ id . const next
