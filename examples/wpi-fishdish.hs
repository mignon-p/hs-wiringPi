-- Example for "Fish Dish" board.
--   https://www.pi-supply.com/product/fish-dish-raspberry-pi-led-buzzer-board/
--   http://store.acmeun.com/products/fish-dish-raspberry-pi-led-buzzer-board.html
-- Fish Dish uses some of the SPI pins as GPIO pins, so you'll need to
-- disable SPI in raspi-config if it is enabled.
-- You need to link this program with "-threaded", or deadlock will occur.

import Control.Concurrent
import Control.Exception
import Control.Monad

import System.Hardware.WiringPi

ledPins = map Gpio [4, 22, 9]

buzzerPin = Gpio 8

buttonPin = Gpio 7

outputPins = ledPins ++ [buzzerPin]

turnOff = forM_ outputPins $ \pin -> digitalWrite pin LOW

waitButton mv goal = do
  val <- takeMVar mv
  when (val /= goal) (waitButton mv goal)

loop mv = forM_ ledPins $ \led -> do
  digitalWrite led HIGH
  waitButton mv HIGH
  digitalWrite buzzerPin HIGH
  waitButton mv LOW
  digitalWrite buzzerPin LOW
  digitalWrite led LOW

main = do
  forM_ outputPins $ \pin -> pinMode pin OUTPUT
  pinMode buttonPin INPUT
  pullUpDnControl buttonPin PUD_OFF -- Fish Dish has an external pulldown
  turnOff
  mv <- newEmptyMVar
  wiringPiISR buttonPin INT_EDGE_BOTH (digitalRead buttonPin >>= putMVar mv)
  putStrLn "(Press control-C to exit)"
  putStrLn "Press the button on the Fish Dish..."
  forever (loop mv) `finally` turnOff
