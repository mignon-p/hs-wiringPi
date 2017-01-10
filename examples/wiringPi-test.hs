-- This is a basic test of most of wiringPi's functionality.
-- You should connect LEDs to wiringPi pins 0, 1, 2, 3, and 7.
-- Connect wiringPi pins 4 and 5 to each other.
-- Connect wiringPi pin 6 to a momentary pushbutton that can pull it to GND.
--   https://www.flickr.com/photos/107479024@N04/32074853902/

import Control.Concurrent
import Control.Monad
import System.Exit

import System.Hardware.WiringPi

shortDelay :: IO ()
shortDelay = threadDelay 50000

assertPin :: Pin -> Value -> IO ()
assertPin pin expected = do
  actual <- digitalRead pin
  when (actual /= expected) $ do
    putStrLn $ "expected " ++ show expected ++ " on pin " ++ show pin ++ " but got " ++ show actual
    exitFailure

main = do
  -- setup
  wiringPiSetup
  pinMode 0 OUTPUT
  pinMode 1 PWM_OUTPUT
  pinMode 2 OUTPUT
  pinMode 3 OUTPUT
  pinMode 4 OUTPUT
  pinMode 5 INPUT
  pinMode 6 INPUT
  pinMode 7 OUTPUT

  pullUpDnControl 5 PUD_OFF
  pullUpDnControl 6 PUD_DOWN

  let ledPins = [7,0,2,3]
      pwmPin = 1

  forM_ ledPins $ \pin -> digitalWrite pin HIGH
  pwmWrite pwmPin 0

  -- test an input connected to an output
  digitalWrite 4 LOW
  shortDelay
  assertPin 5 LOW

  digitalWrite 4 HIGH
  shortDelay
  assertPin 5 HIGH

  -- and the same thing in the other direction
  pinMode 4 INPUT
  pinMode 5 OUTPUT
  pullUpDnControl 4 PUD_OFF

  digitalWrite 5 LOW
  shortDelay
  assertPin 4 LOW

  digitalWrite 5 HIGH
  shortDelay
  assertPin 4 HIGH
  digitalWrite 5 LOW
  pinMode 5 INPUT

  -- test pullup/pulldown functionality (assumes button is not pressed)
  assertPin 6 LOW
  pullUpDnControl 6 PUD_UP
  shortDelay
  assertPin 6 HIGH

  -- now blink LEDs and wait for user to press button
  putStrLn "One LED should be pulsing, and four LEDs should be blinking."
  putStrLn "Press button to exit."
  forM_ [0, 0.1 ..] $ \x -> do
    let y = sin x
        y' = (y + 1) / 2
        analog = y' * 1024
    pwmWrite pwmPin $ round analog
    let n = floor x `mod` 4
    forM_ [0..3] $ \i ->
      digitalWrite (ledPins !! i) (if i == n then HIGH else LOW)
    shortDelay
    button <- digitalRead 6
    when (button == LOW) $ do
      forM_ ledPins $ \pin -> digitalWrite pin LOW
      pwmWrite pwmPin 0
      exitSuccess
