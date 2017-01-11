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
  rev <- piBoardRev
  putStrLn $ "piBoardRev = " ++ show rev

  -- setup
  pinMode (Wpi 0) OUTPUT
  pinMode (Wpi 1) PWM_OUTPUT
  pinMode (Wpi 2) OUTPUT
  pinMode (Wpi 3) OUTPUT
  pinMode (Wpi 4) OUTPUT
  pinMode (Wpi 5) INPUT
  pinMode (Wpi 6) INPUT
  pinMode (Wpi 7) OUTPUT

  pullUpDnControl (Wpi 5) PUD_OFF
  pullUpDnControl (Wpi 6) PUD_DOWN

  let ledPins = map Wpi [7,0,2,3]
      pwmPin = Wpi 1

  forM_ ledPins $ \pin -> digitalWrite pin HIGH
  pwmWrite pwmPin 0

  -- test an input connected to an output
  digitalWrite (Wpi 4) LOW
  shortDelay
  assertPin (Wpi 5) LOW

  digitalWrite (Wpi 4) HIGH
  shortDelay
  assertPin (Wpi 5) HIGH

  -- and the same thing in the other direction
  pinMode (Wpi 4) INPUT
  pinMode (Wpi 5) OUTPUT
  pullUpDnControl (Wpi 4) PUD_OFF

  digitalWrite (Wpi 5) LOW
  shortDelay
  assertPin (Wpi 4) LOW

  digitalWrite (Wpi 5) HIGH
  shortDelay
  assertPin (Wpi 4) HIGH
  digitalWrite (Wpi 5) LOW
  pinMode (Wpi 5) INPUT

  -- test pullup/pulldown functionality (assumes button is not pressed)
  assertPin (Wpi 6) LOW
  pullUpDnControl (Wpi 6) PUD_UP
  shortDelay
  assertPin (Wpi 6) HIGH

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
    button <- digitalRead (Wpi 6)
    when (button == LOW) $ do
      forM_ ledPins $ \pin -> digitalWrite pin LOW
      pwmWrite pwmPin 0
      exitSuccess
