-- This is a basic test of most of wiringPi's functionality.
-- You should connect LEDs to wiringPi pins 0, 1, 2, 3, and 7 via
-- appropriate resistors.  Connect wiringPi pins 4 and 5 to each other
-- via a 1K resistor.  Connect wiringPi pin 6 to a momentary
-- pushbutton that can pull it to GND via a 1K resistor.
--   https://www.flickr.com/photos/107479024@N04/32074853902/

import Control.Concurrent
import Control.Monad
import System.Exit

import System.Hardware.WiringPi

ledPins = map Wpi [7,0,2,3]
pwmPin = Wpi 1
connectedPins = (Wpi 4, Wpi 5)
buttonPin = Wpi 6

shortDelay :: IO ()
shortDelay = threadDelay 50000

assertPin :: Pin -> Value -> IO ()
assertPin pin expected = do
  actual <- digitalRead pin
  when (actual /= expected) $ do
    putStrLn $ "expected " ++ show expected ++ " on pin " ++ show pin ++ " but got " ++ show actual
    exitFailure

-- Given two pins connected together, test that setting the value
-- of one as an output changes the value of the other as an input.
testConnected :: Pin -> Pin -> IO ()
testConnected x y = do
  -- setup
  pinMode x INPUT
  pullUpDnControl x PUD_OFF
  pinMode y OUTPUT

  -- test with both HIGH and LOW
  forM_ [HIGH, LOW] $ \val -> do
    digitalWrite y val
    shortDelay
    assertPin x val

  -- leave both pins as inputs
  pinMode y INPUT

-- test pullup/pulldown functionality (assumes button is not pressed)
testPullUpDown :: IO ()
testPullUpDown = do
  -- setup
  pinMode buttonPin INPUT

  -- test both pulldown and pullup
  forM_ [(PUD_DOWN, LOW), (PUD_UP, HIGH)] $ \(pud, val) -> do
    pullUpDnControl buttonPin pud
    shortDelay
    assertPin buttonPin val

-- blink LEDs and wait for user to press button
blinkAndWait :: IO ()
blinkAndWait = do
  -- setup
  putStrLn "One LED should be pulsing, and four LEDs should be blinking."
  putStrLn "Press button to exit."
  forM_ ledPins $ \pin -> pinMode pin OUTPUT
  pinMode pwmPin PWM_OUTPUT
  pinMode buttonPin INPUT
  pullUpDnControl buttonPin PUD_UP

  -- blink LEDs and wait for button to be pressed
  let loop x = do
        let y = (sin x + 1) / 2
            y' = y ** 2.8       -- gamma correction
            analog = y' * 1024
        pwmWrite pwmPin $ round analog
        let n = floor x `mod` length ledPins
        forM_ (zip ledPins [0..]) $ \(pin, i) ->
          digitalWrite pin (if i == n then HIGH else LOW)
        shortDelay
        button <- digitalRead buttonPin
        when (button == HIGH) $ loop (x + 0.1)

  loop 0

  -- leave all LEDs off
  forM_ ledPins $ \pin -> digitalWrite pin LOW
  pwmWrite pwmPin 0

main = do
  rev <- piGpioLayout
  putStrLn $ "piGpioLayout = " ++ show rev

  testConnected (fst connectedPins) (snd connectedPins)
  testConnected (snd connectedPins) (fst connectedPins)
  testPullUpDown
  blinkAndWait
