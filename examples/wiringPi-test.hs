-- This is a basic test of most of wiringPi's functionality.
-- You should connect LEDs to wiringPi pins 0-7, 23, and 26 via
-- appropriate resistors.  Connect wiringPi pins 21 and 22 to each other
-- via a 1K resistor.  Connect wiringPi pin 25 to a momentary
-- pushbutton that can pull it to GND via a 1K resistor.
-- This is compatible with the hs-wiringPi Test Board:
--   https://github.com/ppelleti/hs-wiringPi-test-board

import Control.Concurrent
import Control.Monad
import Data.Bits
import System.Exit

import System.Hardware.WiringPi

ledPins = map Wpi [0..7]
pwmPins = map Wpi [26, 23]
connectedPins = (Wpi 21, Wpi 22)
buttonPin = Wpi 25
cylon = [0..6] ++ [7,6..1]

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
  putStrLn "Two LEDs should be pulsing, and eight LEDs should be blinking."
  putStrLn "Press button to exit."
  forM_ ledPins $ \pin -> pinMode pin OUTPUT
  forM_ pwmPins $ \pin -> pinMode pin PWM_OUTPUT
  pinMode buttonPin INPUT
  pullUpDnControl buttonPin PUD_UP

  -- blink LEDs and wait for button to be pressed
  let loop x = do
        let k = 2 * pi / fromIntegral (length cylon)
            y1 = (cos (x * k) + 1) / 2
            y2 = 1 - y1
            y' = map (** 2.8) [y1, y2]   -- gamma correction
            analog = map (* 1024) y'
        zipWithM_ (\pin a -> pwmWrite pin $ round a) pwmPins analog
        let n = cylon !! (floor x `mod` length cylon)
        digitalWriteByte $ bit n
        shortDelay
        button <- digitalRead buttonPin
        when (button == HIGH) $ loop (x + 0.1)

  loop 0

  -- leave all LEDs off
  forM_ ledPins $ \pin -> digitalWrite pin LOW
  forM_ pwmPins $ \pin -> pwmWrite pin 0

main = do
  rev <- piGpioLayout
  putStrLn $ "piGpioLayout = " ++ show rev

  testConnected (fst connectedPins) (snd connectedPins)
  testConnected (snd connectedPins) (fst connectedPins)
  testPullUpDown
  blinkAndWait
