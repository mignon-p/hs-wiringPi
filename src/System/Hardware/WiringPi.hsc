{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Hardware.WiringPi
  ( Pin
  , Value (..)
  , Mode (..)
  , Pud (..)
  , wiringPiSetup
  , pinMode
  , pullUpDnControl
  , digitalRead
  ) where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.Types

#include <wiringPi.h>

type Pin = CInt

data Value = LOW | HIGH deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Mode = INPUT | OUTPUT | PWM_OUTPUT | GPIO_CLOCK
          deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Pud = PUD_OFF | PUD_DOWN | PUD_UP
         deriving (Eq, Ord, Show, Read, Enum, Bounded)

valueToInt :: Value -> CInt
valueToInt LOW  = #const LOW
valueToInt HIGH = #const HIGH

intToValue :: CInt -> Value
intToValue #const LOW
  = LOW
intToValue _          = HIGH

modeToInt :: Mode -> CInt
modeToInt INPUT      = #const INPUT
modeToInt OUTPUT     = #const OUTPUT
modeToInt PWM_OUTPUT = #const PWM_OUTPUT
modeToInt GPIO_CLOCK = #const GPIO_CLOCK

pudToInt :: Pud -> CInt
pudToInt PUD_OFF  = #const PUD_OFF
pudToInt PUD_DOWN = #const PUD_DOWN
pudToInt PUD_UP   = #const PUD_UP

foreign import ccall unsafe "wiringPi.h wiringPiSetup"
    c_wiringPiSetup :: IO CInt

foreign import ccall unsafe "wiringPi.h pinMode"
    c_pinMode :: CInt
              -> CInt
              -> IO ()

foreign import ccall unsafe "wiringPi.h pullUpDnControl"
    c_pullUpDnControl :: CInt
                      -> CInt
                      -> IO ()

foreign import ccall unsafe "wiringPi.h digitalRead"
    c_digitalRead :: CInt
                  -> IO CInt

wiringPiSetup :: IO ()
wiringPiSetup = do
  ret <- c_wiringPiSetup
  when (ret /= 0) $
    fail $ "failing return code " ++ show ret ++ " for wiringPiSetup"

pinMode :: Pin -> Mode -> IO ()
pinMode pin mode = c_pinMode pin $ modeToInt mode

pullUpDnControl :: Pin -> Pud -> IO ()
pullUpDnControl pin pud = c_pullUpDnControl pin $ pudToInt pud

digitalRead :: Pin -> IO Value
digitalRead pin = intToValue <$> c_digitalRead pin
