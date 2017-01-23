{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Hardware.WiringPi.Foreign
  ( valueToInt
  , intToValue
  , modeToInt
  , pudToInt
  , pwmModeToInt
  , c_wiringPiSetupGpio
  , c_pinMode
  , c_pullUpDnControl
  , c_digitalRead
  , c_digitalWrite
  , c_pwmWrite
  , c_digitalWriteByte
  , c_pwmSetMode
  , c_pwmSetRange
  , c_pwmSetClock
  , c_piBoardRev
  , c_wpiPinToGpio
  , c_physPinToGpio
  ) where

import Foreign.C.Types ( CInt(..), CUInt(..) )
import System.Hardware.WiringPi.Enums

#include <wiringPi.h>

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

pwmModeToInt :: PwmMode -> CInt
pwmModeToInt PWM_MODE_BAL = #const PWM_MODE_BAL
pwmModeToInt PWM_MODE_MS  = #const PWM_MODE_MS

foreign import ccall unsafe "wiringPi.h wiringPiSetupGpio"
    c_wiringPiSetupGpio :: IO CInt

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

foreign import ccall unsafe "wiringPi.h digitalWrite"
    c_digitalWrite :: CInt
                   -> CInt
                   -> IO ()

foreign import ccall unsafe "wiringPi.h pwmWrite"
    c_pwmWrite :: CInt
               -> CInt
               -> IO ()

foreign import ccall unsafe "wiringPi.h digitalWriteByte"
    c_digitalWriteByte :: CInt
                       -> IO ()

foreign import ccall unsafe "wiringPi.h pwmSetMode"
    c_pwmSetMode :: CInt
                 -> IO ()

foreign import ccall unsafe "wiringPi.h pwmSetRange"
    c_pwmSetRange :: CUInt
                  -> IO ()

foreign import ccall unsafe "wiringPi.h pwmSetClock"
    c_pwmSetClock :: CInt
                  -> IO ()

foreign import ccall unsafe "wiringPi.h piBoardRev"
    c_piBoardRev :: IO CInt

foreign import ccall unsafe "wiringPi.h wpiPinToGpio"
    c_wpiPinToGpio :: CInt
                   -> IO CInt

foreign import ccall unsafe "wiringPi.h physPinToGpio"
    c_physPinToGpio :: CInt
                    -> IO CInt
