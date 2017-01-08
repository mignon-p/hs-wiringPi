{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module System.Hardware.WiringPi
  ( Pin
  , Value (..)
  , Mode (..)
  , Pud (..)
  , AnalogValue
  , wiringPiSetup
  , wiringPiSetupGpio
  , wiringPiSetupPhys
  , wiringPiSetupSys
  , pinMode
  , pullUpDnControl
  , digitalRead
  , digitalWrite
  , pwmWrite
  , digitalWriteByte
  , piBoardRev
  , wpiPinToGpio
  , physPinToGpio
  ) where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.Types

#include <wiringPi.h>

type Pin = CInt

type AnalogValue = CInt

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

foreign import ccall unsafe "wiringPi.h wiringPiSetupGpio"
    c_wiringPiSetupGpio :: IO CInt

foreign import ccall unsafe "wiringPi.h wiringPiSetupPhys"
    c_wiringPiSetupPhys :: IO CInt

foreign import ccall unsafe "wiringPi.h wiringPiSetupSys"
    c_wiringPiSetupSys :: IO CInt

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

foreign import ccall unsafe "wiringPi.h piBoardRev"
    c_piBoardRev :: IO CInt

foreign import ccall unsafe "wiringPi.h wpiPinToGpio"
    c_wpiPinToGpio :: CInt
                   -> IO CInt

foreign import ccall unsafe "wiringPi.h physPinToGpio"
    c_physPinToGpio :: CInt
                    -> IO CInt

doWiringPiSetup :: IO CInt -> String -> IO ()
doWiringPiSetup setupFunc name = do
  ret <- setupFunc
  when (ret /= 0) $
    fail $ "failing return code " ++ show ret ++ " for " ++ name

wiringPiSetup :: IO ()
wiringPiSetup = doWiringPiSetup c_wiringPiSetup "wiringPiSetup"

wiringPiSetupGpio :: IO ()
wiringPiSetupGpio = doWiringPiSetup c_wiringPiSetupGpio "wiringPiSetupGpio"

wiringPiSetupPhys :: IO ()
wiringPiSetupPhys = doWiringPiSetup c_wiringPiSetupPhys "wiringPiSetupPhys"

wiringPiSetupSys :: IO ()
wiringPiSetupSys = doWiringPiSetup c_wiringPiSetupSys "wiringPiSetupSys"

pinMode :: Pin -> Mode -> IO ()
pinMode pin mode = c_pinMode pin $ modeToInt mode

pullUpDnControl :: Pin -> Pud -> IO ()
pullUpDnControl pin pud = c_pullUpDnControl pin $ pudToInt pud

digitalRead :: Pin -> IO Value
digitalRead pin = intToValue <$> c_digitalRead pin

digitalWrite :: Pin -> Value -> IO ()
digitalWrite pin val = c_digitalWrite pin $ valueToInt val

pwmWrite :: Pin -> AnalogValue -> IO ()
pwmWrite = c_pwmWrite

digitalWriteByte :: Word8 -> IO ()
digitalWriteByte w = c_digitalWriteByte $ fromIntegral w

piBoardRev :: IO Int
piBoardRev = fromIntegral <$> c_piBoardRev

wpiPinToGpio :: Pin -> IO Pin
wpiPinToGpio = c_wpiPinToGpio

physPinToGpio :: Pin -> IO Pin
physPinToGpio = c_physPinToGpio
