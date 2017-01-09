{-# LANGUAGE CPP, ForeignFunctionInterface #-}

{-|
Module      : System.Hardware.WiringPi
Description : Bindings to wiringPi library
Copyright   : © Patrick Pelletier, 2017
License     : BSD3
Maintainer  : code@funwithsoftware.org
Stability   : experimental
Portability : Raspberry Pi

This is a Haskell binding to the <http://wiringpi.com/ wiringPi library>.
The functions here correspond directly to the functions in the C library.
To use this library, you must either run as root, or set the @WIRINGPI_GPIOMEM@
environment variable.  However, if you set @WIRINGPI_GPIOMEM@, then
<http://wiringpi.com/wiringpi-update-to-2-29/ PWM does not work>,
so to use PWM you must be root.
-}

module System.Hardware.WiringPi
  ( -- * Types
    Pin
  , Value (..)
  , Mode (..)
  , Pud (..)
  , PwmMode (..)
  , PwmValue
    -- * Setup functions
    -- | See <http://wiringpi.com/reference/setup/ WiringPi Setup functions>.
    -- You must call one of these functions, only once, before calling any
    -- other wiringPi functions.  The pin numbering scheme depends on which
    -- setup function you call.  On error, the program will be terminated,
    -- unless the environment variable @WIRINGPI_CODES@ is set, in which
    -- case an exception will be thrown.
  , wiringPiSetup
  , wiringPiSetupGpio
  , wiringPiSetupPhys
  , wiringPiSetupSys
    -- * Core functions
    -- | See <http://wiringpi.com/reference/core-functions/ Core wiringPi functions>.
  , pinMode
  , pullUpDnControl
  , digitalRead
  , digitalWrite
  , pwmWrite
    -- * Additional functions
    -- | See <http://wiringpi.com/reference/raspberry-pi-specifics/ Raspberry Pi specific functions>.
  , digitalWriteByte
  , pwmSetMode
  , pwmSetRange
  , pwmSetClock
  , piBoardRev
  , wpiPinToGpio
  , physPinToGpio
  ) where

import Control.Applicative
import Control.Monad ( when )
import Data.Word ( Word8, Word16 )
import Foreign.C.Types ( CInt(..), CUInt(..) )

#include <wiringPi.h>

-- | Represents a <http://wiringpi.com/pins/ pin number>.
-- The meaning of the pin number depends on which setup function was called.
type Pin = CInt

-- | Value used with 'pwmWrite'.  Typically ranges from 0-1024, but the
-- range can be increased up to 4096 by calling 'pwmSetRange'.
type PwmValue = Word16

-- | Digital logic level.
data Value = LOW | HIGH deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Pin mode, used with 'pinMode'.
data Mode = INPUT      -- ^ digital input
          | OUTPUT     -- ^ digital output
          | PWM_OUTPUT -- ^ pulse-width modulation; only supported on wiringPi pin 1
          | GPIO_CLOCK -- ^ clock output; only supported on wiringPi pin 7
          deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Use with 'pullUpDnControl' to enable internal pull-up or pull-down
-- resistor.
data Pud = PUD_OFF  -- ^ disable pull-up/pull-down
         | PUD_DOWN -- ^ enable pull-down resistor
         | PUD_UP   -- ^ enable pull-up resistor
         deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Argument to 'pwmSetMode' to set \"balanced\" mode or \"mark-space\" mode.
data PwmMode = PWM_MODE_BAL -- ^ balanced mode
             | PWM_MODE_MS  -- ^ mark-space mode
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

pwmModeToInt :: PwmMode -> CInt
pwmModeToInt PWM_MODE_BAL = #const PWM_MODE_BAL
pwmModeToInt PWM_MODE_MS  = #const PWM_MODE_MS

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

doWiringPiSetup :: IO CInt -> String -> IO ()
doWiringPiSetup setupFunc name = do
  ret <- setupFunc
  when (ret /= 0) $
    fail $ "failing return code " ++ show ret ++ " for " ++ name

-- | Use <http://wiringpi.com/pins/ wiringPi's pin numbers>.
wiringPiSetup :: IO ()
wiringPiSetup = doWiringPiSetup c_wiringPiSetup "wiringPiSetup"

-- | Use Broadcom chip's GPIO numbers.  These are the numbers on the
-- <https://www.adafruit.com/products/1754 Adafruit cobbler>.
wiringPiSetupGpio :: IO ()
wiringPiSetupGpio = doWiringPiSetup c_wiringPiSetupGpio "wiringPiSetupGpio"

-- | Use physical pin numbers on the P1 connector.
wiringPiSetupPhys :: IO ()
wiringPiSetupPhys = doWiringPiSetup c_wiringPiSetupPhys "wiringPiSetupPhys"

-- | Use Broadcom chip's GPIO numbers.  Accesses the GPIO via
-- @\/sys\/class\/GPIO@.  A bunch of functionality is not available
-- in this mode.
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

-- | Default range is 0-1024, but it can be changed with 'pwmSetRange'.
pwmWrite :: Pin -> PwmValue -> IO ()
pwmWrite pin val = c_pwmWrite pin $ fromIntegral val

-- | Write 8 bits to the 8 pins that have wiringPi pin numbers 0-7.
digitalWriteByte :: Word8 -> IO ()
digitalWriteByte w = c_digitalWriteByte $ fromIntegral w

pwmSetMode :: PwmMode -> IO ()
pwmSetMode mode = c_pwmSetMode $ pwmModeToInt mode

-- | Change the range used by 'pwmWrite'.  Default is 1024.
-- <https://raspberrypi.stackexchange.com/questions/4906/control-hardware-pwm-frequency/9725#9725 Maxium is 4096>.
pwmSetRange :: PwmValue -> IO ()
pwmSetRange range = c_pwmSetRange $ fromIntegral range

-- | Change the PWM divisor.  Range is
-- <https://raspberrypi.stackexchange.com/questions/4906/control-hardware-pwm-frequency/9725#9725 2-4095>.
pwmSetClock :: PwmValue -> IO ()
pwmSetClock divisor = c_pwmSetClock $ fromIntegral divisor

piBoardRev :: IO Int
piBoardRev = fromIntegral <$> c_piBoardRev

wpiPinToGpio :: Pin -> IO Pin
wpiPinToGpio = c_wpiPinToGpio

physPinToGpio :: Pin -> IO Pin
physPinToGpio = c_physPinToGpio
