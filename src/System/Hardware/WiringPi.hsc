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
The functions here (mostly) correspond directly to the functions in the C
library, except for how initialization and pin numbering are handled.
To use this library, you must either run as root, or set the @WIRINGPI_GPIOMEM@
environment variable.  However, if you set @WIRINGPI_GPIOMEM@, then
<http://wiringpi.com/wiringpi-update-to-2-29/ PWM does not work>,
so to use PWM you must be root.
-}

module System.Hardware.WiringPi
  ( -- * Types
    Pin (..)
  , Value (..)
  , Mode (..)
  , Pud (..)
  , PwmMode (..)
  , PwmValue
    -- * Setup function
    -- | See <http://wiringpi.com/reference/setup/ WiringPi Setup functions>.
    -- Unlike the C version of wiringPi, the Haskell binding will automatically
    -- call the setup function the first time a wiringPi function is called.
    -- The only reason to call it manually is if you want to check for errors
    -- earlier than your first call.  It is also harmless to call it multiple
    -- times.  In the Haskell binding the \"GPIO\"
    -- numbering scheme is always used internally, but the 'Pin' constructors
    -- allow you to choose whichever numbering scheme you want, on a pin-by-pin
    -- basis.  This avoids having to choose a single pin numbering scheme at
    -- initialization time, as you do with the C library.
  , wiringPiSetupGpio
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
  , pinToBcmGpio
  ) where

import Control.Applicative
import Control.Exception ( evaluate )
import Control.Monad ( when )
import Data.Ord ( comparing )
import Data.Word ( Word8, Word16 )
import Foreign.C.Types ( CInt(..), CUInt(..) )
import System.IO.Unsafe ( unsafePerformIO )

#include <wiringPi.h>

-- | Represents a <http://wiringpi.com/pins/ pin number>.
-- The constructor determines which one of the three
-- pin numbering schemes is used.
data Pin = Wpi Int   -- ^ use wiringPi pin number
         | Gpio Int  -- ^ use BCM_GPIO numbering (these are the numbers on the
                     --   <https://www.adafruit.com/products/1754 Adafruit cobbler>).
         | Phys Int  -- ^ use physical pin numbers on P1 connector
  deriving (Show, Read)

instance Ord Pin where
  compare = comparing pinToBcmGpio

instance Eq Pin where
  p1 == p2 = compare p1 p2 == EQ

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

doWiringPiSetup :: IO ()
doWiringPiSetup = do
  ret <- c_wiringPiSetupGpio
  when (ret /= 0) $
    fail $ "failing return code " ++ show ret ++ " for wiringPiSetupGpio"

-- Use a CAF to do initialization once.  Borrowed this trick from the
-- "network" package.
initWiringPiSetup :: ()
initWiringPiSetup = unsafePerformIO $ doWiringPiSetup

-- | Initialize the wiringPi library.  This is optional, because it will
-- automatically be called on the first use of a wiringPi function.
-- On error, the program will be terminated,
-- unless the environment variable @WIRINGPI_CODES@ is set, in which
-- case an exception will be thrown.
wiringPiSetupGpio :: IO ()
wiringPiSetupGpio = evaluate initWiringPiSetup

pinMode :: Pin -> Mode -> IO ()
pinMode pin mode = do
  wiringPiSetupGpio
  c_pinMode (pin2bcm pin "pinMode") (modeToInt mode)

pullUpDnControl :: Pin -> Pud -> IO ()
pullUpDnControl pin pud = do
  wiringPiSetupGpio
  c_pullUpDnControl (pin2bcm pin "pullUpDnControl") (pudToInt pud)

digitalRead :: Pin -> IO Value
digitalRead pin = do
  wiringPiSetupGpio
  intToValue <$> c_digitalRead (pin2bcm pin "digitalRead")

digitalWrite :: Pin -> Value -> IO ()
digitalWrite pin val = do
  wiringPiSetupGpio
  c_digitalWrite (pin2bcm pin "digitalWrite") (valueToInt val)

-- | Default range is 0-1024, but it can be changed with 'pwmSetRange'.
pwmWrite :: Pin -> PwmValue -> IO ()
pwmWrite pin val = do
  wiringPiSetupGpio
  c_pwmWrite (pin2bcm pin "pwmWrite") (fromIntegral val)

-- | Write 8 bits to the 8 pins that have wiringPi pin numbers 0-7.
digitalWriteByte :: Word8 -> IO ()
digitalWriteByte w = do
  wiringPiSetupGpio
  c_digitalWriteByte $ fromIntegral w

pwmSetMode :: PwmMode -> IO ()
pwmSetMode mode = do
  wiringPiSetupGpio
  c_pwmSetMode $ pwmModeToInt mode

-- | Change the range used by 'pwmWrite'.  Default is 1024.
-- <https://raspberrypi.stackexchange.com/questions/4906/control-hardware-pwm-frequency/9725#9725 Maxium is 4096>.
pwmSetRange :: PwmValue -> IO ()
pwmSetRange range = do
  wiringPiSetupGpio
  c_pwmSetRange $ fromIntegral range

-- | Change the PWM divisor.  Range is
-- <https://raspberrypi.stackexchange.com/questions/4906/control-hardware-pwm-frequency/9725#9725 2-4095>.
pwmSetClock :: PwmValue -> IO ()
pwmSetClock divisor = do
  wiringPiSetupGpio
  c_pwmSetClock $ fromIntegral divisor

piBoardRev :: IO Int
piBoardRev = do
  wiringPiSetupGpio
  fromIntegral <$> c_piBoardRev

wpiPinToGpio :: CInt -> IO CInt
wpiPinToGpio x = do
  wiringPiSetupGpio
  c_wpiPinToGpio x

physPinToGpio :: CInt -> IO CInt
physPinToGpio x = do
  wiringPiSetupGpio
  c_physPinToGpio x

-- | Converts a pin to its \"Broadcom GPIO\" number.  This relies on
-- 'unsafePerformIO' internally, because the pin mapping depends on
-- the board revision.  Returns 'Nothing' if the pin number is
-- invalid; e. g. it is out of range or is a power or ground pin
-- on the physical connector.
pinToBcmGpio :: Pin -> Maybe Int
pinToBcmGpio (Wpi n) = cvtPin n (unsafePerformIO . wpiPinToGpio)
pinToBcmGpio (Gpio n) = cvtPin n id
pinToBcmGpio (Phys n) = cvtPin n (unsafePerformIO . physPinToGpio)

pin2bcm :: Pin -> String -> CInt
pin2bcm p name =
  case pinToBcmGpio p of
    (Just x) -> fromIntegral x
    Nothing -> error $ "Illegal pin " ++ (show p) ++ " passed to " ++ name

cvtPin :: Int -> (CInt -> CInt) -> Maybe Int
cvtPin n f
  | n >= 0 && n < 64 = chkRange $ fromIntegral $ f $ fromIntegral n
  | otherwise = Nothing

chkRange :: Int -> Maybe Int
chkRange n
  | n >= 0 && n < 64 = Just n
  | otherwise = Nothing
