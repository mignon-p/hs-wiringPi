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

valueToInt :: Value -> CInt
valueToInt LOW  = 0
valueToInt HIGH = 1

intToValue :: CInt -> Value
intToValue 0 = LOW
intToValue _ = HIGH

modeToInt :: Mode -> CInt
modeToInt INPUT      = 2
modeToInt OUTPUT     = 3
modeToInt PWM_OUTPUT = 4
modeToInt GPIO_CLOCK = 5

pudToInt :: Pud -> CInt
pudToInt PUD_OFF  = 6
pudToInt PUD_DOWN = 7
pudToInt PUD_UP   = 8

pwmModeToInt :: PwmMode -> CInt
pwmModeToInt PWM_MODE_BAL = 9
pwmModeToInt PWM_MODE_MS  = 10

c_wiringPiSetupGpio :: IO CInt
c_wiringPiSetupGpio = return 0

c_pinMode :: CInt
          -> CInt
          -> IO ()
c_pinMode _ _ = return ()

c_pullUpDnControl :: CInt
                  -> CInt
                  -> IO ()
c_pullUpDnControl _ _ = return ()

c_digitalRead :: CInt
              -> IO CInt
c_digitalRead _ = return 0

c_digitalWrite :: CInt
               -> CInt
               -> IO ()
c_digitalWrite _ _ = return ()

c_pwmWrite :: CInt
           -> CInt
           -> IO ()
c_pwmWrite _ _ = return ()

c_digitalWriteByte :: CInt
                   -> IO ()
c_digitalWriteByte _ = return ()

c_pwmSetMode :: CInt
             -> IO ()
c_pwmSetMode _ = return ()

c_pwmSetRange :: CUInt
              -> IO ()
c_pwmSetRange _ = return ()

c_pwmSetClock :: CInt
              -> IO ()
c_pwmSetClock _ = return ()

c_piBoardRev :: IO CInt
c_piBoardRev = return 2

c_wpiPinToGpio :: CInt
               -> IO CInt
c_wpiPinToGpio _ = return (-1)

c_physPinToGpio :: CInt
                -> IO CInt
c_physPinToGpio _ = return (-1)
