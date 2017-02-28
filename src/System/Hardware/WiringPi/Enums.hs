module System.Hardware.WiringPi.Enums
  ( Value(..)
  , Mode(..)
  , Pud(..)
  , PwmMode(..)
  , IntEdge(..)
  ) where

-- | Digital logic level.
data Value = LOW | HIGH deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Pin mode, used with 'pinMode'.
data Mode = INPUT      -- ^ digital input
          | OUTPUT     -- ^ digital output
          | PWM_OUTPUT -- ^ pulse-width modulation; only supported on wiringPi pins 1, 23, 24, and 26
          | GPIO_CLOCK -- ^ <https://pinout.xyz/pinout/gpclk clock output>;
                       -- only supported on wiringPi pins 7, 21, 22, and 29
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

-- | Interrupt levels, used with 'wiringPiISR'.
data IntEdge = INT_EDGE_SETUP   -- ^ no initialization of the pin will happen
             | INT_EDGE_FALLING -- ^ interrupt on a falling of the incoming signal
             | INT_EDGE_RISING  -- ^ interrupt on a rising of the incoming signal
             | INT_EDGE_BOTH    -- ^ interrupt on both of the incoming signal
             deriving (Eq, Ord, Show, Read, Enum, Bounded)
