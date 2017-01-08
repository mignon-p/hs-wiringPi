import System.Hardware.WiringPi

main = do
  wiringPiSetup
  rev <- piBoardRev
  putStrLn $ "piBoardRev: " ++ show rev
  wpiPins <- mapM wpiPinToGpio [0..16]
  putStrLn $ "wpiPinToGpio: " ++ show wpiPins
  physPins <- mapM physPinToGpio [1..26]
  putStrLn $ "physPinToGpio: " ++ show physPins
