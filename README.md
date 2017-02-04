[![Hackage](https://img.shields.io/hackage/v/wiringPi.svg)](https://hackage.haskell.org/package/wiringPi)

This is a Haskell binding to the [wiringPi library][1], which allows
you to interface with the [GPIO][4] pins on the [Raspberry Pi][2].
Unlike some other solutions for using the Raspberry Pi's GPIO pins,
wiringPi provides access to more advanced features, such as enabling
the internal pull-up or pull-down resistors.

To use this library, you must either run as root, or set the
`WIRINGPI_GPIOMEM` environment variable.  (Set it to anything; the
value does not matter.)  However, PWM will not work if
`WIRINGPI_GPIOMEM` is set.

This library will only build on the Raspberry Pi.  Before building
this library, you must install the "wiringPi" C library on your
Raspberry Pi, like this:

    sudo apt-get install wiringpi

Tested on a Raspberry Pi Model B, with [Raspbian Jessie Lite][3],
using the system-provided Haskell compiler.  (GHC 7.6.3.)

This Haskell binding is licensed under the 3-clause BSD license, and
the examples in the `examples` directory are in the public domain.
However, be aware that the wiringPi C library itself is licensed under
the LGPLv3+.

[1]: http://wiringpi.com/
[2]: https://www.raspberrypi.org/
[3]: https://www.raspberrypi.org/downloads/raspbian/
[4]: https://en.wikipedia.org/wiki/General-purpose_input/output
