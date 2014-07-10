HaMinitel
=========

HaMinitel is a library that allows you to drive a french Minitel through a
serial port in Haskell.

It was started as a mean to experience Haskell and functional programming.

Requirements
------------

It’s written using GHC 7.4 and SerialPort 0.4.

Since an RS-232 port is not directly compatible with the Minitel serial port,
I generally use an FTDI USB serial device converter which is generally mapped
to /dev/ttyUSB0 in Linux.

It works under Linux (tested on a traditional PC and Raspberry Pi).

This library needs the -threaded GHC flag.

Example
-------

Here is a small example which displays an "Hello world!" message onto the
Minitel screen:

    import Minitel.Minitel
    import Minitel.Generateur
    import Minitel.Queue
    import Control.Concurrent
    import Control.Monad
    import System.Hardware.Serialport

    main = do
        m <- minitel "/dev/ttyUSB0" minitelConfigurationStandard

        m <<< seqEfface EffTout
        m <<< seqString VideoTex "Hello world!"

        flush (serial m)

