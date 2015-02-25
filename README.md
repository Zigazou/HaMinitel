HaMinitel
=========

HaMinitel is a library that allows you to drive a french Minitel through a
serial port in Haskell.

It was started as a mean to experience Haskell and functional programming.

Requirements
------------

It’s written using GHC 7.8 and SerialPort 0.4.

Since an RS-232 port is not directly compatible with the Minitel serial port,
I generally use an FTDI USB serial device converter which is generally mapped
to /dev/ttyUSB0 in Linux. It has also been tested with a real RS-232 port to
which is connected a modem (it requires you to send the correct AT sequences
to the modem in order to set it up).

It works under Linux (tested on a traditional PC and Raspberry Pi).

This library needs the -threaded GHC flag.

Example
-------

Here is a small example which displays an "Hello world!" message onto the
Minitel screen:

    import Minitel.Minitel
    import Minitel.Generate.Generator
    import Minitel.Type.Videotex

    main = do
        m <- minitel "/dev/ttyUSB0" standardSettings
        m <<< mString VideoTex "Hello, World!"
        waitForMinitel m

