# Stellarator

An Atari 2600 emulator.
Work in progress...

It'll run Adventure if you have the rom. Try:
stack build && stack exec Stellarator-exe -- -f ADVNTURE.BIN
You need to obtain ADVNTURE.BIN from somewhere like
https://www.atariage.com/system_items.html?SystemID=2600&ItemTypeID=ROM

![Adventure screenshot](docs/adventure.gif?raw=true "Adventure Screenshot")

![Pitfall screenshot](docs/pitfall.gif?raw=true "Pitfall Screenshot")

There are visual artifacts and, more importantly,
*I've made no attempt to get timing right yet.*
There is no audio either.

Use cursor keys to simulate the joystick and the space bar to fire the trigger.
Only player 1 for now.

Also:

C - Game Select
V - Reset

Don't press any other keys unless you want to quit.

Use mouse click to get info about graphics registers for the selected pixel.

Note that although 99% of the video functionality is in place, many games
make use of precise timing of the video circuitry including delays as
signals propagate through it. I only have only implemented delays on
a small part of the hardware and that likely explains most of the quirks
you see.
It doesn't yet reproduce the artifacts I described here:
https://plus.google.com/+DanPiponi/posts/NLUwTTKpV6i

Some games don't function at all, even with the correct rom bank switching.
For example the baddies don't seem to appear in Millipede.
And there's lots of screen horribleness in Asteroids though it is playable.

During a blanking period the "electron gun" renders gray not black.
Useful for debugging.

Command line options
--------------------

    -f <filename> - Load rom from file
    -b unbanked|f8|f6 - select rom bank switching style
                        If it's a 4K cartridge use 'unbanked' (which is the default)
                        If it's an 8K cartridge try f8
                        If it's a 12K cartridge try f6
                        There are other styles that I haven't implemented yet
                        and a 16K+ cartridge, or one with RAM in it,
                        has no chance of working.
                        If you want to try a 2K cartridge (e.g. Combat) make a 4K file
                        from 2 copies of the 2K file and use that.
                        (Will automate this eventually...)

Debugger
--------
Hitting escape while running the emulator drops you into the debugger.
Use q to quit debugging.

I'll document the commands eventually but here's an example:

    {u(row==160){s};u(row>160){s;l}}

It single steps until the VCS is on row 160 of the screen.
It then single steps through the entire row showing each instruction.
Useful when you're trying to decode how an individual scanline is
being rendered.
The debug command history is kept in the file .stellarator

UI events to the main window are ignored while single stepping though
I'll probably fix that eventually.

Credits
-------
1. The primary source of information was http://web.atari.org/stellaes.pdf
2. This was a great secondary source with subtle details: http://www.atarihq.com/danb/files/TIA_HW_Notes.txt
