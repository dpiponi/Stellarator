# Stellarator

An Atari 2600 emulator.
Work in progress...

It'll run Adventure if you have the rom.

Installation
------------
* It's written in Haskell.
* If you don't have stack, install it using the instructions here: https://docs.haskellstack.org/en/stable/
* You'll need to install the SDL libraries somewhere stack can find them.
* I work on a Mac so I used MacPorts which you can install from here: https://www.macports.org

* Once MacPorts is installed, use

    port install libsdl2

* Now you can build Stellarator with:

    stack build
    
* Run it with a commmand like

    stack exec Stellarator-exe -- -f ADVNTURE.BIN

You'll need to obtain ADVNTURE.BIN from somewhere like https://www.atariage.com/system_items.html?SystemID=2600&ItemTypeID=ROM

Screenshots
-----------
![Adventure screenshot](docs/adventure.gif?raw=true "Adventure Screenshot")

![Pitfall screenshot](docs/pitfall.gif?raw=true "Pitfall Screenshot")

Instructions
------------
Use cursor keys to simulate the joystick and the space bar to fire the trigger.
Only player 1 for now.

Also:

    C - Game Select
    V - Reset

Don't press any other keys unless you want to quit.

Use mouse click to get info about graphics registers for the selected pixel.

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

Notes
-----
* The emulation isn't perfect but it's getting there.
* The frame rate is locked to whatever your display refresh rate is.
* I've made no attempt to lock to NTSC's 60 Hz.
* There is no audio yet..

Note that although 99% of the video functionality is in place, many games
make use of precise timing of the video circuitry including delays as
signals propagate through it. I only have only implemented delays on
a small part of the hardware and that likely explains most of the quirks
you see.

Some games don't function at all, even with the correct rom bank switching.
For example the baddies don't seem to appear in Millipede.

Debugger
--------
Hitting escape while running the emulator drops you into the debugger.
Use the `c` command to quit debugging and return to game playing.

I'll document the commands eventually but here's an example:

    u(row==160){s};u(row>160){s;l}

It single steps until the VCS is on row 160 of the screen.
It then single steps through the entire row showing each instruction.
Useful when you're trying to decode how an individual scanline is
being rendered.
The debug command history is kept in the file .stellarator

UI events to the main window are ignored while single stepping though
I'll probably fix that eventually.

Notes
-----

Credits
-------
1. The primary source of information was http://web.atari.org/stellaes.pdf
2. This was a great secondary source with subtle details: http://www.atarihq.com/danb/files/TIA_HW_Notes.txt
3. I've used Stella as my reference "hardware". I've had tiny peeks at Stella's source but I don't want
   to do that too much as it removes some of the fun :-) (http://stella.sourceforge.net)

Playable Games
--------------

Lots of games work:

| Game                   |Result of testing                                                         |
|------------------------|--------------------------------------------------------------------------|
| Adventure:             | Seems to play fine.                                                      |
| Aquaventure:           | Use -b f8. seems to play fine.                                           |
| Air Sea Battle:        | Seems to play fine.                                                      |
| Asteroids:             | Seems to play fine. Flicker is correct behaviour.                        |
| Centipede:             | Use -b f8. seems to play fine.                                           |
| Circus:                | Seems to play fine. Too fast for me...                                   |
| Combat:                | Seems to play fine. Two player though...                                 |
| Commando Raid:         | Seems to play fine. Ugly "comb" effect is correct.                       |
| Cosmic Ark:            | Seems to play fine but star rendering is replaced by ugly vertical line. |
| Defender:              | Seems to play fine.                                                      |
| Donkey Kong:           | Seems to play fine.                                                      |
| Dukes if Hazzard:      | Use -b f6. Seems to play fine.                                           |
| Freeway:               | Seems to play fine.                                                      |
| Frogger 2:             | Uses E8 bank switching. Not implemented.                                 |
| Fun with Numbers:      | Works. Pity the kids that learnt mathematics using this.                 |
| Ikari Warriors:        | 16K ROM not supported yet.                                               |
| Indy 500               | Steering doesn't seem to work. Maybe expects different controller.       |
| Midnight Magic         | Use -b f6. Seems to work fine.                                           |
| Millipede              | Use -b f6. No enemies seem to arrive.                                    |
| Pacman                 | Seems to work fine. Flickering is normal.                                |
| Pinball                | Seems to work fine.                                                      |
| Planet Patrol          | Seems to play fine apart from a bad pixel at top of screen               |
| Pole Position          | Use -b f8. Playable but some clear alignment problems between sprites.   |
| Q*Bert                 | Seems to play fine                                                       |
| Quest for Quintana Roo | Seems to play fine but hard to tell.                                     |
| Raiders of Lost Ark    | Use -b f6. Starts fine. Might need controllers I don't emulate yet.      |
| River Patrol           | Fails with illegal instruction. Maybe needs new bank switching method.   |
| River Raid             | Seems to play fine.                                                      |
| Sea Hawk               | Seems to play fine.                                                      |
| Star Trek: SOS         | Use -b f8. Seems to play fine.                                           |
| Star Master            | ?? Lack of aliens to shoot is in Stella too. Half crosshair missing.     |
| SW: Empire Strikes Back| Seems to play fine.                                                      |
| Vanguard               | Seems to play fine                                                       |
| Xevious                | Use -f b8. Seems to play fine.                                           |
| Yar's Revenge          | Seems to play fine.                                                      |
| Zaxxon                 | Use -f b8. Seems to play fine.                                           |

