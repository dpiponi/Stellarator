# Stellarator

An Atari 2600 emulator. It's meant to be for "research" but I haven't done any yet.

(Status: See Goals below. This project is as complete as I want to make it for now.)

There is another VCS emulator out there with a similar name. If this gets to be a problem I'll rename this. I just happen to love [stellarators](https://en.wikipedia.org/wiki/Stellarator) having once worked on a fusion related project.

Some combinations of ghc and SDL may produce slightly wrong colours. I saw this problem once but it seems to have gone away again.

It'll run Adventure if you have the rom.

Screenshots
-----------
![Adventure screenshot](docs/adventure.gif?raw=true "Adventure Screenshot")

![Pitfall screenshot](docs/pitfall.gif?raw=true "Pitfall Screenshot")

Installation
------------
* It's written in Haskell.

* Before doing anything else, you'll need to install the SDL libraries somewhere the Haskell
  package manager Stack can find them.  I work on a Mac so I used MacPorts which you can install
  from here: https://www.macports.org

  Once MacPorts was installed I used:

```
    port install libsdl2
    port install libsdl2_image
```

  I've had success with homebrew as well, in which case I think you can use:

```
    brew install sdl2
    brew install sdl2_image
```

Getting SDL2 installed seems to be the main stumbling block.

* If you don't have Stack, install it using the instructions here: https://docs.haskellstack.org/en/stable/

* Now clone the project into a directory. In that directory use:

```
    stack build
```

* Run it with a commmand like

```
    stack exec Stellarator-exe -- -f ADVNTURE.BIN
```

You'll need to obtain ADVNTURE.BIN from somewhere like https://www.atariage.com/system_items.html?SystemID=2600&ItemTypeID=ROM

* If you started with ADVNTURE.BIN then hit `v` and start using the cursor keys. Have fun!

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

    -f <filename>
           Load rom from file

    -o <options file>
            Default is .stellarator-options in current directory.
            Allows setting of keys and window size.
            Fragile file format so copy original as closely as
            possible.

    -b unbanked|f8|f6|3f
           select rom bank switching style
           Currently Stellarator automatically picks:

           * unbanked for 2K or 4K roms
           * f8 for 8K roms
           * f6 for 16K roms

           So only use for this currently is to select
           3f for 8K ROMS such as Tigervision's
           Miner 2049er.

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

Goals
-----
I've pretty much done what I want with this project. I wanted to get Adventure to work. Turned out to be pretty easy. I'm pleased I could get other games to work but I don't plan to get every last pixel rendered correctly because the hardware goes down a deep rabbit hole that you ultimately need to understand at the transistor level.

I'll probably add audio when I can get simultaneous audio and video to not crash in Haskell.

I also learnt how to make Haskell with lots of mutable state run fast.

Notes
-----

Credits
-------
1. The primary source of information was https://alienbill.com/2600/101/docs/stella.html
2. This was a great secondary source with subtle details: http://www.atarihq.com/danb/files/TIA_HW_Notes.txt
3. I've used Stella as my reference "hardware". I've had tiny peeks at Stella's source but I don't want
   to do that too much as it removes some of the fun :-) (http://stella.sourceforge.net)
4. 6502test.hex was assembled from the amazing functional test at https://github.com/Klaus2m5/6502_65C02_functional_tests

Playable Games
--------------

Lots of games work:

| Game                   |Result of testing                                                         |
|------------------------|--------------------------------------------------------------------------|
| Adventure              | Seems to play fine.                                                      |
| Aquaventure            | seems to play fine.                                                      |
| Air Sea Battle         | Seems to play fine.                                                      |
| Asteroids              | Seems to play fine. Flicker is correct behaviour.                        |
| Battle Zone            | Seems to play fine but radar partly reflected and some mountain issues.  |
| Centipede              | seems to play fine.                                                      |
| Chopper Command        | Seems to play fine.                                                      |
| Circus                 | Seems to play fine. Too fast for me...                                   |
| Combat                 | Seems to play fine. Two player though...                                 |
| Commando Raid          | Seems to play fine. Ugly "comb" effect is correct.                       |
| Cosmic Ark             | Seems to play fine but star rendering is replaced by ugly vertical line. |
| Defender:              | Seems to play fine.                                                      |
| Demon Attack           | Seems to play fine. I quite like this one.                               |
| Donkey Kong            | Seems to play fine. Amazing implementation.                              |
| Dukes of Hazzard       | Seems to play fine.                                                      |
| Freeway                | Seems to play fine.                                                      |
| Frogger 2              | Uses E8 bank switching. Not implemented.                                 |
| Fun with Numbers       | Works. Pity the kids that learnt mathematics using this.                 |
| Galaxians              | Seems to play fine except for an extra column of aliens!                 |
| Haunted House          | Blank screen :-(                                                         |
| Ikari Warriors         | Seems to play fine                                                       |
| Indy 500               | Steering doesn't seem to work. Maybe expects different controller.       |
| Midnight Magic         | Seems to work fine.                                                      |
| Millipede              | No enemies seem to arrive.                                               |
| Miner 2049er           | Use -b 3f. Seems to work fine.                                           |
| Moonsweeper            | Seems to play fine.                                                      |
| Pacman                 | Seems to work fine. Flickering is normal.                                |
| Phoenix                | Seems to work fine.                                                      |
| Pinball                | Seems to work fine.                                                      |
| Planet Patrol          | Seems to play fine apart from a bad pixel at top of screen               |
| Pole Position          | Seems to play fine modulo a couple of misrendered pixels.                |
| Q*Bert                 | Seems to play fine                                                       |
| Quest for Quintana Roo | Seems to play fine but hard to tell.                                     |
| Raiders of Lost Ark    | Starts fine. Might need controllers I don't emulate yet.                 |
| River Patrol           | Use -b 3f. Seems to play fine.                                           |
| River Raid             | Seems to play fine.                                                      |
| Sea Hawk               | Seems to play fine.                                                      |
| Space Canyon           | Has an illegal opcode. It's clearly there in disassembly :-(             |
| Star Fox               | Plays fine (if you can call it "play"). Unsteady opening screen.         |
| Star Master            | ?? Lack of aliens to shoot is in Stella too. Half crosshair missing.     |
| Star Trek: SOS         | Seems to play fine.                                                      |
| Stargate               | Screen mostly blank?                                                     |
| Stargunner             | Seems to play fine.                                                      |
| Strategy X             | Seems to play fine apart from minor sprite misalignment.                 |
| SW: Empire Strikes Back| Seems to play fine.                                                      |
| Tank City              | Seems to play fine apart from some misrendered score digits              |
| Tank Brigade           | Seems to play fine.                                                      |
| Vanguard               | Seems to play fine                                                       |
| Xenohpobe              | Seems to play fine                                                       |
| Xevious                | Seems to play fine.                                                      |
| Yar's Revenge          | Seems to play fine.                                                      |
| Zaxxon                 | Seems to play fine.                                                      |

