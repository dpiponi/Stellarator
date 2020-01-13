# Stellarator

An Atari 2600 emulator. It's meant to be for "research" but I haven't done any yet.

(Status: See Goals below. This project is as complete as I want to make it for now.)

There is another VCS emulator out there with a similar name. If this gets to be a problem I'll rename this. I just happen to love [stellarators](https://en.wikipedia.org/wiki/Stellarator) having once worked on a fusion related project.


This project has a sister project [Alcator](https://github.com/dpiponi/Alcator).

News
----
* I'm going to port from SDL to GLFW and then maybe I can make audio work.
* Runs at correct frame rate now.
* Added second joystick emulation.
* I'm sure millipede didn't used to work but now it does!
* Added motion blur. Makes Asteroids and Basic look nice.
* Added keypad support and now Basic Programming works.
* Implemented on-cartridge "Super Chip" RAM. That gives an extra 128 bytes of RAM to some Atari games such as Secret Quest.
* Implemented CBS 12K cartridge with 256 bytes of RAM.

Screenshots
-----------
![Adventure screenshot](docs/adventure.gif?raw=true "Adventure Screenshot")

![Pitfall! screenshot](docs/pitfall.gif?raw=true "Pitfall! Screenshot")

![Basic Programming screenshot](docs/basic.gif?raw=true "Basic Programming Screenshot")

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

Similar instructions have worked under Linux but I don't actively maintain that.
I've never tested under Windows. Looking for a volunteer!

Instructions
------------
When using default options file:
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
            Allows setting of keys, window size and controller type.
            Fragile file format so copy original as closely as
            possible.

    -b unbanked|f8|f6|3f|f8sc|f6sc
           select rom bank switching style
           Currently Stellarator automatically picks:

           * unbanked for 2K or 4K roms
           * f8 for 8K roms
           * f6 for 16K roms
           * f4 for 32K roms
           * e0 for Parker 8K roms
           * fa for CBS 12K roms

           * Select 3f for 8K ROMS such as Tigervision's Miner 2049er.
           * append sc for "super chip" aka SARA

Notes
-----
* The emulation isn't perfect but it's getting there.
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
Hitting left-alt while running the emulator drops you into the debugger.
Use the `c` command to quit debugging and return to emulation.


The debug command history is kept in the file .alcator

Command syntax:
```
    {<statement>;<statement>...} - block
    Put multiple commands in a block, eg. r100{s;l} will step and list the current instruction 100 times.

    c - continue
    Return to emulation.

    s - single instruction step
    Eg. r100000s will step 100000 instructions

    r<expr><statement> - repeat statement
    Eg. r(2*y){s;l} will step and list instructions a number of times given by double the Y register.
    (You can leave out the 'r' if the expression can be unambiguously read as an expression.)

    x<string> - execute command
    Executes command in string. Eg. x"p1" will print 1.

    l<expr>
    l<expr>expr> - list disassembly
    Disassemble from given expression with optional number of instructions.
    Eg. l(pc+2)10 lists 10 instructions starting at PC+2.

    p<expr> - print
    Print expression. Eg. p?0x9c7f prints byte at address 0x9c7f

    u<expr><statement> - until
    Perform statement until condition met.
    Eg. u(row==160){s};u(row>160){s;l} will step until row 160 of screen is reached and will then step, disassembling each instruction, until row 160 is finished.
    Eg. u(y>x){l;s} will step until Y register is larger than X.

    <var>=<expr>
    Set value of variable.
    Eg. U=1;V=2;pU+V will print 3.
```

Expression syntax
```
    t - current clock value
    col - current column
    row - current row

    Note the names of flags come from names of branch instructions:
    eq - Z flag
    ne - negated Z flag
    mi - N flag
    pl - negated N flag
    cs - C flag
    cc - negated c flag

    a, x, y, s - registers

    ==, !=, >, <, >=, <=, +, -, <<, >>, *, / ~, &, | all do obvious thing

    ?<expr> - read byte from address
    !<expr> - read 2 byte word from address
```

Complex examples
```
    u(pc==0x5828){s;l} - keep executing until address 5828
    u(?pc==0x20){s;l}  - keep executing until first JSR
    p(!(0x101+s)+1)    - print return address on top of stack
```

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
| Armor Ambush           | Seems to play fine.                                                      |
| Asteroids              | Seems to play fine. Flicker is correct behaviour.                        |
| Basic Programming      | Works fine. Select Keypads. Flicker is correct behaviour.                |
| Battle Zone            | Seems to play fine.                                                      |
| Centipede              | seems to play fine.                                                      |
| Chopper Command        | Seems to play fine.                                                      |
| Circus                 | Seems to play fine. Too fast for me...                                   |
| Combat                 | Seems to play fine. Stellarator has no 2 player support though.          |
| Commando Raid          | Seems to play fine. Ugly "comb" effect is correct.                       |
| Cosmic Ark             | Seems to play fine but star rendering is replaced by ugly vertical line. |
| Cosmic Swarm           | Flays fine. Better with motion blur.                                     |
| Defender:              | Seems to play fine.                                                      |
| Dig Dug                | Use -b f6sc. Seems to play fine apart from some stray pixels.            |
| Demon Attack           | Seems to play fine. I quite like this one.                               |
| Donkey Kong            | Seems to play fine. Amazing implementation.                              |
| Dukes of Hazzard       | Seems to play fine.                                                      |
| Fatal Run              | Use -b f4sc. But RTS at 5F6B after bank switch jumps to bad opcode???    |
| Frogger 2              | Use -b e0. Seems to play fine.                                           |
| Freeway                | Seems to play fine.                                                      |
| Fun with Numbers       | Works. Pity the kids that learnt mathematics using this.                 |
| Galaxians              | Seems to play fine despite extra column of galaxians.                    |
| Grand Prix             | Seems to play fine.                                                      |
| Glacier Patrol         | Seems to play fine, if you can stay awake.                               |
| Gyruss                 | Use -b e8. Seems to play fine.                                           |
| Haunted House          | Seems to play fine. Needs a special hack to make SBC #$0f work.          |
| Ikari Warriors         | Seems to play fine                                                       |
| Indy 500               | Steering doesn't seem to work. Maybe expects different controller.       |
| James Bond 007         | Seems to work fine.                                                      |
| Jr. Pacman             | Seems to work fine. Superchip. Use -b f6sc                               |
| Laser Blast            | Seems to work fine.                                                      |
| Mega Force             | Seems to work fine.                                                      |
| Midnight Magic         | Seems to work fine.                                                      |
| Millipede              | Seems to work.                                                           |
| Miner 2049er           | Use -b 3f. Seems to work fine.                                           |
| Montezuma's Revenge    | Use -b e0. Seems to play fine.                                           |
| Mountain King          | Use -b fa. Seems to play fine. You can get to glitch heaven!             |
| Motocross              | Seems to play fine.                                                      |
| Ms. Pacman             | Seems to play fine.                                                      |
| Nightmare              | Seems to play fine.                                                      |
| Obelix                 | Seems to play fine.                                                      |
| Pacman                 | Seems to work fine. Flickering is normal.                                |
| Phoenix                | Seems to work fine.                                                      |
| Pinball                | Seems to work fine.                                                      |
| Planet Patrol          | Seems to play fine apart from a bad pixel at top of screen               |
| Pole Position          | Seems to play fine modulo some misrendered pixels.                       |
| Q*Bert                 | Seems to play fine. Use qbert-options and keys I,O,K,L.                  |
| Quest for Quintana Roo | Seems to play fine.                                                      |
| Raiders of Lost Ark    | Starts fine. Might need controllers I don't emulate yet.                 |
| River Patrol           | Use -b 3f. Seems to play fine.                                           |
| River Raid             | Seems to play fine.                                                      |
| Robin Hood             | Seems to play fine.                                                      |
| Sea Hawk               | Seems to play fine.                                                      |
| Secret Quest           | Use -b f6sc. Seems to play fine.                                         |
| Space Canyon           | Has an illegal opcode. It's clearly there in disassembly :-(             |
| Space Invaders         | Seems to play fine. Score pixels seem slightly messed up.                |
| Star Fox               | Plays fine (if you can call it "play"). Unsteady opening screen.         |
| Star Master            | ?? Lack of aliens to shoot is in Stella too. Half crosshair missing.     |
| Star Trek: SOS         | Seems to play fine.                                                      |
| Stargate               | You can see some stuff in debug mode. Maybe sudden death each start.     |
| Stargunner             | Seems to play fine.                                                      |
| Strategy X             | Seems to play fine apart from minor sprite misalignment.                 |
| Surround               | Seems to play fine.                                                      |
| Super Cobra            | Use -b e0 Landscape has some stray blocks in sky.                        |
| SW: Empire Strikes Back| Seems to play fine.                                                      |
| SW: Death Star Battle  | Use -b e0. Seems to play fine.                                           |
| SW: The Arcade Game    | Use -b e0. Seems to play fine. Too hard using keyboard!                  |
| Tank City              | Seems to play fine apart from some misrendered score digits              |
| Tank Brigade           | Seems to play fine.                                                      |
| Tanks But No Tanks     | Seems to play fine.                                                      |
| Towering Inferno       | Seems to play fine.                                                      |
| Tutankh                | Use -b e0. At first I thought it wasn't working but it's just bad.       |
| Vanguard               | Seems to play fine                                                       |
| Video Checkers         | Seems to play fine. Use pairs of arrow keys to make diagonal moves.      |
| Wall Defender          | Seems to play fine.                                                      |
| Xenohpobe              | Seems to play fine                                                       |
| Xevious                | Seems to play fine.                                                      |
| Yar's Revenge          | Seems to play fine.                                                      |
| Zaxxon                 | Seems to play fine.                                                      |

Rendering RAM traces
--------------------
In `stellarator.cabal` set `-DTRACE` to 1 instead of 0. Now rebuild.
The `W` key will now write a trace of all RAM written to.
Usually hit trace after about 30 seconds or so.
Then run a command like
```
    c++ -std=c++11 -O3 -o trace trace.cpp && ./trace && open trace.bmp
```
and it'll render a trace that looks a bit like:
![RAM trace](docs/trace.png?raw=true "RAM trace")
The idea is that the image is 1024 pixels wide, one pixel for each bit of RAM.
Vertically is time measured in the number of writes.
By default each row corresponds to the average value of each bit over 512 writes.
The colour indicates intensity of activity.
Blue indicates fewer writes to those bits, yellow indicates more.
