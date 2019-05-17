# Stellarator

An Atari 2600 emulator. It's meant to be for "research" but I haven't done any yet.

(Status: See Goals below. This project is as complete as I want to make it for now.)

There is another VCS emulator out there with a similar name. If this gets to be a problem I'll rename this. I just happen to love [stellarators](https://en.wikipedia.org/wiki/Stellarator) having once worked on a fusion related project.

Some combinations of ghc and SDL may produce slightly wrong colours. I saw this problem once but it seems to have gone away again.

It'll run Adventure if you have the rom.

News
----
* Added second joystick emulation.
* I'm sure millipede didn't used to work but now it does!
* Added motion blur. Makes Asteroids and Basic look nice.
* Added keypad support and now Basic Programming works.
* Implemented on-cartridge "Super Chip" RAM. That gives an extra 128 bytes of RAM to some Atari games such as Secret Quest.

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

           * Select 3f for 8K ROMS such as Tigervision's Miner 2049er.
           * append sc for "super chip" aka SARA

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


It single steps until the VCS is on row 160 of the screen.
It then single steps through the entire row showing each instruction.
Useful when you're trying to decode how an individual scanline is
being rendered.
The debug command history is kept in the file .stellarator

Command syntax:
```
    {<statement>;<statement>...} - block
    Put multiple commands in a block, eg. r100{s;l} will step and list the current instruction 100 times.

    c - continue
    Return to playing game

    g - dump graphics state
    Eg. r10{s;g} will step through 10 instructions dumping graphics state each step.

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
| Gyruss                 | Use -b e8. Seems to play fine.                                           |
| Haunted House          | Seems to play fine. Needs a special hack to make SBC #$0f work.          |
| Ikari Warriors         | Seems to play fine                                                       |
| Indy 500               | Steering doesn't seem to work. Maybe expects different controller.       |
| Midnight Magic         | Seems to work fine.                                                      |
| Millipede              | Seems to work.                                                           |
| Miner 2049er           | Use -b 3f. Seems to work fine.                                           |
| Montezuma's Revenge    | Use -b e0. Seems to play fine.                                           |
| Moonsweeper            | Seems to play fine.                                                      |
| Pacman                 | Seems to work fine. Flickering is normal.                                |
| Phoenix                | Seems to work fine.                                                      |
| Pinball                | Seems to work fine.                                                      |
| Planet Patrol          | Seems to play fine apart from a bad pixel at top of screen               |
| Pole Position          | Seems to play fine modulo some misrendered pixels.                       |
| Q*Bert                 | Seems to play fine                                                       |
| Quest for Quintana Roo | Seems to play fine but hard to tell.                                     |
| Raiders of Lost Ark    | Starts fine. Might need controllers I don't emulate yet.                 |
| River Patrol           | Use -b 3f. Seems to play fine.                                           |
| River Raid             | Seems to play fine.                                                      |
| Sea Hawk               | Seems to play fine.                                                      |
| Secret Quest           | Use -b f6sc. Seems to play fine.                                         |
| Space Canyon           | Has an illegal opcode. It's clearly there in disassembly :-(             |
| Space Invaders         | Seems to play fine. Score pixels seem slightly messed up.                |
| Star Fox               | Plays fine (if you can call it "play"). Unsteady opening screen.         |
| Star Master            | ?? Lack of aliens to shoot is in Stella too. Half crosshair missing.     |
| Star Trek: SOS         | Seems to play fine.                                                      |
| Stargate               | Screen mostly blank? In debug mode you can see some stuff.               |
| Stargunner             | Seems to play fine.                                                      |
| Strategy X             | Seems to play fine apart from minor sprite misalignment.                 |
| Super Cobra            | Use -b e0 Landscape has some stray blocks in sky.                        |
| SW: Empire Strikes Back| Seems to play fine.                                                      |
| SW: Death Star Battle  | Use -b e0. Seems to play fine.                                           |
| SW: The Arcade Game    | Use -b e0. Seems to play fine. Too hard using keyboard!                  |
| Tank City              | Seems to play fine apart from some misrendered score digits              |
| Tank Brigade           | Seems to play fine.                                                      |
| Towering Inferno       | Seems to play fine.                                                      |
| Tutankh                | Use -b e0. At first I thought it wasn't working but it's just bad.       |
| Vanguard               | Seems to play fine                                                       |
| Video Checkers         | Seems to play fine. Use pairs of arrow keys to make diagonal moves.      |
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

Some notes on writing an emulator in Haskell
---------------------------------------------

The obvious choice of language for an emulator is C. It's easy to write fast code for manipulating bits and bytes.
So writing in Haskell was an unusual choice and an interesting challenge. Here are some of the observations I made.

* It took me a while to figure out how to write fast code. There's lots of mutable state and I think the garbage collector gets notified about these changes. So to hide everything from the garbage collector I put much of the state into arrays of bytes and words. You can see the code in `Asm.hs`. I called it that because it meant that a lot of my Haskell code looks like assembly language. Type safe assembly language at least.
* I've never written so much code *without* getting random inexplicable bugs. Most bugs that came up were because I hadn't written code to cover all cases. Failures were largely because of me not understanding the weird Atari hardware, not because I'd failed to translate my understanding into Haskell. When bugs did arise it was often possible to fix them through simply thinking rather than my usual bug hunting methods. Very few segmentation faults. The moment I tried writing audio code I started getting crashes so I'm postponing that.
* It was frustrating not having the option to simply throw in global variables like I would in C. Any changes in state need to be threaded through all of the code. The `AtariMonad` hides much of this but it's still a bit painful. The pain does pay off in terms of having better behaved code though.
* I loved that I was able to refactor code and have it run successfully first time (or almost first time). Strict types really do keep you safe. I already know Haskell is good for this, but it was surprising to see reality match theory.
* Haskell can be prettty verbose. C code that looks like `(a<<8)&0xf0)|(a>>8)&0xf0` needs a lot of typing in Haskell. And some of the state updates were a bit verbose, even with some helper functions.
* There is interest in linear types in Haskell. I think it would be a perfect fit for writing emulators.I think this might allow me to get rid of `Asm.hs` without sacrficing performance. But the back end of the compiler really has to know how to exploit it and the proposal isn't currently focussed on performance. https://ghc.haskell.org/trac/ghc/wiki/LinearTypes
* Haskell compile times are painful. I bet a C version of this code would compile from scratch in 3-4 seconds. It's taking minutes to compile the Haskell. This means the idea->compile->test loop is much longer than I'd like. Thankfully type checking means mistakes are often caught early in compilation.
