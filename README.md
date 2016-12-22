# Stellarator

An Atari 2600 emulator.
Work in progress...

It'll run Adventure if you have the rom. Try:
stack build && stack exec Stellarator-exe -- -f adventure.rom

![Adventure screenshot](docs/adventure.gif?raw=true "Adventure Screenshot")

![Pitfall screenshot](docs/pitfall.gif?raw=true "Pitfall Screenshot")

As you can see, there are visual artifacts and the aspect ratio isn't right yet.
I've made no attempt to get timing right yet.

Use cursor keys to simulate the joystick and the space bar to fire the trigger.

Also:

C - Game Select
V - Reset

Don't press any other keys unless you want to quit.

Use mouse click to get info about graphics registers for the selected pixel.

Debugger
--------
Hitting escape while running the emulator drops you into the debugger.
Use q to quit.

I'll document the commands eventually but here's an example:

    {u(row==160){s};u(row>160){s;l}}

It single steps until the VCS is on row 160 of the screen.
It then single steps through the entire row showing each instruction.
Useful when you're trying to decode how an individual scanline is
being rendered.
The debug command history is kept in the file .stellarator
