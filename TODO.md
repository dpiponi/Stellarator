TODO
----

9. More ROM bank switching styles
13. Load 2K roms
14. Guess ROM type
15. E0 bank switching segments
16. Compare Stella clock to real world clock
17. Build parser a bit more consistently
18. Temp audio
19. What's the black column in freeway?
20. Now performance is OK, unbatch stuff in stellaTickUntil
21. Maybe tick n should become tick with no args (implicitly 1)
22. Stray pixels in Adventure
23. What's with crazy flashing in Asteroids?

Done
----

1. Implement "old" and "new" in ENABL, GRP0, GRP1, VDEL
2. Delay for PFn ?
3. ROM bank switching
5. Why can't I fire in Combat?
4. Where are missiles in Yar?
10. Mysterious "BYTE" in Millipede. 4a is LSR A
12. Rework TV display to show all needed scan lines.
11. Separate memory read and side-effecting (bank switching) memory read.
17. Why do combat and xevious fail? Is it a timer issue?
    They work fine.
    Used wrong bank switching for xevious.
    Forgot to double rom size to 4K for combat.
8. Why does Donkey Kong fail with illegal instruction?
    Seems to work today.
6. Faster
    IOUArrays did the trick.
7. Deal with problem that WSYNC means stella clock /= 3*6502 clock
