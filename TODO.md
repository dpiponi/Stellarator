TODO
----

14. Guess ROM type (reactivated)
15. E0 bank switching segments
16. Compare Stella clock to real world clock
17. Build debug command parser a bit more consistently
18. Temp audio
20. Now performance is OK, unbatch stuff in stellaTickUntil
21. Maybe tick n should become tick with no args (implicitly 1)
24. See if we can get NUSIZ changing to work in Meltdown as in
    http://atariage.com/forums/topic/82052-tia-schematics-and-timing/
27. See what I can do about Cosmic Ark stars
    http://www.biglist.com/lists/stella/archives/199705/msg00024.html
30. In Raiders of the Lost Ark, Indy initially descends a pixel or two to the right
    of where he does in Stella.
31. Allow starting in debugger.
32. Handle UI events in debugger.
33. Why misalignment in Pole Position?
34. What's wrong with Stargate?
35. E8 bank switching
36. Simulate paddles
37. Simulate keypads
38. Second joystick
39. Make options more robust. (Not using read :: Options)

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
22. Stray pixels in Adventure
    For now fixed by reducing grahicsDelay but I think I must be
    misunderstanding something about the timing.
    See docs/adventure_pf_timing.txt
19. What's the black column in Freeway?
    1. Need to simulate "comb" effect
    2. RESBL during blank seems to set ball position to 68+2.
26. Dot in CCE in Freeway
    Inserted graphicsDelay 1 in write to GRPn
26. Implement "undocumented" TIMINT and also implement timer correctly
    http://atariage.com/forums/topic/133686-please-explain-riot-timmers/?p=1617207
23. What's with crazy flashing in Asteroids?
    It's fine. That's how the original worked. Switching to synced OpenGL improves look.
25. Make players etc. wrap
30. Bad pixel on score in Planet Patrol
    It's there in Youtube videos so it's not a Stellarator bug.
13. Load 2K roms
33. 3F Tigervision bank switching.
9. More ROM bank switching styles
   Too open-ended a goal
29. Allow keys to be configurable. Otherwise Q*Bert too confusing.
28. Options file.
40. Configurable window size
    Via options file
