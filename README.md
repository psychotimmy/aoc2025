# Advent of Code 2025

Fortran 77 on RaspberryPiOS (Debian trixie)

Use (with gfortran installed) f77 filename.f -o filename to compile and link.

## Notes

**Day 1** Part 1 is straightforward - we can ignore what the safe's dial should actually read and use MOD - it all comes out in the wash. Part 2 was less straightforward as we do need to care what the dial reads! I also put a check in for moving the dial from 0 to 0 (it doesn't count as a click through or to zero) - but my puzzle data didn't have this edge case in it. I've left my debugging WRITE(\*,\*) statements in the code as it illustrates my logic nicely. I suspect there's some further refinement possible, but it works (and quickly).

**Day 2** Reuses a variant of a string to integer routine I wrote last year (I could have delved into Fortran 90 for CSV handling, but toughed it out in f77). Happy with the maths I used to find the invalid product ids for part 1, even if it is a little brute-force at the end. I bet all those people who are using languages that can deal with ranges are much happier than I am today! I then simplified the code in part 1 (day2-1a.f) although it runs much more slowly - around 0.2s vs around 0.03s on my Pi4B as a basis for part 2. Part 2 takes a long-ish time to run - around 28s - and there will definitely optimisations that can be done, even in Fortran 77. But, I understand how it works, so that's good enough for today. Just looked at the code again and there were a couple of obvious optimisations by moving a couple of assignments outside of loops. Run time for day2-2a.f is now around 20s.  

**Day3** Straightforward stuff today. Part 1 completed before I went out shopping, part 2 after I came home. Part 2 also answers part 1, as the code for part 1 is straightforward to generalise by adding another loop in the function to calculate the maximum number of jolts that a battery bank can deliver from N batteries. The only slightly tricky thing was remembering to convert the CALCJOLTS calculation to use integer*8's correctly. Part 1 takes 0.016s on the Pi4, part 2 (generating both answers) takes 0.018s. Probably not much left to optimise in the part 2 solution.
