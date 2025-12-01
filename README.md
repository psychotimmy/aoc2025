# Advent of Code 2025

Fortran 77 on RaspberryPiOS (Debian trixie)

Use (with gfortran installed) f77 filename.f -o filename to compile and link.

## Notes

Day 1 - Part 1 is straightforward - we can ignore what the safe's dial should actually read and use MOD - it all comes out in the wash. Part 2 was less straightforward as we do need to care what the dial reads! I also put a check in for moving the dial from 0 to 0 (it doesn't count as a click through or to zero) - but my puzzle data didn't have this edge case in it. I've left my debugging WRITE(\*,\*) statements in the code as it illustrates my logic nicely. I suspect there's some further refinement possible, but it works (and quickly).
