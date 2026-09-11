# Regression test cases

## CUTLINE

- CUT-001: stock 6000, kerf 0, parts 3000+3000 -> 1 bar, waste 0.
- CUT-002: stock 6000, kerf 5, parts 3000+3000 -> 2 bars, no negative waste.
- CUT-003: stock 6000, kerf 5, part 6000 -> 1 bar, waste 0. Kerf is between adjacent pieces.
- CUT-004: stock 6000, part 6000.1 -> skipped, no negative waste.

Every repaired bug must be added here and represented by an executable test.
