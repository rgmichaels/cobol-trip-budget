# COBOL Trip Budget

A small learning project written in modern free-form COBOL using GnuCOBOL.

This program simulates a batch-style budget processor for a motorcycle trip,
calculating totals, averages, and maximum expenses from a predefined list.

## Why this project exists

This repo exists to demonstrate:
- Core COBOL syntax and structure
- WORKING-STORAGE data modeling
- Fixed-precision numeric arithmetic (V99)
- PERFORM-based control flow
- Clean, readable procedural COBOL

## Requirements

- GnuCOBOL
- macOS, Linux, or similar Unix environment

## Build and run

```bash
cobc -x -free src/trip_budget.cob -o trip_budget
./trip_budget
