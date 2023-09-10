## Submission
This is a minor update to fix a small bug and enable some further analysis tools. 

- I fixed a small error in robust_isatpanel() that produced incorrect results when "time" was not a factor
-   Implemented the argument `uis` in `isatpanel()` to enable user-specified indicator saturation
- Added new function `get_indicators()`
- Some smaller changes to plotting functions

## R CMD Checks

There were no ERRORs or WARNINGs.
