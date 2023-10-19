## Resubmission

### This is a resubmission: 
The pre-tests failed with the error (only on DEBIAN and only on CRAN, not on win-builder or rhub): 
"Running R code in ‘testthat.R’ had CPU time 3 times elapsed time"
I therefore made changes to the testing files and included skip_on_cran() a few times.



### General changes in version 0.1.5: 

This is a minor update to fix a few small bugs and enable some further analysis tools. 

- Fixed a bug and added automated testing for `robust_isatpanel()`
- I fixed a further small error in robust_isatpanel() that produced incorrect results when "time" was not a factor
- Implemented the argument `uis` in `isatpanel()` to enable user-specified indicator saturation and `t.pval` to make this argument more visible
- Added new function `get_indicators()`
- Some smaller changes to plotting functions to enable the `uis` argument in `isatpanel()`

## R CMD Checks

There were no ERRORs or WARNINGs.
