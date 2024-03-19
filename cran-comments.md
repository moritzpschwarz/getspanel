## Resubmission

This is a resubmission as the first submission did not pass the CRAN tests. In fact, all CRAN tests were passed, but the log file indicated a NOTE saying "Overall checktime 12 min > 10 min", which I assume is why the pre-test did not work. 

I have now selected more tests to be skipped on CRAN to ensure that the check takes less than 10 minutes. 

### General changes in version 0.2.0: 

This is an update to extend the package with a new method to enable further analysis tools. 

- Added a new method to `isatpanel()`
- This necessitated a few changes throughout the package, including all plotting functions
- I also enabled more specific control of subsetting my methods by id and time in `isatpanel()`. For this, I added quite extensive testing. 
- I also implemented codecov to gain a better understanding of code testing coverage in the future. 

## R CMD Checks

There were no ERRORs or WARNINGs.
