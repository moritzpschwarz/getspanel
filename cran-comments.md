### Changes in version 0.2.2: 

This is an update to fix the test failures on CRAN. 
These were caused by changes to other packages (fixest).  
These changes did not affect any of the functionality of the package, but they did break some of the tests.

In addition, this version makes small changes, including: 

- `print.isatpanel()` to make it more informative (especially when using an engine and different vcov settings).
- Small change to `plot_grid()` to retain dividers between observations.
- Test updates and fixing breaking tests

## R CMD Checks

No errors or warnings.
