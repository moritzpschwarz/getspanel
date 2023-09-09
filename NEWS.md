# getspanel 0.1.5

-   Implemented the argument `uis` in `isatpanel()` to enable user-specified indicator saturation
-   Improved Plotting when there is just a single identified break (now the colour scale adjusts)
-   Set the default argument for plotting in `isatpanel` to `TRUE` to enable automatic plotting
-   Changed the default plot in `plot.isatpanel()` to now be a combination of time series and `plot_grid()` using the `cowplot` package

## Bug fixes

-   Bugfix in robust_isatpanel()
-   Bugfixes in plotting to work with uis specifications

# getspanel 0.1.4

-   Bugfixes and additional test files
-   new option in plot_grid() to allow users to exclude plotting certain type of indicators

## Bug fixes

-   Bug fix in isatpanel() when using AR models and not sorting the id data alphabetically
-   Bug fix in break_uncertainty()
-   Bug fix in plot_grid() (changing na.rm = NA to na.rm = TRUE in geom_tile())

# getspanel 0.1.3

-   Bugfixes
-   Changed the name of the function robust.isatpanel to robust_isatpanel to ensure consistency
-   More error checks for csis and cfesis (ensuring that no out of sample groups can be chosen).
-   Small changes to the introduction and the README

## Bug fixes

-   Corrected provided sample data (no. of rows and removed duplicates)
-   Bug fix in internal identify_indicator_timings() function to deal with steps correctly
-   Bug fix in plot.isatpanel to ensure JSIS is plotted correctly
-   Disabled the 'sis' option in isatpanel as it does not work in a panel context but might have been passed to 'isat'

# getspanel 0.1.2

-   Changes to how the option mc.warning is dealt with

## Bug fixes

-   Changed order of y-axis in plot_grid

# getspanel 0.1.1

-   Changes to the package as a response to CRAN tests

## Bug fixes

-   Bug fixes in isatpanel() to handle missing values

# getspanel 0.1.0

-   First CRAN release of the package.
-   Consult the Introduction Vignette for an overview.
