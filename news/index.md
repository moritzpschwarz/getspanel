# Changelog

## getspanel 0.2.2

CRAN release: 2026-10-04

### Minor changes

- [`print.isatpanel()`](http://moritzschwarz.org/getspanel/reference/print.isatpanel.md)
  to make it more informative (especially when using an engine and
  different vcov settings).
- Small change to
  [`plot_grid()`](http://moritzschwarz.org/getspanel/reference/plot_grid.md)
  to retain dividers between observations.
- minor update to testing to fix testing issues on CRAN

## getspanel 0.2.1

CRAN release: 2025-05-13

### Minor changes

- fixed a bug in plotting when in
  [`plot_grid()`](http://moritzschwarz.org/getspanel/reference/plot_grid.md)
  when Dates are being used
- added functionality to exclude indicators to
  [`plot_counterfactual()`](http://moritzschwarz.org/getspanel/reference/plot_counterfactual.md)
- slightly improved standard plots
- added `retained.indicators` as output to `isatpanel`

## getspanel 0.2.0

CRAN release: 2024-03-19

### Major changes

- Changed default `effect` from `individual` to `twoways`
- introduced Trend Indicator Saturation `tis` in `isatpanel`
- Enabled the ability to specify all indicator methods for only a
  specific subset of time and ids rather than for all ids or time (this
  was possible in a simpler form for some indicator methods for id
  already)

### Minor changes

- Introduced new internal functions and tests to ensure that the
  subsetting of time and ids works
- Introduced new tests for `tis`
- Adapted a number of functions to work with `tis`
  (`identify_indicator_timings`, the plotting functions,
- Implemented codecov to check code coverage long-term

## getspanel 0.1.5

CRAN release: 2023-10-19

### Major changes

- Implemented the argument `uis` in
  [`isatpanel()`](http://moritzschwarz.org/getspanel/reference/isatpanel.md)
  to enable user-specified indicator saturation
- Added a new argument `t.pval` to mirror the setting of a target
  p-value to the gets::isat() function.
- Added new function
  [`get_indicators()`](http://moritzschwarz.org/getspanel/reference/get_indicators.md)

### Minor changes

- Set the default argument for plotting in `isatpanel` to `TRUE` to
  enable automatic plotting
- Changed the default plot in
  [`plot.isatpanel()`](http://moritzschwarz.org/getspanel/reference/plot.isatpanel.md)
  to now be a combination of time series and
  [`plot_grid()`](http://moritzschwarz.org/getspanel/reference/plot_grid.md)
  using the `cowplot` package
- Automated testing for
  [`robust_isatpanel()`](http://moritzschwarz.org/getspanel/reference/robust_isatpanel.md)
  using the Koch et al. 2022 results

### Bug fixes

- Bugfix in robust_isatpanel(). Before it appeared like HAC S.E. with
  clusters were possible. This has been fixed. Now also the estimation
  type is taken from the initial model rather than specified as a
  separate argument; hence ‘effect’ has been removed as an argument.  
- Bugfixes in plotting to work with uis specifications
- Improved plotting when there is just a single identified break (now
  the colour scale adjusts)

## getspanel 0.1.4

CRAN release: 2023-05-05

- Bugfixes and additional test files
- new option in plot_grid() to allow users to exclude plotting certain
  type of indicators

### Bug fixes

- Bug fix in isatpanel() when using AR models and not sorting the id
  data alphabetically
- Bug fix in break_uncertainty()
- Bug fix in plot_grid() (changing na.rm = NA to na.rm = TRUE in
  geom_tile())

## getspanel 0.1.3

CRAN release: 2023-01-28

- Bugfixes
- Changed the name of the function robust.isatpanel to robust_isatpanel
  to ensure consistency
- More error checks for csis and cfesis (ensuring that no out of sample
  groups can be chosen).
- Small changes to the introduction and the README

### Bug fixes

- Corrected provided sample data (no. of rows and removed duplicates)
- Bug fix in internal identify_indicator_timings() function to deal with
  steps correctly
- Bug fix in plot.isatpanel to ensure JSIS is plotted correctly
- Disabled the ‘sis’ option in isatpanel as it does not work in a panel
  context but might have been passed to ‘isat’

## getspanel 0.1.2

CRAN release: 2023-01-17

- Changes to how the option mc.warning is dealt with

### Bug fixes

- Changed order of y-axis in plot_grid

## getspanel 0.1.1

- Changes to the package as a response to CRAN tests

### Bug fixes

- Bug fixes in isatpanel() to handle missing values

## getspanel 0.1.0

- First CRAN release of the package.
- Consult the Introduction Vignette for an overview.
