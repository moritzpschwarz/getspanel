# getspanel 0.1.2.1

* Bugfixes
* Changed the name of the function robust.isatpanel to robust_isatpanel to ensure consistency
* More error checks for csis and cfesis (ensuring that no out of sample groups can be chosen).
* Small changes to the introduction and the README


## Bug fixes

* Corrected provided sample data (no. of rows and removed duplicates)
* Bug fix in internal identify_indicator_timings() function to deal with steps correctly
* Bug fix in plot.isatpanel to ensure JSIS is plotted correctly
* Disabled the 'sis' option in isatpanel as it does not work in a panel context but might have been passed to 'isat'


<!--  ## Major Changes

New package, to be filled for new releases

## Bug fixes

New package, to be filled for new releases

Referring to an issue:
(#10) -->
