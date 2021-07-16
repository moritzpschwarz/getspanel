clear
import delimited "C:\Users\morit\Documents\GitHub\getspanel\data-raw\projections\m2_data.csv"
encode iso, gen(iso_numeric)

xtset iso_numeric year

quietly describe, varlist
local vars `r(varlist)'
local omit iso_numeric iso year diffln_gdp_cap
local want : list vars - omit
display "`want'"

xtreg diffln_gdp_cap `want', fe

regress diffln_gdp_cap `want'
xtreg diffln_gdp_cap temp temp_2 prcp prcp_2 time_*, fe

regress diffln_gdp_cap temp temp_2 prcp prcp_2 i.iso_numeric i.year time_*

