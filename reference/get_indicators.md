# Extract the retained indicators from an `isatpanel` object

Extract the retained indicators from an `isatpanel` object

## Usage

``` r
get_indicators(object, uis_breaks = NULL)
```

## Arguments

- object:

  An object produced by the isatpanel function.

- uis_breaks:

  A string with the names of user-specified indicators.

## Value

A list of indicators.

## Examples

``` r
# \donttest{
data(EU_emissions_road)

# Group specification
EU15 <- c("Austria", "Germany", "Denmark", "Spain", "Finland", "Belgium",
         "France", "United Kingdom", "Ireland", "Italy", "Luxembourg",
         "Netherlands", "Greece", "Portugal", "Sweden")

# Prepare sample and data
EU_emissions_road_short <- EU_emissions_road[
EU_emissions_road$country %in% EU15 &
EU_emissions_road$year >= 2000,
]

# Run
result <- isatpanel(
  data = EU_emissions_road_short,
  formula = ltransport.emissions ~ lgdp + I(lgdp^2) + lpop,
  index = c("country", "year"),
  effect = "twoways",
  fesis = TRUE,
  plot = FALSE,
  t.pval = 0.01
)
#> 
#> UIS1.fesis block 1 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 2 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 3 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 4 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 5 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 6 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 7 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 8 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> UIS1.fesis block 9 of 9:
#> 30 path(s) to search
#> Searching: 
#> 1 
#> 2 
#> 3 
#> 4 
#> 5 
#> 6 
#> 7 
#> 8 
#> 9 
#> 10 
#> 11 
#> 12 
#> 13 
#> 14 
#> 15 
#> 16 
#> 17 
#> 18 
#> 19 
#> 20 
#> 21 
#> 22 
#> 23 
#> 24 
#> 25 
#> 26 
#> 27 
#> 28 
#> 29 
#> 30 
#> 
#> GETS of union of retained UIS1.fesis variables... 
#> 1 path(s) to search
#> Searching: 
#> 1 
#> 
#> GETS of union of ALL retained variables...
#> 1 path(s) to search
#> Searching: 
#> 1 
plot(result)

plot_grid(result)


# print the retained indicators
get_indicators(result)
#> $fesis
#>                                  id time                  name
#> 1.fesisAustria.2002         Austria 2002     fesisAustria.2002
#> 1.fesisAustria.2013         Austria 2013     fesisAustria.2013
#> 1.fesisGermany.2003         Germany 2003     fesisGermany.2003
#> 1.fesisGermany.2012         Germany 2012     fesisGermany.2012
#> 1.fesisGreece.2009           Greece 2009      fesisGreece.2009
#> 1.fesisIreland.2006         Ireland 2006     fesisIreland.2006
#> 1.fesisIreland.2010         Ireland 2010     fesisIreland.2010
#> 1.fesisIreland.2015         Ireland 2015     fesisIreland.2015
#> 1.fesisLuxembourg.2004   Luxembourg 2004  fesisLuxembourg.2004
#> 1.fesisLuxembourg.2015   Luxembourg 2015  fesisLuxembourg.2015
#> 1.fesisNetherlands.2014 Netherlands 2014 fesisNetherlands.2014
#> 1.fesisPortugal.2011       Portugal 2011    fesisPortugal.2011
#> 
# }
```
