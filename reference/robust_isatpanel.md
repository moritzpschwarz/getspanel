# Get robust Standard Errors for the isatpanel result

Get robust Standard Errors for the isatpanel result

## Usage

``` r
robust_isatpanel(
  object,
  robust = TRUE,
  HAC = FALSE,
  lag = NULL,
  type = "HC0",
  cluster = "group"
)
```

## Arguments

- object:

  An isatpanel object

- robust:

  Logical (TRUE or FALSE). Should the Standard Errors be robustified for
  Heterogeneity? This uses
  [plm::vcovHC](https://rdrr.io/pkg/plm/man/vcovHC.plm.html) with the
  specified type (default is "HC0").

- HAC:

  Should Heteroscedasticity and Autocorrelation Robust Standard Errors
  be used? This uses
  [plm::vcovNW](https://rdrr.io/pkg/plm/man/vcovNW.html), which uses the
  Newey-West estimator.

- lag:

  Maximum Number of Lags to be used with
  [plm::vcovNW](https://rdrr.io/pkg/plm/man/vcovNW.html) using the
  Newey-West estimator. Cannot be specified when HAC = FALSE. Default is
  `NULL`.

- type:

  Character string. Type of Robust procedure e.g. 'HC0' for White SE or
  'HC3' for Lang.

- cluster:

  Should an object with clustered S.E. be included? Choose between
  'group' or 'time' or FALSE. Uses
  [plm::vcovHC](https://rdrr.io/pkg/plm/man/vcovHC.plm.html) with the
  cluster argument.

## Value

A list with robust estimates

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
robust_isatpanel(result)
#> $plm_object
#> 
#> Model Formula: y ~ lgdp + I.lgdp.2. + lpop + fesisAustria.2002 + fesisAustria.2013 + 
#>     fesisGermany.2003 + fesisGermany.2012 + fesisGreece.2009 + 
#>     fesisIreland.2006 + fesisIreland.2010 + fesisIreland.2015 + 
#>     fesisLuxembourg.2004 + fesisLuxembourg.2015 + fesisNetherlands.2014 + 
#>     fesisPortugal.2011
#> <environment: 0x5581c9ab4910>
#> 
#> Coefficients:
#>                  lgdp             I.lgdp.2.                  lpop 
#>              2.287131             -0.029855             -0.261160 
#>     fesisAustria.2002     fesisAustria.2013     fesisGermany.2003 
#>              0.115518              0.077869             -0.132952 
#>     fesisGermany.2012      fesisGreece.2009     fesisIreland.2006 
#>              0.089422              0.084741              0.100614 
#>     fesisIreland.2010     fesisIreland.2015  fesisLuxembourg.2004 
#>             -0.111447             -0.157951              0.155470 
#>  fesisLuxembourg.2015 fesisNetherlands.2014    fesisPortugal.2011 
#>             -0.170946             -0.076880             -0.042849 
#> 
#> 
#> $robust
#> 
#> t test of coefficients:
#> 
#>                        Estimate Std. Error  t value  Pr(>|t|)    
#> lgdp                   2.287131   0.954749   2.3955  0.017373 *  
#> I.lgdp.2.             -0.029855   0.017096  -1.7463  0.082056 .  
#> lpop                  -0.261160   0.163285  -1.5994  0.111061    
#> fesisAustria.2002      0.115518   0.010315  11.1988 < 2.2e-16 ***
#> fesisAustria.2013      0.077869   0.013883   5.6090 5.653e-08 ***
#> fesisGermany.2003     -0.132952   0.011923 -11.1507 < 2.2e-16 ***
#> fesisGermany.2012      0.089422   0.016780   5.3291 2.296e-07 ***
#> fesisGreece.2009       0.084741   0.025463   3.3280  0.001014 ** 
#> fesisIreland.2006      0.100614   0.018419   5.4625 1.185e-07 ***
#> fesisIreland.2010     -0.111447   0.012579  -8.8598 < 2.2e-16 ***
#> fesisIreland.2015     -0.157951   0.050985  -3.0980  0.002184 ** 
#> fesisLuxembourg.2004   0.155470   0.019633   7.9188 9.252e-14 ***
#> fesisLuxembourg.2015  -0.170946   0.029054  -5.8837 1.359e-08 ***
#> fesisNetherlands.2014 -0.076880   0.011473  -6.7012 1.494e-10 ***
#> fesisPortugal.2011    -0.042849   0.016701  -2.5656  0.010916 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> 
#> $cluster
#> 
#> t test of coefficients:
#> 
#>                        Estimate Std. Error  t value  Pr(>|t|)    
#> lgdp                   2.287131   0.954749   2.3955  0.017373 *  
#> I.lgdp.2.             -0.029855   0.017096  -1.7463  0.082056 .  
#> lpop                  -0.261160   0.163285  -1.5994  0.111061    
#> fesisAustria.2002      0.115518   0.010315  11.1988 < 2.2e-16 ***
#> fesisAustria.2013      0.077869   0.013883   5.6090 5.653e-08 ***
#> fesisGermany.2003     -0.132952   0.011923 -11.1507 < 2.2e-16 ***
#> fesisGermany.2012      0.089422   0.016780   5.3291 2.296e-07 ***
#> fesisGreece.2009       0.084741   0.025463   3.3280  0.001014 ** 
#> fesisIreland.2006      0.100614   0.018419   5.4625 1.185e-07 ***
#> fesisIreland.2010     -0.111447   0.012579  -8.8598 < 2.2e-16 ***
#> fesisIreland.2015     -0.157951   0.050985  -3.0980  0.002184 ** 
#> fesisLuxembourg.2004   0.155470   0.019633   7.9188 9.252e-14 ***
#> fesisLuxembourg.2015  -0.170946   0.029054  -5.8837 1.359e-08 ***
#> fesisNetherlands.2014 -0.076880   0.011473  -6.7012 1.494e-10 ***
#> fesisPortugal.2011    -0.042849   0.016701  -2.5656  0.010916 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> 
# }
```
