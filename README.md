
<!-- README.md is generated from README.Rmd. Please edit that file -->

# getspanel

<!-- badges: start -->

<!-- badges: end -->

This package is in the **early stage of its development** and will therefore contain many bugs and errros. 

The package is a panel adaptation of the `gets` package [see here](https://cran.r-project.org/web/packages/gets/index.html).

This code is being developed by Felix Pretis and Moritz Schwarz. The associated paper will be published under "Panel Break Detection: Detecting Unknown Treatment, Stability, Heterogeneity, and Outliers" by Pretis, Schwarz and Sucarrat. 

## Installation
<!--
You can install the released version of getspanel from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("getspanel")
```
 -->
You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moritzpschwarz/getspanel")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(getspanel)
library(tidyverse) # needed for the plots

data("pandata_simulated")

is1 <- isatpanel(data = pandata_simulated,
                   formula = gdp ~ temp, 
                   index = c("country","year"),
                   
                   effect = "twoways",
                   
                   fesis = TRUE)

is1

plot(is1)

```
An example using coefficient step indicator saturation: 

``` r

is2 <- isatpanel(data = pandata_simulated,
                   formula = gdp ~ temp, 
                   index = c("country","year"),
                   
                   effect = "twoways",
                   
                   csis = TRUE)
                   
                   
is2     

plot(is2)
```

and an example of Coefficient Fixed-Effect Step indicator saturation: 

``` r

is3 <- isatpanel(data = pandata_simulated,
                   formula = gdp ~ temp, 
                   index = c("country","year"),
                   
                   effect = "twoways",
                   
                   cfesis = TRUE)
                   
is3

plot(is3)

```

We can also use e.g. the `fixest` package to estimate our models: 

``` r
is4 <- isatpanel(data = pandata_simulated,
                   formula = gdp ~ temp, 
                   index = c("country","year"),
                   
                   effect = "twoways",
                   engine = "fixest",
                   
                   fesis = TRUE)

```
