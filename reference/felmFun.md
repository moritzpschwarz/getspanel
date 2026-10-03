# Internal lfe/felm Estimation Method

Internal lfe/felm Estimation Method

## Usage

``` r
felmFun(y, x, effect, time, id, cluster = "individual", ...)
```

## Arguments

- y:

  dependent variable

- x:

  matrix of regressors

- effect:

  Fixed Effect specification

- time:

  Character vector of name of the time variable

- id:

  Character vector of the name of the group variable

- cluster:

  Character vector of the variable(s) to cluster Standard Errors at

- ...:

  Further arguments to pass to gets::isat

## Value

List to be used by gets::isat
