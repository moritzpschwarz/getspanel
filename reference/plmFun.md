# plm Function to estimate isatpanel

plm Function to estimate isatpanel

## Usage

``` r
plmFun(y, x, time, id, cluster, effect, model = "pooling", ...)
```

## Arguments

- y:

  Dependent Variable

- x:

  matrix or data.frame of regressors

- time:

  Vector of time variable

- id:

  Vector of group variable

- cluster:

  cluster specification

- effect:

  effect specification

- model:

  model specification

- ...:

  Further arguments passed to plm

## Value

A list to be used by gets::isat
