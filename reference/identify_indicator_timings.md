# Internal function to identify the timing of selected indicators

Internal function to identify the timing of selected indicators

## Usage

``` r
identify_indicator_timings(object, uis_breaks = NULL, isat_object = NULL)
```

## Arguments

- object:

  data.frame

- uis_breaks:

  A character vector with the names of the UIS breaks if the `uis`
  argument was used in
  [isatpanel](http://moritzschwarz.org/getspanel/reference/isatpanel.md).

- isat_object:

  The object of class `isat` produced by
  [isatpanel](http://moritzschwarz.org/getspanel/reference/isatpanel.md).

## Value

A list of data.frames
