# Plotting an isatpanel object

Plotting an isatpanel object

## Usage

``` r
# S3 method for class 'isatpanel'
plot(
  x,
  max.id.facet = 16,
  facet.scales = "free",
  title = NULL,
  zero_line = FALSE,
  ...
)
```

## Arguments

- x:

  An object produced by the isatpanel function

- max.id.facet:

  The resulting plot will be faceted for each individual in the panel.
  Beyond a certain number, this might result in unreadable figures.
  Default set at 16.

- facet.scales:

  To be passed to ggplot2::facet_wrap. Default is "free" (i.e. a
  separate y axis for each panel group/id). Alternatives are: "fixed",
  "fixed_y", and "fixed_x".

- title:

  Plot title. Must be a character vector.

- zero_line:

  Plot a horizontal line at y = 0. Default is FALSE.

- ...:

  Further arguments to be passed to ggplot2.

## Value

A ggplot2 plot that plots an 'isatpanel' object and shows observed data,
the fitted values, and all identified breaks and impulses.
