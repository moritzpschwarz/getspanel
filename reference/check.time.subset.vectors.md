# Internal function to check vectors that subset the indicator selection using the time dimension

Internal function to check vectors that subset the indicator selection
using the time dimension

## Usage

``` r
check.time.subset.vectors(time.vector, vector.name, time, id)
```

## Arguments

- time.vector:

  A vector containing the user input in e.g. `tis_time` or `fesis_time`

- vector.name:

  The name of argument that the user inputted this vector in. This is
  just to make error messages more elaborate.

- time:

  The time dimension of isatpanel.

- id:

  The id dimension of isatpanel.

## Value

Does not return any value but will throw error if something is not
correct.
