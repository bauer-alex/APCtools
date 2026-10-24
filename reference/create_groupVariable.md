# Internal helper to create a group variable as base for a density matrix

Internal helper function to create a group variable based on the
categorization of either age, period or cohort. To be called from within
[`plot_densityMatrix`](plot_densityMatrix.md).

## Usage

``` r
create_groupVariable(dat, APC_var, groups_list)
```

## Arguments

- dat:

  Dataset with a column `"age"`, `"period"` or `"cohort"`, dependent on
  the specified `APC_var`.

- APC_var:

  One of `c("age","period","cohort")`.

- groups_list:

  A list with each element specifying the borders of one row or column
  in the density matrix. E.g., if the period should be visualized in
  decade columns from 1980 to 2009, specify
  `groups_list = list(c(1980,1989), c(1990,1999), c(2000,2009))`. The
  list can be named to specify labels for the categories.

## Value

Vector for the grouping that can be added as additional column to the
data.
