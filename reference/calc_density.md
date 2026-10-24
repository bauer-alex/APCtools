# Internal helper to calculate the (group-specific) density of a variable

Internal helper function that is called in
[`plot_density`](plot_density.md) to calculate the density of a metric
variable. If `plot_density` is called from within
[`plot_densityMatrix`](plot_densityMatrix.md) (i.e., when some of the
columns `c("age_group","period_group","cohort_group")` are part of the
dataset, the density is computed individually for all respective APC
groups.

## Usage

``` r
calc_density(dat, y_var, weights_var = NULL, ...)
```

## Arguments

- dat:

  Dataset with columns `period` and `age` and the main variable
  specified through argument `y_var`.

- y_var:

  Character name of the main variable to be plotted.

- weights_var:

  Optional character name of a weights variable used to project the
  results in the sample to some population.

- ...:

  Additional arguments passed to
  [`density`](https://rdrr.io/r/stats/density.html).

## Value

Dataset with the calculated densities.
