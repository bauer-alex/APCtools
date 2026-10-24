# Internal helper to create a summary table for one estimated GAM model

Internal helper function to be called in
[`create_APCsummary`](create_APCsummary.md). This function creates the
summary table for one model estimated with
[`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`bam`](https://rdrr.io/pkg/mgcv/man/bam.html).

## Usage

``` r
create_oneAPCsummaryTable(model, dat, apc_range = NULL)
```

## Arguments

- model:

  Optional regression model estimated with
  [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`bam`](https://rdrr.io/pkg/mgcv/man/bam.html) to estimate a smoothed
  APC surface. Only used if `y_var` is not specified.

- dat:

  Dataset with columns `period` and `age`. If `y_var` is specified, the
  dataset must contain the respective column. If `model` is specified,
  the dataset must have been used for model estimation with `gam` or
  `bam`.

- apc_range:

  Optional list with one or multiple elements with names
  `"age","period","cohort"` to filter the data. Each element should
  contain a numeric vector of values for the respective variable that
  should be kept in the data. All other values are deleted.

## Value

`data.frame` containing aggregated information on the individual
effects.
