# Internal helper to compute marginal APC effects and their confidence intervals

Internal helper function to add lower and upper confidence boundaries
pointwise

## Usage

``` r
compute_marginalAPCeffects(dat, model, variable, plot_CI = FALSE)
```

## Arguments

- dat:

  Dataset containing predicted effects for a grid of all APC dimensions
  and covariates used in the model.

- model:

  Model fitted with [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`bam`](https://rdrr.io/pkg/mgcv/man/bam.html).

- variable:

  One of `c("age","period","cohort")`, specifying the temporal dimension
  for which the partial effect plots should be created.

- plot_CI:

  Indicator if 95% confidence intervals for marginal APC effects should
  be computed. Defaults to FALSE.

## Details

If the model was estimated with a log or logit link, the function
automatically performs an exponential transformation of the effect.
