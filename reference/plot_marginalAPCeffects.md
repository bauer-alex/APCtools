# Plot of marginal APC effects based on an estimated GAM model

Plot the marginal effect of age, period or cohort, based on an APC model
estimated as a semiparametric additive regression model with
[`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`bam`](https://rdrr.io/pkg/mgcv/man/bam.html). This function is a
simple wrapper to [`plot_partialAPCeffects`](plot_partialAPCeffects.md),
called with argument `hide_partialEffects = TRUE`.

## Usage

``` r
plot_marginalAPCeffects(
  model,
  dat,
  variable = "age",
  vlines_vec = NULL,
  plot_CI = FALSE,
  return_plotData = FALSE
)
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

- variable:

  One of `c("age","period","cohort")`, specifying the temporal dimension
  for which the partial effect plots should be created.

- vlines_vec:

  Optional numeric vector of values on the x-axis where vertical lines
  should be drawn. Can be used to highlight the borders of specific age
  groups, time intervals or cohorts.

- plot_CI:

  Indicator if 95% confidence intervals should be plotted. Defaults to
  FALSE.

- return_plotData:

  If TRUE, a list of the datasets prepared for plotting is returned
  instead of the ggplot object. The list contains one dataset each for
  the overall effect (= evaluations of the APC surface to plot the
  partial effects) and for each marginal APC effect (no matter the
  specified value of the argument `variable`). Defaults to FALSE.

## Value

ggplot object

## References

Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
destination choice patterns: Using generalized additive models to
quantify the impact of age, period, and cohort on travel distances.
*Tourism Economics*. doi:10.1177/1354816620987198.

## Author

Alexander Bauer <alexander.bauer@stat.uni-muenchen.de>, Maximilian
Weigert <maximilian.weigert@stat.uni-muenchen.de>

## Examples

``` r
library(APCtools)
library(mgcv)

data(travel)
model <- gam(mainTrip_distance ~ te(age, period), data = travel)

plot_marginalAPCeffects(model, dat = travel, variable = "age")


# mark specific cohorts
plot_marginalAPCeffects(model, dat = travel, variable = "cohort",
                        vlines_vec = c(1966.5,1982.5,1994.5))

```
