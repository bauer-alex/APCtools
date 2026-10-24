# Internal helper to plot a metric density

Internal helper function to plot one metric density, to be called from
within [`plot_density`](plot_density.md).

## Usage

``` r
plot_density_metric(
  dat,
  y_var,
  plot_type = "density",
  dat_highlightDiagonals = NULL,
  y_var_cat_breaks = NULL,
  y_var_cat_labels = NULL,
  weights_var = NULL,
  log_scale = FALSE,
  xlab = NULL,
  ylab = NULL,
  legend_title = NULL,
  ...
)
```

## Arguments

- dat:

  Dataset with columns `period` and `age` and the main variable
  specified through argument `y_var`.

- y_var:

  Character name of the main variable to be plotted.

- plot_type:

  One of `c("density","boxplot")`. Only used if the `y_var` column is
  metric.

- dat_highlightDiagonals:

  Optional dataset created by
  [`create_highlightDiagonalData`](create_highlightDiagonalData.md) to
  highlight specific diagonals in a density matrix.

- y_var_cat_breaks:

  Optional numeric vector of breaks to categorize `y_var` based on
  calling function [`cut`](https://rdrr.io/r/base/cut.html). Only used
  to highlight the categories based on different colors. And only used
  if the `y_var` column is numeric.

- y_var_cat_labels:

  Optional character vector for the names of the categories that were
  defined based on `y_var_cat_breaks`. The length of this vector must be
  one shorter than `length(y_var_cat_breaks)`. Only used if the `y_var`
  column is numeric.

- weights_var:

  Optional character name of a weights variable used to project the
  results in the sample to some population.

- log_scale:

  Indicator if the main variable should be log10 transformed. Only used
  if the `y_var` column is numeric. Defaults to FALSE.

- xlab, ylab, legend_title:

  Optional plot annotations.

- ...:

  Additional arguments passed to
  [`density`](https://rdrr.io/r/stats/density.html).
