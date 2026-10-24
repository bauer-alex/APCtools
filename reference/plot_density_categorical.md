# Internal helper to plot a categorical density

Internal helper function to plot one categorical density, to be called
from within [`plot_density`](plot_density.md).

## Usage

``` r
plot_density_categorical(
  dat,
  y_var,
  dat_highlightDiagonals = NULL,
  weights_var = NULL,
  xlab = NULL,
  ylab = NULL
)
```

## Arguments

- dat:

  Dataset with columns `period` and `age` and the main variable
  specified through argument `y_var`.

- y_var:

  Character name of the main variable to be plotted.

- dat_highlightDiagonals:

  Optional dataset created by
  [`create_highlightDiagonalData`](create_highlightDiagonalData.md) to
  highlight specific diagonals in a density matrix.

- weights_var:

  Optional character name of a weights variable used to project the
  results in the sample to some population.

- xlab, ylab:

  Optional plot annotations.
