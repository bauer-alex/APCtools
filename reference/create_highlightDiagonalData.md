# Internal helper to create a dataset for ggplot2 to highlight diagonals

Internal helper function to create a dataset for `ggplot2` that can be
used to highlight specific diagonals in a density matrix.

## Usage

``` r
create_highlightDiagonalData(dat, highlight_diagonals)
```

## Arguments

- dat:

  Dataset with columns `period` and `age` and the main variable
  specified through argument `y_var`.

- highlight_diagonals:

  Optional internal parameter which is only specified when
  `plot_density` is called from within `plot_densityMatrix`. See
  [`plot_densityMatrix`](plot_densityMatrix.md) for details.
