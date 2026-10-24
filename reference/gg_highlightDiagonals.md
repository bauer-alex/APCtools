# Internal helper to add the diagonal highlighting to a ggplot

Internal helper function to highlight diagonals in a density matrix. The
function takes an existing ggplot object, adds the diagonal highlighting
and returns the edited ggplot object again.

## Usage

``` r
gg_highlightDiagonals(gg, dat, dat_highlightDiagonals)
```

## Arguments

- gg:

  Existing ggplot object to which the diagonal highlighting should be
  added.

- dat:

  Dataset with columns `period` and `age` and the main variable
  specified through argument `y_var`.

- dat_highlightDiagonals:

  Dataset created by
  [`create_highlightDiagonalData`](create_highlightDiagonalData.md) to
  highlight specific diagonals in a density matrix.
