# Internal helper for gg_addReferenceLines to keep diagonal lines in the plot range

Internal helper function to be called from within
[`gg_addReferenceLines`](gg_addReferenceLines.md). This function takes
the dataset prepared for adding diagonal reference lines in the plot,
checks if some diagonals exceed the plot limits, cuts them accordingly,
if necessary, and again returns the corrected dataset.

## Usage

``` r
ensure_segmentsInPlotRange(dat_segments, plot_dat)
```

## Arguments

- dat_segments:

  Dataset containing information on the diagonal reference lines.

- plot_dat:

  Dataset used for creating the heatmap.
