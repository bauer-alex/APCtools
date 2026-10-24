# Internal helper to add reference lines in an APC heatmap

Internal helper function to add reference lines in an APC heatmap
(vertically, horizontally or diagonally). The function takes an existing
list of ggplot objects, adds the specified reference lines in each plot
and returns the edited ggplot list again. To be called from within
[`plot_APCheatmap`](plot_APCheatmap.md).

## Usage

``` r
gg_addReferenceLines(
  gg_list,
  dimensions,
  plot_dat,
  markLines_list,
  markLines_displayLabels
)
```

## Arguments

- gg_list:

  Existing list of ggplot objects where the reference lines should be
  marked in each individual ggplot.

- dimensions:

  Character vector specifying the two APC dimensions that should be
  visualized along the x-axis and y-axis. Defaults to
  `c("period","age")`.

- plot_dat:

  Dataset used for creating the heatmap.

- markLines_list:

  Optional list that can be used to highlight the borders of specific
  age groups, time intervals or cohorts. Each element must be a numeric
  vector of values where horizontal, vertical or diagonal lines should
  be drawn (depends on which APC dimension is displayed on which axis).
  The list can maximally have three elements and must have names out of
  `c("age","period","cohort")`.

- markLines_displayLabels:

  Optional character vector defining for which dimensions the lines
  defined through `markLines_list` should be marked by a respective
  label. The vector should be a subset of `c("age","period","cohort")`,
  or NULL to suppress all labels. Defaults to
  `c("age","period","cohort")`.
