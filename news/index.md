# Changelog

## APCtools 1.0.9 (2026-10)

- added argument `return_data` to function `plot_APCheatmap`.

## APCtools 1.0.8 (2025-06)

CRAN release: 2025-06-18

- updated some ggplot2 dependencies in the plot_density functions to
  prevent the use of now deprecated ggplot2 arguments

## APCtools 1.0.7 (2025-03)

- Slightly optimized the `plot_hexamap` plot borders by adding
  `xpd = TRUE` to [`par()`](https://rdrr.io/r/graphics/par.html).

## APCtools 1.0.6 (2024-05)

- Added argument `legend_title` to `plot_APCheatmap`.

## APCtools 1.0.5 (2023-08)

- Added option to plot 95% confidence intervals for marginal APC effects
  in `plot_marginalAPCeffects` and `plot_jointMarginalAPCeffects`
- Added simple exponentiation of boundaries as second and default option
  for confidence intervals with log link also to `plot_APCheatmap`
- Fixed a bug in the handling of missing values for APC variables in
  `plot_APCheatmap`

## APCtools 1.0.4 (2022-12)

CRAN release: 2023-01-13

- Fixed a bug in `plot_1Dsmooth`, which caused the confidence bounds to
  be twice as wide as they should be (on the level of the linear
  predictor)
- Added option to include the reference category in the plot output of
  `plot_linearEffects`
- Added second option for confidence intervals with log link (simple
  exponentiation of boundaries). This method is the new default for the
  argument `method_expTransform` in `plot_linearEffects`,
  `plot_1Dsmooth` and `create_modelSummary`.

## APCtools 1.0.3 (2022-04)

- Fixed plot_densityMatrix, which wrongly assigned data to some cells in
  some situations

## APCtools 1.0.2 (2022-04)

- Fixed computation of confidence intervals for smooth effects

## APCtools 1.0.1 (2022-01)

CRAN release: 2022-01-11

- Fixed a unit test error raised by CRAN

## APCtools 1.0.0 (2022-01)

CRAN release: 2022-01-05

- First stable package version
