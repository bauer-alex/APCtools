# APCtools 1.0.5 (2023-08)

- Added option to plot 95% confidence intervals for marginal APC effects
in `plot_marginalAPCeffects` and `plot_jointMarginalAPCeffects`
- Added simple exponentiation of boundaries as second and default option for
confidence intervals with log link also to `plot_APCheatmap` 

# APCtools 1.0.4 (2022-12)

- Fixed a bug in `plot_1Dsmooth`, which caused the confidence bounds to be twice as wide as they should be (on the level of the linear predictor)
- Added option to include the reference category in the plot output of `plot_linearEffects`
- Added second option for confidence intervals with log link (simple exponentiation of boundaries). This method is the new default for the argument `method_expTransform` in `plot_linearEffects`, `plot_1Dsmooth` and `create_modelSummary`. 


# APCtools 1.0.3 (2022-04)

- Fixed plot_densityMatrix, which wrongly assigned data to some cells in some situations


# APCtools 1.0.2 (2022-04)

- Fixed computation of confidence intervals for smooth effects 


# APCtools 1.0.1 (2022-01)

- Fixed a unit test error raised by CRAN


# APCtools 1.0.0 (2022-01)

- First stable package version
