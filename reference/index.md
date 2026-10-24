# Package index

## Visualizing observed structures

Functions to visualize structures observed in the data.

- [`plot_variable()`](plot_variable.md) : Distribution plot of one
  variable against one APC dimension
- [`plot_density()`](plot_density.md) : Plot the density of one metric
  or categorical variable
- [`plot_densityMatrix()`](plot_densityMatrix.md) : Create a matrix of
  density plots
- [`plot_APCheatmap()`](plot_APCheatmap.md) : Heatmap of an APC surface
- [`plot_APChexamap()`](plot_APChexamap.md) : Hexamap of an APC surface

## Visualizing and summarizing model-based results

Functions to visualize and summarize estimation results based on
generalized additive regression models (GAMs), where the APC effects
were estimated with a tensor product surface.

- [`plot_partialAPCeffects()`](plot_partialAPCeffects.md) : Partial APC
  plots based on an estimated GAM model

- [`plot_marginalAPCeffects()`](plot_marginalAPCeffects.md) : Plot of
  marginal APC effects based on an estimated GAM model

- [`plot_jointMarginalAPCeffects()`](plot_jointMarginalAPCeffects.md) :
  Joint plot to compare the marginal APC effects of multiple models

- [`create_APCsummary()`](create_APCsummary.md) : Create a summary table
  for multiple estimated GAM models

- [`plot_linearEffects()`](plot_linearEffects.md) : Plot linear effects
  of a gam in an effect plot

- [`plot_1Dsmooth()`](plot_1Dsmooth.md) :

  Plot 1D smooth effects for
  [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) models

- [`create_modelSummary()`](create_modelSummary.md) : Create model
  summary tables for multiple estimated GAM models

## Datasets

Internal datasets to showcase the package functionalities.

- [`travel`](travel.md) : Data from the German Reiseanalyse survey
- [`drug_deaths`](drug_deaths.md) : Drug deaths of white men in the
  United States

## Internal functions

Further internal functions that are not specifically meant for general
use.

- [`calc_density()`](calc_density.md) : Internal helper to calculate the
  (group-specific) density of a variable
- [`capitalize_firstLetter()`](capitalize_firstLetter.md) : Internal
  function to capitalize the first letter of a character
- [`compute_xCoordinate()`](compute_xCoordinate.md) : Internal helper to
  tilt the x-axis for the hexamap plot
- [`compute_yCoordinate()`](compute_yCoordinate.md) : Internal helper to
  tilt the x-axis for the hexamap plot
- [`create_groupVariable()`](create_groupVariable.md) : Internal helper
  to create a group variable as base for a density matrix
- [`create_highlightDiagonalData()`](create_highlightDiagonalData.md) :
  Internal helper to create a dataset for ggplot2 to highlight diagonals
- [`create_oneAPCsummaryTable()`](create_oneAPCsummaryTable.md) :
  Internal helper to create a summary table for one estimated GAM model
- [`ensure_segmentsInPlotRange()`](ensure_segmentsInPlotRange.md) :
  Internal helper for gg_addReferenceLines to keep diagonal lines in the
  plot range
- [`extract_summary_linearEffects()`](extract_summary_linearEffects.md)
  : Internal helper to extract summary of linear effects in a gam model
- [`get_plotGAMobject()`](get_plotGAMobject.md) : Extract returned
  values of plot.gam() while suppressing creation of the plot
- [`gg_addReferenceLines()`](gg_addReferenceLines.md) : Internal helper
  to add reference lines in an APC heatmap
- [`gg_highlightDiagonals()`](gg_highlightDiagonals.md) : Internal
  helper to add the diagonal highlighting to a ggplot
- [`plot_density_categorical()`](plot_density_categorical.md) : Internal
  helper to plot a categorical density
- [`plot_density_metric()`](plot_density_metric.md) : Internal helper to
  plot a metric density
- [`compute_marginalAPCeffects()`](compute_marginalAPCeffects.md) :
  Internal helper to compute marginal APC effects and their confidence
  intervals
