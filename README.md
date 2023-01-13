
# APCtools <img src="man/figures/hex-sticker/hex-sticker.svg" align="right" width="200"/>

<!-- badges: start -->

[![R build
status](https://github.com/bauer-alex/APCtools/workflows/R-CMD-check/badge.svg)](https://github.com/bauer-alex/APCtools/actions)
[![Codecov test
coverage](https://codecov.io/gh/bauer-alex/APCtools/branch/main/graph/badge.svg?token=KrjDYWRi2W)](https://app.codecov.io/gh/bauer-alex/APCtools)
[![](https://cranlogs.r-pkg.org/badges/grand-total/APCtools)](https://cran.r-project.org/package=APCtools)
[![MIT
license](https://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Routines for Descriptive and Model-Based APC Analysis

  - Authors: [Alexander
    Bauer](https://github.com/bauer-alex),
    [Maximilian
    Weigert](https://www.en.stablab.stat.uni-muenchen.de/people/doktoranden/weigert/index.html),
    [Hawre
    Jalal](https://med.uottawa.ca/epidemiology/people/jalal-hawre)
  - Version: 1.0.4

## Aim of this Package

Age-Period-Cohort (APC) analysis aims to determine relevant drivers for
long-term developments and is used in many fields of science. The main
focus is on disentangling the interconnected effects of age, period, and
cohort. Long-term developments of some characteristic can either be
associated with changes in a person’s life cycle (age), macro-level
developments over the years that simultaneously affect all age groups
(period), or the generational membership of an individual, shaped by
similar socialization processes and historical experiences (cohort). The
critical challenge in APC analysis is the linear dependency of the
components age, period, and cohort (cohort = period - age). Accordingly,
flexible methods and visualization techniques are needed to properly
disentangle observed temporal association structures.

In contrast to other software packages, `APCtools` builds on a flexible
and robust semiparametric regression approach to circumvent this
identification problem. The package includes modern visualization
techniques and general routines to facilitate the interpretability of
the estimated temporal structures and simplify the workflow of an APC
analysis.

## Main Functionality

Sophisticated functions are available both for descriptive and
regression model-based analyses. For the former, we use density (or
ridgeline) matrices, classical heatmaps and *hexamaps* (hexagonally
binned heatmaps) as innovative visualization techniques building on the
concept of Lexis diagrams. Model-based analyses build on the separation
of the temporal dimensions based on generalized additive models, where a
tensor product interaction surface (usually between age and period) is
utilized to represent the third dimension (usually cohort) on its
diagonal. Such tensor product surfaces can also be estimated while
accounting for further covariates in the regression model.

## Documentation and Useful Materials

  - To get an overview of the functionalities of the package, check out
    the [JOSS
    publication](https://joss.theoj.org/papers/10.21105/joss.04056) or
    the [package
    vignette](https://bauer-alex.github.io/APCtools/articles/main_functionality.html).

  - See [Weigert et
    al. (2021)](https://doi.org/10.1177/1354816620987198) or our
    corresponding [research
    poster](https://www.researchgate.net/publication/353852226_Visualization_techniques_for_semiparametric_APC_analysis_Using_Generalized_Additive_Models_to_examine_touristic_travel_distances)
    for methodological details.

  - Hexamaps as a concept for the visualization of APC structures are
    outlined in [Jalal & Burke
    (2020)](https://doi.org/10.1097/EDE.0000000000001236).

## Installation

The most current version from GitHub can be installed via

``` r
devtools::install_github("bauer-alex/APCtools")
```

## How to Contribute

If you encounter problems with the package, find bugs or have
suggestions for additional functionalities please open a [GitHub
issue](https://github.com/bauer-alex/APCtools/issues). Alternatively,
feel free to contact us directly via email.

Contributions (via pull requests or otherwise) are welcome. Before you
open a pull request or share your updates with us, please make sure that
all unit tests pass without errors or warning messages. You can run the
unit tests by calling

``` r
devtools::test()
```

## References

Bauer, A., Weigert, M., and Jalal, H. (2022). APCtools: Descriptive and
Model-based Age-Period-Cohort Analysis. Journal of Open Source Software,
7(73), 4056, <https://doi.org/10.21105/joss.04056>.

Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
destination choice patterns: Using generalized additive models to
quantify the impact of age, period, and cohort on travel distances.
*Tourism Economics*. <https://doi.org/10.1177/1354816620987198>.

Jalal, H., Burke, D. (2020). Hexamaps for Age–Period–Cohort Data
Visualization and Implementation in R. *Epidemiology*, 31 (6), e47-e49.
doi: <https://doi.org/10.1097/EDE.0000000000001236>.
