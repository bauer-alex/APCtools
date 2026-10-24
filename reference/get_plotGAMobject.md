# Extract returned values of plot.gam() while suppressing creation of the plot

Internal helper function to extract the values returned of
[`plot.gam`](https://rdrr.io/pkg/mgcv/man/plot.gam.html) while
suppressing creation of the plot.

## Usage

``` r
get_plotGAMobject(model)
```

## Arguments

- model:

  GAM model fitted with [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html)
  or [`bam`](https://rdrr.io/pkg/mgcv/man/bam.html).
