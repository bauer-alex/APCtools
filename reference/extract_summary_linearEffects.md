# Internal helper to extract summary of linear effects in a gam model

Internal helper function to create a `data.frame` containing the linear
effects summary of a model fitted with
[`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`bam`](https://rdrr.io/pkg/mgcv/man/bam.html).

## Usage

``` r
extract_summary_linearEffects(model, method_expTransform = "simple")
```

## Arguments

- model:

  Model fitted with [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`bam`](https://rdrr.io/pkg/mgcv/man/bam.html).

- method_expTransform:

  One of `c("simple","delta")`, stating if standard errors and
  confidence interval limits should be transformed by a simple exp
  transformation or using the delta method. The delta method can be
  unstable in situations and lead to negative confidence interval
  limits. Only used when the model was estimated with a log or logit
  link.

## Details

If the model was estimated with a log or logit link, the function
automatically performs an exponential transformation of the effect, see
argument `method_expTransform`.
