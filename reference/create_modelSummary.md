# Create model summary tables for multiple estimated GAM models

Create publication-ready summary tables of all linear and nonlinear
effects for models fitted with
[`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`bam`](https://rdrr.io/pkg/mgcv/man/bam.html). The output format of the
tables can be adjusted by passing arguments to
[`kable`](https://rdrr.io/pkg/knitr/man/kable.html) via the `...`
argument.

## Usage

``` r
create_modelSummary(
  model_list,
  digits = 2,
  method_expTransform = "simple",
  ...
)
```

## Arguments

- model_list:

  list of APC models

- digits:

  number of displayed digits

- method_expTransform:

  One of `c("simple","delta")`, stating if standard errors and
  confidence interval limits should be transformed by a simple exp
  transformation or using the delta method. The delta method can be
  unstable in situations and lead to negative confidence interval
  limits. Only used when the model was estimated with a log or logit
  link.

- ...:

  additional arguments to
  [`kable`](https://rdrr.io/pkg/knitr/man/kable.html)

## Value

List of tables created with
[`kable`](https://rdrr.io/pkg/knitr/man/kable.html).

## Details

If the model was estimated with a log or logit link, the function
automatically performs an exponential transformation of the effects.

The table for linear coefficients includes the estimated coefficient
(`coef`), the corresponding standard error (`se`), lower and upper
limits of 95% confidence intervals (`CI_lower`, `CI_upper`) and the
p-values for all coefficients apart from the intercept.

The table for nonlinear coefficients include the estimated degrees of
freedom (`edf`) and the p-value for each estimate.

## Author

Alexander Bauer <alexander.bauer@stat.uni-muenchen.de>

## Examples

``` r
library(APCtools)
library(mgcv)

data(travel)
model <- gam(mainTrip_distance ~ te(age, period) + residence_region +
             household_size + s(household_income), data = travel)

create_modelSummary(list(model), dat = travel)
#> [[1]]
#> 
#> 
#> |model   |param                               |     coef|    se| CI_lower| CI_upper|pvalue |
#> |:-------|:-----------------------------------|--------:|-----:|--------:|--------:|:------|
#> |model 1 |(Intercept)                         |  2327.08| 39.99|  2248.70|  2405.45|-      |
#> |model 1 |residence_regionFormer East Germany |   -94.96| 42.69|  -178.63|   -11.30|0.0261 |
#> |model 1 |household_size2 persons             |  -545.63| 50.28|  -644.18|  -447.09|<.0001 |
#> |model 1 |household_size3 persons             | -1164.36| 62.34| -1286.56| -1042.17|<.0001 |
#> |model 1 |household_size4 persons             | -1506.61| 66.32| -1636.59| -1376.62|<.0001 |
#> |model 1 |household_size5+ persons            | -1649.11| 94.82| -1834.96| -1463.26|<.0001 |
#> 
#> [[2]]
#> 
#> 
#> |model   |param               |   edf|pvalue |
#> |:-------|:-------------------|-----:|:------|
#> |model 1 |te(age,period)      | 13.47|<.0001 |
#> |model 1 |s(household_income) |  8.30|<.0001 |
#> 
```
