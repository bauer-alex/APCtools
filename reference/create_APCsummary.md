# Create a summary table for multiple estimated GAM models

Create a table to summarize the overall effect strengths of the age,
period and cohort effects for models fitted with
[`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
[`bam`](https://rdrr.io/pkg/mgcv/man/bam.html). The output format can be
adjusted by passing arguments to
[`kable`](https://rdrr.io/pkg/knitr/man/kable.html) via the `...`
argument.

## Usage

``` r
create_APCsummary(
  model_list,
  dat,
  digits = 2,
  apc_range = NULL,
  kable = TRUE,
  ...
)
```

## Arguments

- model_list:

  A list of regression models estimated with
  [`gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or
  [`bam`](https://rdrr.io/pkg/mgcv/man/bam.html). If the list is named,
  the names are used as labels. Can also be a single model object
  instead of a list.

- dat:

  Dataset with columns `period` and `age`. If `y_var` is specified, the
  dataset must contain the respective column. If `model` is specified,
  the dataset must have been used for model estimation with `gam` or
  `bam`.

- digits:

  Number of digits for numeric columns. Defaults to 2.

- apc_range:

  Optional list with one or multiple elements with names

- kable:

  Should the output be a table in kable style? Defaults to `TRUE`.
  `"age","period","cohort"` to filter the data. Each element should
  contain a numeric vector of values for the respective variable that
  should be kept in the data. All other values are deleted before
  producing the table.

- ...:

  Optional additional arguments passed to
  [`kable`](https://rdrr.io/pkg/knitr/man/kable.html).

## Value

Table created with [`kable`](https://rdrr.io/pkg/knitr/man/kable.html).

## Details

If the model was estimated with a log or logit link, the function
automatically performs an exponential transformation of the effect.

## Author

Alexander Bauer <alexander.bauer@stat.uni-muenchen.de>

## Examples

``` r
library(APCtools)
library(mgcv)
#> Loading required package: nlme
#> This is mgcv 1.9-4. For overview type '?mgcv'.

data(travel)

# create the summary table for one model
model_pure <- gam(mainTrip_distance ~ te(age, period), data = travel)
create_APCsummary(model_pure, dat = travel)
#> 
#> 
#> |effect | value_withMaxEffect| value_withMinEffect| max_effect| min_effect| difference|
#> |:------|-------------------:|-------------------:|----------:|----------:|----------:|
#> |age    |                  21|                  99|     350.58|    -727.80|    1078.38|
#> |period |                2018|                1971|     428.09|    -504.89|     932.97|
#> |cohort |                2004|                1872|     888.11|    -780.49|    1668.60|

# create the summary table for multiple models
model_cov  <- gam(mainTrip_distance ~ te(age, period) + s(household_income),
                  data = travel)
model_list <- list("pure model"      = model_pure,
                   "covariate model" = model_cov)
create_APCsummary(model_list, dat = travel)
#> 
#> 
#> |model           |effect | value_withMaxEffect| value_withMinEffect| max_effect| min_effect| difference|
#> |:---------------|:------|-------------------:|-------------------:|----------:|----------:|----------:|
#> |pure model      |age    |                  21|                  99|     350.58|    -727.80|    1078.38|
#> |pure model      |period |                2018|                1971|     428.09|    -504.89|     932.97|
#> |pure model      |cohort |                2004|                1872|     888.11|    -780.49|    1668.60|
#> |covariate model |age    |                  14|                  99|     306.88|    -530.72|     837.60|
#> |covariate model |period |                2018|                1971|     404.05|    -505.27|     909.32|
#> |covariate model |cohort |                2004|                1872|     837.70|    -609.60|    1447.31|
```
