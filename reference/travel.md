# Data from the German Reiseanalyse survey

This dataset from the [Reiseanalyse survey](https://reiseanalyse.de/en/)
comprises travel information on German travelers between 1971 and 2018.
Data were collected in a yearly repeated cross-sectional survey of
German pleasure travels, based on a sample representative for the (West)
German citizens (until 2009) or for all German-speaking residents
(starting 2010). Travelers from former East Germany are only included
since 1990. Note that the sample only contains trips with at least five
days of trip length. For details see [Weigert et al.
(2021)](https://journals.sagepub.com/doi/10.1177/1354816620987198).

## Usage

``` r
data(travel)
```

## Format

A dataframe containing

- period:

  Year in which the respondent traveled.

- age:

  Age of the respondent.

- sampling_weight:

  Individual weight of each respondent to account for a not perfectly
  representative sample and project the sample results to the population
  of German citizens (until 2009) or of German-speaking residents
  (starting 2010). Only available since 1974.

- german_citizenship:

  Indicator if the respondent is German citizen or not. Only available
  since 2010. Until 2009, all respondents were German citizens.

- residence_region:

  Indicator if the respondent's main residence is in a federal state in
  the former area of West Germany or in the former area of East Germany.

- household size:

  Categorized size of the respondent's household.

- household_income:

  Joint income (in €) of the respondent's household.

- mainTrip_duration:

  Categorized trip length of the respondent's *main trip*. The main trip
  is the trip which the respondent stated was his/her most important
  trip in the respective year.

- mainTrip_distance:

  Distance (in km) between the center of the respondent's federal state
  and the center of the country of destination, for the *main trip*. The
  main trip is the trip which the respondent stated was his/her most
  important trip in the respective year.

## Details

The data are a 10% random sample of all respondents who undertook at
least one trip in the respective year, between 1971 and 2018. We thank
the [Forschungsgemeinschaft Urlaub und Reisen
e.V.](https://reiseanalyse.de/en/) for allowing us to publish this
sample.

## References

Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A.,
Küchenhoff, H., and Schmude, J. (2021). Semiparametric APC analysis of
destination choice patterns: Using generalized additive models to
quantify the impact of age, period, and cohort on travel distances.
*Tourism Economics*.
[doi:10.1177/1354816620987198](https://doi.org/10.1177/1354816620987198)
.

Forschungsgemeinschaft Urlaub und Reisen e.V. (FUR) (2020b) *Survey of
tourist demand in Germany for holiday travel and short breaks*.
Available at:
<https://reiseanalyse.de/wp-content/uploads/2022/11/RA2020_First-results_EN.pdf>
(accessed 13 January 2023).
