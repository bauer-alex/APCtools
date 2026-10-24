# Drug deaths of white men in the United States

Dataset on the number of unintentional drug overdose deaths in the
United States for each age group between 1999 and 2019, retrieved from
the CDC WONDER Online Database. The data only cover white men.

## Usage

``` r
data(drug_deaths)
```

## Format

A dataframe containing

- period:

  Calendar year

- age:

  Age group.

- deaths:

  Number of observed unintentional drug overdose deaths in the
  respective age group and calendar year.

- population:

  Number of white men in the respective age group and calendar year in
  the U.S. population.

- mortality_rate:

  Drug overdose mortality rate for the respective age group and calendar
  year, reported as the number of deaths per 100,000 people. Calculated
  as `100000 * deaths / population`.

## Details

The data were exported from the CDC WONDER Online Database (see link in
references down below), based on the following settings:

- Group by *Year* and by *Single-Year Ages*

- Demographics: Gender *Male*; Ethnicity *White*

- Cause of death: *Drug / Alcohol Induced Causes*. Then select the more
  specific category *Drug poisonings (overdose) Unintentional
  (X40-X44)*.

## References

Jalal, H., & Burke, D. S. (2020). Hexamaps for Age-Period-Cohort Data
Visualization and Implementation in R. *Epidemiology (Cambridge,
Mass.)*, 31(6), e47.
[doi:10.1097/EDE.0000000000001236](https://doi.org/10.1097/EDE.0000000000001236)
.

Centers for Disease Control and Prevention, National Center for Health
Statistics. Underlying Cause of Death 1999-2019 on CDC WONDER Online
Database, released in 2020. Data are from the Multiple Cause of Death
Files, 1999-2019, as compiled from data provided by the 57 vital
statistics jurisdictions through the Vital Statistics Cooperative
Program. Accessed at
[wonder.cdc.gov/ucd-icd10.html](https://wonder.cdc.gov/ucd-icd10.html)
on 18 June 2025.
