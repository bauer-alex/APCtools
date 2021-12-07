
#' Data from the German Reiseanalyse survey
#' 
#' This dataset from the
#' \href{https://reiseanalyse.de/home/}{Reiseanalyse survey} comprises travel
#' information on German travelers between 1971 and 2018. Data were collected
#' in a yearly repeated cross-sectional survey of German pleasure travels,
#' based on a sample representative for the (West) German citizens (until 2009)
#' or for all German-speaking residents (starting 2010). Travelers from former
#' East Germany are only included since 1990. Note that the sample only contains
#' trips with at least five days of trip length. For details see
#' \href{https://journals.sagepub.com/doi/10.1177/1354816620987198}{Weigert et al. (2021)}.
#' 
#' The data are a 10% random sample of all respondents who undertook at least
#' one trip in the respective year, between 1971 and 2018. We thank the
#' \href{https://reiseanalyse.de/about-us/}{Forschungsgemeinschaft Urlaub und Reisen e.V.}
#' for allowing us to publish this sample.
#' 
#' @docType data
#' 
#' @usage data(travel)
#' 
#' @format A dataframe containing
#' \describe{
#'   \item{period}{Year in which the respondent traveled.}
#'   \item{age}{Age of the respondent.}
#'   \item{sampling_weight}{Individual weight of each respondent to account for
#'   a not perfectly representative sample and project the sample results to
#'   the population of German citizens (until 2009) or of German-speaking
#'   residents (starting 2010). Only available since 1974.}
#'   \item{german_citizenship}{Indicator if the respondent is German citizen or
#'   not. Only available since 2010. Until 2009, all respondents were German
#'   citizens.}
#'   \item{residence_region}{Indicator if the respondent's main residence is in a
#'   federal state in the former area of West Germany or in the former area of
#'   East Germany.}
#'   \item{household size}{Categorized size of the respondent's household.}
#'   \item{household_income}{Joint income (in €) of the respondent's household.}
#'   \item{mainTrip_duration}{Categorized trip length of the respondent's
#'   \emph{main trip}. The main trip is the trip which the respondent stated was
#'   his/her most important trip in the respective year.}
#'   \item{mainTrip_distance}{Distance (in km) between the center of the
#'   respondent's federal state and the center of the country of destination,
#'   for the \emph{main trip}. The main trip is the trip which the respondent
#'   stated was his/her most important trip in the respective year.}
#' }
#' 
#' @references Weigert, M., Bauer, A., Gernert, J., Karl, M., Nalmpatian, A., Küchenhoff,
#' H., and Schmude, J. (2021). Semiparametric APC analysis of destination choice
#' patterns: Using generalized additive models to quantify the impact of age,
#' period, and cohort on travel distances. \emph{Tourism Economics}.
#' \href{https://doi.org/10.1177/1354816620987198}{doi:10.1177/1354816620987198}.
#' 
#' Forschungsgemeinschaft Urlaub und Reisen e.V. (FUR) (2020b) \emph{Survey of
#' tourist demand in Germany for holiday travel and short breaks}. Available at:
#' \href{https://reiseanalyse.de/wp-content/uploads/2019/08/RA2020_Infoflyer_EN.pdf}{https://reiseanalyse.de/wp-content/uploads/2019/08/RA2020_Infoflyer_EN.pdf}
#' (accessed 22 November 2021).
#' 
#' @keywords datasets
#' 
"travel"



#' Drug deaths of white men in the United States
#' 
#' Dataset on the number of unintentional drug overdose deaths in the United
#' States for each age group between 1999 and 2019, retrieved from the CDC
#' WONDER Online Database. The data only cover white men.
#' 
#' The data were exported from the CDC WONDER Online Database (see link in
#' references down below), based on the following settings:
#' * Group by \emph{Year} and by \emph{Single-Year Ages}
#' * Demographics: Gender \emph{Male}; Ethnicity \emph{White}
#' * Cause of death: \emph{Drug / Alcohol Induced Causes}. Then select the more
#' specific category \emph{Drug poisonings (overdose) Unintentional (X40-X44)}.
#' 
#' @docType data
#' 
#' @usage data(drug_deaths)
#' 
#' @format A dataframe containing
#' \describe{
#'   \item{period}{Calendar year}
#'   \item{age}{Age group.}
#'   \item{deaths}{Number of observed unintentional drug overdose deaths in the
#'   respective age group and calendar year.}
#'   \item{population}{Number of white men in the respective age group and
#'   calendar year in the U.S. population.}
#'   \item{mortality_rate}{Drug overdose mortality rate for the respective age
#'   group and calendar year, reported as the number of deaths per 100,000
#'   people. Calculated as \code{100000 * deaths / population}.}
#' }
#' 
#' @references
#' Jalal, H., & Burke, D. S. (2020). Hexamaps for Age-Period-Cohort Data
#' Visualization and Implementation in R. \emph{Epidemiology (Cambridge, Mass.)},
#' 31(6), e47.
#' \href{https://dx.doi.org/10.1097%2FEDE.0000000000001236}{doi:10.1097/EDE.0000000000001236}.
#' 
#' Centers for Disease Control and Prevention, National Center for Health
#' Statistics. Underlying Cause of Death 1999-2019 on CDC WONDER Online Database,
#' released in 2020. Data are from the Multiple Cause of Death Files, 1999-2019,
#' as compiled from data provided by the 57 vital statistics jurisdictions
#' through the Vital Statistics Cooperative Program. Accessed at
#' \href{http://wonder.cdc.gov/ucd-icd10.html}{wonder.cdc.gov/ucd-icd10.html}
#' on 6 December 2021.
#' 
#' @keywords datasets
#' 
"drug_deaths"