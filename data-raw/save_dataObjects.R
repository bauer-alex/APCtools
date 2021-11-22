
library(stringr)


# load the data from the 'data-raw' folder --------------------------------
data_RA <- readRDS("Reiseanalyse_sample.rds")



# save the data in the package --------------------------------------------
usethis::use_data(data_RA, overwrite = TRUE)
