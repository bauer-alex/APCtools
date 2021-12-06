
library(dplyr)


# read the data from the 'data-raw' folder --------------------------------
# travel behavior of German travelers
travel <- readRDS("Reiseanalyse_sample.rds")

# white male drug deaths in the United States
drug_deaths <- read.table("usa_drugDeaths_1999_2019.txt", sep = "\t",
                          header = TRUE, nrows = 2142,
                          na.strings = c("NS","Not Applicable","Unreliable"))



# further data preparation ------------------------------------------------
drug_deaths <- drug_deaths %>% 
  select(Year, Single.Year.Ages.Code, Deaths, Population, Crude.Rate) %>% 
  dplyr::rename(period     = Year,
                age        = Single.Year.Ages.Code,
                deaths     = Deaths,
                population = Population,
                death_rate = Crude.Rate) %>% 
  filter(!is.na(age)) %>% 
  mutate(age        = as.integer(age),
         population = as.integer(population),
         death_rate = as.integer(death_rate))



# save the data in the package --------------------------------------------
usethis::use_data(travel, drug_deaths, overwrite = TRUE)
