
library(APCtools)
library(mgcv)
library(dplyr)
library(ggplot2)
library(ggpubr)

theme_set(theme_minimal(base_size = 16))

# load the travel dataset
data(travel)



# density matrix ----------------------------------------------------------
age_groups    <- list(c(80,89),c(70,79),c(60,69),c(50,59),
                      c(40,49),c(30,39),c(20,29))
period_groups <- list(c(1971,1979),c(1980,1989),c(1990,1999),
                      c(2000,2009),c(2010,2018))

plot_densityMatrix(dat                 = travel,
                   y_var               = "mainTrip_distance",
                   age_groups          = age_groups,
                   period_groups       = period_groups,
                   highlight_diagonals = list("born 1950 - 1959" = 8,
                                              "born 1970 - 1979" = 10),
                   log_scale           = TRUE,
                   xlab                = "main trips travel distance on log10 scale") +
  theme(legend.position = "bottom")
ggsave("1_densityMatrix.png", width = 7, height = 6.5)



# model estimation --------------------------------------------------------
model_pure <- gam(mainTrip_distance ~ te(age, period, bs = "ps", k = c(8,8)),
                  data = travel)



# heatmap and marginal effects --------------------------------------------
gg1 <- plot_APCheatmap(dat            = travel,
                       model          = model_pure,
                       plot_CI        = FALSE,
                       bin_heatmap    = FALSE,
                       markLines_list = list(cohort = c(1900,1920,1939,1946,
                                                        1966,1982,1994)))

# marginal effect plots
gg2 <- plot_jointMarginalAPCeffects(list(model_pure), dat = travel)

# empty ggplot object, to fill some space in the plot grid
gg_empty <- ggplot()
ggpubr::ggarrange(gg1,
                  ggpubr::ggarrange(gg2, gg_empty, nrow = 2, heights = c(.84,.16)),
                  nrow = 1, widths = c(.4,.6))
ggsave("2_modelEffects.png", width = 14, height = 6)



# hexamap -----------------------------------------------------------------
png("3_modelHexamap.png", width = 1200, height = 1000, pointsize = 30)
plot_APChexamap(dat   = travel,
                model = model_pure)
dev.off()
