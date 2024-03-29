
library(APCtools)
library(mgcv)
library(dplyr)
library(ggplot2)
library(ggpubr)

theme_set(theme_minimal(base_size = 16))

# load the travel dataset
data(travel)

# restrict data to travelers with maximally 89 years of age
travel <- travel %>% filter(age <= 89)



# density matrix ----------------------------------------------------------
age_groups    <- list(c(80,89),c(70,79),c(60,69),c(50,59),
                      c(40,49),c(30,39),c(20,29))
period_groups <- list("1971 - 1979" = c(1970,1979), "1980 - 1989" = c(1980,1989),
                      "1990 - 1999" = c(1990,1999), "2000 - 2009" = c(2000,2009),
                      "2010 - 2018" = c(2010,2019))

plot_densityMatrix(dat                 = travel,
                   y_var               = "mainTrip_distance",
                   age_groups          = age_groups,
                   period_groups       = period_groups,
                   highlight_diagonals = list(8, 10),
                   log_scale           = TRUE,
                   xlab                = "Travel distance [km] on log10 scale") +
  theme(legend.position = "bottom")
ggsave("1_densityMatrix.png", width = 10, height = 8)



# model estimation --------------------------------------------------------
model_pure <- gam(mainTrip_distance ~ te(age, period, bs = "ps", k = c(8,8)),
                  data = travel)



# heatmap and marginal effects --------------------------------------------
gg1 <- plot_APCheatmap(dat            = travel,
                       model          = model_pure,
                       plot_CI        = FALSE,
                       bin_heatmap    = FALSE,
                       markLines_list = list(cohort = c(1900,1920,1939,1946,
                                                        1966,1982,1994)),
                       legend_limits = c(-1,1) * 1234)

# marginal effect plots
gg2 <- plot_jointMarginalAPCeffects(list(model_pure), dat = travel)

# empty ggplot object, to fill some space in the plot grid
gg_empty <- ggplot()
ggpubr::ggarrange(gg1,
                  ggpubr::ggarrange(gg2, gg_empty, nrow = 2, heights = c(.84,.16)),
                  nrow = 1, widths = c(.4,.6))
ggsave("2_modelEffects.png", width = 14, height = 6)



# hexamap -----------------------------------------------------------------
# create the following two hexamaps with R and then manually merge them in
# GIMP (s.t. the joined plot can be made a bit more compact by cutting out
# some white space)

# different color scale for the observed data
color_palette <- grDevices::colorRampPalette(c("turquoise4", gray(0.95), "violetred1"))
color_vec     <- color_palette(100)

png("3_observedHexamap.png", width = 1940, height = 2400, pointsize = 60)
plot_APChexamap(dat            = travel,
                y_var          = "mainTrip_distance",
                y_var_logScale = TRUE,
                color_vec      = color_vec,
                legend_title   = "Average travel\ndistance [km]\non log10 scale")
dev.off()

png("3_modelHexamap.png", width = 1940, height = 2400, pointsize = 60)
plot_APChexamap(dat   = travel,
                model = model_pure)
dev.off()
