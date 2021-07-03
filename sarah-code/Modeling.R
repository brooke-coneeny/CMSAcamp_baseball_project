library(tidyverse)
library(mgcv)

# Loading Data ------------------------------------------------------------

batter_all_2019 <- read_rds("private_data/all2019data.rds")


# Fitting First LM for wOBA/LA/EV -----------------------------------------


init_lm_LA_EV <- lm(woba_value ~ launch_angle + launch_speed, data = batter_all_2019)
summary(init_lm_LA_EV)
library(ggfortify)
autoplot(init_lm_LA_EV, ncol = 4) + theme_bw() #a HOT mess... (residualas a mess, QQ looks kinda decent,
                                        # lots of high leverage points?)


# GAM / Spline Model ------------------------------------------------------

gam_model1 <- gam(woba_value ~ s(launch_angle) + s(launch_speed), data = batter_all_2019)
summary(gam_model1)
