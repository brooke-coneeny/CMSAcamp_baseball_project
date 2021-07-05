library(tidyverse)
library(glmnet)
library(mgcv)

batter_all_2019 <- read_rds("private_data/all2019data.rds")

woba_model <- gam(woba_value ~ s(launch_angle) + s(launch_speed), 
                  data = batter_all_2019, method = "REML")

#coefficents are both close to 9 effective degrees of freedom 
#meaning complex and wiggly relationship with woba. 
summary(woba_model)

#cannot draw horizontal line through either plot indicating significance. 
plot(woba_model, pages =1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue")

#check model adequacy - the pvalue for launch speed especially is relatively small
#so we may want to try a different value for k
gam.check(woba_model, pages = 1)

woba_model2 <- gam(woba_value ~ s(launch_angle, k =10) + s(launch_speed, k=15), 
                   data = batter_all_2019, method = "REML", sp = 0.01)

#both edf values are high meaning wiggly relationship with woba, significant p-values
summary(woba_model2) 

#histogram looks okay but too tall in middle, qq plot looks decent
#residual plots look kind of frightening...due to some variable not sure why yet
plot(woba_model2, pages =1, all.terms = TRUE, shade = TRUE, shade.col = "lightblue")

#pvalues are not too low which is good
gam.check(woba_model2)

#check concurvity... looks good because there are no high values 
concurvity(woba_model2, full = TRUE)
