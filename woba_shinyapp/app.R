library(shiny)
library(tidyverse)
library(readr)
library(baseballr)
library(mgcv)
library(ggfortify)

#load data
batter_all_2019 <- read_rds("../private_data/all2019data.rds")
batter_all_2019hp <- batter_all_2019 %>%
  filter(description == "hit_into_play")

# Function that cleans up the edges (if a launch angle is outside of the edges then replace it with the
#+- 2sd value)
clean_edges <- function (data){
  for(i in 1:length(data$launch_angle)){
    if(data$launch_angle[i] < (mean(data$launch_angle)-2*sd(data$launch_angle))){
      data$cleaned_launch_angle[i] <- (mean(data$launch_angle)-2*sd(data$launch_angle))
    }
    else if(data$launch_angle[i] > (mean(data$launch_angle)+2*sd(data$launch_angle))){
      data$cleaned_launch_angle[i] <- (mean(data$launch_angle)+2*sd(data$launch_angle))
    }
    else{
      data$cleaned_launch_angle[i]<-data$launch_angle[i]
    }
  }
  return (data)
}
# Create model for attack angle and height predicting LA ------------------

# Need to calculate the attack angle for every at bat (needs to be the same for each individual player)
batter_all_2019hp <- batter_all_2019hp %>% group_by(player_name) %>% 
  filter(launch_speed <= 120 -.02 * abs(launch_angle - 12)^1.7) %>%
  filter(launch_speed >= quantile(launch_speed, .9, na.rm = TRUE)) %>%
  summarize(attack_angle = median(launch_angle)) %>%
  right_join(batter_all_2019hp, by = c("player_name"))

predicted_LA <- lm(launch_angle ~ attack_angle + plate_z, data=batter_all_2019hp)

final_woba_model2 <- gam(woba_value ~ s(launch_angle, launch_speed, k=200), data = batter_all_2019hp, 
                         method = "REML")

# set.seed(2021)
# # Create empty vectors of exit velocities 
# EV_vector1 <- vector()    #for no change in attack angle
# EV_vector2 <- vector()    #for plus one attack angle
# EV_vector3 <- vector()    #for minus one attack angle

# predicted_LA_adjust_attack <- function(woba_model, LA_model, player_data, orig_woba, orig_attack, attack)
# {
#   
#   # Model the predicted angles given the original attack angle
#   pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
#   pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, 
#                                                       sd = sigma(LA_model)), 
#                                         launch_angle = lm.preds + noise)
#   
#   # Need to sample the data for each predicted angle to find what exit velocity we would give it
#   for(i in 1:length(pred_angles$launch_angle)){
#     
#     # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with 
#     #the cap (plus or minus)
#     if(pred_angles$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
#       pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
#     }
#     else if(pred_angles$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
#       pred_angles$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
#     }
#     
#     # Filter for the player's launch angles plus or minus 3 degrees above the ACTUAL LA
#     hits_at_angle <- player_data %>% 
#       filter(cleaned_launch_angle <= orig_attack+3 & launch_angle >= 
#                orig_attack-3 & !is.na(launch_speed))
#     # Randomly sample 1 exit velocity form similar hits
#     EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
#     pred_EV <- hits_at_angle[EV_sample_index,] 
#     # Add that launch speed to vector as the predicted launch speed 
#     EV_vector1 <- c(EV_vector1, pred_EV$launch_speed)
#   }
#   
#   # Merge predicted launch angle and sampled exit velocity
#   modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector1)
#   preds1 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
#   xwOBA1 <- mean(preds1$gam.preds, na.rm = TRUE)
#   
#   # Repeat with a +3 attack angle
#   plus_one_attack <- player_data
#   plus_one_attack$attack_angle <- plus_one_attack$attack_angle + 3
#   
#   pred_angles2 <- tibble(lm.preds = predict(LA_model, newdata = plus_one_attack))
#   pred_angles2 <- pred_angles2 %>% mutate(noise = rnorm(n = length(pred_angles2$lm.preds), mean = 0, 
#                                                         sd = sigma(LA_model)), 
#                                           launch_angle = lm.preds + noise)
#   
#   # Need to sample the data for each predicted angle to find what exit velocity we would give it
#   for(i in 1:length(pred_angles2$launch_angle)){
#     
#     # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with 
#     #the cap (plus or minus)
#     if(pred_angles2$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
#       pred_angles2$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
#     }
#     else if(pred_angles2$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
#       pred_angles2$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
#     }
#     
#     # Filter for the player's launch angles plus or minus 3 degrees above the ACTUAL LA
#     hits_at_angle <- player_data %>% 
#       filter(cleaned_launch_angle <= orig_attack+3 & launch_angle >= 
#                orig_attack-3 & !is.na(launch_speed))
#     # Randomly sample 1 exit velocity form similar hits
#     EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
#     pred_EV <- hits_at_angle[EV_sample_index,] 
#     # Add that launch speed to vector as the predicted launch speed 
#     EV_vector2 <- c(EV_vector2, pred_EV$launch_speed)
#   }
#   
#   modeled_data_plus_one <- tibble(launch_angle = pred_angles2$launch_angle, launch_speed = EV_vector2)
#   preds2 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_plus_one))  
#   xwOBA2 <- mean(preds2$gam.preds, na.rm = TRUE)
#   
#   # Repeat with a -3 attack angle
#   minus_one_attack <- player_data
#   minus_one_attack$attack_angle <- minus_one_attack$attack_angle - 3
#   
#   pred_angles3 <- tibble(lm.preds = predict(LA_model, newdata = minus_one_attack))
#   pred_angles3 <- pred_angles3 %>% mutate(noise = rnorm(n = length(pred_angles3$lm.preds), mean = 0, 
#                                                         sd = sigma(LA_model)), 
#                                           launch_angle = lm.preds + noise)
#   
#   # Need to sample the data for each predicted angle to find what exit velocity we would give it
#   for(i in 1:length(pred_angles3$launch_angle)){
#     
#     # Check all predicted launch angles and if it outside of 2 sd of their mean - replace with 
#     #the cap (plus or minus)
#     if(pred_angles3$launch_angle[i] < (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))){
#       pred_angles3$launch_angle[i] <- (mean(player_data$launch_angle)-2*sd(player_data$launch_angle))
#     }
#     else if(pred_angles3$launch_angle[i] > (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))){
#       pred_angles3$launch_angle[i] <- (mean(player_data$launch_angle)+2*sd(player_data$launch_angle))
#     }
#     
#     # Filter for the player's launch angles plus or minus 3 degrees above the ACTUAL LA
#     hits_at_angle <- player_data %>% 
#       filter(cleaned_launch_angle <= orig_attack+3 & launch_angle >= 
#                orig_attack-3 & !is.na(launch_speed))
#     # Randomly sample 1 exit velocity form similar hits
#     EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
#     pred_EV <- hits_at_angle[EV_sample_index,] 
#     # Add that launch speed to vector as the predicted launch speed 
#     EV_vector3 <- c(EV_vector3, pred_EV$launch_speed)
#   }
#   
#   modeled_data_minus_one <- tibble(launch_angle = pred_angles3$launch_angle, launch_speed = EV_vector3)
#   preds3 <- tibble(gam.preds = predict(woba_model, newdata = modeled_data_minus_one))  
#   xwOBA3 <- mean(preds3$gam.preds, na.rm = TRUE)
#   
#   
#   # print(player_data$attack_angle[1])
#   # print(pred_angles)    
#   # print(modeled_data)
#   # print(min(modeled_data$launch_angle))
#   # print(max(modeled_data$launch_angle))
#   # print(plus_one_attack$attack_angle[1])
#   # print(pred_angles2)
#   # print(modeled_data_plus_one)
#   # print(min(modeled_data_plus_one$launch_angle))
#   # print(max(modeled_data_plus_one$launch_angle))
#   # print(minus_one_attack$attack_angle[1])
#   # print(pred_angles3)
#   # print(modeled_data_minus_one)
#   # print(min(modeled_data_minus_one$launch_angle))
#   # print(max(modeled_data_minus_one$launch_angle))
#   
#   
#   # If original < +1
#   if(xwOBA1 < xwOBA2){
#     # Recursively call with +1 attack angle data
#     predicted_LA_adjust_attack(woba_model, LA_model, plus_one_attack, orig_woba, orig_attack, attack+3)
#   }
#   # Else if original < -1
#   else if (xwOBA1 < xwOBA3){
#     # Recursively call with -1 attack angle data
#     predicted_LA_adjust_attack(woba_model, LA_model, minus_one_attack, orig_woba, orig_attack, attack-3)
#   }
#   # Else
#   else{
#     # Return the orig_woba, xwOBA, orig_attack, and attack angles
#     return (tibble(original_woba = orig_woba, predicted_woba = xwOBA1, original_attack = orig_attack, 
#                    reccomended_attack = attack)[1,])
#   }
#   
# }
# 
# # Function to call adjust attack multiple times ---------------------------
# 
# # repeat_adjust_attack <- function(player_data, player_woba){
# #   final_results <-tibble(predicted_LA_adjust_attack(final_woba_model2, predicted_LA, player_data, player_woba, 
# #                                                     player_data$attack_angle, player_data$attack_angle))
# #   for(i in 1:99){
# #     results <- predicted_LA_adjust_attack(final_woba_model2, predicted_LA, player_data, player_woba, 
# #                                           player_data$attack_angle, player_data$attack_angle)
# #     final_results <- bind_rows(final_results, results)
# #   }
# #   averages <- colMeans(final_results)
# #   final_results <- bind_rows(final_results, averages)
# #   return(tail(final_results))
# # }

# Testing all possible attack angles --------------------------------------

test_all_attack <- function(woba_model, LA_model, player_data, orig_attack){
  
  # Initialize vectors for results
  original_attack <- c(rep(orig_attack[1], times=31))
  original_woba <- c(rep(mean(player_data$woba_value, na.rm = TRUE), times = 31))
  possible_attack_vec <- c(0:30)
  predicted_woba <- c()
  avg_predicted_woba <- c()
  
  for(possible_attack in 0:30){
    # Repeat 10 times
    for(n in 1:10){
      EV_vector4 <- vector()    # To hold launch speeds for this function
      
      # Find the possible launch angle for this attack angle
      player_data$attack_angle <- possible_attack
      pred_angles <- tibble(lm.preds = predict(LA_model, newdata = player_data))
      pred_angles <- pred_angles %>% mutate(noise = rnorm(n = length(pred_angles$lm.preds), mean = 0, 
                                                          sd = sigma(LA_model)), 
                                            launch_angle = lm.preds + noise)
      
      for(i in 1:length(pred_angles$launch_angle)){
        # Sample a launch speed around their actual attack angle
        hits_at_angle <- player_data %>% 
          filter(cleaned_launch_angle <= orig_attack+3 & launch_angle >= 
                   orig_attack-3 & !is.na(launch_speed))
        # Randomly sample 1 exit velocity form similar hits
        EV_sample_index <- sample(1:nrow(hits_at_angle), 1, replace = TRUE)
        pred_EV <- hits_at_angle[EV_sample_index,] 
        # Add that launch speed to vector as the predicted launch speed 
        EV_vector4 <- c(EV_vector4, pred_EV$launch_speed)
      }
      
      # Create modeled data for this attack angle
      modeled_data <- tibble(launch_angle = pred_angles$launch_angle, launch_speed = EV_vector4)
      preds <- tibble(gam.preds = predict(woba_model, newdata = modeled_data))  
      xwOBA <- mean(preds$gam.preds, na.rm = TRUE)
      
      predicted_woba <- c(predicted_woba, xwOBA)
    }
    avg_predicted_woba <- c(avg_predicted_woba, mean(predicted_woba))
  }
  return (tibble(original_attack = original_attack, possible_attack = possible_attack_vec, 
                 original_woba = original_woba, predicted_woba = avg_predicted_woba))
  
}

ui <- fluidPage(selectizeInput(inputId = "player_name", 
                          label = "Name:", 
                          choices = unique(batter_all_2019hp$player_name), 
                          selected = unique(batter_all_2019hp$player_name[1])),
                actionButton(inputId = "get_predictions",
                               label = "Model predicted wOBA!"),
                plotOutput(outputId = "wOBAplot")
)

server <- function(input, output) {
  
  player_attack_angles <- NULL
  
  player_data <- reactive(batter_all_2019hp %>%
  filter(player_name == input$player_name & !is.na(plate_z) & !is.na(launch_angle), !is.na(launch_speed)) %>%
  clean_edges())
  
  player_woba <- reactive(mean(player_data()$woba_value, na.rm = TRUE))
  
  observeEvent(input$get_predictions, {
    player_attack_angles <- test_all_attack(final_woba_model2, predicted_LA, 
                                                   player_data(), player_data()$attack_angle)
  })
  
  output$wOBAplot <- renderPlot({
    base_plot <- ggplot()
    if(!is.null(player_attack_angles)){
      base_plot <- base_plot + geom_line(data=player_attack_angles, 
                                         aes(x = possible_attack, y = predicted_woba))+
        geom_smooth(data=player_attack_angles, 
                    aes(x = possible_attack, y = predicted_woba))
    }
    base_plot
  }
  )
}
shinyApp(ui=ui, server = server)
