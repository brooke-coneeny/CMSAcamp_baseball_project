#EDA for model to predict fair/foul

fair_foul_batted_balls <- batter_all_1621 %>%
  filter(description2 %in% c("foul", "hit_into_play")) %>%
  mutate(fair = case_when(description2 == "hit_into_play" ~ 1, 
                          description2 == "foul" ~ 0))

fair_foul_dataset <- batted_balls %>%
  filter(balls_in_play >= 50) %>%
  left_join(fair_foul_batted_balls, by=c("year", "player_name")) %>%
  left_join(attack_angles, by = c("year", "player_name")) %>%
  select(player_name, year, attack_angle, launch_speed, launch_angle, balls_in_play, pitch_type, 
         woba_value, description, description2, events, balls, strikes, plate_z, fair, plate_x, 
         release_speed, pfx_z, stand, approach_angle) %>%
  filter(pitch_type %!in% c("PO") & !is.na(pitch_type)) %>%
  mutate(pitch_type = case_when(pitch_type %in% c("CH", "EP") ~ "Offspeed", 
                                pitch_type %in% c("CS", "CU", "KC", "KN", "SC", "SL") ~ "Breaking", 
                                pitch_type %in% c("FA", "FO", "FS", "FT", "SI", "FC", "FF") ~ "Fastball", 
                                TRUE ~ pitch_type), 
         fair = as.factor(fair))%>%
  #plate_x = abs(plate_x)) %>%
  filter(plate_z <=5 & plate_z >= -2.5)

#On the fringes is more fouls (makes sense), fastballs seem to have more fouls up top, breaking seem to 
#have more fouls on the botton, and offspeed seem to have more fairs overall
fair_foul_dataset %>% filter(year==2019) %>%
  ggplot(aes(x=plate_x, y=plate_z, color = fair)) +
  geom_point(alpha = .2) + theme_bw() + ggthemes::scale_color_colorblind() +
  facet_wrap(~pitch_type)

#All seem very similar. Counts where the batter is behind seem to have more fouls relative to fairs
fair_foul_dataset %>% filter(year == 2019, balls != 4) %>%
  ggplot(aes(x=fair))+
  geom_bar()+theme_bw()+facet_wrap(~balls + strikes)

#Flat pitches seem to get more fouls (across all attack angles when you condition on contact)
#Flat fastballs have the most distinct chunk of fouls (especially for loftier swings), flat swings on 
#offspeeds seem to do well
fair_foul_dataset %>% filter(year==2019) %>%
  ggplot(aes(x=attack_angle, y=approach_angle, color = fair)) +
  geom_point(alpha=.2) + theme_bw() + ggthemes::scale_color_colorblind() +
  facet_wrap(~pitch_type)

