#### load libraries ####
library(tidyverse)
library(ggrain)

#### import data ####
poi_df <- read.csv("poi_metrics (1).csv")

#### throwing arm distribution ####
poi_df %>% 
  ggplot(aes(x = p_throws)) +
  geom_bar()

#### pitch velocity by throwing arm rain plot ####
poi_df %>% 
  ggplot(aes(x = p_throws, y = pitch_speed_mph, fill = p_throws)) +
  geom_rain(alpha = .5) +
  labs(x = "Throwing Arm",
       y = "Pitch Velocity (mph)",
       title = "Pitch Velo by Throwing Arm") +
  theme_minimal() +
  theme(legend.position = "none")

##### clean & filter data for linear model ####
poi_lm_df <- poi_df %>% 
  drop_na() %>% 
  select("pitch_speed_mph", "max_shoulder_external_rotation", "max_torso_rotational_velo", "max_elbow_flexion")

#### train linear model ####
lm1 <- lm(pitch_speed_mph ~ ., data = poi_lm_df)

summary(lm1)  

#### pivot data for analysis ####
poi_lm_df_long <- poi_lm_df %>% 
  pivot_longer(cols = c("max_shoulder_external_rotation", "max_torso_rotational_velo", "max_elbow_flexion"), 
               names_to = "metric",
               values_to = "value")

#### scatter plots w/ regression lines ####
poi_lm_df_long %>% 
  ggplot(aes(x = value, y = pitch_speed_mph, color = metric)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ metric, scales = "free") +
  labs(x = element_blank(),
       y = "Pitch Velocity (mph)",
       title = "Significant Metrics Affecting Pitch Velocity") +
  theme_minimal() +
  theme(legend.position = "none")
