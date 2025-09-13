#### install packages ####
install.packages("tidyverse")
install.packages("ggrain")
install.packages("lmerTest")
install.packages("broom.mixed")
install.packages("ggeffects")
install.packages("plotly")


#### load libraries ####
library(tidyverse)
library(ggrain)
library(lmerTest)
library(broom.mixed)
library(ggeffects)
library(plotly)


setwd("C:\\Users\\jerrel.bushel\\OneDrive - City Football Group\\Documents")


#### import data ####
poi_df <- read.csv("poi_metrics.csv")


#### throwing arm distribution ####
poi_df %>% 
  ggplot(aes(x = p_throws)) +
  geom_bar(aes(fill = p_throws)) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, size = 3.5) +
  labs(x = "Throwing Arm",
       y = "Count",
       title = "Throwing Arm Distribution") +
       theme_minimal() +
  theme(legend.position = "none")


#### pitch velocity by throwing arm rain plot ####
poi_df %>% 
  ggplot(aes(x = p_throws, y = pitch_speed_mph, fill = p_throws)) +
  geom_rain(alpha = .5) +
  labs(x = "Throwing Arm",
       y = "Pitch Velocity (mph)",
       title = "Pitch Velocity by Throwing Arm") +
  theme_minimal() +
  theme(legend.position = "none")


##### clean & filter data for linear model ####
poi_lmm_df <- poi_df %>%
  drop_na() %>%
  rename("Pitcher_ID" = "session") %>%
  select(
    "Pitcher_ID",
    "pitch_speed_mph",
    "max_shoulder_external_rotation",
    "max_torso_rotational_velo",
    "max_elbow_flexion"
    )


#### train model ####
lmm <- lmer(pitch_speed_mph ~
              max_shoulder_external_rotation +
              max_torso_rotational_velo +
              max_elbow_flexion +
              (1 | Pitcher_ID),
            data = poi_lmm_df)
summary(lmm)


#### 95% CI prep for plotting ####
coef_tbl <- tidy(lmm, effects = "fixed", conf.int = TRUE, conf.method = "profile") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      max_shoulder_external_rotation = "Max shoulder external rotation",
      max_torso_rotational_velo      = "Max torso rotational velocity",
      max_elbow_flexion              = "Max elbow flexion",
      ),
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  ) %>%
  arrange(estimate) %>%
  mutate(term = factor(term, levels = term))

xrng <- diff(range(c(coef_tbl$conf.low, coef_tbl$conf.high), na.rm = TRUE))
coef_tbl <- coef_tbl %>%
  mutate(label_x = pmax(estimate, conf.high, na.rm = TRUE) + 0.03 * xrng)


#### Forest Plot ####
ggplot(coef_tbl, aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(x = label_x, y = term, label = sig), hjust = 0, vjust = 0.5) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
  labs(
    title = "Fixed-effects coefficient plot",
    x = "Estimated effect on pitch_speed_mph (95% CI)",
    y = NULL,
    caption = "* p < .05, ** p < .01, *** p < .001"
  ) +
  theme_minimal(base_size = 12)


#### Random-effect BLUPs for Pitcher ID intercepts ####
re_list <- ranef(lmm, condVar = TRUE)
re_sess <- re_list$Pitcher_ID
pv <- attr(re_sess, "postVar")[1, 1, ]

cat_df <- tibble(
  Pitcher_ID = rownames(re_sess),
  blup    = as.numeric(re_sess[, "(Intercept)"]),
  se      = sqrt(as.numeric(pv))
) %>%
  mutate(
    lower = blup - 1.96 * se,
    upper = blup + 1.96 * se
  ) %>%
  arrange(blup) %>%
  mutate(Pitcher_ID = factor(Pitcher_ID, levels = Pitcher_ID))


##### Caterpillar plot of random intercepts ####
ggplot(cat_df, aes(x = blup, y = Pitcher_ID)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Random Intercepts by Pitcher ID",
    x = "Random intercept with 95% CI",
    y = NULL
  ) +
  theme_minimal(base_size = 12)


med_vals <- poi_lmm_df %>%
  summarise(
    max_torso_rotational_velo = median(max_torso_rotational_velo, na.rm = TRUE),
    max_elbow_flexion         = median(max_elbow_flexion, na.rm = TRUE)
  )


pred1 <- ggpredict(
  lmm,
  terms = c("max_shoulder_external_rotation"),
  condition = as.list(med_vals),
  type = "fixed"  # fixed-effects (population-level)
)

ggplot(pred1, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(
    title = "Predicted pitch speed vs. max shoulder external rotation",
    x = "Max shoulder external rotation",
    y = "Predicted Pitch Speed (mph)"
  ) +
  theme_minimal()


pred2 <- ggpredict(
  lmm,
  terms = c("max_torso_rotational_velo"),
  condition = as.list(med_vals),
  type = "fixed"
)

ggplot(pred2, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(linewidth = 1) +
  labs(
    title = "Predicted pitch speed vs. max torso rotational velocity",
    x = "Max Torso Rotational Velocity",
    y = "Predicted Pitch Speed (mph)"
  ) +
  theme_minimal()