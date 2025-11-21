library(ggplot2)
library(viridis)

#--- Does game outcome predict state envy? ---

#code game condition
data_complete$game_condition_coded <- ifelse(data_complete$game_condition == "winner", 1, 0)

#state envy composite 
data_complete$state_envy_composite <- rowMeans(
  data_complete[, c("DV2_emotion_envy",
                    "DV2_emotion_envy_2",
                    "DV2_emotion_jealousy",
                    "DV2_emotion_jealousy_2")],
  na.rm = TRUE
)
hist(data_complete$state_envy_composite, main="State Envy Composite", xlab="Envy Score")


summary(lm(data=data_complete, state_envy_composite ~ game_condition_coded))

t.test(x= data_complete$game_condition_coded, y = data_complete$state_envy_composite, 
       alternative = "two.sided", 
       paired = FALSE,
       conf.level = 0.95)



#bar plot with 95% CI
summary_envy <- data_complete %>%
  group_by(game_condition_coded) %>%
  summarise(
    mean_envy = mean(state_envy_composite, na.rm = TRUE),
    se_envy = sd(state_envy_composite, na.rm = TRUE)/sqrt(n())
  )


ggplot(summary_envy, aes(x = factor(game_condition_coded), y = mean_envy)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = mean_envy - se_envy, ymax = mean_envy + se_envy), width = 0.2) +
  labs(
    x = "Game Condition (0 = Loser, 1 = Winner)",
    y = "State Envy Composite",
    title = "Mean Envy by Game Condition"
  ) +
  theme_minimal()



#violin plot
ggplot(data_complete, aes(x = factor(game_condition_coded), y = state_envy_composite)) +
  geom_violin(fill = "lightblue", alpha = 0.5, color = "darkblue") +  # distribution
  geom_jitter(width = 0.1, size = 3, color = "darkred") +             # individual points
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") + # mean
  labs(
    x = "Game Condition (0 = Loser, 1 = Winner)",
    y = "State Envy Composite",
    title = "State Envy by Game Condition"
  ) +
  theme_minimal()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---- corrlation between dispositional envy and state envy ----


#total dispositional envy corr with state envy 
disp_envy_cols <- grep("DV5", names(data_complete), value = TRUE)

data_complete$total_dispositional_envy <- rowMeans(data_complete[, disp_envy_cols], na.rm = TRUE)

cor(data_complete$total_dispositional_envy, data_complete$state_envy_composite)

#type of dispositional envy corr with state envy
disp_envy_a_cols <- grep("Attraction", names(data_complete), value = TRUE)
disp_envy_c_cols <- grep("Competence", names(data_complete), value = TRUE)
disp_envy_w_cols <- grep("Wealth", names(data_complete), value = TRUE)

data_complete$dispositional_envy_a <- rowMeans(data_complete[, disp_envy_a_cols], na.rm = TRUE)
data_complete$dispositional_envy_c <- rowMeans(data_complete[, disp_envy_c_cols], na.rm = TRUE)
data_complete$dispositional_envy_w <- rowMeans(data_complete[, disp_envy_w_cols], na.rm = TRUE)

sapply(data_complete[66:68], function(x) cor(x, data_complete$state_envy_composite, use = "complete.obs"))

#visualizations 
ggplot(data = data_complete, mapping = aes(x = total_dispositional_envy, y=state_envy_composite))+
  geom_point(color="#440154FF", alpha=.5, size = 3) +
  geom_smooth(method = "lm", color = "#31688EFF", se = TRUE) +
  labs(
    x = "Total Dispositional Envy", 
    y = "State Envy", 
    title = "The Relationship Between Dispositional Envy and State Envy") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

data_complete_long <- data_complete %>%
  select(state_envy_composite,
         dispositional_envy_a,
         dispositional_envy_c,
         dispositional_envy_w) %>%
  pivot_longer(
    cols = starts_with("dispositional_envy"),
    names_to = "disp_type",
    values_to = "disp_value"
  ) %>%
  mutate(
    disp_type = recode(disp_type,
                       "dispositional_envy_a" = "Attraction",
                       "dispositional_envy_c" = "Competence",
                       "dispositional_envy_w" = "Wealth")
  )

# Faceted scatter plot
ggplot(data_complete_long, aes(x = disp_value, y = state_envy_composite)) +
  geom_point(color = "#440154FF", alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "#31688EFF", se = TRUE) +
  facet_wrap(~disp_type, scales = "free_y") +
  labs(
    x = "Dispositional Envy",
    y = "State Envy Composite",
    title = "State Envy by Dispositional Envy Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))