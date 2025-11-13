library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)

# Set working directory
path <- "/Users/carsonkoffler/Documents/R_Coding/rentzsch2015/data/osfstorage"
setwd(path)

files <- list.files(path = path, pattern = "\\.csv$", full.names = T) #list all CSV files in OSF folder

length(files) #number of files 

head(files) #View

#read and combine participant data into one file 
all_data <- files %>%
            map_dfr(function(file_path){
              df <- read.csv(file_path, stringsAsFactors = F)
              df$stage <- "pilotB" #add stage of data collection here 
              df
            })

all_data <- as_tibble(all_data)


#summarize participant data 
participants <- all_data %>%
  group_by(subject_id) %>%
  summarise(game_condition = first(random_condition), 
            total_time = max(time_elapsed, na.rm = T), 
            success = first(success))


# Identify survey rows want to keep 
survey_data <- all_data %>%
  dplyr::filter(str_detect(trial_type, "survey")) %>%
  dplyr::select(subject_id, trial_type, response)

# Convert JSON strings to lists and expand
survey_expanded <- survey_data %>%
  mutate(response_list = map(response, ~ {
    # Convert string JSON to R list
    if (!is.na(.x) && .x != "") {
      as.list(fromJSON(.x))
    } else {
      NA
    }
  })) %>%
  dplyr::select(-response) %>%
  unnest_wider(response_list)

# fix text data that was not named
columns_fixed <- survey_expanded %>%
  group_by(subject_id) %>%
  summarise(
    pseudonym = Q0[!map_lgl(Q0, is.null)][1],
    age = Q0[!map_lgl(Q0, is.null)][2],
    attention_check3 = Q0[!map_lgl(Q0, is.null)][3]
  )

survey_expanded <- survey_expanded %>%
  left_join(columns_fixed, by = "subject_id")

#add attention check 1 that was filtered out earlier
attention_check_1 <- all_data %>%
  filter(str_detect(stimulus, "Who won the last round you played\\?")) %>%
  dplyr::select(subject_id, attention_check_1=response)

survey_expanded <- survey_expanded %>%
  left_join(attention_check_1, by = "subject_id")

# Join expanded survey data back to main dataframe
# If multiple survey rows per subject, we can summarize by taking the first non-NA per column
survey_summary <- survey_expanded %>%
  group_by(subject_id) %>%
  summarise(across(everything(), ~ first(na.omit(.x)), .names = "{.col}"))

survey_summary <- survey_summary %>%
  left_join(participants, by = "subject_id")

### Exclude participants who didn't complete the experiment
data_complete <- survey_summary %>%
  filter(success == "true")

n_total <- survey_summary %>%
  distinct(subject_id) %>%
  nrow()

n_excluded_incomplete <- n_total - n_distinct(data_complete$subject_id)

print(paste("Excluded for incompletion:", n_excluded_incomplete))
print(paste("Remaining participants:", n_distinct(data_complete$subject_id)))

### Exclude participants who failed attention checks

#create pass/fail for attention_check_1
data_complete <- data_complete %>%
  mutate(
    attention_check_1 = case_when(
      (game_condition == "winner" & attention_check_1 == 0) ~ "pass", # 0 if select participant won --> winner = participant win
      (game_condition == "loser" & attention_check_1 == 1) ~ "pass", # 1 if select computer win --> loser = computer win
      TRUE ~ "fail"
    )
  )

#LIKERT SCALES coded 0-6 NOT 1-7 

data_attention <- data_complete %>%
  filter(attention_check_1 == "pass" & DV5_attention_check == 6 & attention_check3 > 1)

n_excluded_attention <- n_distinct(data_complete$subject_id) - 
  n_distinct(data_attention$subject_id)

print(paste("Excluded for failed attention checks:", n_excluded_attention))
print(paste("Remaining participants:", n_distinct(data_attention$subject_id)))

exclusion_summary <- tibble(
  Stage = c("Initial", 
            "After incompletion", 
            "After attention checks"),
  N_Participants = c(n_total,
                     n_distinct(data_complete$subject_id),
                     n_distinct(data_attention$subject_id)),
  N_Excluded = c(0,
                 n_excluded_incomplete,
                 n_excluded_attention)
)

exclusion_summary %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Visualize exclusion cascade
ggplot(exclusion_summary %>% filter(!is.na(Stage)), 
       aes(x = reorder(Stage, -N_Participants), y = N_Participants)) +
  geom_bar(stat = "identity", fill = "#1976D2", alpha = 0.7) +
  geom_text(aes(label = N_Participants), vjust = -0.5) +
  labs(title = "Participant Exclusion Cascade",
       x = "Stage",
       y = "Number of Participants") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#codebook(survey_summary)

