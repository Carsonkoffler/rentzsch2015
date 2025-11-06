library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# Set working directory
path <- "/Users/carsonkoffler/Documents/R_Coding/rentzsch2015/docs/osfstorage-archive"
setwd(path)

process_participant <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Keep only survey trials
  survey_types <- c("survey-likert", "survey-multi-choice", "survey-text")
  df_survey <- df %>% filter(trial_type %in% survey_types)
  
  # Safe JSON parsing
  parse_response <- function(x) {
    if (!is.na(x) && nchar(x) > 0) {
      return(fromJSON(x))
    } else {
      return(list())  # empty list for missing responses
    }
  }
  
  df_survey <- df_survey %>%
    mutate(parsed_response = map(response, parse_response))
  
  # Add trial-specific prefix to each parsed response's names
  df_survey <- df_survey %>%
    mutate(parsed_response = map2(parsed_response, trial_index, ~{
      if(length(.x) > 0) {
        names(.x) <- paste0("trial", .y, "_", names(.x))
      }
      .x
    }))
  
  # Unnest the parsed response into separate columns
  df_expanded <- df_survey %>%
    unnest_wider(parsed_response)
  
  id_cols <- c("subject_id", "participant_name", "random_condition")
  
  df_expanded <- df_expanded %>%
    mutate(across(-all_of(c("subject_id", "participant_name", "random_condition")), as.character))
  
  # Collapse so one row per participant
  df_wide <- df_expanded %>%
    group_by(subject_id) %>%
    summarise(across(everything(), ~ first(.[!is.na(.)])), .groups = "drop") %>%
    select(all_of(id_cols), everything())
  
  return(df_wide)
}

# make combined df
df_all <- map_df(files, process_participant)
df_all$random_condition_coded <- ifelse(df_all$random_condition == "loser",
                                        0,
                                        ifelse(df_all$random_condition == "winner",
                                               1,
                                               NA))
df_all[23:41] <- lapply(df_all[23:41], as.numeric)
df_all$state_envy_composite <- (df_all$trial27_Q1 + df_all$trial27_Q4 + df_all$trial27_Q6 + df_all$trial27_Q8)/4


summary(lm.beta(lm(data = df_all, state_envy_composite ~ random_condition_coded)))

#I can run the data but I need to spend some significant time shaping the data correctly. I did a very rough treatment today.

write.csv(df_all, "all_participants_wide.csv", row.names = FALSE)



