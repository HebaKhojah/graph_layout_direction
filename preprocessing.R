# Load required libraries
library(tidyverse)
library(dplyr)
library(MASS) 

anonymised <- read_csv("data/anonymised_data.csv", guess_max = 18000)


wrangle <- function(anonfile) {
  # Extract literacy data and calculate literacy score
  literacy <- anonfile %>%
    filter(!is.na(q1_slider.response)) %>%
    rowwise() %>%
    mutate(
      q3_slider.response = as.integer(gsub("[^0-9]", "", q3_slider.response)),  # Remove non-numeric chars
      literacy = sum(c(q1_slider.response, 
                       q2_slider.response, 
                       q3_slider.response,
                       q4_slider.response, 
                       q5_slider.response), na.rm = TRUE)  # Calculate literacy score
    ) %>%
    dplyr::select("participant", "literacy") 
  
  # Extract demographics
  demographics <- anonfile %>%
    filter(!is.na(gender_slider.response)) %>%
    dplyr::select(c("participant","nationality_slider.response",
                 "age_textbox.text", "gender_slider.response"))
  
  # Select relevant columns and filter only "E" type trials
  processed_data <- anonfile %>%
    group_by(participant)%>%
    mutate(
      spent_time = case_when(
        !is.na(exp_trials.stopped) & !is.na(exp_trials.started) ~ 
          (max(exp_trials.stopped, na.rm = TRUE) - min(exp_trials.started, na.rm = TRUE)) / 60,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select("expName" ,"date", "participant",
            "item_no", "type", "direc","iScorrect", 
            "resp_slider_ex.rt", "spent_time" ) %>% 
      filter(type == "E") %>%
      mutate(
        total_correct = sum(iScorrect, na.rm = TRUE), 
        accuracy_perc = (total_correct / 40) * 100,  # Convert to percentage
        reaction_t = resp_slider_ex.rt * 1000,       # Convert to milliseconds
        accuracy = case_when(
          iScorrect == TRUE  ~ 1,  
          iScorrect == FALSE ~ 0 
        ), 
        item = paste0(item_no, "_", direc) 
        ) %>%
         inner_join(literacy, by = "participant") %>%
         inner_join(demographics, by = "participant") %>%
         rename(
           lang_group = nationality_slider.response,  
            direction = direc,        # graph direction LTR or RTL                
            pid = participant,             
            age = age_textbox.text,                    
            gender = gender_slider.response, 
          ) 
         assign("data_tidy", processed_data, envir = .GlobalEnv)
}
walk(list(anonymised), wrangle)


#extract literacy data
literacy <- function(data) {
   group_literacy <- data_tidy %>%
    distinct(pid, .keep_all = TRUE) %>%
    group_by(lang_group) %>%
    summarise(
      mean_literacy = mean(literacy, na.rm = TRUE),
      sd_literacy = sd(literacy, na.rm = TRUE), n(),
      .groups = "drop") %>%
     ungroup()
    overall_literacy <- data_tidy %>%
      ungroup() %>%
     distinct(pid, .keep_all = TRUE) %>%
     summarise(
       mean_literacy = mean(literacy, na.rm = TRUE),
       sd_literacy = sd(literacy, na.rm = TRUE), n(),
       .groups = "drop") %>%
          mutate(lang_group = "Overall") 
 literacy_summary <- bind_rows(group_literacy, overall_literacy)
  return(literacy_summary)
}
literacy_summary <- literacy(data_tidy)

# extract accuracy data for each group
accuracy <- function(data) {
  group_accuracy <- data_tidy %>%
   distinct(pid, .keep_all = TRUE) %>%
    group_by(lang_group) %>%
    summarise(
      mean_accuracy  = mean(accuracy_perc , na.rm = TRUE),
      sd_accuracy  = sd(accuracy_perc , na.rm = TRUE), n(),
      .groups = "drop") %>%
    ungroup()
  overall_accuracy  <- data_tidy %>%
    ungroup() %>%
    distinct(pid, .keep_all = TRUE) %>%
    summarise(
      mean_accuracy = mean(accuracy_perc , na.rm = TRUE),
      sd_accuracy  = sd(accuracy_perc , na.rm = TRUE), n(),
      .groups = "drop") %>%
          mutate(lang_group = "Overall") 

  accuracy_summary <- bind_rows(group_accuracy, overall_accuracy)
  return(accuracy_summary)
}
accuracy_summary <- accuracy(data_tidy)

reaction <- function(data) {
  group_reaction <- data_tidy %>%
    distinct(pid, .keep_all = TRUE) %>%
    group_by(lang_group) %>%
    #filter(condition == "L") %>%
    summarise(
      mean_reaction  = mean(reaction_t , na.rm = TRUE),
      sd_reaction  = sd(reaction_t , na.rm = TRUE), n(), 
      .groups = "drop") %>%
    ungroup()
  overall_reaction  <- data_tidy %>%
    ungroup() %>%
    #filter(condition == "L") %>%
    distinct(pid, .keep_all = TRUE) %>%
    summarise(
      mean_reaction = mean(reaction_t , na.rm = TRUE), n(),
      sd_reaction = sd(reaction_t, na.rm = TRUE),
      .groups = "drop") %>%
    mutate(lang_group = "Overall") 
  
  reaction_summary <- bind_rows(group_reaction, overall_reaction)
  return(reaction_summary)
}
reaction_summary <- reaction(data_tidy)

reaction <- function(data) {
  # Summarize reaction time by language group and condition
  reaction_summary <- data %>%
    group_by(lang_group, direction) %>%
    summarise(
      mean_reaction = mean(spent_time, na.rm = TRUE),
      sd_reaction = sd(spent_time, na.rm = TRUE),
      count = n(),
      .groups = "drop"
    ) %>%
  ungroup() 
  # Compute the overall reaction time summary
  overall_reaction <- data %>% ungroup() %>%
    summarise(
      mean_reaction = mean(spent_time, na.rm = TRUE),
      sd_reaction = sd(spent_time, na.rm = TRUE),
      count = n()
    ) %>%
    mutate(lang_group = "Overall", direction = "All") # Ensure only one row
  
  # Combine both summaries
  final_reaction_summary <- bind_rows(reaction_summary, overall_reaction)
  
 
  return(final_reaction_summary)
}

# Run the function on your dataset
reaction_summary <- reaction(data_tidy)

# Display the results
reaction_summary

# extract age data
age_summary<- data_tidy %>%
  ungroup()%>%
  distinct(pid, .keep_all = TRUE) %>%  
  summarise(
    mean_age = mean(age, na.rm = TRUE),  
    sd_age = sd(age, na.rm = TRUE), n()
  )


# extract gender
gender <- function(data) {
  # Compute gender distribution for British & Saudi groups
  groups_gender <- data %>%
    distinct(pid, .keep_all = TRUE) %>%  
    group_by(lang_group, gender) %>%
    summarise(percentage = (n() / (nrow(.))) * 100, 
              .groups = "drop") %>%  # Compute within each group
    pivot_wider(names_from = gender, 
                values_from = percentage, values_fill = list(percentage = 0))  # Fill NA with 0
  # Compute overall gender distribution
  overall_gender <- data %>%
    distinct(pid, .keep_all = TRUE) %>%
    group_by(gender) %>%
    summarise(percentage = (n() / nrow(.)) * 100, 
              .groups = "drop") %>%
    pivot_wider(names_from = gender,
                values_from = percentage, values_fill = list(percentage = 0)) %>%
    mutate(lang_group = "Overall")  # Add overall label
  
  # Combine British, Saudi, and Overall into one summary table
  gender_summary <- bind_rows(groups_gender, overall_gender)
  
  return(gender_summary)
}
gender_summary <- gender(data_tidy)

# save the file to be ready for modelling 
write_csv(data_tidy,"data/data_tidy.csv")

# Check for missing values
colSums(is.na(data_tidy))

# View summary statistics
summary(data_tidy)



