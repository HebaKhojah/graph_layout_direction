library(tidyverse)
#this script includes: prepared raw data for preprocissing,
# check the passed sumbission
# anonymisation 


#responses were downloaded from Pavloiva and renamed
En_data <- read_csv("alldata/En_data.csv")
Sa_data <- read_csv("alldata/Sa_data.csv")
Sb_data <- read_csv("alldata/Sb_data.csv")
    

# combined all separate files of raw data from Pavlovia into one data frame.
# convert and unified the columns data type 
# due to differences in number of columns and data types
#to bind the rows with the other data frames
En_data <- En_data %>%
  mutate(
    `Participant ID`= as.numeric(`Participant ID`) ,
    session = as.character(session),
    Session = as.character(Session)
  )
Sb_data <- Sb_data %>%
  mutate(
    age_textbox.text = as.numeric(age_textbox.text) ,
    participant= as.character(participant),
    session = as.character(session) , 
    Session = as.character(Session)
  )



#bind all the data frames into one data frame
merged_data <- bind_rows( Sa_data,Sb_data,En_data)

#filter the data frame from data that were tested by researcher
# and another 1 participant were excluded due to the extreme long time to 
# complete the study ( about more than two hours)
raw_data <- merged_data %>%
  mutate( 
    participant = as.numeric(participant))%>%
  filter(!participant %in% c(93043, 750432,61682) & !is.na(participant))



### Anonymisation and check submission

#first, check submissions before annonymise the data.

#Check the accepted submissions and then anonymise the participant id
# Create a new column 'answer_code' based on the responses
raw_data <- raw_data %>%
  mutate(answer_code = case_when(
    resp_slider_ex.response == "Increases" ~ 1,
    resp_slider_ex.response == "Decreases" ~ 2,
    resp_slider_ex.response == "Remains Constant" ~ 3,
    TRUE ~ NA_integer_
  ))

#compare the responses and the correct answers. 
#correct_ans is column in the data represent (1, 2, 3) for correct answers
raw_data <- raw_data %>%
  group_by(participant) %>%
  mutate(
    iScorrect = case_when(
      !is.na(answer_code) & !is.na(correct_ans) ~ answer_code == correct_ans,  
      TRUE ~ NA # Keep NA if either value is missing 
    )) %>%
  ungroup()
# Select rows where nationality is British or Saudi
# and check the correctness for type A trials 
attention_check <- raw_data %>%
  mutate(
    gender = na_if(gender_slider.response, ""), 
    nationality = na_if(nationality_slider.response, ""),
    name = na_if(name_textbox.text, ""),
    email = na_if(email_textbox.text, "")
  ) %>%
  group_by(participant)%>%
  # Fill missing values within each participant group
  fill(nationality, name, email , .direction = "downup") %>%
  filter(type == "A") %>%  # Filter for attention check trials
  dplyr:: select(participant, name, email, iScorrect, nationality) %>%
  group_by(participant) %>%
  summarise(
    name = first(name),       
    email = first(email), 
    nationality = first (nationality),
    total_correct = sum(as.integer(iScorrect), na.rm = TRUE)) %>% 
  arrange(total_correct) %>%
  mutate(submission = case_when(
    total_correct >= 5 ~ "Accepted",
    TRUE ~ "NotAccepted"  
  ))
# Filter only "Accepted" participants
passed <- attention_check %>%
  filter(submission == "Accepted") %>%
  dplyr:: select(participant, submission) 

# Join passed participant data with raw_data
raw_data <- raw_data %>%
  inner_join(passed, by = "participant")

#contact information of passed participants for compensation
passed <- attention_check %>%
  group_by(nationality)%>%
  filter(submission == "Accepted") %>%
  dplyr:: select (nationality, email,  name)

#file for participants compensation for accepted submission
#save outside the project, to be removed later.
write_csv(passed, "Desktop/Compensations.csv")          

#exclude the name and email for anonymisation
raw_data <- raw_data %>%
   select( - name_textbox.text, -email_textbox.text)

# Second, anonymise the participants ids of the accpeted submissions
anonymised <- raw_data %>%
  group_by(participant) %>%
  mutate(PID = cur_group_id())
# empty the participant column
anonymised$participant <- NULL
anonymised <- anonymised %>%
  rename(participant = PID)

write_csv(anonymised, "data/anonymised_data.csv") 

