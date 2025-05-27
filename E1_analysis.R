library(tidyverse)
library(lme4) 
library(lmerTest)
library(ggplot2)
#library(performance) for check_,modele
library(buildmer)
library(emmeans)


tidied_data <- read.csv("data/data_tidy.csv")
#visualisation
tidied_data %>%
  filter(reaction_t <100000) %>%
  ggplot(aes(x = interaction(lang_group: direction) , y = reaction_t, colour  = direction ) )+
 #geom_violin() +
 # geom_jitter(width = .1, alpha = .2) +
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 50000, by = 10000))+
  geom_boxplot(alpha = .5) +
  #stat_summary( fun.data = mean_cl_boot,color = "black",size = 0.8)+
  labs(x = "language x direction",
       y = "reaction time (ms)",
       labs = ) +
  theme_minimal()



# prepare data for analysis

#transform data to factor
tidied_data <- tidied_data %>%  
  mutate(
    subject = factor(pid), 
    lang_group = factor(lang_group), 
    item = factor(item),
    direction = factor(direction) 
  )


tidied_data %>%
 group_by(lang_group , direction ) %>%
  summarise (rt_mean = mean(reaction_t), 
             rt_sd = sd (reaction_t),
             acc_mean = mean (accuracy),
             acc_sd = sd ( accuracy ),
  ) 
tidied_data %>% 
  group_by(lang_group , direction, accuracy) %>%
    summarise ( n() )


tidied_data %>%
  group_by(lang_group) %>%
  summarise(
    total = n(),
    over_10s = sum(reaction_t <= 20000, na.rm = TRUE),
    percent_over_10s = (over_10s / total) * 100,
    .groups = "drop"
  )

contrasts(tidied_data$direction) <- matrix(c(-0.5, 0.5))
contrasts(tidied_data$lang_group) <- matrix(c(-0.5, 0.5))
levels(tidied_data$direction) #"LTR" is coded as +0.5 RTL" is coded as –0.5
levels(tidied_data$lang_group) #British 0.5, Saudi -0.5


# fitting the model with buildmer()
fact_model <-buildmer  (reaction_t ~  direction  * lang_group + 
                          (1 + direction | subject) + 
                          (1  + direction  |  item),
                          buildmerControl = list(include = 'reaction_t ~ direction   * lang_group ' ),
                        data = tidied_data)
fact_model <- fact_model@model
#fact_model <- update(fact_model, REML = FALSE)
summary( fact_model)


null_model <- lmer(reaction_t ~ 1 +
                    (1 | subject) + 
                     (1  |  item),
                   data = tidied_data)
#runs ANOVA between experimental and null model
anova(null_model, fact_model)

emmeans(fact_model, pairwise ~ lang_group )



#summaries Accuracy 
tidied_reg_data <- tidied_data %>%
  mutate(subject = factor(pid),
         lang_group = factor (lang_group),
         item = factor(item), 
         direction = factor(direction), 
         accuracy= accuracy)


contrasts(tidied_reg_data$direction) <- matrix(c(.5, -.5))
contrasts(tidied_reg_data$lang_group) <- matrix(c(.5, -.5))

tidied_reg_data %>%
  group_by(lang_group, direction) %>%
  summarise (mean_accuracy = mean (accuracy), 
             sd_accuracy = sd( accuracy)
  )
#Accuracy

binomial_model <- buildmer(  accuracy ~ lang_group * direction +
                               (1 | subject) +
                               (1 | item),
                                 data = tidied_reg_data,
                             buildmerControl = list(include = 'accuracy ~ direction   * lang_group ' ),
                               family = binomial()
                               )
binomial_model <- binomial_model@model
summary (binomial_model)
null_binomial_model <- glmer(accuracy  ~ 
                              (1 | subject),
                             data = tidied_reg_data,
                             family = binomial)
anova(binomial_model, null_binomial_model)  
emmeans(binomial_model, pairwise ~ lang_group  )
# GLMM Model for Accuracy, simplified version
formula <- accuracy ~ direction * lang_group + 
  (1 + direction | subject) + 
  (1 + direction * lang_group | item)

# literacy 
fact_lit_model <-buildmer  (reaction_t ~  direction  * lang_group + literacy +
                          (1 + direction  | subject) + 
                          (1  + direction |  item),
                          buildmerControl = list(include = 'reaction_t ~ direction  * lang_group + literacy' ),
                        data = tidied_data)
fact_lit_model <-fact_lit_model <- fact_lit_model@model
summary( fact_lit_model)


null_model <- lmer(reaction_t ~ direction * lang_group +
                     (1 | subject) + 
                     (1 |  item),
                   data = tidied_data)
#runs ANOVA between experimental and null model
anova(null_model, fact_lit_model)

# litearcy in accuracy
formula <- accuracy ~ direction * lang_group + literacy +
  (1 + direction | subject) + 
  (1 + direction  | item)

# Run buildmer for stepwise selection using glmer (GLMM)
result <- buildmer(
  formula = formula,
  data = tidied_data,
  buildmerControl = list(include = 'accuracy ~  literacy' ),
    family = binomial()
)
binomial_model <- result@model
null_binomial_model <- glmer(accuracy  ~ direction * lang_group +
                               (1 | subject) + 
                               ( 1 | item) ,
                             data = tidied_data,
                             family = binomial)



anova(binomial_model, null_binomial_model)  

################################################################

setwd("C:/Users/k81140hk/OneDrive - The University of Manchester/MyDoc/UoM/MyR/Expermint 1/Preparing_data/Preparig_data_Ex1")
print(getwd())

####################################################################


tidied_data <- read.csv("data/data_tidy.csv")

#Summary of reaction time and accuracy
summary(tidied_data$reaction_t)
summary(tidied_data$accuracy_perc)

# visualise the data




tidied_data<- tidied_data %>%
  filter(reaction_t < 20000)
#summarise the means and sd
tidied_reg_data$predicted <- predict(binomial_model, type = "response")
library(tidyr)



##################################################################
British_data <- tidied_data %>%
  filter (lang_group == "British")
dotplot(ranef(British_data, condVar = TRUE))

saudi_data <- tidied_data %>%
  filter (lang_group == "Saudi") 

####################################################################
#paired t-test
#summary of the data ( British) by subject
summary_B <- British_data %>%
  group_by(pid, direction) %>%
  summarise ( B_mean = mean (reaction_t), 
              B_meadian = median(reaction_t),
              B_sd = sd(reaction_t))
#wide format and compute the difference in reaction times
B_wide <- summary_B %>%
  pivot_wider(
    id_cols = pid,
    names_from = direction,
    values_from = B_mean
  ) %>% 
  mutate (difference = L - R)  %>%
  view() 

#visualise British data               
British_data %>%
  ggplot(aes(x = direction , y = reaction_t, fill= direction)) +
  geom_boxplot() +
  theme_minimal() + 
  labs(
    title =  "British participants",
    x = "graph layout",
    y = "Reaction Time (ms)"
  )  

#summary of the data (Saudi)
summary_s <- saudi_data %>%
  group_by(pid, direction) %>%
  summarise ( s_mean = mean (reaction_t), 
              s_meadian = median(reaction_t),
              s_sd = sd(reaction_t)) %>% view()
#wide format and compute the difference in reaction times
s_wide <- summary_s %>%
  pivot_wider(
    id_cols = pid,
    names_from = direction,
    values_from = s_mean
  ) %>% 
  mutate (difference = L - R)  %>%
  view() 


#summary of the data ( British) by item
summary_B <- British_data %>%
  group_by(item_no, direction) %>%
  summarise ( B_mean = mean (reaction_t), 
              B_meadian = median(reaction_t),
              B_sd = sd(reaction_t))

#summary of the data (Saudi) by item
summary_s <- saudi_data %>%
  group_by(item_no, direction) %>%
  summarise ( s_mean = mean (reaction_t), 
              s_meadian = median(reaction_t),
              s_sd = sd(reaction_t)) %>% view()

#applying LMMs on Saudi data
saudi_data <- saudi_data %>%
  mutate(
    subject = factor(pid), 
    item = factor(item),
    direction = factor(direction) 
  )

#simplified version 
saudi_model <- lmer(reaction_t ~  direction + 
                      (1  | subject) + 
                      (1  |  item),
                    data = saudi_data)
summary(saudi_model)
windows()
check_model(saudi_model)

British_data <- British_data %>%
  mutate(
    subject = factor(pid), 
    item = factor(item),
    direction = factor(direction) 
  )
contrasts(British_data$direction) <- matrix(c(.5, -0.5))

#simplified version 
British_model <- lmer(reaction_t ~  direction + 
                        (1  | subject) ,
                      data = British_data)
summary(British_model)
windows()
check_model(British_model)
####################################################################################
saudi_data <- tidied_data %>% 
  filter(lang_group == "Saudi")
#visualise subjects reaction time
saudi_data %>%
  ggplot(aes(x = reaction_t, color = direction, fill = direction)) +
  geom_histogram(aes(y = ..density..),bins = 20, alpha = 0.4, position = "identity") + 
  geom_density(alpha = 0.3, size = 1) +
  labs(x = "RT (ms)", title = "RT by  Saudi pids") +
  theme_minimal() 

British_data %>%
  ggplot(aes(x = reaction_t)) +
  geom_histogram() + 
  labs(x = "RT (ms)", title = "RT by British pids") +
  theme_minimal() +
  facet_wrap(~ pid, scales = "free")
#visualise by items British and Saudis
tidied_data %>%
  filter (item_no <15) %>%
  ggplot(aes(x = reaction_t, color = lang_group, fill = lang_group)) +
  geom_histogram(aes(y = ..density..),bins = 20, alpha = 0.4) + 
  geom_density(alpha = 0.3, size = 1) +
  labs(x = "RT (ms)", title = "RT by items R- L") +
  theme_minimal() +
  facet_wrap(~ item, scales = "free")

#visualise by items L and R
tidied_data %>%
  ggplot(aes(x = reaction_t, color = direction, fill = direction)) +
  geom_histogram(aes(y = ..density..),bins = 20, alpha = 0.4, position = "identity") + 
  geom_density(alpha = 0.3, size = 1) +
  labs(x = "RT (ms)", title = "RT by items R- L") +
  theme_minimal() +
  facet_wrap(~ item_no, scales = "free")

#visualise British data in boxplot L and R
British_data %>%
  ggplot(aes(x = direction , y = reaction_t, fill= direction)) +
  geom_boxplot() +
  theme_minimal() + 
  labs(
    title =  "British participants",
    x = "graph layout",
    y = "Reaction Time (ms)"
  )  
#####################################################################

#######################################################

tidied_data %>% 
  ggplot(aes(x = lang_group, y = reaction_t)) + 
  geom_boxplot(alpha = .5) +
  guides(colour = 'none') +
  labs (
    x = "graph layout direction",
    y = "spent time")

##################################################


##############################################################3

#check missing data
colSums(is.na(tidied_data)) # missing values : age 2 participants
vis_dat(tidied_data)
vis_miss(tidied_data)


tidied_data %>% 
  ggplot(aes(x = lang_group:direction , y = reaction_t, fill = direction  )) + 
   geom_violin() +
   geom_jitter(alpha = .2, width = .1) +
   scale_y_continuous(limits = c(0, 100000)) +
  geom_boxplot(alpha = .5) +
  guides(colour = 'none') +
  labs (
    x = "graph layout direction",
    y = "spent time")


ggplot(tidied_data, aes(x = reaction_t)) + 
  geom_histogram() +
  theme_minimal()



write_csv(data_tidy,"data/data_tidy.csv")

# count number of British data
tidied_data %>% 
  filter ( lang_group == "Saudi" & iScorrect == "FALSE" & direction == "R") %>% 
  
  nrow()

setwd("C:/Users/k81140hk/OneDrive - The University of Manchester/MyDoc/UoM/MyR/Expermint 1/Preparing_data/Preparig_data_Ex1")
setwd("C:/Users/Heba_/OneDrive - The University of Manchester/MyDoc/UoM/MyR/Expermint 1/Preparing_data/Preparig_data_Ex1")

saudi_data %>% 
  filter( pid == 27 ) %>%
  group_by(direction ) %>%
  summarise ( mean_t = mean (reaction_t) , s= sd (reaction_t) )%>%view()
saudi_data %>% 
  filter( pid == 27 ) %>%
  view()
saudi_data %>%
  filter( pid == 27) %>% 
  ggplot(aes(x = direction, y = reaction_t, colour = direction)) +
  geom_violin(width = .5) +
  geom_jitter(width = .1, alpha = .5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  labs(x = "Direction",
       y = "RT (ms.)") +
  guides(colour = "none") +
  coord_flip()



