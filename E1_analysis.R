library(tidyverse)
library(lme4) 
library(lmerTest)
library(ggplot2)
#library(performance) for check_,modele
library(buildmer)
library(emmeans)


tidied_data <- read.csv("data/data_tidy.csv")

#Descriptive analyses and visualisation


#visualisation: 
# Visualise the reaction time in boxplots
tidied_data %>%
  filter(reaction_t <100000) %>%  # exclude the extreme outlier for clarity in visuialisation 
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

#Descriptive analysis for accuracy and reaction time
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


# Prepare data for modelling
#transform data to factor
tidied_data <- tidied_data %>%  
  mutate(
    subject = factor(pid), 
    lang_group = factor(lang_group), 
    item = factor(item),
    direction = factor(direction) 
  )

contrasts(tidied_data$direction) <- matrix(c(-0.5, 0.5))
contrasts(tidied_data$lang_group) <- matrix(c(-0.5, 0.5))

# fitting the model with buildmer()
fact_model <-buildmer  (reaction_t ~  direction  * lang_group + 
                          (1 + direction | subject) + 
                          (1  + direction  |  item),
                          buildmerControl = list(include = 'reaction_t ~ direction   * lang_group ' ),
                         data = tidied_data)
fact_model <- fact_model@model

summary( fact_model)


null_model <- lmer(reaction_t ~ 1 +
                    (1 | subject) + 
                     (1  |  item),
                   data = tidied_data)
#runs ANOVA between experimental and null model
anova(null_model, fact_model)


#summaries Accuracy 
tidied_reg_data <- tidied_data %>%
  mutate(subject = factor(pid),
         lang_group = factor (lang_group),
         item = factor(item), 
         direction = factor(direction), 
         accuracy= accuracy)


contrasts(tidied_reg_data$direction) <- matrix(c(.5, -.5))
contrasts(tidied_reg_data$lang_group) <- matrix(c(.5, -.5))


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





