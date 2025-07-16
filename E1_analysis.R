library(tidyverse)
library(lme4) 
library(lmerTest)
library(ggplot2)
library(buildmer)
library(emmeans)


tidied_data <- read.csv("data/data_tidy.csv")

#Descriptive analyses and visualisation

tidied_data %>%

#visualisation: 
# Visualise the reaction time in boxplots
tidied_data %>%
  ggplot(aes(x = lang_group, y = reaction_t, fill = direction)) +
  scale_y_continuous(limits = c(0, 50000), breaks = seq(0, 40000, by = 5000)) +
  geom_boxplot(alpha = 0.5, outlier.shape = 16, outlier.size = 1, width = 0.6) +
  labs(
    title = "Reaction Times by Graph Direction and Partcipants' Group",
    x = "",
    y = "Reaction Time (ms)",
    fill = "Graph Direction"
  ) +
  scale_fill_discrete( name = "Graph Direction", labels = c("LTR", "RTL"))+
    theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.key.size = unit(2, "lines") ,
    
    legend.position = "top"
  )

summary_data <- tidied_data %>%
  group_by(lang_group, direction) %>%
  summarise(
    mean_accuracy = mean(accuracy, na.rm = TRUE),
    sd_accuracy = sd(accuracy, na.rm = TRUE),
    se_accuracy = sd(accuracy) / sqrt(n()),
    .groups = 'drop'
  )
# Visualise the accuracy barplot
ggplot(summary_data, aes(x = lang_group, y = mean_accuracy, fill = direction)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7),width = 0.4,alpha = 0.5,  
           color = "black") +
  geom_errorbar(aes(ymin = mean_accuracy - se_accuracy , ymax = mean_accuracy + se_accuracy ),
                position = position_dodge(width = 0.7), width = 0.2) +
  geom_hline(yintercept = 0.94, color = "black", size = 0.3)+
   coord_cartesian(ylim = c(0.95, 1.0) ) +
labs(
    title = "Accuracy by Direction and Participants' Group",
    x = "",
    y = "Mean Accuracy",
    fill = "Graph Direction"
  )+
  scale_fill_discrete( name = "Graph Direction", labels = c("LTR", "RTL"))+
  scale_x_discrete(labels = c ("British Partcipants" , "Saudi Partcipants"))+
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.key.size = unit(2, "lines") ,
    legend.position = "top"
  ) 


tidied_data %>% group_by(lang_group,direction) %>% filter(reaction_t <= 30000 & lang_group =="Saudi" ) %>%
  summarise ( 
    mean = mean(reaction_t ), 
                sd = sd ( reaction_t) )
#Descriptive analysis for accuracy and reaction time
tidied_data %>%
 group_by(lang_group , direction ) %>%
  summarise (rt_mean = mean(reaction_t), 
             rt_sd = sd (reaction_t),
             acc_mean = mean (accuracy),
             acc_sd = sd ( accuracy ),
  ) 
tidied_data %>% 
  group_by(lang_group) %>%
   count (reaction_t >=20000) %>%
mutate(percent = round(n / sum(n) * 100, 1))


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


null_model <- lmer(reaction_t ~ 1+
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
         direction = factor(direction)
       )


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

################################################################################
library(tidyverse)
library(MASS)
library(emmeans)
library(scales)
library(buildmer)
library(lme4)
library(kableExtra)
library(papaja)
library(qwraps2)
library(lmerTest)
library(ggdist)
library(ggpubr)
library(conflicted)
library(ggtext)
install.packages("")
library(r2glmm)
library(grid)
library(DescTools)
library(Matrix)

passed <- read.csv("data/data_tidy.csv")
passed <- passed %>%  
  mutate(
    subject = factor(pid), 
    lang_group = factor(lang_group), 
    item = factor(item),
    direction = factor(direction) 
  )

contrasts(passed$direction) <- matrix(c(-0.5, 0.5))
contrasts(passed$lang_group) <- matrix(c(-0.5, 0.5))
comparison <- function(model) {
  
  parens <- function(x) paste0("(",x,")")
  onlyBars <- function(form) reformulate(sapply(findbars(form),
                                                function(x)  parens(deparse(x))),
                                         response=".")
  onlyBars(formula(model))
  cmpr_model <- update(model,onlyBars(formula(model)))
  
  return(cmpr_model)
  
}
anova_results <- function(model, cmpr_model) {
  
  model_name <- deparse(substitute(model))
  
  if (class(model) == "buildmer") model <- model@model
  if (class(cmpr_model) == "buildmer") cmpr_model <- cmpr_model@model
  
  anova_output <- anova(model, cmpr_model)
  
  assign(paste0(model_name, ".Chisq"),
         anova_output$Chisq[2],
         envir = .GlobalEnv)
  assign(paste0(model_name, ".df"),
         anova_output$Df[2],
         envir = .GlobalEnv)
  assign(paste0(model_name, ".p"),
         anova_output$`Pr(>Chisq)`[2],
         envir = .GlobalEnv)
  
}
contrasts_extract <- function(model) {
  
  model_name <- deparse(substitute(model))
  
  if (class(model) == "buildmer") model <- model@model
  
  EMMs <- emmeans(model, pairwise ~ lang * direction)
  
  contrast_df <- as.data.frame(EMMs[2]) %>%
    rename_with(str_replace,
                pattern = "contrasts.", replacement = "",
                matches("contrasts")) %>%
    rename_with(str_to_title, !starts_with("p")) %>%
    select(c("Contrast", "Z.ratio", "p.value"))
  
  return(contrast_df)
}
make_sig_table <- function(model) {
  if (class(model) == "buildmer") model <- model@model
  
  table_df <- as.data.frame(summary(model)$coefficients) %>%
    dplyr::rename(
      Estimate = Estimate,
      `Standard Error` = `Std. Error`,
      df = df,
      `t-value` = `t value`,
      p = `Pr(>|t|)`
    ) %>%
    dplyr::mutate(p = format.pval(p, digits = 3, eps = .001))
  
  r_squared <- r2beta(model, method = "nsj") %>%
    dplyr::mutate(Effect = dplyr::recode(Effect,
                                         "direction" = "direction",
                                         "lang_group" = "lang_group",
                                         "lang_group:direction" = "lang_group x direction")) %>%
    dplyr::select(Effect, Rsq) %>%
    dplyr::mutate(Rsq = round(Rsq, 3)) %>%
    dplyr::mutate(Rsq = ifelse(dplyr::row_number() == 1, NA, Rsq))
  
  rownames(table_df) <- rownames(table_df)  
  result_table <- cbind(table_df, `R²` = r_squared$Rsq)
  print(result_table)
}
model <- buildmer(reaction_t ~ direction * lang_group +
                    (1 + direction*lang_group | subject) + 
                    (1 + direction*lang_group | item),
                  data = passed)


model <- buildmer(accuracy ~ direction * lang_group +
                    (1 + direction*lang_group | subject) + 
                    (1 + direction*lang_group | item),
                    family = binomial(), 
                    data = passed)
model <- model@model

model_cmpr <- comparison(model)

anova_results(model, model_cmpr)

make_sig_table(model)
