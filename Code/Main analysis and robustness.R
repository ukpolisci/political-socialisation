#### Set-up ####
# Set working directory
setwd("~/OneDrive - Nexus365/01. Oxford/Thesis/Data analysis/Code")

# Clear environment
rm(list = ls())

# Set seed
set.seed(123)

# Load packages
library(haven)
library(tidyverse)
library(xtable)
library(ggplot2)
library(stargazer)
library(nnet)

# Load data
panel <- read_dta("Cleaned data/panel.dta")

# Convert empty strings to NA_character_ for all character columns
panel <- panel %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# Subset to children aged 16-17 (cross section)
teens_cs <- panel  %>%
  filter(age %in% c(16, 17)) %>% 
  distinct(child_id, .keep_all = T)

# Remove respondents with NA in the pid variable
teens_cs <- teens_cs[!is.na(teens_cs$pid),]

# Add mother and father party identity variables
teens_cs <- teens_cs %>% 
  mutate(mother_pid = case_when(
    parent1_sex == "Female" ~ parent1_pid,
    parent2_sex == "Female" ~ parent2_pid,
    TRUE ~ NA),
    father_pid = case_when(
      parent1_sex == "Male" ~ parent1_pid,
      parent2_sex == "Male" ~ parent2_pid,
      TRUE ~ NA))

#### BASELINE CORRELATION (TWO-PARENT FAMILIES) -------------------------------- ####
##### Multinomial logistic regression general #####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")

# Run regression
twop <- multinom(pid ~ mother_pid + father_pid,
                     data = teens_twop)

table(model.frame(twop)$pid)

# Table
stargazer(twop,
          title = "Correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party", "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 4,578).",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when parents are both Conservative
both_con <- teens_twop[teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "Conservative", ]

pp_both_con <- predict(twop, newdata = both_con, type = "probs", se.fit = T)

pp_both_con_mean <- round(colMeans(pp_both_con,  na.rm = T), 2)

## Pp child party id when one parent is Conservative
one_con <- teens_twop[teens_twop$mother_pid == "Conservative" | teens_twop$father_pid == "Conservative", ]

pp_one_con <- predict(twop, newdata = one_con, type = "probs", se.fit = T)

pp_one_con_mean <- round(colMeans(pp_one_con,  na.rm = T), 2)

## Pp child party id when parents are both Labour
both_lab <- teens_twop[teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "Labour", ]

pp_both_lab <- predict(twop, newdata = both_lab, type = "probs", se.fit = T)

pp_both_lab_mean <- round(colMeans(pp_both_lab,  na.rm = T), 2)

## Pp child party id when one parent is Labour
one_lab <- teens_twop[teens_twop$mother_pid == "Labour" | teens_twop$father_pid == "Labour", ]

pp_one_lab <- predict(twop, newdata = one_lab, type = "probs", se.fit = T)

pp_one_lab_mean <- round(colMeans(pp_one_lab,  na.rm = T), 2)

## Pp child party id when both parents are no party
both_noparty <- teens_twop[(teens_twop$mother_pid == "No party" & teens_twop$father_pid == "No party"), ]

pp_both_noparty <- predict(twop, newdata = both_noparty, type = "probs")

pp_both_noparty_mean <- round(colMeans(pp_both_noparty,  na.rm = T), 2)

#### ROBUSTNESS 
#### Binary logistic regression ####
# Create binary outcome variables
teens_twop$con <- ifelse(teens_twop$pid == "Conservative", 1, 0)
teens_twop$lab <- ifelse(teens_twop$pid == "Labour", 1, 0)

# Re-level explanatory variables
teens_twop$mother_pid <- relevel(teens_twop$mother_pid, ref = "No party")
teens_twop$father_pid <- relevel(teens_twop$father_pid, ref = "No party")

# Regression for Conservative identifiers
con_teens_twop <- glm(con ~ mother_pid + father_pid,
                     data = teens_twop,
                     family = binomial)

# Regression for Labour identifiers
lab_teens_twop <- glm(lab ~ mother_pid + father_pid,
                      data = teens_twop,
                      family = binomial)

# Table
stargazer(con_teens_twop, lab_teens_twop,
          type = "text")

stargazer(con_teens_twop, lab_teens_twop,
          title = "Binary logistic regression predicting child party identification based on parental party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party", "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour"),
          table.placement = "H",
          digits = 2)

#### Logistic regression models: homogeneous versus heterogeneous parents
##### Year fixed effects #####
# Run regression
twop_fixeff <- multinom(pid ~ mother_pid + father_pid + factor(wave),
                 data = teens_twop)

# Table
stargazer(twop_fixeff,
          title = "Correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)


#### H1A STRENGTH OF CUES ------------------------------------------------------ ####
#### Effect of party identification strength (without choice) on socialisation dummy ####
# Refresh two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father party identification strength variables
teens_twop <- teens_twop %>% 
  mutate(mother_pid_strength = case_when(
    parent1_sex == "Female" ~ parent1_pid_strength,
    parent2_sex == "Female" ~ parent2_pid_strength,
    TRUE ~ NA),
    father_pid_strength = case_when(
      parent1_sex == "Male" ~ parent1_pid_strength,
      parent2_sex == "Male" ~ parent2_pid_strength,
      TRUE ~ NA))

# Collapse categories of party identification strength 
teens_twop <- teens_twop %>%
  mutate(mother_pid_strength_cat = case_when(
    mother_pid_strength == 1 ~ "Weak",
    mother_pid_strength == 2 ~ "Strong",
    mother_pid_strength == 3 ~ "Strong",
    TRUE ~ NA)) %>% 
  mutate(father_pid_strength_cat = case_when(
    father_pid_strength == 1 ~ "Weak",
    father_pid_strength == 2 ~ "Strong",
    father_pid_strength == 3 ~ "Strong",
    TRUE ~ NA))

# Code socialisation outcome (dummy)
teens_twop <- teens_twop %>% 
  mutate(soc = case_when(
    pid == mother_pid | pid == father_pid ~ 1,
    pid != mother_pid & pid != father_pid ~ 0,
    TRUE ~ NA))

# Remove NAs from explanatory variables
teens_twop <- teens_twop[!is.na(teens_twop$mother_pid_strength_cat) & 
                           !is.na(teens_twop$father_pid_strength_cat),]

# Re-level explanatory variables
teens_twop$mother_pid_strength_cat <- relevel(factor(teens_twop$mother_pid_strength_cat), ref = "Weak")
teens_twop$father_pid_strength_cat <- relevel(factor(teens_twop$father_pid_strength_cat), ref = "Weak")

# Run binary logistic regression
strength_binary <- glm(soc ~ mother_pid_strength_cat + father_pid_strength_cat,
                data = teens_twop,
                family = binomial)

# Table
stargazer(strength_binary,
          title = "Effect of parental party identification strength on the likelihood of child poltical socialisation.",
          covariate.labels = c("Mother strong party identification", "Father strong party identification", "Constant"),
          dep.var.labels = "Child political socialisation (binary)",
          notes = "Reference category: Weak party identification.",
          table.placement = "H",
          digits = 2)

##### Multinomial logit with collapsed party identification choice and strength #####
# Collapse party identification and party identification strength into single variable
teens_twop <- teens_twop %>%
  mutate(mother_pid_strength_coll = case_when(
    mother_pid == "Conservative" & mother_pid_strength == 1 ~ "Conservative weak",
    mother_pid == "Conservative" & mother_pid_strength == 2 ~ "Conservative strong",
    mother_pid == "Conservative" & mother_pid_strength == 3 ~ "Conservative strong",
    mother_pid == "Labour" & mother_pid_strength == 1 ~ "Labour weak",
    mother_pid == "Labour" & mother_pid_strength == 2 ~ "Labour strong",
    mother_pid == "Labour" & mother_pid_strength == 3 ~ "Labour strong",
    mother_pid == "No party" ~ "Non identifier",
    TRUE ~ NA)) %>% 
  mutate(father_pid_strength_coll = case_when(
    father_pid == "Conservative" & father_pid_strength == 1 ~ "Conservative weak",
    father_pid == "Conservative" & father_pid_strength == 2 ~ "Conservative strong",
    father_pid == "Conservative" & father_pid_strength == 3 ~ "Conservative strong",
    father_pid == "Labour" & father_pid_strength == 1 ~ "Labour weak",
    father_pid == "Labour" & father_pid_strength == 2 ~ "Labour strong",
    father_pid == "Labour" & father_pid_strength == 3 ~ "Labour strong",
    father_pid == "No party" ~ "Non identifier",
    TRUE ~ NA))

# Remove NAs from explanatory variables
teens_twop <- teens_twop[!is.na(teens_twop$mother_pid_strength_coll) & 
                           !is.na(teens_twop$father_pid_strength_coll),]

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid_strength_coll <- relevel(factor(teens_twop$mother_pid_strength_coll), ref = "Non identifier")
teens_twop$father_pid_strength_coll <- relevel(factor(teens_twop$father_pid_strength_coll), ref = "Non identifier")

# Run regression
twop_strength <- multinom(pid ~ mother_pid_strength_coll + father_pid_strength_coll,
                          data = teens_twop)

table(model.frame(twop_strength)$pid)

# Table
stargazer(twop_strength,
          title = "Effect of parental party identification strength on the correlation between parent and child party identification.",
          covariate.labels = c("Mother Conservative weak", "Mother Conservative strong", "Mother Labour weak", "Mother Labour strong", "Father Conservative weak", "Father Conservative strong", "Father Labour weak", "Father Labour strong", "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          order = c(2, 1, 4, 3, 6, 5, 8, 7, 9),
          notes = "Reference category: Non-identifier.",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when both parents are strong Conservative
strong_con_both <- teens_twop[teens_twop$mother_pid_strength_coll == "Conservative strong" & teens_twop$father_pid_strength_coll == "Conservative strong", ]

pp_strong_con_both <- predict(twop_strength, newdata = strong_con_both, type = "probs", se.fit = T)

pp_strong_con_both_mean <- round(colMeans(pp_strong_con_both,  na.rm = T), 2)

## Pp child party id when one parent is a strong Conservative and the other is a weak Conservative
strong_con_one <- teens_twop[(teens_twop$mother_pid_strength_coll == "Conservative strong" & teens_twop$father_pid_strength_coll == "Conservative weak") |
                               (teens_twop$mother_pid_strength_coll == "Conservative weak" & teens_twop$father_pid_strength_coll == "Conservative strong"), ]

pp_strong_con_one <- predict(twop_strength, newdata = strong_con_one, type = "probs", se.fit = T)

pp_strong_con_one_mean <- round(colMeans(pp_strong_con_one,  na.rm = T), 2)

## Pp child party id when parents are weak Conservative
weak_con_both <- teens_twop[teens_twop$mother_pid_strength_coll == "Conservative weak" & teens_twop$father_pid_strength_coll == "Conservative weak", ]

pp_weak_con_both <- predict(twop_strength, newdata = weak_con_both, type = "probs", se.fit = T)

pp_weak_con_both_mean <- round(colMeans(pp_weak_con_both,  na.rm = T), 2)

## Pp child party id when both parents are strong Labour
strong_lab_both <- teens_twop[teens_twop$mother_pid_strength_coll == "Labour strong" & teens_twop$father_pid_strength_coll == "Labour strong", ]

pp_strong_lab_both <- predict(twop_strength, newdata = strong_lab_both, type = "probs", se.fit = T)

pp_strong_lab_both_mean <- round(colMeans(pp_strong_lab_both,  na.rm = T), 2)

## Pp child party id when one parent is a strong Labour and the other is a weak Labour
strong_lab_one <- teens_twop[(teens_twop$mother_pid_strength_coll == "Labour strong" & teens_twop$father_pid_strength_coll == "Labour weak") |
                               (teens_twop$mother_pid_strength_coll == "Labour weak" & teens_twop$father_pid_strength_coll == "Labour strong"), ]

pp_strong_lab_one <- predict(twop_strength, newdata = strong_lab_one, type = "probs", se.fit = T)

pp_strong_lab_one_mean <- round(colMeans(pp_strong_lab_one,  na.rm = T), 2)

## Pp child party id when parents are weak Labour
weak_lab_both <- teens_twop[teens_twop$mother_pid_strength_coll == "Labour weak" & teens_twop$father_pid_strength_coll == "Labour weak", ]

pp_weak_lab_both <- predict(twop_strength, newdata = weak_lab_both, type = "probs", se.fit = T)

pp_weak_lab_both_mean <- round(colMeans(pp_weak_lab_both,  na.rm = T), 2)

## Pp child party id when both parents are no party
noparty_both <- teens_twop[(teens_twop$mother_pid_strength_coll == "Non identifier" & teens_twop$father_pid_strength_coll == "Non identifier"), ]

pp_noparty_both <- predict(twop_strength, newdata = noparty_both, type = "probs")

pp_noparty_both_mean <- round(colMeans(pp_noparty_both,  na.rm = T), 2)

# Create dataset for plotting
plot_twop_strength <- data.frame(
  Parents_Strength = c("Strong-Strong Lab", "Strong-Weak Lab", "Weak-Weak Lab",
                       "Both No Party",
                       "Weak-Weak Con", "Strong-Weak Con", "Strong-Strong Con"),
  Conservative = c(pp_strong_lab_both_mean[2], pp_strong_lab_one_mean[2], pp_weak_lab_both_mean[2],
                   pp_noparty_both_mean[2], 
                   pp_weak_con_both_mean[2], pp_strong_con_one_mean[2], pp_strong_con_both_mean[2]),
  Labour = c(pp_strong_lab_both_mean[3], pp_strong_lab_one_mean[3], pp_weak_lab_both_mean[3],
             pp_noparty_both_mean[3], 
             pp_weak_con_both_mean[3], pp_strong_con_one_mean[3], pp_strong_con_both_mean[3]),
  No_Party = c(pp_strong_lab_both_mean[1], pp_strong_lab_one_mean[1], pp_weak_lab_both_mean[1],
               pp_noparty_both_mean[1], 
               pp_weak_con_both_mean[1], pp_strong_con_one_mean[1], pp_strong_con_both_mean[1]))

# Convert to long format for plotting
plot_twop_strength_long <- plot_twop_strength %>%
  gather(key = "Child_Party", value = "Probability", -Parents_Strength)

# Transform family composition to factor 
plot_twop_strength_long$Parents_Strength <- factor(plot_twop_strength_long$Parents_Strength, 
                                                   levels = c("Strong-Strong Lab", "Strong-Weak Lab", "Weak-Weak Lab", 
                                                              "Both No Party", 
                                                              "Weak-Weak Con", "Strong-Weak Con", "Strong-Strong Con"))

# Create the plot
fig6_final <- ggplot(plot_twop_strength_long, aes(x = Parents_Strength, y = Probability, color = Child_Party, group = Child_Party)) +
  geom_line(linewidth = .5) +
  geom_point(size = 2) +
  labs(x = "Parental party identification choice-strength", 
       y = "Predicted probability",
       color = "Child party id") +
  scale_color_manual(values = c("Conservative" = "blue", 
                                "Labour" = "red", 
                                "No_Party" = "black"),
                     labels = c("Conservative", "Labour", "No Party")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom") 

ggsave("Figures/fig6 final.png", fig6_final, 
       bg = "white",
       height = 6,
       width = 9)

#### H1B CONSISTENCY OF CUES --------------------------------------------------- ####
##### Multinomial logit with interaction for homogeneous vs heterogeneous families #####
# Refresh two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create homogeneous family interaction categories
teens_twop$het <- ifelse(teens_twop$mother_pid != teens_twop$father_pid, "Yes", "No")

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")

# Run regression
twop_het <- multinom(pid ~ mother_pid*het + father_pid*het,
                        data = teens_twop)

table(model.frame(twop_het)$pid)

# Table
stargazer(twop_het,
          title = "Correlation between parental party identification and child party identification in homogeneous versus heterogeneous families.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party", "Heterogeneous family (dummy)", "Mother Conservative * Heterog. family", "Mother Labour * Heterog. family", "Mother Lib Dem * Heterog. family", "Mother Other Party * Heterog. family", "Father Conservative * Heterog. family", "Father Labour * Heterog. family", "Father Lib Dem * Heterog. family", "Father Other Party * Heterog. family",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          order = c(1:4, 6, 3, 6:10),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when both parents are Conservative
both_con <- teens_twop[teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "Conservative", ]

pp_both_con <- predict(twop_het, newdata = both_con, type = "probs", se.fit = T)

pp_both_con_mean <- round(colMeans(pp_both_con,  na.rm = T), 2)

## Pp child party id when one parent is Conservative and one No Party
one_con_one_np <- teens_twop[((teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "No party") |
                                (teens_twop$mother_pid == "No party" & teens_twop$father_pid == "Conservative")), ]

pp_one_con_one_np <- predict(twop_het, newdata = one_con_one_np, type = "probs")

pp_one_con_one_np_mean <- round(colMeans(pp_one_con_one_np,  na.rm = T), 2)

## Pp child party id when one parent is Conservative and one is any other party
one_con_one_ao <- teens_twop[((teens_twop$mother_pid == "Conservative" & teens_twop$father_pid != "Conservative" & teens_twop$father_pid != "No party") |
                                (teens_twop$mother_pid != "Conservative" & teens_twop$mother_pid != "No party" & teens_twop$father_pid == "Conservative")), ]

pp_one_con_one_ao <- predict(twop_het, newdata = one_con_one_ao, type = "probs")

pp_one_con_one_ao_mean <- round(colMeans(pp_one_con_one_ao,  na.rm = T), 2)

## Pp child party id when both parents are Labour
both_lab <- teens_twop[teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "Labour", ]

pp_both_lab <- predict(twop_het, newdata = both_lab, type = "probs")

pp_both_lab_mean <- round(colMeans(pp_both_lab,  na.rm = T), 2)

## Pp child party id when one parent is Conservative and one No Party
one_lab_one_np <- teens_twop[((teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "No party") |
                                (teens_twop$mother_pid == "No party" & teens_twop$father_pid == "Labour")), ]

pp_one_lab_one_np <- predict(twop_het, newdata = one_lab_one_np, type = "probs")

pp_one_lab_one_np_mean <- round(colMeans(pp_one_lab_one_np, na.rm = T), 2)

## Pp child party id when one parent is Conservative and one is any other party
one_lab_one_ao <- teens_twop[((teens_twop$mother_pid == "Labour" & teens_twop$father_pid != "Labour" & teens_twop$father_pid != "No party") |
                                (teens_twop$mother_pid != "Labour" & teens_twop$mother_pid != "No party" & teens_twop$father_pid == "Labour")), ]

pp_one_lab_one_ao <- predict(twop_het, newdata = one_lab_one_ao, type = "probs")

pp_one_lab_one_ao_mean <- round(colMeans(pp_one_lab_one_ao, na.rm = T), 2)

## Pp child party id when both parents are no party
both_noparty <- teens_twop[(teens_twop$mother_pid == "No party" & teens_twop$father_pid == "No party"), ]

pp_both_noparty <- predict(twop_het, newdata = both_noparty, type = "probs")

pp_both_noparty_mean <- round(colMeans(pp_both_noparty,  na.rm = T), 2)

# Create dataset for plotting
plot_twop <- data.frame(
  Parents_Party = c("Lab-Lab", "Lab-Other", 
                    "Lab-None", "None-None", 
                    "Con-None", "Con-Other", 
                    "Con-Con"),
  Conservative = c(pp_both_lab_mean[2], pp_one_lab_one_ao_mean[2], pp_one_lab_one_np_mean[2],
                   pp_both_noparty_mean[2], 
                   pp_one_con_one_np_mean[2], pp_one_con_one_ao_mean[2], pp_both_con_mean[2]),
  Labour = c(pp_both_lab_mean[3], pp_one_lab_one_ao_mean[3], pp_one_lab_one_np_mean[3],
             pp_both_noparty_mean[3], 
             pp_one_con_one_np_mean[3], pp_one_con_one_ao_mean[3], pp_both_con_mean[3]),
  No_Party = c(pp_both_lab_mean[1], pp_one_lab_one_ao_mean[1], pp_one_lab_one_np_mean[1],
               pp_both_noparty_mean[1], 
               pp_one_con_one_np_mean[1], pp_one_con_one_ao_mean[1], pp_both_con_mean[1]))

# Convert to long format for plotting
plot_twop_long <- plot_twop %>%
  gather(key = "Child_Party", value = "Probability", -Parents_Party)

# Transform family composition to factor 
plot_twop_long$Parents_Party <- factor(plot_twop_long$Parents_Party, 
                                       levels = c("Lab-Lab", "Lab-Other", 
                                                  "Lab-None", "None-None", 
                                                  "Con-None", "Con-Other", 
                                                  "Con-Con"))

# Create the plot
fig7_final <- ggplot(plot_twop_long, aes(x = Parents_Party, y = Probability, color = Child_Party, group = Child_Party)) +
  geom_line(size = .5) +
  geom_point(size = 2) +
  labs(x = "Composition of family partisanship", 
       y = "Predicted probability",
       color = "Child party id") +
  scale_color_manual(values = c("Conservative" = "blue", 
                                "Labour" = "red", 
                                "No_Party" = "black"),
                     labels = c("Conservative", "Labour", "No Party")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom") 

ggsave("Figures/fig7 final.png", fig7_final, 
       bg = "white",
       height = 6,
       width = 9)



#### H1C FREQUENCY OF CUES - TWO PARENTS VS ONE PARENT ------------------------- ####
##### Multinomial logit with interaction for single mother vs traditional families #####
# Subset cross-section to two-parent homogeneous families and single mother families
teens_onepm <- teens_cs %>% 
  filter((family_type == "Traditional" & mother_pid == father_pid) |
           family_type == "Single mother")

# Create family interaction categories
teens_onepm$single_mother <- ifelse(teens_onepm$family_type == "Single mother", "Yes", "No")

# Re-level explanatory variables
teens_onepm$pid <- relevel(factor(teens_onepm$pid), ref = "No party")
teens_onepm$mother_pid <- relevel(factor(teens_onepm$mother_pid), ref = "No party")

# Run regression
onep_mother <- multinom(pid ~ mother_pid*single_mother,
                        data = teens_onepm)

table(model.frame(onep_mother)$pid)
Hmisc::describe(pid ~ mother_pid*single_mother,
                data = teens_onepm)
table(teens_onepm$mother_pid, teens_onepm$single_mother)

# Table
stargazer(onep_mother,
          title = "Correlation between mother party identification and child party identification in single mother versus two-parent homogeneous families.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Single mother (dummy)", "Mother Conservative * Single mother", "Mother Labour * Single mother", "Mother Lib Dem * Single mother", "Mother Other Party * Single mother",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when mother is Conservative in two-parent family
mcon_twop <- teens_onepm[teens_onepm$mother_pid == "Conservative" & 
                           teens_onepm$single_mother == "No", ]

pp_mcon_twop <- predict(onep_mother, newdata = mcon_twop, type = "probs", se.fit = T)

pp_mcon_twop_mean <- round(colMeans(pp_mcon_twop,  na.rm = T), 2)

## Pp child party id when mother is Conservative and single
mcon_sing <- teens_onepm[teens_onepm$mother_pid == "Conservative" & 
                           teens_onepm$single_mother == "Yes", ]

pp_mcon_sing <- predict(onep_mother, newdata = mcon_sing, type = "probs", se.fit = T)

pp_mcon_sing_mean <- round(colMeans(pp_mcon_sing,  na.rm = T), 2)

## Pp child party id when mother is Labour in two-parent family
mlab_twop <- teens_onepm[teens_onepm$mother_pid == "Labour" & 
                           teens_onepm$single_mother == "No", ]

pp_mlab_twop <- predict(onep_mother, newdata = mlab_twop, type = "probs", se.fit = T)

pp_mlab_twop_mean <- round(colMeans(pp_mlab_twop,  na.rm = T), 2)

## Pp child party id when mother is Labour and single
mlab_sing <- teens_onepm[teens_onepm$mother_pid == "Labour" & 
                           teens_onepm$single_mother == "Yes", ]

pp_mlab_sing <- predict(onep_mother, newdata = mlab_sing, type = "probs", se.fit = T)

pp_mlab_sing_mean <- round(colMeans(pp_mlab_sing,  na.rm = T), 2)

# Create dataset for plotting
plot_onepm <- data.frame(
  Family = c("Con mother single", "Con mother with Con partner", 
             "Lab mother single", "Lab mother with Lab partner"),
  Conservative = c(pp_mcon_sing_mean[2], pp_mcon_twop_mean[2],
                   pp_mlab_sing_mean[2], pp_mlab_twop_mean[2]),
  Labour = c(pp_mcon_sing_mean[3], pp_mcon_twop_mean[3],
             pp_mlab_sing_mean[3], pp_mlab_twop_mean[3]),
  No_Party = c(pp_mcon_sing_mean[1], pp_mcon_twop_mean[1],
               pp_mlab_sing_mean[1], pp_mlab_twop_mean[1]))

# Convert to long format for plotting
plot_onepm_long <- plot_onepm %>%
  gather(key = "Child_Party", value = "Probability", -Family)

# Transform family composition to factor 
plot_onepm_long$Family <- factor(plot_onepm_long$Family, 
                                 levels = rev(c("Con mother single", 
                                                "Con mother with Con partner", 
                                                "Lab mother single", 
                                                "Lab mother with Lab partner")))

# Define custom colours for each family type
custom_colors <- c(
  "Con mother single" = "lightblue",
  "Con mother with Con partner" = "blue",
  "Lab mother single" = "rosybrown2",
  "Lab mother with Lab partner" = "red")

# Rename no party category
plot_onepm_long <- plot_onepm_long %>%
  mutate(Child_Party = recode(Child_Party, "No_Party" = "No party"))

fig8_final <- ggplot(plot_onepm_long, aes(x = Probability, y = Family, fill = Family)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(x = "Predicted probability of child party identification", 
       y = NULL, 
       fill = "Mother party id and family type") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  facet_wrap(~ Child_Party) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.labels = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.position = "none")

ggsave("Figures/fig8 final.png", fig8_final, 
       bg = "white",
       height = 6,
       width = 9)

##### Multinomial logit with interaction for single father vs traditional families #####
# Subset cross-section to two-parent homogeneous families and single father families
teens_onepf <- teens_cs %>% 
  filter((family_type == "Traditional" & father_pid == mother_pid) |
           family_type == "Single father")

# Create family interaction categories
teens_onepf$single_father <- ifelse(teens_onepf$family_type == "Single father", "Yes", "No")

# Re-level explanatory variables
teens_onepf$pid <- relevel(factor(teens_onepf$pid), ref = "No party")
teens_onepf$father_pid <- relevel(factor(teens_onepf$father_pid), ref = "No party")

# Run regression
onep_father <- multinom(pid ~ father_pid*single_father,
                        data = teens_onepf)

table(model.frame(onep_father)$pid)
Hmisc::describe(pid ~ father_pid*single_father,
                data = teens_onepf)
table(teens_onepf$father_pid, teens_onepf$single_father)

# Table
stargazer(onep_father,
          title = "Correlation between father party identification and child party identification in single father versus two-parent homogeneous families.",
          covariate.labels = c("Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party", "Single father (dummy)", "Father Conservative * Single father", "Father Labour * Single father", "Father Lib Dem * Single father", "Father Other Party * Single father",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when mother is Conservative in two-parent family
fcon_twop <- teens_onepf[teens_onepf$father_pid == "Conservative" & 
                           teens_onepf$single_father == "No", ]

pp_fcon_twop <- predict(onep_father, newdata = fcon_twop, type = "probs", se.fit = T)

pp_fcon_twop_mean <- round(colMeans(pp_fcon_twop,  na.rm = T), 2)

## Pp child party id when mother is Conservative and single
fcon_sing <- teens_onepf[teens_onepf$father_pid == "Conservative" & 
                           teens_onepf$single_father == "Yes", ]

pp_fcon_sing <- predict(onep_father, newdata = fcon_sing, type = "probs", se.fit = T)

pp_fcon_sing_mean <- round(colMeans(pp_fcon_sing,  na.rm = T), 2)

## Pp child party id when mother is Labour in two-parent family
flab_twop <- teens_onepf[teens_onepf$father_pid == "Labour" & 
                           teens_onepf$single_father == "No", ]

pp_flab_twop <- predict(onep_father, newdata = flab_twop, type = "probs", se.fit = T)

pp_flab_twop_mean <- round(colMeans(pp_flab_twop,  na.rm = T), 2)

## Pp child party id when mother is Labour and single
flab_sing <- teens_onepf[teens_onepf$father_pid == "Labour" & 
                           teens_onepf$single_father == "Yes", ]

pp_flab_sing <- predict(onep_father, newdata = flab_sing, type = "probs", se.fit = T)

pp_flab_sing_mean <- round(colMeans(pp_flab_sing,  na.rm = T), 2)

# Create dataset for plotting
plot_onepf <- data.frame(
  Family = c("Con father single", "Con father with Con partner", 
             "Lab father single", "Lab father with Lab partner"),
  Conservative = c(pp_fcon_sing_mean[2], pp_fcon_twop_mean[2],
                   pp_flab_sing_mean[2], pp_flab_twop_mean[2]),
  Labour = c(pp_fcon_sing_mean[3], pp_fcon_twop_mean[3],
             pp_flab_sing_mean[3], pp_flab_twop_mean[3]),
  No_Party = c(pp_fcon_sing_mean[1], pp_fcon_twop_mean[1],
               pp_flab_sing_mean[1], pp_flab_twop_mean[1]))

# Convert to long format for plotting
plot_onepf_long <- plot_onepf %>%
  gather(key = "Child_Party", value = "Probability", -Family)

# Transform family composition to factor 
plot_onepf_long$Family <- factor(plot_onepf_long$Family, 
                                 levels = rev(c("Con father single", 
                                                "Con father with Con partner", 
                                                "Lab father single", 
                                                "Lab father with Lab partner")))

# Define custom colours for each family type
custom_colors <- c(
  "Con father single" = "lightblue",
  "Con father with Con partner" = "blue",
  "Lab father single" = "rosybrown2",
  "Lab father with Lab partner" = "red")

# Rename no party category
plot_onepf_long <- plot_onepf_long %>%
  mutate(Child_Party = recode(Child_Party, "No_Party" = "No party"))

fig9_final <- ggplot(plot_onepf_long, aes(x = Probability, y = Family, fill = Family)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(x = "Predicted probability of child party identification", 
       y = NULL, 
       fill = "Father party id and family type") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  facet_wrap(~ Child_Party) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.labels = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.position = "none")

ggsave("Figures/fig9 final.png", fig9_final, 
       bg = "white",
       height = 6,
       width = 9)

#### H2 POLITICAL INTEREST ----------------------------------------------------- #####
##### Multinomial logit with interaction for political interest (categorical, each parent) ####
# Refresh two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father political interest variables
teens_twop <- teens_twop %>% 
  mutate(mother_pol_interest = case_when(
    parent1_sex == "Female" ~ parent1_pol_interest,
    parent2_sex == "Female" ~ parent2_pol_interest,
    TRUE ~ NA),
    father_pol_interest = case_when(
      parent1_sex == "Male" ~ parent1_pol_interest,
      parent2_sex == "Male" ~ parent2_pol_interest,
      TRUE ~ NA))

# Categorise political interest
teens_twop <- teens_twop %>% 
  mutate(mother_pol_interest_cat = case_when(
    mother_pol_interest == 1 | mother_pol_interest == 2 ~ "Low",
    mother_pol_interest == 3 | mother_pol_interest == 4 ~ "High",
    TRUE ~ NA),
    father_pol_interest_cat = case_when(
      father_pol_interest == 1 | father_pol_interest == 2 ~ "Low",
      father_pol_interest == 3 | father_pol_interest == 4 ~ "High",
      TRUE ~ NA))

# Remove NAs from explanatory variables
teens_twop <- teens_twop[!is.na(teens_twop$mother_pol_interest_cat) & 
                           !is.na(teens_twop$father_pol_interest_cat),]

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$mother_pol_interest_cat <- relevel(factor(teens_twop$mother_pol_interest_cat), ref = "Low")
teens_twop$father_pol_interest_cat <- relevel(factor(teens_twop$father_pol_interest_cat), ref = "Low")

# Run regression with political interest
twop_int <- multinom(pid ~ mother_pid*mother_pol_interest_cat + father_pid*father_pol_interest_cat,
                     data = teens_twop)

table(model.frame(twop_int)$pid)

# Table
stargazer(twop_int,
          title = "Effect of political interest in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Mother interest high",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Father interest high", "Mother Conservative * Mother interest high", "Mother Labour * Mother interest high", "Mother Lib Dem * Mother interest high", "Mother Other party * Mother interest high", "Father Conservative * Father interest high", "Father Labour * Father interest high", "Father Lib Dem * Father interest high", "Father Other party * Father interest high",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 4,041).",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when both parents are Conservative high interest
both_con_high <- teens_twop[teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "Conservative" & teens_twop$mother_pol_interest_cat == "High" & teens_twop$father_pol_interest_cat == "High", ]

pp_both_con_high <- predict(twop_int, newdata = both_con_high, type = "probs", se.fit = T)

pp_both_con_mean_h <- round(colMeans(pp_both_con_high,  na.rm = T), 2)

## Pp child party id when both parents are Conservative low interest
both_con_low <- teens_twop[teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "Conservative" & teens_twop$mother_pol_interest_cat == "Low" & teens_twop$father_pol_interest_cat == "Low", ]

pp_both_con_low <- predict(twop_int, newdata = both_con_low, type = "probs", se.fit = T)

pp_both_con_mean_l <- round(colMeans(pp_both_con_low,  na.rm = T), 2)

## Pp child party id when both parents are Labour high interest
both_lab_high <- teens_twop[teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "Labour" & teens_twop$mother_pol_interest_cat == "High" & teens_twop$father_pol_interest_cat == "High", ]

pp_both_lab_high <- predict(twop_int, newdata = both_lab_high, type = "probs", se.fit = T)

pp_both_lab_mean_h <- round(colMeans(pp_both_lab_high,  na.rm = T), 2)

## Pp child party id when both parents are Labour low interest
both_lab_low <- teens_twop[teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "Labour" & teens_twop$mother_pol_interest_cat == "Low" & teens_twop$father_pol_interest_cat == "Low", ]

pp_both_lab_low <- predict(twop_int, newdata = both_lab_low, type = "probs", se.fit = T)

pp_both_lab_mean_l <- round(colMeans(pp_both_lab_low,  na.rm = T), 2)

# Create dataset for plotting
plot_int <- data.frame(
  Family = c("Con low interest", "Con high interest", 
             "Lab low interest", "Lab high interest"),
  Conservative = c(pp_both_con_mean_l[2], pp_both_con_mean_h[2],
                   pp_both_lab_mean_l[2], pp_both_lab_mean_h[2]),
  Labour = c(pp_both_con_mean_l[3], pp_both_con_mean_h[3],
             pp_both_lab_mean_l[3], pp_both_lab_mean_h[3]),
  No_Party = c(pp_both_con_mean_l[1], pp_both_con_mean_h[1],
               pp_both_lab_mean_l[1], pp_both_lab_mean_h[1]))

# Convert to long format for plotting
plot_int_long <- plot_int %>%
  gather(key = "Child_Party", value = "Probability", -Family)

# Transform family composition to factor 
plot_int_long$Family <- factor(plot_int_long$Family, 
                          levels = rev(c("Con low interest", 
                                         "Con high interest", 
                                         "Lab low interest", 
                                         "Lab high interest")))

# Rename no party category
plot_int_long <- plot_int_long %>%
  mutate(Child_Party = recode(Child_Party, "No_Party" = "No party"))

# Define custom colours for each family type
custom_colors <- c(
  "Con low interest" = "lightblue",
  "Con high interest" = "blue",
  "Lab low interest" = "rosybrown2",
  "Lab high interest" = "red")

# Plot
fig10_final <- ggplot(plot_int_long, aes(x = Probability, y = Family, fill = Family)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(x = "Predicted probability of child party identification", 
       y = NULL, 
       fill = "Family partisanship and political interest") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  facet_wrap(~ Child_Party) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.labels = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.position = "none")

ggsave("Figures/fig10 final.png", fig10_final, 
       bg = "white",
       height = 6,
       width = 9)

#### H3 CONTACT ---------------------------------------------------------------- ####
##### Multinomial logit eat with child #####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father job hours variables
teens_twop <- teens_twop %>% 
  mutate(mother_ch_eat = case_when(
    parent1_sex == "Female" ~ parent1_ch_eat,
    parent2_sex == "Female" ~ parent2_ch_eat,
    TRUE ~ NA),
    father_ch_eat = case_when(
      parent1_sex == "Male" ~ parent1_ch_eat,
      parent2_sex == "Male" ~ parent2_ch_eat,
      TRUE ~ NA))

# Collapse into yes or no
teens_twop <- teens_twop %>% 
  mutate(mother_ch_eat_coll = case_when(
    mother_ch_eat == "None" ~ "Rarely",
    mother_ch_eat == "1-2 times a week" ~ "Rarely",
    mother_ch_eat == "3-5 times a week" ~ "Rarely",
    mother_ch_eat == "6-7 times a week" ~ "Often",
    TRUE ~ NA),
    father_ch_eat_coll = case_when(
      father_ch_eat == "None" ~ "Rarely",
      father_ch_eat == "1-2 times a week" ~ "Rarely",
      father_ch_eat == "3-5 times a week" ~ "Rarely",
      father_ch_eat == "6-7 times a week" ~ "Often",
      TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$mother_ch_eat_coll <- relevel(factor(teens_twop$mother_ch_eat_coll), ref = "Rarely")
teens_twop$father_ch_eat_coll <- relevel(factor(teens_twop$father_ch_eat_coll), ref = "Rarely")

# Run regression
twop_eat <- multinom(pid ~ mother_pid*mother_ch_eat_coll + father_pid*father_ch_eat_coll,
                     data = teens_twop)

table(model.frame(twop_eat)$pid)

# Table
stargazer(twop_eat,
          title = "Effect of contact in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Mother eat with child often",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Father eat with child often", "Mother Conservative * Mother eat often", "Mother Labour * Mother eat often", "Mother Lib Dem * Mother eat often", "Mother Other party * Mother eat often", "Father Conservative * Father eat often", "Father Labour * Father eat often", "Father Lib Dem * Father eat often", "Father Other party * Father eat often",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 1,781).",
          table.placement = "H",
          digits = 2)

##### Predicted probabilities #####
## Pp child party id when both parents are Conservative high contact
both_con_high <- teens_twop[teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "Conservative" & teens_twop$mother_ch_eat_coll == "Often" & teens_twop$father_ch_eat_coll == "Often", ]

pp_both_con_high <- predict(twop_eat, newdata = both_con_high, type = "probs", se.fit = T)

pp_both_con_mean_h <- round(colMeans(pp_both_con_high,  na.rm = T), 2)

## Pp child party id when both parents are Conservative low contact
both_con_low <- teens_twop[teens_twop$mother_pid == "Conservative" & teens_twop$father_pid == "Conservative" & teens_twop$mother_ch_eat_coll == "Rarely" & teens_twop$father_ch_eat_coll == "Rarely", ]

pp_both_con_low <- predict(twop_eat, newdata = both_con_low, type = "probs", se.fit = T)

pp_both_con_mean_l <- round(colMeans(pp_both_con_low,  na.rm = T), 2)

## Pp child party id when both parents are Labour high contact
both_lab_high <- teens_twop[teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "Labour" & teens_twop$mother_ch_eat_coll == "Often" & teens_twop$father_ch_eat_coll == "Often", ]

pp_both_lab_high <- predict(twop_eat, newdata = both_lab_high, type = "probs", se.fit = T)

pp_both_lab_mean_h <- round(colMeans(pp_both_lab_high,  na.rm = T), 2)

## Pp child party id when both parents are Labour low interest
both_lab_low <- teens_twop[teens_twop$mother_pid == "Labour" & teens_twop$father_pid == "Labour" & teens_twop$mother_ch_eat_coll == "Rarely" & teens_twop$father_ch_eat_coll == "Rarely",  ]

pp_both_lab_low <- predict(twop_eat, newdata = both_lab_low, type = "probs", se.fit = T)

pp_both_lab_mean_l <- round(colMeans(pp_both_lab_low,  na.rm = T), 2)

# Create dataset for plotting
plot_int <- data.frame(
  Family = c("Con low contact", "Con high contact", 
             "Lab low contact", "Lab high contact"),
  Conservative = c(pp_both_con_mean_l[2], pp_both_con_mean_h[2],
                   pp_both_lab_mean_l[2], pp_both_lab_mean_h[2]),
  Labour = c(pp_both_con_mean_l[3], pp_both_con_mean_h[3],
             pp_both_lab_mean_l[3], pp_both_lab_mean_h[3]),
  No_Party = c(pp_both_con_mean_l[1], pp_both_con_mean_h[1],
               pp_both_lab_mean_l[1], pp_both_lab_mean_h[1]))

# Convert to long format for plotting
plot_int_long <- plot_int %>%
  gather(key = "Child_Party", value = "Probability", -Family)

# Transform family composition to factor 
plot_int_long$Family <- factor(plot_int_long$Family, 
                               levels = rev(c("Con low contact", 
                                              "Con high contact", 
                                              "Lab low contact", 
                                              "Lab high contact")))

# Rename no party category
plot_int_long <- plot_int_long %>%
  mutate(Child_Party = recode(Child_Party, "No_Party" = "No party"))

# Define custom colours for each family type
custom_colors <- c(
  "Con low contact" = "lightblue",
  "Con high contact" = "blue",
  "Lab low contact" = "rosybrown2",
  "Lab high contact" = "red")

# Plot
fig11_final <- ggplot(plot_int_long, aes(x = Probability, y = Family, fill = Family)) +
  geom_col(position = "dodge", width = 0.5) +
  labs(x = "Predicted probability of child party identification", 
       y = NULL, 
       fill = "Family partisanship and frequency of contact with the child") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  facet_wrap(~ Child_Party) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.labels = element_text(size = 10),
        strip.text = element_text(size = 10),
        legend.position = "none")

ggsave("Figures/fig11 final.png", fig11_final, 
       bg = "white",
       height = 6,
       width = 9)


#### ROBUSTNESS H3 ------------------------------------------------------------- ####
##### Multinomial logit for job hours ####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father job hours variables
teens_twop <- teens_twop %>% 
  mutate(mother_job_hours = case_when(
    parent1_sex == "Female" ~ parent1_job_hours,
    parent2_sex == "Female" ~ parent2_job_hours,
    TRUE ~ NA),
    father_job_hours = case_when(
      parent1_sex == "Male" ~ parent1_job_hours,
      parent2_sex == "Male" ~ parent2_job_hours,
      TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")

# Run regression
twop_jbhrs <- multinom(pid ~ mother_pid*mother_job_hours + father_pid*father_job_hours,
                 data = teens_twop)

# Table
stargazer(twop_jbhrs,
          title = "Effect of amount of hours worked (parents) in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Mother job hours",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Father eat with child often", "Mother Conservative * Mother job hours", "Mother Labour * Mother job hours", "Mother Lib Dem * Mother job hours", "Mother Other party * Mother job hours", "Father Conservative * Father job hours", "Father Labour * Father job hours", "Father Lib Dem * Father job hours", "Father Other party * Father job hours",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

##### Multinomial logit for part-time job ####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father job hours variables
teens_twop <- teens_twop %>% 
  mutate(mother_job_full = case_when(
    parent1_sex == "Female" ~ parent1_job_full,
    parent2_sex == "Female" ~ parent2_job_full,
    TRUE ~ NA),
    father_job_full = case_when(
      parent1_sex == "Male" ~ parent1_job_full,
      parent2_sex == "Male" ~ parent2_job_full,
      TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")

# Run regression
twop_jbfull <- multinom(pid ~ mother_pid*mother_job_full + father_pid*father_job_full,
                       data = teens_twop)

# Table
stargazer(twop_jbfull,
          title = "Effect of full time job (parents) in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Mother part-time job",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Father part-time job", "Mother Conservative * Mother part-time job", "Mother Labour * Mother part-time job", "Mother Lib Dem * Mother part-time job", "Mother Other party * Mother part-time job", "Father Conservative * Father part-time job", "Father Labour * Father part-time job", "Father Lib Dem * Father part-time job", "Father Other party * Father part-time job",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

##### Multinomial logit for work weekend ####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father job hours variables
teens_twop <- teens_twop %>% 
  mutate(mother_work_wknd = case_when(
    parent1_sex == "Female" ~ parent1_work_wknd,
    parent2_sex == "Female" ~ parent2_work_wknd,
    TRUE ~ NA),
    father_work_wknd = case_when(
      parent1_sex == "Male" ~ parent1_work_wknd,
      parent2_sex == "Male" ~ parent2_work_wknd,
      TRUE ~ NA))

# Collapse into yes or no
teens_twop <- teens_twop %>% 
  mutate(mother_work_wknd_cat = case_when(
    mother_work_wknd == "Never" ~ "No",
    mother_work_wknd == "Sometimes" ~ "Yes",
    mother_work_wknd == "Often" ~ "Yes",
    TRUE ~ NA),
    father_work_wknd_cat = case_when(
      father_work_wknd == "Never" ~ "No",
      father_work_wknd == "Sometimes" ~ "Yes",
      father_work_wknd == "Often" ~ "Yes",
      TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$mother_work_wknd_cat <- relevel(factor(teens_twop$mother_work_wknd_cat), ref = "No")
teens_twop$father_work_wknd_cat <- relevel(factor(teens_twop$father_work_wknd_cat), ref = "No")

# Run regression
twop_workwknd <- multinom(pid ~ mother_pid*mother_work_wknd_cat + father_pid*father_work_wknd_cat,
                        data = teens_twop)

stargazer(twop_workwknd,
          type = "text")

# Table
stargazer(twop_workwknd,
          title = "Effect of working during the weekend (parents) in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Mother works weekend",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Father works weekend", "Mother Conservative * Mother works weekend", "Mother Labour * Mother part-time job", "Mother Lib Dem * Mother works weekend", "Mother Other party * Mother works weekend", "Father Conservative * Father works weekend", "Father Labour * Father works weekend", "Father Lib Dem * Father works weekend", "Father Other party * Father works weekend",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

##### Multinomial logit leisure time #####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create mother and father leisure time variables
teens_twop <- teens_twop %>% 
  mutate(mother_ch_leisure = case_when(
    parent1_sex == "Female" ~ parent1_ch_leisure,
    parent2_sex == "Female" ~ parent2_ch_leisure,
    TRUE ~ NA),
    father_ch_leisure = case_when(
      parent1_sex == "Male" ~ parent1_ch_leisure,
      parent2_sex == "Male" ~ parent2_ch_leisure,
      TRUE ~ NA))

# Collapse into yes or no
teens_twop <- teens_twop %>% 
  mutate(mother_ch_leisure_coll = case_when(
    mother_ch_leisure == "Almost every day" ~ "Often",
    mother_ch_leisure == "Several times a week" ~ "Often",
    mother_ch_leisure == "Once a week" ~ "Often",
    mother_ch_leisure == "Several times a month" ~ "Not often",
    mother_ch_leisure == "Once a month or less" ~ "Not often",
    mother_ch_leisure == "Never or rarely" ~ "Not often",
    TRUE ~ NA),
    father_ch_leisure_coll = case_when(
      father_ch_leisure == "Almost every day" ~ "Often",
      father_ch_leisure == "Several times a week" ~ "Often",
      father_ch_leisure == "Once a week" ~ "Often",
      father_ch_leisure == "Several times a month" ~ "Not often",
      father_ch_leisure == "Once a month or less" ~ "Not often",
      father_ch_leisure == "Never or rarely" ~ "Not often",
      TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$mother_ch_leisure_coll <- relevel(factor(teens_twop$mother_ch_leisure_coll), ref = "Not often")
teens_twop$father_ch_leisure_coll <- relevel(factor(teens_twop$father_ch_leisure_coll), ref = "Not often")

# Run regression
twop_leisure <- multinom(pid ~ mother_pid*mother_ch_leisure_coll + father_pid*father_ch_leisure_coll,
                       data = teens_twop)

# Table
stargazer(twop_leisure,
          title = "Effect of working during the weekend (parents) in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Mother leisure time often",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Father leisure time often", "Mother Conservative * Mother leisure time often", "Mother Labour * Mother leisure time often", "Mother Lib Dem * Mother leisure time often", "Mother Other party * Mother leisure time often", "Father Conservative * Father leisure time often", "Father Labour * Father leisure time often", "Father Lib Dem * Father leisure time often", "Father Other party * Father leisure time often",  "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party.",
          table.placement = "H",
          digits = 2)

#### H4 MODERATING EFFECT OF RELATIONSHIP QAULITY ------------------------------ ####
##### Multinomial logit for turn to #####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Collapse into yes or no
teens_twop <- teens_twop %>% 
  mutate(turn_to_coll = case_when(
    turn_to == "Another non-resident relative" ~ "Other",
    turn_to == "Another resident relative" ~ "Other",
    turn_to == "Father / step father" ~ "Other",
    turn_to == "Mother / step mother" ~ "Mother",
    turn_to == "Non family member" ~ "Other",
    turn_to == "Sibling" ~ "Other",
    TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$turn_to_coll <- relevel(factor(teens_twop$turn_to_coll), ref = "Other")

# Run regression
twop_turnto <- multinom(pid ~ mother_pid*turn_to_coll + father_pid*turn_to_coll,
                       data = teens_twop)

table(model.frame(twop_turnto)$pid)

# Table
stargazer(twop_turnto,
          title = "Effect of quality of parent-child relationship in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Turn to mother", "Mother Conservative * Turn to mother", "Mother Labour * Turn to mother", "Mother Lib Dem * Turn to mother", "Mother Other party * Turn to mother", "Father Conservative * Turn to mother", "Father Labour * Turn to mother", "Father Lib Dem * Turn to mother", "Father Other party * Turn to mother",  "Constant"),
          order = c(1:4, 6:9, 5, 10:18),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 1,781).",
          table.placement = "H",
          digits = 2)

#### ROBUSTNESS H4  ------------------------------------------------------------- ####
##### Talk to mother #####
# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Collapse into yes or no
teens_twop <- teens_twop %>% 
  mutate(turn_to_coll = case_when(
    turn_to == "Another non-resident relative" ~ "Other",
    turn_to == "Another resident relative" ~ "Other",
    turn_to == "Father / step father" ~ "Other",
    turn_to == "Mother / step mother" ~ "Mother",
    turn_to == "Non family member" ~ "Other",
    turn_to == "Sibling" ~ "Other",
    TRUE ~ NA))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$turn_to_coll <- relevel(factor(teens_twop$turn_to_coll), ref = "Other")

# Run regression
twop_turnto <- multinom(pid ~ mother_pid*turn_to_coll + father_pid*turn_to_coll,
                        data = teens_twop)

table(model.frame(twop_turnto)$pid)

# Table
stargazer(twop_turnto,
          title = "Effect of quality of parent-child relationship in moderating the correlation between parental party identification and child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Turn to mother", "Mother Conservative * Turn to mother", "Mother Labour * Turn to mother", "Mother Lib Dem * Turn to mother", "Mother Other party * Turn to mother", "Father Conservative * Turn to mother", "Father Labour * Turn to mother", "Father Lib Dem * Turn to mother", "Father Other party * Turn to mother",  "Constant"),
          order = c(1:4, 6:9, 5, 10:18),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 1,781).",
          table.placement = "H",
          digits = 2)


#### POLITICAL ECONOMY EXPLANATION --------------------------------------------- ####
#### Create panel of household variables ####
# # Load all BHPS datasets
# ba_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/ba_hhresp.dta")
# bb_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bb_hhresp.dta")
# bc_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bc_hhresp.dta")
# bd_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bd_hhresp.dta")
# be_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/be_hhresp.dta")
# bf_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bf_hhresp.dta")
# bg_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bg_hhresp.dta")
# bh_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bh_hhresp.dta")
# bi_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bi_hhresp.dta")
# bj_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bj_hhresp.dta")
# bk_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bk_hhresp.dta")
# bl_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bl_hhresp.dta")
# bm_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bm_hhresp.dta")
# bn_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bn_hhresp.dta")
# bo_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bo_hhresp.dta")
# bp_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bp_hhresp.dta")
# bq_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bq_hhresp.dta")
# br_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/br_hhresp.dta")
# 
# # Create wave identifiers for BHPS
# waves <- c("ba", "bb", "bc", "bd", "be", "bf", "bg", "bh", "bi", "bj", "bk", 
#            "bl", "bm", "bn", "bo", "bp", "bq", "br")
# 
# # Create list of BHPS datasets
# bhps_datasets <- list(
#   ba_hhresp, bb_hhresp, bc_hhresp, bd_hhresp, be_hhresp,
#   bf_hhresp, bg_hhresp, bh_hhresp, bi_hhresp, bj_hhresp,
#   bk_hhresp, bl_hhresp, bm_hhresp, bn_hhresp, bo_hhresp,
#   bp_hhresp, bq_hhresp, br_hhresp)
# 
# # Save BHPS dataset names as strings
# bhps_strings <- c(
#   "ba_hhresp", "bb_hhresp", "bc_hhresp", "bd_hhresp", "be_hhresp",
#   "bf_hhresp", "bg_hhresp", "bh_hhresp", "bi_hhresp", "bj_hhresp",
#   "bk_hhresp", "bl_hhresp", "bm_hhresp", "bn_hhresp", "bo_hhresp",
#   "bp_hhresp", "bq_hhresp", "br_hhresp")
# 
# # Add wave variable to each BHPS dataset
# for (i in seq_along(bhps_datasets)) {
#   bhps_datasets[[i]]$wave <- waves[i]
#   assign(bhps_strings[i], bhps_datasets[[i]])
# }
# 
# # Define function to remove wave prefixes from variable names
# remove_wave_prefixes <- function(data, waves) {
#   pattern <- paste0("^(", paste(waves, collapse = "|"), ")_")
#   new_names <- gsub(pattern, "", names(data))
#   colnames(data) <- new_names
#   return(data)
# }
# 
# # Apply the function to each BHPS dataset
# bhps_datasets_clean <- lapply(bhps_datasets, remove_wave_prefixes, waves)
# 
# # Combine all UKHLS datasets into one
# bhps <- bind_rows(bhps_datasets_clean)
# 
# # Load all UKHLS datasets
# a_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/a_hhresp.dta")
# b_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/b_hhresp.dta")
# c_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/c_hhresp.dta")
# d_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/d_hhresp.dta")
# e_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/e_hhresp.dta")
# f_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/f_hhresp.dta")
# g_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/g_hhresp.dta")
# h_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/h_hhresp.dta")
# i_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/i_hhresp.dta")
# j_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/j_hhresp.dta")
# k_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/k_hhresp.dta")
# l_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/l_hhresp.dta")
# m_hhresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/m_hhresp.dta")
# 
# # Save UKHLS dataset names as strings
# waves <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
# 
# # Create list of UKHLS datasets
# ukhls_datasets <- list(
#   a_hhresp, b_hhresp, c_hhresp, d_hhresp, e_hhresp, f_hhresp, g_hhresp, 
#   h_hhresp, i_hhresp, j_hhresp, k_hhresp, l_hhresp,bm_hhresp)
# 
# # UKHLS dataset names as strings
# ukhls_strings <- c(
#   "a_hhresp", "b_hhresp", "c_hhresp", "d_hhresp", "e_hhresp", "f_hhresp", 
#   "g_hhresp", "h_hhresp", "i_hhresp", "j_hhresp", "k_hhresp", "l_hhresp", "m_hhresp")
# 
# # Add wave variable to each UKHLS dataset
# for (i in seq_along(ukhls_datasets)) {
#   ukhls_datasets[[i]]$wave <- waves[i]
#   assign(ukhls_strings[i], ukhls_datasets[[i]])
# }
# 
# # Apply the function to each UKHLS dataset
# ukhls_datasets_clean <- lapply(ukhls_datasets, remove_wave_prefixes, waves)
# 
# # Combine all UKHLS datasets into one
# ukhls <- bind_rows(ukhls_datasets_clean)
# 
# # Create full panel
# hh_panel <- bind_rows(bhps, ukhls)
# 
# 
# # Save household panel
# write_dta(hh_panel, "hh_panel.dta")

#### Add household-level variables to the panel ####
# Load datasets
hh_panel_raw <- read_dta("Cleaned data/hh_panel.dta")

# Rename gross household monthly income
hh_panel <- hh_panel_raw %>%
  rename(hh_net = fihhmnnet1_dv,
         hh_gross = fihhmngrs_dv)

# Extract relevant variables
hh_panel <- hh_panel %>% 
  select(hidp, wave, hh_net, hh_gross)

# Identify house-wave combinations
hh_panel <- hh_panel %>% 
  mutate(wave = case_when(
    wave == "ba" ~ 1991,
    wave == "bb" ~ 1992,
    wave == "bc" ~ 1993,
    wave == "bd" ~ 1994,
    wave == "be" ~ 1995,
    wave == "bf" ~ 1996,
    wave == "bg" ~ 1997,
    wave == "bh" ~ 1998,
    wave == "bi" ~ 1999,
    wave == "bj" ~ 2000,
    wave == "bk" ~ 2001,
    wave == "bl" ~ 2002,
    wave == "bm" ~ 2003,
    wave == "bn" ~ 2004,
    wave == "bo" ~ 2005,
    wave == "bp" ~ 2006,
    wave == "bq" ~ 2007,
    wave == "br" ~ 2008,
    wave == "a" ~ 2009,
    wave == "b" ~ 2010,
    wave == "c" ~ 2011,
    wave == "d" ~ 2012,
    wave == "e" ~ 2013,
    wave == "f" ~ 2014,
    wave == "g" ~ 2015,
    wave == "h" ~ 2016,
    wave == "i" ~ 2017,
    wave == "j" ~ 2018,
    wave == "k" ~ 2019,
    wave == "l" ~ 2020,
    wave == "m" ~ 2021,
    TRUE  ~  NA))

# Combine hidp and wave
hh_panel$house_wave <- paste(hh_panel$hidp, hh_panel$wave, sep = "_")

# Create hidp_wave identifier in the panel
panel$house_wave <- paste(panel$hidp, panel$wave, sep = "_")

# Identify house_wave combinations that are present in the panel
hh_panel <- hh_panel %>% 
  filter(house_wave %in% panel$house_wave)

# Add house_wave income observations to the panel
panel <- panel %>%
  left_join(hh_panel %>% 
              select(house_wave, hh_net, hh_gross), by = "house_wave")

# Subset to children aged 16-17 (cross section)
teens_cs <- panel  %>%
  filter(age %in% c(16, 17)) %>% 
  distinct(child_id, .keep_all = T)

# Remove respondents with NA in the pid variable
teens_cs <- teens_cs[!is.na(teens_cs$pid),]

# Create two-parent subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Re-code variables
teens_twop <- teens_twop %>% 
  mutate(mother_pid = case_when(
    parent1_sex == "Female" ~ parent1_pid,
    parent2_sex == "Female" ~ parent2_pid,
    TRUE ~ NA),
    father_pid = case_when(
      parent1_sex == "Male" ~ parent1_pid,
      parent2_sex == "Male" ~ parent2_pid,
      TRUE ~ NA),
    mother_age = case_when(
      parent1_sex == "Female" ~ parent1_age,
      parent2_sex == "Female" ~ parent2_age,
    TRUE ~ NA),
    father_age = case_when(
      parent1_sex == "Male" ~ parent1_age,
      parent2_sex == "Male" ~ parent2_age,
      TRUE ~ NA),
    mother_reg_class = case_when(
      parent1_sex == "Female" ~ parent1_reggen_class,
      parent2_sex == "Female" ~ parent2_reggen_class,
      TRUE ~ NA),
    father_reg_class = case_when(
      parent1_sex == "Male" ~ parent1_reggen_class,
      parent2_sex == "Male" ~ parent2_reggen_class,
      TRUE ~ NA),
    mother_nssec_class = case_when(
      parent1_sex == "Female" ~ parent1_nssec_class,
      parent2_sex == "Female" ~ parent2_nssec_class,
      TRUE ~ NA),
    father_nssec_class = case_when(
      parent1_sex == "Male" ~ parent1_nssec_class,
      parent2_sex == "Male" ~ parent2_nssec_class,
      TRUE ~ NA),
    mother_private_sector = case_when(
      parent1_sex == "Female" ~ parent1_job_private,
      parent2_sex == "Female" ~ parent2_job_private,
      TRUE ~ NA),
    father_private_sector = case_when(
      parent1_sex == "Male" ~ parent1_job_private,
      parent2_sex == "Male" ~ parent2_job_private,
      TRUE ~ NA),
    mother_hrs_worked = case_when(
      parent1_sex == "Female" ~ parent1_job_hours,
      parent2_sex == "Female" ~ parent2_job_hours,
      TRUE ~ NA),
    father_hrs_worked = case_when(
      parent1_sex == "Male" ~ parent1_job_hours,
      parent2_sex == "Male" ~ parent2_job_hours,
      TRUE ~ NA),
    mother_educ = case_when(
      parent1_sex == "Female" ~ parent1_educ,
      parent2_sex == "Female" ~ parent2_educ,
      TRUE ~ NA),
    father_educ = case_when(
      parent1_sex == "Male" ~ parent1_educ,
      parent2_sex == "Male" ~ parent2_educ,
      TRUE ~ NA),
    mother_religion = case_when(
      parent1_sex == "Female" ~ parent1_relig,
      parent2_sex == "Female" ~ parent2_relig,
      TRUE ~ NA),
    father_religion = case_when(
      parent1_sex == "Male" ~ parent1_relig,
      parent2_sex == "Male" ~ parent2_relig,
      TRUE ~ NA),
    mother_union = case_when(
      parent1_sex == "Female" ~ parent1_union,
      parent2_sex == "Female" ~ parent2_union,
      TRUE ~ NA),
    father_union = case_when(
      parent1_sex == "Male" ~ parent1_union,
      parent2_sex == "Male" ~ parent2_union,
      TRUE ~ NA),
    mother_income = case_when(
      parent1_sex == "Female" ~ parent1_income_grs,
      parent2_sex == "Female" ~ parent2_income_grs,
      TRUE ~ NA),
    father_income = case_when(
      parent1_sex == "Male" ~ parent1_income_grs,
      parent2_sex == "Male" ~ parent2_income_grs,
      TRUE ~ NA))

#### Direct effect of parental socio-economic variables on child pid choice #####
# Remove NAs on socio-economic and partisanship variables
teens_twop <- teens_twop[complete.cases(teens_twop[, c("pid", "mother_pid", "father_pid",
                                                    "region", "hh_gross", 
                                                    "mother_reg_class", "father_reg_class",
                                                    "mother_educ", "father_educ")]), ]

# Re-code household income
teens_twop$hh_gross <- round(teens_twop$hh_gross, 0)

# Turn household income to quintile
teens_twop$hh_gross_qnt <- cut(teens_twop$hh_gross, 
                              breaks = quantile(teens_twop$hh_gross, 
                                                probs = seq(0, 1, by = 0.2), 
                                                na.rm = T), 
                              include.lowest = T, 
                              labels = 1:5)

# Fix education categories
teens_twop <- teens_twop %>% 
  mutate(mother_educ = case_when(
    mother_educ == "A level etc" ~ "A levels etc",
    TRUE ~ mother_educ),
    father_educ = case_when(
      father_educ == "A level etc" ~ "A levels etc",
      TRUE ~ father_educ))

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")
teens_twop$mother_educ <- relevel(factor(teens_twop$mother_educ), ref = "No qual")
teens_twop$father_educ <- relevel(factor(teens_twop$father_educ), ref = "No qual")

# Run regression
twop_ses <- multinom(pid ~ mother_pid + father_pid + region + 
                           hh_gross_qnt + mother_reg_class + father_reg_class +
                           mother_educ + father_educ,
                         data = teens_twop)

table(model.frame(twop_ses)$pid)

# Table
stargazer(twop_ses,
          title = "Direct of parental socio-economic status on child party identification.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other party",  "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other party", "Region: East of England", "Region: London", "Region: North East", "Region: North West", "Region: Scotland", "Region: South East", "Region: South West", "Region: Wales", "Region: West Midlands", "Region: Yorkshire and the Humber", "HH income: 2nd quintile", "HH income: 3nd quintile", "HH income: 4th quintile", "HH income: 5th quintile", "Mother class: Party skilled", "Mother class: Professional", "Mother class: Skilled manual", "Mother class: Skilled non-manual", "Mother class: Unskilled", "Father class: Party skilled", "Father class: Professional", "Father class: Skilled manual", "Father class: Skilled non-manual", "Father class: Unskilled", "Mother education: A levels", "Mother education: Degree", "Mother education: GCSEs", "Mother education: Lower secondary", "Mother education: Other higher", "Mother education: Other", "Father education: A levels", "Father education: Degree", "Father education: GCSEs", "Father education: Lower secondary", "Father education: Other higher", "Father education: Other", "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 3,417).",
          table.placement = "H",
          digits = 2)


#### BEHAVIOURAL GENETICS EXPLANATION ------------------------------------------ ####
##### Prepare dataset #####
# Load data
panel <- read_dta("Cleaned data/panel.dta")

# Convert empty strings to NA_character_ for all character columns
panel <- panel %>%
  mutate(across(where(is.character), ~ na_if(., "")))

# Subset to children aged 16-17 (cross section)
teens_cs <- panel  %>%
  filter(age %in% c(16, 17)) %>% 
  distinct(child_id, .keep_all = T)

# Remove respondents with NA in the pid variable
teens_cs <- teens_cs[!is.na(teens_cs$pid),]

# Add mother and father party identity variables
teens_cs <- teens_cs %>% 
  mutate(mother_pid = case_when(
    parent1_sex == "Female" ~ parent1_pid,
    parent2_sex == "Female" ~ parent2_pid,
    TRUE ~ NA),
    father_pid = case_when(
      parent1_sex == "Male" ~ parent1_pid,
      parent2_sex == "Male" ~ parent2_pid,
      TRUE ~ NA))

##### Effect of step fathers vs biological fathers #####
# Create two-parent subset
teens_twop <- teens_cs %>%
  filter(!is.na(parent1_id), 
         !is.na(parent2_id))

# Code step-father dummy
teens_twop$stepf_dummy <- ifelse(teens_twop$family_type == "Extended step-father", "Yes", "No")

table(teens_twop$stepf_dummy)

# Re-level explanatory variables
teens_twop$pid <- relevel(factor(teens_twop$pid), ref = "No party")
teens_twop$mother_pid <- relevel(factor(teens_twop$mother_pid), ref = "No party")
teens_twop$father_pid <- relevel(factor(teens_twop$father_pid), ref = "No party")

# Run regression
twop_stepf <- multinom(pid ~ mother_pid + father_pid*stepf_dummy,
                            data = teens_twop)

table(model.frame(twop_stepf)$pid)

# Table
stargazer(twop_stepf,
          title = "Correlation between parental party identification and child party identification in biological versus extended step-father families.",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party", "Step-father dummy", "Father Conservative * Step-father dummy", "Father Labour * Step-father dummy", "Father Lib Dem * Step-father dummy", "Father Other Party * Step-father dummy", "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 4,578).",
          table.placement = "H",
          digits = 2)