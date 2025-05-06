#### Set-up ####
# Set working directory
setwd("~/OneDrive - Nexus365/01. Oxford/Thesis/Data analysis")

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
library(entropy)

#### Clean panel ####
# Load panel
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

# Subset panel to include only ids from the teen cross-section
teens_pnl <- panel %>% 
  filter(child_id %in% teens_cs$child_id)

#### Impact of parental partisanship at 16/17 on partisanship late-stage impressionable years ####
# Create mother and father party identity variables
teens_pnl <- teens_pnl %>%
  mutate(mother_pid = case_when(
    parent1_sex == "Female" ~ parent1_pid,
    parent2_sex == "Female" ~ parent2_pid,
    TRUE ~ NA),
    father_pid = case_when(
      parent1_sex == "Male" ~ parent1_pid,
      parent2_sex == "Male" ~ parent2_pid,
      TRUE ~ NA))

# Create variable for parental partisanship at 16 or 17 on
teens_pnl <- teens_pnl %>%
  mutate(mother_pid_16 = case_when(
    !is.na(mother_pid) & age %in% c(16, 17) ~ mother_pid,
    TRUE ~ NA),
  father_pid_16 = case_when(
    !is.na(father_pid) & age %in% c(16, 17) ~ father_pid,
    TRUE ~ NA),
  yadult_pid = case_when(
    age %in% c(22:99) ~ pid,
    TRUE ~ NA))

# Impute the parental partisanship variables as time-invariant
teens_pnl <- teens_pnl %>%
  group_by(child_id) %>%
  mutate(
    mother_pid_16 = first(mother_pid_16, order_by = wave),
    father_pid_16 = first(father_pid_16, order_by = wave)) %>%
  ungroup()

# Subset to include only young adults
yadults_pnl <- teens_pnl %>%
  filter(age %in% c(22:99)) %>%
  distinct(child_id, .keep_all = T)

# Re-level explanatory variables
yadults_pnl$yadult_pid <- relevel(factor(yadults_pnl$yadult_pid), ref = "No party")
yadults_pnl$mother_pid_16 <- relevel(factor(yadults_pnl$mother_pid_16), ref = "No party")
yadults_pnl$father_pid_16 <- relevel(factor(yadults_pnl$father_pid_16), ref = "No party")

# Run regression
twop_yadults <- multinom(yadult_pid ~ mother_pid_16 + father_pid_16,
              data = yadults_pnl)

table(model.frame(twop_yadults)$yadult_pid)

# Table
stargazer(twop_yadults,
          title = "Correlation between parental party identification when the child was 16 and child party identification during adulthood (above 22 years old).",
          covariate.labels = c("Mother Conservative", "Mother Labour", "Mother Lib Dem", "Mother Other Party", "Father Conservative", "Father Labour", "Father Lib Dem", "Father Other Party", "Constant"),
          dep.var.caption = "Child party identification",
          dep.var.labels = c("Conservative", "Labour", "Lib Dem", "Other Party"),
          notes = "Reference category: No Party (child n = 1,505).",
          table.placement = "H",
          digits = 2)


#### Effect of socialisation versus not ####
# Code socialisation outcome
teens_pnl <- teens_pnl %>% 
  mutate(soc = case_when(
    age %in% c(16, 17) & (pid == mother_pid_16 | pid == father_pid_16) ~ 1,
    age %in% c(16, 17) & (pid != mother_pid_16 & pid != father_pid_16) ~ 0,
    TRUE ~ NA))

# Identify relevant observations
teen_obs <- teens_pnl %>%
  filter(age %in% c(16, 17)) %>%
  pull(child_id) %>%
  unique()

# Calculate entropy
teens_entropy <- teens_pnl %>%
  group_by(child_id) %>%
  mutate(pid_entropy = entropy(table(pid))) %>%
  ungroup() 

# Effect of socialisation on child party id entropy
stability <- lm(pid_entropy ~ soc,
                data = teens_entropy)

stargazer(stability,
          title = "Correlation between party identity socialised in pre-adulthood and party identity stability during adulthood.",
          covariate.labels = c("Party identity socialised (dummy)", "Constant"),
          dep.var.caption = "",
          dep.var.labels = "Party identification entropy",
          table.placement = "H",
          digits = 2)
