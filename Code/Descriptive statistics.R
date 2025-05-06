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

# Load data
panel <- read_dta("Cleaned data/panel.dta")

# Convert empty strings to NA_character_ for all character columns
panel <- panel %>%
  mutate(across(where(is.character), ~ na_if(., "")))

### Checkpoint: 23,143 unique children and 23,261 unique parents
length(unique(panel$child_id)) 
length(unique(panel$parent1_id)) + length(unique(panel$parent2_id)) 
length(unique(panel$house_id))

#### Create and clean teen cross-section ####
# Subset to children aged 16-17 (cross section)
teens_cs <- panel  %>%
  filter(age %in% c(16, 17)) %>% 
  distinct(child_id, .keep_all = T)

### Checkpoint: 12,662 unique teenagers and 14,142 unique parents in 12,332 households
length(unique(teens_cs$child_id)) 
length(unique(teens_cs$parent1_id)) + length(unique(teens_cs$parent2_id)) 
length(unique(teens_cs$house_id))

# Remove respondents with NA in the pid variable
teens_cs <- teens_cs[!is.na(teens_cs$pid),]

### Checkpoint: 11,492 unique teenagers and 13,045 unique parents in 11,211 households
length(unique(teens_cs$child_id)) 
length(unique(teens_cs$parent1_id)) + length(unique(teens_cs$parent2_id)) 
length(unique(teens_cs$house_id))

# Save teen cross-section
write_dta(teens_cs, "teens_cross_section.dta")

#### Create and clean teen panel ####
# Identify children aged 16-17
teens_ids <- panel  %>%
  filter(age %in% c(16, 17)) %>% 
  distinct(child_id, .keep_all = T)

# Subset panel to include only ids from the teen cross-section
teens_pnl <- panel %>% 
  filter(child_id %in% teens_ids$child_id)

### Checkpoint: 12,662 unique teenagers and 14,142 unique parents in 12,332 unique households
length(unique(teens_pnl$child_id)) 
length(unique(teens_pnl$parent1_id)) + length(unique(teens_pnl$parent2_id)) 
length(unique(teens_pnl$house_id))

# Remove respondents with NA in the pid variable
teens_pnl <- teens_pnl[!is.na(teens_pnl$pid),]

### Checkpoint: 12,137 unique teenagers and 13,601 unique parents in 11,839 unique households
length(unique(teens_pnl$child_id)) 
length(unique(teens_pnl$parent1_id)) + length(unique(teens_pnl$parent2_id)) 
length(unique(teens_pnl$house_id))

# Save teen cross-section
write_dta(teens_pnl, "teens_panel.dta")

#### Descriptive statistics for the cross-section ####
# Figure for number of teenagers by wave
teens_by_wave <- teens_cs %>%
  group_by(wave) %>%
  count()

fig2_final <- ggplot(teens_by_wave, aes(x = wave, y = n)) +
  geom_col() +
  labs(x = "Wave",
       y = "Number of teenagers") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

ggsave("Figures/fig2 final.png", fig2_final, 
       bg = "white",
       height = 6,
       width = 9)

# Calculate average household size
teens_cs %>%
  group_by(house_id) %>%
  summarise(household_size = n_distinct(child_id) + n_distinct(parent1_id) + n_distinct(parent2_id, na.rm = T)) %>%
  summarise(avg_household_size = mean(household_size, na.rm = T))

## Table for number of 1- and 2-parent households
xtable(teens_cs %>% 
         group_by(parent_n) %>%
         summarise(households_n = n_distinct(house_id)), 
       caption = "Number of 1- and 2-parent households.")

## Table for number of children per household
xtable(teens_cs %>% 
         group_by(house_id) %>%
         summarize(child_n = n_distinct(child_id)) %>% 
         count(child_n), 
       caption = "Number of children (16-17 years old) per household.")

## Plot for party identification choice of teens
pid_trend_teens <- teens_cs %>%
  group_by(wave, pid) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percent = count / sum(count)) %>%
  ungroup()

fig4_final <- ggplot(pid_trend_teens, aes(x = factor(wave), y = percent, color = factor(pid), group = pid)) +
  geom_line(linewidth = .5) +
  geom_point(size = .5) +
  scale_x_discrete(
    breaks = seq(min(pid_trend_teens$wave),
                 max(pid_trend_teens$wave),
                 by   = 4)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(
    "Conservative" = "blue",
    "Labour" = "red",
    "Lib Dem" = "orange",
    "Other party" = "grey",
    "No party" = "black")) +
  labs(x = "Wave", 
       y = "Percent",
       color = "Party") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave("Figures/fig4 final.png", fig4_final, 
       bg = "white",
       height = 6,
       width = 9)

## Plot for party identification choice of parents
parents_long_pid <- teens_cs %>%
  pivot_longer(cols = c(parent1_pid, parent2_pid), 
               names_to = "parent", 
               values_to = "pid_parents")

pid_trend_parents <- parents_long_pid %>%
  filter(!is.na(pid_parents)) %>% 
  group_by(wave, pid_parents) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(wave) %>%
  mutate(percent = count / sum(count)) %>%
  ungroup()

fig5_final <- ggplot(pid_trend_parents, aes(x = factor(wave), y = percent, color = factor(pid_parents), group = pid_parents)) +
  geom_line(size = .5) +
  geom_point(size = .5) +
  scale_x_discrete(
    breaks = seq(min(pid_trend_teens$wave),
                 max(pid_trend_teens$wave),
                 by   = 4)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(
    "Conservative" = "blue",
    "Labour" = "red",
    "Lib Dem" = "orange",
    "Other party" = "grey",
    "No party" = "black")) +
  labs(x = "Wave", 
       y = "Percent",
       color = "Party") +
  theme_minimal() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave("Figures/fig5 final.png", fig5_final, 
       bg = "white",
       height = 6,
       width = 9)

## Table for share of party-homogeneous and heterogeneous families 
xtable(teens_cs %>%
         filter(!is.na(parent1_pid) & !is.na(parent2_pid)) %>% 
         mutate(
           partisan_comp = case_when(
             parent1_pid == parent2_pid ~ "Parents same party",
             parent1_pid != parent2_pid ~ "Parents different parties"
           )) %>%
         group_by(partisan_comp) %>%
         summarise(count = n_distinct(house_id)) %>% 
         ungroup() %>%
         mutate(proportion = count / sum(count)),
       caption = "Partisan composition of two-parent families.")

## Table for share of party-homogeneous and heterogeneous families excluding no party identification
xtable(teens_cs %>%
         filter(!is.na(parent1_pid) & !is.na(parent2_pid) & parent1_pid != "No party" & parent2_pid != "No party") %>% 
         mutate(
           partisan_comp = case_when(
             parent1_pid == parent2_pid ~ "Parents same party",
             parent1_pid != parent2_pid ~ "Parents different parties"
           )) %>%
         group_by(partisan_comp) %>%
         summarise(count = n_distinct(house_id)) %>% 
         ungroup() %>%
         mutate(proportion = count / sum(count)),
       caption = "Partisan composition of twop-parent families excluding non-identifiers.")

#### Cross-tabulations to check sample sizes ####
## Two-parent families
# Create two-parent families subset
teens_twop <- teens_cs %>% 
  filter(!is.na(parent1_id) & !is.na(parent2_id))

# Create two-parent natural families subset
teens_nat <- teens_twop %>% 
  filter(family_type == "Traditional")

# Create two-parent extended mother families subset (with step-mothers)
teens_stepm <- teens_twop %>% 
  filter(family_type == "Extended step-mother")

# Create two-parent extended father families subset (with step-fathers)
teens_stepf <- teens_twop %>% 
  filter(family_type == "Extended step-father")

# Cross-tab of parent-child pid combinations in full natural families
teens_nat <- teens_nat[complete.cases(teens_nat[, c("bio_mother_pid", "bio_father_pid", "pid")]), ]
table(teens_nat$bio_mother_pid, teens_nat$pid)
table(teens_nat$bio_father_pid, teens_nat$pid)

# Cross-tab of parent-child pid combinations in extended mother families
teens_stepm <- teens_stepm[complete.cases(teens_stepm[, c("step_mother_pid", "bio_father_pid", "pid")]), ]
table(teens_stepm$step_mother_pid, teens_stepm$pid)
table(teens_stepm$bio_father_pid, teens_stepm$pid)

# Cross-tab of parent-child pid combinations in extended mother families
teens_stepf <- teens_stepf[complete.cases(teens_stepf[, c("bio_mother_pid", "step_father_pid", "pid")]), ]
table(teens_stepf$bio_mother_pid, teens_stepf$pid)
table(teens_stepf$step_father_pid, teens_stepf$pid)

## One-parent families
# Create two-parent families subset
teens_onep <- teens_cs %>% 
  filter(!is.na(parent1_id) & is.na(parent2_id))

# Create one-parent mother families subset (single mothers)
teens_onepm <- teens_onep %>% 
  filter(family_type == "Single mother")

# Create one-parent father families subset (single fathers)
teens_onepf <- teens_onep %>% 
  filter(family_type == "Single father")

# Cross-tab of parent-child pid combinations in single mother families
teens_onepm <- teens_onepm[complete.cases(teens_onepm[, c("bio_mother_pid", "pid")]), ]
table(teens_onepm$bio_mother_pid, teens_onepm$pid)

# Cross-tab of parent-child pid combinations in single father families
teens_onepf <- teens_onepf[complete.cases(teens_onepf[, c("bio_father_pid", "pid")]), ]
table(teens_onepf$bio_father_pid, teens_onepf$pid)
