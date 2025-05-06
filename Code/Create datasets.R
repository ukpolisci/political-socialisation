##### Set-up #####
# Set working directory
setwd("~/OneDrive - Nexus365/01. Oxford/Thesis/Data analysis/Code")

# Clear environment
rm(list = ls())

# Load packages
library(haven)
library(dplyr)

##### Load all individual response questionnaires from BHPS #####
# Load BHPS datasets
ba_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/ba_indresp.dta")
bb_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bb_indresp.dta")
bc_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bc_indresp.dta")
bd_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bd_indresp.dta")
be_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/be_indresp.dta")
bf_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bf_indresp.dta")
bg_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bg_indresp.dta")
bh_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bh_indresp.dta")
bi_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bi_indresp.dta")
bj_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bj_indresp.dta")
bk_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bk_indresp.dta")
bl_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bl_indresp.dta")
bm_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bm_indresp.dta")
bn_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bn_indresp.dta")
bo_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bo_indresp.dta")
bp_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bp_indresp.dta")
bq_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bq_indresp.dta")
br_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/br_indresp.dta")

##### Create long-format BHPS panel dataset #####
# Create wave identifiers for BHPS
waves <- c("ba", "bb", "bc", "bd", "be", "bf", "bg", "bh", "bi", "bj", "bk", 
           "bl", "bm", "bn", "bo", "bp", "bq", "br")

# Create list of BHPS datasets
bhps_datasets <- list(
  ba_indresp, bb_indresp, bc_indresp, bd_indresp, be_indresp,
  bf_indresp, bg_indresp, bh_indresp, bi_indresp, bj_indresp,
  bk_indresp, bl_indresp, bm_indresp, bn_indresp, bo_indresp,
  bp_indresp, bq_indresp, br_indresp)

# BHPS dataset names as strings
bhps_strings <- c(
  "ba_indresp", "bb_indresp", "bc_indresp", "bd_indresp", "be_indresp",
  "bf_indresp", "bg_indresp", "bh_indresp", "bi_indresp", "bj_indresp",
  "bk_indresp", "bl_indresp", "bm_indresp", "bn_indresp", "bo_indresp",
  "bp_indresp", "bq_indresp", "br_indresp")

# Add wave variable to each BHPS dataset
for (i in seq_along(bhps_datasets)) {
  bhps_datasets[[i]]$wave <- waves[i]
  assign(bhps_strings[i], bhps_datasets[[i]])
}

# Define function to remove wave prefixes from variable names
remove_wave_prefixes <- function(data, waves) {
  pattern <- paste0("^(", paste(waves, collapse = "|"), ")_")
  new_names <- gsub(pattern, "", names(data))
  colnames(data) <- new_names
  return(data)
}

# Apply the function to each BHPS dataset
bhps_datasets_clean <- lapply(bhps_datasets, remove_wave_prefixes, waves)

# Combine all BHPS datasets into one
bhps <- bind_rows(bhps_datasets_clean)

# Subset to variables of interest 
bhps <- bhps[, c("pidp", "hidp", "wave", "age", "sex", "gor_dv", 
                 "vote1", "vote2", "vote4", "vote5", "vote6", 
                 "jbrgsc_dv", "jbnssec8_dv", "jbsect", "jbhrs", "jbft_dv", 
                 "wktime", "tuin1", "tujbpl", "fimngrs_dv",
                 "hiqual_dv", "qfedhi", "oprlg", "oprlg1",
                 "husits", "howlng", "leikid")]

# Save file
write_dta(bhps, "bhps_subset.dta")

##### Load all individual response questionnaires from UKHLS #####
# Load UKHLS datasets
a_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/a_indresp.dta")
b_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/b_indresp.dta")
c_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/c_indresp.dta")
d_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/d_indresp.dta")
e_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/e_indresp.dta")
f_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/f_indresp.dta")
g_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/g_indresp.dta")
h_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/h_indresp.dta")
i_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/i_indresp.dta")
j_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/j_indresp.dta")
k_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/k_indresp.dta")
l_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/l_indresp.dta")
m_indresp <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/m_indresp.dta")

##### Create long-format UKHLS panel dataset #####
# Create wave identifiers for UKHLS
waves1 <- c("a", "b", "c", "d")
waves2 <- c("e", "f", "g", "h")
waves3 <- c("i", "j", "k", "l", "m")

# Create list of UKHLS datasets
ukhls_datasets1 <- list(a_indresp, b_indresp, c_indresp, d_indresp)
ukhls_datasets2 <- list(e_indresp, f_indresp, g_indresp, h_indresp)
ukhls_datasets3 <- list(i_indresp, j_indresp, k_indresp, l_indresp, m_indresp)

# UKHLS dataset names as strings
ukhls_strings1 <- c("a_indresp", "b_indresp", "c_indresp", "d_indresp")
ukhls_strings2 <- c("e_indresp", "f_indresp", "g_indresp", "h_indresp")
ukhls_strings3 <- c("i_indresp", "j_indresp", "k_indresp", "l_indresp", "m_indresp")

# Add wave variable to each UKHLS dataset
for (i in seq_along(ukhls_datasets1)) {
  ukhls_datasets1[[i]]$wave <- waves1[i]
  assign(ukhls_strings1[i], ukhls_datasets1[[i]])
}
for (i in seq_along(ukhls_datasets2)) {
  ukhls_datasets2[[i]]$wave <- waves2[i]
  assign(ukhls_strings2[i], ukhls_datasets2[[i]])
}

for (i in seq_along(ukhls_datasets3)) {
  ukhls_datasets3[[i]]$wave <- waves3[i]
  assign(ukhls_strings3[i], ukhls_datasets3[[i]])
}

# Apply function to remove wave prefixes to each UKHLS dataset
ukhls_datasets_clean1 <- lapply(ukhls_datasets1, remove_wave_prefixes, waves1)
ukhls_datasets_clean2 <- lapply(ukhls_datasets2, remove_wave_prefixes, waves2)
ukhls_datasets_clean3 <- lapply(ukhls_datasets3, remove_wave_prefixes, waves3)

# Combine all UKHLS datasets
ukhls1 <- bind_rows(ukhls_datasets_clean1)
ukhls2 <- bind_rows(ukhls_datasets_clean2)
ukhls3 <- bind_rows(ukhls_datasets_clean3)

# Subset to variables of interest 
ukhls1 <- ukhls1[, c("pidp", "hidp", "wave", "age_dv", "sex", "gor_dv", "urban_dv",
                 "vote1", "vote2", "vote4", "vote5", "vote6", 
                 "jbrgsc_dv", "jbnssec8_dv", "jbsect", "jbhrs", "jbft_dv", 
                 "wktime", "wkends", "tuin1", "tujbpl", "fimngrs_dv",
                 "hiqual_dv", "qfhigh_dv", "oprlg", "oprlg1",
                 "husits", "howlng", "socialkid", "dinner",
                 "whorufam", "famsup", "upset", "argm", "argf", "tlkm", "tlkf", "eatlivu",
                 "ps9", "ps18", "ps21", "ps27")]

ukhls2 <- ukhls2[, c("pidp", "hidp", "wave", "age_dv", "sex", "gor_dv", "urban_dv",
                     "vote1", "vote2", "vote4", "vote5", "vote6", 
                     "jbrgsc_dv", "jbnssec8_dv", "jbsect", "jbhrs", "jbft_dv", 
                     "wktime", "wkends", "tuin1", "tujbpl", "fimngrs_dv",
                     "hiqual_dv", "qfhigh_dv", "oprlg", "oprlg1",
                     "husits", "howlng", "socialkid", "dinner",
                     "whorufam", "famsup", "upset", "argm", "argf", "tlkm", "tlkf", "eatlivu",
                     "ps9", "ps18", "ps21", "ps27")]
ukhls3 <- ukhls3[, c("pidp", "hidp", "wave", "age_dv", "sex", "gor_dv", "urban_dv",
                     "vote1", "vote2", "vote4", "vote5", "vote6", 
                     "jbrgsc_dv", "jbnssec8_dv", "jbsect", "jbhrs", "jbft_dv", 
                     "wktime", "wkends", "tuin1", "tujbpl", "fimngrs_dv",
                     "hiqual_dv", "qfhigh_dv", "oprlg", "oprlg1",
                     "husits", "howlng", "socialkid", "dinner",
                     "whorufam", "famsup", "upset", "argm", "argf", "tlkm", "tlkf", "eatlivu",
                     "ps9", "ps18", "ps21", "ps27")]

# Create single UKHLS dataset
ukhls <- bind_rows(ukhls1, ukhls2, ukhls3)

# Save file
write_dta(ukhls, "ukhls_subset.dta")