##### Set-up #####
# Set working directory
setwd("~/OneDrive - Nexus365/01. Oxford/Thesis/Data analysis/Code")

# Clear environment
rm(list = ls())

# Set seed
set.seed(123)

# Load packages
library(haven)
library(tidyverse)
library(zoo)

##### Identify parents and children from BHPS #####
# Load datasets
ba_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/ba_egoalt.dta")
bb_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bb_egoalt.dta")
bc_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bc_egoalt.dta")
bd_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bd_egoalt.dta")
be_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/be_egoalt.dta")
bf_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bf_egoalt.dta")
bg_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bg_egoalt.dta")
bh_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bh_egoalt.dta")
bi_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bi_egoalt.dta")
bj_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bj_egoalt.dta")
bk_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bk_egoalt.dta")
bl_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bl_egoalt.dta")
bm_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bm_egoalt.dta")
bn_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bn_egoalt.dta")
bo_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bo_egoalt.dta")
bp_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bp_egoalt.dta")
bq_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/bq_egoalt.dta")
br_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/bhps/br_egoalt.dta")

# Define function to remove wave prefixes
remove_prefix <- function(dataset, prefix) {
  colnames(dataset) <- gsub(paste0("^", prefix, "_"), "", colnames(dataset))
  return(dataset)
}

# Standardise variable names for all datasets
ba_egoalt <- remove_prefix(ba_egoalt, "ba")
bb_egoalt <- remove_prefix(bb_egoalt, "bb")
bc_egoalt <- remove_prefix(bc_egoalt, "bc")
bd_egoalt <- remove_prefix(bd_egoalt, "bd")
be_egoalt <- remove_prefix(be_egoalt, "be")
bf_egoalt <- remove_prefix(bf_egoalt, "bf")
bg_egoalt <- remove_prefix(bg_egoalt, "bg")
bh_egoalt <- remove_prefix(bh_egoalt, "bh")
bi_egoalt <- remove_prefix(bi_egoalt, "bi")
bj_egoalt <- remove_prefix(bj_egoalt, "bj")
bk_egoalt <- remove_prefix(bk_egoalt, "bk")
bl_egoalt <- remove_prefix(bl_egoalt, "bl")
bm_egoalt <- remove_prefix(bm_egoalt, "bm")
bn_egoalt <- remove_prefix(bn_egoalt, "bn")
bo_egoalt <- remove_prefix(bo_egoalt, "bo")
bp_egoalt <- remove_prefix(bp_egoalt, "bp")
bq_egoalt <- remove_prefix(bq_egoalt, "bq")
br_egoalt <- remove_prefix(br_egoalt, "br")

egoalt_bhps <- bind_rows(
  list(
    ba = ba_egoalt,
    bb = bb_egoalt,
    bc = bc_egoalt,
    bd = bd_egoalt,
    be = be_egoalt,
    bf = bf_egoalt,
    bg = bg_egoalt,
    bh = bh_egoalt,
    bi = bi_egoalt,
    bj = bj_egoalt,
    bk = bk_egoalt,
    bl = bl_egoalt,
    bm = bm_egoalt,
    bn = bn_egoalt,
    bo = bo_egoalt,
    bp = bp_egoalt,
    bq = bq_egoalt,
    br = br_egoalt
  ),
  .id = "wave"
)

# Recode relationship variable
egoalt_bhps$relationship_bh <- as_factor(egoalt_bhps$relationship_bh)

# Recode wave variable
egoalt_bhps <- egoalt_bhps %>%
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
    wave == "br" ~ 2008))

# Create cross-wave household identifier
egoalt_bhps <- egoalt_bhps %>%
  group_by(pidp) %>%
  arrange(wave) %>%
  mutate(house_id = first(hidp)) %>%
  ungroup()

# Subset to variables of interest
egoalt_bhps <- egoalt_bhps[, c(1, 2, 4:6, 8, 14, 16)]

# Subset to households with children
egoalt_bhps <- egoalt_bhps %>%
  group_by(house_id) %>%
  filter(any(relationship_bh %in% c("natural son/daughter",
                                    "stepson/stepdaughter",
                                    "other child"))) %>%
  ungroup()

# Subset to parents and children
egoalt_bhps <- egoalt_bhps %>%
  filter(relationship_bh %in% c("natural parent", 
                                "adoptive parent", 
                                "step-parent",
                                "natural son/daughter",
                                "stepson/stepdaughter",
                                "other child"))

# Identify children pidps
children_bhps <- egoalt_bhps %>%
  filter(relationship_bh %in% c("natural son/daughter",
                                "stepson/stepdaughter",
                                "other child")) %>%
  select(wave, pidp, pno, relationship_bh, apno, apidp, house_id) 

# Rename columns
colnames(children_bhps) <- c("wave", "child_id", "child_number", "relationship", "parent_number", "parent_id", "house_id")

# Identify parents pidps
parents_bhps <- egoalt_bhps %>%
  filter(relationship_bh %in% c("natural parent", 
                                "adoptive parent", 
                                "step-parent")) %>%
  select(wave, pidp, pno, relationship_bh, apno, apidp, house_id)

# Rename columns
colnames(parents_bhps) <- c("wave", "parent_id", "parent_number", "relationship", "child_number", "child_id", "house_id")

### Checkpoint: 16,034 unique children and 12,025 unique parents
length(unique(children_bhps$child_id)) 
length(unique(parents_bhps$parent_id))
length(unique(egoalt_bhps$house_id))

# Create unique parent-child dyad identifiers
children_bhps <- children_bhps %>%
  mutate(dyad = paste(parent_id, child_id, sep = "_"))
parents_bhps <- parents_bhps %>%
  mutate(dyad = paste(parent_id, child_id, sep = "_"))

# Identify valid dyads common to both datasets
valid_dyads <- intersect(children_bhps$dyad, parents_bhps$dyad)

# Filter both datasets to keep only rows with valid dyads
children_bhps <- children_bhps %>%
  filter(dyad %in% valid_dyads)
parents_bhps <- parents_bhps %>%
  filter(dyad %in% valid_dyads)

# Check
length(unique(children_bhps$child_id)) # 14,585 children
length(unique(parents_bhps$parent_id)) # 12,019 parents
length(unique(children_bhps$house_id))
length(unique(parents_bhps$house_id))

##### Identify parents and children from UKHLS #####
# Load datasets
a_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/a_egoalt.dta")
b_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/b_egoalt.dta")
c_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/c_egoalt.dta")
d_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/d_egoalt.dta")
e_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/e_egoalt.dta")
f_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/f_egoalt.dta")
g_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/g_egoalt.dta")
h_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/h_egoalt.dta")
i_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/i_egoalt.dta")
j_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/j_egoalt.dta")
k_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/k_egoalt.dta")
l_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/l_egoalt.dta")
m_egoalt <- read_dta("UKDA-6614-stata/stata/stata13_se/ukhls/m_egoalt.dta")

# Standardise variable names for all datasets
a_egoalt <- remove_prefix(a_egoalt, "a")
b_egoalt <- remove_prefix(b_egoalt, "b")
c_egoalt <- remove_prefix(c_egoalt, "c")
d_egoalt <- remove_prefix(d_egoalt, "d")
e_egoalt <- remove_prefix(e_egoalt, "e")
f_egoalt <- remove_prefix(f_egoalt, "f")
g_egoalt <- remove_prefix(g_egoalt, "g")
h_egoalt <- remove_prefix(h_egoalt, "h")
i_egoalt <- remove_prefix(i_egoalt, "i")
j_egoalt <- remove_prefix(j_egoalt, "j")
k_egoalt <- remove_prefix(k_egoalt, "k")
l_egoalt <- remove_prefix(l_egoalt, "l")
m_egoalt <- remove_prefix(m_egoalt, "m")

egoalt_ukhls <- bind_rows(
  list(
    a = a_egoalt,
    b = b_egoalt,
    c = c_egoalt,
    d = d_egoalt,
    e = e_egoalt,
    f = f_egoalt,
    g = g_egoalt,
    h = h_egoalt,
    i = i_egoalt,
    j = j_egoalt,
    k = k_egoalt,
    l = l_egoalt,
    m = m_egoalt
  ),
  .id = "wave"
)

# Recode relationship variable
egoalt_ukhls$relationship_dv <- as_factor(egoalt_ukhls$relationship_dv)

# Recode wave variable
egoalt_ukhls <- egoalt_ukhls %>%
  mutate(wave = case_when(
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
    wave == "m" ~ 2021))

# Create cross-wave household identifier
egoalt_ukhls <- egoalt_ukhls %>%
  group_by(pidp) %>%
  arrange(wave) %>%
  mutate(house_id = first(hidp)) %>%
  ungroup()

# Subset to variables of interest
egoalt_ukhls <- egoalt_ukhls[, c(1:6, 15, 23)]

# Subset to households with children
egoalt_ukhls <- egoalt_ukhls %>%
  group_by(house_id) %>%
  filter(any(relationship_dv %in% c("natural son/daughter",
                                    "stepson/stepdaughter",
                                    "adopted son/daughter"))) %>%
  ungroup()

# Subset to parents and children
egoalt_ukhls <- egoalt_ukhls %>%
  filter(relationship_dv %in% c("natural parent", 
                                "adoptive parent", 
                                "step-parent", 
                                "natural son/daughter",
                                "stepson/stepdaughter",
                                "adopted son/daughter"))

# Identify children pidps
children_ukhls <- egoalt_ukhls %>%
  filter(relationship_dv %in% c("natural son/daughter", 
                                "stepson/stepdaughter", 
                                "adopted son/daughter")) %>%
  select(wave, pidp, pno, relationship_dv, apno, apidp, house_id) 

# Rename columns
colnames(children_ukhls) <- c("wave", "child_id", "child_number", "relationship", "parent_number", "parent_id", "house_id")

# Identify parents pidps
parents_ukhls <- egoalt_ukhls %>%
  filter(relationship_dv %in% c("natural parent", 
                                "adoptive parent", 
                                "step-parent")) %>%
  select(wave, pidp, pno, relationship_dv, apno, apidp, house_id)

# Rename columns
colnames(parents_ukhls) <- c("wave", "parent_id", "parent_number", "relationship", "child_number", "child_id", "house_id")

### Checkpoint: 47,789 unique children and 36,717 unique parents
length(unique(children_ukhls$child_id)) 
length(unique(parents_ukhls$parent_id))

# Create unique parent-child dyad identifiers
children_ukhls <- children_ukhls %>%
  mutate(dyad = paste(parent_id, child_id, sep = "_"))
parents_ukhls <- parents_ukhls %>%
  mutate(dyad = paste(parent_id, child_id, sep = "_"))

# Identify valid dyads common to both datasets
valid_dyads <- intersect(children_ukhls$dyad, parents_ukhls$dyad)

# Filter both datasets to keep only rows with valid dyads
children_ukhls <- children_ukhls %>%
  filter(dyad %in% valid_dyads)
parents_ukhls <- parents_ukhls %>%
  filter(dyad %in% valid_dyads)

# Check
length(unique(children_ukhls$child_id)) # 44,576 children
length(unique(parents_ukhls$parent_id)) # 36,713 parents
length(unique(egoalt_ukhls$house_id))

##### Store parent and children pidps into common datasets #####
# Create single dataset
pidps <- bind_rows(children_bhps, children_ukhls)

# Keep each pidp only in its first wave
pidps <- pidps %>%
  group_by(child_id) %>%
  filter(wave == min(wave)) %>%
  ungroup()

### Checkpoint: 54,536 unique children and 44,389 unique parents
length(unique(pidps$child_id))
length(unique(pidps$parent_id)) 
length(unique(pidps$house_id))

# Check that there are only unique parent-child dyads
pidps <- pidps %>%
  mutate(dyad = paste(parent_id, child_id, sep = "_")) %>%
  distinct(dyad, .keep_all = T)

# Convert all ids to numeric
pidps <- pidps %>%
  mutate(parent_id = as.numeric(parent_id),
         child_id = as.numeric(child_id),
         relationship = as.character(relationship))

# Add variable for number of parents for each child
pidps <- pidps %>%
  group_by(child_id) %>%
  mutate(parent_n = n_distinct(parent_id)) %>%
  ungroup()

# Add parents as columns for each child
parent_ids <- pidps %>%
  group_by(child_id) %>%
  arrange(parent_id) %>%
  reframe(parent1_id = ifelse(length(unique(parent_id)) >= 1, unique(parent_id)[1], NA), 
            parent1_relationship = ifelse(length(unique(parent_id)) >= 1, relationship[match(unique(parent_id)[1], parent_id)], NA_character_),
    parent2_id = ifelse(length(unique(parent_id)) >= 2, unique(parent_id)[2], NA),
    parent2_relationship = ifelse(length(unique(parent_id)) >= 2, relationship[match(unique(parent_id)[2], parent_id)], NA_character_),
    parent_n = parent_n)

# Remove duplicate rows
parent_ids <- parent_ids %>%
  distinct()

# Write parent_ids
write_dta(parent_ids, "parent_ids.dta")

##### Create panel dataset with children and parents #####
# Load panels
bhps <- read_dta("bhps_subset.dta")
ukhls <- read_dta("ukhls_subset.dta")

## Recode BHPS and UKHLS variables
# Wave
bhps <- bhps %>% 
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
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(wave = case_when(
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
    wave == "m" ~ 2021))
  
# Demographics
bhps$age <- ifelse(bhps$age < 0, NA, bhps$age)
ukhls$age_dv <- ifelse(ukhls$age_dv < 0, NA, ukhls$age_dv)

bhps$sex <- ifelse(bhps$sex == 1, "Male",
                   ifelse(bhps$sex == 2, "Female", NA))
ukhls$sex <- ifelse(ukhls$sex == 1, "Male",
                   ifelse(ukhls$sex == 2, "Female", NA))

bhps <- bhps %>% 
  mutate(gor_dv = case_when(
    gor_dv == 1 ~ "North East",
    gor_dv == 2 ~ "North West",
    gor_dv == 3 ~ "Yorkshire and the Humber",
    gor_dv == 4 ~ "East Midlands",
    gor_dv == 5 ~ "West Midlands",
    gor_dv == 6 ~ "East of England",
    gor_dv == 7 ~ "London",
    gor_dv == 8 ~ "South East",
    gor_dv == 9 ~ "South West",
    gor_dv == 10 ~ "Wales",
    gor_dv == 11 ~ "Scotland",
    gor_dv == 12 ~ "Northern Ireland",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(gor_dv = case_when(
    gor_dv == 1 ~ "North East",
    gor_dv == 2 ~ "North West",
    gor_dv == 3 ~ "Yorkshire and the Humber",
    gor_dv == 4 ~ "East Midlands",
    gor_dv == 5 ~ "West Midlands",
    gor_dv == 6 ~ "East of England",
    gor_dv == 7 ~ "London",
    gor_dv == 8 ~ "South East",
    gor_dv == 9 ~ "South West",
    gor_dv == 10 ~ "Wales",
    gor_dv == 11 ~ "Scotland",
    gor_dv == 12 ~ "Northern Ireland",
    TRUE  ~  NA))

bhps <- bhps %>% 
  mutate(qfedhi = case_when(
    qfedhi == 1 ~ "Higher degree",
    qfedhi == 2 ~ "First degree",
    qfedhi == 3 ~ "Teaching qual.",
    qfedhi == 4 ~ "Other higher qual.",
    qfedhi == 5 ~ "Nursing qual.",
    qfedhi == 6 ~ "A Levels or eq.",
    qfedhi == 7 ~ "O Levels or eq.",
    qfedhi == 8 ~ "Commercial qual. or no O Levels",
    qfedhi == 9 ~ "CSE grade 2-5 or Scot grade 4-5",
    qfedhi == 10 ~ "Apprenticeship",
    qfedhi == 11 ~ "Other qual.",
    qfedhi == 12 ~ "No qual.",
    qfedhi == 13 ~ "Still at school",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(qfhigh_dv = case_when(
    qfhigh_dv == 1 ~ "Higher degree",
    qfhigh_dv == 2 ~ "First degree or eq.",
    qfhigh_dv == 3 ~ "Diploma HE",
    qfhigh_dv == 4 ~ "Teaching qual.",
    qfhigh_dv == 5 ~ "Nursing or medical qual.",
    qfhigh_dv == 6 ~ "Other higher degree",
    qfhigh_dv == 7 ~ "A level",
    qfhigh_dv == 8 ~ "Welsh bac",
    qfhigh_dv == 9 ~ "IB",
    qfhigh_dv == 10 ~ "AS level",
    qfhigh_dv == 11 ~ "Scottish higher",
    qfhigh_dv == 12 ~ "Cert. 6th year studies",
    qfhigh_dv == 13 ~ "GCSE or eq.",
    qfhigh_dv == 14 ~ "CSE",
    qfhigh_dv == 15 ~ "Standard or lower",
    qfhigh_dv == 16 ~ "Other",
    qfhigh_dv == 96 ~ "No qual.",
    TRUE  ~  NA))

bhps <- bhps %>% 
  mutate(hiqual_dv = case_when(
    hiqual_dv == 1 ~ "Degree",
    hiqual_dv == 2 ~ "Other higher degree",
    hiqual_dv == 3 ~ "A-Level etc.",
    hiqual_dv == 4 ~ "GCSE etc.",
    hiqual_dv == 5 ~ "Other",
    hiqual_dv == 9 ~ "No qualification",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(hiqual_dv = case_when(
    hiqual_dv == 1 ~ "Degree",
    hiqual_dv == 2 ~ "Other higher degree",
    hiqual_dv == 3 ~ "A-Level etc.",
    hiqual_dv == 4 ~ "GCSE etc.",
    hiqual_dv == 5 ~ "Other",
    hiqual_dv == 9 ~ "No qualification",
    TRUE  ~  NA))

bhps$oprlg <- ifelse(bhps$oprlg == 1, "Yes",
                     ifelse(bhps$oprlg == 2, "No", NA))
ukhls$oprlg <- ifelse(ukhls$oprlg == 1, "Yes",
                     ifelse(ukhls$oprlg == 2, "No", NA))

bhps <- bhps %>% 
  mutate(oprlg1 = case_when(
    oprlg1 == 2 ~ "CoE / Anglican",
    oprlg1 == 3 ~ "Roman Catholic",
    oprlg1 == 4 ~ "CoS",
    oprlg1 == 5 ~ "Free Church / Free Presbyterian CoS",
    oprlg1 == 6 ~ "Episcopalian",
    oprlg1 == 7 ~ "Methodist",
    oprlg1 == 8 ~ "Baptist",
    oprlg1 == 9 ~ "Congregational / United Reform",
    oprlg1 == 10 ~ "Other Christian",
    oprlg1 == 11 ~ "Christian no denomination",
    oprlg1 == 12 ~ "Muslim",
    oprlg1 == 13 ~ "Hindu",
    oprlg1 == 14 ~ "Jewish",
    oprlg1 == 15 ~ "Sikh",
    oprlg1 == 16 ~ "Buddhist",
    oprlg1 == 97 ~ "Other",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(oprlg1 = case_when(
    oprlg1 == 2 ~ "CoE / Anglican",
    oprlg1 == 3 ~ "Roman Catholic",
    oprlg1 == 4 ~ "CoS",
    oprlg1 == 5 ~ "Free Church / Free Presbyterian CoS",
    oprlg1 == 6 ~ "Episcopalian",
    oprlg1 == 7 ~ "Methodist",
    oprlg1 == 8 ~ "Baptist",
    oprlg1 == 9 ~ "Congregational / United Reform",
    oprlg1 == 10 ~ "Other Christian",
    oprlg1 == 11 ~ "Christian no denomination",
    oprlg1 == 12 ~ "Muslim",
    oprlg1 == 13 ~ "Hindu",
    oprlg1 == 14 ~ "Jewish",
    oprlg1 == 15 ~ "Sikh",
    oprlg1 == 16 ~ "Buddhist",
    oprlg1 == 97 ~ "Other",
    TRUE  ~  NA))

# Political attitudes
bhps$vote1 <- ifelse(bhps$vote1 == 1, "Yes",
                       ifelse(bhps$vote1 == 2, "No", NA))
ukhls$vote1 <- ifelse(ukhls$vote1 == 1, "Yes",
                     ifelse(ukhls$vote1 == 2, "No", NA))

bhps$vote2 <- ifelse(bhps$vote2 == 1, "Yes",
                          ifelse(bhps$vote2 == 2, "No", 
                                 ifelse(bhps$vote2 == -8, "Inapplicable", NA)))
ukhls$vote2 <- ifelse(ukhls$vote2 == 1, "Yes",
                     ifelse(ukhls$vote2 == 2, "No", 
                            ifelse(ukhls$vote2 == -8, "Inapplicable", NA)))

bhps$vote4 <- ifelse(bhps$vote4 == 1, "Conservative",
                          ifelse(bhps$vote4 == 2, "Labour", 
                                 ifelse(bhps$vote4 == 3, "Lib Dem", 
                                        ifelse(bhps$vote4 == -8, "Inapplicable",
                                               ifelse((bhps$vote4 < 0 & bhps$vote4 != -8), NA, "Other party")))))
ukhls$vote4 <- ifelse(ukhls$vote4 == 1, "Conservative",
                     ifelse(ukhls$vote4 == 2, "Labour", 
                            ifelse(ukhls$vote4 == 3, "Lib Dem", 
                                   ifelse(ukhls$vote4 == -8, "Inapplicable",
                                          ifelse((ukhls$vote4 < 0 & ukhls$vote4 != -8), NA, "Other party")))))

bhps$vote5 <- ifelse(bhps$vote5 == -8, "Inapplicable",
                     ifelse((bhps$vote5 < 0 & bhps$vote5 != -8), NA, bhps$vote5))
bhps$vote5 <- ifelse(bhps$vote5 == 1, 3, 
                     ifelse(bhps$vote5 == 3, 1, 2))
ukhls$vote5 <- ifelse(ukhls$vote5 == -8, "Inapplicable",
                     ifelse((ukhls$vote5 < 0 & ukhls$vote5 != -8), NA, ukhls$vote5))
ukhls$vote5 <- ifelse(ukhls$vote5 == 1, 3, 
                     ifelse(ukhls$vote5 == 3, 1, 2))

bhps$vote6 <- ifelse(bhps$vote6 < 0, NA, bhps$vote6)
bhps$vote6 <- ifelse(bhps$vote6 == 1, 4, 
                      ifelse(bhps$vote6 == 2, 3,
                             ifelse(bhps$vote6 == 3, 2, 1)))
ukhls$vote6 <- ifelse(ukhls$vote6 < 0, NA, ukhls$vote6)
ukhls$vote6 <- ifelse(ukhls$vote6 == 1, 4, 
                     ifelse(ukhls$vote6 == 2, 3,
                            ifelse(ukhls$vote6 == 3, 2, 1)))

# Workplace and social class
bhps <- bhps %>% 
  mutate(jbrgsc_dv = case_when(
  jbrgsc_dv == 1 ~ "Professional",
  jbrgsc_dv == 2 ~ "Managerial / technical",
  jbrgsc_dv == 3 ~ "Skilled non-manual",
  jbrgsc_dv == 4 ~ "Skilled manual",
  jbrgsc_dv == 5 ~ "Partly skilled",
  jbrgsc_dv == 6 ~ "Unskilled",
  TRUE  ~  NA)) 
ukhls <- ukhls %>% 
  mutate(jbrgsc_dv = case_when(
    jbrgsc_dv == 1 ~ "Professional",
    jbrgsc_dv == 2 ~ "Managerial / technical",
    jbrgsc_dv == 3 ~ "Skilled non-manual",
    jbrgsc_dv == 4 ~ "Skilled manual",
    jbrgsc_dv == 5 ~ "Partly skilled",
    jbrgsc_dv == 6 ~ "Unskilled",
    TRUE  ~  NA)) 

bhps <- bhps %>% 
  mutate(jbnssec8_dv = case_when(
    jbnssec8_dv == 1 ~ "Large employer / higher manager",
    jbnssec8_dv == 2 ~ "Higher professional",
    jbnssec8_dv == 3 ~ "Lower manager / profesional",
    jbnssec8_dv == 4 ~ "Intermediate",
    jbnssec8_dv == 5 ~ "Small employer / own account",
    jbnssec8_dv == 6 ~ "Lower supervisory / technical",
    jbnssec8_dv == 6 ~ "Semi-routine",
    jbnssec8_dv == 6 ~ "Routine",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(jbnssec8_dv = case_when(
    jbnssec8_dv == 1 ~ "Large employer / higher manager",
    jbnssec8_dv == 2 ~ "Higher professional",
    jbnssec8_dv == 3 ~ "Lower manager / profesional",
    jbnssec8_dv == 4 ~ "Intermediate",
    jbnssec8_dv == 5 ~ "Small employer / own account",
    jbnssec8_dv == 6 ~ "Lower supervisory / technical",
    jbnssec8_dv == 6 ~ "Semi-routine",
    jbnssec8_dv == 6 ~ "Routine",
    TRUE  ~  NA))

bhps$jbsect <- ifelse(bhps$jbsect == 1, "Private",
                      ifelse(bhps$jbsect == 2, "Non-private", NA))
ukhls$jbsect <- ifelse(ukhls$jbsect == 1, "Private",
                      ifelse(ukhls$jbsect == 2, "Non-private", NA))

bhps$jbhrs <- ifelse(bhps$jbhrs < 0, NA, bhps$jbhrs)
ukhls$jbhrs <- ifelse(ukhls$jbhrs < 0, NA, ukhls$jbhrs)

bhps$jbft_dv <- ifelse(bhps$jbft_dv == 1, "FT employee",
                      ifelse(bhps$jbft_dv == 2, "PT employee", NA))
ukhls$jbft_dv <- ifelse(ukhls$jbft_dv == 1, "FT employee",
                       ifelse(ukhls$jbft_dv == 2, "PT employee", NA))

bhps <- bhps %>% 
  mutate(wktime = case_when(
    wktime == 1 ~ "Mornings only",
    wktime == 2 ~ "Afternoons only",
    wktime == 3 ~ "Day",
    wktime == 4 ~ "Evenings only",
    wktime == 5 ~ "Night",
    wktime == 6 ~ "Lunchtimes and evenings",
    wktime == 7 ~ "Other times of day",
    wktime == 8 ~ "Rotating shifts",
    wktime == 9 ~ "Varies",
    wktime == 10 ~ "Day and evenings",
    wktime == 97 ~ "Other",
    TRUE ~ NA))
ukhls <- ukhls %>% 
  mutate(wktime = case_when(
    wktime == 1 ~ "Mornings only",
    wktime == 2 ~ "Afternoons only",
    wktime == 3 ~ "Day",
    wktime == 4 ~ "Evenings only",
    wktime == 5 ~ "Night",
    wktime == 6 ~ "Lunchtimes and evenings",
    wktime == 7 ~ "Other times of day",
    wktime == 8 ~ "Rotating shifts",
    wktime == 9 ~ "Varies",
    wktime == 10 ~ "Day and evenings",
    wktime == 97 ~ "Other",
    TRUE ~ NA))

bhps$tuin1 <- ifelse(bhps$tuin1 == 1, "Yes",
                     ifelse(bhps$tuin1 == 2, "No", NA))
ukhls$tuin1 <- ifelse(ukhls$tuin1 == 1, "Yes",
                     ifelse(ukhls$tuin1 == 2, "No", NA))

bhps$tujbpl <- ifelse(bhps$tujbpl == 1, "Yes",
                     ifelse(bhps$tujbpl == 2, "No", NA))
ukhls$tujbpl <- ifelse(ukhls$tujbpl == 1, "Yes",
                      ifelse(ukhls$tujbpl == 2, "No", NA))

bhps$fimngrs_dv <- ifelse(bhps$fimngrs_dv < 0, NA, bhps$fimngrs_dv)
ukhls$fimngrs_dv <- ifelse(ukhls$fimngrs_dv < 0, NA, ukhls$fimngrs_dv)

# Contact and relationship with child
bhps <- bhps %>% 
  mutate(husits = case_when(
    husits == 1 ~ "Self",
    husits == 2 ~ "Partner",
    husits == 3 ~ "Shared",
    husits == 4 ~ "Other",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(husits = case_when(
    husits == 1 ~ "Self",
    husits == 2 ~ "Partner",
    husits == 3 ~ "Shared",
    husits == 4 ~ "Other",
    TRUE  ~  NA))

bhps$howlng <- ifelse(bhps$howlng < 0, NA, bhps$howlng)
ukhls$howlng <- ifelse(ukhls$howlng < 0, NA, ukhls$howlng)

bhps <- bhps %>% 
  mutate(leikid = case_when(
    leikid == 1 ~ "Never or rarely",
    leikid == 2 ~ "Once a month or less",
    leikid == 3 ~ "Several times a month",
    leikid == 4 ~ "Once a week",
    leikid == 5 ~ "Several times a week",
    leikid == 6 ~ "Almost every day",
    TRUE ~ NA))
ukhls <- ukhls %>% 
  mutate(socialkid = case_when(
    socialkid == 1 ~ "Never or rarely",
    socialkid == 2 ~ "Once a month or less",
    socialkid == 3 ~ "Several times a month",
    socialkid == 4 ~ "Once a week",
    socialkid == 5 ~ "Several times a week",
    socialkid == 6 ~ "Almost every day",
    TRUE ~ NA))

# Recode UKHLS only variables
ukhls$urban_dv <- ifelse(ukhls$urban_dv == 1, "Urban",
                         ifelse(ukhls$urban_dv == 2, "Rural", NA))

ukhls$wkends <- ifelse(ukhls$wkends == 1, "Often",
                         ifelse(ukhls$wkends == 2, "Sometimes",
                                ifelse(ukhls$wkends == 3, "Never", NA)))

ukhls$howlng <- ifelse(ukhls$howlng < 0, NA, ukhls$howlng)

ukhls <- ukhls %>% 
  mutate(dinner = case_when(
    dinner == 1 ~ "None",
    dinner == 2 ~ "1-2 time a week",
    dinner == 3 ~ "3-5 times a week",
    dinner == 4 ~ "6-7 times a week",
    TRUE ~ NA))

ukhls <- ukhls %>% 
  mutate(whorufam = case_when(
    whorufam == 1 ~ "Very important",
    whorufam == 2 ~ "Fairly important",
    whorufam == 3 ~ "Not very important",
    whorufam == 4 ~ "Not at all important",
    TRUE ~ NA))

ukhls$famsup <- ifelse(ukhls$famsup == 1, "Often",
                       ifelse(ukhls$famsup == 2, "Sometimes",
                              ifelse(ukhls$famsup == 3, "Never", NA)))

ukhls <- ukhls %>% 
  mutate(upset = case_when(
    upset == 1 ~ "Mother / step mother",
    upset == 2 ~ "Father / step father",
    upset == 3 ~ "Sibling",
    upset == 4 ~ "Another resident relative",
    upset == 5 ~ "Another non-resident relative",
    upset == 6 ~ "Non family member",
    TRUE  ~  NA))

ukhls <- ukhls %>% 
  mutate(argm = case_when(
    argm == 1 ~ "Most days",
    argm == 2 ~ "More than once a week",
    argm == 3 ~ "Less than once a week",
    argm == 4 ~ "Hardly ever",
    TRUE  ~  NA))

ukhls <- ukhls %>% 
  mutate(argf = case_when(
    argf == 1 ~ "Most days",
    argf == 2 ~ "More than once a week",
    argf == 3 ~ "Less than once a week",
    argf == 4 ~ "Hardly ever",
    TRUE  ~  NA))

ukhls <- ukhls %>% 
  mutate(tlkm = case_when(
    tlkm == 1 ~ "Most days",
    tlkm == 2 ~ "More than once a week",
    tlkm == 3 ~ "Less than once a week",
    tlkm == 4 ~ "Hardly ever",
    TRUE  ~  NA))

ukhls <- ukhls %>% 
  mutate(tlkf = case_when(
    tlkf == 1 ~ "Most days",
    tlkf == 2 ~ "More than once a week",
    tlkf == 3 ~ "Less than once a week",
    tlkf == 4 ~ "Hardly ever",
    TRUE  ~  NA))

ukhls <- ukhls %>% 
  mutate(eatlivu = case_when(
    eatlivu == 1 ~ "None",
    eatlivu == 2 ~ "1-2 time a week",
    eatlivu == 3 ~ "3-5 times a week",
    eatlivu == 4 ~ "6-7 times a week",
    TRUE ~ NA))

ukhls$ps9 <- ifelse(ukhls$ps9 < 0, NA, ukhls$ps9)

ukhls$ps18 <- ifelse(ukhls$ps18 < 0, NA, ukhls$ps18)

ukhls$ps21 <- ifelse(ukhls$ps21 < 0, NA, ukhls$ps21)

ukhls$ps27 <- ifelse(ukhls$ps27 < 0, NA, ukhls$ps27)

# Collapse some variables
bhps <- bhps %>% 
  mutate(educ = case_when(
    qfedhi == "Higher degree" ~ "First degree or higher",
    qfedhi ==  "First degree" ~ "First degree or higher",
    qfedhi == "Teaching qual." ~ "Other higher",
    qfedhi == "Other higher qual." ~ "Other higher",
    qfedhi == "Nursing qual." ~ "Other higher",
    qfedhi == "A Levels or eq." ~ "A levels etc",
    qfedhi == "O Levels or eq." ~ "GCSE etc",
    qfedhi == "Commercial qual. or no O Levels" ~ "Lower secondary",
    qfedhi == "CSE grade 2-5 or Scot grade 4-5" ~ "Lower secondary",
    qfedhi == "Apprenticeship" ~ "Lower secondary",
    qfedhi == "Other qual." ~ "Other qual",
    qfedhi == "No qual." ~ "No qual",
    qfedhi == "Still at school" ~ "No qual",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(educ = case_when(
    qfhigh_dv == "Higher degree" ~ "First degree or higher",
    qfhigh_dv == "First degree or eq." ~ "First degree or higher",
    qfhigh_dv == "Diploma HE" ~ "Other higher",
    qfhigh_dv == "Teaching qual." ~ "Other higher",
    qfhigh_dv == "Nursing or medical qual." ~ "Other higher",
    qfhigh_dv == "Other higher degree" ~ "Other higher",
    qfhigh_dv == "A level" ~ "A level etc",
    qfhigh_dv == "Welsh bac" ~ "A level etc",
    qfhigh_dv == "IB" ~ "A level etc",
    qfhigh_dv == "AS level" ~ "A level etc",
    qfhigh_dv == "Scottish higher" ~ "A level etc",
    qfhigh_dv == "Cert. 6th year studies" ~ "A level etc",
    qfhigh_dv == "GCSE or eq." ~ "GCSE etc",
    qfhigh_dv == "CSE" ~ "Lower secondary",
    qfhigh_dv == "Standard or lower" ~ "Lower secondary",
    qfhigh_dv == "Other" ~ "Other qual",
    qfhigh_dv == "No qual." ~ "No qual",
    TRUE  ~  NA))

bhps <- bhps %>% 
  mutate(oprlg1 = case_when(
    oprlg1 == "CoE / Anglican" ~ "CoE / Anglican",
    oprlg1 == "Roman Catholic" ~ "Roman Catholic",
    oprlg1 == "CoS" ~ "Other Protestant",
    oprlg1 =="Free Church / Free Presbyterian CoS" ~ "Other Protestant",
    oprlg1 == "Episcopalian" ~ "Other Protestant",
    oprlg1 == "Methodist" ~ "Other Protestant",
    oprlg1 == "Baptist" ~ "Other Protestant",
    oprlg1 == "Congregational / United Reform" ~ "Other Protestant",
    oprlg1 == "Other Christian" ~ "Other Christian",
    oprlg1 == "Christian no denomination" ~ "Other Christian",
    oprlg1 == "Muslim" ~ "Muslim",
    oprlg1 == "Hindu" ~ "Hindu",
    oprlg1 == "Jewish" ~ "Jewish",
    oprlg1 == "Sikh" ~ "Sikh",
    oprlg1 == "Buddhist" ~ "Buddhist",
    oprlg1 == "Other" ~ "Other",
    TRUE  ~  NA))
ukhls <- ukhls %>% 
  mutate(oprlg1 = case_when(
    oprlg1 == "CoE / Anglican" ~ "CoE / Anglican",
    oprlg1 == "Roman Catholic" ~ "Roman Catholic",
    oprlg1 == "CoS" ~ "Other Protestant",
    oprlg1 =="Free Church / Free Presbyterian CoS" ~ "Other Protestant",
    oprlg1 == "Episcopalian" ~ "Other Protestant",
    oprlg1 == "Methodist" ~ "Other Protestant",
    oprlg1 == "Baptist" ~ "Other Protestant",
    oprlg1 == "Congregational / United Reform" ~ "Other Protestant",
    oprlg1 == "Other Christian" ~ "Other Christian",
    oprlg1 == "Christian no denomination" ~ "Other Christian",
    oprlg1 == "Muslim" ~ "Muslim",
    oprlg1 == "Hindu" ~ "Hindu",
    oprlg1 == "Jewish" ~ "Jewish",
    oprlg1 == "Sikh" ~ "Sikh",
    oprlg1 == "Buddhist" ~ "Buddhist",
    oprlg1 == "Other" ~ "Other",
    TRUE  ~  NA))

bhps$relig <- ifelse(bhps$oprlg == "No", "No religion", 
                     ifelse(is.na(bhps$oprlg), NA, bhps$oprlg1))
ukhls$relig <- ifelse(ukhls$oprlg == "No", "No religion", 
                     ifelse(is.na(ukhls$oprlg), NA, ukhls$oprlg1))

bhps <- bhps[, c(1:20, 25:29)]
ukhls <- ukhls[, c(1:22, 27:44)]

# Rename variables
colnames(bhps) <- c("person_id", "hidp", "wave", "age", "sex", "region", 
                    "pid_support", "pid_closer", "pid_which", "pid_strength", "pol_interest",
                    "reggen_class", "nssec_class", "job_private", "job_hours", "job_full", "work_time", 
                    "union", "union_assoc", "income_grs",  
                    "ch_care", "housework", "ch_leisure", 
                    "educ", "relig")

colnames(ukhls) <- c("person_id", "hidp", "wave", "age", "sex", "region", "urban",
                     "pid_support", "pid_closer", "pid_which", "pid_strength", "pol_interest",
                     "reggen_class", "nssec_class", "job_private", "job_hours", "job_full", "work_time", "work_wknd",
                     "union", "union_assoc", "income_grs",  
                     "ch_care", "housework", "ch_leisure", "ch_eat",
                     "fam_important", "fam_support", "turn_to", 
                     "argue_mother", "argue_father", "talk_mother", "talk_father", 
                     "eat_parents", "ps_express", "ps_pref", "ps_opinion", "ps_warm",
                     "educ", "relig")

# Impute time-invariant characteristics
bhps <- bhps %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(sex = na.locf(sex, na.rm = F, fromLast = F)) %>% 
  mutate(sex = na.locf(sex, na.rm = F, fromLast = T)) %>%
  mutate(region = na.locf(region, na.rm = F, fromLast = F)) %>% 
  mutate(region = na.locf(region, na.rm = F, fromLast = T)) %>%
  mutate(reggen_class = na.locf(reggen_class, na.rm = F, fromLast = F)) %>%
  mutate(reggen_class = na.locf(reggen_class, na.rm = F, fromLast = T)) %>% 
  mutate(nssec_class = na.locf(nssec_class, na.rm = F, fromLast = F)) %>%
  mutate(nssec_class = na.locf(nssec_class, na.rm = F, fromLast = T)) %>%
  mutate(job_private = na.locf(job_private, na.rm = F, fromLast = F)) %>%
  mutate(job_private = na.locf(job_private, na.rm = F, fromLast = T)) %>% 
  mutate(job_hours = na.locf(job_hours, na.rm = F, fromLast = F)) %>%
  mutate(job_hours = na.locf(job_hours, na.rm = F, fromLast = T)) %>% 
  mutate(job_full = na.locf(job_full, na.rm = F, fromLast = F)) %>%
  mutate(job_full = na.locf(job_full, na.rm = F, fromLast = T)) %>% 
  mutate(work_time = na.locf(work_time, na.rm = F, fromLast = F)) %>%
  mutate(work_time = na.locf(work_time, na.rm = F, fromLast = T)) %>% 
  mutate(union = na.locf(union, na.rm = F, fromLast = F)) %>% 
  mutate(union = na.locf(union, na.rm = F, fromLast = T)) %>%
  mutate(union_assoc = na.locf(union_assoc, na.rm = F, fromLast = F)) %>% 
  mutate(union_assoc = na.locf(union_assoc, na.rm = F, fromLast = T)) %>%
  mutate(income_grs = na.locf(income_grs, na.rm = F, fromLast = F)) %>% 
  mutate(income_grs = na.locf(income_grs, na.rm = F, fromLast = T)) %>%
  mutate(ch_care = na.locf(ch_care, na.rm = F, fromLast = F)) %>% 
  mutate(ch_care = na.locf(ch_care, na.rm = F, fromLast = T)) %>%
  mutate(housework = na.locf(housework, na.rm = F, fromLast = F)) %>% 
  mutate(housework = na.locf(housework, na.rm = F, fromLast = T)) %>%
  mutate(ch_leisure = na.locf(ch_leisure, na.rm = F, fromLast = F)) %>% 
  mutate(ch_leisure = na.locf(ch_leisure, na.rm = F, fromLast = T)) %>%
  mutate(educ = na.locf(educ, na.rm = F, fromLast = F)) %>%
  mutate(educ = na.locf(educ, na.rm = F, fromLast = TRUE)) %>% 
  mutate(relig = na.locf(relig, na.rm = F, fromLast = F)) %>%
  mutate(relig = na.locf(relig, na.rm = F, fromLast = TRUE))

ukhls <- ukhls %>%
  arrange(person_id, wave) %>%
  group_by(person_id) %>%
  mutate(sex = na.locf(sex, na.rm = F, fromLast = F)) %>% 
  mutate(sex = na.locf(sex, na.rm = F, fromLast = T)) %>%
  mutate(region = na.locf(region, na.rm = F, fromLast = F)) %>% 
  mutate(region = na.locf(region, na.rm = F, fromLast = T)) %>%
  mutate(urban = na.locf(urban, na.rm = F, fromLast = F)) %>% 
  mutate(urban = na.locf(urban, na.rm = F, fromLast = T)) %>%
  mutate(reggen_class = na.locf(reggen_class, na.rm = F, fromLast = F)) %>%
  mutate(reggen_class = na.locf(reggen_class, na.rm = F, fromLast = T)) %>% 
  mutate(nssec_class = na.locf(nssec_class, na.rm = F, fromLast = F)) %>%
  mutate(nssec_class = na.locf(nssec_class, na.rm = F, fromLast = T)) %>%
  mutate(job_private = na.locf(job_private, na.rm = F, fromLast = F)) %>%
  mutate(job_private = na.locf(job_private, na.rm = F, fromLast = T)) %>% 
  mutate(job_hours = na.locf(job_hours, na.rm = F, fromLast = F)) %>%
  mutate(job_hours = na.locf(job_hours, na.rm = F, fromLast = T)) %>% 
  mutate(job_full = na.locf(job_full, na.rm = F, fromLast = F)) %>%
  mutate(job_full = na.locf(job_full, na.rm = F, fromLast = T)) %>% 
  mutate(work_time = na.locf(work_time, na.rm = F, fromLast = F)) %>%
  mutate(work_time = na.locf(work_time, na.rm = F, fromLast = T)) %>% 
  mutate(work_wknd = na.locf(work_wknd, na.rm = F, fromLast = F)) %>%
  mutate(work_wknd = na.locf(work_wknd, na.rm = F, fromLast = T)) %>% 
  mutate(union = na.locf(union, na.rm = F, fromLast = F)) %>% 
  mutate(union = na.locf(union, na.rm = F, fromLast = T)) %>%
  mutate(union_assoc = na.locf(union_assoc, na.rm = F, fromLast = F)) %>% 
  mutate(union_assoc = na.locf(union_assoc, na.rm = F, fromLast = T)) %>%
  mutate(income_grs = na.locf(income_grs, na.rm = F, fromLast = F)) %>% 
  mutate(income_grs = na.locf(income_grs, na.rm = F, fromLast = T)) %>%
  mutate(ch_care = na.locf(ch_care, na.rm = F, fromLast = F)) %>% 
  mutate(ch_care = na.locf(ch_care, na.rm = F, fromLast = T)) %>%
  mutate(housework = na.locf(housework, na.rm = F, fromLast = F)) %>% 
  mutate(housework = na.locf(housework, na.rm = F, fromLast = T)) %>%
  mutate(ch_leisure = na.locf(ch_leisure, na.rm = F, fromLast = F)) %>% 
  mutate(ch_leisure = na.locf(ch_leisure, na.rm = F, fromLast = T)) %>%
  mutate(ch_eat = na.locf(ch_eat, na.rm = F, fromLast = F)) %>% 
  mutate(ch_eat = na.locf(ch_eat, na.rm = F, fromLast = T)) %>%
  mutate(fam_important = na.locf(fam_important, na.rm = F, fromLast = F)) %>% 
  mutate(fam_important = na.locf(fam_important, na.rm = F, fromLast = T)) %>%
  mutate(fam_support = na.locf(fam_support, na.rm = F, fromLast = F)) %>% 
  mutate(fam_support = na.locf(fam_support, na.rm = F, fromLast = T)) %>%
  mutate(turn_to = na.locf(turn_to, na.rm = F, fromLast = F)) %>% 
  mutate(turn_to = na.locf(turn_to, na.rm = F, fromLast = T)) %>%
  mutate(argue_mother = na.locf(argue_mother, na.rm = F, fromLast = F)) %>% 
  mutate(argue_mother = na.locf(argue_mother, na.rm = F, fromLast = T)) %>%
  mutate(argue_father = na.locf(argue_father, na.rm = F, fromLast = F)) %>% 
  mutate(argue_father = na.locf(argue_father, na.rm = F, fromLast = T)) %>%
  mutate(talk_mother = na.locf(talk_mother, na.rm = F, fromLast = F)) %>% 
  mutate(talk_mother = na.locf(talk_mother, na.rm = F, fromLast = T)) %>%
  mutate(talk_father = na.locf(talk_father, na.rm = F, fromLast = F)) %>% 
  mutate(talk_father = na.locf(talk_father, na.rm = F, fromLast = T)) %>%
  mutate(eat_parents = na.locf(eat_parents, na.rm = F, fromLast = F)) %>% 
  mutate(eat_parents = na.locf(eat_parents, na.rm = F, fromLast = T)) %>%
  mutate(ps_express = na.locf(ps_express, na.rm = F, fromLast = F)) %>% 
  mutate(ps_express = na.locf(ps_express, na.rm = F, fromLast = T)) %>%
  mutate(ps_pref = na.locf(ps_pref, na.rm = F, fromLast = F)) %>% 
  mutate(ps_pref = na.locf(ps_pref, na.rm = F, fromLast = T)) %>%
  mutate(ps_opinion = na.locf(ps_opinion, na.rm = F, fromLast = F)) %>% 
  mutate(ps_opinion = na.locf(ps_opinion, na.rm = F, fromLast = T)) %>%
  mutate(ps_warm = na.locf(ps_warm, na.rm = F, fromLast = F)) %>% 
  mutate(ps_warm = na.locf(ps_warm, na.rm = F, fromLast = T)) %>%
  mutate(educ = na.locf(educ, na.rm = F, fromLast = F)) %>%
  mutate(educ = na.locf(educ, na.rm = F, fromLast = TRUE)) %>% 
  mutate(relig = na.locf(relig, na.rm = F, fromLast = F)) %>%
  mutate(relig = na.locf(relig, na.rm = F, fromLast = TRUE))

# Create single panel
panel_raw <- bind_rows(bhps, ukhls)

# Create cross-wave household identifier
panel_raw <- panel_raw %>%
  group_by(person_id) %>%
  arrange(wave) %>%
  mutate(house_id = first(hidp)) %>%
  ungroup()

#### Checkpoint: unique households
length(unique(panel_raw$house_id)) # 73,432 unique households

# Write panel
write_dta(panel_raw, "panel_raw.dta")

# Add columns for parent1
panel_raw$parent1_id <- NA
panel_raw$parent1_relationship <- NA_character_
panel_raw$parent1_age <- NA_character_
panel_raw$parent1_sex <- NA_character_
panel_raw$parent1_educ <- NA_character_
panel_raw$parent1_relig <- NA_character_
panel_raw$parent1_pid_support <- NA_character_
panel_raw$parent1_pid_closer <- NA_character_
panel_raw$parent1_pid_which <- NA_character_
panel_raw$parent1_pid_strength <- NA_character_
panel_raw$parent1_pol_interest <- NA_character_
panel_raw$parent1_reggen_class <- NA_character_
panel_raw$parent1_nssec_class <- NA_character_
panel_raw$parent1_job_private <- NA_character_
panel_raw$parent1_job_hours <- NA
panel_raw$parent1_job_full <- NA_character_
panel_raw$parent1_work_time <- NA_character_
panel_raw$parent1_work_wknd<- NA_character_
panel_raw$parent1_union <- NA_character_
panel_raw$parent1_union_assoc <- NA_character_
panel_raw$parent1_income_grs <- NA
panel_raw$parent1_ch_care <- NA_character_
panel_raw$parent1_housework <- NA
panel_raw$parent1_ch_leisure <- NA_character_
panel_raw$parent1_ch_eat <- NA_character_
panel_raw$parent1_ps_express <- NA
panel_raw$parent1_ps_pref <- NA
panel_raw$parent1_ps_opinion <- NA
panel_raw$parent1_ps_warm <- NA

# Add columns for parent2
panel_raw$parent2_id <- NA
panel_raw$parent2_relationship <- NA_character_
panel_raw$parent2_age <- NA_character_
panel_raw$parent2_sex <- NA_character_
panel_raw$parent2_educ <- NA_character_
panel_raw$parent2_relig <- NA_character_
panel_raw$parent2_pid_support <- NA_character_
panel_raw$parent2_pid_closer <- NA_character_
panel_raw$parent2_pid_which <- NA_character_
panel_raw$parent2_pid_strength <- NA_character_
panel_raw$parent2_pol_interest <- NA_character_
panel_raw$parent2_reggen_class <- NA_character_
panel_raw$parent2_nssec_class <- NA_character_
panel_raw$parent2_job_private <- NA_character_
panel_raw$parent2_job_hours <- NA
panel_raw$parent2_job_full <- NA_character_
panel_raw$parent2_work_time <- NA_character_
panel_raw$parent2_work_wknd <- NA_character_
panel_raw$parent2_union <- NA_character_
panel_raw$parent2_union_assoc <- NA_character_
panel_raw$parent2_income_grs <- NA
panel_raw$parent2_ch_care <- NA_character_
panel_raw$parent2_housework <- NA
panel_raw$parent2_ch_leisure <- NA_character_
panel_raw$parent2_ch_eat <- NA_character_
panel_raw$parent2_ps_express <- NA
panel_raw$parent2_ps_pref <- NA
panel_raw$parent2_ps_opinion <- NA
panel_raw$parent2_ps_warm <- NA

# Add number of parents variable
panel_raw$parent_n <- NA

# Create full panel
panel_full <- panel_raw %>%
  distinct(person_id, wave) %>%
  complete(person_id, wave) %>%
  left_join(panel_raw, by = c("person_id", "wave"))

# Subset to include only parents and children
panel_full <- panel_full %>% 
  filter(person_id %in% c(parent_ids$child_id, parent_ids$parent1_id, 
                          parent_ids$parent2_id))

# Initialise empty data frame
panel <- data.frame(matrix(ncol = 100, nrow = 0))

# Re-name columns
colnames(panel) <- colnames(panel_full)

# Add children observations
panel <- panel_full %>% 
  filter(person_id %in% parent_ids$child_id)

# Add parents to the panel
panel <- panel %>%
  left_join(parent_ids, by = c("person_id" = "child_id")) %>%
  mutate(parent1_id = parent1_id.y,
         parent1_relationship = parent1_relationship.y,
         parent2_id = parent2_id.y,
         parent2_relationship = parent2_relationship.y) %>%
  select(-parent1_id.x, -parent1_id.y, -parent1_relationship.x, -parent1_relationship.y,
         -parent2_id.x, -parent2_id.y, -parent2_relationship.x, -parent2_relationship.y) 

# Add observations for parent1
panel <- panel %>%
  left_join(panel_full %>% 
              select(person_id, wave, age, sex, educ, relig,
                     pid_support, pid_closer, pid_which, pid_strength, pol_interest, 
                     reggen_class, nssec_class, job_private, job_hours, job_full, work_time, work_wknd,
                     union, union_assoc, income_grs,
                     ch_care, housework, ch_leisure, ch_eat,
                     ps_express, ps_pref, ps_opinion, ps_warm) %>% 
              rename(parent1_age = age,
                     parent1_sex = sex,
                     parent1_educ = educ,
                     parent1_relig = relig,
                     parent1_pid_support = pid_support,
                     parent1_pid_closer = pid_closer,
                     parent1_pid_which = pid_which,
                     parent1_pid_strength = pid_strength,
                     parent1_pol_interest = pol_interest,
                     parent1_reggen_class = reggen_class,
                     parent1_nssec_class = nssec_class,
                     parent1_job_private = job_private,
                     parent1_job_hours = job_hours,
                     parent1_job_full = job_full,
                     parent1_work_time = work_time,
                     parent1_work_wknd = work_wknd,
                     parent1_union = union,
                     parent1_union_assoc = union_assoc,
                     parent1_income_grs = income_grs,
                     parent1_ch_care = ch_care,
                     parent1_housework = housework,
                     parent1_ch_leisure = ch_leisure,
                     parent1_ch_eat = ch_eat,
                     parent1_ps_express = ps_express,
                     parent1_ps_pref = ps_pref,
                     parent1_ps_opinion = ps_opinion,
                     parent1_ps_warm = ps_warm),
            by = c("parent1_id" = "person_id", "wave")) %>% 
  mutate(parent1_age = parent1_age.y,
         parent1_sex = parent1_sex.y,
         parent1_educ = parent1_educ.y,
         parent1_relig = parent1_relig.y,
         parent1_pid_support = parent1_pid_support.y,
         parent1_pid_closer = parent1_pid_closer.y,
         parent1_pid_which = parent1_pid_which.y,
         parent1_pid_strength = parent1_pid_strength.y,
         parent1_pol_interest = parent1_pol_interest.y,
         parent1_reggen_class = parent1_reggen_class.y,
         parent1_nssec_class = parent1_nssec_class.y,
         parent1_job_private = parent1_job_private.y,
         parent1_job_hours = parent1_job_hours.y,
         parent1_job_full = parent1_job_full.y,
         parent1_work_time = parent1_work_time.y,
         parent1_work_wknd = parent1_work_wknd.y,
         parent1_union = parent1_union.y,
         parent1_union_assoc = parent1_union_assoc.y,
         parent1_income_grs = parent1_income_grs.y,
         parent1_ch_care = parent1_ch_care.y,
         parent1_housework = parent1_housework.y,
         parent1_ch_leisure = parent1_ch_leisure.y,
         parent1_ch_eat = parent1_ch_eat.y,
         parent1_ps_express = parent1_ps_express.y,
         parent1_ps_pref = parent1_ps_pref.y,
         parent1_ps_opinion = parent1_ps_opinion.y,
         parent1_ps_warm = parent1_ps_warm.y) %>%
  select(-ends_with(".x"), -ends_with(".y"))

# Add observation for parent2
panel <- panel %>%
  left_join(panel_full %>% 
              select(person_id, wave, age, sex, educ, relig,
                     pid_support, pid_closer, pid_which, pid_strength, pol_interest, 
                     reggen_class, nssec_class, job_private, job_hours, job_full, work_time, work_wknd,
                     union, union_assoc, income_grs,
                     ch_care, housework, ch_leisure, ch_eat,
                     ps_express, ps_pref, ps_opinion, ps_warm) %>% 
              rename(parent2_age = age,
                     parent2_sex = sex,
                     parent2_educ = educ,
                     parent2_relig = relig,
                     parent2_pid_support = pid_support,
                     parent2_pid_closer = pid_closer,
                     parent2_pid_which = pid_which,
                     parent2_pid_strength = pid_strength,
                     parent2_pol_interest = pol_interest,
                     parent2_reggen_class = reggen_class,
                     parent2_nssec_class = nssec_class,
                     parent2_job_private = job_private,
                     parent2_job_hours = job_hours,
                     parent2_job_full = job_full,
                     parent2_work_time = work_time,
                     parent2_work_wknd = work_wknd,
                     parent2_union = union,
                     parent2_union_assoc = union_assoc,
                     parent2_income_grs = income_grs,
                     parent2_ch_care = ch_care,
                     parent2_housework = housework,
                     parent2_ch_leisure = ch_leisure,
                     parent2_ch_eat = ch_eat,
                     parent2_ps_express = ps_express,
                     parent2_ps_pref = ps_pref,
                     parent2_ps_opinion = ps_opinion,
                     parent2_ps_warm = ps_warm),
            by = c("parent2_id" = "person_id", "wave")) %>% 
  mutate(parent2_age = parent2_age.y,
         parent2_sex = parent2_sex.y,
         parent2_educ = parent2_educ.y,
         parent2_relig = parent2_relig.y,
         parent2_pid_support = parent2_pid_support.y,
         parent2_pid_closer = parent2_pid_closer.y,
         parent2_pid_which = parent2_pid_which.y,
         parent2_pid_strength = parent2_pid_strength.y,
         parent2_pol_interest = parent2_pol_interest.y,
         parent2_reggen_class = parent2_reggen_class.y,
         parent2_nssec_class = parent2_nssec_class.y,
         parent2_job_private = parent2_job_private.y,
         parent2_job_hours = parent2_job_hours.y,
         parent2_job_full = parent2_job_full.y,
         parent2_work_time = parent2_work_time.y,
         parent2_work_wknd = parent2_work_wknd.y,
         parent2_union = parent2_union.y,
         parent2_union_assoc = parent2_union_assoc.y,
         parent2_income_grs = parent2_income_grs.y,
         parent2_ch_care = parent2_ch_care.y,
         parent2_housework = parent2_housework.y,
         parent2_ch_leisure = parent2_ch_leisure.y,
         parent2_ch_eat = parent2_ch_eat.y,
         parent2_ps_express = parent2_ps_express.y,
         parent2_ps_pref = parent2_ps_pref.y,
         parent2_ps_opinion = parent2_ps_opinion.y,
         parent2_ps_warm = parent2_ps_warm.y) %>%
  select(-ends_with(".x"), -ends_with(".y"))

# Add number of parents for each child
panel <- panel %>%
  left_join(parent_ids %>% 
              select(child_id, parent_n), by = c("person_id" = "child_id"))

# Remove duplicate rows
panel <- panel %>%
  distinct()

# Rename id variable
panel <- panel %>% 
  rename(child_id = person_id)

# Remove unnecessary variables
panel <- panel %>% 
  select(-reggen_class, -nssec_class, -job_private, -job_hours, -job_full, -work_time, -union, -union_assoc, 
         -income_grs, -ch_care, -housework, -ch_leisure, -educ, -relig, -work_wknd, -ch_eat, 
         -ps_express, -ps_pref, -ps_opinion, -ps_warm)

# Create parent type variable
panel <- panel %>%
  mutate(
    parent1_type = case_when(
      parent1_sex == "Male" & parent1_relationship == "adopted son/daughter" ~ "Adoptive father",
      parent1_sex == "Male" & parent1_relationship == "natural son/daughter" ~ "Biological father",
      parent1_sex == "Male" & parent1_relationship == "other child" ~ "Other father",
      parent1_sex == "Male" & parent1_relationship == "stepson/stepdaughter" ~ "Step father",
      parent1_sex == "Female" & parent1_relationship == "adopted son/daughter" ~ "Adoptive mother",
      parent1_sex == "Female" & parent1_relationship == "natural son/daughter" ~ "Biological mother",
      parent1_sex == "Female" & parent1_relationship == "other child" ~ "Other mother",
      parent1_sex == "Female" & parent1_relationship == "stepson/stepdaughter" ~ "Step mother",
      TRUE ~ NA_character_),
    parent2_type = case_when(
      parent2_sex == "Male" & parent2_relationship == "adopted son/daughter" ~ "Adoptive father",
      parent2_sex == "Male" & parent2_relationship == "natural son/daughter" ~ "Biological father",
      parent2_sex == "Male" & parent2_relationship == "other child" ~ "Other father",
      parent2_sex == "Male" & parent2_relationship == "stepson/stepdaughter" ~ "Step father",
      parent2_sex == "Female" & parent2_relationship == "adopted son/daughter" ~ "Adoptive mother",
      parent2_sex == "Female" & parent2_relationship == "natural son/daughter" ~ "Biological mother",
      parent2_sex == "Female" & parent2_relationship == "other child" ~ "Other mother",
      parent2_sex == "Female" & parent2_relationship == "stepson/stepdaughter" ~ "Step mother",
      TRUE ~ NA_character_))

# Create pid_dummy variable
panel <- panel %>%
  mutate(
    pid_dummy = case_when(
      pid_support == "No" & pid_closer == "No" ~ 0,
      pid_support == "No" & pid_closer == "Yes" ~ 1,
      pid_support == "Yes" & pid_closer == "Inapplicable" ~ 1,
      TRUE ~ NA),
    parent1_pid_dummy = case_when(
      parent1_pid_support == "No" & parent1_pid_closer == "No" ~ 0,
      parent1_pid_support == "No" & parent1_pid_closer == "Yes" ~ 1,
      parent1_pid_support == "Yes" & parent1_pid_closer == "Inapplicable" ~ 1,
      TRUE ~ NA),
    parent2_pid_dummy = case_when(
      parent2_pid_support == "No" & parent2_pid_closer == "No" ~ 0,
      parent2_pid_support == "No" & parent2_pid_closer == "Yes" ~ 1,
      parent2_pid_support == "Yes" & parent2_pid_closer == "Inapplicable" ~ 1,
      TRUE ~ NA)) %>% 
  select(-pid_support, -pid_closer, -parent1_pid_support, -parent1_pid_closer, -parent2_pid_support, -parent2_pid_closer)

# Recode pid variable
panel <- panel %>%
  mutate(
    pid = case_when(
      pid_which == "Inapplicable" ~ "No party",
      pid_which == "Conservative" ~ "Conservative",
      pid_which == "Labour" ~ "Labour",
      pid_which == "Lib Dem" ~ "Lib Dem",
      pid_which == "Other party" ~ "Other party",
      TRUE ~ NA_character_),
    parent1_pid = case_when(
      parent1_pid_which == "Inapplicable" ~ "No party",
      parent1_pid_which == "Conservative" ~ "Conservative",
      parent1_pid_which == "Labour" ~ "Labour",
      parent1_pid_which == "Lib Dem" ~ "Lib Dem",
      parent1_pid_which == "Other party" ~ "Other party",
      TRUE ~ NA),
    parent2_pid = case_when(
      parent2_pid_which == "Inapplicable" ~ "No party",
      parent2_pid_which == "Conservative" ~ "Conservative",
      parent2_pid_which == "Labour" ~ "Labour",
      parent2_pid_which == "Lib Dem" ~ "Lib Dem",
      parent2_pid_which == "Other party" ~ "Other party",
      TRUE ~ NA)) %>% 
  select(-pid_which, -parent1_pid_which, -parent2_pid_which)

### Checkpoint: 25,959 unique children and 25,699 unique parents in 22,625 unique households
length(unique(panel$child_id)) 
length(unique(panel$parent1_id)) + length(unique(panel$parent2_id)) 
length(unique(panel$house_id))

# Remove waves 2016, 2018 and 2021 (outcome variables not measured)
panel <- panel %>% 
  filter(wave %in% c(1991:2015, 2017, 2019:2020))

# Create family type variable
panel <- panel %>%
  mutate(
    family_type = case_when(
      parent1_type == "Biological father" & parent2_type == "Biological mother" ~ "Traditional",
      parent1_type == "Biological mother" & parent2_type == "Biological father" ~ "Traditional",
      parent1_type == "Biological mother" & is.na(parent2_type) ~ "Single mother",
      parent1_type == "Biological father" & is.na(parent2_type) ~ "Single father",
      parent1_type == "Biological father" & parent2_type == "Step mother" ~ "Extended step-mother",
      parent1_type == "Step mother" & parent2_type == "Biological father" ~ "Extended step-mother",
      parent1_type == "Biological mother" & parent2_type == "Step father" ~ "Extended step-father",
      parent1_type == "Step father" & parent2_type == "Biological mother" ~ "Extended step-father",
      TRUE ~ "Other"))

# Create mother and father pid variables
panel <- panel %>%
  mutate(
    bio_mother_pid = case_when(
      parent1_type == "Biological mother" ~ parent1_pid,
      parent2_type == "Biological mother" ~ parent2_pid,
      TRUE ~ NA),
    bio_father_pid = case_when(
      parent1_type == "Biological father" ~ parent1_pid,
      parent2_type == "Biological father" ~ parent2_pid,
      TRUE ~ NA),
    step_mother_pid = case_when(
      parent1_type == "Step mother" ~ parent1_pid,
      parent2_type == "Step mother" ~ parent2_pid,
      TRUE ~ NA),
    step_father_pid = case_when(
      parent1_type == "Step father" ~ parent1_pid,
      parent2_type == "Step father" ~ parent2_pid,
      TRUE ~ NA))

# Create mother and father pid_strength variables
panel <- panel %>%
  mutate(
    bio_mother_pid_strength = case_when(
      parent1_type == "Biological mother" ~ parent1_pid_strength,
      parent2_type == "Biological mother" ~ parent2_pid_strength,
      TRUE ~ NA),
    bio_father_pid_strength = case_when(
      parent1_type == "Biological father" ~ parent1_pid_strength,
      parent2_type == "Biological father" ~ parent2_pid_strength,
      TRUE ~ NA),
    step_mother_pid_strength = case_when(
      parent1_type == "Step mother" ~ parent1_pid_strength,
      parent2_type == "Step mother" ~ parent2_pid_strength,
      TRUE ~ NA),
    step_father_pid_strength = case_when(
      parent1_type == "Step father" ~ parent1_pid_strength,
      parent2_type == "Step father" ~ parent2_pid_strength,
      TRUE ~ NA))

# Create mother and father pint variables
panel <- panel %>%
  mutate(
    bio_mother_pint = case_when(
      parent1_type == "Biological mother" ~ parent1_pol_interest,
      parent2_type == "Biological mother" ~ parent2_pol_interest,
      TRUE ~ NA),
    bio_father_pint = case_when(
      parent1_type == "Biological father" ~ parent1_pol_interest,
      parent2_type == "Biological father" ~ parent2_pol_interest,
      TRUE ~ NA),
    step_mother_pint = case_when(
      parent1_type == "Step mother" ~ parent1_pol_interest,
      parent2_type == "Step mother" ~ parent2_pol_interest,
      TRUE ~ NA),
    step_father_pint = case_when(
      parent1_type == "Step father" ~ parent1_pol_interest,
      parent2_type == "Step father" ~ parent2_pol_interest,
      TRUE ~ NA))

### Checkpoint: 25,959 unique children and 25,699 unique parents in 21,905 unique households
length(unique(panel$child_id)) 
length(unique(panel$parent1_id)) + length(unique(panel$parent2_id)) 
length(unique(panel$house_id))

# Remove respondents from Northern Ireland
panel <- panel %>% 
  filter(region != "Northern Ireland")

### Checkpoint: 23,145 unique children and 23,261 unique parents in 20,144 households
length(unique(panel$child_id))
length(unique(panel$parent1_id)) + length(unique(panel$parent2_id))
length(unique(panel$house_id))

# Save panel
write_dta(panel, "panel.dta")