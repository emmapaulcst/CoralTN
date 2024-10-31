library(dplyr)
library(tidyr)
library(rlist)
library(readr)
library(parallel)
library(purrr)
source("rFunctions/allDuplicated.R")

#### DATASETS ####

# SELECTED TRANSECTS #
transect <- read_csv("PAPER_DATA/script_output/selection_surveyID.csv") %>% select(-dup)

# TOPOLOGY #
data_topo <- read_csv("PAPER_DATA/script_output/topology.csv")
data_topo <- left_join(transect, data_topo, by = "SiteCode")
site <- data_topo %>% select(SurveyID)

# BENTHOS #

data_benthos <- read_csv("PAPER_DATA/original_data/rls_benthos.csv", col_types = "ccnnnnnnnnnnnnnnnnnnnn") %>% 
  select(-"0", - "#N/A" )

benthos <- semi_join(data_benthos, site, by = "SurveyID")  %>% 
  select("SiteCode", "Macroalgae", "Stony corals", "Substrate") %>% 
  mutate(across('Macroalgae':'Substrate',
                ~ case_when(is.na(.) == T ~ 0, TRUE ~ .)))

# PREDICTORS #
data_predictors <- read_csv("PAPER_DATA/original_data/rls_predictors.csv")
predictors <- semi_join(data_predictors, site, by = "SurveyID")

site_info <- predictors[1:12] %>%
  select(SiteCode, SiteLatitude:SiteLongitude, Country:Location, MPA)

env <- predictors %>% 
  select(SiteCode, mean_chl_1year, mean_DHW_1year, mean_npp_1year, mean_pH_1year, 
         mean_sss_1year, mean_sst_1year, mean_wave, Branch_space, Rugosity, coral, algae, turf)

anthro <- predictors %>% 
  select(SiteCode, Naturalresourcesrents, MarineEcosystemDependency, 
         neartt, poptot, gravtot1, gravtot2, gravtot3)

#### ASSEMBLE ####

bga <- list(data_topo, site_info, env, anthro, benthos) %>%
  purrr::reduce(inner_join, by = "SiteCode") %>% 
  filter(is.na(Branch_space) == F)

#write.csv(bga, "PAPER_DATA/script_output/bga.csv", row.names = F)
