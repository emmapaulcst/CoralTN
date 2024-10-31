library(dplyr)
library(readr)
library(parallel)
library(tidyr)
library(fishtree)
source("rFunctions/allDuplicated.R")
library(tibble)
library(purrr)

#### DATA ####

mat_int <- read_csv("PAPER_DATA/original_data/interaction_matrix.csv")
prey <- as.data.frame(colnames(mat_int[2:ncol(mat_int)]))
colnames(prey) <- "prey"

load("PAPER_DATA/script_output/list_mat_int_Ic.rdata")

#### CAT PREYS ####

prey_cat <- read_delim("PAPER_DATA/original_data/prey_categories.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  mutate(prey_cat = case_when(prey_cat == "benthic autotrophs" ~ "bAut",
                              prey_cat == "detritus" ~ "det",
                              prey_cat == "fish" ~ "fish",
                              prey_cat == "mobile benthic invertebrate" ~ "mInv",
                              prey_cat == "mobile macroinvertebrate" ~ "mInv",
                              prey_cat == "mobile microinvertebrate" ~ "mInv",
                              prey_cat == "sessile benthic invertebrate" ~ "sInv",
                              prey_cat == "zooplankton" ~ "zooP",
                              TRUE ~ prey_cat)) %>% 
  mutate(prey_cat = case_when(prey == "Chordata"| prey == "Mollusca" ~ NA,
                              TRUE ~ prey_cat)) %>% 
  filter(is.na(prey_cat) == F)


tibble_cat <- as.data.frame(c("bAut", "det", "fish", "mInv", "sInv", "zooP"), nm = "prey_cat")

somme_prey_cat <- function(s){
  
  tibble <- as.data.frame(t(list_mat_int[[s]][-1]))
  tibble <- rownames_to_column(tibble, var = 'prey')
  
  tibble_fluxes <- left_join(prey_cat, tibble, by = "prey")  %>% 
    rowwise() %>% 
    mutate(flux_per_prey = sum(c_across(colnames(tibble[3]):colnames(tibble[ncol(tibble)])), na.rm = T)) %>% 
    select(prey, prey_cat, flux_per_prey) %>% 
    group_by(prey_cat) %>% 
    summarise(flux = round(sum(flux_per_prey), 2)) %>% 
    filter(is.na(prey_cat) == F)
  
  tibble_fluxes <- full_join(tibble_fluxes, tibble_cat, by = "prey_cat") %>%
    pivot_wider(names_from = prey_cat, values_from = flux) %>% 
    select("bAut", "det", "fish", "mInv", "sInv", "zooP")
  
  tibble_fluxes <- tibble_fluxes %>% 
    mutate(across('bAut':'zooP', ~ case_when(is.na(.) == T ~ 0,
                                                  TRUE ~ .)))
  return(tibble_fluxes)
}

S <- length(list_mat_int)
list_col_sums <- mclapply(1:S, somme_prey_cat, mc.cores = 55)

flux <- data.frame(matrix(unlist(list_col_sums), nrow = S, byrow = TRUE), stringsAsFactors = FALSE) 
colnames(flux) <- t(tibble_cat)
flux$site <- names(list_mat_int)

flux <- flux %>% 
  select(site, 'bAut':'zooP')

flux_prop <- flux %>% 
  mutate(sum_tot = rowSums(across('bAut':'zooP'), na.rm=T)) %>% 
  mutate(across('bAut':'zooP', ~ .x/sum_tot)) %>% 
  rename("bAut_prop" = "bAut", "det_prop" = "det", "fish_prop" = "fish", "mInv_prop" = "mInv", 
         "sInv_prop" = "sInv", "zooP_prop" = "zooP")

flux <- left_join(flux, flux_prop, by = "site") %>% 
  select(site, 'bAut':'zooP', sum_tot, "bAut_prop":"zooP_prop")

#write.csv(flux, "PAPER_DATA/script_output/flux_per_prey_Ic.csv", row.names = F)

# More information on fluxes #

# mean <- flux_prop %>% 
#   summarise(across(bAut_prop:zooP_prop, ~ round(mean(.x, na.rm = T), 2))) %>% 
#   pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to = "mean")
# 
# median <- flux_prop %>%
#   summarise(across(bAut_prop:zooP_prop, ~ round(median(.x, na.rm = T), 2))) %>% 
#   pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =  "median")
# 
# max <- flux_prop %>%
#   summarise(across(bAut_prop:zooP_prop, ~ round(max(.x, na.rm = T), 2))) %>% 
#   pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =   "max")
# 
# min <- flux_prop %>%
#   summarise(across(bAut_prop:zooP_prop, ~ round(min(.x, na.rm = T), 2))) %>% 
#   pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =  "min")
# 
# sd <- flux_prop %>%
#   summarise(across(bAut_prop:zooP_prop, ~ round(sd(.x, na.rm = T), 2))) %>% 
#   pivot_longer(cols = bAut_prop:zooP_prop, names_to = "flux", values_to =  "sd")
# 
# tab <- list(mean, median, max, min, sd) %>%
#   purrr::reduce(inner_join, by = "flux")
