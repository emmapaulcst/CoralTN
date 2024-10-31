library(dplyr)
library(tidyr)
library(readr)
library(parallel)
library(R.utils)
source("rFunctions/allDuplicated.R")


#### DATA ####
# CNP fluxes
fluxes_CNP <- read_csv("PAPER_DATA/original_data/fluxes_CNP.csv") %>%
  mutate(size_fishflux = as.numeric(round(size, digits = 0), .keep = "unused")) %>% 
  select(species, size_fishflux, temperature:Ip_median)

# RLS fish
fish <- read_csv("PAPER_DATA/original_data/rls_fish.csv") %>% 
  mutate(TL = as.numeric(TL))

# RLS SST
sst <- read_csv("PAPER_DATA/original_data/rls_predictors.csv") %>% 
  select(SurveyID, mean_sst_1year) %>% 
  mutate(temperature = round(mean_sst_1year, digits = 0), .keep = "unused") 

# Combine fish and SST
fish <- left_join(fish, sst, by = "SurveyID") %>% 
  mutate(size_rls = round(TL, digits = 0), .keep = "unused") %>% 
  rename(species = species_name) %>% 
  select(SiteCode:species, size_rls, temperature, Sum_abundance:Sum_biomass)

fish_sp <- fish %>% group_by(species) %>% summarise()
fish_sp_fishflux <- fluxes_CNP %>% group_by(species) %>% summarise()
all <- semi_join(fish_sp, fish_sp_fishflux, by = c("species"))

#### FLUX ####

##### Which sizes are missing ? ####

## quite long to run
## use the csv instead
fish_size_all <- read_csv("PAPER_DATA/script_output/fish_size_all.csv")

fish_size_fishflux <- fluxes_CNP %>%
  group_by(species, size_fishflux) %>%
  summarise() %>%
  arrange(species, size_fishflux)

fish_size_rls <- fish %>%
  group_by(species, size_rls) %>%
  summarise() %>%
  arrange(species, size_rls)

pfioupfiou <- vector()
for (i in 1:nrow(fish_size_rls)){
  fish_sp <- fish_size_rls$species[i]
  size_rls <- fish_size_rls$size_rls[i]

  pfiou <- fish_size_fishflux %>%
    filter(species == as.character(fish_sp) & size_fishflux == as.character(size_rls))

  if (nrow(pfiou) == 0){
    pfioupfiou <- rbind(pfioupfiou, NA)
  } else {
    pfioupfiou <- rbind(pfioupfiou, as.character(size_rls))
  }
}

fish_size_all <- cbind(fish_size_rls, pfioupfiou)
fish_size_all <- fish_size_all %>%
  rename(size_fishflux = `...3` ) %>%
  arrange(species, size_rls, size_fishflux) %>%
  mutate(size_rls = as.numeric(size_rls), size_fishflux = as.numeric(size_fishflux))
#write.csv(fish_size_all, "PAPER_DATA/script_output/fish_size_all.csv", row.names = F)

##### Replace missing size by the closest size ####
fish <- left_join(fish, fish_size_all, by = c("species", "size_rls")) %>% 
  select(SiteCode:size_rls, size_fishflux, temperature:Sum_biomass) %>% 
  arrange(species, size_rls, temperature)

fish_sp <- fish %>% group_by(species) %>% summarise()

new_size <- function(i){
    subset_fish <- fish %>% filter(species == as.character(fish_sp$species[i]))
    subset_fish_size_rls <- fish_size_all %>%  filter(species == as.character(fish_sp$species[i]))
    
    min <- min(subset_fish_size_rls$size_fishflux, na.rm = T)
    max <- max(subset_fish_size_rls$size_fishflux, na.rm = T)
    
    subset_fish_size_rls <- subset_fish_size_rls %>% 
      mutate(size_rls_new = case_when(
        is.na(size_fishflux) == T & size_rls < min ~ min,
        is.na(size_fishflux) == T & size_rls > max ~ max,
        TRUE ~ size_rls
      ))
    
    test <- left_join(subset_fish, subset_fish_size_rls, by = c("species", "size_rls", "size_fishflux")) %>% 
      select(SiteCode:size_fishflux, size_rls_new, temperature:Sum_biomass) %>% 
      arrange(species, size_rls, temperature)
        return(test)
}

list_fish_new_size <- mclapply(1:nrow(fish_sp), new_size, mc.cores = 50)

fish_new_size <- data.frame()
for (i in 1:nrow(fish_sp)){
  fish_new_size <- rbind(fish_new_size, list_fish_new_size[[i]])
}

##### Assign fluxes to new TLS ####

fish_new_size_fluxes <- left_join(fish_new_size, fluxes_CNP, by = c("species", "size_rls_new" = "size_fishflux", "temperature"))

fish_new_size_fluxes <- fish_new_size_fluxes %>% 
  select(SiteCode:Sum_biomass, Ic_median:Ip_median)

fish_new_size_fluxes <- fish_new_size_fluxes %>% 
  mutate(Ic_median = Ic_median * Sum_abundance,
         In_median = In_median * Sum_abundance,
         Ip_median = Ip_median * Sum_abundance)

#write.csv(fish_new_size_fluxes, "PAPER_DATA/script_output/rls_fish_flux.csv", row.names = F)


