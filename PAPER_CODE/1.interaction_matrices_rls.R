library(dplyr)
library(tidyr)
library(rlist)
library(readr)
library(parallel)
library(beepr)

#### DATA ####

## fish
fish <- read_csv("data/rls_fish_flux.csv")

# Select randomly one transect per site
transect <- fish %>% group_by(SiteCode, SurveyID) %>% summarise() %>% arrange(SiteCode, SurveyID)
transect$dup <- duplicated(transect$SiteCode)
transect <- transect %>% filter(dup == F)
#write.csv(transect, "PAPER_DATA/script_output/selection_surveyID.csv", row.names = F)

# Extract the data corresponding to these transects
fish <- semi_join(fish, transect, by = "SurveyID")

# site
site <- fish %>%
  group_by(SiteCode) %>%
  summarise()

# number of sites
s <- nrow(site)

## mat int 
mat_int <- read_csv("data/interaction_matrix.csv")

#### INTERACTION MATRICES ####

gives_mat_int <- function(s){
  
  species <- as.data.frame(fish %>%
                             filter(SiteCode == as.character(site[s,1])) %>%
                             group_by(species) %>%
                             summarise())
  
  sub_mat_int <- semi_join(mat_int, species, by = c("predator" = "species"))
  
  return(sub_mat_int)
}

list_mat_int <- mclapply(1:s, gives_mat_int, mc.cores = 55)
names(list_mat_int) <- site$SiteCode[1:s]

#### REMOVE EMPTY COL/ROW ####

rmv_colrow_NA <- function(mat_int_site){
  if(is.null(mat_int_site) == F){
    if(nrow(mat_int_site) == 0){
      mat_int_site <- NULL
    }
    else{
      # col with NA only      
      col <- vector()
      for (j in 2:ncol(mat_int_site)){
        k <- 0
        for (i in 1:nrow(mat_int_site)){
          if (is.na(mat_int_site[i,j]) == F){
            k <- k+1
            break
          }
        }
        if (k == 0){
          col <- c(col, j)
        }
      }
      
      # row with NA only
      row <- vector()
      for (i in 1:nrow(mat_int_site)){
        k <- 0
        for (j in 2:ncol(mat_int_site)){
          if (is.na(mat_int_site[i,j]) == F){
            k <- k+1
            break
          }
        }
        if (k == 0){
          row <- c(row, i)
        }
      }
      
      # remove empty col/row
      if (length(col) == ncol(mat_int_site)-1 | length(row) == nrow(mat_int_site)){
        mat_int_site <- NULL
      }
      else {
        if (length(col) != 0){
          mat_int_site <- mat_int_site[c(-col)]
        }
        if (length(row) != 0){
          mat_int_site <- mat_int_site[-row,]
        }
      }
    }
    return(mat_int_site)
  }
}

list_mat_int_rmv <- mclapply(list_mat_int[1:s], rmv_colrow_NA, mc.cores = 55)

#### WEIGHT MATRICES WITH C FLUXES ####

###### Assess C fluxes ####
 
total_flux_per_sp_per_site <- fish %>%
  group_by(SiteCode, Site, species) %>%
  summarise(total_flux = sum(Ic_median)) %>%
  filter(is.na(total_flux) == F)

###### Weight matrices ####

list_mat_int_rmv_w <- list()

for(l in 1:s){
  if(is.null(list_mat_int_rmv[[l]]) == F){
    
    sub_list_ponderation <- total_flux_per_sp_per_site %>% 
      filter(SiteCode == as.character(names(list_mat_int_rmv[l])))
    
    duh <- inner_join(list_mat_int_rmv[[l]], sub_list_ponderation, by = c("predator" = "species")) %>% 
      select(-SiteCode, -Site)
    
    duh <- duh %>% 
      mutate(across(colnames(duh[2]):colnames(duh[ncol(duh)-1]), ~ .x*total_flux)) %>% 
      select(-total_flux)
    
    list_mat_int_rmv_w[[l]] <- duh
  }
}
names(list_mat_int_rmv_w) <- site$SiteCode[1:s]

###### Remove empty col/row #####
list_mat_int_rmv_w_rmv <- mclapply(list_mat_int_rmv_w[1:s], rmv_colrow_NA, mc.cores = 55)

###### List of interactions matrices ####

list_mat_int <- list_mat_int_rmv_w_rmv
#save(list = "list_mat_int", file = 'PAPER_DATA/script_output/list_mat_int_Ic.rdata')



