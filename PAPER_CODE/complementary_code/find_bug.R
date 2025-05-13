get_fit_final <- function(path_to_fit){
  fit_final <- read_csv(path_to_fit)
  
  return(fit_final)
}

get_data <- function(path_to_data){
  data <- read_csv(path_to_data)
  
  return(data)
}

get_old_topo <- function(path_to_topo){
  topo <- read_csv(path_to_topo)
  
  return(topo)
}

get_old_list_mat_int <- function(path_to_old_list_mat_int){
  old_list_mat_int <- load(path_to_old_list_mat_int) 
  old_list_mat_int <- get(old_list_mat_int)
  # old_list_mat_int <- load(path_to_old_list_mat_int)
  
  return(old_list_mat_int)
}

# tar_load(data)
# tar_load(data_sem)
# 
# 
# # env
# sem_env <- data_sem %>% select(dhw:gravity)
# env <- data %>% select(dhw:gravity)
# 
# all.equal(sem_env, env)
# 
# # benthos
# sem_benthos <- data_sem %>% select(coral:turf)
# benthos <- data %>% select(coral:turf)
# 
# all.equal(sem_benthos, benthos)
# 
# # topo
# sem_topo <- data_sem %>% select(S, C, Qn, N)
# topo <- data %>% select(S, C, Qn, N)
# bga_qn <- bga %>% ungroup() %>%
#   inner_join(., site_557, by = "SiteCode") %>%
#   select(S, C, Qn, NODF2) %>% rename(N = NODF2)
# 
# all.equal(sem_topo, topo)
# 
# # topo
# sem_flux <- data_sem %>% select(bAut:zooP)
# flux <- data %>% select(bAut:zooP)
# 
# all.equal(sem_flux, flux)
# 
# site_557 <- data %>% select(SiteCode)

# Component “Qn”: Mean relative difference: 0.06622463"