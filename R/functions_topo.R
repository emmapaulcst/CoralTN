
get_mat_int <- function(path){
  mat_int <- load(path) 
  mat_int <- get(mat_int)
  #mat_int <- as.data.frame(mat_int[[1]])
  return(mat_int)
}

# test <- get_mat_int("data/list_mat_int_Ic.rdata")

complexity <- function(tibble){
  #number of nodes
  S <- round(nrow(tibble) + ncol(tibble) - 1, 4)      
  S_prey <- round(ncol(tibble) - 1, 4)
  S_pred <- round(nrow(tibble), 4)
  
  #number of links
  L <- 0
  for (j in 1:ncol(tibble)){
    for (i in 1:nrow(tibble)){
      if (is.na(tibble[i,j]) == F){
        L <- L+1
      }
    }
  }
  L <- round(L, 4)
  
  #link density
  D <- round(L/S, 4)
  
  #connectance
  C <- round(L/(nrow(tibble)*(ncol(tibble)-1)), 4)
  
  complexity <- c(S, S_prey, S_pred, L, D, C)
  
  return(complexity)
}

# test <- complexity(test)


complexity_df <- function(list_mat_int){
  topo_list <- mclapply(list_mat_int[1:562], complexity, mc.cores = 52)
  site <- names(topo_list)

  topo_df <- data.frame(matrix(unlist(topo_list), nrow = 562, byrow = TRUE), stringsAsFactors = FALSE)
  topo_df$SiteCode <- site

  topo_df <- topo_df %>%
    rename(S = X1, Sprey = X2, Spred = X3, L = X4, D = X5, C = X6,
           # G = X7, sG = X8, V = X9, sV = X10,
           # Cc = X11, nCc = X12, Bc = X13, nBc = X14,
           # NODF2 = X15, weighted_NODF = X16,
           # Qb = X17, norm_Qb = X18, nb_of_modules = X19, Qn = X20
           ) %>%
    select(SiteCode, S:C) %>%
    arrange(SiteCode)
}