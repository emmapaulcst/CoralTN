#### LIBRARIES ####

# library(readr)
# library(dplyr)
# library(ggplot2)
# library(magick)
# library(viridis)
# library(brms)
# 
# library(tidyr)
# library(purrr)
# library(vegan)
# library(bipartite)
# library(patchwork)
# library(rlist)
# library(network)
# library(sna)
# library(ergm)
# library(DiagrammeR)
# library(rsvg)
# library(cowplot)
# library(FactoMineR)
# library(Factoshiny)
# 
# library(rworldmap)
# library(ggrepel)
# 
# library(ggfortify)
# library(ggConvexHull)
# library(ggpubr)
# library(ggcorrplot)
# library(corrplot)
# library(ggpmisc)
# library(GGally)
# library(ggbreak) 
# 
# library(rworldmap)
# library(maps)
# 
# library(DiagrammeRsvg)
# library(htmltools)
# library(svglite)
# library(grid)
# library(khroma)


#### MAKE SUPP 3 - PCA ####

makeSupp3_z <- function(bga){
  
  .archi_bga <- bga %>% 
    rename(N = NODF2, 
           Od = G, 
           Id = V,
           Region = Realm) %>%    
    mutate(Region_short = case_when(Region == "Tropical Eastern Pacific" ~ "TEP",
                                    Region == "Tropical Atlantic" ~ "TA", 
                                    Region == "Eastern Indo-Pacific" ~ "EIP",
                                    Region == "Central Indo-Pacific" ~ "CIP",
                                    Region == "Western Indo-Pacific" ~ "WIP")) %>% 
    mutate(Region = base::factor(Region, levels = c("Tropical Eastern Pacific",
                                                    "Tropical Atlantic", 
                                                    "Eastern Indo-Pacific", 
                                                    "Central Indo-Pacific", 
                                                    "Western Indo-Pacific"))) %>% 
    mutate(Region_short = base::factor(Region_short, levels = c("TEP",
                                                                "TA", 
                                                                "EIP", 
                                                                "CIP", 
                                                                "WIP"))) %>% 
    ungroup()
  
  .archi <- .archi_bga %>% 
    select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           # N = (log(N+1) - mean(log(N))) / sd(log(N+1)),
           # Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1)),
           zN = (zN - mean(zN)) / sd(zN),
           zQn = (zQn - mean(zQn)) / sd(zQn)) %>% 
    rename(N = zN, Qn = zQn)
  
  pca_res <- prcomp(.archi[4:11], scale. = TRUE) 
  (pca_all_z <- autoplot(pca_res, data = .archi, size = 2, colour = 'Region', fill = 'Region', alpha = 0.7, 
                   loadings = TRUE, loadings.colour = 'grey15', 
                   loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            title = element_text(size = 20),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.position = "right") +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")))
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp3_pcaArchiAll_z.png", dpi = 150, unit = "px", width=1500, height=1200)
  
  return(pca_all_z)
}

# Supp3 <- makeSupp3_z(bga)

#### MAKE SUPP 4 - CORR ####

makeSupp4_z <- function(bga){
  
  bga <- bga %>%
    rename(N = NODF2, 
           Od = G, 
           Id = V,
           Region = Realm) %>%    
    mutate(Region_short = case_when(Region == "Tropical Eastern Pacific" ~ "TEP",
                                    Region == "Tropical Atlantic" ~ "TA", 
                                    Region == "Eastern Indo-Pacific" ~ "EIP",
                                    Region == "Central Indo-Pacific" ~ "CIP",
                                    Region == "Western Indo-Pacific" ~ "WIP")) %>% 
    mutate(Region = base::factor(Region, levels = c("Tropical Eastern Pacific",
                                                    "Tropical Atlantic", 
                                                    "Eastern Indo-Pacific", 
                                                    "Central Indo-Pacific", 
                                                    "Western Indo-Pacific"))) %>% 
    mutate(Region_short = base::factor(Region_short, levels = c("TEP",
                                                                "TA", 
                                                                "EIP", 
                                                                "CIP", 
                                                                "WIP"))) %>% 
    ungroup()
  
  archi <- bga %>% 
    select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, zN, zQn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           # L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           # Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           # Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           # Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           
           # S = (S - mean(S)) / sd(S),
           # C = (C - mean(C)) / sd(C),
           
           zN = (zN - mean(zN)) / sd(zN),
           zQn = (zQn - mean(zQn)) / sd(zQn)) %>% 
    rename(N = zN, Qn = zQn)
  
  mcor_archi = cor(archi[4:11])
  p.mat_archi <- cor_pmat(mcor_archi)
  
  archi_corrplot <- ggcorrplot(mcor_archi, 
                           hc.order = F,
                           outline.col = "white", 
                           type = "lower",
                           method = "circle",
                           lab = T, lab_size = 3,
                           p.mat = p.mat_archi,
                           insig = "blank",
                           ggtheme = ggplot2::theme_minimal,
                           colors = c("#619CFF", "white", "#F8766D"),
                           legend.title = "Correlation") +
    theme(plot.background = element_rect(fill = "white", color = "white"),
          title = element_text(size = 20),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.position = "right")
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp4_corr_z.png", dpi = 150, unit = "px", width = 1000, height = 750)
  
  return(archi_corrplot)
}

# Supp4 <- makeSupp4_z(bga)


#### MAKE SUPP 6 & 7- PPCHECKS ####

plot_ppchecks_z <- function(fit_z_sem){

  resp <- c("coral", "turf", "algae",
            "S", "C", "zN", "zQn",
            "bAut", "det", "zooP", "sInv", "mInv", "fish" )
  legend <- c("Coral", "Turf", "Algae",
              "Node number", "Connectance", "Nestedness", "Modularity",
              "Benthic autotrophs", "Detritus", "Zooplankton", "Sessile invertebrates", "Mobile invertebrates", "Fish")
  
  # dens_overlay
  for (i in 1:length(resp)){
    (ppCheck_plot <- pp_check(fit_z_sem, resp = as.character(resp[i]), type = "dens_overlay") +
       labs(x = as.character(legend[i]), y = "Density", title = paste("Posterior predictive distribution of", as.character(legend[i]))))
    
    ggsave(ppCheck_plot, filename = paste("PAPER_FIGS/script_output_figs/Supp/Supp6_ppCheck/ppCheck_DensOverlay_", as.character(resp[i]),"_z.png", sep=""),
           dpi = 150, unit = "px", width = 1000, height = 500)
  }
  
  # scatter average
  for (i in 1:length(resp)){
    (ppCheck_plot <- pp_check(fit_z_sem, resp = as.character(resp[i]), type = "scatter_avg") +
       labs(title = paste("Scatterplot between", as.character(legend[i]), "data and the average value of \n the posterior predictive distribution of each data point")))
    ggsave(ppCheck_plot, filename = paste("PAPER_FIGS/script_output_figs/Supp/Supp7_ppCheck/ppCheck_ScatterAvg_", as.character(resp[i]),"_z.png", sep=""), 
           dpi = 150, unit = "px", width = 1000, height = 500)
  }
}

# ppChecks <- plot_ppchecks()

#### MAKE SUPP 8 - CAT ####

tar_load(bga)

makeSupp8_z <- function(bga){
  
  archi_bga <- bga %>% 
    rename(N = NODF2, 
           Od = G, 
           Id = V,
           Region = Realm) %>%    
    mutate(Region_short = case_when(Region == "Tropical Eastern Pacific" ~ "TEP",
                                    Region == "Tropical Atlantic" ~ "TA", 
                                    Region == "Eastern Indo-Pacific" ~ "EIP",
                                    Region == "Central Indo-Pacific" ~ "CIP",
                                    Region == "Western Indo-Pacific" ~ "WIP")) %>% 
    mutate(Region = base::factor(Region, levels = c("Tropical Eastern Pacific",
                                                    "Tropical Atlantic", 
                                                    "Eastern Indo-Pacific", 
                                                    "Central Indo-Pacific", 
                                                    "Western Indo-Pacific"))) %>% 
    mutate(Region_short = base::factor(Region_short, levels = c("TEP",
                                                                "TA", 
                                                                "EIP", 
                                                                "CIP", 
                                                                "WIP"))) %>% 
    ungroup()
  
  data <- archi_bga %>% 
    mutate(sum_benthos = coral + algae + turf) %>%
    filter(sum_benthos <= 100) %>% 
    mutate(coral_prop = (coral/sum_benthos)*100,
           algae_prop = (algae/sum_benthos)*100,
           turf_prop = (turf/sum_benthos)*100) %>%
    mutate(coral_max = sum_benthos - coral,
           algae_max = sum_benthos - algae,
           turf_max = sum_benthos - turf) %>%
    select(SiteCode, Region,
           coral:turf, 
           coral_prop:turf_prop,
           sum_benthos,
           coral_max:turf_max,
           S, C, zN, zQn) %>% 
    rename(N = zN, Qn = zQn)
  
  .top_coral <- data %>%
    arrange(coral_prop) %>%
    top_n(28, coral_prop) %>%
    mutate(tiptop = "coral")
  .top_algae <- data %>%
    arrange(algae_prop) %>%
    top_n(28, algae_prop) %>%
    mutate(tiptop = "algae")
  .top_turf <- data %>%
    arrange(turf_prop) %>%
    top_n(28, turf_prop) %>%
    mutate(tiptop = "turf")
  
  top <- rbind(.top_coral, .top_algae, .top_turf) %>%
    select(SiteCode, Region, tiptop, S, C, N, Qn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           N = (N - mean(N)) / sd(N),
           Qn = (Qn - mean(Qn)) / sd(Qn)) %>% 
    select(SiteCode, Region, tiptop, S, C, N, Qn)
  
  pca_res <- prcomp(top[4:7], scale. = TRUE)
  
  (p <- autoplot(pca_res, data = top, colour = 'tiptop', fill = 'tiptop',
                 loadings = TRUE, loadings.colour = 'grey15',
                 loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      scale_color_manual(values = c("forestgreen", "coral2", "cadetblue3")) +
      scale_fill_manual(values = c("forestgreen", "coral2", "cadetblue3")) +
      guides(fill = guide_legend(title = "Dominant cover"), colour = guide_legend(title = "Dominant cover")) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = "white"),
            title = element_text(size = 20),
            axis.title = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.position = "right"))
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp8_pcaCAT_z.png", dpi = 150, unit = "px", width = 1000, height = 750)
}

# Supp8 <- makeSupp8()

#### MAKE SUPP 9 - KER ####

makeSupp9_z  <- function(flux_per_prey_Ic){
  
  flux <- flux_per_prey_Ic %>%
    select(site, bAut_prop:zooP_prop) %>%
    rename("bAut" = "bAut_prop",
           "det" = "det_prop",
           "fish" = "fish_prop",
           "mInv" = "mInv_prop",
           "sInv" = "sInv_prop",
           "zooP" = "zooP_prop",
           "SiteCode" = "site")
  
  longer_flux <- as.data.frame(flux %>% 
                                 pivot_longer(cols = bAut:zooP, names_to = "flux_cat", values_to = "flux_prop") %>% 
                                 mutate(flux_cat = as.factor(flux_cat)) %>% 
                                 select(flux_cat, flux_prop))
  
  ker <- ggplot(longer_flux) + 
    geom_density(aes(x = flux_prop, fill = flux_cat, color = flux_cat), alpha = 0.4) +
    scale_color_manual(values = c("#00BA38", "#B38683", "#619CFF","#00BFC4", "#F8766D", "#F564E3")) +
    scale_fill_manual(values = c("#00BA38", "#B38683", "#619CFF","#00BFC4", "#F8766D", "#F564E3")) +
    labs(title = "", x = "Proportion of Carbon flowing through major pathways", y = "Density", color = "Carbon pathways", fill = "Carbon pathways") +
    theme_minimal() +
    theme(plot.margin = margin(0.1,0.1,0.1,0.1, 'cm'),
          axis.text = element_text(size=12),
          axis.title = element_text(size=12),
          plot.title = element_text(size=14),
          legend.title = element_text(size=14),
          legend.text = element_text(size= 12),
          legend.position = "right")
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp9_kerDensity_z.png", dpi = 150, unit = "px", width = 1000, height = 750)
}

# Supp9 <- makeSupp9()
