#### LIBRARIES ####

library(readr)
library(dplyr)
library(ggplot2)
library(magick)
library(viridis)
library(brms)

library(tidyr)
library(purrr)
library(vegan)
library(bipartite)
library(patchwork)
library(rlist)
library(network)
library(sna)
library(ergm)
library(DiagrammeR)
library(rsvg)
library(cowplot)
library(FactoMineR)
library(Factoshiny)

library(rworldmap)
library(ggrepel)

library(ggfortify)
library(ggConvexHull)
library(ggpubr)
library(ggcorrplot)
library(corrplot)
library(ggpmisc)
library(GGally)
library(ggbreak) 

library(rworldmap)
library(maps)

library(DiagrammeRsvg)
library(htmltools)
library(svglite)
library(grid)
library(khroma)


#### MAKE SUPP 2 - MAP ####

makeSupp2 <- function(){
world_map <- map_data("world") %>% 
  filter(lat > -50 & lat < 50)

.archi_bga <- read_csv("/home/emma/article_memoire/PAPER_DATA/script_output/bga.csv") %>% 
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
                                                              "WIP")))
(ggplot() +
   geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey90") +
   geom_point(data = .archi_bga, aes(x = SiteLongitude, y = SiteLatitude, color = Region_short, alpha = 0.4), size = 3, alpha = 0.8) +
   scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
   scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
   theme(panel.background = element_rect(fill = "lightblue"),
         title = element_text(size = 20),
         axis.title = element_text(size = 14),
         legend.title = element_text(size = 14),
         legend.text = element_text(size = 12),
         axis.text = element_text(size = 12),
         legend.position = "top") +
   coord_fixed() +
   labs(x = "Longitude", y = "Latitude", color = "Regions"))

ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp2_map.png", dpi = 150, unit = "px", width = 1500, height = 750)
}

Supp2 <- makeSupp2()

#### MAKE SUPP 3 - PCA ####

makeSupp3 <- function(){
  
  .archi_bga <- read_csv("/home/emma/article_memoire/PAPER_DATA/script_output/bga.csv") %>% 
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
                                                                "WIP")))
  
  .archi <- .archi_bga %>% 
    select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, N, Qn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           N = (log(N+1) - mean(log(N))) / sd(log(N+1)),
           Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1)))
  
  pca_res <- prcomp(.archi[4:11], scale. = TRUE) 
  (pca <- autoplot(pca_res, data = .archi, size = 2, colour = 'Region', fill = 'Region', alpha = 0.7, 
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
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp3_pcaArchiAll.png", dpi = 150, unit = "px", width=1500, height=1200)
}

Supp3 <- makeSupp3()

#### MAKE SUPP 4 - CORR ####

makeSupp4 <- function(){
  
  .archi_bga <- read_csv("/home/emma/article_memoire/PAPER_DATA/script_output/bga.csv") %>% 
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
                                                                "WIP")))
  
  .archi <- .archi_bga %>% 
    select(SiteCode, Region, Region_short, S, L, C, Bc, Od, Id, N, Qn) %>% 
    mutate(S = (log(S+1) - mean(log(S+1))) / sd(log(S+1)),
           L = (log(L+1) - mean(log(L+1))) / sd(log(L+1)),
           C = (log(C+1) - mean(log(C+1))) / sd(log(C+1)),
           Bc = (log(Bc+1) - mean(log(Bc+1))) / sd(log(Bc+1)),
           Od = (log(Od+1) - mean(log(Od+1))) / sd(log(Od+1)),
           Id = (log(Id+1) - mean(log(Id+1))) / sd(log(Id+1)),
           N = (log(N+1) - mean(log(N))) / sd(log(N+1)),
           Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1)))
  
  mcor_archi = cor(.archi[4:11])
  p.mat_archi <- cor_pmat(mcor_archi)
  
  archi_plot <- ggcorrplot(mcor_archi, 
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
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp4_corr.png", dpi = 150, unit = "px", width = 1000, height = 750)
  
}

Supp4 <- makeSupp4()


#### MAKE SUPP 5 - DAG ####

plotDAG <- function(){
  
  ##### node ####
  
  node <- as.data.frame(
    rbind("bAut", "det", "fish", "mInv", "sInv", "zooP",
          "S", "C", "N", "Qn", 
          "Coral", "Algae", "Turf",
          "Grav.", "NPP", "SST", "DHW")) %>% 
    rename(label = V1)
  
  node <- node %>% 
    mutate(label = factor(node$label, 
                          levels = c("S", "C", "N", "Qn", 
                                     "bAut", "det", "fish", "mInv", "sInv", "zooP",
                                     "Coral", "Algae", "Turf",
                                     "Grav.", "NPP", "SST", "DHW"))) %>% 
    mutate(type = case_when(
      label == "DHW" | label == "SST" | label == "NPP" | label == "Grav." ~ 1,
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
      label == "S" | label == "C" | label == "N" | label == "Qn" ~ 3,
      label == "bAut" | label == "det" | label == "fish" | label == "mInv" | label == "sInv" | label == "zooP" ~ 4)) %>%
    arrange(type) %>% 
    mutate(id = seq(1:17)) %>%
    select(id, label)
  
  ##### edge ####
  
  .benthos <- cbind(
    c(rep("Coral", 3), rep("Algae", 3), rep("Turf", 3)),
    c(rep(c("NPP", "Grav.", "DHW"), 3)),
    rep(1, 9))
  
  .archi <- cbind(
    c(rep("S", 6), rep("C", 6), rep("N", 6), rep("Qn", 6)),
    c(rep(c("NPP", "Grav.", "SST", "Coral", "Algae", "Turf"), 4)),
    rep(1, 24))
  
  .flux <- rbind(
    .bAut <- cbind(
      rep("bAut", 7),
      c("Algae", "Turf", "Grav.", "S", "C", "N", "Qn"),
      rep(1, 7)),
    
    .det <- cbind(
      rep("det", 8),
      c("Algae", "Turf", "NPP",  "Grav.", "S", "C", "N", "Qn"),
      rep(1, 8)),
    
    .fish <- cbind(
      rep("fish", 6),
      c("Coral", "Grav.", "S", "C", "N", "Qn"),
      rep(1, 6)),
    
    .mInv <- cbind(
      rep("mInv", 8),
      c("Coral", "Algae", "Turf", "Grav.", "S", "C", "N", "Qn"),
      rep(1, 8)),
    
    .sInv <- cbind(
      rep("sInv", 8),
      c("Coral", "Algae", "Turf", "Grav.", "S", "C", "N", "Qn"),
      rep(1, 8)),
    
    .zooP <- cbind(
      rep("zooP", 7),
      c("Coral", "NPP", "Grav.", "S", "C", "N", "Qn"),
      rep(1, 7))
  )
  
  edge <- as.data.frame(rbind(.benthos, .archi, .flux)) %>% 
    rename(var = V1, dep = V2, weight = V3)
  
  edge <- edge %>% 
    mutate(var = factor(edge$var, 
                        levels = c("S", "C", "N", "Qn", 
                                   "bAut", "det", "fish", "mInv", "sInv", "zooP",
                                   "Coral", "Algae", "Turf"))) %>% 
    mutate(dep = factor(edge$dep, 
                        levels = c("S", "C", "N", "Qn", 
                                   "Coral", "Algae", "Turf",
                                   "Grav.", "NPP", "SST", "DHW"))) %>% 
    mutate(weight = as.numeric(weight)) %>% 
    rename(from = dep, to = var)
  
  
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  edge <- edge %>% 
    mutate(to_benthos = case_when(
      from_label != "SST" & (
        to_label == "Coral" | to_label == "Algae" | to_label == "Turf") ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(magouille = case_when(
      from_label == "SST" & (
        to_label == "Coral" | to_label == "Algae" | to_label == "Turf") ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_benthos_bga = case_when(
      from_label == "Coral" | from_label == "Algae" | from_label == "Turf" | 
        from_label == "SST"| from_label == "NPP"| from_label == "Grav."| from_label == "DHW" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_benthos = case_when(
      from_label == "Coral" | from_label == "Algae" | from_label == "Turf" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_bga = case_when(
      from_label == "DHW" | from_label == "SST"| from_label == "NPP"| from_label == "Grav." ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(to_archi = case_when(
      to_label == "S" | to_label == "C"| to_label == "N"| to_label == "Qn" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(from_archi = case_when(
      from_label == "S" | from_label == "C"| from_label == "N"| from_label == "Qn" ~ TRUE,
      TRUE ~ FALSE))
  
  edge <- edge %>% 
    mutate(to_flux = case_when(
      to_label == "bAut" | to_label == "fish" | to_label == "det" | to_label == "zooP" |
        to_label == "sInv" | to_label == "mInv" ~ TRUE,
      TRUE ~ FALSE))
  
  ##### graph ####
  graph1 <- create_graph() %>%
    
    add_nodes_from_table(
      table = node,
      label_col = label) %>%
    
    set_node_attrs(
      node_attr = fontcolor,
      values = "white") %>% 
  
  # ENV + HP
  set_node_attrs(
    node_attr = fillcolor,
    values = "#CC4411", 
    nodes = c(1:4)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#CC4411",
      nodes = c(1:4)) %>%
    set_node_attrs(
      node_attr = rank,
      values = 1,
      nodes = c(1:4)) %>%
    
    # BENTHOS
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#EEBB44",
      nodes = c(5:7)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#EEBB44",
      nodes = c(5:7)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  2,
      nodes =  c(5:7)) %>%
    
    # ARCHI S C N Qn
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#74BBCD",
      nodes = c(8:11)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#74BBCD",
      nodes = c(8:11)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  3,
      nodes =  c(8:11)) %>%
    
    #FLUX
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#E9695F",
      nodes = c(12:17)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#E9695F",
      nodes = c(12:17)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  4,
      nodes =  c(12:17))
  
  graph2 <- graph1 %>%
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>%
    
    select_edges(conditions = from_bga == T) %>% 
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#CC4411") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#CC4411") %>%
    clear_selection() %>% 
    
    select_edges(conditions = from_benthos == T) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#EEBB44") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#EEBB44") %>%
    clear_selection() %>%
    
    select_edges(conditions = from_archi == T) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#74BBCD") %>%
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#74BBCD") %>%
    clear_selection()
  
  graph3 <- graph2 %>% 
    set_edge_attrs( 
      edge_attr = width,
      value = abs(graph2[["edges_df"]]$weight)*1) %>% 
    copy_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = penwidth)
  
  graphDAG <- graph3 %>% 
    add_global_graph_attrs(attr = "width", value = 0.5, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontsize", value = 12, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"), 
                           attr_type = c("graph", "graph", "graph"))
  
  render_graph(graphDAG)
  
  return(graphDAG)
}

makeSupp5 <- function(){
  graphDAG <- plotDAG()
  export_graph(graphDAG, height = 1000, file_name = "PAPER_FIGS/script_output_figs/Supp/Supp5_DAG.pdf", file_type = "pdf")
}

Supp5 <- makeSupp5()

#### MAKE SUPP 6 & 7- PPCHECKS ####

plot_ppchecks <- function(){
  load("PAPER_DATA/script_output/fit_final.RData")
  
  resp <- c("coral", "turf", "algae",
            "S", "C", "N", "Qn",
            "bAut", "det", "zooP", "sInv", "mInv", "fish" )
  legend <- c("Coral", "Turf", "Algae",
              "Node number", "Connectance", "Nestedness", "Modularity",
              "Benthic autotrophs", "Detritus", "Zooplankton", "Sessile invertebrates", "Mobile invertebrates", "Fish")
  
  # dens_overlay
  for (i in 1:length(resp)){
    (ppCheck_plot <- pp_check(fit_final, resp = as.character(resp[i]), type = "dens_overlay") +
       labs(x = as.character(legend[i]), y = "Density", title = paste("Posterior predictive distribution of", as.character(legend[i]))))
    ggsave(ppCheck_plot, filename = paste("PAPER_FIGS/script_output_figs/Supp/Supp6_ppCheck/ppCheck_DensOverlay_", as.character(resp[i]),".png", sep=""),
           dpi = 150, unit = "px", width = 1000, height = 500)
  }

  # scatter average
  for (i in 1:length(resp)){
    (ppCheck_plot <- pp_check(fit_final, resp = as.character(resp[i]), type = "scatter_avg") +
       labs(title = paste("Scatterplot between", as.character(legend[i]), "data and the average value of \n the posterior predictive distribution of each data point")))
    ggsave(ppCheck_plot, filename = paste("PAPER_FIGS/script_output_figs/Supp/Supp7_ppCheck/ppCheck_ScatterAvg_", as.character(resp[i]),".png", sep=""), 
           dpi = 150, unit = "px", width = 1000, height = 500)
  }
}

ppChecks <- plot_ppchecks()

#### MAKE SUPP 8 - CAT ####

makeSupp8 <- function(){
  .archi_bga <- read_csv("/home/emma/article_memoire/PAPER_DATA/script_output/bga.csv") %>% 
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
                                                                "WIP")))
  
  data <- .archi_bga %>% 
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
           S, C, N, Qn)
  
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
           N = (log(N+1) - mean(log(N+1))) / sd(log(N+1)),
           Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1))) %>% 
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
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp8_pcaCAT.png", dpi = 150, unit = "px", width = 1000, height = 750)
}

Supp8 <- makeSupp8()

#### MAKE SUPP 9 - KER ####

makeSupp9  <- function(){
  flux <- read_csv("PAPER_DATA/script_output/flux_per_prey_Ic.csv") %>%
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
  
  ggsave(file = "PAPER_FIGS/script_output_figs/Supp/Supp9_kerDensity.png", dpi = 150, unit = "px", width = 1000, height = 750)
}

Supp9 <- makeSupp9()
