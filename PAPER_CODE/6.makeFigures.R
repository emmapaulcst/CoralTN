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
# library(scales)
# 
# library(DiagrammeRsvg)
# library(htmltools)
# library(svglite)
# 
# library(grid)
# 
# library(khroma)


#### common functions ####

makeGraph <- function(list){
  # biP edge dataframe
  # n number of consumers
  # m number of resources
  
  biP <- list[[1]]
  n <- list[[2]]
  m <- list[[3]]
  S <- n + m
  
  colnames(biP) <- c("from_id", "to_id")
  biP$from_id <- as.character(biP$from_id)
  biP$to_id <- as.character(biP$to_id)
  
  node <- rbind(biP %>% select(from_id) %>% rename(label = from_id), 
                biP %>% select(to_id) %>% rename(label = to_id)) %>% 
    group_by(label) %>% summarise() %>% 
    mutate(id = seq(1:S)) %>%
    select(id, label)
  
  edge <- biP %>% 
    select(from_id, to_id) %>% 
    rename(from = from_id, to = to_id)
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  graph <- create_graph(
    directed = T) %>%
    
    add_nodes_from_table(
      table = node,
      label_col = label) %>%
    
    set_node_attrs(
      node_attr = fontcolor,
      values = "black") %>% 
    
    set_node_attrs(
      node_attr = fillcolor,
      values = "#FEE08B",
      nodes = c(1:n)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#FEE08B",
      nodes = c(1:n)) %>% 
    
    set_node_attrs(
      node_attr = fillcolor,
      values = "#66C2A5",
      nodes = c(n+1:m)) %>%
    set_node_attrs(
      node_attr = color,
      values = "#66C2A5",
      nodes = c(n+1:m)) %>%
    
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>% 
    set_edge_attrs(
      edge_attr = color,
      value =  "grey50") %>% 
    set_edge_attrs(
      edge_attr = fillcolor,
      value =  "grey50")
  
  graph <- graph %>% 
    add_global_graph_attrs(attr = "width", value = 0.4, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontsize", value = 12, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"), 
                           attr_type = c("graph", "graph", "graph"))
  
  plot <- render_graph(graph,
                       output = NULL,
                       as_svg = FALSE)
  
  return (graph)
}

#### MAKE FIG 1 ####

makeFig1 <- function(){
  
  graphFig1 <- list(
    graphA = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "4", "4", # high S  and low C
                                               "5", "6", "6", "7", "7", "8", "8", "9"),
                                             ncol = 2)),
                  n = 4,
                  m = 5),
    
    graphB = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", # low S and high C 
                                               "4", "5", "5", "4", "5", "4"),
                                             ncol = 2)),
                  n = 3,
                  m = 2),
    
    graphC = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "2", "3", "3", "3", "4", "4", #regular 
                                               "5", "6", "5", "6", "7", "6", "7", "8", "7", "8"),
                                             ncol = 2)),
                  n = 4,
                  m = 4),
    
    graphD = list(biP = as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "4", "4", # modular
                                               "5", "6", "5", "6", "7", "8", "7", "8"),
                                             ncol = 2)),
                  n <- 4,
                  m <- 4),
    
    graphE = list(biP = as.data.frame(matrix(c("1", "1", "1", "1", "2", "2", "2", "3", "3", "4", # nested
                                               "5", "6", "7", "8", "6", "7", "8", "7", "8", "8"),
                                             ncol = 2)),
                  n = 4,
                  m = 4))
  
  graphA <- makeGraph(graphFig1[[1]])
  export_graph(graphA, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig1/graphA.png", file_type = "png")
  
  graphB <- makeGraph(graphFig1[[2]])
  export_graph(graphB, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig1/graphB.png", file_type = "png")
  
  graphC <- makeGraph(graphFig1[[3]])
  export_graph(graphC, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig1/graphC.png", file_type = "png")
  
  graphD <- makeGraph(graphFig1[[4]])
  export_graph(graphD, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig1/graphD.png", file_type = "png")
  
  graphE <- makeGraph(graphFig1[[5]])
  export_graph(graphE, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig1/graphE.png", file_type = "png")
  
  # OR (for a better quality)
  # graphA <- export_graph(makeGraph(graphFig1[["graphA"]]), file_name = "PAPER_FIGS/script_output_figs/Fig1/graphA.pdf", file_type = "pdf")
  # graphB <- export_graph(makeGraph(graphFig1[["graphB"]]), file_name = "PAPER_FIGS/script_output_figs/Fig1/graphB.pdf", file_type = "pdf")
  # graphC <- export_graph(makeGraph(graphFig1[["graphC"]]), file_name = "PAPER_FIGS/script_output_figs/Fig1/graphC.pdf", file_type = "pdf")
  # graphD <- export_graph(makeGraph(graphFig1[["graphD"]]), file_name = "PAPER_FIGS/script_output_figs/Fig1/graphD.pdf", file_type = "pdf")
  # graphE <- export_graph(makeGraph(graphFig1[["graphE"]]), file_name = "PAPER_FIGS/script_output_figs/Fig1/graphE.pdf", file_type = "pdf")
  
}

# Fig1 <- makeFig1()

#### MAKE FIG 2 ####

makeFig2 <- function(){
  
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
           Qn = (log(Qn+1) - mean(log(Qn+1))) / sd(log(Qn+1))) %>% 
    select(SiteCode, Region, Region_short, S, C, N, Qn)
  
  #### pca ####
  pca_res <- prcomp(.archi[4:7], scale. = TRUE) 
  (pca <- autoplot(pca_res, data = .archi, size = 2, colour = 'Region', fill = 'Region', alpha = 0.7,
                   loadings = TRUE, loadings.colour = 'grey15',
                   loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      theme_minimal() +
      theme(
        title = element_text(size = 20),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "right") +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")))
  
  #### boxplots ####
  # node number
  boxS <-  ggplot() +
    geom_boxplot(data = .archi_bga, aes(x = Region_short, y = S, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none")
  
  # connectance
  boxC <- ggplot() +
    geom_boxplot(data = .archi_bga, aes(x = Region_short, y = C, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none")
  
  # modularity
  boxQn <- ggplot() +
    geom_boxplot(data = .archi_bga, aes(x = Region_short, y = Qn, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none")
  
  # nestedness
  boxN <- ggplot() +
    geom_boxplot(data = .archi_bga, aes(x = Region_short, y = N, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none")
  
  box <- ggarrange(boxS, boxC, boxQn, boxN, nrow = 1, ncol = 4, labels = c("2", "3", "4", "5"))
  
  #### all #####
  ggarrange(pca, box, nrow = 2, ncol = 1, heights = c(2,1), labels = c("1", ""))
  ggsave(file = "PAPER_FIGS/script_output_figs/Fig2/archiAll.png", dpi = 150, unit = "px", width = 1500, height = 1500)
  
  
  #### graphs ####
  graphFig2 <- list(
    graphDiverse = list(biP <- as.data.frame(matrix(c("1", "2", "2", "3", "3", "3", "4", "4", "5", "5", "6", # diverse
                                                      "7", "7", "8", "7", "8", "i", "8", "j", "i", "j", "j"),
                                                    ncol = 2)),
                        n <- 6,
                        m <- 4),
    
    graphConnected = list(biP <- as.data.frame(matrix(c("1", "1", "2", "2", "3", "3", "3", "3", # connected
                                                        "4", "5", "5", "6", "4", "5", "6", "7"),
                                                      ncol = 2)),
                          n <- 3,
                          m <- 4),
    
    graphModular = list(biP <- as.data.frame(matrix(c("1", "2", "2", "3", # modular
                                                      "4", "5", "6", "6"),
                                                    ncol = 2)),
                        n <- 3,
                        m <- 3))
  
  graphDiverse <- makeGraph(graphFig2[[1]])
  export_graph(graphDiverse, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig2/graphDiverse.png", file_type = "png")
  
  graphConnected <- makeGraph(graphFig2[[2]])
  export_graph(graphConnected, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig2/graphConnected.png", file_type = "png")
  
  graphModular <- makeGraph(graphFig2[[3]])
  export_graph(graphModular, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig2/graphModular.png", file_type = "png")
  
  return()
}

# Fig2 <- makeFig2()

#### MAKE FIG 3 ####

makeFig3 <- function(){
  
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
                                                                "WIP"))) %>% 
    select(SiteCode, Region, Region_short, SiteLongitude, SiteLatitude, S, L, C, Bc, Od, Id, N, Qn)
  
  
  flux <- read_csv("/home/emma/article_memoire/PAPER_DATA/script_output/flux_per_prey_Ic.csv") %>%
    select(site, bAut_prop:zooP_prop) %>%
    rename("bAut" = "bAut_prop",
           "det" = "det_prop",
           "fish" = "fish_prop",
           "mInv" = "mInv_prop",
           "sInv" = "sInv_prop",
           "zooP" = "zooP_prop",
           "SiteCode" = "site") 
  
  flux <- right_join(.archi_bga %>% select(SiteCode, Region, Region_short, SiteLongitude, SiteLatitude), flux, by = "SiteCode") %>% 
    filter(is.na(Region) == F)
  
  #### pca ####
  pca_res <- prcomp(flux[6:11], scale. = TRUE)
  
  (pca <- autoplot(pca_res, data = flux, size = 2, colour = 'Region', fill = 'Region', alpha = 0.7,
                   loadings = TRUE, loadings.colour = 'grey15',
                   loadings.label = TRUE, loadings.label.colour = "black", loadings.label.size = 4, loadings.label.repel = T) +
      theme_minimal() +
      theme(
        title = element_text(size = 20),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.position = "right") +
      scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")))
  
  #### boxplots flux #####
  # fish
  boxFish <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = fish, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "fish") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14), 
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.8))
  
  # bAut
  boxbAut <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = bAut, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "bAut") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14), 
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.8))
  
  # mInv
  boxmInv <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = mInv, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "mInv") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.8))
  
  # zooP
  boxzooP <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = zooP, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "zooP") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14), 
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.3))
  
  # sInv
  boxsInv <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = sInv, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "sInv") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.3))
  
  # det
  boxdet <- ggplot() +
    geom_boxplot(data = flux, aes(x = Region_short, y = det, colour = Region_short, fill = Region_short, alpha = 0.4)) +
    labs(x = "", y = "det") +
    scale_colour_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    scale_fill_manual(values = c("#66CCEE", "#228833", "#EE6677", "#CCBB44", "#4477AA")) +
    theme_minimal() +
    theme(
      title = element_text(size = 20),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none") +
    scale_y_continuous(limits = c(0, 0.3))
  
  box <- ggarrange(boxbAut, boxmInv, boxFish, boxdet, boxzooP, boxsInv, nrow = 2, ncol = 3, labels = c("2", "3", "4", "5", "6", "7"))
  box <- annotate_figure(box, left = text_grob("Proportion of carbon flowing through major pathways", 
                                               color = "black", size = 14, rot = 90))
  
  #### all #####
  (all <- ggarrange(pca, box, nrow = 2, ncol = 1, heights = c(1,1), labels = c("1", "")))
  ggsave(file = "PAPER_FIGS/script_output_figs/Fig3/fluxAll.png", dpi = 150, unit = "px", width = 1510, height = 2000)
  
}

# Fig3 <- makeFig3()

#### MAKE FIG 4 ####

plotSEM <- function(){
  
  load("PAPER_DATA/script_output/fit_final.RData")
  
  fe <- brms::fixef(fit_final) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("name") %>%
    tidyr::separate(name, into = c("dep", "var"), sep = "_") %>%
    filter(var != "Intercept") %>%
    mutate(dep = case_when(
      dep == "coral" ~ "Coral",
      dep == "algae" ~ "Algae",
      dep == "turf" ~ "Turf",
      dep == "mobInv" ~ "mInv",
      dep == "sessbInv" ~ "sInv",
      TRUE ~ dep)) %>%
    mutate(var = case_when(
      var == "algae" ~ "Algae",
      var == "coral" ~ "Coral",
      var == "gravity" ~ "Grav.",
      var == "sst" ~ "SST",
      var == "turf" ~ "Turf",
      var == "npp" ~ "NPP",
      var == "dhw" ~ "DHW",
      TRUE ~ var))
  
  fe$dep <- factor(fe$dep, levels = c("S", "C", "N", "Qn", 
                                      "bAut", "det", "fish", "mInv", "sInv", "zooP",
                                      "Coral", "Algae", "Turf")) 
  fe$var <- factor(fe$var, levels = c("S", "C", "N", "Qn", 
                                      "Grav.", "NPP", "SST", "Turf", "Algae", "Coral",
                                      "DHW"))
  write.csv(fe, "PAPER_DATA/script_output/fixed_effects.csv", row.names = F)
  
  
  node <- rbind(fe %>% select(var) %>% rename(label = var), 
                fe %>% select(dep) %>% rename(label = dep)) %>% 
    group_by(label) %>% summarise() %>%
    mutate(type = case_when(
      label == "DHW" | label == "SST" | label == "NPP" | label == "Grav." ~ 1,
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
      label == "S" | label == "C" | label == "N" | label == "Qn" ~ 3,
      label == "bAut" | label == "det" | label == "fish" | label == "mInv" | label == "sInv" | label == "zooP" ~ 4)) %>%
    arrange(type) %>% 
    mutate(id = seq(1:17)) %>%
    select(id, label)
  
  edge <- fe %>%
    mutate(influence = case_when(
      Q2.5 > 0 & Q97.5 > 0 ~ TRUE,
      Q2.5 < 0 & Q97.5 < 0 ~ TRUE,
      TRUE ~ FALSE)) %>% 
    filter(influence == TRUE) %>% 
    select(var, dep, Estimate) %>% 
    rename(from = var, to = dep, weight = Estimate)
  
  edge <- left_join(edge, node, by = c("from" = "label"))
  edge <- edge %>% rename(from_label = from, from = id)
  edge <- left_join(edge, node, by = c("to" = "label"))
  edge <- edge %>% rename(to_label = to, to = id)
  
  graphSEM <- create_graph() %>%
    
    add_nodes_from_table(
      table = node,
      label_col = label) %>%
    
    set_node_attrs(
      node_attr = fontcolor,
      values = "white") %>% 
    
    # node color
    # environmental + human pressures
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
      nodes = c(2:4)) %>%
    
    # benthos
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
    
    # architecture
    set_node_attrs(
      node_attr = fillcolor,
      values =  "#74BBCD",
      nodes = c(8:12)) %>%
    set_node_attrs(
      node_attr = color,
      values =  "#74BBCD",
      nodes = c(8:12)) %>%
    set_node_attrs(
      node_attr = rank,
      values =  3,
      nodes =  c(8:12)) %>%
    
    # flux
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
  
  # edges color
  graphSEM <- graphSEM %>%
    add_edges_from_table(
      table = edge,
      from_col = from,
      to_col = to,
      from_to_map = id_external) %>%
    
    select_edges(conditions = weight > 0) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#7CAE00") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#7CAE00") %>%
    clear_selection() %>%
    
    select_edges(conditions = weight < 0) %>%
    set_edge_attrs_ws(
      edge_attr = color,
      value =  "#E68650") %>% 
    set_edge_attrs_ws(
      edge_attr = fillcolor,
      value =  "#E68650") %>%
    clear_selection()
  
  # edges width
  graphSEM <- graphSEM %>% 
    set_edge_attrs(
      edge_attr = width,
      value = abs(graphSEM[["edges_df"]]$weight)*8) %>% 
    copy_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = penwidth)
  
  # global attributes
  graphSEM <- graphSEM %>% 
    add_global_graph_attrs(attr = "width", value = 0.4, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontsize", value = 9, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"),
                           attr_type = c("graph", "graph", "graph"))
  
  render_graph(graphSEM,
               output = NULL,
               as_svg = FALSE
  )
  
  export_graph(graphSEM, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig4/graphSEM.png", file_type = "png")
  return(graphSEM)  
}

plotCE <- function(){
  load("PAPER_DATA/script_output/fit_final.RData")
  
  cef_grav <- conditional_effects(fit_final,
                                  effects = c("gravity"))
  
  cef_npp <- conditional_effects(fit_final,
                                 effects = c("npp"))
  
  cef_coral <- conditional_effects(fit_final,
                                   effects = c("coral"))
  
  cef_algae <- conditional_effects(fit_final,
                                   effects = c("algae"))
  
  
  cef_modularity <- conditional_effects(fit_final,
                                        effects = c("Qn"))
  
  
  cef_nestedness <- conditional_effects(fit_final,
                                        effects = c("N"))
  
  plot_fish_grav <- plot(cef_grav, 
                         points = getOption("brms.plot_points", FALSE),
                         re_formula = NULL,
                         point_args = list(
                           color = "grey80",
                           alpha = 0.5),
                         line_args = list(color = "#E9695F"),
                         theme = theme_minimal(),
                         ask = F) [[3]] +
    labs(x = "Gravity",
         y = "fish")
  
  plot_S_algae <- plot(cef_algae,
                       points = getOption("brms.plot_points", FALSE),
                       re_formula = NULL,
                       point_args = list(
                         color = "grey80",
                         alpha = 0.5),
                       line_args = list(color = "#74BBCD"),
                       theme = theme_minimal(),
                       ask = F) [[7]] +
    labs(x = "Algae",
         y = "S")
  
  plot_mInv_coral <- plot(cef_coral, 
                          points = getOption("brms.plot_points", FALSE),
                          re_formula = NULL,
                          point_args = list(
                            color = "grey80",
                            alpha = 0.5),
                          line_args = list(color = "#E9695F"),
                          theme = theme_minimal(),
                          ask = F) [[4]] +
    labs(x = "Coral",
         y = "mInv")
  
  plot_zooP_coral <- plot(cef_coral, 
                          points = getOption("brms.plot_points", FALSE),
                          re_formula = NULL,
                          point_args = list(
                            color = "grey80",
                            alpha = 0.5),
                          line_args = list(color = "#E9695F"),
                          theme = theme_minimal(),
                          ask = F) [[6]] +
    labs(x = "Coral",
         y = "zooP")
  
  plot_C_coral <- plot(cef_coral, 
                       points = getOption("brms.plot_points", FALSE),
                       re_formula = NULL,
                       point_args = list(
                         color = "grey80",
                         alpha = 0.5),
                       line_args = list(color = "#74BBCD"),
                       theme = theme_minimal(),
                       ask = F) [[6]] +
    labs(x = "Coral",
         y = "C")
  
  plot_C_npp <- plot(cef_npp, 
                     points = getOption("brms.plot_points", FALSE),
                     re_formula = NULL,
                     point_args = list(
                       color = "grey80",
                       alpha = 0.5),
                     line_args = list(color = "#74BBCD"),
                     theme = theme_minimal(),
                     ask = F) [[6]] +
    labs(x = "NPP",
         y = "C")
  
  grid.newpage()
  (CE <- cbind(rbind(ggplotGrob(plot_S_algae), ggplotGrob(plot_C_npp), ggplotGrob(plot_C_coral)),
               rbind(ggplotGrob(plot_fish_grav), ggplotGrob(plot_mInv_coral), ggplotGrob(plot_zooP_coral))))
  grid.draw(CE)
  
  return(CE)
}


makeFig4 <- function(){
  
  # SEM 
  graphSEM <- plotSEM()
  render_graph(graphSEM, 
               output = NULL,
               as_svg = FALSE
  )
  export_graph(graphSEM, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig4/graphSEM.png", file_type = "png") # ou svg
  
  # Conditional effects
  CE <- plotCE()
  grid.draw(CE)
  ggsave('PAPER_FIGS/script_output_figs/Fig4/plotCE.png', plot = CE, unit = "px", width = 2000, height = 2000, dpi = 200)
  
}

Fig4 <- makeFig4()



