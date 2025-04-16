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
# library(rstan)

#### FIGURES ####

make_zFig2 <- function(bga){
  
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
    select(SiteCode, Region, Region_short, S, C, zN, zQn)
  
  #### pca ####
  pca_res <- prcomp(archi[4:7], scale. = TRUE) 
  (pca <- autoplot(pca_res, data = archi, size = 2, colour = 'Region', fill = 'Region', alpha = 0.7,
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
    geom_boxplot(data = bga, aes(x = Region_short, y = S, colour = Region_short, fill = Region_short, alpha = 0.4)) +
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
    geom_boxplot(data = bga, aes(x = Region_short, y = C, colour = Region_short, fill = Region_short, alpha = 0.4)) +
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
    geom_boxplot(data = bga, aes(x = Region_short, y = zQn, colour = Region_short, fill = Region_short, alpha = 0.4)) +
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
    geom_boxplot(data = bga, aes(x = Region_short, y = zN, colour = Region_short, fill = Region_short, alpha = 0.4)) +
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
  Fig2 <- ggarrange(pca, box, nrow = 2, ncol = 1, heights = c(2,1), labels = c("1", ""))
  ggsave(file = "PAPER_FIGS/script_output_figs/Fig2/archiAll_z.png", dpi = 150, unit = "px", width=1500, height=1200)  #width=10, height=8
  
  return(Fig2)
}


#Fig2 <- makeFig2()

# fit_sem <- fit_z_sem

make_zFig4_SEM <- function(fit_sem){
  
  ##### Fe ####
  
  fe <- brms::fixef(fit_sem) %>% # mettre cette partie dans un .csv plutôt
    as.data.frame() %>%
    tibble::rownames_to_column("name") %>%
    tidyr::separate(name, into = c("dep", "var"), sep = "_") %>%
    dplyr::filter(var != "Intercept") %>% 
    # mutate(dep = case_when(
    #   dep == "coral" ~ "Coral",
    #   dep == "algae" ~ "Algae",
    #   dep == "turf" ~ "Turf",
    #   dep == "mobInv" ~ "mInv",
    #   dep == "sessbInv" ~ "sInv",
    #   TRUE ~ dep)) %>%
    # mutate(var = case_when(
    #   var == "algae" ~ "Algae",
    #   var == "coral" ~ "Coral",
    #   var == "gravity" ~ "Grav.",
  #   var == "sst" ~ "SST",
  #   var == "turf" ~ "Turf",
  #   var == "npp" ~ "NPP",
  #   var == "dhw" ~ "DHW",
  #   TRUE ~ var))
  dplyr::mutate(dep = case_when(
    dep == "coral" ~ "Coral",
    dep == "algae" ~ "Algae",
    dep == "turf" ~ "Turf",
    dep == "S" ~ "Node number",
    dep == "C" ~ "Connectance",
    dep == "zQn" ~ "Modularity",
    dep == "zN" ~ "Nestedness",
    dep == "mInv" ~ "Mobile invertebrates",
    dep == "sInv" ~ "Sessile invertebrates",
    dep == "bAut" ~ "Benthic autotrophs",
    dep == "det" ~ "Detritus",
    dep == "fish" ~ "Fish",
    dep == "zooP" ~ "Zooplankton",
    TRUE ~ dep)) %>% 
    dplyr::mutate(var = case_when(
      var == "coral" ~ "Coral",
      var == "algae" ~ "Algae",
      var == "turf" ~ "Turf",
      var == "gravity" ~ "Gravity",
      var == "sst" ~ "Sea Surface Temperature",
      var == "npp" ~ "Net Primary Production",
      var == "dhw" ~ "Degree Heating Weeks",
      var == "S" ~ "Node number",
      var == "C" ~ "Connectance",
      var == "zQn" ~ "Modularity",
      var == "zN" ~ "Nestedness",
      TRUE ~ var)) 
  
  # fe$dep <- factor(fe$dep, levels = c("S", "C", "Cc", "nCc", "Bc", "nBc", "N", "Qn", 
  #                                     "bAut", "det", "fish", "mInv", "sInv", "zooP",
  #                                     "Coral", "Algae", "Turf")) 
  # fe$var <- factor(fe$var, levels = c("S", "C", "Cc", "nCc", "Bc", "nBc", "N", "Qn", 
  #                                     "Grav.", "NPP", "SST", "Turf", "Algae", "Coral",
  #                                     "DHW"))
  
  fe$dep <- factor(fe$dep, levels = c("Node number", "Connectance",  "Nestedness", "Modularity", 
                                      "Benthic autotrophs", "Detritus", "Fish", "Mobile invertebrates", "Sessile invertebrates", "Zooplankton",
                                      "Coral", "Algae", "Turf")) 
  fe$var <- factor(fe$var, levels = c("Node number", "Connectance", "Nestedness", "Modularity", 
                                      "Gravity", "Net Primary Production", "Sea Surface Temperature", "Degree Heating Weeks",
                                      "Turf", "Algae", "Coral"))
  
  ##### Nodes ####
  
  # node <- rbind(fe %>% select(var) %>% rename(label = var), 
  #               fe %>% select(dep) %>% rename(label = dep)) %>% 
  #   group_by(label) %>% summarise() %>%
  #   mutate(type = case_when(
  #     label == "DHW" | label == "SST" | label == "NPP" | label == "Grav." ~ 1,
  #     label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
  #     label == "S" | label == "C" | label == "N" | label == "Qn" ~ 3,
  #     label == "bAut" | label == "det" | label == "fish" | label == "mInv" | label == "sInv" | label == "zooP" ~ 4)) %>%
  #   arrange(type) %>% 
  #   mutate(id = seq(1:17)) %>% #1:18
  #   select(id, label)
  
  node <- rbind(fe %>% select(var) %>% rename(label = var), 
                fe %>% select(dep) %>% rename(label = dep)) %>% 
    group_by(label) %>% summarise() %>%
    mutate(type = case_when(
      label == "Degree Heating Weeks" | label == "Sea Surface Temperature" | label == "Net Primary Production" | label == "Gravity" ~ 1,
      label == "Coral" | label == "Algae" | label == "Turf" ~ 2,
      label == "Node number" | label == "Connectance" | label == "Nestedness" | label == "Modularity" ~ 3,
      label == "Benthic autotrophs" | label == "Detritus" | label == "Fish" | 
        label == "Mobile invertebrates" | label == "Sessile invertebrates" | label == "Zooplankton" ~ 4)) %>%
    arrange(type) %>% 
    mutate(id = seq(1:17)) %>% #1:18
    select(id, label)
  
  #Prepare label sizes
  node_labels <- as.vector(node$label)
  node_widths <- nchar(node_labels) * 0.1
  node_heights <- rep(0.5, length(node_labels)) 
  
  ##### Edges ####
  
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
  
  ##### Graph ####
  graphSEM <- create_graph() %>%
    
    add_nodes_from_table(
      table = node,
      label_col = label) %>%
    
    set_node_attrs(
      node_attr = fontcolor,
      values = "white") %>% 
    set_node_attrs(
      node_attr = width,
      values = node_widths) %>% 
    set_node_attrs(
      node_attr = shape,
      values = "rectangle") %>% 
    
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
    add_global_graph_attrs(attr = "bgcolor", value = "transparent", attr_type = "graph") %>% 
    add_global_graph_attrs(attr = "width", value = 0.4, attr_type = "node") %>% 
    add_global_graph_attrs(attr = "fontsize", value = 9, attr_type = "node") %>% 
    add_global_graph_attrs(attr = c("layout", "rankdir", "splines"),
                           value = c("dot", "TD", "true"),
                           attr_type = c("graph", "graph", "graph"))
  
  final_graphSEM <- render_graph(graphSEM,
                                 output = NULL,
                                 as_svg = FALSE
  )
  
  
  export_graph(graphSEM, height = 500, file_name = "PAPER_FIGS/script_output_figs/Fig4/graphSEM_z.png", file_type = "png") # ou svg
  
  return(final_graphSEM)  
}

make_zFig4_CE_data <- function(fit_z_sem){
  
  Fig4_CE_data <- list(  
    cef_grav <- conditional_effects(fit_z_sem,
                                    effects = c("gravity")),
    
    cef_npp <- conditional_effects(fit_z_sem,
                                   effects = c("npp")),
    
    cef_coral <- conditional_effects(fit_z_sem,
                                     effects = c("coral")),
    
    cef_algae <- conditional_effects(fit_z_sem,
                                     effects = c("algae")),
    
    
    cef_modularity <- conditional_effects(fit_z_sem,
                                          effects = c("zQn")),
    
    
    cef_nestedness <- conditional_effects(fit_z_sem,
                                          effects = c("zN")))
  
  return(Fig4_CE_data)
}

make_zFig4_CE <- function(Fig4_CE_data){
  
  plot_fish_grav <- plot(Fig4_CE_data[[1]], 
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
  
  plot_S_algae <- plot(Fig4_CE_data[[4]],
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
  
  plot_mInv_coral <- plot(Fig4_CE_data[[3]], 
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
  
  plot_zooP_coral <- plot(Fig4_CE_data[[3]], 
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
  
  plot_C_coral <- plot(Fig4_CE_data[[3]], 
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
  
  plot_C_npp <- plot(Fig4_CE_data[[2]], 
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
  
  # grid.newpage()
  # (CE <- cbind(rbind(ggplotGrob(plot_S_algae), ggplotGrob(plot_C_npp), ggplotGrob(plot_C_coral)),
  #              rbind(ggplotGrob(plot_fish_grav), ggplotGrob(plot_mInv_coral), ggplotGrob(plot_zooP_coral))))
  # plot <- grid.draw(CE)
  # return(plot)
  
  # Combine the grobs
  CE <- grid.arrange(
    ggplotGrob(plot_S_algae),
    ggplotGrob(plot_C_npp),
    ggplotGrob(plot_C_coral),
    ggplotGrob(plot_fish_grav),
    ggplotGrob(plot_mInv_coral),
    ggplotGrob(plot_zooP_coral),
    ncol = 2  # Adjust number of columns as needed
  )
  
  grid.draw(CE)
  ggsave('PAPER_FIGS/script_output_figs/Fig4/plotCE_z.png', plot = CE, unit = "px", width = 2000, height = 2000, dpi = 200)
  
  
  # Return the combined grob object
  return(CE)
  
}
