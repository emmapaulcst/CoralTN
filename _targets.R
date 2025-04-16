# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
# https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("readr", "dplyr", "tidyr", "ggplot2", "rlist", "parallel", "vegan", "bipartite",
               "purrr", "tibble", "brms", "ggpubr", "scales", "loo", "DiagrammeR", "rstantools",
               "grid", "Rcpp", "gridExtra", "cmdstanr", "magick", "ggfortify", "ggcorrplot"))

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
tar_source("R/functions_mat_int.R")
tar_source("R/functions_topo.R")
tar_source("R/functions_bga.R")
tar_source("R/functions_flux_per_cat.R")
tar_source("R/functions_sem.R")
tar_source("R/functions_sem_zscores.R")
tar_source("R/functions_makeFigs.R")
tar_source("R/functions_make_zFigs.R")
tar_source("R/functions_makeSupp.R")
tar_source("R/functions_make_zSupp.R")
tar_source("R/find_bug.R")

# tar_source("R/allDuplicated.R")
# tar_source("R/modularity_beckett.R")

list(
  #### BUILD INT MATRICES ####
  # Get global interaction matrix from ML
  tar_target(path_to_global_mat_int, "data/interaction_matrix.csv", format = "file"),
  tar_target(global_mat_int, get_global_mat_int(path_to_global_mat_int)),
  
  # Get reef fish visual censuses from RLS
  tar_target(path_to_fish_rls, "data/rls_fish_flux.csv", format = "file"),
  
  # Old fish
  # tar_target(path_to_fish_rls, "find_bug/rls_fish_99_flux_turf_no_duplicate.csv", format = "file"),
  
  tar_target(fish_rls, get_fish_rls(path_to_fish_rls)),
  
  # Save selected SurveyID
  tar_target(surveyID, get_surveyID(fish_rls)),
  # Save site name
  tar_target(site, get_site(fish_rls)),
  # Save carbon fluxes
  tar_target(total_flux_per_sp_per_site, get_c_fluxes(fish_rls)),
  
  # Get all interaction matrices
  tar_target(list_mat_int, gives_list_mat_int(site, fish_rls, global_mat_int)),
  # Rmv empty columns in the matrices
  tar_target(list_mat_int_rmv, gives_list_mat_int_rmv(site, list_mat_int)),
  # Weight matrices with carbon fluxes
  tar_target(list_mat_int_rmv_w, weight_matrices(site, list_mat_int_rmv, total_flux_per_sp_per_site)),
  # Rmv empty columns to be sure
  tar_target(list_mat_int_Ic, gives_list_mat_int_rmv(site, list_mat_int_rmv_w)),
  
  # Old list mat int
  # tar_target(path_to_old_list_mat_int, "find_bug/list_mat_int_cut-off_rescaled_turf_Ic.rdata", format = "file"),
  # tar_target(list_mat_int_Ic_old, get_old_list_mat_int(path_to_old_list_mat_int))
  
  #### ASSESS TOPOLOGY ####
  # tar_target(path_to_mat_int, "data/list_mat_int_Ic.rdata", format = "file"),
  # tar_target(mat_int, get_mat_int(path_to_mat_int)),
  
  # NEVER EVER EVEEEER SUPRESS TOPOLOGY #
  tar_target(topology, get_all_topo(list_mat_int_Ic, surveyID)),
  
  # Old topo
  # tar_target(path_to_topo, "find_bug/archi_Ic_cut-off_rescaled_turf_Cc_bis.csv", format = "file"),
  # tar_target(topology, get_old_topo(path_to_topo))

  #### BUILD BGA DATASET ####
  # Get benthos data
  tar_target(path_to_benthos, "data/rls_benthos.csv", format = "file"),
  tar_target(benthos, get_benthos(path_to_benthos, surveyID)),

  # Get predictors data
  tar_target(path_to_predictors, "data/rls_predictors.csv", format = "file"),
  tar_target(predictors, get_predictors(path_to_predictors, surveyID)),

  # get z-scores
  tar_target(path_to_z_scores, "data/z_scores.csv", format = "file"),
  tar_target(z_scores, get_z_scores(path_to_z_scores)),

  # tar_target(path_to_mat_int, "data/list_mat_int_Ic.rdata", format = "file"),
  # tar_target(mat_int, get_mat_int(path_to_mat_int)),

  # Gives bga dataset
  tar_target(bga, gives_bga_dataset(topology, z_scores, benthos, predictors)),

  #### CARBON FLUXES ####
  # Get prey categories
  tar_target(path_to_prey_cat, "data/prey_categories.csv", format = "file"),
  tar_target(prey_cat, get_prey_cat(path_to_prey_cat)),

  # Gives carbon fluxes per categories
  tar_target(flux_per_prey_Ic, gives_flux_per_cat(prey_cat, list_mat_int_Ic)),
  # More info on carbon fluxes
  # tar_target(info_on_fluxes, get_info_on_fluxes(flux_per_prey_Ic))

  #### SEM ####
  tar_target(data_sem, gives_data_SEM(bga, flux_per_prey_Ic)),

  # Old data for sem
  # tar_target(path_to_data, "find_bug/data.csv", format = "file"),
  # tar_target(data, get_data(path_to_data)),

  tar_target(fit_sem, make_SEM(data_sem)),
  # tar_target(fit_sem_5000, make_SEM_5000(data_sem)),
  
  #### z_SEM ####
  tar_target(data_z_sem, gives_zdata_SEM(bga, flux_per_prey_Ic)),
  tar_target(fit_z_sem, make_zSEM(data_z_sem)),
  # tar_target(fit_z_sem_5000, make_SEM_5000(data_sem))
  
  #### MAKE FIGS ####
  ##### Fig1 ####
  tar_target(Fig1_data, makeFig1_data()),
  tar_target(Fig1_graphA, makeGraph(Fig1_data[['graphA']])),
  tar_target(Fig1_graphB, makeGraph(Fig1_data[['graphB']])),
  tar_target(Fig1_graphC, makeGraph(Fig1_data[['graphC']])),
  tar_target(Fig1_graphD, makeGraph(Fig1_data[['graphD']])),
  tar_target(Fig1_graphE, makeGraph(Fig1_data[['graphE']])),

  ##### Fig2 ####
  tar_target(Fig2, makeFig2(bga)),
  tar_target(Fig2_data, makeFig2_data()),
  tar_target(Fig2_graphS, makeGraph(Fig2_data[['graphDiverse']])),
  tar_target(Fig2_graphC, makeGraph(Fig2_data[['graphConnected']])),
  tar_target(Fig2_graphQ, makeGraph(Fig2_data[['graphModular']])),

  ##### Fig3 ####
  tar_target(Fig3, makeFig3(bga, flux_per_prey_Ic)),

  ##### Fig 4 ####
  tar_target(Fig4_sem, makeFig4_SEM(fit_sem)),
  tar_target(Fig4_ce_data, makeFig4_CE_data(fit_sem)),
  tar_target(Fig4_ce, makeFig4_CE(Fig4_ce_data)), #use this to plot : grid.draw(tar_read(Fig4_ce))

  # tar_target(Fig4_sem_5000, makeFig4_SEM(fit_sem_5000)),
  # tar_target(Fig4_ce_data_5000, makeFig4_CE_data(fit_sem_5000)),
  # tar_target(Fig4_ce_5000, makeFig4_CE(Fig4_ce_data_5000)), #use this to plot : grid.draw(tar_read(Fig4_ce))

  
  #### MAKE Z-FIGS ####
  ##### Fig2 ####
  tar_target(Fig2_z, make_zFig2(bga)),

  ##### Fig 4 ####
  tar_target(Fig4_z_sem, make_zFig4_SEM(fit_z_sem)),
  tar_target(Fig4_z_ce_data, make_zFig4_CE_data(fit_z_sem)),
  tar_target(Fig4_z_ce, make_zFig4_CE(Fig4_z_ce_data)), #use this to plot : grid.draw(tar_read(Fig4_ce))

  # tar_target(Fig4_sem_5000, makeFig4_SEM(fit_sem_5000)),
  # tar_target(Fig4_ce_data_5000, makeFig4_CE_data(fit_sem_5000)),
  # tar_target(Fig4_ce_5000, makeFig4_CE(Fig4_ce_data_5000)) #use this to plot : grid.draw(tar_read(Fig4_ce))
  
  #### MAKE SUPP ####
  tar_target(Supp2, makeSupp2(bga)),
  tar_target(Supp3, makeSupp3(bga)),
  tar_target(Supp4, makeSupp4(bga)),
  tar_target(Supp5, plotDAG()),
  tar_target(Supp6, plot_ppchecks(fit_sem)),
  tar_target(Supp8, makeSupp8(bga)),
  tar_target(Supp9, makeSupp9(flux_per_prey_Ic)),
  
  #### MAKE Z-SUPP ####
  tar_target(Supp3_z, makeSupp3_z(bga)),
  tar_target(Supp4_z, makeSupp4_z(bga)),
  tar_target(Supp6_z, plot_ppchecks_z(fit_z_sem)),
  tar_target(Supp8_z, makeSupp8_z(bga)),
  tar_target(Supp9_z, makeSupp9_z(flux_per_prey_Ic))
  
  
  
)



