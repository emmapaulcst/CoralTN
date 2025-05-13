# R/functions.R
library(readr)
library(dplyr)
library(ggplot2)

get_data1 <- function(file){
  read_csv(file, col_types = cols()) %>%
    filter(!is.na(Ozone))
}

fit_model1 <- function(data){
  lm(Ozone ~ Temp, data) %>% 
    coefficients()
}

plot_model1 <- function(data, model) {
  ggplot(data) +
    geom_point(aes(x = Temp, y = Ozone)) +
    geom_abline(intercept = model[1], 
                slope = model[2])
}

get_data2 <- function(file){
  read_csv(file, col_types = cols())
}

fit_model2 <- function(data){
  lm(C ~ S, data) %>% 
    coefficients()
}

plot_model2 <- function(data, model) {
  ggplot(data) +
    geom_point(aes(x = S, y = C)) +
    geom_abline(intercept = model[1], 
                slope = model[2])
}
