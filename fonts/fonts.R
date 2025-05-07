# Install if necessary
install.packages("showtext")

# Load packages
library(showtext)
library(ggplot2)
library(ggpubr)

# Add Myriad font (update path if needed)
font_add(family = "Myriad", regular = "~/CoralTN/fonts/Myriad Pro Regular.ttf")
font_add(family = "Myriad", regular = "~/CoralTN/fonts/Myriad Pro Bold.ttf")

# Automatically use showtext for all plots
showtext_auto()

showtext::fonts()

library(extrafont)
fonts()
extrafont::font_import()   # This can take a few minutes
extrafont::loadfonts(device = "pdf")
font_import(prompt = FALSE) 
