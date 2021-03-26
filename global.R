##################################################
# Shiny App * UK Postcodes Visualizer - global.R #
##################################################

pkgs <- c('popiFun', 'Cairo', 'data.table', 'DT', 'fst', 'htmltools', 'leaflet', 'leaflet.extras', 'shiny', 'shinycssloaders', 'shinyWidgets', 'sp')
lapply(pkgs, require, char = TRUE)

options(spinner.color = '#e5001a', spinner.size = 1, spinner.type = 4)
options(bitmapType = 'cairo', shiny.usecairo = TRUE)
source('functions.R')
