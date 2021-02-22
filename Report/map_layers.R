library(dplyr)
library(leaflet)
library(htmlwidgets)


setwd("C:/Users/kiera/Work/NE_work")
getwd()
source('./data_prep_figure4.R')


name_gl <- get_names('rename_habitat.csv')
name_swap_vector <- name_gl[[1]]
bb_list <- name_gl[[2]]
bp_list <- name_gl[[3]]



df_all <- read_all_data('all_plots.csv')
# Taking the data from just one site
df <- filter(df_all, SITECODE == 'B12') %>%
# transform the eastings and northings into latitude and longitude
  transform_coords(.) %>%
  fix_names(.)


map_hab_by_year(df, 'BAP_BROAD')