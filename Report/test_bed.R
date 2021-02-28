library(dplyr)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(plotly)
library(tidyverse)



setwd("C:/Users/kiera/Work/NE_work/Report")
getwd()
source('./data_prep_figure4.R')

################################################################################
name_swap_bap <- get_names('./dataframes/rename_habitat.csv')[[1]]
name_swap_nvc <- get_names('./dataframes/rename_habitat.csv')[[2]]
bb_list <- get_names('./dataframes/rename_habitat.csv')[[3]]
bp_list <- get_names('./dataframes/rename_habitat.csv')[[4]]

#df_all <- read_all_data('all_plots.csv')
df_all <- read_csv('./dataframes/report_plots.csv')
site_code <- 'B12'

# Taking the data from just one site
df_site <- filter(df_all, SITECODE == site_code) %>%
  fix_names(., name_swap_bap) %>%
  # transform the eastings and northings into latitude and longitude
  transform_coords(.)

# Converts the NVC abbreviations to the full names
df_site$NVC_groupc <- name_swap(df_site$NVC_groupc, name_swap_nvc)
df_site$YEAR <- as.character(df_site$YEAR)
df_site$Species_diversity <- round(df_site$Species_diversity, 2)

# PLOT 30 for Lullington is really weird and should be removed as it ruins the
# scale
# df_site <- df_site %>%
#   filter(PLOT_ID != 30) %>%
#   filter(PLOT_ID != '30a') %>%
#   filter(PLOT_ID != '30')

# Making the sub dataframes with only the major habitats selected
#object[[1]] is the data frame. object[[2]] is the list of habitats
df_site.bb <- select_by(df_site, 'BAP_BROAD', 20, include = 'Inland Rock')
df_site.bp <- select_by(df_site, 'BAP_PRIORITY', 15)
df_site.nvcb <- select_by(df_site, 'NVC_groupb', 15)
df_site.nvcc <- select_by(df_site, 'NVC_groupc', 20)

# the centre points of the maps
east_cent <- get_centre_coords(df_site)[[1]]
north_cent <- get_centre_coords(df_site)[[2]]

unique_years <- unique(df_site$YEAR)
unique_habs <- unique(df_site$BAP_BROAD)
unique_nvchabs <- unique(df_site$NVC_groupc)

df_change <- get_change_by_year(df_site)

################################################################################

habitat <- 'BAP_BROAD'

list_of_years <- unique(df_site$YEAR)
list_of_habs <- unique(df_site[[habitat]])

factpal_site <- colorFactor(topo.colors(length(list_of_habs)), list_of_habs)

centre_coords <- get_centre_coords(df_site)
east_cent <- centre_coords[[1]]
north_cent <- centre_coords[[2]]

n <- leaflet(df_site) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
  addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                   color = factpal_site(df_site[[habitat]]),
                   group = ~YEAR,
                   label = ~NVC_groupb,
                   stroke = FALSE, fillOpacity = 0.8) %>%
  addLegend(pal = factpal_site, 
            values = df_site[[habitat]], 
            opacity = 1,
            title = habitat,
            position = 'bottomleft') %>%
  addLayersControl(baseGroups = list_of_years,
                   options = layersControlOptions(collapsed = F))


frameWidget(n, width='95%')
