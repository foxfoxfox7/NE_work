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
site_code <- 'B14'

# Taking the data from just one site
df_site <- filter(df_all, Sitecode == site_code) %>%
  fix_names(., name_swap_bap) %>%
  # transform the eastings and northings into latitude and longitude
  transform_coords(.)

# Converts the NVC abbreviations to the full names
df_site$NVC_habitat <- name_swap(df_site$NVC_habitat, name_swap_nvc)
df_site$Year <- as.character(df_site$Year)
#df_site$Species_diversity <- round(df_site$Species_diversity, 2)



# Making the sub dataframes with only the major habitats selected
#object[[1]] is the data frame. object[[2]] is the list of habitats
df_site.bb <- select_by(df_site, 'BAP_broad', 20)#include = c('Calcareous Grassland', 'Dwarf Shrub Heath'))
df_site.bp <- select_by(df_site, 'BAP_priority', 15)
df_site.nvcb <- select_by(df_site, 'NVC_group', 15)
df_site.nvcc <- select_by(df_site, 'NVC_habitat', 15)

# the centre points of the maps
east_cent <- get_centre_coords(df_site)[[1]]
north_cent <- get_centre_coords(df_site)[[2]]

unique_years <- unique(df_site$Year)
unique_habs <- unique(df_site$BAP_broad)
unique_nvchabs <- unique(df_site$NVC_habitat)

df_change <- get_change_by_year(df_site)

df_all2 <- read_csv('./dataframes/all_plots.csv')
hab_sum <- get_hab_sums(df_all2)

################################################################################




library(RColorBrewer)


library(viridis)




df <- df_site
habitat <- 'BAP_broad'
list_of_years <- unique(df$Year)
list_of_habs <- unique(df[[habitat]])





factpal_site <- colorFactor(viridis(length(list_of_habs)), list_of_habs)
factpal_site <- colorFactor(viridis(length(list_of_habs)), list_of_habs)






nn <- length(list_of_habs)

factpal_site <- colorFactor(brewer.pal(n = nn, name = 'Set2'), list_of_habs)





centre_coords <- get_centre_coords(df)
east_cent <- centre_coords[[1]]
north_cent <- centre_coords[[2]]

n <- leaflet(df) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
  addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                   color = factpal_site(df[[habitat]]),
                   group = ~Year,
                   label = ~NVC_group,
                   stroke = FALSE, fillOpacity = 0.7) %>%
  addLegend(pal = factpal_site, 
            values = df[[habitat]], 
            opacity = 0.7,
            title = habitat,
            position = 'bottomleft') %>%
  addLayersControl(baseGroups = list_of_years,
                   options = layersControlOptions(collapsed = F))

n
