library(dplyr)
library(leaflet)
library(htmlwidgets)


setwd("C:/Users/kiera/Work/NE_work/Report")
getwd()
source('./data_prep_figure4.R')

name_swap_bap <- get_names('rename_habitat.csv')[[1]]
name_swap_nvc <- get_names('rename_habitat.csv')[[2]]
bb_list <- get_names('rename_habitat.csv')[[3]]
bp_list <- get_names('rename_habitat.csv')[[4]]

#df_all <- read_all_data('presentation_surveys.xlsx')
#df_all <- read_excel("presentation_surveys.xlsx", sheet = "presentation_plots")
df_all <- read_csv('report_plots.csv')

# Taking the data from just one site
df <- filter(df_all, SITECODE == 'B12') %>%
# transform the eastings and northings into latitude and longitude
  transform_coords(.) %>%
  fix_names(., name_swap_bap)

df$NVC_groupc <- name_swap(df$NVC_groupc, name_swap_nvc)





