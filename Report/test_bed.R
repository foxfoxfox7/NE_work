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

name_swap_bap <- get_names('rename_habitat.csv')[[1]]
name_swap_nvc <- get_names('rename_habitat.csv')[[2]]
bb_list <- get_names('rename_habitat.csv')[[3]]
bp_list <- get_names('rename_habitat.csv')[[4]]

#df_all <- read_all_data('presentation_surveys.xlsx')
#df_all <- read_excel("presentation_surveys.xlsx", sheet = "presentation_plots")
df_all <- read_csv('report_plots.csv')

# Taking the data from just one site
df <- filter(df_all, SITECODE == 'B14') %>%
# transform the eastings and northings into latitude and longitude
  transform_coords(.) %>%
  fix_names(., name_swap_bap)

df$YEAR <- as.character(df$YEAR)
df$Species_diversity <- round(df$Species_diversity, 2)

df$NVC_groupc <- name_swap(df$NVC_groupc, name_swap_nvc)

unique_years <- unique(df$YEAR)
unique_habs <- unique(df$BAP_BROAD)
unique_nvchabs <- unique(df$NVC_groupc)

total_change <- get_change_by_year(df)

################################################################################

df_pc <- read_csv('all_species_pc.csv')
indicators <- read_csv('Positive_indicators.csv')

keep_cols <- c('PLOT_ID', 'SITECODE', 'YEAR', 'EASTINGS', 'NORTHINGS', 
               'BAP_BROAD', 'NVC_FIRST')

df_pc[ , !(colnames(df_pc) %in% keep_cols)][is.na(df_pc[ , !(colnames(df_pc) %in% keep_cols)])] <- 0

site_code <- 'B14'

spec_list <- indicators[[site_code]]
spec_list <- spec_list[!is.na(spec_list)]

df <- df_pc[, (colnames(df_pc) %in% c(keep_cols, spec_list))] %>%
  filter(SITECODE == site_code)

list_of_years <- unique(df$YEAR)
species_names <- colnames(df)[-(1:length(keep_cols))]

years_av_list = list()
for (ii in 1:length(list_of_years)) {
  df_year <- df %>%
    filter(YEAR == list_of_years[ii])
  
  year_av <- summarize_all(df_year[, -(1:length(keep_cols))], mean)
  year_av$Year <- list_of_years[ii]

  years_av_list[[ii]] <- year_av
}


df_pc_av <- bind_rows(years_av_list)

df2 <- reshape2::melt(df_pc_av ,  id.vars = 'Year', variable.name = 'Species')
p <- ggplot(df2, aes(Year,value)) + 
  geom_line(aes(colour = Species)) +
  geom_point(aes(colour = Species)) +
  labs(title = 'Changing species populations', y = 'Average percentage cover')


ggplotly(p)
