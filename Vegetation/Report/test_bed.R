setwd("C:/Users/kiera/Work/NE_work/Report")
getwd()
source('./data_prep_figure4.R')

################################################################################
name_swap_bap <- get_names('./dataframes/rename_habitat.csv')[[1]]
name_swap_nvc <- get_names('./dataframes/rename_habitat.csv')[[2]]
bb_list <- get_names('./dataframes/rename_habitat.csv')[[3]]
bp_list <- get_names('./dataframes/rename_habitat.csv')[[4]]

#df_all <- read_all_data('all_plots.csv')
df_all <- read_csv('./dataframes/report_plots.csv', col_types = cols())
site_code <- 'B14'

# Taking the data from just one site
df_site <- filter(df_all, Sitecode == site_code) %>%
  fix_names(., name_swap_bap) %>%
  # transform the eastings and northings into latitude and longitude
  transform_coords(.) %>%
  distinct(Plot_ID, Sitecode, Year, .keep_all = TRUE)


# Converts the NVC abbreviations to the full names
df_site$NVC_habitat <- name_swap(df_site$NVC_habitat, name_swap_nvc)
df_site$Year <- as.character(df_site$Year)

df_site <- rem_plot(df_site, c(30, '30', '30a')) # Lullington
#df_site <- rem_plot(df_site, c(14, '14', '14a')) # thursley

# Making the sub dataframes with only the major habitats selected
#object[[1]] is the data frame. object[[2]] is the list of habitats
df_site.bb <- select_by(df_site, 'BAP_broad', 700, include = c('Calcareous Grassland', 'Dwarf Shrub Heath'))
df_site.bp <- select_by(df_site, 'BAP_priority', 15)
df_site.nvcb <- select_by(df_site, 'NVC_group', 15)
df_site.nvcc <- select_by(df_site, 'NVC_habitat', 500, include = c('Calcareous grassland (CG)', 'Heath (H)'))

# the centre points of the maps
east_cent <- get_centre_coords(df_site)[[1]]
north_cent <- get_centre_coords(df_site)[[2]]

unique_years <- unique(df_site$Year)
unique_habs <- unique(df_site$BAP_broad)
unique_nvchabs <- unique(df_site$NVC_habitat)

df_change <- df_site %>%
  get_change_by_year() %>%
  distinct(Plot_ID, Year, .keep_all = TRUE)

hab_sum <- read_csv('./dataframes/all_plots.csv', col_types = cols()) %>%
  get_hab_sums()