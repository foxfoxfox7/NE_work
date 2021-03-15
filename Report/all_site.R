setwd("C:/Users/kiera/Work/NE_work/Report")
getwd()
source('./data_prep_figure4.R')

################################################################################

name_swap_bap <- get_names('./dataframes/rename_habitat.csv')[[1]]
name_swap_nvc <- get_names('./dataframes/rename_habitat.csv')[[2]]
bb_list <- get_names('./dataframes/rename_habitat.csv')[[3]]
bp_list <- get_names('./dataframes/rename_habitat.csv')[[4]]

#df_all <- read_all_data('all_plots.csv')
df_all <- read_csv('./dataframes/all_plots.csv') %>%
  fix_names(., name_swap_bap) %>%
  # transform the eastings and northings into latitude and longitude
  transform_coords(.) %>%
  distinct(Plot_ID, Sitecode, Year, .keep_all = TRUE)

# Converts the NVC abbreviations to the full names
df_all$NVC_habitat <- name_swap(df_all$NVC_habitat, name_swap_nvc)
df_all$Plot_ID <- sub("a$", "", df_all$Plot_ID)

site_code <- 'B14'
df_site <- filter(df_all, Sitecode == site_code)

df_site.bb <- select_by(df_site, 'BAP_broad', 20, include = c('Dwarf Shrub Heath'))

plot_feat_by_hab_year('BAP_broad', 'Species_richness')

################################################################################

plot_feat <- function(df, feature) {
  
  df <- df[!is.na(df[ ,feature]),]
  
  # PLotting according to year (next to each other showing change)
  pp <- ggplot(df, aes(y=.data[[feature]], x=Year)) +
    geom_boxplot() + 
    facet_wrap(.data[['BAP_broad']] ~ .) +
    theme_bw() + 
    theme(axis.title=element_text(size=14)) +
    ylim(c(0,25))# +
    #geom_abline(intercept = coefs[1], slope = coefs[2])
  print(pp)
}


plot_feat2 <- function(df, feature) {
  
  df <- df[!is.na(df[ ,feature]),]
  
  # PLotting according to year (next to each other showing change)
  pp <- ggplot(df, aes(y=.data[[feature]], x=Year)) +
    geom_point() + 
    theme_bw() + 
    theme(axis.title=element_text(size=14)) +
    ylim(c(0,25)) +
  geom_smooth(method=lm)
  print(pp)
}

df_hab <- df_all %>%
  filter(NVC_FIRST == 'H' | BAP_broad == 'Dwarf Shrub Heath')

coefs <- coef(lm(Species_richness ~ Year, data = df_all))
print(coefs)

plot_feat2(df_hab, 'Species_richness')



df_hab2 <- df_hab
df_hab2$Year <- as.character(df_hab2$Year)
plot_feat(df_hab2, 'Species_richness')




plot_feat(df_all, 'pH')

df_hab <- filter(df_all, BAP_broad == 'Dwarf Shrub Heath')
plot_feat(df_hab, 'Species_richness')


plot_feat(df_all, 'Species_richness')

df_count <- df_hab %>%
  group_by(Year) %>%
  dplyr::count(BAP_broad) %>%
  pivot_wider(names_from = Year, values_from = n)

df_count %>%
  kbl(caption = "Total number of Heath plots in LTMN surveys by year.") %>%
  kable_styling()

################################################################################

plot_species <- function(df, spec_list) {
  
  # these are the columns we will keep for each plot to combine with the species 
  # in the plot
  keep_cols <- c('Plot_ID', 'Sitecode', 'Year', 'Eastings', 'Northings', 
                 'BAP_broad', 'NVC_FIRST')
  # getting the appropriate list from the table of indicator species
  # taking only the appropriate species from the chosen site from the data
  df <- df[, (colnames(df) %in% c(keep_cols, spec_list))]
  
  list_of_years <- unique(df$Year)
  
  # going through each year in turn, taking an average of the plots
  years_av_list = list()
  for (ii in 1:length(list_of_years)) {
    df_year <- df %>%
      filter(Year == list_of_years[ii])
    
    year_av <- summarize_all(df_year[, -(1:length(keep_cols))], mean)
    year_av$Year <- list_of_years[ii]
    
    years_av_list[[ii]] <- year_av
  }
  df_pc_av <- bind_rows(years_av_list)
  
  df_pc_av <- df_pc_av %>%
    mutate(Total = select(., spec_list) %>% rowSums(na.rm = TRUE))
  
  # this reshapes to allow a nice line graph to be drawn (reshape2 might be
  # deprecated at some point)
  df2 <- reshape2::melt(df_pc_av ,  id.vars = 'Year', variable.name = 'Species')
  
  p <- ggplot(df2, aes(Year,value)) + 
    geom_line(aes(colour = Species), size = 1) +
    geom_point(aes(colour = Species)) +
    labs(y = 'Average percentage cover') +
    theme(legend.text = element_text(face = "italic")) + 
    theme_bw() + 
    theme(
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 14),
      axis.title=element_text(size=14)
    )
  
  # this turns the line plot into a nice widget with controls for the html
  p
  
}

df_pc <- read_csv('./dataframes/all_all_species_pc.csv')
#importint the list of indicators we want to track

df_pc <- df_pc %>%
  filter(Sitecode == 'B14')

df_pc <- df_pc %>%
  filter(NVC_FIRST == 'H' | BAP_broad == 'Dwarf Shrub Heath')

spec_l <- c('Erica cinerea', 'Calluna vulgaris', 'Erica tetralix')
plot_species(df_pc, spec_l)


spec_l <- c('Pteridium aquilinum', 'Erica cinerea', 'Calluna vulgaris')
plot_species(df_pc, spec_l)


spec_l <- c('Festuca rubra', 'Rubus fruticosus agg.', 'Pteridium aquilinum')
plot_species(df_pc, spec_l)


df_hab <- df_pc %>%
  filter(BAP_broad == 'Dwarf Shrub Heath')

df_count <- df_hab %>%
  group_by(Sitecode, Year) %>%
  dplyr::count(BAP_broad) %>%
  pivot_wider(names_from = Year, values_from = n)

df_count %>%
  kbl(caption = "Total number of Heath plots in LTMN surveys by year.") %>%
  kable_styling()


################################################################################

site_code <- 'B14'
df_site <- filter(df_all, Sitecode == site_code)

plot_feat_by_hab_year('BAP_broad', 'Species_richness')

plot_list_bank <- c('13','24','31','27','10','45','16','3','4','44')

plot_list_middle <- c('19','17','48','40','32','33','43','29','35','41','22',
                      '20','6','23','15','7','34','12','2')


df_site <-filter(df_site, Plot_ID %in% plot_list_middle)


# the centre points of the maps
east_cent <- get_centre_coords(df_site)[[1]]
north_cent <- get_centre_coords(df_site)[[2]]

unique_years <- unique(df_site$Year)
unique_habs <- unique(df_site$BAP_broad)
unique_nvchabs <- unique(df_site$NVC_habitat)


map_hab_by_year(df_site, 'NVC_habitat', 'NVC_FIRST')

df_site$NVC_community <- df_site$NVC_group

habitat <- 'NVC_habitat'
year_sel <- 1

unique_habs1 <- unique(df_site[[habitat]])
unique_years <- unique(df_site$Year)

nn <- length(unique_habs1)
factpal_site <- colorFactor(brewer.pal(n = nn, name = 'Set2'), unique_habs1)

df_year <- df_site %>% filter(Year == unique_years[year_sel])

n <- leaflet(df_year) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 16.4) %>%
  addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                   color = ~factpal_site(df_year[[habitat]]),
                   stroke = FALSE, fillOpacity = 0.8,
                   label=~Plot_ID) %>%
  addLegend(pal = factpal_site, values = ~df_year[[habitat]], opacity = 1,
            title = habitat)

n
