library(tidyverse)
library(stringdist)
library(readxl)
library(leaflet)
library(sp)
library(rgdal)
library(plyr)

setwd('C:/Users/Kieran/Desktop/NE')
getwd()


################################################################################
# My functions
################################################################################



# Converts EASTINGS and NORTHINGS to longitude and latitude
convert_coords <- function(easting,northing) {
  out = cbind(easting,northing)
  mask = !is.na(easting)
  
  # INput strings for the coordinate conversion function
  wgs84 = "+init=epsg:4326"
  bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 
         +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
  sp <-  sp::spTransform(
    sp::SpatialPoints(list(easting[mask],northing[mask]),proj4string=sp::CRS(bng)),
    sp::CRS(wgs84))
  out[mask,]=sp@coords
  out
}

# This function selects only the most frequent habitat types to analyse
select_by <- function(category, cutoff) {
  table_cat <- as.data.frame(table(df_site[[category]]))
  print(table_cat)
  cat_selection <- table_cat %>%
    filter(Freq > cutoff) %>%
    .$Var1
  print(cat_selection)

  selected_df <- df_site[df_site[[category]] %in% cat_selection, ]
  output_list <- list(selected_df, cat_selection)

  return(output_list)
}

rename <- function(input, list_comp) {
  # Makes automatic name changes where the name is close to the names in a list
  # of pre-determined names from a list
  if ( (!input %in% list_comp) & (!is.na(input)) ) {
    list_of_matches <- stringdist(input, list_comp, method='jw')
    if (min(list_of_matches) < naming_cutoff) {
      new_name <- list_comp[which.min(list_of_matches)]
      return(new_name)
    } else {
      return(NA)
    }
  } else {
    return(input)
  }
}

################################################################################
# importing all information
################################################################################

naming_cutoff <- 0.2

# Renaming some of the habitat names as they are too far off and wont be found
name_swap <- read.csv('rename_habitat.csv')

name_swap_vector <- with(name_swap, setNames(right_name, wrong_name))
name_swap_vector <- name_swap_vector[!is.na(name_swap_vector)]
name_swap_vector <- name_swap_vector[name_swap_vector != ""]

# Getting the lsit of approved names for bap broad and priorty
bb_list <- name_swap$Bap_broad
bb_list <- bb_list[!is.na(bb_list)]
bb_list <- bb_list[bb_list != ""]
bp_list <- name_swap$Bap_priority
bp_list <- bp_list[!is.na(bp_list)]
bb_list <- bb_list[bb_list != ""]

################################################################################
# setting up the dataframes
################################################################################

# reading in the dataframe of all sites
df <- read.csv('all_plots.csv', ) %>%
  tibble(.) %>%
  # Gets rid of the added index column when loading csv
  subset(., select = -X)

# Removing all the rows that have no real information
# These are usually because the plot wasn't surveyed
blank_check <- c('Species_richness', 'bare.x', 'NVC_FIRST', 'MEAN_HEIGHT')
blank_len <- length(blank_check)
df <- df[rowSums(is.na(df[blank_check]))!=blank_len,]

# Need to make year character so ggplots recognises it as categories
df$YEAR <- as.character(df$YEAR)
df$PLOT_ID <- as.character(df$PLOT_ID)

# Taking the data from just one site
df_site <- filter(df, SITECODE == 'B14')

print(unique(df_site$BAP_BROAD))
print(unique(df_site$BAP_PRIORITY))

# Replacing know errors with the correct versions
# (when they are too far away from the correct version for the auto to do it)
for (jj in 1:length(name_swap_vector)) {
  df_site$BAP_BROAD <- replace(
    df_site$BAP_BROAD,
    df_site$BAP_BROAD == names(name_swap_vector[jj]), name_swap_vector[jj]
  )
}

# Renaming any typos in the bap habitats and changing to NA when they don't
# match our list of habitats
df_site$BAP_BROAD <- sapply(df_site$BAP_BROAD, rename, list_comp=bb_list)
df_site$BAP_PRIORITY <- sapply(df_site$BAP_PRIORITY, rename, list_comp=bp_list)

# Making the sub dataframes with only the major habitats selected
#object[[1]] is the data frame. object[[2]] is the list of habitats
df_site.bb <- select_by('BAP_BROAD', 20)
df_site.bp <- select_by('BAP_PRIORITY', 15)
df_site.nvcb <- select_by('NVC_groupb', 15)
df_site.nvcc <- select_by('NVC_groupc', 15)

################################################################################
# Species graphs
################################################################################
# 
# print(names(df_site))
# 
# # PLotting according to year (next to each other showing change)
# pp <- ggplot(df_site.bb[[1]], aes(y=LIGHT, x=YEAR)) +
#   geom_boxplot()
# print(pp)
# 
# # Separated out by habitat (ontop of each other)
# print(pp + facet_grid(BAP_BROAD ~ .))
# 
# # Showing the proportion of each NVC group in each bap habitat
# # with years next to each other showing change
# df_nvc_count <- df_site.bb[[1]] %>%
#   group_by(BAP_BROAD, YEAR) %>%
#   dplyr::count(NVC_groupc)
# 
# for (ii in 1:length(df_site.bb[[2]])) {
#   print(df_site.bb[[2]][ii])
#   ss <- ggplot(df_nvc_count %>% filter(BAP_BROAD == toString(df_site.bb[[2]][ii])),
#                aes(fill=NVC_groupc, y=n, x=YEAR)) +
#     geom_bar(position="stack", stat="identity")
#   print(ss)
# }
# 
# # Showing the proportion of each NVC subgroup in each NVC group
# # with years next to each other showing change
# df_nvc_subgroup_count <- df_site.nvcc[[1]] %>%
#   group_by(NVC_groupc, YEAR) %>%
#   dplyr::count(NVC_groupb)
# 
# for (ii in 1:length(df_site.nvcc[[2]])) {
#   print(df_site.nvcc[[2]][ii])
#   ss <- ggplot(df_nvc_subgroup_count %>% filter(NVC_groupc == toString(df_site.nvcc[[2]][ii])),
#                aes(fill=NVC_groupb, y=n, x=YEAR)) +
#     geom_bar(position="stack", stat="identity")
#   print(ss)
# }
# 
# # PLotting according to habitat (next to each other)
# qq <- ggplot(df_site.bb[[1]], aes(x=BAP_BROAD, y=Species_richness)) +
#   geom_boxplot()
# print(qq)

################################################################################
# maps
################################################################################

# Converting the coordinates from BNG (UTM) to longitude and latitude
df_coords <- data.frame(coords = convert_coords(df_site$EASTINGS, df_site$NORTHINGS))
df_coords$PLOT_ID <- df_site$PLOT_ID
df_site2 <- full_join(df_site, df_coords) %>%
  .[!duplicated(.), ]

# Finding the centre point of the site
east_min <- min(df_site2[!is.na(df_site2[ ,'coords.easting']),][ ,'coords.easting'])
east_max <- max(df_site2[!is.na(df_site2[ ,'coords.easting']),][ ,'coords.easting'])
north_min <- min(df_site2[!is.na(df_site2[ ,'coords.northing']),][ ,'coords.northing'])
north_max <- max(df_site2[!is.na(df_site2[ ,'coords.northing']),][ ,'coords.northing'])

east_cent = (east_min + east_max) / 2
north_cent = (north_min + north_max) / 2

unique_years <- unique(df_site2$YEAR)
unique_habs <- unique(df_site2$BAP_BROAD)


track_cols <- c('PLOT_ID', 'coords.easting', 'coords.northing', 'BAP_BROAD', 
                'BAP_PRIORITY', 'NVC_groupb', 'NVC_groupc')

change_cols <- c('Species_richness', 'Species_diversity', 'LIGHT', 'WETNESS', 
                 'PH', 'FERTILITY', 'COMPETITION', 'STRESS', 'RUDERALS', 
                 'MEAN_HEIGHT', 'litter', 'bare.x')

df_year_list = list()
for (ii in 1:length(unique_years)) {
  df_year <- df_site2 %>%
    filter(YEAR == unique_years[ii]) %>%
    select(track_cols, change_cols)
  
  print(df_year)
  
  # Adding the year to the column so we can put different years in the same year
  year_marker <- unique_years[ii]
  colnames(df_year)[-1] <- paste(colnames(df_year)[-1], year_marker, sep = '_')
  df_year$PLOT_ID <- gsub('a$', '', df_year$PLOT_ID)
  
  df_year_list[[ii]] <- df_year
}

df_change <- df_year_list[[1]]

for (ii in 1:(length(df_year_list)-1)) {
  df_change <- full_join(df_change, df_year_list[[ii+1]])
}


year_change_list = list()
for (ii in 1:(length(unique_years)-1)) {
  
  
  
  east_col <- paste('coords.easting', unique_years[ii+1], sep='_')
  north_col <- paste('coords.northing', unique_years[ii+1], sep='_')
  
  # building the basic blocks of the change df
  yearly_change <- tibble(
    df_change[ , 'PLOT_ID'],
    YEAR = unique_years[ii+1],
    df_change[ ,east_col],
    df_change[ , north_col]
  )
  
  # removing the year from the end of the coordinates colums so they join
  names(yearly_change)[names(yearly_change) == east_col] <- 
    gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == east_col])
  names(yearly_change)[names(yearly_change) == north_col] <- 
    gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == north_col])
  
  for (jj in 1:length(change_cols)) {
    # choosing the feature to look at the difference
    feature <- change_cols[jj]
    
    # getting the two columns to take the difference between
    col_from <- paste(feature, unique_years[ii], sep='_')
    col_to <- paste(feature, unique_years[ii+1], sep='_')
    
    # Making the new feature column names
    feature_col_name <- paste(feature, 'diff', sep='_')
    feature_col_name_norm <- paste(feature, 'diff', 'norm', sep='_')
    
    # Saving the difference between the years as a new column
    # and another column for the normalised difference
    yearly_change[ ,feature_col_name] <- 
      df_change[ ,col_to] - df_change[ ,col_from]
    yearly_change[ ,feature_col_name_norm] <- 
      (df_change[ ,col_to] - df_change[ ,col_from]) / 
      ((df_change[ ,col_to] + df_change[ ,col_from]) /2 ) # the average (norm)
  }

  year_change_list[[ii]] <- yearly_change
}

total_change <- bind_rows(year_change_list)

################################################################################
# maps
################################################################################

df_feature <- df_site2[!is.na(df_site2[ ,'FERTILITY']),]

# make palette
domain <- range(df_feature[ ,'FERTILITY'])
pal <- colorNumeric(palette = "Purples", domain = domain)
# making the reverse pallette for the legend
pal_rev <- colorNumeric(palette = "Purples", domain = domain, reverse = TRUE)

m <- leaflet(df_feature) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
  addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                   color = ~pal(FERTILITY),
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addLegend(pal = pal_rev, values = ~FERTILITY,
            labFormat = labelFormat(transform = 
                                      function(x) sort(x, decreasing = TRUE)))
m



#factpal <- colorFactor(topo.colors(length(bb_list)), bb_list)
factpal_site <- colorFactor(topo.colors(length(unique_habs)), unique_habs)

for (ii in 1:length(unique_years)) {
  
  df_year <- df_site2 %>% filter(YEAR == unique_years[ii])

  m <- leaflet(df_year) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
    addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                     radius = ~Species_richness/3,
                     color = ~factpal_site(BAP_BROAD),
                     stroke = FALSE, fillOpacity = 0.8) %>%
    addLegend(pal = factpal_site, values = ~BAP_BROAD, opacity = 1)
    
  print(m)
}


# Selecting the feature to investigate
pf <- change_cols[6]
print(pf)
# Getting the names of the columns (feature and feature_norm)
pf_col <- paste(pf, 'diff', sep='_')
pf_col_norm <- paste(pf, 'diff', 'norm', sep='_')
# getting the range of the data to make a colour gradient
pf_max <- max(abs(na.omit(total_change[pf_col_norm][[pf_col_norm]])))
pf_domain <- c(-pf_max, pf_max)
# the creation of the colour gradient function
pf_pal <- colorNumeric(palette = c('red', 'white', 'blue'), domain = pf_domain)
# the reverse palette for the legend
pf_pal_rev <- colorNumeric(palette = c('red', 'white', 'blue'), 
                       domain = pf_domain,
                       reverse = TRUE)

for (ii in 1:(length(unique_years)-1)) {

  # Setting up the dataframe for analysis, filtering by year and omiting NAs
  df_year_pf <- total_change %>% filter(YEAR == unique_years[ii+1]) %>%
    .[!is.na(.[ ,pf_col_norm]),]
  
  m <- leaflet(df_year_pf) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                     color = pf_pal(df_year_pf[[pf_col_norm]]),
                     stroke = FALSE, fillOpacity = 0.8) %>%
    addLegend(pal = pf_pal_rev, values = df_year_pf[[pf_col_norm]],
              labFormat = labelFormat(transform = 
                                      function(x) sort(x, decreasing = TRUE)))
  print(m)
}





# print(df_site3$coords.easting)
# print(df_site3$coords.northing)
# p <- leaflet() %>% 
#   addWMSTiles(
#     "https://api.os.uk/maps/raster/v1/wmts?key=1YWdNkHgvWmilsXKITE5JHpHU0We2DOt",
#     layers = "Outdoor:27700",
#   ) %>% 
#   addTiles() %>%
#   setView(lng=east_cent, lat=north_cent, zoom = 14.4) %>%
#   addMarkers(data = df_site, lng = ~coords.easting, lat = ~coords.northing)
# p