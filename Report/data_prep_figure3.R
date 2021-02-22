library(tidyverse)
library(stringdist)
library(readxl)
library(leaflet)
library(sp)
library(rgdal)
library(plyr)
library(mapview)

################################################################################
# My functions
################################################################################

read_all_data <- function(file_name2) {
  # reading in the dataframe of all sites
  df <- read.csv(file_name2, ) %>%
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
  
  return(df)
}

get_names <- function(file_name) {
  # Renaming some of the habitat names as they are too far off and wont be found
  name_swap <- read.csv(file_name)
  
  name_swap_vector <- with(name_swap, setNames(right_name, wrong_name))
  name_swap_vector <- name_swap_vector[!is.na(name_swap_vector)]
  name_swap_vector <- name_swap_vector[name_swap_vector != ""]
  
  # Getting the lsit of approved names for bap broad and priorty
  bb_list <- name_swap$Bap_broad
  bb_list <- bb_list[!is.na(bb_list)]
  bb_list <- bb_list[bb_list != ""]
  bp_list <- name_swap$Bap_priority
  bp_list <- bp_list[!is.na(bp_list)]
  bp_list <- bp_list[bp_list != ""]
  
  return(list(name_swap_vector, bb_list, bp_list))
}

# This function selects only the most frequent habitat types to analyse
select_by <- function(category, cutoff) {
  table_cat <- as.data.frame(table(df_site[[category]]))
  cat_selection <- table_cat %>%
    filter(Freq > cutoff) %>%
    .$Var1
  
  selected_df <- df_site[df_site[[category]] %in% cat_selection, ]
  output_list <- list(selected_df, cat_selection)
  
  return(output_list)
}

rename <- function(input, list_comp, naming_cutoff=0.2) {
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

fix_names <- function(df) {
  # Replacing know errors with the correct versions
  # (when they are too far away from the correct version for the auto to do it)
  for (jj in 1:length(name_swap_vector)) {
    df$BAP_BROAD <- replace(
      df$BAP_BROAD,
      df$BAP_BROAD == names(name_swap_vector[jj]), name_swap_vector[jj]
    )
  }
  
  # Renaming any typos in the bap habitats and changing to NA when they don't
  # match our list of habitats
  df$BAP_BROAD <- sapply(df$BAP_BROAD, rename, list_comp=bb_list)
  df$BAP_PRIORITY <- sapply(df$BAP_PRIORITY, rename, list_comp=bp_list)
  
  return(df)
}

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

transform_coords <- function(df) {
  # Converting the coordinates from BNG (UTM) to longitude and latitude
  df_coords <- data.frame(coords = convert_coords(df$EASTINGS, df$NORTHINGS))
  df_coords$PLOT_ID <- df$PLOT_ID
  df2 <- full_join(df, df_coords, by = 'PLOT_ID') %>%
    .[!duplicated(.), ]

  return(df2)
}

get_centre_coords <- function(df) {
  # Finding the centre point of the site
  east_min <- min(df[!is.na(df[ ,'coords.easting']),][ ,'coords.easting'])
  east_max <- max(df[!is.na(df[ ,'coords.easting']),][ ,'coords.easting'])
  north_min <- min(df[!is.na(df[ ,'coords.northing']),][ ,'coords.northing'])
  north_max <- max(df[!is.na(df[ ,'coords.northing']),][ ,'coords.northing'])
  
  east_cent = (east_min + east_max) / 2
  north_cent = (north_min + north_max) / 2
  
  return(list(east_cent, north_cent))
}

get_change_by_year <- function(df) {
  
  df_year_list = list()
  for (ii in 1:length(unique_years)) {
    df_year <- df %>%
      filter(YEAR == unique_years[ii]) %>%
      select(all_of(track_cols), all_of(change_cols))

    # Adding the year to the column so we can put different years in the same year
    year_marker <- unique_years[ii]
    colnames(df_year)[-1] <- paste(colnames(df_year)[-1], year_marker, sep = '_')
    df_year$PLOT_ID <- gsub('a$', '', df_year$PLOT_ID)
    
    df_year_list[[ii]] <- df_year
  }
  
  df_change <- df_year_list[[1]]
  
  for (ii in 1:(length(df_year_list)-1)) {
    df_change <- full_join(df_change, df_year_list[[ii+1]], by = 'PLOT_ID')
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
  
  return(total_change)
}

################################################################################
# Species graphs
################################################################################

plot_feat_by_hab_year <- function(habitat, feature) {
  if (habitat == 'BAP_BROAD'){
    df <- df_site.bb[[1]]
  } else if (habitat == 'BAP_PRIORITY') {
    df <- df_site.bp[[1]]
  } else if (habitat == 'NVC_groupb') {
    df <- df_site.nvcb[[1]]
  } else if (habitat == 'NVC_groupc') {
    df <- df_site.nvcc[[1]]
  } else {
    df <- df_site
    print('Habitat grouping not recognised')
  }
  
  ylim1 = boxplot.stats(df[[feature]])$stats[c(1, 5)]
  
  # PLotting according to year (next to each other showing change)
  pp <- ggplot(df, aes(y=.data[[feature]], x=YEAR)) +
    geom_boxplot() + 
    coord_cartesian(ylim = ylim1*1.05) + 
    facet_wrap(.data[[habitat]] ~ .)
  print(pp)
}

# Showing the proportion of each NVC group in each bap habitat
# with years next to each other showing change
habitat_by_habitat <- function(group, count){
  if (group == 'BAP_BROAD'){
    df <- df_site.bb[[1]]
    hab_list <- df_site.bb[[2]]
  } else if (group == 'BAP_PRIORITY') {
    df <- df_site.bp[[1]]
    hab_list <- df_site.bp[[2]]
  } else if (group == 'NVC_groupb') {
    df <- df_site.nvcb[[1]]
    hab_list <- df_site.nvcb[[2]]
  } else if (group == 'NVC_groupc') {
    df <- df_site.nvcc[[1]]
    hab_list <- df_site.nvcc[[2]]
  } else {
    print('Habitat grouping not recognised')
  }
  
  df_count <- df %>%
    group_by(!!sym(group), YEAR) %>%
    dplyr::count(!!sym(count))
  
  for (ii in 1:length(hab_list)) {
    
    df_hab <- df_count %>%
      filter(!!sym(group) == toString(hab_list[ii]))
    
    ss <- ggplot(df_hab, aes(fill=.data[[count]], y=n, x=YEAR)) +
      geom_bar(position="stack", stat="identity")
    print(ss)
  }
  
}

# # PLotting according to habitat (next to each other)
# qq <- ggplot(df_site.bb[[1]], aes(x=BAP_BROAD, y=Species_richness)) +
#   geom_boxplot()
# print(qq)

################################################################################
# maps
################################################################################

map_feature <- function(df, feature) {
  
  df_feature <- df[!is.na(df[ ,feature]),]
  
  # make palette
  domain <- range(df_feature[ ,feature])
  pal <- colorNumeric(palette = "Purples", domain = domain)
  # making the reverse pallette for the legend
  pal_rev <- colorNumeric(palette = "Purples", domain = domain, reverse = TRUE)
  
  m <- leaflet(df_feature) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                     color = ~pal(df_feature[[feature]]),
                     stroke = FALSE, fillOpacity = 0.5) %>%
    addLegend(pal = pal_rev, values = ~df_feature[[feature]],
              labFormat = labelFormat(transform =
                                        function(x) sort(x, decreasing = TRUE)))
  
  #mapshot(m, file = 'map_feature.png')
  m
}

map_feature_by_hab <- function(df, habitat, feature, size_mod=3) {
  
  unique_habs1 <- unique(df[[habitat]])
  #factpal <- colorFactor(topo.colors(length(bb_list)), bb_list)
  factpal_site <- colorFactor(topo.colors(length(unique_habs1)), unique_habs1)
  
  #year_map_list = list()
  for (ii in 1:length(unique_years)) {
    
    df_year <- df %>% filter(YEAR == unique_years[ii])
    
    n <- leaflet(df_year) %>%
      addTiles() %>%
      setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
      addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                       radius = ~df_year[[feature]]/size_mod,
                       color = ~factpal_site(df_year[[habitat]]),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal = factpal_site, values = ~df_year[[habitat]], opacity = 1)
    
    #year_map_list[[ii]] <- m
    mapshot(n, file = 'map_habitat.png')
    print(n)
  }
  #print(year_map_list[[length(unique_years)]])
}

map_change <- function(feature, norm=TRUE){
  
  # Getting the names of the columns (feature and feature_norm)
  if (norm) {
    pf_col <- paste(feature, 'diff', 'norm', sep='_')
  } else {
    pf_col <- paste(feature, 'diff', sep='_')
  }

  # getting the range of the data to make a colour gradient
  pf_max <- max(abs(na.omit(total_change[pf_col][[pf_col]])))
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
      .[!is.na(.[ ,pf_col]),]
    
    m <- leaflet(df_year_pf) %>%
      addTiles() %>%
      setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
      addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                       color = pf_pal(df_year_pf[[pf_col]]),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal = pf_pal, values = df_year_pf[[pf_col]])#,
    #labFormat = labelFormat(transform =
    #function(x) sort(x, decreasing = TRUE)))
    print(m)
  }
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