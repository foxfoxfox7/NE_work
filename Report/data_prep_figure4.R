library(stringdist)
library(readxl)
library(leaflet)
library(sp)
library(rgdal)
library(plyr)
library(mapview)
library(janitor)
library(knitr)
library(reshape)
library(colorspace)
library(kableExtra)
library(widgetframe)
library(visNetwork, quietly = TRUE)
library(ggthemes)
library(RColorBrewer)
library(tidyverse)

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
  blank_check <- c('Species_richness', 'Bare_ground', 'NVC_FIRST', 'Vegetation_height')
  blank_len <- length(blank_check)
  df <- df[rowSums(is.na(df[blank_check]))!=blank_len,]
  
  # Need to make year character so ggplots recognises it as categories
  df$Year <- as.character(df$Year)
  df$Plot_ID <- as.character(df$Plot_ID)
  
  return(df)
}

get_names <- function(file_name) {
  # Renaming some of the habitat names as they are too far off and wont be found
  name_swap <- read.csv(file_name)
  
  name_swap_bap <- with(name_swap, setNames(right_name, wrong_name))
  name_swap_bap <- name_swap_bap[!is.na(name_swap_bap)]
  name_swap_bap <- name_swap_bap[name_swap_bap != ""]
  
  name_swap_nvc <- with(name_swap, setNames(NVC_name, NVC_code))
  name_swap_nvc <- name_swap_nvc[!is.na(name_swap_nvc)]
  name_swap_nvc <- name_swap_nvc[name_swap_nvc != ""]
  
  # Getting the lsit of approved names for BAP_broad and priorty
  bb_list <- name_swap$BAP_broad
  bb_list <- bb_list[!is.na(bb_list)]
  bb_list <- bb_list[bb_list != ""]
  bp_list <- name_swap$BAP_priority
  bp_list <- bp_list[!is.na(bp_list)]
  bp_list <- bp_list[bp_list != ""]
  
  return(list(name_swap_bap, name_swap_nvc, bb_list, bp_list))
}

name_swap <- function(vector, names) {
  
  for (jj in 1:length(names)) {
    vector <- replace(
      vector,
      vector == names(names[jj]), names[jj])
  }
  
  return(vector)
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

fix_names <- function(df, name_swap_vector) {

  # Replacing know errors with the correct versions
  # (when they are too far away from the correct version for the auto to do it)
  # for (jj in 1:length(name_swap_vector)) {
  #   df$BAP_broad <- replace(
  #     df$BAP_broad,
  #     df$BAP_broad == names(name_swap_vector[jj]), name_swap_vector[jj])
  # }
  df$BAP_broad <- name_swap(df$BAP_broad, name_swap_vector)

  # Renaming any typos in the bap habitats and changing to NA when they don't
  # match our list of habitats
  df$BAP_broad <- unname(sapply(df$BAP_broad, rename, list_comp=bb_list))
  df$BAP_priority <- unname(sapply(df$BAP_priority, rename, list_comp=bp_list))

  
  return(df)
}

# This function selects only the most frequent habitat types to analyse
select_by <- function(df, category, cutoff, include = FALSE) {
  table_cat <- as.data.frame(table(df[[category]]))
  cat_selection <- table_cat %>%
    filter(Freq > cutoff) %>%
    .$Var1 %>%
    as.character(.)
  
  if (is.character(include)) {
    cat_selection <- c(cat_selection, include)
  }
  
  selected_df <- df[df[[category]] %in% cat_selection, ]
  output_list <- list(selected_df, cat_selection)
  
  return(output_list)
}

# Converts Eastings and Northings to longitude and latitude
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
  df_coords <- data.frame(coords = convert_coords(df$Eastings, df$Northings))
  
  colnames(df_coords) <- c('Longitude', 'Latitude')
  
  #df_coords$Plot_ID <- df$Plot_ID
  
  df_coords[ ,c('Plot_ID', 'Year', 'Sitecode')] <- df[ ,c('Plot_ID', 'Year', 'Sitecode')]
  df2 <- full_join(df, df_coords, by = c('Plot_ID', 'Year', 'Sitecode')) %>%
    .[!duplicated(.), ]

  return(df2)
}

get_centre_coords <- function(df) {
  # Finding the centre point of the site
  east_min <- min(df[!is.na(df[ ,'Longitude']),][ ,'Longitude'])
  east_max <- max(df[!is.na(df[ ,'Longitude']),][ ,'Longitude'])
  north_min <- min(df[!is.na(df[ ,'Latitude']),][ ,'Latitude'])
  north_max <- max(df[!is.na(df[ ,'Latitude']),][ ,'Latitude'])
  
  east_cent = (east_min + east_max) / 2
  north_cent = (north_min + north_max) / 2
  
  return(list(east_cent, north_cent))
}

get_change_by_year <- function(df) {
  
  # these columns give inofrmation about the plot but aren't quantitatively comp
  track_cols <- c('Plot_ID', 'Longitude', 'Latitude', 'BAP_broad',
                  'BAP_priority', 'NVC_group', 'NVC_habitat')
  # these columns wil be compared between years
  change_cols <- c('Species_richness', 'Species_diversity', 'Light', 'Wetness',
                   'pH', 'Fertility', 'Competition', 'Stress', 'Ruderals',
                   'Vegetation_height', 'Litter', 'Bare_ground')
  
  df_year_list = list()
  for (ii in 1:length(unique_years)) {
    df_year <- df %>%
      filter(Year == unique_years[ii]) %>%
      select(all_of(track_cols), all_of(change_cols))

    # Adding the year to the column so we can put different years in the same row
    year_marker <- unique_years[ii]
    colnames(df_year)[-1] <- paste(colnames(df_year)[-1], year_marker, sep = '_')
    df_year$Plot_ID <- gsub('a$', '', df_year$Plot_ID)
    
    df_year_list[[ii]] <- df_year
  }
  
  df_change <- df_year_list[[1]]
  
  for (ii in 1:(length(df_year_list)-1)) {
    df_change <- full_join(df_change, df_year_list[[ii+1]], by = 'Plot_ID')
  }
  
  year_change_list = list()
  for (ii in 1:(length(unique_years)-1)) {
    
    east_col <- paste('Longitude', unique_years[ii+1], sep='_')
    north_col <- paste('Latitude', unique_years[ii+1], sep='_')
    bap_col <- paste('BAP_broad', unique_years[ii+1], sep='_')
    nvc_col <- paste('NVC_habitat', unique_years[ii+1], sep='_')
    
    # building the basic blocks of the change df
    yearly_change <- tibble(
      df_change[ , 'Plot_ID'],
      Year = unique_years[ii+1],
      df_change[ ,east_col],
      df_change[ , north_col],
      df_change[ ,bap_col],
      df_change[ ,nvc_col]
    )
    
    # removing the year from the end of the coordinates colums so they join
    names(yearly_change)[names(yearly_change) == east_col] <-
      gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == east_col])
    names(yearly_change)[names(yearly_change) == north_col] <-
      gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == north_col])
    names(yearly_change)[names(yearly_change) == bap_col] <-
      gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == bap_col])
    names(yearly_change)[names(yearly_change) == nvc_col] <-
      gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == nvc_col])
    
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
      yearly_change[ ,feature_col_name_norm] <- 
        yearly_change[ ,feature_col_name_norm] * 100
    }
    
    year_change_list[[ii]] <- yearly_change
  }
  
  total_change <- bind_rows(year_change_list)
  
  return(total_change)
}

get_hab_sums <- function(df) {
  av_cols <- c('Species_richness', 'Species_diversity', 'Light', 'Fertility',
               'pH', 'Wetness', 'Stress', 'Competition', 'Ruderals', 
               'Vegetation_height', 'Litter', 'Bare_ground')
  
  df[ ,av_cols][is.na(df[ ,av_cols])] <- 0
  
  df <- df %>%
    fix_names(., name_swap_bap)
  
  all_habs <- unique(df$BAP_broad)
  
  hab_summary_list = list()
  for (ii in 1:length(all_habs)) {
    df_hab <- df %>%
      filter(BAP_broad == all_habs[ii])
    
    hab_av <- summarize_all(df_hab[, av_cols], mean)
    hab_av$Habitat <- all_habs[ii]
    
    hab_summary_list[[ii]] <- hab_av
  }
  
  hab_summary <- bind_rows(hab_summary_list)
  
  hab_summary <- hab_summary[complete.cases(hab_summary[ , 'Habitat']),]
  #row.names(hab_summary) <- hab_summary$Habitat
  
  return(hab_summary)
}

rem_plot <- function(df, plot) {
  for (ii in 1:length(plot)) {
    df <- df %>%
      filter(Plot_ID != plot[ii])
  }
  
  return(df)
}

################################################################################
# Species graphs
################################################################################

display_averages <- function(df, feature) {
  df_show <- df %>%
    filter(Habitat %in% df_site.bb[[2]]) %>%
    .[ ,c('Habitat', feature)]
  df_show[[feature]] <- round(df_show[[feature]], 1)
  df_show %>%
    kbl(caption = "Average values using all plots across LTMN sites and years.") %>%
    kable_styling()
}

display_survey_dates <- function(df) {

  df_dates <- df %>%
    select(Year, Date) %>%
    group_by(Year) %>%
    summarise(
      Earliest_date = min(Date, na.rm = T),
      Latest_date = max(Date, na.rm = T)
    ) 
  
  df_dates$Earliest_date <- format(df_dates$Earliest_date, '%d-%b')
  df_dates$Latest_date <- format(df_dates$Latest_date, '%d-%b')
  
  df_dates %>%
    kbl(caption = "The range of dates the plots were surveyed on.") %>%
    kable_styling()
}

display_no_plots <- function(df, habitat, feature, title = "Total number of plots for each year and habitat") {
  
  df <- df[!is.na(df[[habitat]]), ]
  
  col1 <- min(df$Year)
  
  df_na <- df %>%
    group_by(Year, !!sym(habitat)) %>%
    summarise(na_count = sum(!is.na(!!sym(feature))), .groups = 'drop_last') %>%
    pivot_wider(names_from = Year, values_from = na_count) %>% 
    arrange(desc(!!sym(col1))) %>%
    adorn_totals("row")
  
  df_na[is.na(df_na)] <- 0
  
  df_na  %>%
    kbl(caption = title) %>%
    kable_styling() %>%
    kable_material("striped")
}

plot_feat_by_hab_year <- function(habitat, feature) {
  if (habitat == 'BAP_broad'){
    df <- df_site.bb[[1]]
  } else if (habitat == 'BAP_priority') {
    df <- df_site.bp[[1]]
  } else if (habitat == 'NVC_group') {
    df <- df_site.nvcb[[1]]
  } else if (habitat == 'NVC_habitat') {
    df <- df_site.nvcc[[1]]
  } else {
    df <- df_site
    print('Habitat grouping not recognised')
  }
  
  df <- df[!is.na(df[ ,feature]),]
  
  #ylim1 = boxplot.stats(df[[feature]])$stats[c(1, 5)]
  
  # PLotting according to year (next to each other showing change)
  pp <- ggplot(df, aes(y=.data[[feature]], x=Year)) +
    geom_boxplot() + 
    #coord_cartesian(ylim = ylim1*1.05) + 
    facet_wrap(.data[[habitat]] ~ .) +
    theme_economist() +#+scale_colour_economist()
    #theme_bw() + 
    theme(axis.title=element_text(size=14))
  print(pp)
}

# Showing the proportion of each NVC group in each bap habitat
# with years next to each other showing change
habitat_by_habitat <- function(group, count){
  if (group == 'BAP_broad'){
    df <- df_site.bb[[1]]
    hab_list <- df_site.bb[[2]]
  } else if (group == 'BAP_priority') {
    df <- df_site.bp[[1]]
    hab_list <- df_site.bp[[2]]
  } else if (group == 'NVC_group') {
    df <- df_site.nvcb[[1]]
    hab_list <- df_site.nvcb[[2]]
  } else if (group == 'NVC_habitat') {
    df <- df_site.nvcc[[1]]
    hab_list <- df_site.nvcc[[2]]
  } else {
    print('Habitat grouping not recognised')
  }
  
  df_count <- df %>%
    group_by(!!sym(group), Year) %>%
    dplyr::count(!!sym(count))
  
  for (ii in 1:length(hab_list)) {
    
    df_hab <- df_count %>%
      filter(!!sym(group) == toString(hab_list[ii]))
    
    ss <- ggplot(df_hab, aes(fill=.data[[count]], y=n, x=Year)) +
      geom_bar(position="stack", stat="identity") + 
      ggtitle(hab_list[ii]) + 
      ylab('Number of plots')
    print(ss)
  }
  
}

plot_ind_species <- function(plot_file, ind_file, site) {

  #importing the information. col_types = cols() takes the default and supresses
  # the output of the guess
  df_pc <- read_csv(plot_file, col_types = cols())
  #importint the list of indicators we want to track
  indicators <- read_csv(ind_file, col_types = cols())
  
  # these are the columns we will keep for each plot to combine with the species 
  # in the plot
  keep_cols <- c('Plot_ID', 'Sitecode', 'Year', 'Eastings', 'Northings', 
                 'BAP_broad', 'NVC_FIRST')
  df_pc[ , !(colnames(df_pc) %in% keep_cols)][is.na(df_pc[ , !(colnames(df_pc) %in% keep_cols)])] <- 0
  
  # getting the appropriate list from the table of indicator species
  spec_list <- indicators[[site]]
  spec_list <- spec_list[!is.na(spec_list)]
  
  # taking only the appropriate species from the chosen site from the data
  df <- df_pc[, (colnames(df_pc) %in% c(keep_cols, spec_list))] %>%
    filter(Sitecode == site)
  
  list_of_years <- unique(df$Year)
  species_names <- colnames(df)[-(1:length(keep_cols))]
  
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
  
  # this reshapes to allow a nice line graph to be drawn (reshape2 might be
  # deprecated at some point)
  df2 <- reshape2::melt(df_pc_av ,  id.vars = 'Year', variable.name = 'Species')

  p <- ggplot(df2, aes(Year,value)) + 
    geom_line(aes(colour = Species)) +
    geom_point(aes(colour = Species)) +
    labs(title = 'Changing species populations', y = 'Average percentage cover') +
    theme(legend.text = element_text(face = "italic"))
  
  # this turns the line plot into a nice widget with controls for the html
  plotly::ggplotly(p)
}

################################################################################
# maps
################################################################################

map_feature <- function(df, feature) {
  
  df_feature <- df[!is.na(df[ ,feature]),]
  
  # make palette
  domain <- range(df_feature[ ,feature])
  pal <- colorNumeric(palette = "reds", domain = domain)
  # making the reverse pallette for the legend
  pal_rev <- colorNumeric(palette = "reds", domain = domain, reverse = TRUE)
  
  m <- leaflet(df_feature) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                     color = ~pal(df_feature[[feature]]),
                     stroke = FALSE, fillOpacity = 0.5) %>%
    addLegend(pal = pal_rev, values = ~df_feature[[feature]],
              title = feature,
              labFormat = labelFormat(transform =
                                        function(x) sort(x, decreasing = TRUE)))
  
  #mapshot(m, file = 'map_feature.png')
  m
}

map_feature_by_hab <- function(df, habitat, feature, size_mod=3, year_sel=3) {
  
  unique_habs1 <- unique(df[[habitat]])
  nn <- length(unique_habs1)
  factpal_site <- colorFactor(brewer.pal(n = nn, name = 'Set2'), unique_habs1)
  
  df_year <- df %>% filter(Year == unique_years[year_sel])
  
  n <- leaflet(df_year) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                     radius = ~df_year[[feature]]/size_mod,
                     color = ~factpal_site(df_year[[habitat]]),
                     stroke = FALSE, fillOpacity = 0.8,
                     label=~Plot_ID) %>%
    addLegend(pal = factpal_site, values = ~df_year[[habitat]], opacity = 1,
              title = habitat)
  
  n

}

map_change <- function(feature, norm=TRUE, year_sel=3){
  
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
  pf_pal <- colorNumeric(palette = c('purple', 'white', 'yellow'), domain = pf_domain)
  # the reverse palette for the legend
  pf_pal_rev <- colorNumeric(palette = c('purple', 'white', 'yellow'),
                             domain = pf_domain,
                             reverse = TRUE)
  

    
  # Setting up the dataframe for analysis, filtering by year and omiting NAs
  df_year_pf <- total_change %>% filter(Year == unique_years[year_sel]) %>%
    .[!is.na(.[ ,pf_col]),]

  ll <- leaflet(df_year_pf) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                     color = pf_pal(df_year_pf[[pf_col]]),
                     stroke = FALSE, fillOpacity = 0.8) %>%
    addLegend(pal = pf_pal, values = df_year_pf[[pf_col]],
              title = feature)#,
  #labFormat = labelFormat(transform =
  #function(x) sort(x, decreasing = TRUE)))
  ll

}

# print(df_site3$Longitude)
# print(df_site3$Latitude)
# p <- leaflet() %>% 
#   addWMSTiles(
#     "https://api.os.uk/maps/raster/v1/wmts?key=1YWdNkHgvWmilsXKITE5JHpHU0We2DOt",
#     layers = "Outdoor:27700",
#   ) %>% 
#   addTiles() %>%
#   setView(lng=east_cent, lat=north_cent, zoom = 14.4) %>%
#   addMarkers(data = df_site, lng = ~Longitude, lat = ~Latitude)
# p

################################################################################
# interactive maps
################################################################################

map_hab_by_year <- function(df, habitat, label) {
  
  df <- df[!is.na(df[[habitat]]), ]
  
  list_of_years <- unique(df$Year)
  list_of_habs <- unique(df[[habitat]])
  
  nn <- length(list_of_habs)
  factpal_site <- colorFactor(brewer.pal(n = nn, name = 'Set2'), list_of_habs)
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  n <- leaflet(df) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
    addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                     color = factpal_site(df[[habitat]]),
                     group = ~Year,
                     label = df[[label]],
                     stroke = FALSE, fillOpacity = 0.7) %>%
    addLegend(pal = factpal_site, 
              values = df[[habitat]], 
              opacity = 0.7,
              title = habitat,
              position = 'bottomleft') %>%
    addLayersControl(baseGroups = list_of_years,
                     options = layersControlOptions(collapsed = F))
  
  n
}

int_map_feat <- function(df, features) {
  df <- df[!is.na(df[ ,features[1]]),]
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  df$unique_id2 <- as.character(1:length(df$Plot_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  list_of_years <- unique(df$Year)

  m <- leaflet(df) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14)
  
  for (jj in 1:length(features)) {
    
    domain <- range(df[[features[jj]]])
    pal <- colorNumeric(palette = "reds", domain = domain)
    
    m <- addCircleMarkers(
      map = m,
      lat=~Latitude, 
      lng=~Longitude, 
      color = pal(df[[features[jj]]]),
      stroke = FALSE, fillOpacity = 1,
      group=~Year, 
      label=~BAP_broad, 
      layerId = ~paste(unique_id2, features[jj], sep="")) %>%
      addLegend(pal = pal,
                values = df[[features[jj]]],
                title = features[jj],
                position = 'bottomleft',
                group = features[jj])
    
  }
  
  widget_text <- sprintf("
    function(el, x) {
      var myMap = this;
      var baseLayer = '%s';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('%s' !== id.substring(25,)){
            layer.getElement().style.display = 'none';
          }
        }
      })
      console.log(myMap.baselayer);
      myMap.on('baselayerchange',
        function (e) {
          baseLayer=e.name;
          myMap.eachLayer(function (layer) {
              var id = layer.options.layerId;
              if (id){
                if (e.name !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                  layer.closePopup();
                }
                if (e.name === id.substring(25,)){
                  layer.getElement().style.display = 'block';
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                if (baseLayer !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                }
            }
          })
        })
    }", features[1], features[1])
  
  m <- addLayersControl(map = m,
                        baseGroups = features,
                        overlayGroups = list_of_years,
                        options = layersControlOptions(collapsed = F)
  ) %>%
    htmlwidgets::onRender(
      widget_text
    ) %>%
    htmlwidgets::onRender("
    function(el, x) {
      var updateLegend = function () {
          var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

          document.querySelectorAll('.legend').forEach(a => a.hidden=true);
          document.querySelectorAll('.legend').forEach(l => {
            if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
          });
      };
      updateLegend();
      this.on('baselayerchange', e => updateLegend());
    }")
  m
}



int_map_feat_by_hab <- function(df, features) {
  
  df$BAP_broad[is.na(df$BAP_broad)] <- 'NA'
  
  df$unique_id2 <- as.character(1:length(df$Plot_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  ################
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  list_of_years <- unique(df$Year)
  list_of_habs <- unique(df$BAP_broad)
  #print(list_of_habs)
  
  df <- df %>%
    filter(Year == list_of_years[length(list_of_years)])
  
  #################
  
  ma <- leaflet(df) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14)
  
  for (jj in 1:length(features)) {
    
    df_feat <- subset(df, !is.na(df[,features[jj]]))
    
    #domain <- quantile(df_feat[[features[jj]]], probs = seq(0, 1, 1/40))[c(2,40)]
    domain <- range(df_feat[[features[jj]]])
    pal <- colorNumeric(palette = 'Blues', domain = domain)
    
    ma <- addCircleMarkers(
      map = ma,
      lat=~Latitude, 
      lng=~Longitude,
      #color = ~factpal_site(BAP_broad),
      color = pal(df_feat[[features[jj]]]),
      #radius = ~((df_feat[[features[jj]]] / max(df_feat[[features[jj]]])) * 10),
      stroke = FALSE, fillOpacity = 1,
      group=~BAP_broad, 
      label=round(df_feat[[features[jj]]], 2), 
      layerId = ~paste(unique_id2, features[jj], sep=""))# %>%
      # addLegend(pal = pal,
      #           values = df_feat[[features[jj]]],
      #           title = features[jj],
      #           position = 'bottomleft',
      #           group = features[jj])
    
  }
  
  widget_text <- sprintf("
    function(el, x) {
      var myMap = this;
      var baseLayer = '%s';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('%s' !== id.substring(25,)){
            layer.getElement().style.display = 'none';
          }
        }
      })
      console.log(myMap.baselayer);
      myMap.on('baselayerchange',
        function (e) {
          baseLayer=e.name;
          myMap.eachLayer(function (layer) {
              var id = layer.options.layerId;
              if (id){
                if (e.name !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                  layer.closePopup();
                }
                if (e.name === id.substring(25,)){
                  layer.getElement().style.display = 'block';
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                if (baseLayer !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                }
            }
          })
        })
    }", features[1], features[1])
  
  ma <- addLayersControl(map = ma,
                        baseGroups = features,
                        overlayGroups = list_of_habs,
                        options = layersControlOptions(collapsed = F)) %>%
    htmlwidgets::onRender(
      widget_text
    )
  
  ma
}

map_habitats <- function(df) {
  
  df$BAP_broad[is.na(df$BAP_broad)] <- 'NA'
  df$NVC_group[is.na(df$NVC_group)] <- 'NA'
  df$NVC_habitat[is.na(df$NVC_habitat)] <- 'NA'
  
  df$unique_id2 <- as.character(1:length(df$Plot_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  ################
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  list_of_years <- unique(df$Year)
  list_of_habs <- unique(df$BAP_broad)
  list_of_nvchabs <- unique(df$NVC_habitat)
  
  nn <- length(list_of_nvchabs)
  factpal_site <- colorFactor(brewer.pal(n = nn, name = 'Set2'), list_of_nvchabs)
  
  #################
  
  
  
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addLegend(pal = factpal_site,
              values = df$NVC_habitat,
              title = 'NVC habitat',
              opacity = 0.7,
              position = 'bottomleft')
  
  for (jj in 1:length(list_of_years)) {
    
    df_year <- df %>%
      filter(Year == list_of_years[jj])
    
    m <- addCircleMarkers(
      map = m,
      lat=df_year$Latitude, 
      lng=df_year$Longitude,
      color = factpal_site(df_year$NVC_habitat),
      stroke = FALSE, fillOpacity = 0.7,
      group=df_year$BAP_broad, 
      label=df_year$NVC_FIRST, 
      layerId = paste(df_year$unique_id2, list_of_years[jj], sep=""))
    
  }
  
  widget_text <- sprintf("
    function(el, x) {
      var myMap = this;
      var baseLayer = '%s';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('%s' !== id.substring(25,)){
            layer.getElement().style.display = 'none';
          }
        }
      })
      console.log(myMap.baselayer);
      myMap.on('baselayerchange',
        function (e) {
          baseLayer=e.name;
          myMap.eachLayer(function (layer) {
              var id = layer.options.layerId;
              if (id){
                if (e.name !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                  layer.closePopup();
                }
                if (e.name === id.substring(25,)){
                  layer.getElement().style.display = 'block';
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                if (baseLayer !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                }
            }
          })
        })
    }", list_of_years[1], list_of_years[1])
  
  m <- addLayersControl(map = m,
                        baseGroups = list_of_years,
                        overlayGroups = list_of_habs,
                        options = layersControlOptions(collapsed = F)) %>%
    htmlwidgets::onRender(
      widget_text
    )
  m
}

change_map_int <- function(df, feat, norm = FALSE, leg_title='Change') {
  
  feature <- paste(feat, 'diff', sep='_')
  feature2 <- paste(feat, 'diff', sep='_')
  
  if (norm == TRUE) {
    feature <- paste(feature, 'norm', sep='_')
  }
  
  df <- df[!is.na(df[ ,feature]),]
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  df$unique_id2 <- as.character(1:length(df$Plot_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  
  # getting the range of the data to make a colour gradient
  pf_max <- max(abs(na.omit(df[feature][[feature]])))
  pf_domain <- c(-pf_max, pf_max)
  # the creation of the colour gradient function
  pf_pal <- colorNumeric(palette = c('purple', 'white', 'yellow'),
                         domain = pf_domain)
  pf_pal_rev <- colorNumeric(palette = c('purple', 'white', 'yellow'),
                         domain = pf_domain, reverse = TRUE)
  
  
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addLegend(pal = pf_pal_rev,
              values = pf_domain,
              title = leg_title,
              position = 'bottomleft',
              labFormat = labelFormat(transform =
                                        function(x) sort(x, decreasing = TRUE)))
  
  for (jj in 1:(length(unique_years)-1)) {
    
    df_year <- df %>%
      filter(Year == unique_years[jj+1])
    
    labs <- lapply(seq(nrow(df_year)), function(i) {
      paste0( '<p>Habitat: ', df_year[i, "BAP_broad"], '</p><p>Actual change: ', 
              as.character(round(df_year[i, feature2], 2)))
    })
    
    m <- addCircleMarkers(
      map = m,
      lat = df_year$Latitude, 
      lng = df_year$Longitude, 
      color = pf_pal(df_year[[feature]]),
      stroke = FALSE, fillOpacity = 1,
      group = df_year$BAP_broad, 
      #label = sprintf('Change: %s Habitat: %s', df_year[[feature2]], df_year$BAP_broad),#df_year[[feature2]], 
      label = lapply(labs, htmltools::HTML),
      layerId = paste(df_year$unique_id2, unique_years[jj+1], sep=""))
  }
  
  widget_text <- sprintf("
    function(el, x) {
      var myMap = this;
      var baseLayer = '%s';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('%s' !== id.substring(25,)){
            layer.getElement().style.display = 'none';
          }
        }
      })
      console.log(myMap.baselayer);
      myMap.on('baselayerchange',
        function (e) {
          baseLayer=e.name;
          myMap.eachLayer(function (layer) {
              var id = layer.options.layerId;
              if (id){
                if (e.name !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                  layer.closePopup();
                }
                if (e.name === id.substring(25,)){
                  layer.getElement().style.display = 'block';
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                if (baseLayer !== id.substring(25,)){
                  layer.getElement().style.display = 'none';
                }
            }
          })
        })
    }", unique_years[2], unique_years[2])
  
  m <- addLayersControl(map = m,
                        baseGroups = unique_years[-1],
                        overlayGroups = unique_habs,
                        options = layersControlOptions(collapsed = F)) %>%
    htmlwidgets::onRender(
      widget_text
    )
  m
}
