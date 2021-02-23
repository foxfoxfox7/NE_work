library(tidyverse)
library(stringdist)
library(readxl)
library(leaflet)
library(sp)
library(rgdal)
library(plyr)
library(mapview)
library(janitor)

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
  
  name_swap_bap <- with(name_swap, setNames(right_name, wrong_name))
  name_swap_bap <- name_swap_bap[!is.na(name_swap_bap)]
  name_swap_bap <- name_swap_bap[name_swap_bap != ""]
  
  name_swap_nvc <- with(name_swap, setNames(NVC_name, NVC_code))
  name_swap_nvc <- name_swap_nvc[!is.na(name_swap_nvc)]
  name_swap_nvc <- name_swap_nvc[name_swap_nvc != ""]
  
  # Getting the lsit of approved names for bap broad and priorty
  bb_list <- name_swap$Bap_broad
  bb_list <- bb_list[!is.na(bb_list)]
  bb_list <- bb_list[bb_list != ""]
  bp_list <- name_swap$Bap_priority
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
  #   df$BAP_BROAD <- replace(
  #     df$BAP_BROAD,
  #     df$BAP_BROAD == names(name_swap_vector[jj]), name_swap_vector[jj])
  # }
  df$BAP_BROAD <- name_swap(df$BAP_BROAD, name_swap_vector)

  # Renaming any typos in the bap habitats and changing to NA when they don't
  # match our list of habitats
  df$BAP_BROAD <- unname(sapply(df$BAP_BROAD, rename, list_comp=bb_list))
  df$BAP_PRIORITY <- unname(sapply(df$BAP_PRIORITY, rename, list_comp=bp_list))

  
  return(df)
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
  
  # these columns give inofrmation about the plot but aren't quantitatively comp
  track_cols <- c('PLOT_ID', 'coords.easting', 'coords.northing', 'BAP_BROAD',
                  'BAP_PRIORITY', 'NVC_groupb', 'NVC_groupc')
  # these columns wil be compared between years
  change_cols <- c('Species_richness', 'Species_diversity', 'LIGHT', 'WETNESS',
                   'PH', 'FERTILITY', 'COMPETITION', 'STRESS', 'RUDERALS',
                   'MEAN_HEIGHT', 'litter', 'bare.x')
  
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
    print(ss + ggtitle(hab_list[ii]))
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
              title = feature,
              labFormat = labelFormat(transform =
                                        function(x) sort(x, decreasing = TRUE)))
  
  #mapshot(m, file = 'map_feature.png')
  m
}

map_feature_by_hab <- function(df, habitat, feature, size_mod=3, year_sel=3) {
  
  unique_habs1 <- unique(df[[habitat]])
  #factpal <- colorFactor(topo.colors(length(bb_list)), bb_list)
  factpal_site <- colorFactor(topo.colors(length(unique_habs1)), unique_habs1)
  
  df_year <- df %>% filter(YEAR == unique_years[year_sel])
  
  n <- leaflet(df_year) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
    addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                     radius = ~df_year[[feature]]/size_mod,
                     color = ~factpal_site(df_year[[habitat]]),
                     stroke = FALSE, fillOpacity = 0.8) %>%
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
  pf_pal <- colorNumeric(palette = c('red', 'white', 'blue'), domain = pf_domain)
  # the reverse palette for the legend
  pf_pal_rev <- colorNumeric(palette = c('red', 'white', 'blue'),
                             domain = pf_domain,
                             reverse = TRUE)
  

    
  # Setting up the dataframe for analysis, filtering by year and omiting NAs
  df_year_pf <- total_change %>% filter(YEAR == unique_years[year_sel]) %>%
    .[!is.na(.[ ,pf_col]),]

  ll <- leaflet(df_year_pf) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                     color = pf_pal(df_year_pf[[pf_col]]),
                     stroke = FALSE, fillOpacity = 0.8) %>%
    addLegend(pal = pf_pal, values = df_year_pf[[pf_col]],
              title = feature)#,
  #labFormat = labelFormat(transform =
  #function(x) sort(x, decreasing = TRUE)))
  ll

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

################################################################################
# interactive maps
################################################################################

map_hab_by_year <- function(df, habitat) {
  
  list_of_years <- unique(df$YEAR)
  list_of_habs <- unique(df[[habitat]])
  
  factpal_site <- colorFactor(topo.colors(length(list_of_habs)), list_of_habs)
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  n <- leaflet(df) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14.3) %>%
    addCircleMarkers(lng = ~coords.easting, lat = ~coords.northing,
                     color = factpal_site(df[[habitat]]),
                     group = ~YEAR,
                     stroke = FALSE, fillOpacity = 0.8) %>%
    addLegend(pal = factpal_site, 
              values = df[[habitat]], 
              opacity = 1,
              title = habitat,
              position = 'bottomleft',) %>%
    addLayersControl(baseGroups = list_of_years,
                     options = layersControlOptions(collapsed = F))
  
  n
}

int_map_feat <- function(df, features) {
  df <- df[!is.na(df[ ,features[1]]),]
  
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  df$unique_id2 <- as.character(1:length(df$PLOT_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  list_of_years <- unique(df$YEAR)

  m <- leaflet(df) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14)
  
  for (jj in 1:length(features)) {
    
    domain <- range(df[[features[jj]]])
    pal <- colorNumeric(palette = "Purples", domain = domain)
    
    m <- addCircleMarkers(
      map = m,
      lat=~coords.northing, 
      lng=~coords.easting, 
      color = pal(df[[features[jj]]]),
      stroke = FALSE, fillOpacity = 1,
      group=~YEAR, 
      label=~BAP_BROAD, 
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
  
  df$BAP_BROAD[is.na(df$BAP_BROAD)] <- 'NA'
  
  df$unique_id2 <- as.character(1:length(df$PLOT_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  ################
  
  features <- c('MEAN_HEIGHT', 'litter', 'bare.x')
  print(names(df))
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  list_of_years <- unique(df$YEAR)
  list_of_habs <- unique(df$BAP_BROAD)
  print(list_of_habs)
  
  df <- df %>%
    filter(YEAR == list_of_years[length(list_of_years)])
  
  #################
  
  m <- leaflet(df) %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14)
  
  for (jj in 1:length(features)) {
    
    #df_feat <- subset(df, !is.na(df[,features[jj]]))
    
    #domain <- quantile(df_feat[[features[jj]]], probs = seq(0, 1, 1/40))[c(2,40)]
    domain <- range(df_feat[[features[jj]]])
    pal <- colorNumeric(palette = "Purples", domain = domain)
    
    m <- addCircleMarkers(
      map = m,
      lat=~coords.northing, 
      lng=~coords.easting,
      #color = ~factpal_site(BAP_BROAD),
      color = pal(df_feat[[features[jj]]]),
      #radius = ~((df_feat[[features[jj]]] / max(df_feat[[features[jj]]])) * 10),
      stroke = FALSE, fillOpacity = 1,
      group=~BAP_BROAD, 
      label=df_feat[[features[jj]]], 
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
                        overlayGroups = list_of_habs,
                        options = layersControlOptions(collapsed = F)) %>%
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

map_habitats <- function(df) {
  
  df$BAP_BROAD[is.na(df$BAP_BROAD)] <- 'NA'
  df$NVC_groupb[is.na(df$NVC_groupb)] <- 'NA'
  df$NVC_groupc[is.na(df$NVC_groupc)] <- 'NA'
  
  df$unique_id2 <- as.character(1:length(df$PLOT_ID)) %>%
    str_pad(., width = 25, side = 'right', pad = 'z')
  
  ################
  
  print(names(df))
  centre_coords <- get_centre_coords(df)
  east_cent <- centre_coords[[1]]
  north_cent <- centre_coords[[2]]
  
  list_of_years <- unique(df$YEAR)
  list_of_habs <- unique(df$BAP_BROAD)
  list_of_nvchabs <- unique(df$NVC_groupc)
  
  factpal_site <- colorFactor(topo.colors(length(list_of_nvchabs)), list_of_nvchabs)
  
  #################
  
  
  
  m <- leaflet() %>%
    addTiles() %>%
    setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
    addLegend(pal = factpal_site,
              values = df$NVC_groupc,
              title = 'NVC habitat',
              position = 'bottomleft')
  
  for (jj in 1:length(list_of_years)) {
    
    df_year <- df %>%
      filter(YEAR == list_of_years[jj])
    
    m <- addCircleMarkers(
      map = m,
      lat=df_year$coords.northing, 
      lng=df_year$coords.easting,
      color = factpal_site(df_year$NVC_groupc),
      stroke = FALSE, fillOpacity = 7,
      group=df_year$BAP_BROAD, 
      label=df_year$NVC_groupb, 
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


