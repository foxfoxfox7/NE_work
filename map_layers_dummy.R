library(dplyr)
library(leaflet)
library(htmlwidgets)



setwd('C:/Users/Kieran/Desktop/NE/NE_work')
getwd()
source('./data_prep_figure4.R')


name_gl <- get_names('rename_habitat.csv')
name_swap_vector <- name_gl[[1]]
bb_list <- name_gl[[2]]
bp_list <- name_gl[[3]]

df_all <- read_all_data('all_plots.csv')
# Taking the data from just one site
df_site <- filter(df_all, SITECODE == 'B15')
df_site <- fix_names(df_site)

# transform the eastings and northings into latitude and longitude
df_site <- transform_coords(df_site)
df_site <- df_site[!duplicated(df_site[ ,c('PLOT_ID', 'YEAR')]), ]

df_site$unique_id2 <- as.character(1:length(df_site$PLOT_ID)) %>%
  str_pad(., width = 25, side = 'right', pad = 'z')
print(names(df_site))
########################################

# the centre points of the maps
centre_coords <- get_centre_coords(df_site)
east_cent <- centre_coords[[1]]
north_cent <- centre_coords[[2]]



data <- data.frame(ID = df_site$unique_id2,
                   Name = df_site$BAP_BROAD,
                   Species_diversity = df_site$Species_diversity,
                   Species_richness = df_site$Species_richness,
                   Lat = df_site$coords.northing,
                   Lon = df_site$coords.easting)

data %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
  addCircles(lat=~Lat, 
             lng=~Lon, 
             radius = ~Species_diversity, 
             group=~Name, 
             label=~Name, 
             popup=~as.character(Species_diversity), 
             layerId = ~paste(ID,"Species_diversity", sep="")) %>%
  addCircles(lat=~Lat, 
             lng=~Lon, 
             radius = ~Species_richness, 
             group=~Name, 
             label=~Name, 
             popup=~as.character(Species_richness), 
             layerId = ~paste(ID,"Species_richness", sep="")) %>%
  addLayersControl(
    baseGroups = c("Species_diversity", "Species_richness"),
    overlayGroups = unique(df_site$BAP_BROAD),
    options = layersControlOptions(collapsed = F)
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var baseLayer = 'Species_diversity';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('Species_diversity' !== id.substring(1,)){
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
                if (e.name !== id.substring(1,)){
                  layer.getElement().style.display = 'none';
                  layer.closePopup();
                }
                if (e.name === id.substring(1,)){
                  layer.getElement().style.display = 'block';
                }
              }

          });
        })
        myMap.on('overlayadd', function(e){
          myMap.eachLayer(function(layer){
            var id = layer.options.layerId;
            if (id){
                if (baseLayer !== id.substring(1,)){
                  layer.getElement().style.display = 'none';
                }
            }    
          })
        })
    }")


