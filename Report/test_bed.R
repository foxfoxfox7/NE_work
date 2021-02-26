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

int_map_feat_by_hab(df, c('STRESS', 'COMPETITION', 'RUDERALS'))

features <- c('STRESS', 'COMPETITION', 'RUDERALS')
df$BAP_BROAD[is.na(df$BAP_BROAD)] <- 'NA'

df$unique_id2 <- as.character(1:length(df$PLOT_ID)) %>%
  str_pad(., width = 25, side = 'right', pad = 'z')

################

centre_coords <- get_centre_coords(df)
east_cent <- centre_coords[[1]]
north_cent <- centre_coords[[2]]

list_of_years <- unique(df$YEAR)
list_of_habs <- unique(df$BAP_BROAD)
#print(list_of_habs)

df <- df %>%
  filter(YEAR == list_of_years[length(list_of_years)])

#################

ma <- leaflet(df) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14)

for (jj in 1:length(features)) {
  
  df_feat <- subset(df, !is.na(df[,features[jj]]))
  
  #domain <- quantile(df_feat[[features[jj]]], probs = seq(0, 1, 1/40))[c(2,40)]
  domain <- range(df_feat[[features[jj]]])
  pal <- colorNumeric(palette = "Purples", domain = domain)
  
  ma <- addCircleMarkers(
    map = ma,
    lat=~coords.northing, 
    lng=~coords.easting,
    #color = ~factpal_site(BAP_BROAD),
    color = pal(df_feat[[features[jj]]]),
    #radius = ~((df_feat[[features[jj]]] / max(df_feat[[features[jj]]])) * 10),
    stroke = FALSE, fillOpacity = 1,
    group=~BAP_BROAD, 
    label=df_feat[[features[jj]]], 
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
  )# %>%
# htmlwidgets::onRender("
# function(el, x) {
#   var updateLegend = function () {
#       var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
# 
#       document.querySelectorAll('.legend').forEach(a => a.hidden=true);
#       document.querySelectorAll('.legend').forEach(l => {
#         if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
#       });
#   };
#   updateLegend();
#   this.on('baselayerchange', e => updateLegend());
# }")
ma



















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
