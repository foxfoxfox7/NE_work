library(dplyr)
library(leaflet)
library(htmlwidgets)



data <- data.frame(ID = c("1", "2","3","4","5","6","7","8", '9'),
                   Name = c("2010", "2010", "2010", "2014", "2014", "2019", 
                            "2019", "2019", '2019'),
                   LIGHT = c(12,43,54,34,23,77,44,22, 33),
                   Value2 = c(6,5,2,7,5,6,4,3, 2),
                   Lat = c(51.1, 51.6, 57.3, 52.4, 56.3, 54.3, 60.4, 49.2, 50),
                   Lon = c(5, -3, -2, -1, 4, 3, -5, 0, 1))
data %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lat=~Lat, 
             lng=~Lon, 
             radius = ~LIGHT*1000, 
             group=~Name, 
             label=~Name, 
             popup=~as.character(LIGHT), 
             layerId = ~paste(ID,"LIGHT", sep="")) %>%
  addCircles(lat=~Lat, 
             lng=~Lon, 
             radius = ~Value2, 
             group=~Name, 
             label=~Name, 
             popup=~as.character(Value2), 
             layerId = ~paste(ID,"Value2", sep="")) %>%
  addLayersControl(
    baseGroups = c("LIGHT", "Value2"),
    overlayGroups = c("2010", "2014", "2019"),
    options = layersControlOptions(collapsed = F)
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var baseLayer = 'Value1';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('Value1' !== id.substring(1,)){
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





df_site2 %>%
  leaflet() %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
  addCircleMarkers(lat=~coords.northing, 
                   lng=~coords.easting, 
                   color = ~pal_f(FERTILITY),
                   stroke = FALSE, fillOpacity = 0.7,
                   group=~YEAR, 
                   label=~YEAR, 
                   popup=~as.character(FERTILITY), 
                   layerId = ~paste(PLOT_ID,"FERTILITY", sep="")) %>%
  addCircleMarkers(lat=~coords.northing, 
                   lng=~coords.easting, 
                   color = ~pal_w(WETNESS),
                   stroke = FALSE, fillOpacity = 0.7, 
                   group=~YEAR, 
                   label=~YEAR, 
                   popup=~as.character(WETNESS), 
                   layerId = ~paste(PLOT_ID,"WETNESS", sep="")) %>%
  addLayersControl(
    baseGroups = c("FERTILITY", "WETNESS"),
    overlayGroups = c('2010', '2014', '2019'),
    options = layersControlOptions(collapsed = F)
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var baseLayer = 'Value1';
      myMap.eachLayer(function(layer){
        var id = layer.options.layerId;
        if (id){
          if ('Value1' !== id.substring(1,)){
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

