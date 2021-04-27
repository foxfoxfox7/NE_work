library(tidyverse)
library(readxl)

library(raster)
library(rgdal)
library(sp)



setwd('C:/Users/kiera/Work/NE_work/Weather/')

################################################################################
# my functions
################################################################################

EastNorth_to_LongLat <- function(df) {
  df_plot <- df
  coordinates(df_plot) <- c("EASTINGS", "NORTHINGS")
  proj4string(df_plot) <- projection(bng_proj)
  plotsWGS <- spTransform(df_plot, projection("+proj=longlat +datum=WGS84"))
  
  df$Latitude <- plotsWGS@coords[,2]
  df$Longitude <- plotsWGS@coords[,1]
  
  return(df)
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

################################################################################
# getting centre coords
################################################################################

survey_dir <- './Data/'
survey_file <- 'Ingleborough_LTMN_Vegetation_2010_MAVIS.XLSX'

file_path <- paste0(survey_dir, survey_file)
wpd <- read_excel(file_path, sheet = "Whole Plot Data", skipEmptyRows = TRUE)


bng_proj <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 
  +x_0=400000 +y_0=-100000 +ellps=airy
  +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs'

wpd <- EastNorth_to_LongLat(wpd)
centre_coords <- get_centre_coords(df)

################################################################################
# MET office data
################################################################################

data_dir <- './Data/MET/'
data_files <- list.files(data_dir)

df_raster <- brick(paste0(data_dir, data_files[[2]])) %>%
  projectRaster(., crs="+proj=longlat +datum=WGS84") %>%
  rasterToPoints(.) %>%
  data.frame(.)

near_x <- which.min(abs(df_raster[['x']]-centre_coords[[1]]))
near_y <- which.min(abs(df_raster[['y']]-centre_coords[[2]]))

print(centre_coords)
print(list(df_raster[['x']][near_x], df_raster[['y']][near_y]))

df_slice <- df_raster[which(df_raster[['x']] == df_raster[['x']][near_x] & 
                              df_raster[['y']] == df_raster[['y']][near_y]), ] %>%
  .[ , !names(.) %in% c('x', 'y')]
  
df_site_rep <- data.frame(Date = colnames(df_slice),
                          Rainfall_mm = as.numeric(as.vector(df_slice)))

df_site_rep$Date <- sub('.', '', df_site_rep$Date) %>%
  as.Date(., '%Y.%m.%d')

plot(df_site_rep)

################################################################################
# scrap
################################################################################


dat1 <- brick(paste0(data_dir, data_files[[2]]))  
rp = projectRaster(dat1,crs="+proj=longlat +datum=WGS84")
df_raster <- data.frame(rasterToPoints(rp))

coordinates(plots) <- c("EASTINGS", "NORTHINGS")
proj4string(plots) <- projection("+init=epsg:27700")
plotsWGS <- spTransform(plots, projection("+init=epsg:4326"))

a1 <- 0
a2 <- 359
angle = 180 - abs(abs(a1 - a2) - 180)
print(angle)

snap1 <- dat1[[23]]
plot(snap1)
site_perimeter <- extent(400000, 600000, 200000, 400000)
snap1.crop <- crop(snap1, site_perimeter)
plot(snap1.crop)