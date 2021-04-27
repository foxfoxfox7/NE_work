library(RPostgres)

library(tidyverse)
library(dplyr)
library(naniar)
library(zoo) #na approx
library(stringr)

library(dbplyr)
library(readxl) # reading in the excel files

library(raster)
library(rgdal)
library(sp)



laptop_dir <- 'C:/Users/kiera/Work/NE_work/Vegetation/postgresql/'
setwd(laptop_dir)

con <- DBI::dbConnect(RPostgres::Postgres(), 
                      dbname = "ltmn_veg",  
                      host = 'localhost',
                      port = 5432, 
                      user = "kieran", 
                      password = "2757")



df_all <- read.csv('../Report/dataframes/all_plots.csv', na.strings=c("N/A", "", 'NA', '<NA>'))
df_all <- df_all[grepl("^NA", rownames(df_all))==F,]
df_all<- df_all[rowSums(is.na(df_all)) != ncol(df_all), ]
#df_species <- read.csv('../Report/dataframes/all_species_f.csv')

df_all[['Survey']] <- paste(df_all[['Sitecode']], df_all[['Year']], sep='_')
df_all[['Plot_ID_u']] <- paste(df_all[['Sitecode']], df_all[['Year']], df_all[['Plot_ID']], sep='_')

df_all <- df_all %>%
  dplyr::group_by(Survey) %>%
  fill(Date, .direction = "downup") %>%
  dplyr::ungroup()

df_all[['plot_move']] <- grepl('[a]$', df_all[['Plot_ID']])
df_all[['Plot_ID']] <- gsub('[a]$', '', df_all[['Plot_ID']])
df_all[['Date']] <- as.Date(df_all[['Date']], '%Y-%m-%d')

#df_all <- df_all[(df_all[['Sitecode']] != '<NA>'), ]
#df_all <- df_all[!grepl('[NA]', df_all$Sitecode), ]
df_all <- df_all[!is.na(df_all$Sitecode), ]

df_all[is.na(df_all$Species_richness), "Species_richness"] <- 0
df_all[(df_all$Species_richness == 0), "Species_diversity"] <- 0

#df_all[is.na(df_all[['NVC_FIRST']]),'Species_richness'] <- 0
#df_all[is.na(df_all$Species_richness), c("Species_richness", "Species_diversity")] <- 0

df_all <- df_all[!is.na(df_all$Eastings),]

bng_proj <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 
  +x_0=400000 +y_0=-100000 +ellps=airy
  +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs'

EastNorth_to_LongLat <- function(df) {
  df_plot <- df
  coordinates(df_plot) <- c("Eastings", "Northings")
  proj4string(df_plot) <- projection(bng_proj)
  plotsWGS <- spTransform(df_plot, projection("+proj=longlat +datum=WGS84"))
  
  df$Latitude <- plotsWGS@coords[,2]
  df$Longitude <- plotsWGS@coords[,1]
  
  return(df)
}

df_all <- EastNorth_to_LongLat(df_all)

df_all <- df_all %>%
  group_by(Survey, NVC_habitat) %>%
  mutate(Light_interp = na.approx(Light, na.rm=FALSE))


df_all[is.na(df_all[['Light']]), 'Light_interp']




colnames(df_all) <- str_to_lower(colnames(df_all))

dbWriteTable(con, "plot_list", value = df_all, overwrite = T, append = F, row.names = FALSE)

df_test <- dbGetQuery(con, "SELECT species_richness, bap_broad FROM plot_list
                      WHERE sitecode = 'B01'
                      ORDER BY species_richness;")

df_test <- dbGetQuery(con, "SELECT dat_interp FROM plot_list;")

################################################################################

summary(df_all)

df_all[is.na(df_all[['STD_HEIGHT']]),]
NVC_cols <- c("NVC_FIRST", "NVC_subgroup", "NVC_group", "NVC_habitat")
df_all[(df_all$NVC_subgroup == 'FAILED'), NVC_cols]# <- 'NA'
df_all <- df_all[is.na(df_all$Species_richness), ]
df_all <- replace_with_na(df_all, list('FAILED:to', 'FAILED'))

df_all <- df_all %>%
  group_by(Survey) %>%
  complete(Date) %>% 
  ungroup()
df_all$Date <- fill(df_all$Date, .direction='down')

df_all <- df_all[ ,!names(df_all) %in% c('Light_interp', 'col_name')]

################################################################################


dbListTables(con)
dbWriteTable(con, "site_list", value = df, overwrite = T, append = F, row.names = FALSE)
df_test <- dbGetQuery(con, "SELECT * FROM site_list;")

################################################################################

file_dir <- '../Data/All_data_old/'
list_of_files <- list.files(file_dir)

codes <-c()
for (ii in 1:length(list_of_files)) {
  
  file_path <- paste(file_dir, list_of_files[ii], sep='')
  wpd <- read_excel(file_path, sheet = "Whole Plot Data")
  
  codes <- c(codes, wpd$SITECODE[1])
}

list_of_files_u <- list_of_files %>%
  substr(.,1,nchar(list_of_files)-32) %>%
  unique()

df_site <- data.frame(Sitecode = unique(codes),
                      Name = list_of_files_u,
                      veg_surveys = 0)

################################################################################


colnames(df_all)
interp_cols <- c('Light', 'Wetness', 'pH', 'Fertility', 'Competition', 'Stress', 'Ruderals')

for (ii in 1:length(interp_cols)) {
  
  var_sym <- rlang::sym(interp_cols[ii])
  new_name <- rlang::sym(paste(interp_cols[ii], 'interp', sep='_'))
  
  # df_all <- df_all %>%
  #   group_by(Survey, NVC_habitat) %>%
  #   mutate(!!var_sym := na.approx(!!new_name, na.rm=FALSE)) %>%
  #   ungroup()
  
  df_all <- df_all %>%
    group_by(Survey, NVC_habitat) %>%
    mutate('{interp_cols[ii]}' := na.approx('{interp_cols[ii]}_interp', na.rm=FALSE)) %>%
    ungroup()
}

name <- 'Light'

df_all <- df_all %>%
  group_by(Survey, NVC_habitat) %>%
  mutate('{name}_interp' := na.approx('{name}', na.rm=FALSE)) %>%
  ungroup()


df_all <- df_all %>%
  group_by(Survey, NVC_habitat) %>%
  mutate(Light_interp = na.approx(Light, na.rm=FALSE)) %>%
  ungroup()
