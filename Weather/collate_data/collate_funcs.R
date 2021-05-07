library(tidyverse)
library(readxl)
library(zoo)



combine_weather <- function(w_list) {
  
  df_all <- bind_rows(weather_list) %>%
    .[order(.$date), ]
  
  dt1 <- df_all[['date']][1]
  dt2 <- df_all[['date']][length(df_all[['date']])]
  dt_gap <- df_all[['date']][3] - df_all[['date']][2]
  ts <- seq.POSIXt(dt1, dt2, dt_gap)
  df_time <- data.frame(date = ts)
  
  df <- full_join(df_time,df_all) %>%
    .[order(.$date), ] %>%
    .[!is.na(.$date), ] %>%
    fill(site)
  
  ncols <- length(colnames(df))
  
  df[rowSums(is.na(df[,4:ncols]))==(ncols-3),][['data_source']] <- 'NO_DATA'
  
  return(df) 
}


from_raw <- function(df_in) {
  
  
  df_in[df_in == -6999] <- NA
  df_in <- df_in[rowSums(is.na(df_in)) != ncol(df_in), ]
  
  df_in[['DATE']] <- as.character(df_in[['DATE']])
  df_in[['HOUR_MIN']] <- as.character(df_in[['HOUR_MIN']])
  df_in[['HOUR_MIN']] <- gsub(".*? ", "", df_in[['HOUR_MIN']])
  
  df_in[['DATE']] <- paste(df_in[['DATE']], df_in[['HOUR_MIN']], sep=' ')
  df_in[['DATE']] <- as.POSIXct(df_in[['DATE']],
                                format="%Y-%m-%d %H:%M:%S",
                                tz=Sys.timezone())
  df_in[['DATE']] <- round(df_in[['DATE']], units="hours")
  
  df_in <- df_in[!is.na(df_in$DATE), ]
  df_in <- df_in[order(df_in$DATE), ]
  df_in %>% distinct(DATE, .keep_all= TRUE)
  
  dt1 <- df_in[['DATE']][1]
  dt2 <- df_in[['DATE']][length(df_in[['DATE']])]
  #dt_gap <- df_in[['DATE']][3] - df_in[['DATE']][2]
  dt_gap <- as.POSIXct('2002-05-03 02:00:00') - as.POSIXct('2002-05-03 01:00:00')
  ts <- seq.POSIXt(dt1, dt2, dt_gap)
  df_time <- data.frame(date = ts)
  
  keep_cols <- c('SITE', 'DATA_SOURCE', 'DATE', 'DRY_BULB_TEMP', 'SOLAR_RAD', 
                 'RELATIVE_HUMIDITY', 'RAIN_MM', 'WIND_SPEED', 'WIND_DIR', 
                 'SOIL_TEMP_10CM', 'SOIL_TEMP_30CM')
  ncols <- length(keep_cols)
  df_data <- subset(df_in, select = keep_cols)
  
  colnames(df_data) <- c('site', 'data_source', 'date', 'air_temp', 'radiation',
                         'relative_humidity', 'rain_mm', 'wind_speed_kn', 
                         'wind_direction', 'soil_temp_10cm', 'soil_temp_30cm')
  
  df <- full_join(df_time,df_data) %>%
    tibble(.) %>%
    .[!is.na(.$date), ] %>%
    fill(c(data_source, site))
  
  df[rowSums(is.na(df[,4:ncols]))==(ncols-3),][['data_source']] <- 'NO_DATA'
  
  return(df)
}


from_camp2 <- function(df_in, sitecode) {
  
  df <- tibble(site = sitecode,
               data_source = 'Campbells_2nd',
               date = df_in$`Timestamp (UTC+0)`,
               air_temp = df_in$AIRTemp_Avg,
               radiation = df_in$Radiation_Avg,
               relative_humidity = df_in$RH,
               rain_mm = df_in$Rain_mm_Tot,
               wind_speed_kn = df_in$WS_knot_Avg,
               wind_direction = df_in$WindDir_Wvc,
               soil_temp_10cm = df_in$Soil_10cm_Avg,
               soil_temp_30cm = df_in$Soil_30cm_Avg)
  
  df[['date']] <- as.POSIXct(df[['date']],
                             format="%Y-%m-%d %H:%M",
                             tz=Sys.timezone())
  
  df <- df%>%
    .[!is.na(.$date), ] %>%
    .[order(.$date), ]
  
  return(df)
}

