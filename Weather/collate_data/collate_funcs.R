library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)


# this is a list of codes for the site and the representative site
# eg B03 means burnham beeches
site_list <- list(B03 = 'HEATHROW',
                  B36 = 'LINTON ON OUSE',
                  B48 = 'FYLINGDALES',
                  B39 = 'DONNA NOOK NO 2',
                  B30 = 'BROOMS BARN',
                  B16 = 'MONKS WOOD',
                  B42 = 'WALNEY ISLAND',
                  B50 = 'GREAT DUN FELL NO 2',
                  B46 = 'CHIVENOR',
                  B01 = 'CROSBY',
                  B45 = 'MONKS WOOD')

# these are all the ays in which the date can be enterred in some of the
# csv files, they can all be converted into the same datetime
date_codes <- c('dbY', 'dby', 'dmy', 'dmY', 'dmyHM', 'dmYHM', 'dbyHM', 'dbYHM',
                'dmyHMS', 'dmYHMS', 'dbyHMS', 'dbYHMS')

################################################################################
# Extract from file
# these are used to extract data from the various foms it is in
# i create a tibble with a bunch of the most relevant formats
# it would be easy to addcolumns to that tibble to get it to publication standard
################################################################################

from_mod_collate <- function(file_path, suffix = '_suf') {

  df <- read.csv(file_path) %>%
    tibble()

  colnames(df) <- str_to_lower(df[3,])
  colnames(df)[1] <- 'date'
  colnames(df) <- make.names(colnames(df), unique=TRUE)
  colnames(df)[2:ncol(df)] <- paste0(colnames(df)[2:ncol(df)], suffix)

  df <- tail(df, -5)
  df[['date']] <- as.POSIXct(df[['date']], formate='%Y-%m-%d')
  df <- df[df$date >= as.POSIXct('2005-01-01', formate='%Y-%m-%d'), ]

  #df[ ,2:ncol(df)] <- as.numeric(df[ ,2:ncol(df)])
  df[2:ncol(df)] <- sapply(df[2:ncol(df)],as.numeric)

  return(df)
}

from_raw <- function(file_path) {

  raw_col_typ <- c('text', 'text', 'date', 'numeric', 'date', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'text', 'text', 'text', 'text')

  df_in <- read_excel(file_path, sheet = 1, col_types = raw_col_typ)

  df_in[df_in == -6999] <- NA
  df_in <- df_in[rowSums(is.na(df_in)) != ncol(df_in), ]

  df_in[['DATE']] <- as.character(df_in[['DATE']])
  df_in[['HOUR_MIN']] <- as.character(df_in[['HOUR_MIN']])
  df_in[['HOUR_MIN']] <- gsub(".*? ", "", df_in[['HOUR_MIN']])

  df_in[['DATE']] <- paste(df_in[['DATE']], df_in[['HOUR_MIN']], sep=' ')
  df_in[['DATE']] <- as.POSIXct(df_in[['DATE']],
                                format="%Y-%m-%d %H:%M:%S",
                                tz=Sys.timezone())

  keep_cols <- c('SITE', 'DATA_SOURCE', 'DATE', 'DRY_BULB_TEMP', 'SOLAR_RAD',
                 'RELATIVE_HUMIDITY', 'RAIN_MM', 'WIND_SPEED', 'WIND_DIR',
                 'SOIL_TEMP_10CM', 'SOIL_TEMP_30CM')
  ncols <- length(keep_cols)
  df_data <- subset(df_in, select = keep_cols)

  colnames(df_data) <- c('site', 'data_source', 'date', 'air_temp', 'radiation',
                         'relative_humidity', 'rain_mm', 'wind_speed_kn',
                         'wind_direction', 'soil_temp_10cm', 'soil_temp_30cm')

  # Fills in gaps in the data timeline with NO_DATA
  df <- make_date_gaps(df_data)

  return(df)
}

from_camp2 <- function(file_path, sitecode) {

  df_in <- read_excel(file_path, sheet = 1)

  df <- tibble(site = sitecode,
               data_source = 'Campbells_2nd',
               date = df_in$`Timestamp (UTC+0)`,
               air_temp = as.numeric(df_in$AIRTemp_Avg),
               radiation = as.numeric(df_in$Radiation_Avg),
               relative_humidity = as.numeric(df_in$RH),
               rain_mm = as.numeric(df_in$Rain_mm_Tot),
               wind_speed_kn = as.numeric(df_in$WS_knot_Avg),
               wind_direction = as.numeric(df_in$WindDir_Wvc),
               soil_temp_10cm = as.numeric(df_in$Soil_10cm_Avg),
               soil_temp_30cm = as.numeric(df_in$Soil_30cm_Avg))

  df[['date']] <- as.POSIXct(df[['date']],
                             format="%Y-%m-%d %H:%M",
                             tz=Sys.timezone())

  df <- make_date_gaps(df)

  return(df)
}

from_cosmos_collate <- function(file_path, sitecode) {

  df_in <- read.csv(file_path)

  df <- tibble(site = sitecode,
               data_source = 'COSMOS',
               date = df_in$DATE_AND_TIME,
               air_temp = df_in$TEMP_DEGC,
               radiation = NA,
               relative_humidity = df_in$HUMIDITY_PERCENT,
               rain_mm = df_in$RAIN_MM,
               wind_speed_kn = df_in$WIND_SPEED_MS,
               wind_direction = df_in$WIND_DIRECT_DEGREES,
               soil_temp_10cm = NA,
               soil_temp_30cm = NA)

  df[['date']] <- as.POSIXct(df[['date']],
                             format="%d-%m-%Y %H.%M",
                             tz=Sys.timezone())

  df <- make_date_gaps(df)

  return(df)
}

from_cosmos_month_excel <- function(file_path, sitecode) {

  raw_col_typ <- c('date', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                   'numeric', 'numeric')

  df_in <- read_excel(file_path, sheet='Data',
                      col_types = raw_col_typ, skip = 1) %>%
    tail(., -1)
  colnames(df_in) <- make.names(colnames(df_in), unique=TRUE)
  colnames(df_in)

  df <- tibble(date = df_in$Date,
               site = sitecode,
               data_source = 'COSMOS',
               max_temp = as.numeric(df_in$Maximum.Air.Temperature),
               min_temp = as.numeric(df_in$Minimum.Air.Temperature),
               rain_sum = as.numeric(df_in$Total.Precipitation))

  df[['date']] <- as.POSIXct(df[['date']], format = '%d/%m/%Y',
                             tz=Sys.timezone())

  return(df)
}

from_cosmos_month_csv3 <- function(file_path, sitecode, date_f = '%Y-%m-%d') {

  df_in <- read.csv(file_path, skip = 5) %>%
    tail(., -1)

  colnames(df_in)[1] <- 'date'

  df <- tibble(date = df_in$date,
               site = sitecode,
               data_source = 'COSMOS',
               max_temp = as.numeric(df_in$Max.air.temperature..1.day.),
               min_temp = as.numeric(df_in$Min.air.temperature..1.day.),
               rain_sum = as.numeric(df_in$Total.precipitation))


  df[['date']] <- as.POSIXct(df[['date']], format = date_f,
                             tz=Sys.timezone())

  return(df)
}

from_aws <- function(file_path) {

  df_in <- read_excel(file_path, sheet=1, na = c('', "NA", "N/A", '', -6999)) %>%
    .[rowSums(is.na(.)) != ncol(.), ]

  colnames(df_in) <- str_to_lower(colnames(df_in))

  years <- unique(df_in$year_rtm)
  year_splits = list()
  for (ii in seq_along(years)) {
    df_year <- df_in[(df_in$year_rtm == years[ii]), ]
    start_date <- paste0(years[ii], '-01-01')
    df_year$day_rtm <- as.Date(df_year$day_rtm, origin = start_date)

    year_splits[[ii]] <- df_year
  }

  df_out <- bind_rows(year_splits)

  df_out$hour_min <- as.character(df_out$hour_min) %>%
    sub(".*? ", "", .)
  df_out$date <- paste(as.character(df_out$day_rtm), df_out$hour_min) %>%
    as_datetime(., format = '%Y-%m-%d %H:%M:%S')

  df <- tibble(date = df_out$date,
               data_source = 'campbells_1st',
               site = df_out$site,
               air_temp = df_out$t107_c_a,
               radiation = df_out$solar_ra,
               relative_humidity = df_out$rh,
               rain_mm = df_out$rain_mm,
               wind_speed_kn = (df_out$ws_ms_s_)*1.944,
               wind_direction = df_out$winddir_...10,
               soil_temp_10cm = NA,
               soil_temp_30cm = NA)

  return(df)
}

from_aws_processed2 <- function(file_path, sitecode, excel=FALSE) {

  if (!excel) {
    df_in <- read.csv(file_path)
  } else {
    df_in <- read_excel(file_path, sheet=1)
  }

  df_in$OB_TIME[grep("[0-9]{2}/[0-9]{2}/[0-9]{4}$",df_in$OB_TIME)] <- paste(
    df_in$OB_TIME[grep("[0-9]{2}/[0-9]{2}/[0-9]{4}$",df_in$OB_TIME)],"00:00:00")

  df_in[['OB_TIME']] <- as.POSIXct(df_in[['OB_TIME']],
                                   format='%d/%m/%Y %H:%M',
                                   tz=Sys.timezone())

  df <- tibble(site = sitecode,
               data_source = 'aws',
               date = df_in$OB_TIME,
               air_temp = as.numeric(df_in$AIR_TEMPERATURE),
               radiation = NA,
               relative_humidity = as.numeric(df_in$RLTV_HUM),
               rain_mm = as.numeric(df_in$PRCP_AMT),
               wind_speed_kn = as.numeric(df_in$WIND_SPEED),
               wind_direction = as.numeric(df_in$WIND_DIRECTION),
               soil_temp_10cm = as.numeric(df_in$Q10CM_SOIL_TEMP),
               soil_temp_30cm = as.numeric(df_in$Q30CM_SOIL_TEMP))

  df <- make_date_gaps(df)

  return(df)
}

from_rep <- function(file_path, sitecode, excel = FALSE) {

  if (excel) {
    df_in <- read_excel(file_path, sheet=1)
  } else {
    df_in <- read.csv(file_path)
  }
  colnames(df_in) <- c('site', 'date', 'radiation', 'wetbulb', 'air_temp',
                       'wind_speed', 'wind_direction', 'rain', 'temp_10cm',
                       'temp_30cm', 'humidity')

  site_name <- site_list[[sitecode]]

  df_in <- df_in %>%
    distinct(site, date, .keep_all=TRUE) %>%
    subset(site = site_name)

  df_in$date <- parse_date_time(df_in$date, date_codes)

  df <- tibble(site = sitecode,
               data_source = 'Representative',
               date = df_in$date,
               air_temp = as.numeric(df_in$air_temp),
               radiation = as.numeric(df_in$radiation),
               relative_humidity = as.numeric(df_in$humidity),
               rain_mm = as.numeric(df_in$rain),
               wind_speed_kn = as.numeric(df_in$wind_speed),
               wind_direction = as.numeric(df_in$wind_direction),
               soil_temp_10cm = as.numeric(df_in$temp_10cm),
               soil_temp_30cm = as.numeric(df_in$temp_30cm))

  return(df)
}

################################################################################
# Processing
################################################################################

# combines two or more dataframes that are in hourly time gaps
combine_weather_hourly <- function(w_list) {

  df <- bind_rows(w_list) %>%
    .[order(.$date), ] %>%
    # this fills in gaps in the timeseries with NA data
    make_date_gaps(.)

  return(df)
}

# combines dfs that are in the day timegap format
combine_weather_daily <- function(w_list) {

  df <- bind_rows(w_list) %>%
    .[order(.$date), ] %>%
    # this fills in gaps in the timeseries with NA data (for day data)
    make_date_gaps_days()

  return(df)
}

# combines modelled and collected data in a df ready for analysis
prep_plot <- function(dfmod, dfcol, site) {

  # selects columns that have the name of the site in them
  df_comb <- dfmod %>%
    dplyr:: select(grep("date", names(dfmod)), grep(site, names(dfmod))) %>%
    inner_join(., dfcol[ ,c('date', 'max_temp', 'min_temp', 'rain_sum')])
  colnames(df_comb) <- c('date', 'mod_tmax', 'mod_tmin', 'mod_rain', 'col_tmax',
                         'col_tmin', 'col_rain')
  return(df_comb)
}

# convert_daily <- function(df) {
#
#   df_daily_data <- df %>%
#     group_by(date(date)) %>%
#     summarise(max_temp = max(air_temp), #, na.rm=TRUE
#               min_temp = min(air_temp), #, na.rm=TRUE
#               rain_sum = sum(rain_mm)) #, na.rm=TRUE
#   colnames(df_daily_data)[1] <- 'date'
#
#   #changing NO_DATA to NA so it can be omitted in the calcualtions
#   # has to be converted to dataframe so chr col can be turned to logical....
#   df <- data.frame(df)
#   df[df$data_source == 'NO_DATA', 'data_source'] <- NA
#
#   df_daily_id <- df %>%
#     group_by(date(date)) %>%
#     dplyr::summarise(
#       site = dplyr::first(site),
#       data_source = dplyr::first(na.omit(data_source)))
#   colnames(df_daily_id)[1] <- 'date'
#
#   # changing the NA data source back to NO_DATA
#   df_daily_id[is.na(df_daily_id$data_source), 'data_source'] <- 'NO_DATA'
#
#   df_daily <- full_join(df_daily_id, df_daily_data)
#
#   # reenters the missing data on rainsum as NA instead of 0 (Na were ignored above)
#   #df_daily[df_daily$data_source == 'NO_DATA', 'rain_sum'] <- NA
#
#   return(df_daily)
# }

# converts from hourly to daily data
convert_daily2 <- function(df) {

  # there are sometimes missing data in the rain and not in the temp and
  # vice versa so these need to be done separately
  df_temp <- df %>%
    subset(!is.na(air_temp)) %>%
    group_by(date(date)) %>%
    summarise(max_temp = max(air_temp),
              min_temp = min(air_temp))

  df_rain <- df %>%
    subset(!is.na(rain_mm)) %>%
    group_by(date(date)) %>%
    summarise(rain_sum = sum(rain_mm))

  df_daily_data <- full_join(df_temp, df_rain)
  colnames(df_daily_data)[1] <- 'date'

  #changing NO_DATA to NA so it can be omitted in the calcualtions
  # has to be converted to dataframe so chr col can be turned to logical....
  df <- data.frame(df)
  df[df$data_source == 'NO_DATA', 'data_source'] <- NA

  # extracting the info on site and data course and filling gaps
  df_daily_id <- df %>%
    group_by(date(date)) %>%
    dplyr::summarise(
      site = dplyr::first(site),
      data_source = dplyr::first(na.omit(data_source)))
  colnames(df_daily_id)[1] <- 'date'

  # changing the NA data source back to NO_DATA
  df_daily_id[is.na(df_daily_id$data_source), 'data_source'] <- 'NO_DATA'

  df_daily <- full_join(df_daily_id, df_daily_data)

  return(df_daily)
}

################################################################################
# Backend
################################################################################

# this is the function that finds gaps in the data and fills it with NA
# it does it by finding the first and last day, mkaing a time series with
# even gas between them and then adding the data we have back to this
# complete time series
# consider using date formae rather than datetime (POSIXct) if you want
make_date_gaps <- function(df_in) {

  # some of the data is not on the hour, but with various minutes past the hour
  df_in[['date']] <- round(df_in[['date']], units="hours")

  df_in <- df_in %>%
    .[!is.na(.$date), ] %>%
    .[order(.$date), ] %>%
    distinct(date, .keep_all= TRUE)

  dt1 <- df_in[['date']][1]
  dt2 <- df_in[['date']][length(df_in[['date']])]
  dt_gap <- as.POSIXct('2002-05-03 02:00:00') - as.POSIXct('2002-05-03 01:00:00')
  ts <- seq.POSIXt(dt1, dt2, dt_gap) #'hour'
  df_time <- data.frame(date = ts)

  df <- full_join(df_time,df_in) %>%
    tibble(.) %>%
    .[!is.na(.$date), ] %>%
    fill(c(data_source, site))

  df[rowSums(is.na(df[,4:ncol(df)]))==(ncol(df)-3),][['data_source']] <- 'NO_DATA'

  return(df)
}

make_date_gaps_days <- function(df_in) {

  # there are sometimes extraneous hours added in by the winter summer time
  # clock change
  df_in[['date']] <- round_date(df_in[['date']], unit="days")

  df_in <- df_in %>%
    .[!is.na(.$date), ] %>%
    .[order(.$date), ] %>%
    distinct(date, .keep_all= TRUE)

  dt1 <- df_in[['date']][1] %>%
    as.POSIXct(., format = '%Y-%m-%d')
  dt2 <- df_in[['date']][length(df_in[['date']])] %>%
    as.POSIXct(., format = '%Y-%m-%d')
  # the DST day makes it the same time each day wihtout changing timzone every 6 months
  ts <- seq.POSIXt(dt1, dt2, 'DSTday', tz=Sys.timezone()) %>%
    # the round then takes off the hour if the initial time was +1 hour
    round(., units="days")
  df_time <- data.frame(date = ts)

  df <- full_join(df_time,df_in) %>%
    tibble(.) %>%
    .[!is.na(.$date), ] %>%
    fill(c(data_source, site)) %>%
    .[order(.$date), ] %>%
    distinct(date, .keep_all= TRUE)

  df[rowSums(is.na(df[,4:ncol(df)]))==(ncol(df)-3),][['data_source']] <- 'NO_DATA'

  return(df)
}
