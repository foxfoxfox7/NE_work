library(forecast)
library(xts)
library(TSdist)


setwd('C:/Users/x955120/NE_work/Weather/collate_data/')
# Has all the functions used to collate the data
source('./collate_funcs.R')

################################################################################
# Ainsdale
################################################################################

data_ains <- '../Data/Ainsdale/'
file_n <- "Ainsdale QA.xlsx"
file_n2 <- "LTMN_AWS_Ainsdale_raw_data.xlsx"

# My functio for getting data that has been preprocessed into a 'raw' format
df_ains <- from_raw(paste0(data_ains, file_n2)) %>%
  # converts the hourly data to daily with max temp, min temp and sum rain
  convert_daily2()

################################################################################
# Bure marshes
################################################################################

data_bure <- '../Data/BureMarshes/'

file_in <- 'LTMN_AWS_Bure_Marshes_raw_data.xlsx'
df1 <- from_raw(paste0(data_bure, file_in))

file_in <- 'Bure Marshes export 01 07 2020 to 31 12 2020.XLSX'
# From the cambells contract data source
df2 <- from_camp2(paste0(data_bure, file_in), 'B01')

weather_list <- list(df1, df2)
# combining two or more weather dfs of different time periods
# it will fill missing time period with NA data
df_bure <- combine_weather_hourly(weather_list) %>%
  convert_daily2()

################################################################################
# Burnham Beeches
################################################################################

data_bb <- '../Data/BurnhamBeeches/'
file_in <- 'LTMN_AWS_Burnham_Beeches_raw_data.xlsx'

df_bb <- from_raw(paste0(data_bb, file_in)) %>%
  convert_daily2()

# There were two days with crazy rainfall which are clearly errors
# There is a QA code which i did not use as I didn't have time and i dont think
# it will have a major effect on the overall trnds but it might be worth a look
df_bb[(!is.na(df_bb$rain_sum)) & (df_bb$rain_sum > 1000), 'rain_sum'] <- NA

################################################################################
# Cross Fell
################################################################################

data_cross <- '../Data/CrossFell/'
data_ceh <- 'CEH/'

cf1 <- 'moorh for NE.CSV'
# Someone has collated a bunch of the cosmos  data into this format
# this a group collecting weather data nearby, we dont have a contract with
# them but they give us their data
df_cross1 <- from_cosmos_collate(paste0(data_cross, cf1), 'B50') %>%
  convert_daily2()

cf2 <- 'MOORH_MAY_2017.xlsx'
# the cosmos data used to come in excel form and now comes in csv
# there are some differences in the excel sheets from csv so each function needed
df_cross2 <- from_cosmos_month_excel(paste0(data_cross, cf2), 'B50')

cf3 <- 'MOORH_JUN_2017.csv'
# when they converted to csv format, there were a few montsh where there were
# a few differences in the files
df_cross3 <- from_cosmos_month_csv3(paste0(data_cross, cf3), 'B50', date_f = '%d/%m/%Y')

# making a list of the ones we have clculated above, ready to be added below
cross_list1 <- list(df_cross1, df_cross2, df_cross3)


# Getting all the data from the months of CEH csv files
list_of_files <- list.files(paste0(data_cross, data_ceh))

# loops through each file and extracts the information
list_cross = list()
for (ii in 1:length(list_of_files)) {

  file_path <- paste0(data_cross, data_ceh, list_of_files[ii])
  df <- from_cosmos_month_csv3(file_path, 'B50')
  # adds the df to a list ready to be combined
  list_cross[[ii]] <- df
}

# combining the various months with the rest of the data we have
df_cross <- combine_weather_daily(c(cross_list1, list_cross))
# there are some complications with the date being in datetime formate with
# the change in time zone each 6 months adding an hour. so i convert to date
df_cross[['date']] <- as.Date(df_cross[['date']])

################################################################################
# Ingleborough
################################################################################

data_ing <- '../Data/Ingleborough2/'
camp <- 'camp2/'

ing1 <- 'LTMN_AWS_ Data_Ingleborough 05-14.XLSX'
# aws is another contract and the data is in its own format
df_ing1 <- from_aws(paste0(data_ing, ing1)) %>%
  convert_daily2()

ing2 <- 'MO AWS to Aug 2016 - processed weather data 03-15 08-16.CSV'
# some of the aws data has been preprocesed and is in a different format
df_ing2 <- from_aws_processed2(paste0(data_ing, ing2), 'B12') %>%
  convert_daily2()

individual_list_ing <- list(df_ing1, df_ing2)

# same again, looking through all the files in the folder, looping through them
# extracting the information and adding them to a list
list_of_files <- list.files(paste0(data_ing, camp))
list_ing = list()
for (ii in 1:length(list_of_files)) {

  file_path <- paste0(data_ing, camp, list_of_files[ii])
  df <- from_camp2(file_path, 'B12') %>%
    convert_daily2()
  list_ing[[ii]] <- df
}

# combining them all into one dataframe with NA where there is missing info
df_ing <- combine_weather_daily(c(individual_list_ing, list_ing))

################################################################################
# The Lizard
################################################################################

data_liz <- '../Data/Lizard/'

liz1 <- 'LIZ_MO_AWS_version1_and_version0_combined.xlsx'
df_liz1 <- from_aws_processed2(paste0(data_liz, liz1), 'B40', excel=TRUE) %>%
  convert_daily2()

liz2 <- 'LTMN_AWS_The_Lizard_raw_data.xlsx'
df_liz2 <- from_raw(paste0(data_liz, liz2)) %>%
  convert_daily2()

df_liz <- combine_weather_daily(list(df_liz1, df_liz2))

# COSMOS RPERESNTATIVE!!!!!!!!!!!
# this a group collecting weather data nearby, we dont have a contract with
# them but they give us their data
# for this site we have our own data and cosmos data
sitecode <- 'B40'
cosmos <- 'COSMOS/'
cos_csv <- 'csv/'
cos_ex <- 'excel/'

liz_c1 <- 'COSMOS lizrd for NE.CSV'
df_liz_c1 <- from_cosmos_collate(paste0(data_liz, cosmos, liz_c1), sitecode = 'B40') %>%
  convert_daily2()

liz_c2 <- 'LIZRD_JUN_2017.csv'
df_liz_c2 <- from_cosmos_month_csv3(paste0(data_liz, cosmos, liz_c2), 'B40', date_f = '%d/%m/%Y')

ind_files_liz <- list(df_liz_c1, df_liz_c2)

# here we have two loops, through the csv files in one folder
list_of_files <- list.files(paste0(data_liz, cosmos, cos_csv))
list_liz = list()
for (ii in 1:length(list_of_files)) {

  file_path <- paste0(data_liz, cosmos, cos_csv, list_of_files[ii])
  df <- from_cosmos_month_csv3(file_path, 'B12')#
  list_liz[[ii]] <- df
}

# and through the excel files in another folder
list_of_files2 <- list.files(paste0(data_liz, cosmos, cos_ex))
list_liz22 = list()
for (ii in 1:length(list_of_files2)) {

  file_path <- paste0(data_liz, cosmos, cos_ex, list_of_files2[ii])
  df <- from_cosmos_month_excel(file_path, 'B12')
  list_liz22[[ii]] <- df
}

df_liz_cos <- combine_weather_daily(c(ind_files_liz, list_liz, list_liz22))

################################################################################
# Representative data
################################################################################

data_rep <- '../Data/Representative/'
rep_csv <- 'csv/'
rep_ex <- 'excel/'

# The representative data is a MET office weather station near to our site

# the data is either in csv or excel. this time there is data for each rep
# site in each file so they all need to be processed. there are representative
# sites for only a few of the sites we study. this function looks through
# all the data and and keeps the data for a particular repreentative site
get_rep_site <- function(sitecode) {

  list_files_rep_csv <- list.files(paste0(data_rep, rep_csv))
  list_rep_csv = list()
  for (ii in 1:length(list_files_rep_csv)) {

    file_path <- paste0(data_rep, rep_csv, list_files_rep_csv[ii])
    print(file_path)
    df <- from_rep(file_path, sitecode)
    list_rep_csv[[ii]] <- df
  }

  list_files_rep_ex <- list.files(paste0(data_rep, rep_ex))
  list_rep_ex = list()
  for (ii in 1:length(list_files_rep_ex)) {

    file_path <- paste0(data_rep, rep_ex, list_files_rep_ex[ii])
    print(file_path)
    df <- from_rep(file_path, sitecode, excel=TRUE)
    list_rep_ex[[ii]] <- df
  }

  df_representative <- combine_weather_hourly(c(list_rep_ex, list_rep_csv)) %>%
    convert_daily2()

  return(df_representative)
}

# I choose the rep site using these codes. we have a code for each site
# these are the function above, the list of codes and what rep site they
# refer to in the functions file
df_ains_rep <- get_rep_site('B01')
df_burnham_rep <- get_rep_site('B03')
df_cross_rep <- get_rep_site('B50')

################################################################################
# Modelled data
################################################################################

# The data here is only max temp, min temp and rain sum (hence why I have
# chosen those metrics for our data). this is modelled data by the met
# office

data_modelled <- '../Data/modelled/Met Office HADUK LTMN Daily data 09 2019/'

daily_maxt <- 'daily_maxtemp_natural_england.csv'
# refers to a function in collate functions, the string at the end is added
# to the column names so they can all be combined
df_tmax <- from_mod_collate(paste0(data_modelled, daily_maxt), '_tmax')

daily_mint <- 'daily_mintemp_natural_england.csv'
df_tmin <- from_mod_collate(paste0(data_modelled, daily_mint), '_tmin')

daily_rain <- 'daily_rainfall_natural_england.csv'
df_rain <- from_mod_collate(paste0(data_modelled, daily_rain), '_rain')

# jons the three of them up
df_modelled <- full_join(df_tmax, df_tmin) %>%
  full_join(., df_rain)

################################################################################
# combine
################################################################################

print(colnames(df_modelled)[grep('rain', colnames(df_modelled))])

# this is a function in collate functions for combining the modelled data and
# the collected data in one data frame ready for analysis
df_comb <- prep_plot(df_modelled, df_ains, 'ainsdale')
# df_comb <- prep_plot(df_modelled, df_ains_rep, 'ainsdale')
# df_comb <- prep_plot(df_modelled, df_bure, 'bure.marshes')
# df_comb <- prep_plot(df_modelled, df_bb, 'burnham.beeches')
# df_comb <- prep_plot(df_modelled, df_burnham_rep, 'burnham.beeches')
# df_comb <- prep_plot(df_modelled, df_cross, 'cross.fell')
# df_comb <- prep_plot(df_modelled, df_cross_rep, 'cross.fell')
# df_comb <- prep_plot(df_modelled, df_ing, 'ingleborough')
# df_comb <- prep_plot(df_modelled, df_liz, 'the.lizard')
# df_comb <- prep_plot(df_modelled, df_liz_cos, 'the.lizard')

# creates new columns that use the colected data unless NA, then uses modelled
df_comb$col_mod_tmax <- ifelse(is.na(df_comb$col_tmax), df_comb$mod_tmax, df_comb$col_tmax)
df_comb$col_mod_tmin <- ifelse(is.na(df_comb$col_tmin), df_comb$mod_tmin, df_comb$col_tmin)

df_comb$col_mod_rain <- ifelse(is.na(df_comb$col_rain), df_comb$mod_rain, df_comb$col_rain)
# creating columns for the cumulative rainfall
df_comb$cum_mod_rain <- cumsum(df_comb[['mod_rain']])
df_comb$cum_col_rain <- cumsum(df_comb[['col_mod_rain']])

# creating rolling means for the temperatures
df_comb <- df_comb %>%
  dplyr::mutate(m_tmin_r = zoo::rollapply(mod_tmin, 7, mean, na.rm = TRUE, fill = NA),
                c_time_r = zoo::rollapply(col_tmin, 7, mean, na.rm = TRUE, fill = NA),
                m_tmax_r = zoo::rollapply(mod_tmax, 7, mean, na.rm = TRUE, fill = NA),
                c_tmax_r = zoo::rollapply(col_tmax, 7, mean, na.rm = TRUE, fill = NA),
                cm_tmax_r = zoo::rollapply(col_mod_tmax, 7, mean, na.rm = TRUE, fill = NA),
                cm_tmin_r = zoo::rollapply(col_mod_tmin, 7, mean, na.rm = TRUE, fill = NA))

# finds longest streaks of NA rainfall. converts NA to 700, counts streaks  over
# 10 of  700
# this is used later on for blocking those patches out of the graphs
df_miss_rain <- df_comb %>%
  mutate(col_rain = ifelse(is.na(col_rain), 700, col_rain)) %>%
  group_by(group = cumsum(c(0, diff(col_rain) != 0))) %>%
  filter(col_rain == 700 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

# does the same for the temperature
df_miss_temp <- df_comb %>%
  mutate(col_tmax = ifelse(is.na(col_tmax), 700, col_tmax)) %>%
  group_by(group = cumsum(c(0, diff(col_tmax) != 0))) %>%
  filter(col_tmax == 700 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

# this is the function that decomposes the data into seasonal, trend and
# fluctuations
decompose_to_tib <- function(col) {

  Y <- msts(df_comb[[col]], seasonal.periods=365.25)
  dec <- decompose(Y)

  df_m_tmax_dec <- tibble(date = df_comb$date,
                          total = dec[['x']],
                          seasonal = dec[['seasonal']],
                          trend = dec[['trend']],
                          fluctuations = dec[['random']])
  return(df_m_tmax_dec)
}

# using the decompose function to get some dfs ready for below
df_m_rain_dec <- decompose_to_tib('cum_mod_rain')
df_c_rain_dec <- decompose_to_tib('cum_col_rain')
df_m_tmax_dec <- decompose_to_tib('m_tmax_r')
df_c_tmax_dec <- decompose_to_tib('cm_tmax_r')

################################################################################
# visualise
################################################################################

# plots the collected vs modeled temperature
ggplot(df_comb) +
  geom_line(aes(x=date, y=m_tmax_r, color='blue')) +
  geom_line(aes(x=date, y=c_tmax_r, color='red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Temp max comparison') + ylab('Temperature')

# this is the modelled temperature with the trend overlaid
ggplot(df_m_tmax_dec) +
  geom_line(aes(x = date, y=trend, color = 'blue')) +
  geom_line(aes(x = date, y=total, color = 'red')) +
  scale_color_discrete(name = "Datasets", labels = c("trend", "data")) +
  ggtitle('Modelled temp max trend') + ylab('Temperature')

# this is the collected temperature with the trend overlaid
ggplot(df_c_tmax_dec) +
  geom_line(aes(x = date, y=trend, color = 'blue')) +
  geom_line(aes(x = date, y=total, color = 'red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("trend", "data")) +
  ggtitle('Collected temp max trend') + ylab('Temperature')

# comparing the seasonal pattern isolated from the modelled and collected temp
ggplot(NULL) +
  geom_line(data=df_m_tmax_dec, aes(x = date, y=seasonal, color = 'blue')) +
  geom_line(data=df_c_tmax_dec, aes(x = date, y=seasonal, color = 'red')) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of temp max seasonality') + ylab('Temperature')

# comparing the overall trend from the modelled and collected temp
ggplot(NULL) +
  geom_line(data=df_m_tmax_dec, aes(x = date, y=trend, color = 'blue')) +
  geom_line(data=df_c_tmax_dec, aes(x = date, y=trend, color = 'red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of temp max trend') + ylab('Temperature')

# comparing the fluctuations in the modelled and collected temp
ggplot(NULL) +
  geom_line(data=df_m_tmax_dec, aes(x = date, y=fluctuations, color = 'blue')) +
  geom_line(data=df_c_tmax_dec, aes(x = date, y=fluctuations, color = 'red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of temp max fluctuations') + ylab('Temperature')





# comparing the summed rainfall for collected and modelled
ggplot(df_comb) +
  geom_line(aes(x=date, y=cum_mod_rain, color='blue')) +
  geom_line(aes(x=date, y=cum_col_rain, color='red')) +
  geom_rect(data = df_miss_rain, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of summed rainfall') + ylab('rain')

# comparing the seasonal pattern isolated from the modelled and collected rain
ggplot(NULL) +
  geom_line(data=df_m_rain_dec, aes(x = date, y=seasonal, color = 'blue')) +
  geom_line(data=df_c_rain_dec, aes(x = date, y=seasonal, color = 'red')) +
  geom_rect(data=df_miss_rain, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of rainfall seasonality') + ylab('rain')

# calculating the initial non NA position of the trends
# these will be removed from the data to compare from a 0 start point
init_m_rain <- df_m_rain_dec$trend[!is.na(df_m_rain_dec$trend)][1]
init_c_rain <- df_c_rain_dec$trend[!is.na(df_c_rain_dec$trend)][1]

# comparing the trend from the modelled and collected rain
ggplot(NULL) +
  geom_line(data=df_m_rain_dec, aes(x = date, y=trend - init_m_rain, color = 'blue')) +
  geom_line(data=df_c_rain_dec, aes(x = date, y=trend - init_c_rain, color = 'red')) +
  geom_rect(data = df_miss_rain, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of rainfall trend') + ylab('rain')

# comparing the fluctuations in the modelled and collected rain
ggplot(NULL) +
  geom_line(data=df_m_rain_dec, aes(x = date, y=fluctuations, color = 'blue')) +
  geom_line(data=df_c_rain_dec, aes(x = date, y=fluctuations, color = 'red')) +
  geom_rect(data = df_miss_rain, ymin = -Inf, ymax = +Inf,
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of rainfall fluctuations') + ylab('rain')

################################################################################
# quantifying
################################################################################

# the number of total days form the first entry to the last
length(df_comb$date)

# the non NA data
df_rain <- df_comb[!is.na(df_comb$col_rain), c('date', 'mod_rain', 'col_rain')]
# now the number of days that have non NA data
length(df_rain$mod_rain)

# number of days above 10 mm rainfall
sum(df_rain$mod_rain > 10)
sum(df_rain$col_rain > 10)

# thi si the dynamic time warping algorthm. doesn't work very well for rain
# i think anywyay, there should be some better metrics
DTWDistance(df_rain$mod_rain, df_rain$col_rain)

# counting days of less than 1 mm rainfall by converting them to 700
# and then counting runs of 700
df_m_dry <- df_rain %>%
  mutate(mod_rain = ifelse(mod_rain < 1, 700, mod_rain)) %>%
  group_by(group = cumsum(c(0, diff(mod_rain) != 0))) %>%
  filter(mod_rain == 700 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

# same with the collected data
df_c_dry <- df_rain %>%
  mutate(col_rain = ifelse(col_rain < 1, 700, col_rain)) %>%
  group_by(group = cumsum(c(0, diff(col_rain) != 0))) %>%
  filter(col_rain == 700 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

# This is an attempt at using the DTW for cumulative rainfall. see what you
# think
# df_rain2 <- df_comb[!is.na(df_comb$col_rain), c('date', 'cum_mod_rain', 'cum_col_rain')]
# rain_norm_f <- max(c(df_rain2$cum_mod_rain, df_rain2$cum_col_rain))
# rain_norm_f
# DTWDistance(df_rain2$cum_mod_rain, df_rain2$cum_col_rain)

# days of non NA temperature data
df_tmax <- df_comb[!is.na(df_comb$col_tmax), c('date', 'mod_tmax', 'col_tmax')]
df_tmin <- df_comb[!is.na(df_comb$col_tmin), c('date', 'mod_tmin', 'col_tmin')]
# the number of days that has data
length(df_tmax$mod_tmax)

# normalising the temperature data
temp_norm_f <- max(c(df_tmax$mod_tmax, df_tmax$col_tmax))
df_tmax$mod_norm <- df_tmax$mod_tmax/temp_norm_f
df_tmax$col_norm <- df_tmax$col_tmax/temp_norm_f
# calculating the DTW. this seems to work for me
DTWDistance(df_tmax$mod_norm, df_tmax$col_norm)

# 80% of the top temperature in modelled and collected
top_thresh <- 0.8*max(c(df_tmax$mod_tmax, df_tmax$col_tmax))
top_thresh
# days abover that temperature
sum(df_tmax$mod_tmax > top_thresh)
sum(df_tmax$col_tmax > top_thresh)

# days where the temp went down to 0
sum(df_tmin$mod_tmin <= 0)
sum(df_tmin$col_tmin <= 0)
