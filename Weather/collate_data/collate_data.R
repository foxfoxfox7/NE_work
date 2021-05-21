library(forecast)
library(xts)
library(SimilarityMeasures)
library(TSdist)


setwd('C:/Users/kiera/Work/NE_work/Weather/collate_data/')
source('./collate_funcs.R')

################################################################################
# Ainsdale
################################################################################

data_ains <- '../Data/Ainsdale/'
file_n <- "Ainsdale QA.xlsx"
file_n2 <- "LTMN_AWS_Ainsdale_raw_data.xlsx"

df_ains <- from_raw(paste0(data_ains, file_n2)) %>%
  convert_daily2()

################################################################################
# Bure marshes
################################################################################

data_bure <- '../Data/BureMarshes/'

file_in <- 'LTMN_AWS_Bure_Marshes_raw_data.xlsx'
df1 <- from_raw(paste0(data_bure, file_in))

file_in <- 'Bure Marshes export 01 07 2020 to 31 12 2020.XLSX'
df2 <- from_camp2(paste0(data_bure, file_in), 'B01')

weather_list <- list(df1, df2)
df_bure <- combine_weather_hourly(weather_list) %>%
  convert_daily2()

################################################################################
# Burnham Beeches
################################################################################

data_bb <- '../Data/BurnhamBeeches/'
file_in <- 'LTMN_AWS_Burnham_Beeches_raw_data.xlsx'

df_bb <- from_raw(paste0(data_bb, file_in)) %>%
  convert_daily2()

df_bb[(!is.na(df_bb$rain_sum)) & (df_bb$rain_sum > 1000), 'rain_sum'] <- NA

################################################################################
# Cross Fell
################################################################################

data_cross <- '../Data/CrossFell/'
data_ceh <- 'CEH/'

cf1 <- 'moorh for NE.CSV'
df_cross1 <- from_cosmos_collate(paste0(data_cross, cf1), 'B50') %>%
  convert_daily2()

cf2 <- 'MOORH_MAY_2017.xlsx'
df_cross2 <- from_cosmos_month_excel(paste0(data_cross, cf2), 'B50')

cf3 <- 'MOORH_JUN_2017.csv'
df_cross3 <- from_cosmos_month_csv3(paste0(data_cross, cf3), 'B50', date_f = '%d/%m/%Y')

cross_list1 <- list(df_cross1, df_cross2, df_cross3)


# Getting all the data from the months of CEH csv files
list_of_files <- list.files(paste0(data_cross, data_ceh))

list_cross = list()
for (ii in 1:length(list_of_files)) {
  
  file_path <- paste0(data_cross, data_ceh, list_of_files[ii])
  df <- from_cosmos_month_csv3(file_path, 'B50')
  list_cross[[ii]] <- df
}

# combining the various months with the rest of the data we have
df_cross <- combine_weather_daily(c(cross_list1, list_cross))
df_cross[['date']] <- as.Date(df_cross[['date']])

################################################################################
# Ingleborough
################################################################################

data_ing <- '../Data/Ingleborough2/'
camp <- 'camp2/'

ing1 <- 'LTMN_AWS_ Data_Ingleborough 05-14.XLSX'
df_ing1 <- from_aws(paste0(data_ing, ing1)) %>%
  convert_daily2()

ing2 <- 'MO AWS to Aug 2016 - processed weather data 03-15 08-16.CSV'
df_ing2 <- from_aws_processed2(paste0(data_ing, ing2), 'B12') %>%
  convert_daily2()

individual_list_ing <- list(df_ing1, df_ing2)


list_of_files <- list.files(paste0(data_ing, camp))
list_ing = list()
for (ii in 1:length(list_of_files)) {
  
  file_path <- paste0(data_ing, camp, list_of_files[ii])
  df <- from_camp2(file_path, 'B12') %>%
    convert_daily2()
  list_ing[[ii]] <- df
}

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


list_of_files <- list.files(paste0(data_liz, cosmos, cos_csv))
list_liz = list()
for (ii in 1:length(list_of_files)) {
  
  file_path <- paste0(data_liz, cosmos, cos_csv, list_of_files[ii])
  df <- from_cosmos_month_csv3(file_path, 'B12')#
  list_liz[[ii]] <- df
}

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

df_ains_rep <- get_rep_site('B01')
df_burnham_rep <- get_rep_site('B03')
df_cross_rep <- get_rep_site('B50')

################################################################################
# Modelled data
################################################################################

data_modelled <- '../Data/modelled/Met Office HADUK LTMN Daily data 09 2019/'

daily_maxt <- 'daily_maxtemp_natural_england.csv'
df_tmax <- from_mod_collate(paste0(data_modelled, daily_maxt), '_tmax')

daily_mint <- 'daily_mintemp_natural_england.csv'
df_tmin <- from_mod_collate(paste0(data_modelled, daily_mint), '_tmin')

daily_rain <- 'daily_rainfall_natural_england.csv'
df_rain <- from_mod_collate(paste0(data_modelled, daily_rain), '_rain')

df_modelled <- full_join(df_tmax, df_tmin) %>%
  full_join(., df_rain)

################################################################################
# combine
################################################################################

print(colnames(df_modelled)[grep('rain', colnames(df_modelled))])

df_comb <- prep_plot(df_modelled, df_ains, 'ainsdale')
df_comb <- prep_plot(df_modelled, df_ains_rep, 'ainsdale')
df_comb <- prep_plot(df_modelled, df_bure, 'bure.marshes')
df_comb <- prep_plot(df_modelled, df_bb, 'burnham.beeches')
df_comb <- prep_plot(df_modelled, df_burnham_rep, 'burnham.beeches')
df_comb <- prep_plot(df_modelled, df_cross, 'cross.fell')
df_comb <- prep_plot(df_modelled, df_cross_rep, 'cross.fell')
df_comb <- prep_plot(df_modelled, df_ing, 'ingleborough')
df_comb <- prep_plot(df_modelled, df_liz, 'the.lizard')
df_comb <- prep_plot(df_modelled, df_liz_cos, 'the.lizard')

df_comb$col_mod_tmax <- ifelse(is.na(df_comb$col_tmax), df_comb$mod_tmax, df_comb$col_tmax)
df_comb$col_mod_tmin <- ifelse(is.na(df_comb$col_tmin), df_comb$mod_tmin, df_comb$col_tmin)

df_comb$col_mod_rain <- ifelse(is.na(df_comb$col_rain), df_comb$mod_rain, df_comb$col_rain)
df_comb$cum_mod_rain <- cumsum(df_comb[['mod_rain']])
df_comb$cum_col_rain <- cumsum(df_comb[['col_mod_rain']])

df_comb <- df_comb %>%
  dplyr::mutate(m_tmin_r = zoo::rollapply(mod_tmin, 7, mean, na.rm = TRUE, fill = NA),
                c_time_r = zoo::rollapply(col_tmin, 7, mean, na.rm = TRUE, fill = NA),
                m_tmax_r = zoo::rollapply(mod_tmax, 7, mean, na.rm = TRUE, fill = NA),
                c_tmax_r = zoo::rollapply(col_tmax, 7, mean, na.rm = TRUE, fill = NA),
                cm_tmax_r = zoo::rollapply(col_mod_tmax, 7, mean, na.rm = TRUE, fill = NA),
                cm_tmin_r = zoo::rollapply(col_mod_tmin, 7, mean, na.rm = TRUE, fill = NA))

df_miss_rain <- df_comb %>%
  mutate(col_rain = ifelse(is.na(col_rain), 69, col_rain)) %>%
  group_by(group = cumsum(c(0, diff(col_rain) != 0))) %>%
  filter(col_rain == 69 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

df_miss_temp <- df_comb %>%
  mutate(col_tmax = ifelse(is.na(col_tmax), 69, col_tmax)) %>%
  group_by(group = cumsum(c(0, diff(col_tmax) != 0))) %>%
  filter(col_tmax == 69 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

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

df_m_rain_dec <- decompose_to_tib('cum_mod_rain')
df_c_rain_dec <- decompose_to_tib('cum_col_rain')
df_m_tmax_dec <- decompose_to_tib('m_tmax_r')
df_c_tmax_dec <- decompose_to_tib('cm_tmax_r')

################################################################################
# visualise
################################################################################


ggplot(df_comb) +
  geom_line(aes(x=date, y=m_tmax_r, color='blue')) +
  geom_line(aes(x=date, y=c_tmax_r, color='red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Temp max comparison') + ylab('Temperature')

ggplot(df_m_tmax_dec) +
  geom_line(aes(x = date, y=trend, color = 'blue')) +
  geom_line(aes(x = date, y=total, color = 'red')) +
  scale_color_discrete(name = "Datasets", labels = c("trend", "data")) +
  ggtitle('Modelled temp max trend') + ylab('Temperature')

ggplot(df_c_tmax_dec) +
  geom_line(aes(x = date, y=trend, color = 'blue')) +
  geom_line(aes(x = date, y=total, color = 'red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("trend", "data")) +
  ggtitle('Collected temp max trend') + ylab('Temperature')

ggplot(NULL) +
  geom_line(data=df_m_tmax_dec, aes(x = date, y=seasonal, color = 'blue')) +
  geom_line(data=df_c_tmax_dec, aes(x = date, y=seasonal, color = 'red')) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of temp max seasonality') + ylab('Temperature')

ggplot(NULL) +
  geom_line(data=df_m_tmax_dec, aes(x = date, y=trend, color = 'blue')) +
  geom_line(data=df_c_tmax_dec, aes(x = date, y=trend, color = 'red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of temp max trend') + ylab('Temperature')

ggplot(NULL) +
  geom_line(data=df_m_tmax_dec, aes(x = date, y=fluctuations, color = 'blue')) +
  geom_line(data=df_c_tmax_dec, aes(x = date, y=fluctuations, color = 'red')) +
  geom_rect(data = df_miss_temp, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of temp max fluctuations') + ylab('Temperature')






ggplot(df_comb) +
  geom_line(aes(x=date, y=cum_mod_rain, color='blue')) +
  geom_line(aes(x=date, y=cum_col_rain, color='red')) +
  geom_rect(data = df_miss_rain, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of summed rainfall') + ylab('rain')

ggplot(NULL) +
  geom_line(data=df_m_rain_dec, aes(x = date, y=seasonal, color = 'blue')) +
  geom_line(data=df_c_rain_dec, aes(x = date, y=seasonal, color = 'red')) +
  geom_rect(data=df_miss_rain, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of rainfall seasonality') + ylab('rain')

init_m_rain <- df_m_rain_dec$trend[!is.na(df_m_rain_dec$trend)][1]
init_c_rain <- df_c_rain_dec$trend[!is.na(df_c_rain_dec$trend)][1]

ggplot(NULL) +
  geom_line(data=df_m_rain_dec, aes(x = date, y=trend - init_m_rain, color = 'blue')) +
  geom_line(data=df_c_rain_dec, aes(x = date, y=trend - init_c_rain, color = 'red')) +
  geom_rect(data = df_miss_rain, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected")) +
  ggtitle('Comparison of rainfall trend') + ylab('rain')

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

length(df_comb$date)

df_rain <- df_comb[!is.na(df_comb$col_rain), c('date', 'mod_rain', 'col_rain')]
length(df_rain$mod_rain)

sum(df_rain$mod_rain > 10)
sum(df_rain$col_rain > 10)

rain_norm_f <- max(c(df_rain$mod_rain, df_rain$col_rain))
df_rain$mod_norm <- df_rain$mod_rain/rain_norm_f
df_rain$col_norm <- df_rain$col_rain/rain_norm_f
DTWDistance(df_rain$mod_norm, df_rain$col_norm)

df_m_dry <- df_rain %>%
  mutate(mod_rain = ifelse(mod_rain < 1, 69, mod_rain)) %>%
  group_by(group = cumsum(c(0, diff(mod_rain) != 0))) %>%
  filter(mod_rain == 69 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

df_c_dry <- df_rain %>%
  mutate(col_rain = ifelse(col_rain < 1, 69, col_rain)) %>%
  group_by(group = cumsum(c(0, diff(col_rain) != 0))) %>%
  filter(col_rain == 69 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))

# df_rain2 <- df_comb[!is.na(df_comb$col_rain), c('date', 'cum_mod_rain', 'cum_col_rain')]
# rain_norm_f <- max(c(df_rain2$cum_mod_rain, df_rain2$cum_col_rain))
# rain_norm_f
# DTWDistance(df_rain2$cum_mod_rain, df_rain2$cum_col_rain)


df_tmax <- df_comb[!is.na(df_comb$col_tmax), c('date', 'mod_tmax', 'col_tmax')]
df_tmin <- df_comb[!is.na(df_comb$col_tmin), c('date', 'mod_tmin', 'col_tmin')]
length(df_tmax$mod_tmax)

temp_norm_f <- max(c(df_tmax$mod_tmax, df_tmax$col_tmax))
df_tmax$mod_norm <- df_tmax$mod_tmax/temp_norm_f
df_tmax$col_norm <- df_tmax$col_tmax/temp_norm_f
DTWDistance(df_tmax$mod_norm, df_tmax$col_norm)

top_thresh <- 0.8*max(c(df_tmax$mod_tmax, df_tmax$col_tmax))
top_thresh
sum(df_tmax$mod_tmax > top_thresh)
sum(df_tmax$col_tmax > top_thresh)

sum(df_tmin$mod_tmin <= 0)
sum(df_tmin$col_tmin <= 0)

################################################################################
# scrap
################################################################################

# df_in$date[grep("[0-9]{2}-[A-Za-z]{3}-[0-9]{4}$",df_in$date)] <- paste(
#   df_in$date[grep("[0-9]{2}-[A-Za-z]{3}-[0-9]{4}$",df_in$date)],"00:00:00")

# df_in[['date']] <- as.POSIXct(df_in[['date']],
#                               format="%d-%b-%Y %H:%M:%S",
#                               tz=Sys.timezone())

# add_time <- function(input) {
#   print(input)
#   
#   if (!grepl('[ ]', input)) {
#     return(paste0(input, ' 00:00'))
#   } else {
#     return(input)
#   }
# }

# df3$OB.TIME2 <- map_chr(df3$OB.TIME, add_time)
# df3$OB.TIME3 <- gsub('[/+]', '-', df3$OB.TIME2)