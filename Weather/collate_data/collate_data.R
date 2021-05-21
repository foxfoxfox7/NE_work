
setwd('C:/Users/kiera/Work/NE_work/Weather/collate_data/')
source('./collate_funcs.R')



################################################################################
# Ainsdale
################################################################################

data_ains <- '../Data/Ainsdale/'
file_n <- "Ainsdale QA.xlsx"
file_n2 <- "LTMN_AWS_Ainsdale_raw_data.xlsx"

df_ains <- from_raw(paste0(data_ains, file_n2)) %>%
  convert_daily()

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
  convert_daily()

################################################################################
# Burnham Beeches
################################################################################

data_bb <- '../Data/BurnhamBeeches/'
file_in <- 'LTMN_AWS_Burnham_Beeches_raw_data.xlsx'

df_bb <- from_raw(paste0(data_bb, file_in)) %>%
  convert_daily2()

df_bb[(!is.na(df_bb$rain_sum)) & (df_bb$rain_sum > 1000), 'rain_sum'] <- NA


convert_daily2 <- function(df) {
  
  #DO EACH ONE INDIVIDUALLY
  # REMOVE THE NA FIRST
  # THEN COMBINE
  
  df_temp <- df %>%
    subset(!is.na(air_temp)) %>%
    group_by(date(date)) %>% 
    summarise(max_temp = max(air_temp),
              min_temp = min(air_temp))
  
  df_rain <- df %>%
    subset(!is.na(rain_sum)) %>%
    group_by(date(date)) %>% 
    summarise(rain_sum = sum(rain_mm))
  
  df_daily_data <- full_join(df_temp, df_rain)
  colnames(df_daily_data)[1] <- 'date'
  
  
  # df_daily_data[df_daily_data$max_temp == -Inf, 2:ncol(df_daily_data)] <- NA
  # df_daily_data[df_daily_data$max_temp == -Inf, c('max_temp', 'min_temp')] <- NA
  
  #changing NO_DATA to NA so it can be omitted in the calcualtions
  # has to be converted to dataframe so chr col can be turned to logical....
  df <- data.frame(df)
  df[df$data_source == 'NO_DATA', 'data_source'] <- NA
  
  df_daily_id <- df %>% 
    group_by(date(date)) %>% 
    dplyr::summarise(
      site = dplyr::first(site),
      data_source = dplyr::first(na.omit(data_source)))
  colnames(df_daily_id)[1] <- 'date'
  
  # changing the NA data source back to NO_DATA
  df_daily_id[is.na(df_daily_id$data_source), 'data_source'] <- 'NO_DATA'
  
  df_daily <- full_join(df_daily_id, df_daily_data)
  
  # reenters the missing data on rainsum as NA instead of 0 (Na were ignored above)
  #df_daily[df_daily$data_source == 'NO_DATA', 'rain_sum'] <- NA
  
  return(df_daily)
}


################################################################################
# Cross Fell
################################################################################

data_cross <- '../Data/CrossFell/'
data_ceh <- 'CEH/'

cf1 <- 'moorh for NE.CSV'
df_cross1 <- from_cosmos_collate(paste0(data_cross, cf1), 'B50') %>%
  convert_daily()

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

################################################################################
# Ingleborough
################################################################################

data_ing <- '../Data/Ingleborough2/'
camp <- 'camp2/'

ing1 <- 'LTMN_AWS_ Data_Ingleborough 05-14.XLSX'
df_ing1 <- from_aws(paste0(data_ing, ing1)) %>%
  convert_daily()

ing2 <- 'MO AWS to Aug 2016 - processed weather data 03-15 08-16.CSV'
df_ing2 <- from_aws_processed2(paste0(data_ing, ing2), 'B12') %>%
  convert_daily()

individual_list_ing <- list(df_ing1, df_ing2)


list_of_files <- list.files(paste0(data_ing, camp))
list_ing = list()
for (ii in 1:length(list_of_files)) {
  
  file_path <- paste0(data_ing, camp, list_of_files[ii])
  df <- from_camp2(file_path, 'B12') %>%
    convert_daily()
  list_ing[[ii]] <- df
}

df_ing <- combine_weather_daily(c(individual_list_ing, list_ing))

################################################################################
# The Lizard
################################################################################

data_liz <- '../Data/Lizard/'

liz1 <- 'LIZ_MO_AWS_version1_and_version0_combined.xlsx'
df_liz1 <- from_aws_processed2(paste0(data_liz, liz1), 'B40', excel=TRUE) %>%
  convert_daily()

liz2 <- 'LTMN_AWS_The_Lizard_raw_data.xlsx'
df_liz2 <- from_raw(paste0(data_liz, liz2)) %>%
  convert_daily()

df_liz <- combine_weather_daily(list(df_liz1, df_liz2))

# COSMOS RPERESNTATIVE!!!!!!!!!!!
sitecode <- 'B40'
cosmos <- 'COSMOS/'
cos_csv <- 'csv/'
cos_ex <- 'excel/'

liz_c1 <- 'COSMOS lizrd for NE.CSV'
df_liz_c1 <- from_cosmos_collate(paste0(data_liz, cosmos, liz_c1), sitecode = 'B40') %>%
  convert_daily()

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
    convert_daily()
  
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

df_comb$col_mod_rain <- ifelse(is.na(df_comb$col_rain), df_comb$mod_rain, df_comb$col_rain)
df_comb$cum_mod_rain <- cumsum(df_comb[['mod_rain']])
df_comb$cum_col_rain <- cumsum(df_comb[['col_mod_rain']])

df_comb <- df_comb %>%
  mutate(no_rain_data = is.na(col_rain))

df_comb <- df_comb %>%
  dplyr::mutate(m_tmin_r = zoo::rollapply(mod_tmin, 7, mean, na.rm = TRUE, fill = NA),
                c_time_r = zoo::rollapply(col_tmin, 7, mean, na.rm = TRUE, fill = NA),
                m_tmax_r = zoo::rollapply(mod_tmax, 7, mean, na.rm = TRUE, fill = NA),
                c_tmax_r = zoo::rollapply(col_tmax, 7, mean, na.rm = TRUE, fill = NA))

df_miss2 <- df_comb %>%
  mutate(col_rain = ifelse(is.na(col_rain), 69, col_rain)) %>%
  group_by(group = cumsum(c(0, diff(col_rain) != 0))) %>%
  filter(col_rain == 69 & n() > 10) %>%
  summarize("start.date"=min(date),
            "end.date"=max(date),
            "Length of Run"=n()) %>%
  ungroup() %>%
  select(-matches("group"))


ggplot(df_comb) +
  geom_line(aes(x=date, y=cum_mod_rain, color='blue')) +
  geom_line(aes(x=date, y=cum_col_rain, color='red')) +
  geom_rect(data = df_miss2, ymin = -Inf, ymax = +Inf, 
            aes(xmin = start.date, xmax = end.date),
            fill = 'skyblue', alpha = 0.5) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected"))

ggplot(df_comb) +
  geom_line(aes(x=date, y=m_tmax_r, color='blue')) +
  geom_line(aes(x=date, y=c_tmax_r, color='red')) +
  scale_color_discrete(name = "Datasets", labels = c("Modelled", "Collected"))





ggplot() +
  geom_line(df_cs, aes(x=date, y=rain_mod)) +
  geom_line(df_cs, aes(x=date, y=rain_col), color='red') +
  geom_point(df_comb, aes(x=date, y=col_rain), color='red')






ggplot(df_comb) +
  geom_point(aes(x=date, y=mod_rain)) +
  geom_point(aes(x=date, y=col_rain), color='red')










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