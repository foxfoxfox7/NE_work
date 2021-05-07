source('./collate_funcs.R')


setwd('C:/Users/kiera/Work/NE_work/Weather/collate_data/')


################################################################################
# Ainsdale
################################################################################

data_ains <- '../Data/Ainsdale/'
file_n <- "Ainsdale QA.xlsx"
file_n2 <- "LTMN_AWS_Ainsdale_raw_data.xlsx"

weather_in <- read_excel(paste0(data_ains, file_n2), sheet = 1)
df <- from_raw(weather_in)

################################################################################
# Bure marshes
################################################################################


data_bure <- '../Data/BureMarshes/'

file_in <- 'LTMN_AWS_Bure_Marshes_raw_data.xlsx'
weather_in <- read_excel(paste0(data_bure, file_in), sheet = 1)
df1 <- from_raw(weather_in)

file_in <- 'Bure Marshes export 01 07 2020 to 31 12 2020.XLSX'
weather_in <- read_excel(paste0(data_bure, file_in), sheet = 1)
df2 <- from_camp2(weather_in, 'B01')

weather_list <- list(df1, df2)
df <- combine_weather(weather_list)

################################################################################
# Burnham Beeches
################################################################################

data_bure <- '../Data/BurnhamBeeches/'

raw_col_typ <- c('text', 'text', 'date', 'numeric', 'date', 'numeric',
                 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                 'numeric', 'numeric', 'numeric', 'numeric', 'numeric',
                 'text', 'text', 'text', 'text')
file_in <- 'LTMN_AWS_Burnham_Beeches_raw_data.xlsx'
df_in <- read_excel(paste0(data_bure, file_in), sheet = 1,
                         col_types = raw_col_typ)
df <- from_raw(df_in)




















################################################################################
# Cross Fell
################################################################################

data_cross <- '../Data/CrossFell/'
data_ceh <- 'CEH/'

cf1 <- 'moorh for NE.CSV'

df1 <- read.csv(paste0(data_cross, cf1))


