library(tidyverse)
library(readxl)
library(plyr)
library(matrixStats)
library(diverse)
library(openxlsx)


setwd('C:/Users/kiera/Work/NE_work/Dataframe')
getwd()


################################################################################
# Reading in information from disk
################################################################################

data <- '../Data/'
list_of_files <- list.files(data)
N_files <- length(list_of_files)
print(list_of_files)

name_swap <- read.csv('rename_habitat.csv')
name_swap_vector <- with(name_swap, setNames(right_name, wrong_name))

spec_num_list = list()
gf_list = list()
survey_list = list()

survey_list_pc = list()
survey_list_f = list()

for (ii in 1:N_files) {
  print(list_of_files[ii])
  file_name <- paste(data, list_of_files[ii], sep='')

  st <- read_excel(file_name, sheet = "Species Template", na = c('', "NA", "N/A"))
  # Getting the names of the columns
  col_names <- names(st)

  # Getting the names of the CELL columns
  cell_names <- col_names[grep('CELL', col_names)]
  # converting any non numeric to numeric
  st[cell_names] <- lapply(st[cell_names], function(x) as.numeric(as.character(x)))
  # converting all plot_ids to character as some are and some are not
  st$PLOT_ID <- as.character(st$PLOT_ID)

  ################################################################################
  # Initial edits to species list ready for transformation
  # keeping certain columns, changing the plot ID, changing names to pre-approved
  # ones, changing missing data to holding place string, redoing freq
  ################################################################################

  # The list of columns I want to keep. only part of the string
  keep_list <- c('PLOT_ID', 'DESC', 'CELL', 'PERCENT', 'FREQ')
  # Reducing the size of the tibble to only the kept columns
  st <- st %>%
    select(contains(keep_list))

  # There are often many trailing empyty rows
  st <- st[!is.na(st$DESC_LATIN), ]

  st$PLOT_ID <- as.character(st$PLOT_ID)
  # Making the manual swaps
  for (jj in 1:length(name_swap_vector)) {
    st$DESC_LATIN <- replace(
      st$DESC_LATIN,
      st$DESC_LATIN == names(name_swap_vector[jj]), name_swap_vector[jj]
    )
  }

  # FIXING THE FREQUENCY COLUMNS
  # Changing all missing data in CELL columns to 0
  #st[cell_names][is.na(st[cell_names])] <- 0
  # Calculating FREQ from the contents of CELL
  st <- st %>%
    mutate(FREQ2 = rowSums(.[cell_names]))

  st$PERCENT_COVER <- as.numeric(st$PERCENT_COVER)
  st$FREQ2 <- as.numeric(st$FREQ2)

  # Changing all NAs in percent cover or frequency to 999 so they can be
  # changed back later after we have dealth with other NAs
  st[, c('PERCENT_COVER', 'FREQUENCY')][is.na(st[, c('PERCENT_COVER', 'FREQUENCY')])] <- 999

  ################################################################################
  # Pivotting the data for Frequency
  ################################################################################

  # The list of columns I want to keep. only part of the string
  keep_list_freq <- c('PLOT_ID', 'DESC', 'FREQ2')
  # Reducing the size of the tibble to only the kept columns
  st_freq <- st %>%
    select(contains(keep_list_freq))

  # Getting a list of the plot IDs
  plot_ids <- unique(st_freq['PLOT_ID'])
  N_plots <- length(plot_ids$PLOT_ID)

  # Making a pivot table for each plot and combining
  plot_pivots <- list()
  for (jj in 1:N_plots) {
    single_plot <- st_freq %>%
      # filters for only one plot
      filter(PLOT_ID == plot_ids$PLOT_ID[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$DESC_LATIN), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = DESC_LATIN, values_from = FREQ2)

    plot_pivots[[jj]] <- single_plot
  }
  frequency <- rbind.fill(plot_pivots)

  # Converting NA to 0
  frequency[is.na(frequency)] <- 0
  # These ones should be NA, they are missing data from the start
  frequency[frequency == 999] <- NA

  ################################################################################
  # Pivotting the data for percent cover
  ################################################################################

  # The list of columns I want to keep. only part of the string
  keep_list_pc <- c('PLOT_ID', 'DESC', 'PERCENT_COVER')
  # Reducing the size of the tibble to only the kept columns
  st_pc <- st %>%
    select(contains(keep_list_pc))

  # Making a pivot table for each plot and combining
  plot_pivots_pc <- list()
  for (jj in 1:N_plots) {
    single_plot_pc <- st_pc %>%
      # filters for only one plot
      filter(PLOT_ID == plot_ids$PLOT_ID[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$DESC_LATIN), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = DESC_LATIN, values_from = PERCENT_COVER)

    plot_pivots_pc[[jj]] <- single_plot_pc
  }

  # fills when the columns aren't the same and fills blanks with NA
  percent_cover <- rbind.fill(plot_pivots_pc) %>%
    as_tibble(.)

  # Converting NA to 0
  percent_cover[is.na(percent_cover)] <- 0
  # These ones should be NA, they are missing data from the start
  percent_cover[percent_cover == 999] <- NA



  ##############################################################################
  # setting up the wpd
  ##############################################################################
  
  
  # Getting the data out of whole plot data to group the spec data
  wpd <- read_excel(file_name, sheet = "Whole Plot Data", na = c('', "NA", "N/A"))
  # Always convert to character as some are character and some are num
  wpd$PLOT_ID <- as.character(wpd$PLOT_ID)

  # These are the columns we want from wpd
  wpd_data_cols <- c('PLOT_ID', 'SITECODE', 'YEAR', 'EASTINGS' , 'NORTHINGS',
                     'BAP_BROAD', 'NVC_FIRST')
  wpd_data <- wpd %>%
    select(contains(wpd_data_cols))
  
  # some of the datse are in date format and some are in string
  # Helps with some of the typos involving capital letters
  wpd_data$BAP_BROAD <- str_to_title(wpd_data$BAP_BROAD)
  
  # Date is used for CEH
  #wpd_data$SDATE <- as.character(wpd_data$SDATE)

  # SEPARATING OUT THE NVC COLUMN data
  wpd_data$NVC_FIRST <- wpd_data$NVC_FIRST %>%
    str_extract(., '^.*?(?=:)') %>%
    gsub('(\\d)\\D+$', '\\1', .) %>%
    gsub('[0-9]+', '', .)
  
  # wpd_data$NVC_group <- str_extract(wpd_data$NVC_FIRST, '^.*?(?=:)')
  # wpd_data$NVC_groupb <- gsub('(\\d)\\D+$', '\\1', wpd_data$NVC_group)
  # wpd_data$NVC_groupc <- gsub('[0-9]+', '', wpd_data$NVC_groupb)

  
  
  ##############################################################################
  # combining
  ##############################################################################
  
  # Combining on PLOT_ID
  full_species_pc <- full_join(wpd_data, percent_cover, by = "PLOT_ID")
  full_species_f <- full_join(wpd_data, frequency, by = "PLOT_ID")


  survey_list_pc[[ii]] <- full_species_pc
  survey_list_f[[ii]] <- full_species_f
}

################################################################################
# Combning it together ready for analysis
################################################################################

survey_total_pc <- bind_rows(survey_list_pc)
survey_total_f <- bind_rows(survey_list_f)

# replacing NA in all species cols. not in the first set of cols that we
# take from wpd
survey_total_pc[, -(1:length(wpd_data_cols))][is.na(survey_total_pc[, -(1:length(wpd_data_cols))])] <- 0
survey_total_f[, -(1:length(wpd_data_cols))][is.na(survey_total_f[, -(1:length(wpd_data_cols))])] <- 0

new_name_cols <- c('Plot_ID', 'Sitecode', 'Year', 'Eastings' , 'Northings',
                   'BAP_broad', 'NVC_FIRST')

colnames(survey_total_pc)[1:length(wpd_data_cols)] <- new_name_cols
colnames(survey_total_f)[1:length(wpd_data_cols)] <- new_name_cols

write.csv(survey_total_pc, 'all_all_species_pc.csv', row.names = FALSE)
write.csv(survey_total_f, 'all_all_species_f.csv', row.names = FALSE)

wb <- createWorkbook()
survey_sheet <- addWorksheet(wb, 'presentation_plots')
writeData(wb, survey_sheet, survey_total)
saveWorkbook(wb, 'presentation_surveys.xlsx')

